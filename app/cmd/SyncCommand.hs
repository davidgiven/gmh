module SyncCommand where
import GlobalOptions
import qualified Flags as Flags
import qualified Database as Database
import qualified System.Posix.Files as Posix
import Data.IORef
import Data.Text
import qualified Network.Connection as Connection
import qualified Network.IMAP as IMAP
import qualified Network.IMAP.Types as IMAP
import Network.IMAP.Types (IMAPConnection, IMAPSettings)
import ListT (ListT)
import qualified ListT as ListT
import qualified UI as UI
import Data.Int

data Options =
    Options {
        forceUidRefresh :: Bool
    } deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options {
        forceUidRefresh = False
    }

optionsDescription :: [Flags.Flag Options]
optionsDescription =
    [
        Flags.boolFlag ["--force-uid-refresh"] setForceUidRefresh
    ]
    where
        setForceUidRefresh options value = options { forceUidRefresh = value }

data State =
    State {
        uidValidity :: Int,
        messageCount :: Int,
        highestModSeq :: Int
    }

run :: GlobalOptions -> [Text] -> IO ()
run globalOptions argv = do
    print argv
    print options
    print rest
    case rest of
        [] -> doSync globalOptions options
        _ -> error "syntax: sync"
    where
        (Flags.ParsedFlags options rest) = Flags.parse defaultOptions argv optionsDescription

doSync :: GlobalOptions -> Options -> IO ()
doSync globalOptions options = do
    db <- Database.open (databasePath globalOptions)
    let ?db = db
    state <- newIORef defaultState
    let ?state = state
    imap <- connect
    let ?imap = imap

    uidsValid <-
        if (forceUidRefresh options)
            then return False
            else checkUidValidity
    if uidsValid
        then return ()
        else refreshUids

    print uidsValid
    return ()
    where
        defaultState =
            State {
                uidValidity = 0,
                messageCount = 0,
                highestModSeq = 0
            }

connect :: (?state :: IORef State) => (?db :: Database.Connection) => IO IMAPConnection
connect = do
    username <- Database.getTextVariable ?db "username" ""
    password <- Database.getTextVariable ?db "password" ""
    if (username == "") || (password == "") then
        error "invalid username and/or password (did you remember to use the login command?)"
    else
        return ()

    UI.log "connecting..."
    imap <- IMAP.connectServer imapParams Nothing
    UI.log "authenticating..."
    expectOK $ IMAP.login imap username password
    UI.log "opening mailbox..."
    expectOK $ IMAP.select imap "\"[Gmail]/All Mail\""
    return imap
    where
        tlsSettings = Connection.TLSSettingsSimple False False False
        imapParams = Connection.ConnectionParams "imap.gmail.com" 993 (Just tlsSettings) Nothing

checkUidValidity :: (?state :: IORef State) => (?db :: Database.Connection) => (?imap :: IMAPConnection) => IO Bool
checkUidValidity = do
    oldValidity <- Database.getVariable ?db "uidvalidity" 0
    realValidity <- uidValidity <$> readIORef ?state
    return (oldValidity == realValidity)

refreshUids :: (?state :: IORef State) => (?db :: Database.Connection) => (?imap :: IMAPConnection) => IO ()
refreshUids = do
    UI.log "refreshing UIDs..."
    expectOK $ IMAP.fetchG ?imap "1:* (UID X-GM-MSGID)"
    Database.commit ?db

expectOK :: (?state :: IORef State) => ListT IO IMAP.CommandResult -> IO ()
expectOK results = do
    tagged <- processResponses results
    case IMAP.resultState tagged of
        IMAP.OK -> return ()
        _ -> error ("bad response from IMAP command: " ++ show tagged)

processResponse :: (?state :: IORef State) => IMAP.UntaggedResult -> IO ()
processResponse response =
    case response of
        (IMAP.OKResult _) -> return ()
        (IMAP.Recent _) -> return ()
        (IMAP.UIDNext _) -> return ()
        (IMAP.Flags _) -> return ()
        (IMAP.PermanentFlags _) -> return ()

        (IMAP.Exists newCount) ->
            modifyIORef' ?state (\s -> s { messageCount = newCount })
        (IMAP.UIDValidity newValidity) ->
            modifyIORef' ?state (\s -> s { uidValidity = newValidity })
        (IMAP.HighestModSeq newModSeq) ->
            modifyIORef' ?state (\s -> s { highestModSeq = newModSeq })

        (IMAP.Capabilities capabilities) ->
            if (elem (IMAP.CExperimental "X-GM-EXT-1") capabilities)
            then return ()
            else error "server doesn't look like an IMAP server!"

        response ->
            print ("unknown IMAP response: " ++ show response)

processResponses :: (?state :: IORef State) => ListT IO IMAP.CommandResult -> IO IMAP.TaggedResult
processResponses results =
    ListT.fold iterator defaultResult results
    where
        defaultResult =
            IMAP.TaggedResult {
                IMAP.commandId = "",
                IMAP.resultState = IMAP.BAD,
                IMAP.resultRest = "missing tagged result response to command"
            }

        iterator :: IMAP.TaggedResult -> IMAP.CommandResult -> IO IMAP.TaggedResult
        iterator old (IMAP.Untagged response) = do
            processResponse response
            return old
        iterator old (IMAP.Tagged response) = do
            return response
