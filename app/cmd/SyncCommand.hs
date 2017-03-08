module SyncCommand where
import GlobalOptions
import qualified Flags as Flags
import qualified Database as Database
import qualified System.Posix.Files as Posix
import Data.Text
import qualified Network.Connection as Connection
import qualified Network.IMAP as IMAP
import qualified Network.IMAP.Types as IMAP
import Network.IMAP.Types (IMAPConnection, IMAPSettings)
import ListT (toList, ListT)

data Options = Options {}
defaultOptions = Options {}
optionsDescription = []

run :: GlobalOptions -> [Text] -> IO ()
run globalOptions argv = do
    case argv of
        [] -> doSync globalOptions
        _ -> error "syntax: sync"
    where
        (Flags.ParsedFlags options rest) = Flags.parse defaultOptions argv optionsDescription

doSync :: GlobalOptions -> IO ()
doSync globalOptions = do
    db <- Database.open (databasePath globalOptions)
    imap <- connect db
    return ()

connect :: Database.Connection -> IO IMAPConnection
connect db = do
    username <- Database.getVariable db "username" ""
    password <- Database.getVariable db "password" ""
    if (username == "") || (password == "") then
        error "invalid username and/or password (did you remember to use the login command?)"
    else
        return ()

    print "connecting..."
    imap <- IMAP.connectServer imapParams Nothing
    print "authenticating..."
    result <- toList $ IMAP.login imap username password
    processResults result
    return imap
    where
        tlsSettings = Connection.TLSSettingsSimple False False False
        imapParams = Connection.ConnectionParams "imap.gmail.com" 993 (Just tlsSettings) Nothing

processResults :: [IMAP.CommandResult] -> IO ()
processResults [] = do return ()
processResults (result:rest) = do
    print result
    processResults rest
