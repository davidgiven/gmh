module LoginCommand where
import GlobalOptions
import qualified Flags as Flags
import qualified Database as Database
import qualified System.Posix.Files as Posix
import Data.Text

data Options = Options {}
defaultOptions = Options {}
optionsDescription = []

run :: GlobalOptions -> [Text] -> IO ()
run globalOptions argv = do
    case argv of
        (username:password:[]) -> doLogin globalOptions username password
        _ -> error "syntax: login <username> <password>"
    where
        (Flags.ParsedFlags options rest) = Flags.parse defaultOptions argv optionsDescription

doLogin :: GlobalOptions -> Text -> Text -> IO ()
doLogin globalOptions username password = do
    Posix.setFileMode (unpack (databasePath globalOptions)) 0o600
    db <- Database.open (databasePath globalOptions)
    Database.setTextVariable db "username" username
    Database.setTextVariable db "password" password
    Database.commit db
