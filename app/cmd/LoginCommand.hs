module LoginCommand where
import GlobalOptions
import qualified Flags as Flags
import qualified Database as Database
import qualified System.Posix.Files as Posix

data Options = Options {}
defaultOptions = Options {}
optionsDescription = []

run :: GlobalOptions -> [String] -> IO ()
run globalOptions argv = do
    case argv of
        (username:password:[]) -> doLogin globalOptions username password
        _ -> error "syntax: login <username> <password>"
    where
        (Flags.ParsedFlags options rest) = Flags.parse defaultOptions argv optionsDescription

doLogin :: GlobalOptions -> String -> String -> IO ()
doLogin globalOptions username password = do
    Posix.setFileMode (databasePath globalOptions) 0o600
    db <- Database.open (databasePath globalOptions)
    Database.setVariable db "username" username
    Database.setVariable db "password" password
    Database.commit db
