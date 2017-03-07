module InitCommand where
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
        [] -> doInit globalOptions
        _ -> error "syntax: init"
    where
        (Flags.ParsedFlags options rest) = Flags.parse defaultOptions argv optionsDescription

doInit :: GlobalOptions -> IO ()
doInit globalOptions = do
    db <- Database.open (databasePath globalOptions)
    Database.init db
    Database.commit db
    Posix.setFileMode (unpack (databasePath globalOptions)) 0o600
