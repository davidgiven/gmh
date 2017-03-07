module GlobalOptions where
import qualified Flags as Flags
import Data.Text

data GlobalOptions =
    GlobalOptions {
        databasePath :: Text
    } deriving (Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
    GlobalOptions {
        databasePath = "gmh.sqlite"
    }

globalOptionsDescription :: [Flags.Flag GlobalOptions]
globalOptionsDescription =
    [
        Flags.textFlag ["-D", "--database"] setDatabasePath
    ]
    where
        setDatabasePath options value = options { databasePath = value }
