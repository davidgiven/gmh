module GlobalOptions where
import qualified Flags as Flags

data GlobalOptions =
    GlobalOptions {
        databasePath :: String
    } deriving (Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
    GlobalOptions {
        databasePath = "gmh.sqlite"
    }

globalOptionsDescription :: [Flags.Flag GlobalOptions]
globalOptionsDescription =
    [
        Flags.stringFlag ["-D", "--database"] setDatabasePath
    ]
    where
        setDatabasePath options value = options { databasePath = value }
