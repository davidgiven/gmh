{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import qualified Flags as Flags
import qualified System.Environment as Environment

data GlobalOptions =
    GlobalOptions {
        databasePath :: String
    } deriving (Show)

main :: IO ()
main =
    do
        argv <- Environment.getArgs
        globalOptions <- return (Flags.parse defaultOptions argv optionsDescription)
        print globalOptions
    where
        defaultOptions =
            GlobalOptions {
                databasePath = "gmh.sqlite"
            }
        optionsDescription =
            [
                Flags.stringFlag ["-D", "--database"] setDatabasePath
            ]
        setDatabasePath options value = options { databasePath = value }
