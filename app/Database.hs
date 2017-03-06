{-# LANGUAGE OverloadedStrings #-}
module Database where
import qualified Database.SQLite.Simple as Simple
import GlobalOptions

data Connection =
    Connection {
        raw :: Simple.Connection
    }

open :: String -> IO Connection
open filename = do
    raw <- Simple.open filename
    Simple.execute_ raw "BEGIN;"
    return Connection {
        raw = raw
    }

commit :: Connection -> IO ()
commit conn = do
    Simple.execute_ (raw conn) "COMMIT;"

setVariable :: Connection -> String -> String -> IO ()
setVariable conn name value = do
    Simple.execute (raw conn) "INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)"
        (name, value)
