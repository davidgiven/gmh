module Database where
import qualified Database.SQLite3 as SQLite
import GlobalOptions
import qualified Text.RawString.QQ as QQ
import Data.IORef
import Data.Text
import qualified Data.Map as Map

data Connection =
    Connection {
        raw :: SQLite.Database,
        statementCache :: IORef (Map.Map Text SQLite.Statement)
    }

prepare :: Connection -> Text -> IO SQLite.Statement
prepare conn sql = do
    cache <- readIORef (statementCache conn)
    case (Map.lookup sql cache) of
        Just stmt -> do
            SQLite.reset stmt
            return stmt
        Nothing -> do
            stmt <- SQLite.prepare (raw conn) sql
            writeIORef (statementCache conn) (Map.insert sql stmt cache)
            return stmt

open :: Text -> IO Connection
open filename = do
    raw <- SQLite.open filename
    statementCache <- newIORef Map.empty
    SQLite.exec raw "BEGIN;"
    return Connection { raw, statementCache }

init :: Connection -> IO ()
init conn = do
    SQLite.exec (raw conn) sqlInitScript
    commit conn
    return ()

commit :: Connection -> IO ()
commit conn = do
    SQLite.exec (raw conn) "COMMIT; BEGIN;"

setVariable :: Connection -> Text -> Text -> IO ()
setVariable conn name value = do
    stmt <- prepare conn "INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)"
    SQLite.bindSQLData stmt 1 (SQLite.SQLText name)
    SQLite.bindSQLData stmt 2 (SQLite.SQLText value)
    SQLite.step stmt
    return ()

sqlInitScript :: Text
sqlInitScript = [QQ.r|
    CREATE TABLE IF NOT EXISTS variables (
        name TEXT PRIMARY KEY,
        value TEXT
    );

    CREATE TABLE IF NOT EXISTS messages (
        gmailId INTEGER PRIMARY KEY,
        threadId INTEGER,
        uid INTEGER,
        flags TEXT,
        date INTEGER,
        headers TEXT,
        messageId TEXT,
        downloaded INTEGER DEFAULT 0
    );
    CREATE VIRTUAL TABLE IF NOT EXISTS messageData USING FTS4(
        subject TEXT,
        body TEXT
    );
    CREATE INDEX IF NOT EXISTS messages_by_uid ON messages (uid);
    CREATE INDEX IF NOT EXISTS messages_by_threadId ON messages (uid);
    CREATE INDEX IF NOT EXISTS messages_by_downloaded ON messages (downloaded);
    CREATE INDEX IF NOT EXISTS messages_by_date ON messages (date);

    CREATE TABLE IF NOT EXISTS labels (
        labelId INTEGER PRIMARY KEY,
        name TEXT UNIQUE
    );

    CREATE INDEX IF NOT EXISTS labels_by_name ON labels (name);

    CREATE TABLE IF NOT EXISTS labelMap (
        gmailId INTEGER,
        labelId INTEGER,
        FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
        FOREIGN KEY (labelId) REFERENCES labels(labelId) ON DELETE CASCADE
    );
    CREATE INDEX IF NOT EXISTS labelMap_by_msg ON labelMap (gmailId);
    CREATE INDEX IF NOT EXISTS labelMap_by_label ON labelMap (labelId);

    CREATE TABLE IF NOT EXISTS addresses (
        addressId INTEGER PRIMARY KEY,
        email TEXT UNIQUE,
        name TEXT
    );
    CREATE INDEX IF NOT EXISTS addresses_by_email ON addresses (email);

    CREATE TABLE IF NOT EXISTS addressMap (
        gmailId INTEGER,
        addressId INTEGER,
        kind INTEGER,
        FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
        FOREIGN KEY (addressId) REFERENCES addresses(addressId) ON DELETE CASCADE
    );
    CREATE INDEX IF NOT EXISTS addressMap_by_gmailId ON addressMap (gmailId);
    CREATE INDEX IF NOT EXISTS addressMap_by_addressId ON addressMap (addressId);

    CREATE TABLE IF NOT EXISTS selected (
        gmailId INTEGER,
        FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE
    );
|]
