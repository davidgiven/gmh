require "./database"
require "./flags"

class InitFlags
    define_flags
end

def doInitCommand(globalFlags : GlobalFlags)
    flags = InitFlags.new.parse(globalFlags.argv)
    if flags.argv.size != 0
        raise UserException.new("syntax: init")
    end

    db = Database.new(globalFlags)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS variables (
            name TEXT PRIMARY KEY,
            value TEXT
        )
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS variables (
            name TEXT PRIMARY KEY,
            value TEXT
        )
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS messages (
            gmailId INTEGER PRIMARY KEY,
            threadId INTEGER,
            uid INTEGER,
            date INTEGER,
            headers TEXT,
            messageId TEXT,
            downloaded INTEGER DEFAULT 0
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS messages_by_uid ON messages (uid)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS messages_by_threadId ON messages (threadId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS messages_by_downloaded ON messages (downloaded)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS messages_by_date ON messages (date)
    SQL)

    db.exec(<<-SQL
        CREATE VIRTUAL TABLE IF NOT EXISTS messageData USING FTS4(
            subject TEXT,
            body TEXT
        )
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS labels (
            labelId INTEGER PRIMARY KEY,
            name TEXT UNIQUE
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS labels_by_name ON labels (name)
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS labelMap (
            gmailId INTEGER,
            labelId INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
            FOREIGN KEY (labelId) REFERENCES labels(labelId) ON DELETE CASCADE
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS labelMap_by_msg ON labelMap (gmailId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS labelMap_by_label ON labelMap (labelId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS labelMap_by_both ON labelMap (gmailId, labelId)
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS flags (
            flagId INTEGER PRIMARY KEY,
            name TEXT UNIQUE
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS flags_by_name ON flags (name)
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS flagMap (
            gmailId INTEGER,
            flagId INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
            FOREIGN KEY (flagId) REFERENCES flags(flagId) ON DELETE CASCADE
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS flagMap_by_msg ON flagMap (gmailId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS flagMap_by_flag ON flagMap (flagId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS flagMap_by_both ON flagMap (flagId, gmailId)
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS flagMapChanges (
            gmailId INTEGER,
            oldFlagId INTEGER,
            newFlagId INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
            FOREIGN KEY (oldFlagId) REFERENCES flags(flagId) ON DELETE CASCADE,
            FOREIGN KEY (newFlagId) REFERENCES flags(flagId) ON DELETE CASCADE
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS flagMapChanges_by_msg ON flagMapChanges (gmailId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS flagMapChanges_by_both ON flagMapChanges (oldFlagId, gmailId)
    SQL)
    db.exec(<<-SQL
        CREATE VIEW IF NOT EXISTS flagMapFused
            (gmailId, flagId)
            AS
                SELECT flagMap.gmailId, COALESCE(flagMapChanges.newFlagId, flagMap.flagId) AS flagId
                FROM flagMap LEFT JOIN flagMapChanges
                ON flagMap.gmailId = flagMapChanges.gmailId
                    AND (flagMap.flagId IN (flagMapChanges.oldFlagId, flagMapChanges.newFlagId))
            UNION ALL
                SELECT gmailId, newFlagId AS flagId
                FROM flagMapChanges
                WHERE oldFlagId IS NULL
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS addresses (
            addressId INTEGER PRIMARY KEY,
            email TEXT UNIQUE,
            name TEXT
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS addresses_by_email ON addresses (email)
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS addressMap (
            gmailId INTEGER,
            addressId INTEGER,
            kind INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
            FOREIGN KEY (addressId) REFERENCES addresses(addressId) ON DELETE CASCADE
        )
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS addressMap_by_gmailId ON addressMap (gmailId)
    SQL)
    db.exec(<<-SQL
        CREATE INDEX IF NOT EXISTS addressMap_by_addressId ON addressMap (addressId)
    SQL)

    db.exec(<<-SQL
        CREATE TABLE IF NOT EXISTS selected (
            gmailId INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE
        )
    SQL)

    db.commit
end
