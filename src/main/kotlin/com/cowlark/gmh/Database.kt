/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import org.sqlite.JDBC
import org.sqlite.SQLiteConfig
import java.sql.Connection

fun connect_to_database(filename: String): Connection {
    val config = SQLiteConfig()
    config.enforceForeignKeys(true)
    config.setSynchronous(SQLiteConfig.SynchronousMode.OFF)

    val connection = JDBC.createConnection(JDBC.PREFIX + filename, config.toProperties())
    connection.autoCommit = false

    val statement = connection.createStatement()
    statement.execute("""
        CREATE TABLE IF NOT EXISTS variables (
            name TEXT PRIMARY KEY,
            value TEXT
        )
    """)
    statement.execute("""
        CREATE TABLE IF NOT EXISTS messages (
            msgId INTEGER PRIMARY KEY,
            thrId INTEGER,
            value TEXT
        )
    """)
    statement.execute("""
        CREATE TABLE IF NOT EXISTS flags (
            id INTEGER PRIMARY KEY,
            name TEXT UNIQUE
        )
    """)
    statement.execute("""
        CREATE INDEX IF NOT EXISTS flags_by_name ON flags (name)
    """)
    statement.execute("""
        CREATE TABLE IF NOT EXISTS flagMap (
            msgId INTEGER,
            flagId INTEGER,
            FOREIGN KEY (msgId) REFERENCES messages(msgId) ON DELETE CASCADE,
            FOREIGN KEY (flagId) REFERENCES flags(id) ON DELETE CASCADE
        )
    """)
    statement.execute("""
        CREATE INDEX IF NOT EXISTS flagMap_by_msg ON flagMap (msgId)
    """)
    connection.commit()

    return connection
}

class Database constructor(filename: String) {
    val connection = connect_to_database(filename)

    val getVarStatement = connection.prepareStatement(
            "SELECT value FROM variables WHERE (name = ?)"
    )

    val setVarStatement = connection.prepareStatement(
            "INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)"
    )
    
    val insertSkeletonStatement = connection.prepareStatement(
            "INSERT OR IGNORE INTO messages (msgId) VALUES (?)"
    )

    val insertFlagStatement = connection.prepareStatement(
            "INSERT OR IGNORE INTO flags (name) VALUES (?)"
    )

    val clearFlagMappingStatement = connection.prepareStatement(
            "DELETE FROM flagMap WHERE msgId = ?"
    )
    
    val addFlagMappingStatement = connection.prepareStatement(
            "INSERT INTO flagMap (msgId, flagId) VALUES (" +
                    "?, (SELECT id FROM flags WHERE name = ?))"

    )

    fun commit() {
        connection.commit()
    }

    fun rollback() {
        connection.rollback()
    }

    fun get(name: String, default: String): String {
        getVarStatement.setString(1, name)
        val results = getVarStatement.executeQuery()
        try {
            if (!results.next())
                return default
            return results.getString(1)
        }
        finally {
            results.close()
        }
    }

    fun set(name: String, value: String) {
        setVarStatement.setString(1, name)
        setVarStatement.setString(2, value)
        setVarStatement.execute()
    }

    fun addSkeleton(skeleton: MessageSkeleton) {
        insertSkeletonStatement.setLong(1, skeleton.messageId)
        insertSkeletonStatement.execute()

        clearFlagMapping(skeleton.messageId)
        skeleton.flags.map {
            addFlag(it)
            addFlagMapping(skeleton.messageId, it)
        }
    }

    fun addFlag(flag: String) {
        insertFlagStatement.setString(1, flag)
        insertFlagStatement.execute()
    }

    fun clearFlagMapping(msgId: Long) {
        clearFlagMappingStatement.setLong(1, msgId)
        clearFlagMappingStatement.execute()
    }

    fun addFlagMapping(msgId: Long, flag: String) {
        addFlagMappingStatement.setLong(1, msgId)
        addFlagMappingStatement.setString(2, flag)
        addFlagMappingStatement.execute()
    }
}