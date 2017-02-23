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
        CREATE TABLE IF NOT EXISTS bodies (
            id TEXT PRIMARY KEY,
            received INTEGER,
            subject TEXT,
            value TEXT
        )
    """)
    statement.execute("""
        CREATE TABLE IF NOT EXISTS summary (
            id TEXT PRIMARY KEY
        )
    """)
    connection.commit()

    return connection
}

class Database constructor(filename: String) {
    val connection = connect_to_database(filename)

    val getVarStatement = connection.prepareStatement(
            "SELECT value FROM variables WHERE (name = ?)"
    )

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
}