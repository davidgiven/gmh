/* GMH
 * © 2013 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import org.sqlite.JDBC
import org.sqlite.SQLiteConfig
import java.sql.Connection

class Dummy {}

fun connect_to_database(): Connection
{
    val config = SQLiteConfig()
    config.enforceForeignKeys(true)
    config.setSynchronous(SQLiteConfig.SynchronousMode.OFF)

    val connection = JDBC.createConnection(JDBC.PREFIX + "gmh.sqlite", config.toProperties())
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
        id TEXT PRIMARY KEY,
        received INTEGER,
        subject TEXT,
        value TEXT
      )
    """)
    connection.commit()

    return connection
}

