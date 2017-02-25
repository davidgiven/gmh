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
            uid INTEGER,
            flags TEXT,
            value BLOB
        )
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS messages_by_uid ON messages (uid)
    """)
  statement.execute("""
        CREATE TABLE IF NOT EXISTS labels (
            id INTEGER PRIMARY KEY,
            name TEXT UNIQUE
        )
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS labels_by_name ON labels (name)
    """)
  statement.execute("""
        CREATE TABLE IF NOT EXISTS labelMap (
            msgId INTEGER,
            labelId INTEGER,
            FOREIGN KEY (msgId) REFERENCES messages(msgId) ON DELETE CASCADE,
            FOREIGN KEY (labelId) REFERENCES labels(id) ON DELETE CASCADE
        )
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS labelMap_by_msg ON labelMap (msgId)
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS labelMap_by_label ON labelMap (labelId)
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

  val addMessageStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO messages (msgId) VALUES (?)"
  )

  val setMessageUidStatement = connection.prepareStatement(
      "UPDATE messages SET uid = ? WHERE msgId = ?"
  )

  val setMessageFlagsStatement = connection.prepareStatement(
      "UPDATE messages SET flags = ? WHERE msgId = ?"
  )

  val addLabelStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO labels (name) VALUES (?)"
  )

  val clearMessageLabelsStatement = connection.prepareStatement(
      "DELETE FROM labelMap WHERE msgId = ?"
  )

  val addMessageLabelStatement = connection.prepareStatement(
      "INSERT INTO labelMap (msgId, labelId) VALUES (" +
          "?, (SELECT id FROM labels WHERE name = ?))"

  )

  val getUidsWithNoBodyStatement = connection.prepareStatement(
      "SELECT uid FROM messages WHERE value is null " +
          "ORDER BY uid DESC"
  )

  val setMessageValueStatement = connection.prepareStatement(
      "UPDATE messages SET value = ? WHERE msgId = ?"
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

  fun addMessage(msgId: Long) {
    addMessageStatement.setLong(1, msgId)
    addMessageStatement.execute()
  }

  fun setMessageUid(msgId: Long, uid: Long) {
    setMessageUidStatement.setLong(1, uid)
    setMessageUidStatement.setLong(2, msgId)
    setMessageUidStatement.execute()
  }

  fun setMessageFlags(msgId: Long, flags: String) {
    setMessageFlagsStatement.setString(1, flags)
    setMessageFlagsStatement.setLong(2, msgId)
    setMessageFlagsStatement.execute()
  }

  fun setMessageLabels(msgId: Long, labels: Set<String>) {
    clearMessageLabels(msgId)
    labels.map {
      addLabel(it)
      addMessageLabel(msgId, it)
    }
  }

  fun addLabel(flag: String) {
    addLabelStatement.setString(1, flag)
    addLabelStatement.execute()
  }

  fun clearMessageLabels(msgId: Long) {
    clearMessageLabelsStatement.setLong(1, msgId)
    clearMessageLabelsStatement.execute()
  }

  fun addMessageLabel(msgId: Long, flag: String) {
    addMessageLabelStatement.setLong(1, msgId)
    addMessageLabelStatement.setString(2, flag)
    addMessageLabelStatement.execute()
  }

  fun getUidsWithNoBody(): List<Long> {
    val responses = getUidsWithNoBodyStatement.executeQuery()
    try {
      val result = mutableListOf<Long>()
      while (responses.next())
        result.add(responses.getLong(1))
      return result
    }
    finally {
      responses.close()
    }
  }

  fun setMessageValue(msgId: Long, value: String) {
    setMessageValueStatement.setString(1, value)
    setMessageValueStatement.setLong(2, msgId)
    setMessageValueStatement.execute()
  }
}