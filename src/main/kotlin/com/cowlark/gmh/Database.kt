/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import com.sun.mail.imap.protocol.ENVELOPE
import org.sqlite.JDBC
import org.sqlite.SQLiteConfig
import java.sql.Connection
import javax.mail.internet.InternetAddress

enum class AddressKind {
  FROM,
  TO,
  CC,
  BCC,
  REPLYTO,
  SENDER
}

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
            gmailId INTEGER PRIMARY KEY,
            threadId INTEGER,
            uid INTEGER,
            flags TEXT,
            value BLOB,
            date INTEGER,
            subject TEXT,
            messageId TEXT
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
            gmailId INTEGER,
            labelId INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
            FOREIGN KEY (labelId) REFERENCES labels(id) ON DELETE CASCADE
        )
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS labelMap_by_msg ON labelMap (gmailId)
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS labelMap_by_label ON labelMap (labelId)
    """)
  statement.execute("""
        CREATE TABLE IF NOT EXISTS addresses (
            id INTEGER PRIMARY KEY,
            email TEXT UNIQUE,
            name TEXT
        )
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS addresses_by_email ON addresses (email)
    """)
  statement.execute("""
        CREATE TABLE IF NOT EXISTS addressMap (
            gmailId INTEGER,
            addressId INTEGER,
            kind INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE,
            FOREIGN KEY (addressId) REFERENCES addresses(id) ON DELETE CASCADE
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

  val setVarStatement = connection.prepareStatement(
      "INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)"
  )

  val addMessageStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO messages (gmailId) VALUES (?)"
  )

  val setMessageUidStatement = connection.prepareStatement(
      "UPDATE messages SET uid = ? WHERE gmailId = ?"
  )

  val setMessageThreadIdStatement = connection.prepareStatement(
      "UPDATE messages SET threadId = ? WHERE gmailId = ?"
  )

  val setMessageFlagsStatement = connection.prepareStatement(
      "UPDATE messages SET flags = ? WHERE gmailId = ?"
  )

  val addLabelStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO labels (name) VALUES (?)"
  )

  val clearMessageLabelsStatement = connection.prepareStatement(
      "DELETE FROM labelMap WHERE gmailId = ?"
  )

  val addMessageLabelStatement = connection.prepareStatement(
      "INSERT INTO labelMap (gmailId, labelId) VALUES (" +
          "?, (SELECT id FROM labels WHERE name = ?))"
  )

  val getUidsWithNoBodyStatement = connection.prepareStatement(
      "SELECT uid FROM messages WHERE value is null " +
          "ORDER BY uid DESC"
  )

  val setMessageValueStatement = connection.prepareStatement(
      "UPDATE messages SET " +
          "value = ?, date = ?, subject = ?, messageId = ? " +
          "WHERE gmailId = ?"
  )

  val addAddressStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO addresses (email, name) VALUES (?, ?)"
  )

  val addMessageAddressStatement = connection.prepareStatement(
      "INSERT INTO addressMap (gmailId, addressId, kind) VALUES (" +
          "?, (SELECT id FROM addresses WHERE email = ?), ?)"
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

  fun addMessage(gmailId: Long) {
    addMessageStatement.setLong(1, gmailId)
    addMessageStatement.execute()
  }

  fun setMessageUid(gmailId: Long, uid: Long) {
    setMessageUidStatement.setLong(1, uid)
    setMessageUidStatement.setLong(2, gmailId)
    setMessageUidStatement.execute()
  }

  fun setMessageThreadId(gmailId: Long, threadId: Long) {
    setMessageThreadIdStatement.setLong(1, threadId)
    setMessageThreadIdStatement.setLong(2, gmailId)
    setMessageThreadIdStatement.execute()
  }

  fun setMessageFlags(gmailId: Long, flags: String) {
    setMessageFlagsStatement.setString(1, flags)
    setMessageFlagsStatement.setLong(2, gmailId)
    setMessageFlagsStatement.execute()
  }

  fun setMessageLabels(gmailId: Long, labels: Set<String>) {
    clearMessageLabels(gmailId)
    labels.map {
      addLabel(it)
      addMessageLabel(gmailId, it)
    }
  }

  fun addLabel(flag: String) {
    addLabelStatement.setString(1, flag)
    addLabelStatement.execute()
  }

  fun clearMessageLabels(gmailId: Long) {
    clearMessageLabelsStatement.setLong(1, gmailId)
    clearMessageLabelsStatement.execute()
  }

  fun addMessageLabel(gmailId: Long, flag: String) {
    addMessageLabelStatement.setLong(1, gmailId)
    addMessageLabelStatement.setString(2, flag)
    addMessageLabelStatement.execute()
  }

  fun addAddress(email: String, name: String?) {
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

  fun setMessageValue(gmailId: Long, value: String, envelope: ENVELOPE) {
    setMessageValueStatement.setString(1, value)
    setMessageValueStatement.setLong(2, envelope.date?.time ?: 0L)
    setMessageValueStatement.setString(3, envelope.subject)
    setMessageValueStatement.setString(4, envelope.messageId)
    setMessageValueStatement.setLong(5, gmailId)
    setMessageValueStatement.execute()

    fun add(address: InternetAddress, kind: AddressKind) {
      addAddressStatement.setString(1, address.address)
      addAddressStatement.setString(2, address.personal)
      addAddressStatement.execute()

      addMessageAddressStatement.setLong(1, gmailId)
      addMessageAddressStatement.setString(2, address.address)
      addMessageAddressStatement.setInt(3, kind.ordinal)
      addMessageAddressStatement.execute()
    }

    if (envelope.bcc != null)
      envelope.bcc.map { add(it, AddressKind.BCC) }
    if (envelope.cc != null)
      envelope.cc.map { add(it, AddressKind.CC) }
    if (envelope.from != null)
      envelope.from.map { add(it, AddressKind.FROM) }
    if (envelope.to != null)
      envelope.to.map { add(it, AddressKind.TO) }
    if (envelope.replyTo != null)
      envelope.replyTo.map { add(it, AddressKind.REPLYTO) }
    if (envelope.sender != null)
      envelope.sender.map { add(it, AddressKind.SENDER) }
  }
}