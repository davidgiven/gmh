/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.lib

import com.sun.mail.imap.protocol.ENVELOPE
import org.sqlite.JDBC
import org.sqlite.SQLiteConfig
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.util.*
import javax.mail.internet.InternetAddress


enum class AddressKind {
  FROM,
  TO,
  CC,
  BCC,
  REPLYTO,
  SENDER
}

class Message(
    val gmailId: Long,
    val threadId: Long,
    val uid: Long,
    val flags: String,
    val date: Date,
    val subject: String,
    val from: List<InternetAddress>,
    val to: List<InternetAddress>,
    val cc: List<InternetAddress>,
    val bcc: List<InternetAddress>,
    val replyTo: List<InternetAddress>,
    val sender: List<InternetAddress>,
    val downloaded: Boolean
) {}

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
            messageId TEXT,
            downloaded INTEGER DEFAULT 0
        )
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS messages_by_uid ON messages (uid)
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS messages_by_downloaded ON messages (downloaded)
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS messages_by_date ON messages (date)
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
  statement.execute("""
        CREATE INDEX IF NOT EXISTS addressMap_by_gmailId ON addressMap (gmailId)
    """)
  statement.execute("""
        CREATE INDEX IF NOT EXISTS addressMap_by_addressId ON addressMap (addressId)
    """)
  statement.execute("""
        CREATE TABLE IF NOT EXISTS selected (
            gmailId INTEGER,
            FOREIGN KEY (gmailId) REFERENCES messages(gmailId) ON DELETE CASCADE
        )
    """)

  connection.commit()

  return connection
}

class Database constructor(filename: String) {
  private val connection = connect_to_database(filename)
  private var statementCache = mutableMapOf<String, PreparedStatement>()

  private fun prepare(statement: String): PreparedStatement {
    return statementCache.getOrElse(statement, {
      var prepared = connection.prepareStatement(statement)
      statementCache.put(statement, prepared)
      return prepared
    })
  }

  private val getVarStatement = connection.prepareStatement(
      "SELECT value FROM variables WHERE (name = ?)"
  )

  private val setVarStatement = connection.prepareStatement(
      "INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)"
  )

  private val addMessageStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO messages (gmailId) VALUES (?)"
  )

  private val setMessageUidStatement = connection.prepareStatement(
      "UPDATE messages SET uid = ? WHERE gmailId = ?"
  )

  private val setMessageThreadIdStatement = connection.prepareStatement(
      "UPDATE messages SET threadId = ? WHERE gmailId = ?"
  )

  private val setMessageFlagsStatement = connection.prepareStatement(
      "UPDATE messages SET flags = ? WHERE gmailId = ?"
  )

  private val addLabelStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO labels (name) VALUES (?)"
  )

  private val clearMessageLabelsStatement = connection.prepareStatement(
      "DELETE FROM labelMap WHERE gmailId = ?"
  )

  private val addMessageLabelStatement = connection.prepareStatement(
      "INSERT INTO labelMap (gmailId, labelId) VALUES (" +
          "?, (SELECT id FROM labels WHERE name = ?))"
  )

  private val setMessageValueStatement = connection.prepareStatement(
      "UPDATE messages SET " +
          "downloaded = 1, value = ?, date = ?, subject = ?, messageId = ? " +
          "WHERE gmailId = ?"
  )

  private val addAddressStatement = connection.prepareStatement(
      "INSERT OR IGNORE INTO addresses (email, name) VALUES (?, ?)"
  )

  private val addMessageAddressStatement = connection.prepareStatement(
      "INSERT INTO addressMap (gmailId, addressId, kind) VALUES (" +
          "?, (SELECT id FROM addresses WHERE email = ?), ?)"
  )

  private val clearSelectionStatement = connection.prepareStatement(
      "DELETE FROM selected"
  )

  private val addLabelToSelectionStatement = connection.prepareStatement(
      "INSERT INTO selected (gmailId) " +
          "SELECT gmailId FROM labelMap WHERE " +
          "labelId = (SELECT id FROM labels WHERE name = ?)"
  )

  private val countSelectionSizeStatement = connection.prepareStatement(
      "SELECT COUNT(*) FROM selected"
  )

  private val getSelectionStatement = connection.prepareStatement(
      "SELECT selected.gmailId FROM selected LEFT JOIN messages " +
          "WHERE selected.gmailId = messages.gmailId " +
          "AND messages.uid IS NOT NULL " +
          "ORDER BY messages.date ASC"
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

  fun getNonDownloadedUids(): List<Long> {
    val statement = prepare(
        "SELECT uid FROM messages WHERE " +
            "downloaded = 0 AND uid IS NOT NULL " +
            "ORDER BY date DESC"
    )

    statement.executeQuery().use {
      responses ->
      val result = mutableListOf<Long>()
      while (responses.next())
        result.add(responses.getLong(1))
      return result
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

  fun clearSelection() {
    clearSelectionStatement.execute()
  }

  fun addLabelToSelection(label: String) {
    addLabelToSelectionStatement.setString(1, label)
    addLabelToSelectionStatement.execute()
  }

  fun countSelectionSize(): Int {
    countSelectionSizeStatement.executeQuery().use {
      responses ->
      responses.next()
      return responses.getInt(1)
    }
  }

  fun getSelection(): List<Long> {
    getSelectionStatement.executeQuery().use {
      responses ->
      val result = mutableListOf<Long>()
      while (responses.next())
        result.add(responses.getLong(1))
      return result
    }
  }

  fun getMessage(gmailId: Long): Message {
    val getMessageStatement = prepare(
        "SELECT threadId, uid, flags, date, subject, downloaded FROM messages " +
            "WHERE gmailId = ?"
    )

    getMessageStatement.setLong(1, gmailId)
    getMessageStatement.executeQuery().use {
      responses ->
      if (!responses.next())
        throw RuntimeException("Gmail ID $gmailId does not exist")

      val threadId = responses.getLong(1)
      val uid = responses.getLong(2)
      val flags = responses.getString(3)
      val date = Date(responses.getLong(4))
      val subject = responses.getString(5)
      val downloaded = responses.getBoolean(6)

      fun getAddresses(kind: AddressKind): List<InternetAddress> {
        val addressStatement = prepare(
            "SELECT addresses.email, addresses.name " +
                "FROM addressMap LEFT JOIN addresses " +
                "ON addressMap.addressId = addresses.id " +
                "WHERE addressMap.gmailId = ? " +
                "AND addressMap.kind = ?"
        )
        addressStatement.setLong(1, gmailId)
        addressStatement.setInt(2, kind.ordinal)
        addressStatement.executeQuery().use {
          val result = mutableListOf<InternetAddress>()
          while (it.next()) {
            val email = it.getString(1)
            val name = it.getString(2)
            result.add(InternetAddress(email, name))
          }
          return result
        }
      }

      return Message(
          gmailId = gmailId,
          threadId = threadId,
          uid = uid,
          flags = flags,
          date = date,
          subject = subject ?: "",
          cc = getAddresses(AddressKind.CC),
          bcc = getAddresses(AddressKind.BCC),
          from = getAddresses(AddressKind.FROM),
          replyTo = getAddresses(AddressKind.REPLYTO),
          sender = getAddresses(AddressKind.SENDER),
          to = getAddresses(AddressKind.TO),
          downloaded = downloaded
      )
    }
  }
}

private inline fun <T : ResultSet, R> T.use(block: (T) -> R): R {
    try {
      return block(this)
    } finally {
      this.close()
    }
}