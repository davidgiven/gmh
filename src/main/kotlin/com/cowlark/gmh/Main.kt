/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import com.sun.mail.gimap.GmailFolder
import com.sun.mail.gimap.GmailStore
import com.sun.mail.iap.Argument
import com.sun.mail.iap.Response
import com.sun.mail.imap.protocol.*
import org.sqlite.util.StringUtils
import java.nio.charset.StandardCharsets
import javax.mail.Flags
import javax.mail.Folder
import javax.mail.Session

val SQUARE_BRACKET: Byte = '['.toByte()
val FETCH_BATCH_SIZE = 500

fun fatal(message: String) {
  System.err.printf("error: %s\n", message)
  System.exit(1)
}

fun log(message: String) {
  System.err.printf("log: %s\n", message)
}

fun connect_to_imap(username: String, password: String): GmailFolder {
  val properties = System.getProperties()
  properties.setProperty("mail.store.protocol", "gimap")

  val session = Session.getDefaultInstance(properties, null)

  val store = session.getStore("gimap") as GmailStore
  store.connect("imap.gmail.com", username, password)
  if (!store.hasCapability("CONDSTORE") || !store.hasCapability("X-GM-EXT-1"))
    fatal("this doesn't look like a Gmail server")

  val folder = store.getFolder("[Gmail]/All Mail") as GmailFolder
  return folder
}

class MessageSkeleton(response: FetchResponse) {
  var labels: Set<String>? = null
  var uid = 0L
  var gmailId = 0L
  var threadId = 0L
  var flags: String? = null
  var value: String? = null
  var envelope: ENVELOPE? = null

  init {
    if (response.extensionItems != null) {
      response.extensionItems.map {
        when (it.key) {
          "X-GM-MSGID"  -> gmailId = it.value as Long
          "X-GM-THRID"  -> threadId = it.value as Long
          "X-GM-LABELS" -> labels = (it.value as Array<String>).toSet()
          else          -> {
          }
        }
      }
    }

    val uidObject = response.getItem(UID::class.java)
    if (uidObject != null)
      uid = uidObject.uid

    val flagsObject = response.getItem(FLAGS::class.java)
    if (flagsObject != null) {
      flags = ""

      if (flagsObject.contains(Flags.Flag.DELETED))
        flags += "D"
      if (!flagsObject.contains(Flags.Flag.SEEN))
        flags += "U"
      if (flagsObject.contains(Flags.Flag.ANSWERED))
        flags += "R"
    }

    val bodyObject = response.getItem(BODY::class.java)
    if (bodyObject != null) {
      value = String(bodyObject.byteArray.bytes, StandardCharsets.ISO_8859_1)
      // The body object for some reason contains the first line of the FETCH
      // response.
      value = value!!.substring(value!!.indexOf('\n') + 1)
    }

    envelope = response.getItem(ENVELOPE::class.java)

    if (gmailId == 0L)
      log(String.format("message with UID %d has no message ID. This is weird.", uid));
  }

  fun set(db: Database) {
    assert(gmailId != 0L)
    db.addMessage(gmailId)
    if (threadId != 0L)
      db.setMessageThreadId(gmailId, threadId)
    if (uid != 0L)
      db.setMessageUid(gmailId, uid)
    if (flags != null)
      db.setMessageFlags(gmailId, flags!!)
    if (labels != null)
      db.setMessageLabels(gmailId, labels!!)
    if (value != null)
      db.setMessageValue(gmailId, value!!, envelope!!)
  }
}

fun main(args: Array<String>) {
  log("connecting")
  val db = Database("gmh.sqlite")
  val folder = connect_to_imap("david.given@gmail.com", "fjsqqmpawuvmrxmd")
  folder.open(Folder.READ_ONLY)

  var uidValidity = db.get("uidvalidity", "0").toLong()
  val modSeq = db.get("modseq", "0").toLong()
  var newModSeq = modSeq

  fun process_response(response: Response) {
    if (response.isOK) {
      response.skipSpaces()
      if (response.readByte() == SQUARE_BRACKET) {
        when (response.readAtom()) {
          "HIGHESTMODSEQ" ->
            newModSeq = response.readLong()
        }
      }
    }
    else if (response is FetchResponse) {
      MessageSkeleton(response).set(db)
    }
  }

  fun process_responses(message: String, responses: Array<Response>) {
    Progressbar(message, responses.size).use {
      progress ->
      for (i in 0..(responses.size - 1)) {
        progress.show(i)
        process_response(responses[i])
      }
    }
  }

  fun process_responses(responses: Array<Response>) {
    for (i in 0..(responses.size - 1))
      process_response(responses[i])
  }

  if (uidValidity != folder.uidValidity) {
    log("refreshing UIDs")
    db.set("uidvalidity", folder.uidValidity.toString())
    folder.doCommand {
      p ->
      process_responses(
          "Updating UIDs",
          p.command("FETCH",
              Argument()
                  .writeAtom("1:*")
                  .writeArgument(
                      Argument()
                          .writeAtom("UID")
                          .writeAtom("X-GM-MSGID")
                  )
          )
      )
      db.commit()
    }
  }


  log("fetching message updates")
  folder.doCommand {
    p ->
    process_responses(
        "Thinking",
        p.command("FETCH",
            Argument()
                .writeAtom("1:*")
                .writeArgument(
                    Argument()
                        .writeAtom("X-GM-MSGID")
                        .writeAtom("X-GM-LABELS")
                        .writeAtom("FLAGS")
                )
                .writeArgument(
                    Argument()
                        .writeAtom("CHANGEDSINCE")
                        .writeNumber(modSeq)
                )
        )
    )
  }

  db.set("modseq", newModSeq.toString())
  db.commit()

  log("fetching message bodies")
  folder.doCommand {
    p ->
    val pendingBodies = db.getUidsWithNoBody()
    Progressbar("Downloading", pendingBodies.size).use {
      progress ->
      var count = 0
      pendingBodies.asSequence().batch(FETCH_BATCH_SIZE).forEach {
        uids ->
        progress.show(count)
        count += uids.size

        process_responses(
            p.command("UID",
                Argument()
                    .writeAtom("FETCH")
                    .writeAtom(StringUtils.join(uids.map(Long::toString), ","))
                    .writeArgument(
                        Argument()
                            .writeAtom("X-GM-MSGID")
                            .writeAtom("X-GM-THRID")
                            .writeAtom("ENVELOPE")
                            .writeAtom("BODY.PEEK[]")
                    
                    )
            )
        )
        db.commit()
      }
    }
  }
}
