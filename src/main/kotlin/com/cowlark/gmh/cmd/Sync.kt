/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.cmd

import com.cowlark.gmh.GlobalOptions
import com.cowlark.gmh.lib.Database
import com.cowlark.gmh.lib.HasOptions
import com.cowlark.gmh.lib.Option
import com.cowlark.gmh.lib.Progressbar
import com.cowlark.gmh.lib.batch
import com.cowlark.gmh.lib.fatal
import com.cowlark.gmh.lib.log
import com.cowlark.gmh.lib.parseFlags
import com.sun.mail.gimap.GmailFolder
import com.sun.mail.gimap.GmailStore
import com.sun.mail.iap.Argument
import com.sun.mail.iap.Response
import com.sun.mail.imap.protocol.BODY
import com.sun.mail.imap.protocol.ENVELOPE
import com.sun.mail.imap.protocol.FLAGS
import com.sun.mail.imap.protocol.FetchResponse
import com.sun.mail.imap.protocol.UID
import org.sqlite.util.StringUtils
import java.nio.charset.StandardCharsets
import javax.mail.Flags
import javax.mail.Folder
import javax.mail.Session

private val FETCH_BATCH_SIZE = 500

class SyncOptions : HasOptions() {
  @Option(
      longName = "--force-uid-refresh",
      help = "Force a UID resync (useful is the database gets confused)"
  ) var forceUidRefresh = false
}


private fun connect_to_imap(username: String, password: String): GmailFolder {
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

private class MessageSkeleton(response: FetchResponse) {
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

      flagsObject.userFlags.map {
        when (it) {
          "Junk"            -> flags += "J"
          "NonJunk"         -> flags += "H"
          "\$Forwarded"     -> flags += "F"
          "\$Phishing"      -> flags += "P"
          "Old"             -> flags += "O"
          "\$MDNSent"       -> Unit
          "receipt-handled" -> Unit
          "\$label3"        -> Unit
          else              -> {
            log("mysterious user flag $it, ignoring")
          }
        }
      }
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
    if (value != null) {
      val split = value!!.indexOf("\r\n\r\n")
      val header = value!!.slice(0 .. split-1)
      val body = value!!.substring(split+4)
      db.setMessageValue(gmailId, header, body, envelope!!)
    }
  }
}

fun SyncCommand(globalOptions: GlobalOptions) {
  val syncOptions = SyncOptions()
  parseFlags(syncOptions, globalOptions.rest)
  if (syncOptions.rest.isNotEmpty())
    fatal("sync option error")

  log("connecting")
  val db = Database(globalOptions.databasePath)
  val folder = connect_to_imap("david.given@gmail.com", "fjsqqmpawuvmrxmd")
  folder.open(Folder.READ_ONLY)

  var uidValidity = db.get("uidvalidity", "0").toLong()
  val modSeq = db.get("modseq", "0").toLong()
  var newModSeq = folder.highestModSeq
  log("fetching updates from $modSeq to $newModSeq")

  fun process_response(response: Response) {
    if (response.isOK) {
    } else if (response.isBAD) {
      fatal("IMAP server returned error: " + response)
    } else if (response is FetchResponse) {
      MessageSkeleton(response).set(db)
    } else {
      log("unhandled response " + response)
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

  if (syncOptions.forceUidRefresh || (uidValidity != folder.uidValidity)) {
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
                        .writeAtom("UID")
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
    val pendingBodies = db.getNonDownloadedUids()
    Progressbar("Downloading", pendingBodies.size).use {
      progress ->
      var count = 0
      progress.show(0)
      pendingBodies.asSequence().batch(FETCH_BATCH_SIZE).forEach {
        uids ->

        val uidlist = StringUtils.join(uids.map(Long::toString), ",")
        p.writeCommand("UID",
                Argument()
                    .writeAtom("FETCH")
                    .writeAtom(uidlist)
                    .writeArgument(
                        Argument()
                            .writeAtom("X-GM-MSGID")
                            .writeAtom("X-GM-THRID")
                            .writeAtom("ENVELOPE")
                            .writeAtom("BODY.PEEK[]")

                    )
            )

        while (true) {
          val response = p.readResponse()
          process_response(response)
          if (response is FetchResponse) {
            count++
            progress.show(count)
          }
          if (response.isOK)
            break
        }
        db.commit()
      }
    }
  }
}
