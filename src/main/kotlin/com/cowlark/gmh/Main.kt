/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import com.sun.mail.gimap.GmailFolder
import com.sun.mail.gimap.GmailMsgIdTerm
import com.sun.mail.gimap.GmailStore
import com.sun.mail.iap.Argument
import com.sun.mail.imap.protocol.FLAGS
import com.sun.mail.imap.protocol.FetchResponse
import com.sun.mail.imap.protocol.MODSEQ
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import javax.mail.Flags
import javax.mail.Folder
import javax.mail.Session

val SQUARE_BRACKET: Byte = '['.toByte()
val FETCH_BATCH_SIZE = 10

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
    var labels = mutableSetOf<String>()
    var messageId = 0L
    var flags = ""
    var lastModified = 0L

    init {
        response.extensionItems.map {
            when (it.key) {
                "X-GM-MSGID" -> messageId = it.value as Long
                "X-GM-LABELS" -> labels.addAll(it.value as Array<String>)
                else -> {}
            }
        }

        lastModified = (response.getItem(MODSEQ::class.java) as MODSEQ).modseq

        val flagsObject = response.getItem(FLAGS::class.java)

        if (flagsObject.contains(Flags.Flag.DELETED))
            flags += "D"
        if (!flagsObject.contains(Flags.Flag.SEEN))
            flags += "U"
        if (flagsObject.contains(Flags.Flag.ANSWERED))
            flags += "R"
    }
}

fun main(args: Array<String>) {
    log("connecting")
    val db = Database("gmh.sqlite")
    val folder = connect_to_imap("david.given@gmail.com", "fjsqqmpawuvmrxmd")
    folder.open(Folder.READ_ONLY)

    val currentModSeq = db.get("modseq", "0").toLong()
    var highestModSeq = currentModSeq

    log("fetching message updates")
    folder.doCommand {
        p ->
        val responses = p.command("FETCH",
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
                                        .writeNumber(currentModSeq)
                        )
        )
        log("updating database with about " + responses.size + " new messages")
        Progressbar("Thinking", responses.size).use {
            progress ->
            for (i in 0..(responses.size - 1)) {
                progress.show(i)
                var response = responses[i]
                if (response.isOK) {
                    response.skipSpaces()
                    if (response.readByte() == SQUARE_BRACKET) {
                        when (response.readAtom()) {
                            "HIGHESTMODSEQ" ->
                                highestModSeq = response.readLong()
                        }
                    }
                }
                else if (response is FetchResponse) {
                    db.addSkeleton(MessageSkeleton(response))
                }
            }
        }
    }

    db.set("modseq", highestModSeq.toString())
    db.commit()

    log("fetching message bodies")
    val pendingBodies = db.getMessagesWithNoBody()
    Progressbar("Downloading", pendingBodies.size).use {
        progress ->
        var count = 0;
        pendingBodies.map {
            progress.show(count)
            val messages = folder.search(GmailMsgIdTerm(it))
            if (messages.size == 1) {
                val message = messages.get(0)
                val body = ByteArrayOutputStream()
                message.writeTo(body)
                db.setMessageBody(it, String(body.toByteArray(), StandardCharsets.ISO_8859_1))
                db.commit()
            }
            count++
        }
    }
}
