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
import com.sun.mail.imap.protocol.FLAGS
import com.sun.mail.imap.protocol.FetchResponse
import javax.mail.Flags
import javax.mail.Folder
import javax.mail.Session

val SQUARE_BRACKET: Byte = '['.toByte()

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
    var flags = mutableSetOf<String>()
    var messageId = 0L;

    init {
        response.extensionItems.map {
            when (it.key) {
                "X-GM-MSGID" -> messageId = it.value as Long
            }
        }

        val flagsObject = response.getItem(FLAGS::class.java)
        if (flagsObject.contains(Flags.Flag.ANSWERED))
            flags.add("ANSWERED")
        if (flagsObject.contains(Flags.Flag.DELETED))
            flags.add("DELETED")
        if (flagsObject.contains(Flags.Flag.DRAFT))
            flags.add("DRAFT")
        if (flagsObject.contains(Flags.Flag.FLAGGED))
            flags.add("FLAGGED")
        if (flagsObject.contains(Flags.Flag.RECENT))
            flags.add("RECENT")
        if (flagsObject.contains(Flags.Flag.SEEN))
            flags.add("SEEN")
        if (flagsObject.contains(Flags.Flag.USER))
            flags.add("USER")
        flagsObject.userFlags.map {
            flags.add(it)
        }
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
                progress.show(i)
            }
        }
    }

    db.set("modseq", highestModSeq.toString())
    db.commit()
}
