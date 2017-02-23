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
import com.sun.mail.imap.IMAPFolder
import javax.mail.Folder
import javax.mail.Session


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

fun main(args: Array<String>) {
    log("connecting")
    val db = Database("gmh.sqlite")
    val folder = connect_to_imap("david.given@gmail.com", "fjsqqmpawuvmrxmd")
    folder.open(Folder.READ_ONLY)

    val currentModSeq = db.get("modseq", "0").toLong()

    if (currentModSeq == 0L) {
        log("fetching initial message summaries (may be some time)")
        folder.doCommand {
            p ->
            p.command("FETCH",
                    Argument()
                            .writeString("1:10")
                            .writeArgument(
                                Argument()
                                        .writeString("X-GM-MSGID")
                                        .writeString("FLAGS")
                            )
            ).map {
                log(it.toString())
            }
        }
    }
    else {
        log("fetching message summaries")
        val result = folder.doCommand(
                IMAPFolder.ProtocolCommand {
                    p ->
                    p.simpleCommand("SEARCH", null)
                })
    }
}