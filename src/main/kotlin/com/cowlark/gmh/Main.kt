/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import com.sun.mail.imap.IMAPFolder
import com.sun.mail.imap.IMAPStore
import javax.mail.Session

fun fatal(message: String)
{
    System.err.printf("error: %s\n", message)
    System.exit(1)
}

fun log(message: String)
{
    System.err.printf("log: %s\n", message)
}

fun connect_to_imap(): IMAPFolder
{
    val properties = System.getProperties()
    properties.setProperty("mail.store.protocol", "imaps")

    val session = Session.getDefaultInstance(properties, null)

    val store = session.getStore("imaps") as IMAPStore
    store.connect("imap.gmail.com", "david.given@gmail.com", "fjsqqmpawuvmrxmd")
    if (!store.hasCapability("CONDSTORE") || !store.hasCapability("X-GM-EXT-1"))
        fatal("this doesn't look like a Gmail server")

    val folder = store.getFolder("[Gmail]/All Mail") as IMAPFolder
    return folder
}

fun main(args: Array<String>)
{
    log("connecting")
    val db = connect_to_database()
    val folder = connect_to_imap()
}