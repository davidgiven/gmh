/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.cmd

import com.cowlark.gmh.GlobalOptions
import com.cowlark.gmh.lib.Database
import com.cowlark.gmh.lib.fatal
import javax.mail.internet.MimeUtility

fun ScanCommand(globalOptions: GlobalOptions) {
  val db = Database(globalOptions.databasePath)

  if (globalOptions.rest.size != 0)
    fatal("syntax: scan")

  db.getSelection().map {
    val message = db.getMessage(it)
    val sb = StringBuilder()
    sb.append(message.uid)
    sb.append(":")
    sb.append(message.flags)
    sb.append(":")
    sb.append(message.date)
    sb.append(":")
    if (message.downloaded) {
      if (!message.from.isEmpty())
        sb.append(message.from[0])
      sb.append(":")
      sb.append(MimeUtility.decodeText(message.subject))
    } else {
      sb.append(" (not downloaded)")
    }
    System.out.println(sb)
  }
}
