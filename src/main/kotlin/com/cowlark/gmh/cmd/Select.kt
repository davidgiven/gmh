/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.cmd

import com.cowlark.gmh.GlobalOptions
import com.cowlark.gmh.lib.Database
import com.cowlark.gmh.lib.log

fun SelectCommand(globalOptions: GlobalOptions) {
  val db = Database(globalOptions.databasePath)

  if (globalOptions.rest.size == 0) {
    log("selecting \\Inbox")
    globalOptions.rest = listOf("\\Inbox")
  }

  val queries = mutableListOf<String>()

  var options = globalOptions.rest
  while (options.isNotEmpty()) {
    val option = options.first()
    options = options.drop(1)

    when {
      option.startsWith("flag:") -> {
        val flag = option.substring(5)
        queries.add(db.flagQuery(flag))
      }

      else -> {
        try {
          val labelId = option.toLong()
          queries.add(db.labelQuery(labelId))
        }
        catch (e: NumberFormatException) {
          queries.add(db.labelQuery(option))
        }
      }
    }
  }

  db.clearSelection()
  db.addToSelection(queries.joinToString(" AND "))

  val count = db.countSelectionSize()
  db.commit()

  log("selected $count messages")
}
