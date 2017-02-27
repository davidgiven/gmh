/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import com.cowlark.gmh.cmd.SyncCommand
import com.cowlark.gmh.lib.Option
import com.cowlark.gmh.lib.fatal
import com.cowlark.gmh.lib.log
import com.cowlark.gmh.lib.parseFlags

class GlobalOptions {
  @Option(
      shortName = "-D",
      longName = "--database",
      help = "Path to the SQLite file"
  ) var databasePath = System.getenv("HOME") + "/.gmh.sqlite"
}

fun main(argv: Array<String>) {
  val globalOptions = GlobalOptions()
  val rest = parseFlags(globalOptions, argv)
  if (rest.size == 0)
    fatal("no command given --- try 'help'")

  when (rest[0]) {
    "sync" -> SyncCommand(globalOptions)
    "help" -> log("no help yet")
    else   -> fatal("unexpected parameter '" + rest[0] + "' --- try 'help'")
  }
}
