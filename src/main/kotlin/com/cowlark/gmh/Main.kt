/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import com.cowlark.gmh.cmd.SelectCommand
import com.cowlark.gmh.cmd.SyncCommand
import com.cowlark.gmh.lib.HasOptions
import com.cowlark.gmh.lib.Option
import com.cowlark.gmh.lib.fatal
import com.cowlark.gmh.lib.log
import com.cowlark.gmh.lib.parseFlags

class GlobalOptions : HasOptions() {
  @Option(
      shortName = "-D",
      longName = "--database",
      help = "Path to the SQLite file"
  ) var databasePath = System.getenv("HOME") + "/.gmh.sqlite"
}

fun main(argv: Array<String>) {
  val globalOptions = GlobalOptions()
  parseFlags(globalOptions, argv)

  val command = globalOptions.rest.getOrElse(0, {
    fatal("no command given --- try 'help'")
  })
  globalOptions.rest = globalOptions.rest.sliceArray(1 .. globalOptions.rest.size-1)

  when (command) {
    "sync"   -> SyncCommand(globalOptions)
    "select" -> SelectCommand(globalOptions)
    "help"   -> log("no help yet")
    else     -> fatal("unexpected parameter '$command' --- try 'help'")
  }
}
