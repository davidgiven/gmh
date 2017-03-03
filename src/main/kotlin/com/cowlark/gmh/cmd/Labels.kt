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

fun LabelsCommand(globalOptions: GlobalOptions) {
  val db = Database(globalOptions.databasePath)

  if (globalOptions.rest.size != 0)
    fatal("syntax: labels")

  db.getLabels().map {
    System.out.println("" + it.labelId + ": " + it.name)
  }
}
