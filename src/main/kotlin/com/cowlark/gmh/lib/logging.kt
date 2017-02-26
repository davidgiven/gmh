/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.lib

fun fatal(message: String) {
  System.err.printf("error: %s\n", message)
  System.exit(1)
}

fun log(message: String) {
  System.err.printf("log: %s\n", message)
}


