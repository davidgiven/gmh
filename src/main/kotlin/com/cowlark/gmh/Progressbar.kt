/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh

import java.io.Closeable

class Progressbar(val message: String, val max: Int): Closeable {
    var lastUpdate = 0L

    override fun close() {
        clear()
    }

    fun show(progress: Int) {
        val now = System.currentTimeMillis()
        if ((now - lastUpdate) > 250) {
            System.err.print("\r$message: $progress/$max")
            System.err.flush()
            lastUpdate = now
        }
    }

    fun clear() {
        System.err.print("\n")
    }
}
