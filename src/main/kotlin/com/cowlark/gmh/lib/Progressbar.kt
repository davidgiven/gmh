/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.lib

import java.io.Closeable

private val PROGRESS_BAR_CHARS = arrayOf(" ", "▏","▎","▍","▌","▋","▊","▉","█")
private val PROGRESS_BAR_LEN = 43
private val WIDTH = (System.getenv("COLUMNS") ?: "80").toInt()

class Progressbar(val message: String, val max: Int) : Closeable {
  var startTime = System.currentTimeMillis()
  var lastUpdate = 0L

  override fun close() {
    lastUpdate = 0L
    show(max)
    clear()
  }

  private fun asTime(duration_secs: Double): String {
    if (duration_secs.isInfinite())
      return "(unknown)"
    else {
      val seconds = (duration_secs % 60.0).toInt()
      val minutes = (duration_secs / 60.0).toInt()
      return String.format("%dm:%02ds", minutes, seconds)
    }
  }

  fun show(progress: Int) {
    val now = System.currentTimeMillis()
    if ((now - lastUpdate) > 250) {
      val sb = StringBuilder()

      sb.append("\r")
      sb.append(" ".repeat(WIDTH -1))
      sb.append("\r")

      if (progress == max)
        sb.append(PROGRESS_BAR_CHARS[PROGRESS_BAR_CHARS.size - 1].repeat(PROGRESS_BAR_LEN))
      else {
        val scaledProgress = (progress * PROGRESS_BAR_LEN * PROGRESS_BAR_CHARS.size) / max
        val numfilled = scaledProgress / PROGRESS_BAR_CHARS.size
        val subchar = scaledProgress % PROGRESS_BAR_CHARS.size
        var numempty = PROGRESS_BAR_LEN - numfilled - 1

        sb.append(PROGRESS_BAR_CHARS[PROGRESS_BAR_CHARS.size - 1].repeat(numfilled))
        if (subchar >= 0)
          sb.append(PROGRESS_BAR_CHARS[subchar])
        sb.append(PROGRESS_BAR_CHARS[0].repeat(numempty))
      }

      val persec = progress.toDouble () / (now - startTime).toDouble() * 1000.0
      val timeSoFar = (now - startTime).toDouble() / 1000.0
      val estimatedTotal = max.toDouble() / persec

      sb.append(" $progress/$max; " + persec.toInt() + "/sec, " + asTime(estimatedTotal - timeSoFar) + " left")
      System.err.print(sb.toString())
      System.err.flush()
      lastUpdate = now
    }
  }

  fun clear() {
    System.err.print("\n")
  }
}
