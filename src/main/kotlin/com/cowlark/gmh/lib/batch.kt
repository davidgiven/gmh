/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.lib

fun <T> Sequence<T>.batch(n: Int): Sequence<List<T>> {
  return BatchingSequence(this, n)
}

private class BatchingSequence<T>(val source: Sequence<T>, val batchSize: Int) : Sequence<List<T>> {
  override fun iterator(): Iterator<List<T>> = object : AbstractIterator<List<T>>() {
    val iterate = if (batchSize > 0) source.iterator() else emptyList<T>().iterator()
    override fun computeNext() {
      if (iterate.hasNext()) setNext(iterate.asSequence().take(batchSize).toList())
      else done()
    }
  }
}