/* GMH
 * © 2017 David Given
 * This file is redistributable under the terms of the
 * New BSD License. Please see the COPYING file in the
 * project root for the full text.
 */

package com.cowlark.gmh.lib

import kotlin.reflect.KMutableProperty
import kotlin.reflect.jvm.javaType

@Target(AnnotationTarget.PROPERTY)
annotation class Option(
    val shortName: String = "",
    val longName: String = "",
    val help: String = ""
)

fun parseFlags(flagsObject: Any, argv: Array<String>): Array<String> {
  val flags = mutableMapOf<String, KMutableProperty<*>>()

  flagsObject.javaClass.kotlin.members
      .filterIsInstance(KMutableProperty::class.java)
      .map {
        property ->
        property.annotations
            .filterIsInstance(Option::class.java)
            .map {
              if (!it.shortName.isEmpty())
                flags.put(it.shortName, property)
              if (!it.longName.isEmpty())
                flags.put(it.longName, property)
            }
      }

  var index = 0
  while (index < argv.size) {
    val arg = argv[index]
    if (!arg.startsWith("-"))
      break

    fun handleArgument(key: String, value: String?): Int {
      val property = flags[key]
      if (property == null)
        fatal("unrecognised option: $key")

      when (property!!.returnType.javaType) {
        String::class.java -> {
          property!!.setter.call(flagsObject, value)
          return 1
        }

        else               -> {
          fatal("unsupported flag type " + property.returnType.javaType.typeName)
        }
      }

      return 0
    }

    val equalsIndex = arg.indexOf('=')
    if (equalsIndex == -1) {
      // The argument is in the next index (if any).
      index += handleArgument(arg, argv.getOrNull(index + 1))
    }
    else {
      // The argument is separated by a =.
      val key = arg.slice(0 .. equalsIndex-1)
      val value = arg.slice(equalsIndex+1 .. arg.length-1)
      handleArgument(key, value)
    }

    index++
  }

  return argv.sliceArray(index..argv.size)
}
