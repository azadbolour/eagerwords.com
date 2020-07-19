/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.util

import java.io.{ByteArrayOutputStream, PrintStream}

object StreamUtil {
  def runWithPrintStream[T](printStream: PrintStream)(block: => T): T = {
    val saveOut = System.out
    System.setOut(printStream)
    try {
      System.setOut(printStream)
      block
    }
    finally {
      System.setOut(saveOut)
    }
  }

  def runAndMapStdOut[R](mapper: String => String)(block: => R): (R, String) = {
    val outputStream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val printStream = new PrintStream(outputStream)
    try {
      val result: R = runWithPrintStream(printStream)(block)
      val bytes = outputStream.toByteArray
      val stringStdOut = new String(bytes)
      val mappedStdOut = mapper(stringStdOut)
      (result, mappedStdOut)
    } finally {
      printStream.close()
    }
  }

}
