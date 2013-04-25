// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.file

import org.scalastyle.FileChecker
import org.scalastyle.LineError
import org.scalastyle.Line
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class IndentationChecker extends FileChecker {
  val DefaultTabSize = 2
  val errorKey = "indentation"

  private def spaces(column: Int, tabSize: Int): String = {
    val m = column % tabSize
    val length = if (m == 0) {
      tabSize
    } else {
      tabSize - m
    }

    String.format("%" + length + "s", " ")
  }

  private def replaceTabs(s: String, tabSize: Int): String = {
    val sb = new StringBuilder(s)
    val len = sb.length
    var i = 0;

    while (i < len) {
      if (sb.charAt(i) == '\t') {
        sb.replace(i, i + 1, spaces(i, tabSize))
      }
      i += 1
    }

    if (sb.endsWith("\r")) {
      sb.setLength(sb.length-1);
    }

    sb.toString
  }

  private def isExempt(line1: Line, line2: Line) =
    line1.text.matches(""".*class.*\([^\)]*""")

  def verify(lines: Lines): List[ScalastyleError] = {
    val tabSize = getInt("tabSize", DefaultTabSize)

    val lineIndents = for {
      (line, index) <- lines.lines.zipWithIndex
      if line.text.matches(""".*\S.*""") // ignore blank lines
      val normalizedLine = replaceTabs(line.text, tabSize)
      val whitespaceLength = normalizedLine.prefixLength { _ == ' ' }
      val multiLineComment = normalizedLine(whitespaceLength) == '*'
      val indent = (whitespaceLength - (if (multiLineComment) 1 else 0))
    } yield (index, indent)

    val tabstopErrors = for {
      (index, indent) <- lineIndents
      if indent % tabSize != 0
    } yield {
      LineError(index + 1)
    }

    val extraIndentErrors = for {
      Array((index1, indent1), (index2, indent2)) <- lineIndents.sliding(2)
      if (indent2 - indent1) > tabSize
      if !isExempt(lines.lines(index1), lines.lines(index2))
    } yield {
      LineError(index2 + 1)
    }

    (tabstopErrors ++ extraIndentErrors).toList
  }
}
