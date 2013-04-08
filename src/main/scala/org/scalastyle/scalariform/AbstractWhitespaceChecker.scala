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

package org.scalastyle.scalariform

import scalariform.parser.AstNode
import scalariform.parser.CompilationUnit
import scalariform.parser.ImportClause
import scalariform.lexer.TokenType
import scalariform.lexer.Token
import org.scalastyle.ScalastyleError
import org.scalastyle.ScalariformChecker
import org.scalastyle.PositionError
import scalariform.lexer.Tokens.DOT
import scalariform.lexer.Tokens.COMMA
import scalariform.lexer.Tokens.VARID
import scalariform.lexer.Tokens.LBRACE
import scalariform.lexer.Tokens.RBRACE
import scalariform.lexer.Tokens.LPAREN
import scalariform.lexer.Tokens.RPAREN
import scalariform.utils.Range

abstract class AbstractWhitespaceChecker(val errorKey: String) extends ScalariformChecker
{
  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (matches(left, right, charsBetweenTokens(left, right)))
    ) yield {
      PositionError(right.offset)
    }

    it.toList
  }

  protected def matches(left: Token, right: Token, distance: Int): Boolean
}

class RequiredWhitespaceChecker extends AbstractWhitespaceChecker("required.whitespace") {
  private val DefaultRegex = "^.*$"
  lazy val tokenBeforeRegex = getString("tokenBeforeRegex", DefaultRegex).r
  lazy val tokenAfterRegex = getString("tokenAfterRegex", DefaultRegex).r

  override protected def matches(left: Token, right: Token, distance: Int) =
    tokenBeforeRegex.findFirstIn(left.text) != None &&
    tokenAfterRegex.findFirstIn(right.text) != None &&
    distance == 0
}

class NoWhitespaceChecker extends AbstractWhitespaceChecker("no.whitespace") {
  private val DefaultRegex = "^.*$"
  lazy val tokenBeforeRegex = getString("tokenBeforeRegex", DefaultRegex).r
  lazy val tokenAfterRegex = getString("tokenAfterRegex", DefaultRegex).r

  override protected def matches(left: Token, right: Token, distance: Int) =
    tokenBeforeRegex.findFirstIn(left.text) != None &&
    tokenAfterRegex.findFirstIn(right.text) != None &&
    distance != 0
}

abstract class ExcludeAstNode[T <: AstNode](checker: ScalariformChecker)(implicit manifest: Manifest[T]) extends ScalariformChecker {
  val errorKey = checker.errorKey

  private def errorInRange(error: ScalastyleError, range: Range) =
    error match {
      case PositionError(pos, _) => range.contains(Range(pos, 1))
      case _ => false
    }

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val excludedRanges = VisitorHelper.getAll[T](ast.immediateChildren)(manifest) map { _.rangeOpt }

    checker.verify(ast) filter { error =>
      !excludedRanges.flatten.exists { errorInRange(error, _) }
    }
  }
}

class LeftBraceWhitespaceChecker extends ExcludeAstNode[ImportClause](
  new AbstractWhitespaceChecker("left.brace.whitespace") {
    override protected def matches(left: Token, right: Token, distance: Int) =
      distance == 0 &&
        ((left.tokenType == LBRACE && !right.isNewline) ||
        (right.tokenType == LBRACE && !left.isNewline && left.tokenType != LPAREN))
  }
)

class RightBraceWhitespaceChecker extends ExcludeAstNode[ImportClause](
  new AbstractWhitespaceChecker("right.brace.whitespace") {
    override protected def matches(left: Token, right: Token, distance: Int) =
      distance == 0 &&
        ((right.tokenType == RBRACE && !left.isNewline) ||
        (left.tokenType == RBRACE && !right.isNewline && right.tokenType != DOT && right.tokenType != COMMA && right.tokenType != RPAREN))
  }
)
