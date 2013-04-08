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

import scala.Array.canBuildFrom

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import scalariform.lexer.Token
import scalariform.parser.BlockImportExpr
import scalariform.parser.CompilationUnit
import scalariform.parser.ImportClause
import VisitorHelper.visit

abstract class AbstractImportChecker extends ScalariformChecker {
  case class ImportClauseVisit(t: ImportClause, importExpr: List[ImportClauseVisit], otherImportExprs: List[ImportClauseVisit]);
  case class Import(text: String, offset: Int);

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    init()

    val it = for (
      t <- localvisit(ast.immediateChildren);
      f <- traverse(t)
    ) yield {
      PositionError(t.t.firstToken.offset)
    }

    it.toList
  }

  protected def init(): Unit = {}

  private[this] def traverse(t: ImportClauseVisit): List[ImportClauseVisit] = {
    val l = t.importExpr.map(traverse(_)).flatten ::: t.otherImportExprs.map(traverse(_)).flatten
    if (matches(t)) t :: l else l
  }

  private[this] def imports(tokens: List[Token]): Import = {
    Import(tokens.foldLeft("")((a, b) => a + b.text), tokens.headOption map { _.offset } getOrElse 0)
  }

  private[this] def imports(t: BlockImportExpr): List[Import] = {
    val is = t.importSelectors

    val firsts = is.firstImportSelector.firstToken ::
            is.otherImportSelectors.map(_._2).map(is => is.firstToken)
    firsts.map(f => Import(imports(t.prefixExpr.tokens).text + f.text, f.offset))
  }

  protected final def imports(t: ImportClauseVisit): List[Import] = {
    t.t.importExpr match {
      case t: BlockImportExpr => imports(t)
      case _ => List(imports(t.t.importExpr.tokens))
    }
  }

  def matches(t: ImportClauseVisit): Boolean

  protected[this] def localvisit(ast: Any): List[ImportClauseVisit] = ast match {
    case t: ImportClause => List(ImportClauseVisit(t, localvisit(t.importExpr), localvisit(t.otherImportExprs)))
    case t: Any => visit(t, localvisit)
  }
}

class IllegalImportsChecker extends AbstractImportChecker {
  val errorKey = "illegal.imports"

  val DefaultIllegalImports = "sun._"
  var illegalImportsList: List[String] = _

  // sun._ => sun\.
  // sun.com.foobar => sun\.com\.foobar
  private def toMatchList(s: String) = {
    s.trim().split(" *, *").map(s => s.replaceAll("_$", "")).toList
  }

  override protected def init() = {
    illegalImportsList = toMatchList(getString("illegalImports", DefaultIllegalImports))
  }

  def matches(t: ImportClauseVisit): Boolean = {
    val list = imports(t)
    illegalImportsList.exists(ill => list.exists(_.text.startsWith(ill)))
  }
}

class UnderscoreImportChecker extends AbstractImportChecker {
  val errorKey = "underscore.import"

  def matches(t: ImportClauseVisit): Boolean = imports(t).exists(_.text.endsWith("._"))
}

class ImportGroupingChecker extends ScalariformChecker {
  val errorKey = "import.grouping"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = VisitorHelper.getAll[ImportClause](ast.immediateChildren)

    if (it.size == 0) {
      List()
    } else {
      val importTokens = it.map(ic => ic.tokens).flatten
      val (min, max) = (importTokens.head.offset, importTokens.last.offset)

      val s = ast.tokens.find(t => t.offset >= min && t.offset <= max && !t.isNewline && !(t.text == ";") && !importTokens.contains(t))

      s match {
        case Some(x) => it.dropWhile(ic => ic.firstToken.offset <= x.offset).map(ic => PositionError(ic.firstToken.offset))
        case None => List()
      }
    }
  }
}

class ImportSortingChecker extends AbstractImportChecker {
  import scala.collection.immutable.WrappedString

  val errorKey = "import.sorting"

  private val maxSeparationOfConsecutiveImports = 10
  private val camelWordRegex = "([A-Z][a-z]*|[0-9]*)".r

  def matches(t: ImportClauseVisit): Boolean = false

  def splitCamel(s: String) = {
    val (pre, post) = s.span(c => c.isLower || c == '_' )
    pre :: (camelWordRegex.findAllIn(post) filterNot { _ == "" } map { _.toLowerCase } toList)
  }

  def gt(x: Seq[String], y: Seq[String]): Boolean =
    (x.zip(y) dropWhile { case (a, b) => a == b }).headOption map { case (a, b) =>
      val aPre :: aPost = splitCamel(a)
      val bPre :: bPost = splitCamel(b)
      if (aPre == bPre) gt(aPost, bPost) else (aPre > bPre)
    } getOrElse { x.length > y.length }

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val importList = localvisit(ast.immediateChildren) flatMap imports

    if (importList.size == 0) {
      List()
    } else {
      val importListNext = importList.tail
      val importListPairs = importList zip importListNext

      for {
          (first, second) <- importListPairs if gt(first.text.split('.').toSeq, second.text.split('.').toSeq) &&
          ((second.offset - first.offset) < (first.text.length + maxSeparationOfConsecutiveImports))
      } yield {
        PositionError(second.offset)
      }
    }
  }
}
