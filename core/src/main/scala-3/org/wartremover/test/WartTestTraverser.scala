package org.wartremover.test

import org.wartremover.WartTraverser
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TypeTree
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.quoted.PickledQuotes
import dotty.tools.dotc.core.Contexts.FreshContext

import scala.quoted.{Expr, FromExpr, Quotes, ToExpr}
import scala.quoted.runtime.impl.{ExprImpl, QuotesImpl}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import dotty.tools.dotc.interfaces.Diagnostic as DiagnosticInterface

import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer

class MyReporter extends Reporter{
  private[this] val lock = new Object
  private[this] val values = ListBuffer.empty[Diagnostic]
  override def doReport(diagnostic: Diagnostic)(using Context): Unit = {
    lock.synchronized {
      values += diagnostic
    }
  }

  def result: List[Diagnostic] = lock.synchronized{
    values.result()
  }
}

object WartTestTraverser {
  case class Result(errors: List[String], warnings: List[String])
  object Result {
    implicit val toExprInstance: ToExpr[Result] =
      new ToExpr[Result] {
        override def apply(x: Result)(using Quotes) = '{
          Result(
            errors = ${Expr(x.errors)},
            warnings = ${Expr(x.warnings)}
          )
        }
      }
  }

  inline def apply[A <: WartTraverser](inline t: A)(inline a: Any): Result = ${applyImpl[A](t = 't, expr = 'a)}

  private[this] def applyImpl[A <: WartTraverser](t: Expr[A], expr: Expr[Any])(using q1: Quotes): Expr[Result] = {
    val q2 = q1.asInstanceOf[QuotesImpl]
    val wart = Class.forName(t.show + NameTransformer.MODULE_SUFFIX_STRING)
    val reporter = new MyReporter
    import q2.ctx
    q2.ctx.asInstanceOf[FreshContext].setReporter(reporter)
    println(expr.show)
    val traverser1 = new q1.reflect.TreeTraverser {
      override def traverseTree(tree: q1.reflect.Tree)(owner: q1.reflect.Symbol): Unit = {
        tree match {
          case t: TypeTree =>
            println(t.show)
            q1.reflect.report.warning("aaa")
          case _ =>
        }
        traverseTreeChildren(tree)(owner)
      }
    }
    val tree = q1.reflect.asTerm(expr)
    traverser1.traverseTree(tree)(tree.symbol)

    val quoted = PickledQuotes.quotedExprToTree(expr)
    val traverser = new TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
        case t: TypeTree =>
        case _ =>
          traverseChildren(tree)
      }
    }
    traverser.apply((), quoted)
    val result1 = reporter.result
    println(result1)
    val warnings = result1.collect{ case a if a.level() == DiagnosticInterface.WARNING => a.message() }
    val errors = result1.collect{ case a if a.level() == DiagnosticInterface.ERROR => a.message() }
    Expr(Result(errors = errors, warnings = warnings))
  }
}
