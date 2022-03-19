package org.wartremover

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.quoted.PickledQuotes
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.report
import java.util.concurrent.atomic.AtomicBoolean
import scala.quoted.Quotes
import scala.util.control.NonFatal
import scala.reflect.NameTransformer

object WartremoverPhase {}

class WartremoverPhase(
  errorWarts: List[WartTraverser],
  warningWarts: List[WartTraverser],
  loadFailureWarts: List[(String, Throwable)],
  excluded: List[String],
  logLevel: LogLevel,
  initialLog: AtomicBoolean
) extends PluginPhase {
  override def phaseName = "wartremover"

  override def run(using c: Context): Unit = {
    logLevel match {
      case LogLevel.Info | LogLevel.Debug =>
        if (initialLog.getAndSet(false)) {
          if (errorWarts.nonEmpty) {
            report.echo("error warts = " + errorWarts.map(_.getClass.getName.dropRight(1)).mkString(", "))
          }
          if (warningWarts.nonEmpty) {
            report.echo("warning warts = " + warningWarts.map(_.getClass.getName.dropRight(1)).mkString(", "))
          }
          if (loadFailureWarts.nonEmpty) {
            report.warning(s"load failure warts = " + loadFailureWarts.mkString(", "))
          }
        }
      case LogLevel.Disable =>
    }
    logLevel match {
      case LogLevel.Info | LogLevel.Disable =>
      case LogLevel.Debug =>
        report.echo("run wartremover " + c.compilationUnit.source.file.toString)
    }
    super.run
  }

  override val runsAfter = Set(TyperPhase.name)

  override def prepareForUnit(tree: Tree)(using Context): Context = wartTraverse(tree)

  final def wartTraverse(tree: Tree)(using c: Context): c.type = {
    val c2 = QuotesCache.init(c.fresh)
    val q = scala.quoted.runtime.impl.QuotesImpl()(using c2)
    def runWart(w: WartTraverser, onlyWarning: Boolean): Unit = {
      val universe = new WartUniverse(q, w, onlyWarning = onlyWarning)
      val traverser = w.apply(universe)
      val t = tree.asInstanceOf[traverser.q.reflect.Tree]
      try {
        traverser.traverseTree(t)(t.symbol)
      } catch {
        case NonFatal(e) =>
          logLevel match {
            case LogLevel.Disable =>
            case LogLevel.Info | LogLevel.Debug =>
              report.warning(e.toString, tree.srcPos)
          }
      }
    }

    errorWarts.foreach(w => runWart(w = w, onlyWarning = false))
    warningWarts.foreach(w => runWart(w = w, onlyWarning = true))
    c
  }

}
