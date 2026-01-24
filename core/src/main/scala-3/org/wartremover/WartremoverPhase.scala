package org.wartremover

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.report
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.LongAdder
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

object WartremoverPhase {
  private[wartremover] def defaultWartremoverPhaseName: String = "wartremover"
  private[wartremover] def defaultRunsAfter: String = TyperPhase.name
}

class WartremoverPhase(
  errorWarts: List[WartTraverser],
  warningWarts: List[WartTraverser],
  loadFailureWarts: List[(String, Throwable)],
  excluded: List[String],
  logLevel: LogLevel,
  initialLog: AtomicBoolean,
  override val runsAfter: Set[String],
  override val phaseName: String,
  profile: Option[String]
) extends PluginPhase {
  private val profileResult: TrieMap[String, LongAdder] = TrieMap.empty[String, LongAdder]

  def this(
    errorWarts: List[WartTraverser],
    warningWarts: List[WartTraverser],
    loadFailureWarts: List[(String, Throwable)],
    excluded: List[String],
    logLevel: LogLevel,
    initialLog: AtomicBoolean,
    runsAfter: Set[String],
    phaseName: String,
  ) = this(
    errorWarts = errorWarts,
    warningWarts = warningWarts,
    loadFailureWarts = loadFailureWarts,
    excluded = excluded,
    logLevel = logLevel,
    initialLog = initialLog,
    runsAfter = runsAfter,
    phaseName = phaseName,
    profile = None
  )

  def this(
    errorWarts: List[WartTraverser],
    warningWarts: List[WartTraverser],
    loadFailureWarts: List[(String, Throwable)],
    excluded: List[String],
    logLevel: LogLevel,
    initialLog: AtomicBoolean,
  ) = this(
    errorWarts = errorWarts,
    warningWarts = warningWarts,
    loadFailureWarts = loadFailureWarts,
    excluded = excluded,
    logLevel = logLevel,
    initialLog = initialLog,
    runsAfter = Set(WartremoverPhase.defaultRunsAfter),
    phaseName = WartremoverPhase.defaultWartremoverPhaseName,
    profile = None
  )

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    try {
      super.runOn(units)
    } finally {
      Profile.report(profileResult, profile, logLevel)
    }
  }

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
          if (excluded.nonEmpty) {
            report.echo("excluded = " + excluded.mkString(", "))
          }
        }
      case LogLevel.Disable =>
    }
    val skip = excluded.exists(c.source.file.absolute.path.startsWith)
    logLevel match {
      case LogLevel.Info | LogLevel.Disable =>
      case LogLevel.Debug =>
        if (skip) {
          report.echo("skip wartremover " + c.compilationUnit.source.file.toString)
        } else {
          report.echo("run wartremover " + c.compilationUnit.source.file.toString)
        }
    }
    if (!skip) {
      super.run
    }
  }

  private def runWartremover(
    tree: tpd.Tree,
    q: scala.quoted.Quotes,
    universe: WartUniverse.Aux[q.type],
    warts: List[WartTraverser]
  )(using c: Context): Unit = {
    val (fusionTraversers, normalTraversers) = warts.partitionMap { w =>
      w.apply(universe) match {
        case x: universe.FusionTraverser =>
          Left(w -> x)
        case x =>
          Right(w -> x)
      }
    }

    normalTraversers.foreach { (w, traverser) =>
      val t = tree.asInstanceOf[traverser.q.reflect.Tree]
      withProfile(w.fullName) {
        withLog(tree) {
          traverser.traverseTree(t)(t.symbol)
        }
      }
    }

    val fusion = universe.FusionTraversers(fusionTraversers.map(_._2))
    val fusionName = fusionTraversers.map(_._1.fullName).mkString("-")
    withProfile(fusionName) {
      withLog(tree) {
        val t = tree.asInstanceOf[universe.quotes.reflect.Tree]
        fusion.traverseTree(t)(t.symbol)
      }
    }
  }

  override def prepareForUnit(tree: tpd.Tree)(using c: Context): Context = {
    val c2 = QuotesCache.init(c.fresh)
    val q = scala.quoted.runtime.impl.QuotesImpl()(using c2)
    runWartremover(
      tree,
      q,
      WartUniverse(
        onlyWarning = false,
        logLevel = logLevel,
        quotes = q,
      ),
      errorWarts,
    )
    runWartremover(
      tree,
      q,
      WartUniverse(
        onlyWarning = true,
        logLevel = logLevel,
        quotes = q,
      ),
      warningWarts
    )

    c
  }

  private inline def withLog[A](tree: tpd.Tree)(inline a: => A)(using Context): Unit = {
    try {
      a
    } catch {
      case NonFatal(e) =>
        logLevel match {
          case LogLevel.Disable =>
          case LogLevel.Info | LogLevel.Debug =>
            report.warning(e.toString, tree.srcPos)
        }
    }
  }

  private inline def withProfile[A](inline profileName: String)(inline a: => A): A = {
    val start = System.nanoTime()
    try {
      a
    } finally {
      if (profile.isDefined) {
        val adder = profileResult.getOrElseUpdate(profileName, new LongAdder)
        adder.add(System.nanoTime() - start)
      }
    }
  }
}
