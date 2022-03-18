package org.wartremover

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.quoted.{PickledQuotes, QuotesCache}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.report
import scala.quoted.Quotes
import scala.reflect.NameTransformer

object WartremoverPhase {
  
}

class WartremoverPhase(
  errorWarts: List[WartTraverser],
  warningWarts: List[WartTraverser],
  loadFailureWarts: List[(String, Throwable)],
  excluded: List[String]
) extends PluginPhase {
  override def phaseName = "wartremover"

  override def run(using c: Context): Unit = {
    report.echo("run wartremover " + c.compilationUnit.source.file.toString)
    if (false) {
      // TODO report only once
      report.echo("error warts = " + errorWarts.map(_.getClass.getName.dropRight(1)).mkString(", "))
      report.echo("warning warts = " + warningWarts.map(_.getClass.getName.dropRight(1)).mkString(", "))
      if (loadFailureWarts.nonEmpty) {
        report.warning(s"load failure warts = " + loadFailureWarts.mkString(", "))
      }
    }
    super.run
  }

  override val runsAfter = Set(TyperPhase.name)

  /*
  override def prepareForIdent(tree: Ident)(using Context): Context = wartTraverse(tree)
  override def prepareForSelect(tree: Select)(using Context): Context = wartTraverse(tree)
  override def prepareForThis(tree: This)(using Context): Context = wartTraverse(tree)
  override def prepareForSuper(tree: Super)(using Context): Context = wartTraverse(tree)
  override def prepareForApply(tree: Apply)(using Context): Context = wartTraverse(tree)
  override def prepareForTypeApply(tree: TypeApply)(using Context): Context = wartTraverse(tree)
  override def prepareForLiteral(tree: Literal)(using Context): Context = wartTraverse(tree)
  override def prepareForNew(tree: New)(using Context): Context = wartTraverse(tree)
  override def prepareForAssign(tree: Assign)(using Context): Context = wartTraverse(tree)
  override def prepareForBlock(tree: Block)(using Context): Context = wartTraverse(tree)
  override def prepareForIf(tree: If)(using Context): Context = wartTraverse(tree)
  override def prepareForClosure(tree: Closure)(using Context): Context = wartTraverse(tree)
  override def prepareForMatch(tree: Match)(using Context): Context = wartTraverse(tree)
  override def prepareForCaseDef(tree: CaseDef)(using Context): Context = wartTraverse(tree)
  override def prepareForLabeled(tree: Labeled)(using Context): Context = wartTraverse(tree)
  override def prepareForReturn(tree: Return)(using Context): Context = wartTraverse(tree)
  override def prepareForWhileDo(tree: WhileDo)(using Context): Context = wartTraverse(tree)
  override def prepareForTry(tree: Try)(using Context): Context = wartTraverse(tree)
  override def prepareForSeqLiteral(tree: SeqLiteral)(using Context): Context = wartTraverse(tree)
  override def prepareForInlined(tree: Inlined)(using Context): Context = wartTraverse(tree)
  override def prepareForTypeTree(tree: TypeTree)(using Context): Context = wartTraverse(tree)
  override def prepareForBind(tree: Bind)(using Context): Context = wartTraverse(tree)
  override def prepareForAlternative(tree: Alternative)(using Context): Context = wartTraverse(tree)
  override def prepareForUnApply(tree: UnApply)(using Context): Context = wartTraverse(tree)
  override def prepareForValDef(tree: ValDef)(using c: Context): Context = {
    if (tree.isEmpty) {
      c
    } else {
      wartTraverse(tree)
    }
  }
  override def prepareForDefDef(tree: DefDef)(using Context): Context = wartTraverse(tree)
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context = wartTraverse(tree)
  override def prepareForPackageDef(tree: PackageDef)(using Context): Context = wartTraverse(tree)
  override def prepareForOther(tree: Tree)(using Context): Context = wartTraverse(tree)
  */

  // TODO
  //  override def prepareForStats(trees: List[Tree])(using Context): Context = wartTraverse(tree)

  // always MatchError ???
  // override def prepareForTemplate(tree: Template)(using Context): Context = wartTraverse(tree)
  // override def prepareForTyped(tree: Typed)(using Context): Context = wartTraverse(tree)

  override def prepareForUnit(tree: Tree)(using Context): Context = wartTraverse(tree)

  final def wartTraverse(tree: Tree)(using c: Context): c.type = {
    val c2 = QuotesCache.init(c.fresh)
    val q = scala.quoted.runtime.impl.QuotesImpl()(using c2)
    //println(warts.size)
    //println("traverse " + tree.toString)

    def runWart(w: WartTraverser, onlyWarning: Boolean): Unit = {
      val universe = new WartUniverse(q, w, onlyWarning = onlyWarning)
      val traverser = w.apply(universe)
      val t = tree.asInstanceOf[traverser.q.reflect.Tree]
      traverser.traverseTree(t)(t.symbol)
    }

    errorWarts.foreach(w => runWart(w = w, onlyWarning = false))
    warningWarts.foreach(w => runWart(w = w, onlyWarning = true))
    c
  }

}
