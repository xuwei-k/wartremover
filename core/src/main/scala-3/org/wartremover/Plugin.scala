package org.wartremover

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.typer.TyperPhase

import scala.reflect.NameTransformer

class Plugin extends StandardPlugin {
  override def name = "wartremover"

  override def description = "wartremover"

  override def init(options: List[String]): List[PluginPhase] = {
    val warts = options.collect{
      case s"traverser:${name}" =>
        val clazz = Class.forName(name)
        val field = clazz.getField(NameTransformer.MODULE_INSTANCE_NAME)
        field.setAccessible(true)
        val instance = field.get(null)
        instance.asInstanceOf[WartTraverser]
    }
    (new InferenceMatchablePhase(warts)) :: Nil
  }
}

class InferenceMatchablePhase(warts: List[WartTraverser]) extends PluginPhase {
  override def phaseName = "wartremover"

  override val runsBefore = Set(TyperPhase.name)

  override def prepareForIdent(tree: Ident)(using Context): Context = {
    warts.foreach(_.prepareForIdent(tree))
    ctx
  }

  override def prepareForSelect(tree: Select)(using Context): Context = {
    warts.foreach(_.prepareForSelect(tree))
    ctx
  }

  override def prepareForThis(tree: This)(using Context): Context = {
    warts.foreach(_.prepareForThis(tree))
    ctx
  }

  override def prepareForSuper(tree: Super)(using Context): Context = {
    warts.foreach(_.prepareForSuper(tree))
    ctx
  }

  override def prepareForApply(tree: Apply)(using Context): Context = {
    warts.foreach(_.prepareForApply(tree))
    ctx
  }

  override def prepareForTypeApply(tree: TypeApply)(using Context): Context = {
    warts.foreach(_.prepareForTypeApply(tree))
    ctx
  }

  override def prepareForLiteral(tree: Literal)(using Context): Context = {
    warts.foreach(_.prepareForLiteral(tree))
    ctx
  }

  override def prepareForNew(tree: New)(using Context): Context = {
    warts.foreach(_.prepareForNew(tree))
    ctx
  }

  override def prepareForTyped(tree: Typed)(using Context): Context = {
    warts.foreach(_.prepareForTyped(tree))
    ctx
  }

  override def prepareForAssign(tree: Assign)(using Context): Context = {
    warts.foreach(_.prepareForAssign(tree))
    ctx
  }

  override def prepareForBlock(tree: Block)(using Context): Context = {
    warts.foreach(_.prepareForBlock(tree))
    ctx
  }

  override def prepareForIf(tree: If)(using Context): Context = {
    warts.foreach(_.prepareForIf(tree))
    ctx
  }

  override def prepareForClosure(tree: Closure)(using Context): Context = {
    warts.foreach(_.prepareForClosure(tree))
    ctx
  }

  override def prepareForMatch(tree: Match)(using Context): Context = {
    warts.foreach(_.prepareForMatch(tree))
    ctx
  }

  override def prepareForCaseDef(tree: CaseDef)(using Context): Context = {
    warts.foreach(_.prepareForCaseDef(tree))
    ctx
  }

  override def prepareForLabeled(tree: Labeled)(using Context): Context = {
    warts.foreach(_.prepareForLabeled(tree))
    ctx
  }

  override def prepareForReturn(tree: Return)(using Context): Context = {
    warts.foreach(_.prepareForReturn(tree))
    ctx
  }

  override def prepareForWhileDo(tree: WhileDo)(using Context): Context = {
    warts.foreach(_.prepareForWhileDo(tree))
    ctx
  }

  override def prepareForTry(tree: Try)(using Context): Context = {
    warts.foreach(_.prepareForTry(tree))
    ctx
  }

  override def prepareForSeqLiteral(tree: SeqLiteral)(using Context): Context = {
    warts.foreach(_.prepareForSeqLiteral(tree))
    ctx
  }

  override def prepareForInlined(tree: Inlined)(using Context): Context = {
    warts.foreach(_.prepareForInlined(tree))
    ctx
  }

  override def prepareForTypeTree(tree: TypeTree)(using Context): Context = {
    warts.foreach(_.prepareForTypeTree(tree))
    ctx
  }

  override def prepareForBind(tree: Bind)(using Context): Context = {
    warts.foreach(_.prepareForBind(tree))
    ctx
  }

  override def prepareForAlternative(tree: Alternative)(using Context): Context = {
    warts.foreach(_.prepareForAlternative(tree))
    ctx
  }

  override def prepareForUnApply(tree: UnApply)(using Context): Context = {
    warts.foreach(_.prepareForUnApply(tree))
    ctx
  }

  override def prepareForValDef(tree: ValDef)(using Context): Context = {
    warts.foreach(_.prepareForValDef(tree))
    ctx
  }

  override def prepareForDefDef(tree: DefDef)(using Context): Context = {
    warts.foreach(_.prepareForDefDef(tree))
    ctx
  }

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context = {
    warts.foreach(_.prepareForTypeDef(tree))
    ctx
  }

  override def prepareForTemplate(tree: Template)(using Context): Context = {
    warts.foreach(_.prepareForTemplate(tree))
    ctx
  }

  override def prepareForPackageDef(tree: PackageDef)(using Context): Context = {
    warts.foreach(_.prepareForPackageDef(tree))
    ctx
  }

  override def prepareForStats(trees: List[Tree])(using Context): Context = {
    warts.foreach(_.prepareForStats(trees))
    ctx
  }

  override def prepareForUnit(tree: Tree)(using Context): Context = {
    warts.foreach(_.prepareForUnit(tree))
    ctx
  }

  override def prepareForOther(tree: Tree)(using Context): Context = {
    warts.foreach(_.prepareForOther(tree))
    ctx
  }
}
