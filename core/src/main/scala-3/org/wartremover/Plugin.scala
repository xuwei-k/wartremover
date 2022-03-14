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

}
