package org.wartremover

import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import scala.reflect.NameTransformer

class Plugin extends StandardPlugin {
  override def name = "wartremover"

  override def description = "wartremover"

  private def loadWart(name: String): Either[(String, Throwable), WartTraverser] = {
    try {
      val clazz = Class.forName(name + NameTransformer.MODULE_SUFFIX_STRING)
      val field = clazz.getField(NameTransformer.MODULE_INSTANCE_NAME)
      field.setAccessible(true)
      val instance = field.get(null)
      Right(instance.asInstanceOf[WartTraverser])
    } catch {
      case e: Throwable => Left((name, e))
    }
  }

  override def init(options: List[String]): List[PluginPhase] = {
    val (errors1, errorWarts) = options.collect {
      case s"traverser:${name}" =>
        loadWart(name)
    }.partitionMap(identity)
    val (errors2, warningWarts) = options.collect {
      case s"only-warn-traverser:${name}" =>
        loadWart(name)
    }.partitionMap(identity)
    val newPhase = new WartremoverPhase(
      errorWarts = errorWarts,
      warningWarts = warningWarts,
      loadFailureWarts = errors1 ++ errors2
    )
    newPhase :: Nil
  }
}