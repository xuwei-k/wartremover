package org.wartremover

import scala.quoted.Quotes
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.reflect.NameTransformer
import scala.util.control.NonFatal

class WartRemoverTastyInspector {

  def run(
    tastyFiles: Array[String],
    dependenciesClasspath: Array[String],
    warts: Array[String]
  ): Unit = {
    if (tastyFiles.isEmpty) {
      println("tastyFiles is empty")
    } else if(warts.isEmpty) {
      println("warts is empty")
    } else {
      runImpl(
        traverser = warts.flatMap{ name => 
          try {
            val clazz = Class.forName(name + NameTransformer.MODULE_SUFFIX_STRING)
            val field = clazz.getField(NameTransformer.MODULE_INSTANCE_NAME)
            val instance = field.get(null)
            Some(instance.asInstanceOf[WartTraverser])
          } catch {
            case NonFatal(e) =>
              println(s"failed load warts $name $e")
              None
          }
        }.reduceLeft(_ compose _),
        tastyFiles = tastyFiles.toList,
        dependenciesClasspath = dependenciesClasspath.toList,
      )
    }
  }

  private[this] def runImpl(
    traverser: WartTraverser,
    tastyFiles: List[String],
    dependenciesClasspath: List[String],
  ): Unit = {
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        val universe: WartUniverse.Aux[q.type] =
          new WartUniverse(onlyWarning = false, logLevel = LogLevel.Debug) {
            override type Q = q.type
            override val quotes: q.type = q
          }
        val treeTraverser = traverser.apply(universe)
        tastys.foreach { tasty =>
          val tree = tasty.ast
          treeTraverser.traverseTree(tree)(tree.symbol)
        }
      }
    }
    TastyInspector.inspectAllTastyFiles(
      tastyFiles = tastyFiles,
      jars = Nil,
      dependenciesClasspath = dependenciesClasspath,
    )(inspector)
  }
}
