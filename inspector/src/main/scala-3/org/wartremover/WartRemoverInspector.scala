package org.wartremover

import java.util.concurrent.atomic.AtomicInteger
import scala.quoted.Quotes
import scala.reflect.NameTransformer
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.util.control.NonFatal

class WartRemoverTastyInspector {

  /**
   * Don't use scala types because this method call by sbt plugin
   */
  def run(
    tastyFiles: Array[String],
    dependenciesClasspath: Array[String],
    errorWarts: Array[String],
    warningWarts: Array[String],
  ): Int = {
    if (tastyFiles.isEmpty) {
      println("tastyFiles is empty")
      0
    } else {
      val errorTraversers = errorWarts.flatMap(load).toList
      val warningTraversers = warningWarts.flatMap(load).toList
      if (errorTraversers.isEmpty && warningWarts.isEmpty) {
        println("warts is empty")
        0
      } else {
        runImpl(
          errorTraversers = errorTraversers,
          warningTraversers = warningTraversers,
          tastyFiles = tastyFiles.toList,
          dependenciesClasspath = dependenciesClasspath.toList,
        )
      }
    }
  }

  private[this] def load(name: String): Option[WartTraverser] = {
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
  }

  private[this] def runImpl(
    errorTraversers: List[WartTraverser],
    warningTraversers: List[WartTraverser],
    tastyFiles: List[String],
    dependenciesClasspath: List[String],
  ): Int = {
    val errorCount = new AtomicInteger()
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        def run(onlyWarning: Boolean, traverser: WartTraverser) = {
          val universe: WartUniverse.Aux[q.type] =
            new WartUniverse(onlyWarning = onlyWarning, logLevel = LogLevel.Debug) {
              override type Q = q.type
              override val quotes: q.type = q
              override def onError(msg: String, pos: Position): Unit = {
                super.onError(msg = msg, pos = pos)
                errorCount.incrementAndGet
              }
            }

          val treeTraverser = traverser.apply(universe)
          tastys.foreach { tasty =>
            val tree = tasty.ast
            treeTraverser.traverseTree(tree)(tree.symbol)
          }
        }

        errorTraversers.foreach(t => run(onlyWarning = false, traverser = t))
        warningTraversers.foreach(t => run(onlyWarning = true, traverser = t))
      }
    }
    TastyInspector.inspectAllTastyFiles(
      tastyFiles = tastyFiles,
      jars = Nil,
      dependenciesClasspath = dependenciesClasspath,
    )(inspector)

    errorCount.get
  }
}
