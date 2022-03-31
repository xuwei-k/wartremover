import org.wartremover.LogLevel
import org.wartremover.WartTraverser
import org.wartremover.WartUniverse
import scala.quoted.Quotes
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector

class WartRemoverTastyInspector {

  def run(
    tastyFiles: Array[String],
    dependenciesClasspath: Array[String],
    warts: Array[String]
  ): Unit = {
    runImpl(
      traverser = warts.map(name => Class.forName(name + "$").asInstanceOf[WartTraverser]).reduceLeft(_ compose _),
      tastyFiles = tastyFiles.toList,
      dependenciesClasspath = dependenciesClasspath.toList,
    )
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
        val count = tastys.size
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
