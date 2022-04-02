package org.wartremover

import java.net.URLClassLoader
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.atomic.AtomicInteger
import org.wartremover.WartRemoverInspector.*
import scala.jdk.OptionConverters.*
import scala.quoted.Quotes
import scala.reflect.NameTransformer
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.util.control.NonFatal
import java.net.URL

object WartRemoverInspector {
  private final case class WartInspectResult(
    errors: Array[WartDiagnostic],
    warnings: Array[WartDiagnostic],
  ) extends InspectResult

  private final case class WartDiagnosticImpl(
    message: String,
    position: Position
  ) extends WartDiagnostic

  private final case class SourcePos(
    override val start: Int,
    override val startLine: Int,
    override val startColumn: Int,
    override val end: Int,
    override val endLine: Int,
    override val endColumn: Int,
    override val sourceFile: SourceFile,
    sourceCodeOpt: Option[String],
  ) extends Position {
    override def sourceCode: Optional[String] = sourceCodeOpt.toJava
  }

  private final case class SrcFile(
    pathOpt: Option[Path],
    override val name: String,
    override val path: String,
    contentOpt: Option[String],
  ) extends SourceFile {
    override def content: Optional[String] = contentOpt.toJava
    override def getJPath: Optional[Path] = pathOpt.toJava
  }
}

class WartRemoverInspector extends WartInspector {

  override def run(param: InspectParam): InspectResult = {
    hoge(
      tastyFiles = param.tastyFiles,
      dependenciesClasspath = param.dependenciesClasspath,
      wartClasspath = param.wartClasspath,
      errorWarts = param.errorWarts,
      warningWarts = param.warningWarts
    )
  }

  def hoge(
    tastyFiles: Array[String],
    dependenciesClasspath: Array[String],
    wartClasspath: Array[URL],
    errorWarts: Array[String],
    warningWarts: Array[String],
  ): InspectResult = {
    println("dependenciesClasspath = " + dependenciesClasspath.toList)
    println("wartClasspath = " + wartClasspath.toList)
    if (tastyFiles.isEmpty) {
      println("tastyFiles is empty")
      InspectResult.empty()
    } else {
      val classLoader = new URLClassLoader(wartClasspath, getClass.getClassLoader)
      val (errorLoadFail, errorTraversers) = errorWarts.toList.partitionMap(Plugin.loadWart(_, classLoader))
      val (warnLoadFail, warningTraversers) = warningWarts.toList.partitionMap(Plugin.loadWart(_, classLoader))
      println("load fail warts = " + (errorLoadFail ++ warnLoadFail).map(_._1).mkString(", "))
      if (errorTraversers.isEmpty && warningWarts.isEmpty) {
        println("warts is empty")
        InspectResult.empty()
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

  private[this] def runImpl(
    errorTraversers: List[WartTraverser],
    warningTraversers: List[WartTraverser],
    tastyFiles: List[String],
    dependenciesClasspath: List[String],
  ): InspectResult = {
    val errors, warnings = Array.newBuilder[WartDiagnostic]

    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        def convertPos(p: Position): org.wartremover.Position = {
          SourcePos(
            start = p.start,
            startLine = p.startLine,
            startColumn = p.startColumn,
            end = p.end,
            endLine = p.endLine,
            endColumn = p.endColumn,
            sourceFile = SrcFile(
              pathOpt = p.sourceFile.getJPath,
              name = p.sourceFile.name,
              path = p.sourceFile.path,
              contentOpt = p.sourceFile.content,
            ),
            sourceCodeOpt = p.sourceCode,
          )
        }

        def run(onlyWarning: Boolean, traverser: WartTraverser) = {
          val universe: WartUniverse.Aux[q.type] =
            new WartUniverse(onlyWarning = onlyWarning, logLevel = LogLevel.Debug) {
              override type Q = q.type
              override val quotes: q.type = q
              override def onError(msg: String, pos: Position): Unit = {
                errors += WartDiagnosticImpl(
                  message = msg,
                  position = convertPos(pos),
                )
                super.onError(msg = msg, pos = pos)
              }
              override def onWarn(msg: String, pos: Position): Unit = {
                warnings += WartDiagnosticImpl(
                  message = msg,
                  position = convertPos(pos),
                )
                super.onWarn(msg = msg, pos = pos)
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

    WartInspectResult(
      errors = errors.result(),
      warnings = warnings.result()
    )
  }
}
