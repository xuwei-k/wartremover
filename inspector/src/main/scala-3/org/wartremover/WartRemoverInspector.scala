package org.wartremover

import argonaut.DecodeJson
import argonaut.EncodeJson
import java.net.URLClassLoader
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.atomic.AtomicInteger
import scala.quoted.Quotes
import scala.reflect.NameTransformer
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.util.control.NonFatal
import java.net.URL

class WartRemoverInspector {
  private[this] implicit val sourceFileInstance: EncodeJson[SourceFile] =
    EncodeJson.derive[SourceFile]
  private[this] implicit val positionInstance: EncodeJson[Position] =
    EncodeJson.derive[Position]
  private[this] implicit val diagnosticInstance: EncodeJson[Diagnostic] =
    EncodeJson.derive[Diagnostic]
  private[this] implicit val inspectResultInstance: EncodeJson[InspectResult] =
    EncodeJson.derive[InspectResult]
  private[this] implicit val inspectParamInstance: DecodeJson[InspectParam] =
    DecodeJson.derive[InspectParam]

  def run(json: String): String = {
    val param = argonaut.JsonParser
      .parse(json)
      .left
      .map(sys.error(_))
      .merge
      .as[InspectParam]
      .fold(
        (e, _) => sys.error(e),
        identity
      )
    val result = run(param)
    implicitly[EncodeJson[InspectResult]].encode(result).spaces2
  }

  private[this] def run(param: InspectParam): InspectResult = {
    println("dependenciesClasspath = " + param.dependenciesClasspath.toList)
    println("wartClasspath = " + param.wartClasspath.toList)
    if (param.tastyFiles.isEmpty) {
      println("tastyFiles is empty")
      InspectResult.empty
    } else {
      val classLoader = new URLClassLoader(param.wartClasspath.map(new URL(_)).toArray, getClass.getClassLoader)
      val (errorLoadFail, errorTraversers) = param.errorWarts.toList.partitionMap(Plugin.loadWart(_, classLoader))
      val (warnLoadFail, warningTraversers) = param.warningWarts.toList.partitionMap(Plugin.loadWart(_, classLoader))
      val loadFailed = errorLoadFail ++ warnLoadFail
      if (loadFailed.nonEmpty) {
        if (param.failIfWartLoadError) {
          throw loadFailed.head._2
        } else {
          println("load fail warts = " + loadFailed.map(_._1).mkString(", "))
        }
      }
      if (errorTraversers.isEmpty && warningTraversers.isEmpty) {
        println("warts is empty")
        InspectResult.empty
      } else {
        runImpl(
          errorTraversers = errorTraversers,
          warningTraversers = warningTraversers,
          tastyFiles = param.tastyFiles.toList,
          dependenciesClasspath = param.dependenciesClasspath.toList,
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
    val errors, warnings = List.newBuilder[Diagnostic]

    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        def convertPos(p: Position): org.wartremover.Position = {
          org.wartremover.Position(
            start = p.start,
            startLine = p.startLine,
            startColumn = p.startColumn,
            end = p.end,
            endLine = p.endLine,
            endColumn = p.endColumn,
            sourceFile = org.wartremover.SourceFile(
              name = p.sourceFile.name,
              path = p.sourceFile.path,
            ),
            sourceCode = p.sourceCode,
          )
        }

        def run(onlyWarning: Boolean, traverser: WartTraverser) = {
          val universe: WartUniverse.Aux[q.type] =
            new WartUniverse(onlyWarning = onlyWarning, logLevel = LogLevel.Debug) {
              override type Q = q.type
              override val quotes: q.type = q
              override def onError(msg: String, pos: Position): Unit = {
                errors += Diagnostic(
                  message = msg,
                  position = convertPos(pos),
                )
                super.onError(msg = msg, pos = pos)
              }
              override def onWarn(msg: String, pos: Position): Unit = {
                warnings += Diagnostic(
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

    InspectResult(
      errors = errors.result(),
      warnings = warnings.result()
    )
  }
}
