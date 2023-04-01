package wartremover

import sbt.file
import java.io.File
import java.io.FileNotFoundException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import sbt.complete.Parser
import sbt.complete.Parser.token
import sbt.complete.FileExamples
import sbt.complete.DefaultParsers.*
import sbt.io.GlobFilter
import java.net.URI

case class InspectArgs(
  sources: Seq[String],
  warts: Seq[Wart]
)

object InspectArgs {
  def from(values: Seq[InspectArg]): InspectArgs = {
    val sources = List.newBuilder[String]
    val warts = List.newBuilder[Wart]
    values.foreach {
      case x: InspectArg.FromSource =>
        sources ++= x.getSourceContents
      case InspectArg.WartName(value) =>
        warts += Wart.custom(value)
    }
    InspectArgs(
      sources = sources.result(),
      warts = warts.result()
    )
  }
}

private[wartremover] sealed abstract class InspectArg extends Product with Serializable

private[wartremover] object InspectArg {
  private[wartremover] sealed abstract class FromSource extends InspectArg {
    def getSourceContents: Seq[String]
  }
  def fromFile(x: File): Seq[String] = {
    if (x.isFile) {
      scala.io.Source.fromFile(x)(scala.io.Codec.UTF8).getLines().mkString("\n") :: Nil
    } else if (x.isDirectory) {
      x.listFiles(_.isFile)
        .map { f =>
          scala.io.Source.fromFile(f)(scala.io.Codec.UTF8).getLines().mkString("\n")
        }
        .toList
    } else {
      throw new FileNotFoundException(x.getAbsolutePath)
    }
  }

  final case class SourceFile(value: Path) extends FromSource {
    def getSourceContents: Seq[String] = fromFile(value.toFile)
  }
  final case class Uri(value: URI) extends FromSource {
    def getSourceContents: Seq[String] = {
      value.getScheme match {
        case null =>
          fromFile(file(value.toString))
        case _ =>
          scala.io.Source.fromURL(value.toURL)(scala.io.Codec.UTF8).getLines().mkString("\n") :: Nil
      }
    }
  }
  final case class WartName(value: String) extends InspectArg
}

/**
 * derived from sbt-scalafix
 * [[https://github.com/scalacenter/sbt-scalafix/blob/2051bf05f79befc8db66/src/main/scala/scalafix/internal/sbt/ScalafixCompletions.scala]]
 */
private[wartremover] object InspectArgsParser {
  def get(workingDirectory: Path): Parser[Seq[InspectArg]] = {
    def toAbsolutePath(path: Path, cwd: Path): Path = {
      if (path.isAbsolute) path
      else cwd.resolve(path)
    }.normalize()

    class AbsolutePathExamples(cwd: Path, prefix: String = "") extends FileExamples(cwd.toFile, prefix) {
      override def withAddedPrefix(addedPrefix: String): FileExamples = {
        val nextPrefix =
          if (addedPrefix.startsWith(".")) addedPrefix
          else s"${prefix}${addedPrefix}"
        val (b, p) = AbsolutePathCompleter.mkBase(nextPrefix, cwd)
        new AbsolutePathExamples(b, p)
      }
    }
    object AbsolutePathCompleter {
      def mkBase(prefix: String, fallback: Path): (Path, String) = {
        val path = toAbsolutePath(Paths.get(prefix), fallback)
        if (prefix.endsWith(File.separator)) {
          path -> ""
        } else if (path.getFileName != null) {
          path.getParent -> path.getFileName.toString
        } else {
          fallback -> ""
        }
      }
    }

    val f1: Parser[InspectArg] = (token("file:") ~> token(
      StringBasic
        .examples(new AbsolutePathExamples(workingDirectory))
        .map { f =>
          toAbsolutePath(Paths.get(f), workingDirectory)
        }
        .filter(f => Files.isDirectory(f) || (Files.isRegularFile(f) && f.toFile.getName.endsWith(".scala")), x => x)
        .map(InspectArg.SourceFile)
    ))

    (token(Space) ~> f1).* | f2 | f3
  }

  private[this] val f2: Parser[Seq[InspectArg]] = {
    (token(Space) ~> token(basicUri).examples("https://").map(InspectArg.Uri)).*
  }

  private[this] val f3: Parser[Seq[InspectArg]] =
    distinctParser(_root_.wartremover.Warts.all.map(_.clazz).toSet).map(_.map(InspectArg.WartName))

  private[this] def distinctParser(exs: Set[String]): Parser[Seq[String]] = {
    val base = token(Space) ~> token(NotSpace examples exs)
    base.flatMap { ex =>
      val (_, notMatching) = exs.partition(GlobFilter(ex).accept)
      distinctParser(notMatching).map { result => ex +: result }
    } ?? Nil
  }
}
