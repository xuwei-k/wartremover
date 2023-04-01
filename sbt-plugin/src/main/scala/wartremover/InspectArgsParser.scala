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
import wartremover.InspectArg.Type
import java.net.URI

case class InspectArgs(
  sources: Seq[String],
  warts: Seq[Wart]
)

object InspectArgs {
  val empty: InspectArgs = InspectArgs(Nil, Nil)
  def from(values: Seq[InspectArg]): Map[Type, InspectArgs] = {
    val warnSources = List.newBuilder[String]
    val warnWarts = List.newBuilder[Wart]
    val errorSources = List.newBuilder[String]
    val errorWarts = List.newBuilder[Wart]
    values.foreach {
      case x: InspectArg.FromSource =>
        x.tpe match {
          case Type.Warn =>
            warnSources ++= x.getSourceContents
          case Type.Err =>
            errorSources ++= x.getSourceContents
        }
      case x: InspectArg.WartName =>
        x.tpe match {
          case Type.Warn =>
            warnWarts += Wart.custom(x.value)
          case Type.Err =>
            errorWarts += Wart.custom(x.value)
        }
    }
    Map(
      Type.Warn -> InspectArgs(
        sources = warnSources.result(),
        warts = warnWarts.result()
      ),
      Type.Err -> InspectArgs(
        sources = errorSources.result(),
        warts = errorWarts.result()
      ),
    )
  }
}

private[wartremover] sealed abstract class InspectArg extends Product with Serializable {
  def tpe: Type
  def withType(tpe: Type): InspectArg
}

private[wartremover] object InspectArg {
  sealed abstract class Type extends Product with Serializable
  object Type {
    case object Err extends Type
    case object Warn extends Type
    case object Empty extends Type
  }
  private[wartremover] sealed abstract class FromSource extends InspectArg {
    def getSourceContents: Seq[String]
  }
  private def fromFile(x: File): Seq[String] = {
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

  final case class SourceFile(value: Path, tpe: Type) extends FromSource {
    def getSourceContents: Seq[String] = fromFile(value.toFile)

    def withType(tpe: Type): InspectArg = copy(tpe = tpe)
  }
  final case class Uri(value: URI, tpe: Type) extends FromSource {
    def getSourceContents: Seq[String] = {
      value.getScheme match {
        case null =>
          fromFile(file(value.toString))
        case _ =>
          scala.io.Source.fromURL(value.toURL)(scala.io.Codec.UTF8).getLines().mkString("\n") :: Nil
      }
    }
    def withType(tpe: Type): InspectArg = copy(tpe = tpe)
  }
  final case class WartName(value: String, tpe: Type) extends InspectArg {
    def withType(tpe: Type): InspectArg = copy(tpe = tpe)
  }
}

/**
 * derived from sbt-scalafix
 * [[https://github.com/scalacenter/sbt-scalafix/blob/2051bf05f79befc8db66/src/main/scala/scalafix/internal/sbt/ScalafixCompletions.scala]]
 */
private[wartremover] object InspectArgsParser {
  private[this] val typeParser: Parser[Type] = {
    val base: Parser[Type] =
      Space ~> (token("--warn").map(_ => Type.Warn) | token("--error").map(_ => Type.Err))

    base.?.map(_.getOrElse(Type.Warn))
  }

  private implicit class ParserOps(private val self: Parser[String]) extends AnyVal {
    def doNotParserTypeArgs: Parser[String] =
      self.filter(!_.startsWith("--"), x => x)
  }

  def get(workingDirectory: Path): Parser[Seq[InspectArg]] =
    get(workingDirectory, p => Files.exists(p))
  def get(workingDirectory: Path, pathFilter: Path => Boolean): Parser[Seq[InspectArg]] = {
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

    val f1: Parser[InspectArg] = token(Space) ~> (token("file:") ~> token(
      StringBasic.doNotParserTypeArgs
        .examples(new AbsolutePathExamples(workingDirectory))
        .map { f =>
          toAbsolutePath(Paths.get(f), workingDirectory)
        }
        .filter(pathFilter, x => x)
    )).map { x =>
      InspectArg.SourceFile(x, Type.Empty)
    }

    (typeParser ~ (f1 | f2 | f3).+).+.map {
      _.flatMap { case (tpe, values) =>
        values.map(_.withType(tpe))
      }
    }
  }

  private[this] val f2: Parser[InspectArg] = {
    val examples = Seq(
      "https://",
      "https://raw.githubusercontent.com/",
      "http://",
    )
    val head = examples.map(token(_)).reduceLeft(_ | _)
    token(Space) ~> (head ~ token(URIClass)).examples(examples *).map { case (p, uri) =>
      InspectArg.Uri(new URI(s"${p}${uri}"), Type.Empty)
    }
  }

  private[this] val f3: Parser[InspectArg] =
    token(Space) ~> token(NotSpace examples _root_.wartremover.Warts.all.map(_.clazz).toSet).doNotParserTypeArgs.map {
      wartName =>
        InspectArg.WartName(wartName, Type.Empty)
    }

}
