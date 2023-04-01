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
}

private[wartremover] object InspectArg {
  sealed abstract class Type extends Product with Serializable
  object Type {
    case object Err extends Type
    case object Warn extends Type
  }
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

  final case class SourceFile(value: Path, tpe: Type) extends FromSource {
    def getSourceContents: Seq[String] = fromFile(value.toFile)
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
  }
  final case class WartName(value: String, tpe: Type) extends InspectArg
}

/**
 * derived from sbt-scalafix
 * [[https://github.com/scalacenter/sbt-scalafix/blob/2051bf05f79befc8db66/src/main/scala/scalafix/internal/sbt/ScalafixCompletions.scala]]
 */
private[wartremover] object InspectArgsParser {
  private[this] val typeParser: Parser[Type] = {
    val base: Parser[Type] =
      token("--warn").map(_ => Type.Warn) | token("--error").map(_ => Type.Err)

    base.?.map(_.getOrElse(Type.Warn))
  }

  def get(workingDirectory: Path): Parser[Map[Type, InspectArgs]] =
    get0(workingDirectory).map(InspectArgs.from)

  private def get0(workingDirectory: Path): Parser[Seq[InspectArg]] = {
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

    val f1: Parser[Seq[InspectArg]] = {
      val f: Parser[InspectArg] = token(Space) ~> (typeParser ~ (token("file:") ~> token(
        StringBasic
          .examples(new AbsolutePathExamples(workingDirectory))
          .map { f =>
            toAbsolutePath(Paths.get(f), workingDirectory)
          }
          .filter(f => Files.isDirectory(f) || (Files.isRegularFile(f) && f.toFile.getName.endsWith(".scala")), x => x)
      ))).map { case (tpe, x) =>
        InspectArg.SourceFile(x, tpe)
      }
      f.*
    }

    f1 | f2 | f3
  }

  private[this] val f2: Parser[Seq[InspectArg]] = {
    val examples = Seq(
      "https://",
      "https://raw.githubusercontent.com/",
      "http://",
    )
    (token(Space) ~> (typeParser ~ token(basicUri).examples(examples *)).map { case (tpe, uri) =>
      InspectArg.Uri(uri, tpe)
    }).*
  }

  private[this] val f3: Parser[Seq[InspectArg]] =
    distinctParser(_root_.wartremover.Warts.all.map(_.clazz).toSet)

  private[this] def distinctParser(exs: Set[String]): Parser[Seq[InspectArg]] = {
    val base = token(Space) ~> (typeParser ~ token(NotSpace examples exs)).map { case (tpe, wartName) =>
      InspectArg.WartName(wartName, tpe)
    }
    base.flatMap { ex =>
      val (_, notMatching) = exs.partition(GlobFilter(ex.value).accept)
      distinctParser(notMatching).map { result => ex +: result }
    } ?? Seq.empty[InspectArg]
  }
}
