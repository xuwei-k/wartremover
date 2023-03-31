package wartremover

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import sbt.complete.Parser
import sbt.complete.Parser.token
import sbt.complete.FileExamples
import sbt.complete.DefaultParsers.StringBasic

/**
 * derived from sbt-scalafix
 * [[https://github.com/scalacenter/sbt-scalafix/blob/2051bf05f79befc8db66/src/main/scala/scalafix/internal/sbt/ScalafixCompletions.scala]]
 */
private[wartremover] object FileParser {
  def get(workingDirectory: Path): Parser[Path] = {
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

    token("file:") ~> token(
      StringBasic
        .examples(new AbsolutePathExamples(workingDirectory))
        .map { f =>
          toAbsolutePath(Paths.get(f), workingDirectory)
        }
        .filter(f => Files.exists(f), x => x)
    )
  }
}
