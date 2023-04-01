package wartremover

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import sbt.complete.Parser
import sbt.uri
import wartremover.InspectArg.SourceFile
import wartremover.InspectArg.Type
import wartremover.InspectArg.Uri
import wartremover.InspectArg.WartName

class InspectArgsParserTest extends AnyFunSuite with EitherValues {
  private[this] val parser = InspectArgsParser.get(
    new File(".").toPath,
    Function.const(true)
  )
  private[this] def parse(input: String): Either[String, Seq[InspectArg]] =
    Parser.parse(input, parser)

  private[this] def r(input: String): Seq[InspectArg] =
    parse(input).value

  private[this] def l(input: String): String =
    parse(input).left.value

  test("failure") {
    assert(l("no_space").startsWith("Expected whitespace character"))
    assert(l(" --error").startsWith("Expected whitespace character"))
  }
  val expectToken = Seq("file:", "https://", "http://", "non-whitespace character")
  test("empty after '--error' or '--warn'") {
    val x1 = l(" --error ")
    expectToken.foreach { t =>
      x1.contains(s"Expected '${t}'")
    }
    val x2 = l(" --warn ")
    expectToken.foreach { t =>
      x2.contains(s"Expected '${t}'")
    }
  }
  test("repeat '--error'") {
    val x = l(" --error --error")
    expectToken.foreach { t =>
      x.contains(s"Expected '${t}'")
    }
  }
  test("success") {
    assert(r(" foo") == List(WartName("foo", Type.Warn)))
    assert(r(" https://example.com") == List(Uri(uri("https://example.com"), Type.Warn)))
    assert(
      r(" --error https://example.com/1 https://example.com/2") ==
        List(
          Uri(uri("https://example.com/1"), Type.Err),
          Uri(uri("https://example.com/2"), Type.Err)
        )
    )

    assert(
      r(" https://example.com/1 file://foo/bar  aaa.bbb https://example.com/2") ==
        List(
          Uri(uri("https://example.com/1"), Type.Warn),
          SourceFile(new File("/foo/bar").toPath, Type.Warn),
          WartName("aaa.bbb", Type.Warn),
          Uri(uri("https://example.com/2"), Type.Warn),
        )
    )
  }
}
