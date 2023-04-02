package wartremover

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import sbt.complete.Parser
import sbt.uri
import wartremover.InspectArg.FailIfWartLoadError
import wartremover.InspectWart.SourceFile
import wartremover.InspectWart.Type
import wartremover.InspectWart.Uri
import wartremover.InspectWart.WartName

class InspectArgsParserTest extends AnyFunSuite with EitherValues {
  private[this] val parser = InspectArgsParser.get(
    workingDirectory = new File(".").toPath,
    pathFilter = Function.const(true)
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
    assert(parse(" --fail-if-wart-load-error=invalid").isLeft)
  }
  test("only '--'") {
    val x = l(" -- ")
    Seq(
      "Expected '--warn'",
      "Expected '--error'",
      "Expected non-whitespace character",
    ).foreach { e =>
      assert(x.contains(e))
    }
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
    assert(r(" --fail-if-wart-load-error=true") == List(FailIfWartLoadError(true)))
    assert(r(" --fail-if-wart-load-error=false") == List(FailIfWartLoadError(false)))
    assert(r(" foo") == List(InspectArg.Wart(WartName("foo"), Type.Warn)))
    assert(r(" https://example.com") == List(InspectArg.Wart(Uri(uri("https://example.com")), Type.Warn)))
    assert(
      r(" --error https://example.com/1 https://example.com/2") ==
        List(
          InspectArg.Wart(Uri(uri("https://example.com/1")), Type.Err),
          InspectArg.Wart(Uri(uri("https://example.com/2")), Type.Err)
        )
    )

    assert(
      r(" https://example.com/1 file://foo/bar --fail-if-wart-load-error=true aaa.bbb --error https://example.com/2") ==
        List(
          InspectArg.Wart(Uri(uri("https://example.com/1")), Type.Warn),
          InspectArg.Wart(SourceFile(new File("/foo/bar").toPath), Type.Warn),
          InspectArg.FailIfWartLoadError(true),
          InspectArg.Wart(WartName("aaa.bbb"), Type.Warn),
          InspectArg.Wart(Uri(uri("https://example.com/2")), Type.Err),
        )
    )
  }
}
