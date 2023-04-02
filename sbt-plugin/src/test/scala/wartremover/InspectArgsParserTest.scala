package wartremover

import org.scalactic.source.Position
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
    workingDirectory = new File("core/src/main/scala-2/").toPath.toAbsolutePath,
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
  test("completions") {
    def f(input: String, expected: Set[String])(implicit p: Position) = {
      val actual = input.foldLeft(parser)(_ derive _).completions(0).get.map(_.display)
      if (actual.size != expected.size) {
        actual.toList.sorted.map("\"" + _ + "\",").foreach(println)
      }
      assert(actual == expected)
    }

    f(
      " htt",
      Set(
        "p://",
        "ps://",
        "ps://raw.githubusercontent.com/",
        "p:// ",
        "ps:// ",
        "ps://raw.githubusercontent.com/ ",
        "p://  ",
        "ps://  ",
        "ps://raw.githubusercontent.com/  ",
      )
    )

    f(
      " org.wartremover",
      Set(
        "org.wartremover.warts.Any",
        "org.wartremover.warts.AnyVal",
        "org.wartremover.warts.ArrayEquals",
        "org.wartremover.warts.AsInstanceOf",
        "org.wartremover.warts.AutoUnboxing",
        "org.wartremover.warts.CollectHeadOption",
        "org.wartremover.warts.DefaultArguments",
        "org.wartremover.warts.DropTakeToSlice",
        "org.wartremover.warts.EitherProjectionPartial",
        "org.wartremover.warts.Enumeration",
        "org.wartremover.warts.Equals",
        "org.wartremover.warts.ExplicitImplicitTypes",
        "org.wartremover.warts.FilterEmpty",
        "org.wartremover.warts.FilterHeadOption",
        "org.wartremover.warts.FilterSize",
        "org.wartremover.warts.FinalCaseClass",
        "org.wartremover.warts.FinalVal",
        "org.wartremover.warts.ForeachEntry",
        "org.wartremover.warts.GetGetOrElse",
        "org.wartremover.warts.GetOrElseNull",
        "org.wartremover.warts.GlobalExecutionContext",
        "org.wartremover.warts.ImplicitConversion",
        "org.wartremover.warts.ImplicitParameter",
        "org.wartremover.warts.IsInstanceOf",
        "org.wartremover.warts.IterableOps",
        "org.wartremover.warts.JavaConversions",
        "org.wartremover.warts.JavaNetURLConstructors",
        "org.wartremover.warts.JavaSerializable",
        "org.wartremover.warts.LeakingSealed",
        "org.wartremover.warts.ListAppend",
        "org.wartremover.warts.ListUnapply",
        "org.wartremover.warts.ListUnapplySeq",
        "org.wartremover.warts.MapUnit",
        "org.wartremover.warts.MutableDataStructures",
        "org.wartremover.warts.NoNeedImport",
        "org.wartremover.warts.NonUnitStatements",
        "org.wartremover.warts.Nothing",
        "org.wartremover.warts.Null",
        "org.wartremover.warts.Option2Iterable",
        "org.wartremover.warts.OptionPartial",
        "org.wartremover.warts.Overloading",
        "org.wartremover.warts.PlatformDefault",
        "org.wartremover.warts.Product",
        "org.wartremover.warts.PublicInference",
        "org.wartremover.warts.Recursion",
        "org.wartremover.warts.RedundantConversions",
        "org.wartremover.warts.Return",
        "org.wartremover.warts.ReverseFind",
        "org.wartremover.warts.ReverseIterator",
        "org.wartremover.warts.ReverseTakeReverse",
        "org.wartremover.warts.ScalaApp",
        "org.wartremover.warts.Serializable",
        "org.wartremover.warts.SizeIs",
        "org.wartremover.warts.SizeToLength",
        "org.wartremover.warts.SortFilter",
        "org.wartremover.warts.SortedMaxMin",
        "org.wartremover.warts.SortedMaxMinOption",
        "org.wartremover.warts.StringPlusAny",
        "org.wartremover.warts.ThreadSleep",
        "org.wartremover.warts.Throw",
        "org.wartremover.warts.ToString",
        "org.wartremover.warts.TripleQuestionMark",
        "org.wartremover.warts.TryPartial",
        "org.wartremover.warts.Var",
        "org.wartremover.warts.While",
      )
    )

    f(" --e", Set("--error"))

    f(" --w", Set("--warn"))

    f(" --f", Set("--fail-if-wart-load-error="))
    f(
      " --fail-if-wart-load-error=",
      Set(
        "true",
        "false",
        "true ",
        "false ",
      )
    )

    f(
      " file:",
      Set(
        "org",
        "org/wartremover",
        "org/wartremover/Main.scala",
        "org/wartremover/Plugin.scala",
        "org/wartremover/WartTraverser.scala",
        "org/wartremover/test",
        "org/wartremover/test/WartTestTraverser.scala",
        "org/wartremover/warts",
        "org/wartremover/warts/Any.scala",
        "org/wartremover/warts/ExplicitImplicitTypes.scala",
        "org/wartremover/warts/FilterEmpty.scala",
        "org/wartremover/warts/FinalVal.scala",
        "org/wartremover/warts/GetOrElseNull.scala",
        "org/wartremover/warts/ImplicitParameter.scala",
        "org/wartremover/warts/IsInstanceOf.scala",
        "org/wartremover/warts/IterableOps.scala",
        "org/wartremover/warts/JavaSerializable.scala",
        "org/wartremover/warts/LeakingSealed.scala",
        "org/wartremover/warts/Nothing.scala",
        "org/wartremover/warts/PublicInference.scala",
        "org/wartremover/warts/Return.scala",
        "org/wartremover/warts/Serializable.scala",
        "org/wartremover/warts/Throw.scala",
        "org/wartremover/warts/TripleQuestionMark.scala",
        "org/wartremover/warts/TryPartial.scala",
      )
    )
  }
}
