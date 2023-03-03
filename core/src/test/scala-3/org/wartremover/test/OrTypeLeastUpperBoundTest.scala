package org.wartremover
package test

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import org.wartremover.warts.OrTypeLeastUpperBound
import org.scalatest.funsuite.AnyFunSuite

class OrTypeLeastUpperBoundTest extends AnyFunSuite with ResultAssertions {

  import OrTypeLeastUpperBoundTest.*

  test("All") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.All) {
      List(IArray(1), false)
      def x1 = List(A1(1), B(2))
      List("a", true)
      val x2 = List(Right[Int, Int](1), Option("a"))
    }
    assert(result.errors.size == 4)
    assert(result.errors.forall(_.contains("least upper bound is")), result)

    val mustEmpty = WartTestTraverser(OrTypeLeastUpperBound.All) {
      for {
        x1 <- Option(5)
        x2 <- x1 match {
          case 2 =>
            Option(L(3))
          case _ =>
            Option(R("a"))
        }
      } yield ()
      Tuple2(
        _2 = 3,
        _1 = if (true) None else Some(2)
      )
      List(Right(1), Left(2))
      List(Some(77), None)
      List(false, 1.5)
    }
    assertEmpty(mustEmpty)
  }

  test("Any") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.Any) {
      List(IArray(1), false)
    }
    assertError(result)("least upper bound is `scala.Any`. `scala.IArray$package.IArray[scala.Int] | scala.Boolean`")
  }

  test("AnyRef") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.AnyRef) {
      List(A1(1), B(2))
    }
    assertError(result)(
      "least upper bound is `java.lang.Object & scala.Product & java.io.Serializable`. `org.wartremover.test.OrTypeLeastUpperBoundTest.A1 | org.wartremover.test.OrTypeLeastUpperBoundTest.B`"
    )
    val mustEmpty = WartTestTraverser(OrTypeLeastUpperBound.AnyRef) {
      List(4L, 9)
    }
    assertEmpty(mustEmpty)
  }

  test("Matchable") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.Matchable) {
      List("a", true)
    }
    assertError(result)("least upper bound is `scala.Matchable`. `java.lang.String | scala.Boolean`")
    val mustEmpty = WartTestTraverser(OrTypeLeastUpperBound.Matchable) {
      List(2L, 3.5)
    }
    assertEmpty(mustEmpty)
  }

  test("Product") {
    val mustError1 = WartTestTraverser(OrTypeLeastUpperBound.Product) {
      List(A1(1), B(2))
    }
    assertError(mustError1)(
      "least upper bound is `java.lang.Object & scala.Product & java.io.Serializable`. `org.wartremover.test.OrTypeLeastUpperBoundTest.A1 | org.wartremover.test.OrTypeLeastUpperBoundTest.B`"
    )

    val mustError2 = WartTestTraverser(OrTypeLeastUpperBound.Product) {
      List(Right[Int, Int](1), Option("a"))
    }
    assertError(mustError2)(
      "least upper bound is `java.lang.Object & scala.Product & java.io.Serializable`. `scala.util.Right[scala.Int, scala.Int] | scala.Option[java.lang.String]`"
    )

    val mustEmpty = WartTestTraverser(OrTypeLeastUpperBound.Product) {
      List(A1(1), A2(2))
    }
    assertEmpty(mustEmpty)
  }

  test("Serializable") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.Serializable) {
      List(A1(1), B(2))
    }
    assertError(result)(
      "least upper bound is `java.lang.Object & scala.Product & java.io.Serializable`. `org.wartremover.test.OrTypeLeastUpperBoundTest.A1 | org.wartremover.test.OrTypeLeastUpperBoundTest.B`"
    )
  }

  private def byte: Byte = 1
  private def char: Char = 'a'
  private def short: Short = 2
  private def int: Int = 3
  private def long: Long = 4
  private def bool: Boolean = true
  private def unit: Unit = ()
  private def float: Float = 5.0f
  private def double: Double = 6.0

  test("AnyVal.Strict") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.AnyVal.Strict) {
      List(short, int)
      List(int, long)
      List(char, int)
      List(unit, bool)
      List(float, double)
      List(int, double)
    }
    assert(result.errors.size == 6)
    assert(result.errors.forall(_.contains("least upper bound is")), result)
  }

  test("AnyVal.OnlyWeakConformance") {
    import OrTypeLeastUpperBound.AnyVal.OnlyWeakConformance
    val results = List(
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, bool)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, byte)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, short)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, int)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, long)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, float)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(unit, double)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(bool, byte)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(bool, short)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(bool, int)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(bool, long)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(bool, float)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(bool, double)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(double, long)
      },
      WartTestTraverser(OnlyWeakConformance) {
        List(float, long)
      }
    )
    results.zipWithIndex.foreach { (r, i) =>
      assert(r.errors.size == 1, (r, i))
      assert(r.errors.forall(_.contains("least upper bound is")), r)
    }
  }

  test("AnyVal.OnlyWeakConformance allow waek conformance") {
    val result = WartTestTraverser(OrTypeLeastUpperBound.AnyVal.OnlyWeakConformance) {
      List(byte, short, char, int, long)
      List(byte, short, char, int, float, double)
      List(byte, short, float)
      List(byte, short, char, int)
      List(byte, short)
    }
    assertEmpty(result)
  }
}

object OrTypeLeastUpperBoundTest {
  sealed abstract class A
  case class A1(x: Int) extends A
  case class A2(x: Int) extends A

  case class B(x: Int)

  sealed abstract class E[+A, +B] extends Product with Serializable
  final case class L[+A](a: A) extends E[A, Nothing]
  final case class R[+B](b: B) extends E[Nothing, B]
}
