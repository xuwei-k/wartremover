package org.wartremover
package test

import org.junit.Test
import org.wartremover.warts.PublicInference

class PublicInferenceTest extends ResultAssertions {
  @Test def `Non-public fields and methods are allowed` = {
    case class X(i: Int)
    val result = WartTestTraverser(PublicInference) {
      class Y {
        private[this] val a1 = X(2)
        private val a2 = X(2)
        protected[this] val a3 = X(2)
        protected val a4 = X(2)
        private[test] val a5 = X(2)

        private[this] def b1 = X(2)
        private def b2 = X(2)
        protected[this] def b3 = X(2)
        protected def b4 = X(2)
        private[test] def b5 = X(2)
      }
    }
    assertEmpty(result)
  }

  @Test def `Public members without type ascription are disabled` = {
    val result = WartTestTraverser(PublicInference) {
      class c {
        val v = 1
        def f() = ()
      }
    }
    assertErrors(result)("Public member must have an explicit type ascription", 2)
  }

  @Test def `Inherited public members without type ascription are allowed` = {
    val result = WartTestTraverser(PublicInference) {
      trait t {
        def m(): Unit
      }
      class c extends t {
        def m() = {}
      }
    }
    assertEmpty(result)
  }

  @Test def `Public members with explicit types are enabled` = {
    val result = WartTestTraverser(PublicInference) {
      class c {
        val v: Long = 1
        def f(): Unit = ()
      }
    }
    assertEmpty(result)
  }

  @Test def `Non-public members without type ascription are enabled` = {
    val result = WartTestTraverser(PublicInference) {
      class c {
        private val v = 1
        protected def f() = ()
      }
    }
    assertEmpty(result)
  }

  @Test def `Explicitly typed multiline should work correctly` = {
    val result = WartTestTraverser(PublicInference) {
      val a:
        Int = 1
    }
    assertEmpty(result)
  }

  @Test def `Multiline should work correctly even if not explicitly typed` = {
    val result = WartTestTraverser(PublicInference) {
      val a
        = 1
    }
    assertEmpty(result)
  }

  @Test def `Multiline def should also work` = {
    val res = WartTestTraverser(PublicInference) {
      object X {
        def f(
               a: Int
             ): Int = 1
      }
    }
    assertEmpty(res)
  }

  @Test def `Even a very complex example should pass` = {
    val result = WartTestTraverser(PublicInference) {
      class A {
        val a_val = 2 // fails #1
        var a_var = 2 // fails #2
        implicit val b_val = 2 // fails #3
        implicit var b_var = 2 // fails #4
        val c_var: Int = 3
        var c_val: Int = 3

        def a() = { // fails #5
          val a_val = 2
          var a_var = 2
          println {
            val k_val = 2
            var k_var = 2
            k_val + k_var
          }
        }
      }
    }
    assertErrors(result)("Public member must have an explicit type ascription", 5)
  }

  @Test def `Members of non-public classes are ignored` = {
    val result = WartTestTraverser(PublicInference) {
      object o {
        private class c {
          val v = 1
          def f() = ()
        }
      }
    }
    assertEmpty(result)
  }

  @Test def `Case classes are enabled` = {
    val result = WartTestTraverser(PublicInference) {
      case class c(i: Int)
    }
    assertEmpty(result)
  }

  @Test def `PublicInference should work with partial functions` = {
    val result = WartTestTraverser(PublicInference) {
      Seq(1).collect { case 1 => 1 }
    }
    assertEmpty(result)
  }

  @Test def `Public inference of string, char or boolean literals is allowed` = {
    val result = WartTestTraverser(PublicInference) {
      class c {
        val s = ""
        val c = ' '
        val b = true
      }
    }
    assertEmpty(result)
  }

  @Test def `Overridden getters are allowed` = {
    val result = WartTestTraverser(PublicInference) {
      trait Foo {
        def bar: Int
      }

      object Foo {
        def apply(i: Int): Foo = new Foo {
          override val bar = i
        }
      }
    }
    assertEmpty(result)
  }

  @Test def `Explicit self types are allowed` = {
    val result = WartTestTraverser(PublicInference) {
      class c { self => }
    }
    assertEmpty(result)
  }

  @Test def `PublicInference wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(PublicInference) {
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      class c {
        val v = 1
        def f() = ()
      }
    }
    assertEmpty(result)
  }
}
