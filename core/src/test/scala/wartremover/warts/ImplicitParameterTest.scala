package org.wartremover
package test

import org.junit.Test
import org.wartremover.warts.ImplicitParameter

class ImplicitParameterTest extends ResultAssertions {

  @Test def `Implicit parameters are disabled` = {
    val result = WartTestTraverser(ImplicitParameter) {
      def f()(implicit s: String) = ()

      def f2[A]()(implicit a: A) = ()
    }
    assertErrors(result)("Implicit parameters are disabled", 2)
  }

  @Test def `Context bounds are enabled` = {
    val result = WartTestTraverser(ImplicitParameter) {
      def f[A: Seq]() = ()
    }
    assertEmpty(result)
  }

  @Test def `Parent context bounds are enabled` = {
    val result = WartTestTraverser(ImplicitParameter) {
      class C[F] {
        def f[A](a: A)(implicit F: List[F]) = {}
      }
    }
    assertEmpty(result)
  }

  @Test def `Parent abstract types are enabled` = {
    val result = WartTestTraverser(ImplicitParameter) {
      class C {
        type A
        def f()(implicit s: Seq[A]) = {}
      }
    }
    assertEmpty(result)
  }

  @Test def `Desugared context bounds are enabled` = {
    val result = WartTestTraverser(ImplicitParameter) {
      def f[A, B](implicit ev: Either[A, _], ev2: Either[_, Seq[B]]) = ()
    }
    assertEmpty(result)
  }

  @Test def `Defs without parameters don't lead to a crash` = {
    val result = WartTestTraverser(ImplicitParameter) {
      def f = ()
    }
    assertEmpty(result)
  }

  @Test def `ImplicitParameter wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(ImplicitParameter) {
      @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
      def f()(implicit s: String) = ()
    }
    assertEmpty(result)
  }

}
