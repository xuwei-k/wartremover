package org.wartremover
package test

import org.junit.Assert.assertEquals
import org.junit.Test
import org.wartremover.warts.Unsafe

class UnsafeTest {
  @Test def `can't use null, var, non-unit statements, Option#get, LeftProjection#get, RightProjection#get, or any2stringadd` = {
    val result = WartTestTraverser(Unsafe) {
      val x = List(1, true, "three")
      var u = {} + "Hello!"
      Some(10).get
      println(Left(42).left.get)
      println(Left(42).right.get)
      println(Right(42).left.get)
      println(Right(42).right.get)
      println(null)
    }
    assertEquals(
      "result.errors",
      Set("[wartremover:Any] Inferred type containing Any",
           "[wartremover:EitherProjectionPartial] LeftProjection#get is disabled - use LeftProjection#toOption instead",
           "[wartremover:EitherProjectionPartial] RightProjection#get is disabled - use RightProjection#toOption instead",
           "[wartremover:NonUnitStatements] Statements must return Unit",
           "[wartremover:Null] null is disabled",
           "[wartremover:OptionPartial] Option#get is disabled - use Option#fold instead",
           "[wartremover:StringPlusAny] Implicit conversion to string is disabled",
           "[wartremover:Var] var is disabled"),
           result.errors.toSet)
    assertEquals("result.warnings", List.empty, result.warnings)
  }
}
