package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.StringPlusAny

class StringPlusAnyTest extends ResultAssertions {
  @Test def `Implicit conversion to string is disabled` = {
    val result = WartTestTraverser(StringPlusAny) {
      {} + "lol"
      "lol" + 1
      "" + (if (true) 5 else "")
    }
    assertErrors(result)("Implicit conversion to string is disabled", 3)
  }

  @Test def `Primitive conversion to string is disabled` = {
    val result = WartTestTraverser(StringPlusAny) {
      1 + "lol"
    }
    assertError(result)("Implicit conversion to string is disabled")
  }

  @Test def `Non-string + usage is allowed` = {
    val result = WartTestTraverser(StringPlusAny) {
      1 + 1
    }
    assertEmpty(result)
  }

  @Test def `string literal concatenation is allowed` = {
    val result = WartTestTraverser(StringPlusAny) {
      "a" + "b"
    }
    assertEmpty(result)
  }

  @Test def `concatenating strings with if statements is allowed.` = {
    val result = WartTestTraverser(StringPlusAny) {
      "" + (if (true) "" else "")
      (if (true) "" else "") + ""
    }
    assertEmpty(result)
  }

  @Test def `inserting into a Set is allowed` = {
    val result = WartTestTraverser(StringPlusAny) {
      Set("") + ""
    }
    assertEmpty(result)
  }

  @Test def `custom-defined + is allowed` = {
    val result = WartTestTraverser(StringPlusAny) {
      class C { def +(s: String) = s }
      new C + "a"
    }
    assertEmpty(result)
  }
 
  @Test def `Concatenation of a string with a block containing an if-statement is allowed.` = {
    val result = WartTestTraverser(StringPlusAny) {
      "" + { val x = ""; if (true) x else x }
    }
    assertEmpty(result)
  }
  
  @Test def `Adding float variables to an int value is allowed.` = {
    val result = WartTestTraverser(StringPlusAny) {
      val a:Float = 1
      0 + a
    }
    assertEmpty(result)
  }

  @Test def `StringPlusAny wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(StringPlusAny) {
      @SuppressWarnings(Array("org.wartremover.warts.StringPlusAny"))
      val foo = {} + "lol"
    }
    assertEmpty(result)
  }

  @Test def `adding Float values is allowed` = {
    val result = WartTestTraverser(StringPlusAny) {
      1f + 1f
    }
    assertEmpty(result)
  }
  
  @Test def `adding with StringOps is allowed` = {
    val result = WartTestTraverser(StringPlusAny) {
      "" + ("" padTo (1, ' '))
    }
    assertEmpty(result)
  }
}

