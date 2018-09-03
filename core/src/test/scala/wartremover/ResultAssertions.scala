package org.wartremover
package test

import org.junit.Assert.assertEquals

trait ResultAssertions {

  def assertEmpty(result: WartTestTraverser.Result) = {
    assertEquals("result.errors", List.empty, result.errors)
    assertEquals("result.warnings", List.empty, result.warnings)
  }

  def assertError(result: WartTestTraverser.Result)(message: String) = assertErrors(result)(message, 1)

  def assertErrors(result: WartTestTraverser.Result)(message: String, times: Int) = {
    assertEquals("result.errors", List.fill(times)(message), result.errors.map(skipTraverserPrefix))
    assertEquals("result.warnings", List.empty, result.warnings.map(skipTraverserPrefix))
  }

  def assertWarnings(result: WartTestTraverser.Result)(message: String, times: Int) = {
    assertEquals("result.errors", List.empty, result.errors.map(skipTraverserPrefix))
    assertEquals("result.warnings", List.fill(times)(message), result.warnings.map(skipTraverserPrefix))
  }

  private val messageFormat = """^\[wartremover:\S+\] (.+)$""".r

  private def skipTraverserPrefix(msg: String) = msg match {
    case messageFormat(rest) => rest
    case s => s
  }
}
