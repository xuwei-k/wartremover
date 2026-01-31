package org.wartremover
package warts

import scala.annotation.nowarn
import scala.quoted.Quotes
import scala.quoted.quotes

object RedundantConversions extends WartTraverser {

  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      private final case class Value(
        method: String,
        check: PartialFunction[Select, String]
      )

      private val values: List[Value] = List(
        Value(
          "toList",
          {
            case s if s.qualifier.tpe.baseClasses.exists(_.fullName == "scala.collection.immutable.List") =>
              "redundant toList conversion"
          }
        ),
        Value(
          "toSeq",
          {
            case s if s.qualifier.tpe.baseClasses.exists(_.fullName == "scala.collection.immutable.Seq") =>
              "redundant toSeq conversion"
          }
        ),
        Value(
          "toVector",
          {
            case s if s.qualifier.tpe.baseClasses.exists(_.fullName == "scala.collection.immutable.Vector") =>
              "redundant toVector conversion"
          }
        ),
        Value(
          "toStream",
          {
            case s if s.qualifier.tpe.baseClasses.exists(_.fullName == "scala.collection.immutable.Stream") =>
              "redundant toStream conversion"
          }
        ),
        Value(
          "toSet",
          {
            case s if s.qualifier.tpe.baseClasses.exists(_.fullName == "scala.collection.immutable.Set") =>
              "redundant toSet conversion"
          }
        ),
        Value(
          "toIndexedSeq",
          {
            case s if s.qualifier.tpe.baseClasses.exists(_.fullName == "scala.collection.immutable.IndexedSeq") =>
              "redundant toIndexedSeq conversion"
          }
        ),
        Value(
          "toString",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[String] =>
              "redundant toString conversion"
          }
        ),
        Value(
          "toInt",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Int] =>
              "redundant toInt conversion"
          }
        ),
        Value(
          "toLong",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Long] =>
              "redundant toLong conversion"
          }
        ),
        Value(
          "toFloat",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Float] =>
              "redundant toFloat conversion"
          }
        ),
        Value(
          "toDouble",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Double] =>
              "redundant toDouble conversion"
          }
        ),
        Value(
          "toByte",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Byte] =>
              "redundant toByte conversion"
          }
        ),
        Value(
          "toShort",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Short] =>
              "redundant toShort conversion"
          }
        ),
        Value(
          "toChar",
          {
            case s if s.qualifier.tpe <:< TypeRepr.of[Char] =>
              "redundant toChar conversion"
          }
        )
      )

      private val methodNames: Seq[String] = values.map(_.method)

      @nowarn("msg=LazyList")
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if methodNames.forall(sourceCodeNotContains(tree, _)) =>
          case t if hasWartAnnotation(t) =>
          case s @ Select(_, method) =>
            values.iterator
              .filter(x => sourceCodeContains(tree, x.method))
              .find(_.method == method)
              .flatMap(_.check.lift.apply(s)) match {
              case Some(err) =>
                error(selectNamePosition(s), err)
              case None =>
                super.traverseTree(tree)(owner)
            }
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
