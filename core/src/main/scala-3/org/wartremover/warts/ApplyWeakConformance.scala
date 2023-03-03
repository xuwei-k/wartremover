package org.wartremover
package warts

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[wartremover] object ApplyWeakConformance {
  inline def asString[A1, A2]: Option[String] = ${ asStringImpl[A1, A2] }
  def asStringImpl[A1: Type, A2: Type](using q: Quotes): Expr[Option[String]] = {
    import q.reflect.*
    val a = new ApplyWeakConformance[q.type](strict = true)
    Expr(
      a.weakConformance(TypeRepr.of[A1], TypeRepr.of[A2]).map(_.show)
    )
  }
}
private[wartremover] class ApplyWeakConformance[Q <: Quotes](strict: Boolean)(using val q: Q) {
  import q.reflect.*

  private[this] val weight: Seq[(TypeRepr, Int)] = Seq(
    TypeRepr.of[Byte] -> 2,
    TypeRepr.of[Char] -> 3,
    TypeRepr.of[Short] -> 4,
    TypeRepr.of[Int] -> 12,
    TypeRepr.of[Long] -> 24,
    TypeRepr.of[Float] -> 20,
    TypeRepr.of[Double] -> 60
  )

  private[this] def leastCommonMultiple(x1: Int, x2: Int): Int =
    List(x1, x2).foldLeft(1) { (a, b) =>
      b * a / Iterator.iterate((a, b)) { case (y1, y2) => (y2, y1 % y2) }.dropWhile(_._2 != 0).next._1.abs
    }

  def weakConformance(t1: TypeRepr, t2: TypeRepr): Option[TypeRepr] = {
    (t1, t2) match {
      case (o1: OrType, _) =>
        weakConformance(o1.left, o1.right).flatMap(x => weakConformance(x, t2))
      case (_, o2: OrType) =>
        weakConformance(o2.left, o2.right).flatMap(x => weakConformance(t1, x))
      case (_, _) =>
        (
          weight.find(_._1 =:= t1),
          weight.find(_._1 =:= t2)
        ) match {
          case (Some(w1), Some(w2)) =>
            val max = List(w1, w2).maxBy(_._2)
            val min = List(w1, w2).minBy(_._2)
            if ((max._2 % min._2) == 0) {
              Some(max._1)
            } else {
              if (strict) {
                None
              } else {
                val lcm = leastCommonMultiple(max._2, min._2)
                weight.find(x => (x._2 % lcm) == 0).map(_._1)
              }
            }
          case _ =>
            None
        }
    }
  }
}
