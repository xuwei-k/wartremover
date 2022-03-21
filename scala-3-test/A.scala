package foo

case class B[X](a: X)

object C {
  trait T[R]
  type T1 = String
  object T1 extends T[Unit]
}

class A {
  def x1 = List(2, true) // AnyVal

  // Null, asInstanceOf
  null.asInstanceOf[Long]

  // Return, While, Var
  def x2: Int = {
    var i = 0
    while(i <= 10) {
      return i
    }
    99
  }

  // ArrayEquals
  def x3 = Array(1) == Array(2)


  // ListUnapply, DefaultArguments
  def x4[B](a: collection.Seq[B] = Nil): Int = a match {
    case _ :: _ :: _ =>
      0
    case _ :: _ =>
      1
    case _ =>
      2
  }

  // Any
  def x5 = List(IArray(3), false)

  // ListAppend
  List(3) :+ 4

  "str" + Predef

  Nil.head

  false.isInstanceOf[true]

  scala.util.Try(9).get

  Option(false).get

  Option("") : Iterable[String]

  // PlatformDefault
  scala.io.Source.fromFile("foo.txt")

  // SizeIs
  List(9).size == 2

  // ThreadSleep
  Thread.sleep(33)
}

// ScalaApp
object B extends App
