class A {
  def x1 = List(2, true) // AnyVal

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
}

// ScalaApp
object B extends App
