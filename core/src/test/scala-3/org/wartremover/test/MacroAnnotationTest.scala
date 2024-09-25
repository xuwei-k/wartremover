package foo

@scala.annotation.experimental
@org.wartremover.WartremoverAnnotation(org.wartremover.warts.OrTypeLeastUpperBound.All)
class Foo {
  def f(x: Boolean) = if (x) 2 else "a"
}
