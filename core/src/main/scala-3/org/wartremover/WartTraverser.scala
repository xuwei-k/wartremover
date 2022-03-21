package org.wartremover

abstract class WartTraverser { self =>
  private[wartremover] val simpleName: String = this.getClass.getSimpleName.dropRight(1)
  private[wartremover] val fullName: String = this.getClass.getName.dropRight(1)

  protected final def warning(u: WartUniverse)(pos: u.quotes.reflect.Position, message: String): Unit =
    u.warning(pos = pos, message = message, wartName = simpleName)

  protected final def error(u: WartUniverse)(pos: u.quotes.reflect.Position, message: String): Unit =
    u.error(pos = pos, message = message, wartName = simpleName)

  def hasWartAnnotation(u: WartUniverse)(t: u.quotes.reflect.Tree): Boolean = {
    hasWartAnnotationSymbol(u)(t.symbol) || Option(t.symbol.maybeOwner)
      .filterNot(_.isNoSymbol)
      .filter(s => s.isClassDef || s.isValDef || s.isDefDef)
      .exists(hasWartAnnotationSymbol(u))
  }

  private[this] def hasWartAnnotationSymbol(u: WartUniverse)(s: u.quotes.reflect.Symbol): Boolean = {
    import u.quotes
    import u.quotes.reflect.*
    val SuppressWarningsSymbol = TypeTree.of[java.lang.SuppressWarnings].symbol

    val args: Set[String] = s
      .getAnnotation(SuppressWarningsSymbol)
      .collect {
        case a1 if a1.isExpr =>
          PartialFunction
            .condOpt(a1.asExpr) { case '{ new java.lang.SuppressWarnings($a2: Array[String]) } =>
              PartialFunction
                .condOpt(a2.asTerm) { case Apply(Apply(_, Typed(e, _) :: Nil), _) =>
                  e.asExprOf[Seq[String]].value
                }
                .flatten
            }
            .flatten
      }
      .flatten
      .toList
      .flatten
      .toSet

    args.contains(fullName) || args("org.wartremover.warts.All")
  }

  def apply(u: WartUniverse): u.Traverser

  def compose(o: WartTraverser): WartTraverser = new WartTraverser {
    override def apply(u: WartUniverse): u.Traverser = {
      import u.quotes.reflect.*
      new u.Traverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
          self.apply(u).traverseTree(tree)(owner)
          o.apply(u).traverseTree(tree)(owner)
        }
      }
    }
  }
}

object WartTraverser {
  def sumList(u: WartUniverse)(l: List[WartTraverser]): u.Traverser =
    l.reduceRight(_ compose _)(u)
}
