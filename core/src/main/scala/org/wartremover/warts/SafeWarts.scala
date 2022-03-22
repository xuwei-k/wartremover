package org.wartremover
package warts

private[warts] object SafeWarts {
  val safeTraversers: List[WartTraverser] = List(
    Any,
    AsInstanceOf,
    DefaultArguments,
    EitherProjectionPartial,
    IsInstanceOf,
    IterableOps,
    NonUnitStatements,
    Null,
    OptionPartial,
    Product,
    Return,
    Serializable,
    StringPlusAny,
    Throw,
    TryPartial,
    Var
  )
}
