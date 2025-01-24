package org.wartremover
package warts

object SeqApply
    extends ExprMatch({ case '{ ($x: collection.Seq[?]).apply($n) } =>
      "Seq.apply is disabled"
    })
