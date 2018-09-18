package org.wartremover
package warts

/**
 * 以下の設定が必須
 * {{{
 * scalacOptions += "-Yrangepos"
 * }}}
 */
object LineCount extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._

    new u.Traverser {
      override def traverse(tree: Tree): Unit = {

        val synthetic = isSynthetic(u)(tree) // 自動生成メソッドか？

        tree match {
          case t @ DefDef(_, methodName, _, _, _, _) if !synthetic =>
            val p = t.pos
            // これで、該当のメソッドのソースコードそのままのStringが手に入るはず
            val methodContent = String.valueOf(p.source.content.slice(p.start, p.end))
            // TODO どこかから、しきい値を読み込んで任意の値に設定する
            // JVM起動時の引数？コンパイルオプション？
            val maxLineCount = 5
            val count = methodContent.lines.size
            if (count > maxLineCount) {
              error(u)(p, s"${methodName}というメソッドが${count}行あります。${maxLineCount}行以内にしてください。")
            }
          case _ =>
            // TODO これだと "def" のみなので、ValDefなども同様にチェック？

            super.traverse(tree)
        }
      }
    }
  }
}
