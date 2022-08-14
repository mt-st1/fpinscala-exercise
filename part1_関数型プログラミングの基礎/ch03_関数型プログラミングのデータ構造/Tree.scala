sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // [ex3.25] 2分木のノード(Leaf と Branch)の数を数える size 関数を記述せよ
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // [ex3.26] Tree[Int]の最大の要素を返す maximum 関数を記述せよ
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // [ex3.27] 2分木のルートから任意のLeafまでの最長パスを返す depth 関数を記述せよ
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // [ex3.28] 2分岐の各要素を特定の関数を使って変更する map 関数を記述せよ
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // [ex3.29] size, maximum, depth, map を一般化し、それらの類似点を抽象化する新しい fold 関数を記述せよ
  //        そして、このより汎用的な fold 関数を使ってそれらを再実装せよ
  //        この fold 関数と List の左畳み込み および 右畳み込み の間にある類似性を抽出することは可能か
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[A](t: Tree): Int = fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree): Int = fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
