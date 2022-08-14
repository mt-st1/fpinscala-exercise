object Program {
  // [ex2.1] n番目のフィボナッチ数を取得する関数
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int) =
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  // [ex2.2] 指定された比較関数に従ってArray[A]がソートされているかどうか調べる関数
  def isSorted[A] (as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (as.size <= n) true
      else if (gt(as[n], as[n+1])) false
      else loop(n+1)

    loop(0)
  }

  // [ex2.3] カリー化
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // [ex2.4] アンカリー化
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // [ex2.5] 関数合成 (gを適用してからfを適用 -> `f compose g` = `g andThen f`)
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
