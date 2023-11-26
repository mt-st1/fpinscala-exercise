package fpinscala.part1.ch05

import Stream._

trait Stream[+A] {  // 遅延リスト (ストリーム)

  // Stream の先頭を取り出す関数
  def headOpion: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // [ex5.1] Stream を List に変換し、それによりストリームを強制的に評価する関数を記述せよ
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  // [ex5.2] Stream の先頭からn個の要素を取り出す関数 take(n) と、
  //         Stream の先頭からn個の要素をスキップする drop(n) 関数を記述せよ
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), take(n - 1))
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // [ex5.3] Streamの先頭から指定された述語とマッチする要素をすべて取り出すtakeWhile関数を記述せよ
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // p(a) がfalseを返し続けている間は, b が評価されfoldRightが再帰的に実行される
  // p(a) がtrueを返したら, bは評価されずtrueが返り終了する
  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // [ex5.4] Streamの要素のうち、指定された述語とマッチするものをすべてチェックするforAllを実装せよ
  //         この実装では、マッチしない値が検出された時点でチェックを終了しなければならない
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // [ex.5.5] foldRightを使って takeWhile を実装せよ
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => 
      if (p(a)) cons(a, b)
      else empty
    )

  // [ex5.6] foldRightを使って headOption を実装せよ
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // [ex5.7] foldRightを使って map, filter, append, flatMap を実装せよ
  //         appendメソッドはその引数に関して非正格でなければならない
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))
  
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // [ex5.13] unfoldを使ってmap, take, takeWhile, zipWith, zipAll を実装せよ
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case (Cons(h, t)) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h, t), Empty) =>
        Some(((Some(h()), Option.empty[B]), (t(), empty[B])))
      case (Empty, Cons(h, t)) =>
        Some(((Option.empty[A], Some(h())), (empty[A], t())))
      case (Empty, Empty) =>
        None
    }

  // [ex5.14] ここまで記述してきた関数を使って startsWith を実装せよ
  // この関数は、あるStreamが別のStreamのプレフィックスであるかどうかを調べる
  // たとえば、Stream(1, 2, 3) startsWith Stream(1, 2) の結果はtrueになる
  def startsWith[A](s: Stream[A]): Boolean =
    this
      .zipAllViaUnfold(s)
      .takeWhileViaUnfold(!_._2.isEmpty)
      .forAll {
        case (h1, h2) => h1 == h2
      }

  // [ex5.15] unfoldを使ってtailsを実装せよ
  // 与えられたStreamに対し, tailsは元のStreamから始まる入力シーケンスのサフィックスであるStreamを返す
  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((this, t()))
    case _ => None
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // [ex5.16] tailsをscanRight関数として一般化せよ
  // foldRightと同様に、この関数は中間結果のストリームを返す
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // 空でないストリームを作成するためのスマートコンストラクタ
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // 特定の型の空のストリームを作成するためのスマートコンストラクタ
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = cons(1, ones);

  // [ex5.8] ones を少し一般化し、指定された値の無限ストリームを返す constant 関数を記述せよ
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // [ex5.9] n で始まって n + 1, n + 2 と続く整数の無限ストリーム生成する関数を記述せよ
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // [ex5.10] フィボナッチ数列(0,1,1,2,3,5,8,...) の無限ストリームを生成するfibs関数を記述せよ
  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] =
      cons(n1, go(n2, n1 + n2))
    go(0, 1)
  }

  // [ex5.11] より汎用ストリーム生成関数 unfold を記述せよ
  //          この関数は、初期状態に加えて、以下の状態と、生成されるストリームの次の値を生成する関数を受け取る
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  // [ex5.12] unfold を使って fibs, from, constant, ones を記述せよ
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (n0, n1) => Some((n0, (n1, n0+n1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  def constantViaFold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaFold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

}
