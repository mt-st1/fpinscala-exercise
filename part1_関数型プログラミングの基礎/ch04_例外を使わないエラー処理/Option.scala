//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  // [ex4.1] リスト4.4のすべての関数(map, flatMap, getOrElse, orElse, filter)をOptionで実装せよ
  // map と getOrElse 以外の関数はすべてパターンマッチを使用せず実装できる

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(v => if (f(v)) Some(v) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // [ex4.2] flatMapをベースとしてvariance(分散)関数を実装せよ
  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // 通常の関数(A => B) を Optionに対応する関数(Option[A] => Option[B]) に変換
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f  // (x: Option[A] => x map f) と同義

  // 例外ベースのAPIをOptionベースのAPIに変換する汎用目的の関数
  // aの評価中に発生する例外をすべてキャッチし、それらをNoneに変換する (例外発生しなければ評価結果をSomeに包む)
  // aの型として => A が指定されており、非正格な引数(遅延引数, 関数内でその引数が呼び出されたときに評価される)を使用
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => Non }

  // [ex4.3] 2項関数を使ってOption型の2つの値を結合する総称関数 map2 を記述せよ
  // どちらかのOptioon値が None の場合は、戻り値も None になる
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
  // for内包表記で記述すると以下のようになる
  def map2ViaForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // [ex4.4] Optionのリストを1つのOptionにまとめるsequence関数を記述せよ
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequenceViaFoldRight[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, acc) => map2(x, acc)(_ :: _))

  // [ex4.5] 失敗する可能性のある関数を使って、1回の走査でリストをマッピングするtraverse関数を実装せよ
  // 以下のように map と sequence を組み合わせるやり方では、2回走査することになるため効率が悪い
  // def parseInts(a: List[String]): Option[List[Int]] =  // List[String] -> List[Option[String]] -> Option[List[String]]
  //   sequence(a map (s => Try(s.toInt)))
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, acc) => map2(f(x), acc)(_ :: _))

}
