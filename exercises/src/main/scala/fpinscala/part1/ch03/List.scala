package fpinscala.part1.ch03

sealed trait List[+A]  // Listデータ型
case object Nil extends List[Nothing]  // 空のリストを表すデータコンストラクタ
case class Cons[+A](head: A, tail: List[A]) extends List[A]  // 空ではないリストを表すデータコンストラクタ

// Listコンパニオンオブジェクト (リストの作成や操作のための関数を含んでいる)
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // [ex3.2] Listの最初の要素を削除する関数
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

  // [ex3.3] Listの最初の要素を別の値と置き換える関数
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

  // [ex3.4] tailを一般化して、リストの先頭からn個の要素を削除する関数
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  // [ex3.5] 述語とマッチする場合に限り、Listからその要素までの要素を削除する関数
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)  // パターンガード
      case _ => l
    }

  def append[A](l1: List[A], l2: List[A]): List[A] =
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }

  // [ex3.6] Listの末尾を除く全ての要素で構成されたListを返す関数
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // [ex3.7] foldRightを使って実装されたproductは、0.0を検出した場合に、直ちに再帰を中止して0.0を返せるか
  // 返せない. メソッド実行前にまず引数が評価され、次いでメソッド本体のコードが実行される (正格評価)

  // [ex3.8] foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))のように、NilおよびCons自体をfoldRightに渡した場合はどうなるか
  //         これがfoldRightとLisのデータコンストラクタとの関係について何を表していると思うか
  // 入力のリストを生成し直して返す

  // [ex3.9] foldRightを使ってリストの長さを計算
  def length[A](l: List[A]): Int =
    foldRight(l, 0){(_, acc) => acc + 1}

  // [ex3.10] リスト再帰の総称関数foldLeftを記述
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // [ex3.11] foldLeftを使ってsum, product, およびlengthを計算する関数
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // [ex3.12] 要素が逆に並んだリストを返す関数を記述
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, x) => Cons(x, acc))

  // [ex3.13] foldRightをベースとしてfoldLeftを記述することは可能か、その逆はどうか
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, acc) => f(acc, x))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, x) => f(x, acc))

  // [ex3.14] foldLeft または foldRightをベースとして append を実装せよ
  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  // [ex3.15] 複数のリストからなるリストを1つのリストとして連結する関数を記述せよ
  //          この関数の実行時間はすべてのリストの長さに対して線形になるはずである
  def concat[A](nestedList: List[List[A]]): List[A] = foldRight(nestedList, Nil:List[A])(append)

  // [ex3.16] 各要素に1を足すことで整数のリストを変換する関数を記述せよ
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((x, acc) => Cons(x + 1, acc))

  // [ex3.17] List[Double]の各値をStringに変換する関数を記述せよ
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((x, acc) => Cons(x.toString, acc))

  // [ex3.18] リストの各要素を変更し、かつリストの構造をそのまま保つ総称関数 map を記述せよ
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  // [ex3.19] 与えられた述語条件が満たされるまでリストから要素を削除するfilter関数を記述せよ
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil:List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  // [ex3.20] mapと同じような働きをするflatMap関数を記述せよ
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  // [ex3.21] flatMapを使ってfilterを実装せよ
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

  // [ex3.22] リストを2つ受け取り、対応する要素同士を足し合わせて新しいリストを生成する関数を記述せよ
  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }

  // [ex3.23] ex3.22で作成した関数を、整数または加算に限定されないように一般化せよ
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // [ex3.24] Listに別のListがシーケンスとして含まれているかどうかを調べるhasSubsequenceを実装せよ
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }

    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
