package ch03_functional_data_structures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*

  p.43 연습문제 3.1

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  위 패턴 부합 표현식의 결과는? => 3
   */

  /*
  p.45 연습문제 3.2
  List의 첫 요소를 제거하는 함수 tail 구현
   */

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new Exception("empty list")
    case Cons(x, xs) => xs
  }

  /*
  p.45 연습문제 3.3
  List의 첫 요소를 다른 값으로 대체하는 함수 setHead를 구현
   */
  def setHead[A](l: List[A], v: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(v, xs)
  }

  /*
  p.46 연습문제 3.4
  tail을 일반화해서, 목록에서 처음 n개의 요소를 제거하는 함수 drop을 구현
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    @scala.annotation.tailrec
    def go(l: List[A], cur: Int): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) if (cur == 1) => xs
      case Cons(x, xs) => go(xs, cur - 1)
    }
    go(l, n)
  }

  def drop2[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop2(t, n - 1)
    }

  /*
  p.46 연습문제 4.5
  주어진 predicate와 부합하는 List의 앞 요소(prefix)들을 제거하는 함수 dropwhile 구현
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  /*
  p.46
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /*
  p.47 연습문제 3.6
  한 List의 마지막 요소를 제외한 모든 요소로 이루어진 List를 돌려주는 함수 init을 구현
  이 함수를 tail처럼 상수 시간으로 구현할 수 없는 이유는?
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @scala.annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(x, xs) => buf += x; go(xs)
    }
    go(l)
  }

  /*
  p.49 오른쪽 접기 함수와 간단한 용례
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  /*
  p.51 연습문제 3.7

  foldRight으로 구현된 product2가 0.0을 만났을 때 즉시 재귀를 멈추고 0.0을 돌려주나?

  -> No, foldRight은 일단 순회를 완료해야 실제 계산이 수행되기 때문이다.

   */

  /*
  p.51 연습문제 3.8

  foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _)) 처럼 Nil과 Cons 자체를 foldRight에 전달한다면?

  -> List(1,2,3) 반환

  foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  Cons(1, Cons(2, Cons(3, Nil)))

   */

  /*
  p.51 연습문제 3.9
  foldRight을 이용해 목록의 길이를 계산
   */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  /*
  p.51 연습문제 3.10
  foldLeft 구현(tail recursion 사용)
   */

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }


  /*
  p.52 연습문제 3.11
  sum, product와 목록의 길이를 계산하는 함수를 foldLeft를 이용해 작성
   */

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productViaFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthViaFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((b, _) => b + 1)

  /*
  p.52 연습문제 3.12
  목록의 역을 돌려주는 함수를 작성.
  fold 함수를 이용해 작성할 수 있는지 시도해볼 것
   */

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  /*
  p.52 연습문제 3.13
  foldLeft를 foldRight을 이용해 구현 가능? 그 반대 방향은?
  foldLeft 사용시 foldRight을 꼬리 재귀적으로 구현할 수 있으므로, 긴 목록에 대해서도
  스택이 넘치지 않는 장점이 생김
  */

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((b, a) => f(a, b))

  // ???
  def foldLeftViaFoldRight2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  // ???
  def foldRightViaFoldLeft2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  /*
  p.52 연습문제 3.14
  append를 foldLeft나 foldRight로 구현
   */
  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, b) => Cons(a, b))

  /*
  p.52 연습문제 3.15
  목록들의 목록을 하나의 목록으로 연결하는 함수를 작성
  실행 기간은 반드시 모든 목록의 전체 길이에 선형으로 비례해야 한다.
   */
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())((b, a) => append(b, a))

  /*
  p.53 연습문제 3.16
  정수 목록의 각 요소에 1을 더해, 목록을 변환하는 함수를 작성(새 List를 돌려주는 순수 함수여야 함)
   */
  def addOne(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

  /*
  p.53 연습문제 3.17
  List[Double]의 각 값을 String으로 변환하는 함수를 작성
   */
  def double2String(l: List[Double]): List[String] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, double2String(t))
    }

  /*
  p.53 연습문제 3.18
  목록의 구조를 유지하면서, 목록의 각 요소를 수정하는 작업을 일반화하는 함수 map 작성
  */
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def map_1[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((h, t) => Cons(f(h), t))

  def map_2[A, B](as: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    @scala.annotation.tailrec
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(as)
    List(buf.toList: _*)
  }

  /*
  p.53 연습문제 3.19
  목록에서 주어진 술어를 만족하지 않는 요소들을 제거하는 함수 filter을 작성
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    }

  def filter_1[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter_2[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @scala.annotation.tailrec
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if(f(h)) buf += h; go(t)
    }
    go(as)
    List(buf.toList: _*)
  }

  /*
  p.53 연습문제 3.20
  map과 비슷하되, 하나의 요소가 아니라 목록을 최종 결과 목록에 삽입하는 함수 flatMap 구현
   */

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  /*
  p.53 연습문제 3.21
  flatmap을 이용해 filter를 구현하기.
   */

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  /*
  p.54 연습문제 3.22
  목록 두 개를 받아서, 대응되는 요소들을 더한 값들로 이루어진 새 목록을 구축하는 함수 작성
  ex) List(1,2,3), List(4,5,6) -> List(5,7,9)
   */

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  /*
  p.54 연습문제 3.23
  연습문제 3.22의 함수를 일반화
   */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /*
  p.55 연습문제 3.24
  List가 또 다른 List를 부분 순차열로서 담고 있는지 점검하는 hasSubsequence 함수를 구
   */

  @scala.annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
      case _ => false
    }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case Cons(h, t) => if (startsWith(sup, sub)) true else hasSubsequence(t, sub)
    }

}