package ch05_laziness

import Stream._

import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /*
   p.89 연습문제 5.1
   Stream을 List로 반환하되 평가를 강제해 REPL로 목록의 요소들을 볼 수 있게 하는 함수 작성
   */
  // non tail-recursive
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // tail-recursive
  def toList_1: List[A] = {
    @scala.annotation.tailrec
    def go(l: List[A], r: Stream[A]): List[A] = r match {
      case Empty => l
      case Cons(h, t) => go(h() :: l, t())
    }

    go(Nil, this).reverse
  }

  // tail-recursive 방식도 마지막에 reverse 연산이 있기 때문에
  // Time Complexity: O (2*n)

  def toList_2: List[A] = {
    val buf = new ListBuffer[A]()

    @scala.annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => Nil
      case Cons(h, t) =>
        buf += h();
        go(t())
    }

    go(this)
    buf.toList
  }


  /*
  p.89 연습문제 5.2
  Stream의 처음 n개의 요소를 돌려주는 함수 take(n),
  Stream의 처음 n개의 요소를 건너뛴 Stream을 돌려주는 drop(n)을 작성
   */
  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case _ => Empty
    }
  }

  def take_1(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = {
    @scala.annotation.tailrec
    def go(n: Int, cur: Stream[A]): Stream[A] =
      if (n == 0) cur
      else cur match {
        case Cons(h, t) => go(n - 1, t())
        case _ => Empty
      }

    go(n, this)
  }

  def drop_1(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /*
  연습문제 5.3
  Stream에서 주어진 술어를 만족하는 선행 요소들을 모두 돌려주는 takeWhile 함수 작성

   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists_1(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /*
  p.91 연습문제 5.4
  Stream의 모든 요소가 주어진 술어를 만족하는지 점검하는 forAll 함수를 구현
  만족하지 않는 값을 만났을 때 즉시 순회를 마쳐야 함.

   */
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll_1(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /*
  p.91 연습문제 5.5
  foldRight을 이용해서 takeWhile을 구현
  Stream에서 주어진 술어를 만족하는 선행 요소들을 모두 돌려주는 takeWhile 함수 작성
   */

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  /*
  p.91 연습문제 5.6
  foldRight을 이용해 headOption을 구현

   */

  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  /*
  p.91 연습문제 5.7
  foldRight을 이용해 map, filter, append, flatMap을 구현. append 메서드는 자신의 인수에 대해
  엄격하지 않아야 함.


   */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (f(h)) cons(h, t) else t)

  // B: A의 SuperType으로 전환
  // def append(s: Stream[A]): Stream[A] : error
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h) append t)

  /*
  p.96 연습 문제 5.13
  unfold를 이용해 map, take, takeWhile, zipWith, zipAll을 구현

   */

  def mapViaUnfold[B](f: A => B): Stream[B] =
    // unfold(this)(v => v match {...})과 같은 형태
    // pattern match annoymous function
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n-1))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case (Empty, _) => None
      case (_, Empty) => None

    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this,s2)) {
      case (Empty, Cons(h2, t2))  => Some(((None, Some(h2())), (empty, t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    }


  /*
  p.96 연습 문제 5.14
  앞에서 작성한 함수들을 사용해, startsWith를 구현.
  이 함수는 한 Stream이 다른 Stream의 prefix인지를 점검해야 함.
   */
//  def startsWith[AA>: A](s: Stream[AA]): Boolean =
//    this.zipWith(s)((a, b) => ((a, b))).forAll(v => v._1 == v._2)

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll(v => v._1 == v._2)

  /*
  p.97 연습 문제 5.15
  unfold를 이용해 tails 구현
  tails는 주어진 입력 Stream과 suffix들로 이루어진 Stream을 반환
  ex)
  tails(Stream(1,2,3))
  => Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())

   */
  def tails: Stream[Stream[A]] =
    unfold(this){
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _ => None
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    this.tails exists(_ startsWith s)

  /*
  p.97 (어려움) 연습문제 5.16 - tails를 일반화한 scanRight 함수를 작성.
  이 함수는 중간 결과들의 Stream을 돌려주는 foldRight와 유사
  단, 시간 복잡도 : O(n)으로 할 것.
  이 함수를 unfold로 구현할 수 있을까?
  unfold로 구현할 수 없다면, 앞에서 작성한 다른 함수로 구현할 수 있을까?

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /*
 p.94 연습문제 5.8
 ones를 일반화하여, 주어진 값의 무한 Stream을 생성하 함수 constant를 구현
  */
  def constants[A](a: A): Stream[A] =
    cons(a, constants(a))

  /*
  p.95 연습문제 5.9
  n에서 시작하여, n+1, n+2 ... 로 이루어진 무한 정수 Stream을 생성하는 함수 구현
   */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /*
  p.95 연습문제 5.10
  무한 피보나치 수 0, 1, 1, 2, 3, 5, 8, ...으로 이루어진 무한 정수 Stream을 생성하는 함수
  fibs 작성

   */
  def fibs: Stream[Int] = {
    def go(l: Int, r: Int): Stream[Int] =
      cons(l, go(r, l + r))
    go(0, 1)
  }

  /*
  p.95 연습문제 5.11
  좀 더 일반화된 Stream 구축 함수 unfold를 작성.
  이 함수는 초기 상태 하나와 다음 상태 및 다음 값(생성된 stream)을 산출하는 함수 하나를 받는다.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  /*
  p.96 연습 문제 5.12
  unfold를 이용해 fibs, from, constant, ones를 작성
   */
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(v => Option(v._1, (v._2, v._1 + v._2)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(v => Option(v, v + 1))

  def constantViaUnfold[A](n: A): Stream[A] =
    unfold(n)(v => Option(v, v))

  def onesViaUnfold: Stream[Int] = constantViaUnfold(1)


}