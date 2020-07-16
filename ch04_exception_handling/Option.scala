package ch04_exception_handling

sealed trait Option[+A] {

  /*
    p.67 연습문제 4.2
    아래의 다섯 함수 구현

   */

  // Option이 None이 아니면 f를 적용
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }

  // Option이 None이 아니면 f(실패할 수 있음)을 적용
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(v) => f(v)
    }

  def flatMap_1[B](f: A => Option[B]): Option[B] =
    this map (f) getOrElse (None)

  // Option의 Some 안의 결과를 돌려줌, Option이 None이면 default를 돌려줌
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(v) => v
    }

  // 첫 Option이 정의되어 있으면 그 것을 돌려주고, 아니면 둘째 Option을 돌려줌
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(v) => Some(v)
    }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  // 값이 f를 만족하지 않으면 Some을 None으로 변환
  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(v) if (f(v)) => this
      case _ => None
    }

  def filter_1(f: A => Boolean): Option[A] =
    this flatMap (a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*
    p.68 연습문제 4.2
    variance 함수를 flatMap을 이용해 구현하라.
   */

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /*
    p.72 연습문제 4.3
    두 Option 값을 이항 함수를 이용해 결합하는 일반적 함수 map2를 작성,
    두 Option중 하나라도 None이면 map2의 결과도 None이어야 함

   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a1 <- a;
      b1 <- b
    } yield f(a1, b1)

  /*
    p.73 연습문제 4.4 Option 들의 List을 받고, 그 List에 있는 모든 Some값으로 구성된 List를 담은 Option을
    돌려주는 함수 sequence를 작성. 원래 List에 None이 1개라도 있으면, 함수의 결과도 None이어야 함.

   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (h1 => sequence(t).map(h1 :: _))
    }

  def sequenceViaMap2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((l, r) => map2(l, r)(_ :: _))

  /*
    p.74 연습문제 4.5 traverse 함수를 구현하라. (map, sequence 이용 -> 비효율)
    더 나아가서, sequence를 이 traverse로 구현하라.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(b => traverse(t)(f).map(b :: _))
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((l, r) => map2(f(l), r)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)


  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    (age + numberOfSpeedingTickets).toDouble

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  //  def parseInsuranceRateQuote(
  //    age:String, numberOfSpeedingTickets: String): Option[Int] = {
  //    val optAge: Option[Int] = Try(age.toInt)
  //    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  //    lift(insuranceRateQuote)(optAge, optTickets)
  //
  //  }

}