package ch04_exception_handling

sealed trait Either[+E, +A] {

  /*
    p.77 연습문제 4.6
    Right 값에 대해 작용하는 버전의 map, flatMap, orElse, map2, Either를 구현
   */

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(v) => Right(f(v))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => b
      case Right(v) => Right(v)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a1 => b.map(b1 => f(a1, b1)))

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a1 <- this
      b1 <- b
    } yield f(a1, b1)


}


case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  /*
    p.77 연습문제 4.7
    Either에 대한 sequence와 traverse를 작성
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h.flatMap(h1 => sequence(t).map(h1 :: _))
    }

  def sequenceViatraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((l, r) => f(l).map2(r)(_ :: _))

}