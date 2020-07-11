package ch02_higher_function

object MyMoudle {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, acc * n)
    }
    go(n, 1)
  }

  /*
    p.27 연습문제 2.1

    n번째 피보나치 수를 돌려주는 재귀 함수를 작성
    반드시 꼬리 재귀 사용
   */

  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(l: Int, r: Int, cur: Int): Int = {
      if (cur >= n) l
      else go(r, l + r, cur + 1)
    }
    go(0, 1, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // p.30 배열에서 한 요소를 찾는 다형적 함수
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @scala.annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length - 1) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  /*
  p.31 연습문제 2.2

  Array[A]가 주어진 비교 함수에 의거해 정렬되어 있는지 점검하는 isSorted 함수를 구현
   */

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  /*
  p.34 연습문제 2.3
  인수가 두 개인 함수 f를 인수 하나를 받고 그것으로 f를 부분 적용하는 함수로 변환하는
  currying 함수 작성

   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => partial1(a, f)
  }

  /*
  p.35 연습문제 2.4
  curry의 변환을 역으로 수행하는 uncurry 구현

   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /*
  p.35 연습문제 2.5
  두 함수를 합성하는 고차 함수를 구현
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

}