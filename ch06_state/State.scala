
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /*
  p.105 연습 문제 6.1
  RNG.nextInt를 이용해서 0 이상 Int.MaxValue 이하의 난수 정수를 생성하는 함수를 작성
  nextInt가 Int.MinValue를 돌려주는 경우도 확실하게 처리하기
   */

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val n1 = if (n < 0) -1 * (n + 1) else n
    (n1, rng2)
  }

  /*
  p.105 연습 문제 6.2
  0 이상 1 미만의 Double 난수를 발생하는 함수를 작성
   */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val n1 = n.toDouble / (Int.MaxValue + 1)
    (n1, rng2)
  }

  /*
  p.105 연습 문제 6.3
  각각 난수쌍 (Int, Double), (Double, Int), (Double, Double, Double)
  하나를 발생하는 함수들을 작성
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /*
  p.105 연습 문제 6.4
  정수 난수들의 목록을 생성하는 함수를 작성
   */
  def ints(counts: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(c: Int, cur: List[Int], rng: RNG): (List[Int], RNG) =
      if (c == 0) (cur, rng)
      else {
        val (n, rng1) = rng.nextInt
        go(c - 1, n :: cur, rng1)
      }

    go(counts, List(), rng)
  }

  // RNG 상태 동작 자료 형식에 대한 alias를 만들어 놓기.
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /*
  p.107 연습 문제 6.5
  연습 문제 6.2의 double을 map을 이용해서 좀 더 우아한 방식으로 구현하기
   */
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))

  /*
  p.108 연습 문제 6.6
  map2 함수를 구현. 이 함수는 두 상태 동작 ra, rb와 이들의 결과를 조합하는 함수 f를 받고,
  두 동작을 조합한 새 동작을 돌려줌
   */

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /*
  p.108 연습 문제 6.7
  두 RNG 상태를 조합할 수 있다면, 그런 상태 전이들의 List 전체를 조합하는 것도 가능할 것
  상태 전이들의 List를 한 상태 전이로 조합하는 함수 sequence를 구현
  그 후 이 함수를 이용해 이전에 작성한 ints 함수를 다시 구현.
   */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      @scala.annotation.tailrec
      def go(rs: List[Rand[A]], cur: List[A], rng: RNG): (List[A], RNG) =
        rs match {
          case Nil => (cur.reverse, rng)
          case h :: t => {
            val (n, rng2) = h(rng)
            go(t, n :: cur, rng2)
          }
        }

      go(fs, List(), rng)
    }

  /*
    책 풀이 -> foldRight를 사용
    unit을 초기값에 넣을 수 있구나.. (함수형 프로그래밍 참 어렵다.)
    시간 복잡도 : O(N)
   */
  def sequence_1[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List[A]()))((r, acc) => map2(r, acc)(_ :: _))

  /*
    foldRight 대신foldLeft를 하면..?
    -> 역순의 결과가 나옴 -> reverse를 사용해서 뒤집어야 하는데,
    reverse를 어떻게 추가를 해야 할지...
  */
  def sequence_2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft(unit(List[A]()))((r, acc) => map2(acc, r)(_ :: _))

  /*
    sequence -> 함수 내부에서 rng가 명시적으로 표현됨, sequence_1 대비 장황함
    sequence_1 -> 함수 내부에서 RNG 값이 전혀 표현되어 있지 않음
  */

  // sequence를 이용한 연습 문제 6.4의 ints 구현

  def ints_1(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

}