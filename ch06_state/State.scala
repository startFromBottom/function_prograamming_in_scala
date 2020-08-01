
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


}