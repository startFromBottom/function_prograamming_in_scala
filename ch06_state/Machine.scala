package ch06_state

import State._

/*

  p.115 연습 문제 6.11

  State를 사용하는 경험을 쌓기 위해, 간단한 사탕 판매기를 본뜬 finite state automata를 구현
  이 판매기에는 두 종류의 input이 존재한다.
    1. 사용자가 넣는 동전
    2. 돌리면 사탕이 나오는 손잡이
  또한, 이 판매기는 사탕이 몇 개나 남았는지, 동전이 몇 개나 들었는지도 추적한다.

  사탕 판매기의 작동 규칙은 다음과 같다.

  A-1. 잠겨진 판매기에 동전을 넣으면, 사탕이 남아 있는 경우 잠김이 풀린다.
  A-2. 풀린 판매기의 손잡이를 돌리면 사탕이 나오고 판매기가 잠긴다.
  A-3. 잠긴 판매기의 손잡이를 돌리거나 풀린 판매기에 동전을 넣으면 아무 일도 생기지 않는다.
  A-4. 사탕이 없는 판매기는 모든 입력을 무시한다.

  simulateMachine 메서드는 input의 List에 기초하여 판매기를 작동하고,
  작동이 끝난 후 판매기에 있는 동전, 사탕의 개수를 돌려줘야 한다.

  예컨대, 동전이 10개, 사탕이 5개 있는 Machine에서 총 4개의 사탕이 성공적으로 팔렸다면
  출력은 (14, 1)이어야 함.

 */

/*
def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // 현재 상태를 얻어 s에 배정
    _ <- set(f(s)) // 새 상태를 s를 f에 적용한 결과로 설정
  } yield ()


 */

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  /*
  A-1. 잠겨진 판매기에 동전을 넣으면, 사탕이 남아 있는 경우 잠김이 풀린다.
  A-2. 풀린 판매기의 손잡이를 돌리면 사탕이 나오고 판매기가 잠긴다.
  A-3. 잠긴 판매기의 손잡이를 돌리거나 풀린 판매기에 동전을 넣으면 아무 일도 생기지 않는다.
  A-4. 사탕이 없는 판매기는 모든 입력을 무시한다.
   */
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s // A-4
      case (Turn, Machine(true, _, _)) => s // A-3
      case (Coin, Machine(false, _, _)) => s // A-3
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin) // A-2
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1) // A-1
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}

