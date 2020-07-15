package ch03_functional_data_structures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /*
  p.57 연습문제 3.25
  트리의 노드, 즉 leaf와 branch의 개수를 세는 함수 size를 작성
   */
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  /*
  p.57 연습문제 3.26
  Tree[Int]에서 가장 큰 요소를 돌려주는 함수 maximum을 작성
   */
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  /*
  p.58 연습문제 3.27
  root에서 임의의 잎으로의 가장 긴 경로의 길이를 돌려주는 함수 depth를 작성
   */
  def depth(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => 0
      case Branch(l, r) => 1 + depth(l) max depth(r)
    }

  /*
  p.58 연습문제 3.28
  tree의 각 요소를 주어진 함수로 수정하는 map 작성
   */

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  /*
  p.58 연습문제 3.29
  size, maximum, depth, map 유사성 요약 => 일반화한 새 함수 fold를 작성
  그 다음, 그 함수들을 새 fold를 이용해 다시 구현
   */

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)((b1, b2) => b1 max b2)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((d1, d2) => 1 + (d1 max d2))

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  
}