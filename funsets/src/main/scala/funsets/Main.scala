package funsets

object Main extends App {
  import FunSets._
  // println(contains(singletonSet(1), 1))
  // println(contains(union((x: Int) => x > 0, (x: Int) => x < 10), 1))
  // println(contains(intersect((x: Int) => x > 0, (x: Int) => x < 10), 11))
  // println(forall((x: Int) => x > -999, (p: Int) => p * 2 < 10000))
  // println(forall(intersect(_ > 10, _ < 100), (p: Int) => p > 0))
  // println(forall(union(_ == 10, _ == 100), (p: Int) => p > 0))
  // println(forall(union(_ == 10, _ == 12), (p: Int) => p % 2 == 0))
  // println(forall(union(_ == 10, _ == 12), (p: Int) => p % 2 == 1))
  // println(forall(_ > 10000, (p: Int) => true))
  // println(contains(filter((x: Int) => x > 0, (x: Int) => x < 10), 5))
  // println(contains(filter((x: Int) => x > 0, (x: Int) => true), -1000))

  // // def negFilter(s: FunSet, p: FunSet): FunSet = (x: Int) => !filter(s, p)(x)
  // def negFilter(s: FunSet, p: FunSet): FunSet = filter(y => !s(y), z => p(z))

  // println(contains(negFilter((x: Int) => x > 0, (x: Int) => x < 10), 5))
  // println(contains(negFilter((x: Int) => x > 0, (x: Int) => true), -1000))
  // println(exists(_ > 100, _ % 2 == 0))
  // println(exists(intersect(_ > 0, _ < 100), _ % 2 == 0))
  // println(exists(_ > 10000, p => p % 2 == 0))
  // println(exists(_ > 10000, p => true))
  // println(exists(union(_ == 10, _ == 12), (p: Int) => p % 2 == 0))
  // println(exists(union(_ == 10, _ == 12), (p: Int) => p % 2 == 1))
  println(contains(map(_ > 0, _ * 2), 3))
  println(contains(map(_ > 0, _ * 2), 4))
  println(contains(map(_ > 0, _ -1), 999))
  println(contains(map(_ > 0, _ -1), 1000))
  println(exists(map(_ > 0, _ * 2), _ % 2 == 0))
  println(exists(map(_ > 0, _ * 2), _ % 2 == 1))

  println(forall(map(_ > 0, _ * 2), _ % 2 == 0))
  println(forall(map(_ > 0, _ * 2), _ % 2 == 1))
}
