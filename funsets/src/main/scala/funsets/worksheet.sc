import funsets.FunSets._

val n = 1 + 2
println(n)

println(contains(map(_ > 0, _ * 2), 3))
println(contains(map(_ > 0, _ * 2), 4))
println(contains(map(_ > 0, _ -1), 999))
println(contains(map(_ > 0, _ -1), 1000))
println(exists(map(_ > 0, _ * 2), _ % 2 == 0))
println(exists(map(_ > 0, _ * 2), _ % 2 == 1))

println(forall(map(_ > 0, _ * 2), _ % 2 == 0))
println(forall(map(_ > 0, _ * 2), _ % 2 == 1))