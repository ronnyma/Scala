import funsets.FunSets._

val s1 = singletonSet(1)
val s2 = singletonSet(2)
val s = union(s1,s2)

val c = filter(s, (x:Int) => x == 2)

c(1)

