def from(n: Long, d: Long = 1L): Stream[Long] = n #:: from(n + d, d)

val triangles: Stream[Long] = from(1).scanLeft(0L)(_+_) 

def divisors(n: Long): List[Long] = (2L to math.sqrt(n).toLong).view.find(n % _ == 0).fold(List(n))((i: Long) => i :: divisors(n / i))

def countOfDivisors(n: Long): Int = divisors(n).groupBy(i => i).map(_._2.size + 1).product

def solve(threshould: Int): Option[(Long, Int)] = triangles.map(t => (t, countOfDivisors(t))).find(threshould <= _._2)

solve(500).map(p => assert(p._1 == 76576500L))
