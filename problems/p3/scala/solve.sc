def factors(n: Long): List[Long] = (2 to math.sqrt(n).toInt).view.find(n % _ == 0).fold(List(n))(i => i :: factors(n / i))

assert(factors(13195L) == List(5L, 7L, 13L, 29L))
assert(factors(600851475143L) == List(71L, 839L, 1471L, 6857L))
