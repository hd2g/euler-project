def isPrime(n: Int): Boolean = if (n == 2 || n == 3) true else if (n % 2 == 0) false else !(3 to math.sqrt(n).toInt by 2).exists(n % _ == 0)

def from(n: Int): Stream[Int] = n #:: from(n + 1)

val primes = from(2).filter(isPrime)

assert(primes(6 - 1) == 13)
assert(primes(10001 - 1) == 104743)
