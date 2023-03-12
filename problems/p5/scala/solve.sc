def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

def lcm(a: Long, b: Long): Long = if (a == 0 || b == 0) 0 else (a * b) / gcd(a, b)

assert((2L to 10L).reduceLeft(lcm) == 2520L)
assert((2L to 20L).reduceLeft(lcm) == 232792560L)
