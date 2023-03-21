#include <iostream>
#include <vector>
#include <numeric>

int main(int argc, char* argv[]) {
  const unsigned int max_prime = 2000000;
  std::vector<unsigned int> primes;
  primes.push_back(2);

  for (unsigned int i = 3; i <= max_prime; i += 2) {
    bool is_prime = true;
    for (const auto prime : primes) {
      if (prime * prime > i) break;
      if (i % prime == 0) {
        is_prime = false;
        break;
      }
    }
    if (is_prime) primes.push_back(i);
  }

  unsigned long long result = std::accumulate(primes.cbegin(), primes.cend(), 0LL);
  std::cout << result << std::endl;

  return 0;
}
