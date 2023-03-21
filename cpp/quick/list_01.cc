
__main__

std::list<int> primes = {3, 5, 11, 13};

util::print_vector(primes, "primes");

auto five_spot = std::find(begin(primes), end(primes), 5);
primes.insert(five_spot, 7);  // This will insert before 5.

util::print_vector(primes, "primes");

primes.erase(five_spot);
util::print_vector(primes, "primes");








