
struct RNG {
	explicit RNG( uint32_t seed ) : iterations{}, number {seed} {}
	uint32_t next();
	size_t get_iterations() const;
private:
	size_t iterations;
	uint32_t number;
};

uint32_t RNG::next() {
	++iterations;
	number = 0x3FFFFFFF & (0x41C64E6D * number + 12345) % 0x8000000;
	return number;
}

size_t RNG::get_iterations() const {
	return iterations;
}

__main__

RNG rng{ 0x4c4347 };
while ( rng.next() != 0x474343 ) {
	// do nothing
	std::cout << ".";
 }

std::cout << "\n iterations = " << rng.get_iterations() << std::endl;

