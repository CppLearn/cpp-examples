#include <sstream>

__main__

float velocity = 24.5f;

std::stringstream s;
s << "\n launch velocity: " << velocity;

std::cout << s.str() << "\n";

