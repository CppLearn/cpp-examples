
#include <iterator>

__main__

std::vector<std::string> poem = {"mickey", "mouse", "was", "in", "the", "house"};

auto poem_it = poem.begin();
std::cout << "\n :: " << *poem_it;

poem_it = std::next(poem_it);
std::cout << "\n :: " << *poem_it;

poem_it = std::next(poem_it);
std::cout << "\n :: " << *poem_it;

poem_it = std::next(poem_it);
std::cout << "\n :: " << *poem_it;

poem_it = std::next(poem_it);
std::cout << "\n :: " << *poem_it;

poem_it = std::next(poem_it);
std::cout << "\n :: " << *poem_it;


