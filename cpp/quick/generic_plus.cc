
template <typename A, typename B>
auto generic_plus(A a, B b) -> decltype(a + b)
{
	return a + b;
}

__main__

auto c = generic_plus(3.0, 4.0);
std::cout << "\n c = " << c << "\n";

std::string d = "dog";
std::string z = "zebra ";

auto s = generic_plus(z, d);
std::cout << "\n s = " << s << "\n";



