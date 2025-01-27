
struct animal {
	animal() = default;
	int age = 5;
};

using Yard = std::vector<std::unique_ptr<animal>>;

__main__

Yard backyard;
animal dog;

; std::cout << "\n This animal is " << dog.age << " years old.\n";

std::unique_ptr<animal> uptr = std::make_unique<animal>();
std::cout << "\n This animal is " << dog.age << " years old.\n";




