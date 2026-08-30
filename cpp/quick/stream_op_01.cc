#include <iostream>

class Person {
private:
	std::string name;
	int age;
	
public:
	Person(std::string n, int a) : name(n), age(a) {}
	
	std::string get_name() const {
		return name;
	}
	int get_age() const {
		return age;
	}
	
};

// 2. Define the function outside the class body
// 3. It doesn't need to be friend because it has public getters.

std::ostream& operator<<(std::ostream& os, const Person& p) {
	// Write data to the stream
	os << "Name: " << p.get_name() << ", Age: " << p.get_age();
    
	// Always return the stream to allow for chain loading (e.g., cout << p1 << p2;)
	return os;
}

__main__

Person bob("Bob", 30);
std::cout << "\n" << bob << "\n" << std::endl; // Output: Name: Bob, Age: 30
return 0;

