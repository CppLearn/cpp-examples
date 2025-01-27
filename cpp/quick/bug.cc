
// This is the Insect base class.

class Insect {
public:
	Insect(const int nlegs) : num_legs_{nlegs} {};
protected:
	int num_legs_;
};

// This is the bug derived class.

class Bug : Insect {

public:
	Bug(std::string name, std::string color) : Insect(6), name_{name}, color_{color} {}
  
	inline void display() {
		std::cout << "\n Bug: ";
		std::cout << "\n     name: " << name_;
		std::cout << "\n     color: " << color_ << std::endl;
		std::cout << "\n     num legs: " << num_legs_;
		std::cout << "\n";
	}
  
private:
	std::string name_;
	std::string color_;
	
};

__main__

Bug b("charlie", "blue");
b.display();

