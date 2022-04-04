
class Bug {

public:
	Bug(std::string name, std::string color) : name_{name}, color_{color}, num_legs_{0} {};
  
	inline void display() {
		std::cout << ui::color["white"];
		std::cout << "\n Bug: ";
		std::cout << "\n     name: " << name_;
		std::cout << ui::color["green"] << "\n     color: " << color_ << ui::color["reset"] << std::endl;
		std::cout << "\n     num legs: " << num_legs_;
		std::cout << "\n";
	}
  
private:
	std::string name_;
	std::string color_;
	int num_legs_;
};

__main__

Bug b("charlie", "blue");
b.display();

