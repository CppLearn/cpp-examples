
class Monster {
	std::string name_;
public:

	// This constructor takes an rvalue reference
	Monster(std::string&& name) 
		: name_(std::move(name)) // "Steals" the memory directly into name_
	{}

	void ShowName() const {
		std::cout << "\n Monster Name: " << name_ << "\n\n";
	}
	
};

__main__

Monster m("Hi. My name is VOlgor the mountain monster.");
m.ShowName();


