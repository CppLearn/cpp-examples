
class Weapon {
	
public:
  Weapon() = default;
	Weapon(std::string name,
				 int damage) : name_{name}, damage_{damage} {};
protected:
  std::string name_;
  int damage_;
};

class Handheld : public Weapon {

public:	
  Handheld(const std::string& name,
					 const int damage = 10,
					 const bool two_handed = false) : Weapon{name, damage},
																						two_handed_{two_handed} {};
private:
  bool two_handed_ = false;
  
}; // end Handheld.


class Monster {
	
public:
  Monster() = default;
	Monster(std::string name, int height_feet) : name_{name},
																							 height_feet_{height_feet} {};
  virtual void add_weapon(const Weapon w) {
    weapons_.push_back(w);
  }
  virtual ~Monster() = default;
  virtual std::string get_name() = 0;
  int get_height() {return height_feet_;};
protected:
  std::string name_;
  int height_feet_;
  std::vector<Weapon> weapons_;
};



class Goblin : public Monster {
	
public:
	Goblin(std::string name, int height_feet) : Monster{name, height_feet} {};
	
  std::string get_name() override {
    std::cout << "\n :: Goblin :: get_name() \n" << "\n";
		return name_;
  }

	void laugh() {
		std::cout << "\n :: Goblin :: \n";
		std::cout << "\n Goblin laughing :D :D :D !!!";
		std::cout << "\n\n";
	}
 
};
 
__main__

Handheld sword("Wishbringer",
							 50,
							 true);

Goblin goblin_01("googa", 4);

goblin_01.add_weapon(sword);

std::string goblins_name = goblin_01.get_name();
std::cout << "\n goblin's name: " << goblins_name << "\n";

goblin_01.laugh();

