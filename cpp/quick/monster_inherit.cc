
class Weapon {
public:
	Weapon() = default;
private:
	int damage_;
};

template <typename T>
class Monster {
public:
	Monster() = default;
	virtual ~Monster() = default;
	virtual T get_weapon() = 0;
	virtual T get_name() = 0;
	virtual T get_height() = 0;
private:
	std::string name_;
	int height_feet_;
	T weapon_;
};

__main__

Monster<SWORD> m;



