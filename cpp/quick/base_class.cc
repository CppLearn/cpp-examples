
struct Primate {
	virtual void identify() = 0;	
};

struct Human : Primate {
	void identify() { std::cout << "\n I'm a human!\n"; };
};

struct Gorilla : Primate {
	void identify() { std::cout << "\n I'm a gorilla!\n"; };
	
	template<class T>
	typename std::enable_if<std::is_base_of<Primate, T>::value, T>::type
	interact_with(T animal) {
		T offspring;
		return offspring;
	}

};

struct Bird {};

__main__

Gorilla Grok;
Human Rick;
// Bird big_bird;

auto offspring = Grok.interact_with<Human>(Rick);
offspring.identify();

// Will get compile errors because big_bird isn't a primate.
// Bird big_bird;

// auto offspring = Grok.interact_with<Bird>(big_bird);




