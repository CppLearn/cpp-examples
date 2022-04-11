
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

	mate_with(T animal) {
		T baby;
		return baby;
	}

};

struct Bird {};

__main__

Gorilla Grok;
Human Rick;

auto baby = Grok.mate_with<Human>(Rick);
baby.identify();

// Will get compile errors because big_bird isn't a primate.
// Bird big_bird;
// auto baby2 = Grok.mate_with<Bird>(big_bird);



