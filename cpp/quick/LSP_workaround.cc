
struct Bug {
	virtual void identify() {
		std::cout << "\n I'm a basic bug. :( \n";
	};
};

struct GrassHopper : public Bug {
		void identify() override {
			std::cout << "\n I'm a hopper! \n";
}
	
};

void identify(Bug& b) {

	// if dynamic_cast fails for a pointer, it returns nullptr, that's
	// why it's used instead of a dynamic cast to a reference.
	
	if (dynamic_cast<GrassHopper *>(&b)) {
		std::cout << "\n Woah. We're dealing with a grasshopper here! \n";
	} else {
		b.identify();
	}
	
}

__main__

Bug fred;
GrassHopper phil;

identify(phil);
identify(fred);




