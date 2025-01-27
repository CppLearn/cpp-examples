#include <iostream>
#include <memory>

class Base {
public:
    virtual ~Base() {
        std::cout << "Base destructor\n";
    }

    virtual void whoami() {
        std::cout << "Base\n";
    }
};

class Derived : public Base {
public:
    ~Derived() override {
        std::cout << "Derived destructor\n";
    }

    void whoami() override {
        std::cout << ":: I am Derived!\n";
    }
};

__main__

std::vector< std::unique_ptr<Base> > mother_ship;

// std::unique_ptr<Base> ptr = std::make_unique<Derived>();
// ptr->whoami();

mother_ship.push_back(std::make_unique<Derived>());

for(const auto& ship : mother_ship) {
	ship->whoami();
 }




