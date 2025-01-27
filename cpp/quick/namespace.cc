
double mass = 9e13;
namespace galaxy {
  double mass = 4e9;
  namespace solar_system {
    double mass = 4e5;
    namespace planet {
      double mass = 2e3;
    }
  }
}

__main__

std::cout << "\n The mass of the universe is: " << ::mass;
std::cout << "\n The mass of the galaxy is: " << galaxy::mass;
std::cout << "\n The mass of the solar_system is: " << galaxy::solar_system::mass;
std::cout << "\n The mass of the planet is: " << galaxy::solar_system::planet::mass;

