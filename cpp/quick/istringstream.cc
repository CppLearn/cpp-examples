


__main__

std::string data("Frodo 81104 Shire");
std::istringstream iss(data);
std::string name, zip, city;

iss >> name >> zip >> city;

std::cout << "\n Name: " << name;
std::cout << "\n Zip Code: " << zip;
std::cout << "\n City: " << city;






