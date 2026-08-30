

__main__

auto announce = [&] (std::string name) -> std::string {
	std::cout
		<<"\n\n"
		<< "Hi "
		<< name
		<< ", I'm a C++ lambda function that is making an announcement.\n";
	return name;
 };

std::string name = "Johnny";
std::string new_name = announce(name);
std::cout << "\n Returned name is: " << new_name << "\n\n";

