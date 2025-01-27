
__main__

bool keep_going = true;
std::string ans;

while (keep_going) {

	int rand_num = rand() % 100;	
	std::cout << "\n rand = " << rand_num << std::endl;
	std::cout << "\n keep generating random nums?: ";
	std::cin >> ans;
	size_t found = -1;
	found = ans.find("y");
	if (found >= ans.length()) {
		keep_going = false;
	}	

 }
