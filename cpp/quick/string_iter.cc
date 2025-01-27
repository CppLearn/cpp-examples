
__main__

std::string s = "titanic";
std::vector<char> vec_string;

std::cout << "length of s: " << s.size() << "\n";

for(const auto& c : s) {
	std::cout << "\n c = " << c;
	vec_string.push_back(c);
}




	
