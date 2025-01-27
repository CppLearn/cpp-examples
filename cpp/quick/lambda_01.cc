
template<typename F>
void DescribeCreature(F&& lambda, std::string creature) {
	lambda(creature);
}

__main__

std::vector<std::string> creatures = {"dog", "cat", "mouse", "dragon"};
std::vector<std::string> output;

for( const auto& c : creatures ) {
	DescribeCreature( [](const std::string& c)
										{ std::cout << "\n >> " << c; }, c);
 }


std::cout << "\n";

