
__main__

std::vector<double> probs = {0.23, 1.0, 0.42, 0.72, 0.115, 0.320};


std::cout << "\n";
for (const auto& p : probs) {
	std::cout << " " << p;
 }

std::cout << "\n";

auto last = *(probs.end()-1);

std::cout << "\n Last item: " << last << "\n";


