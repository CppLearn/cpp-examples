#include <algorithm>
#include <iterator>

using Triangle = std::vector<std::vector<int>>;

std::vector<int> get_next_row(const std::vector<int>& last_row)
{
	std::vector next_row{1};
	if (last_row.empty()) {
		std::cout << "\n The last row you push in was empty.";		
		return next_row;
	} else {
		std::cout << "\n Last row was not empty.";		
	}
	
	for (size_t idx = 0; idx+1 < last_row.size(); ++idx) {
		next_row.emplace_back(last_row[idx] + last_row[idx + 1]);
	}
	next_row.emplace_back(1);
	
	return next_row;
}

/*
auto generate_triangle(int rows)
{
	std::vector<int> data;
	std::vector<std::vector<int>> triangle;

	for (int row = 0; row < rows; ++row) {
		data = get_next_row(data);
		triangle.push_back(data);
	}
	return triangle;
}
*/
 
auto generate_triangle(int rows)
{
	Triangle triangle{ {1} };
	for (int row = 1; row < rows; ++row) {
		triangle.push_back(get_next_row(triangle.back()));
	}
	return triangle;
}

void display(std::vector<int> row)
{
	std::cout << "\n";
	for(auto& e: row) {
		std::cout << " " << e;
	}
}
		 
__main__

int nrows;

std::cout << "\n Enter # of rows you want: ";
std::cin >> nrows;

auto triangle = generate_triangle(nrows);
for(size_t i = 0; i < triangle.size(); ++i) {
	display(triangle[i]);
 }

std::cout << "\n";








