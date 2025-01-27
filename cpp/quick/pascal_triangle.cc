
std::vector<int> get_next_row(const std::vector<int>& last_row)
{
	std::vector next_row{1};
	if (last_row.empty()) {
		return next_row;
	}
	
	for (size_t idx = 0; idx+1 < last_row.size(); ++idx) {
		next_row.emplace_back(last_row[idx] + last_row[idx + 1]);
	}
	next_row.emplace_back(1);
	
	return next_row;
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

std::vector<int> next_row;
std::vector<std::vector<int>> triangle;

std::cout << "\n Enter # of rows you want: ";

std::cin >> nrows;

for(int i = 0; i < nrows; i++) {
	std::vector<int> next_row = get_next_row(next_row);
	triangle.push_back(next_row);
 }

for(size_t i = 0; i < triangle.size(); i++) {
	display(triangle[i]);
 }



 





