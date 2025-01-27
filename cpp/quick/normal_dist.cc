#include <fstream>

__main__

std::ofstream out_file;
std::default_random_engine engine;

// create distribution with mean and stddev.
double mean = 0.0;
double stddev = 3.0;

std::normal_distribution<double> dist(mean, stddev);

// create 10 random values with that distribution.
std::vector<float> rand_values;
int i = 1000;
while (i) {
  rand_values.push_back(dist(engine));
  i--;
 }

util::print_vector(rand_values, "rand vector");

out_file.open("centaur.dat");
for (const auto& v : rand_values) {
  out_file << "\n" << v;
 }

out_file.close();


// gnuplot> binwidth = 0.2
// gnuplot> bin(x, width) = width * floor(x/width)
// gnuplot> plot "centaur.dat" using (bin($1, binwidth)):(1.0) smooth freq with boxes


	
