
__main__

std::default_random_engine engine;

// create distribution with mean and stddev.
double mean = 10.0;
double stddev = 3.0;

std::normal_distribution<double> dist(mean, stddev);

// create 10 random values with that distribution.
std::vector<float> rand_values;
int i = 10;
while (i) {
  rand_values.push_back(dist(engine));
  i--;
 }

util::print_vector(rand_values, "rand vector");


