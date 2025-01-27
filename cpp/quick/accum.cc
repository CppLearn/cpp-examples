
__main__

std::vector<double> v = {1.0, 2.0, 3.2, 4.0};

// accumulate between 1 and 2, init sum with 3.
double sum = std::accumulate(v.begin(), v.end(), 5.0);

std::cout << "\n sum = " << sum << "\n\n";



