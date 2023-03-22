
#include <chrono>

//  ---------------------------------------------------------------  //
//  Custom square root using Babylonian method!                      //
//  ---------------------------------------------------------------  //

inline double bab_root(double x, double eps = 1e-12) {

	double sq = 1.0, sqo;
	
	do {
		sqo = sq;
		sq = 0.5 * (sqo + x / sqo);
	} while ( abs(sq - sqo) > eps);
	
	return sq;
	
} // end bab_root.

__main__

double ans = bab_root(2.0);

std::cout << "\n The sqrt of 2.0 is approx: " << ans << "\n";

std::chrono::time_point<std::chrono::steady_clock> start = std::chrono::steady_clock::now();

double r3;

int rep = 3;
for (int i = 0; i < rep; ++i) {
	r3 = bab_root(3.0);
 }

auto end = std::chrono::steady_clock::now();

std::cout << "\n bab_root(3.0) = " << r3 << ", the calculation took " << std::chrono::duration_cast<std::chrono::nanoseconds>((end - start) / rep).count() / 1000. << " microsecs \n";


