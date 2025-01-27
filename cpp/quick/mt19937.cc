
__main__

//  ---------------------------------------------------------------  //
//  mt19937 is pseudo-random number generator that uses the          //
//  Mersenne twister algorithm.                                      //
//  ---------------------------------------------------------------  //

std::mt19937 engine;
std::normal_distribution<double> normal_dist;


std::cout << "\n ___ Normal distribution random value ___\n";

for(int i = 0; i < 50; i++) {
	double rand_value = normal_dist(engine);
	std::cout << "\n " << rand_value;
 }




