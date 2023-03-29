#include <cmath>
#include <fstream>
#include <unistd.h>

#include "micro_timer.hpp"

int main(int argc, char *argv[])
{
	MicroTimer mt;

	std::cout << "\n The overhead bias is: " << mt.bias_ << " usecs \n";

	mt.start("sleep test .1");
	sleep(0.1);
	mt.stop("sleep test .1");
	
	mt.start("sleep test 1");
	sleep(1);
	mt.stop("sleep test 1");

	mt.start("sleep test 5");
	sleep(5);
	mt.stop("sleep test 5");
	
	mt.report();
	
}
