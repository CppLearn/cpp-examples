
#include <array>
#include "micro_timer.hpp"

//  ---------------------------------------------------------------  //
//  NanoTimer implementation file.                                   //
//  ---------------------------------------------------------------  //

double MicroTimer::benchmark(const int num_trials) {

	double sum_trials = 0.0;
	for(unsigned i = 0; i < num_trials; ++i) {
		Timer t;
		t.stop();
		sum_trials +=
			std::chrono::duration_cast<std::chrono::nanoseconds>((t.stop_ - t.start_)).count() / 1000.0;
	}

	// compute overhead (bias)	
	return sum_trials / num_trials;
	
}

void MicroTimer::calc() {
	
	//  ---------------------------------------------------------------  //
	//  Calculation of duration is done by MicroTimer not in Timer       //
	//  object.                                                          //
	//  ---------------------------------------------------------------  //
	
	for (auto& t : timers_) { // loop over timers map.
		
		//
		// t.first is the timer name.
		// t.second is the actual timer. t.second is a unique_ptr, therefore
		// we can't copy it to a temp for clarity, so we use it in place.
		//
		
		//  ---------------------------------------------------------------  //
		//  This computation for microsecond timing is from:                 //
		//  Gottschling, P. (2020). Addison-Wesley.                          // 
		//  Discovering modern C++ an intensive course for scientists...     //
		//  ---------------------------------------------------------------  //
		
		t.second->micros_ =
			(std::chrono::duration_cast<std::chrono::nanoseconds>
			 ((t.second->stop_ - t.second->start_)).count() / 1000.0) - bias_;
		}
	
}

void MicroTimer::report() {

	this->calc(); // calculate duration for timers.

	// Print out each timer.
	for (auto& t : timers_) { // loop over timers map.
		std::cout << "\n [microtimer] " << t.first << " = " << t.second->micros_ << " usecs.";
	}
	std::cout << "\n\n";
	
}








