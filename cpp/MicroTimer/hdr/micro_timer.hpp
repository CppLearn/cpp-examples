#pragma once

#include <chrono>
#include <iostream>
#include <map>
#include <memory>
#include <string>

//  ---------------------------------------------------------------  //
//  MicroTimer Include File                                          //
//  ---------------------------------------------------------------  //

class MicroTimer {

public:
	
	MicroTimer() {
		std::cout << "\n Microtimer created! \n";
		bias_ = benchmark(1000);
	}
	~MicroTimer() = default;
	
	void start(const std::string& label) {
		std::cout << "\n [microtimer] starting " << label;
		timers_[label] = std::make_unique<Timer>();
	}
	
	void stop(const std::string& label) {
		timers_[label]->stop();
	}

	//  ---------------------------------------------------------------  //
	//  Run benchmark to compute bias.                                   //
	//  ---------------------------------------------------------------  // 
	double benchmark(const int num_trials);
	void calc();
	void report();

	// public member variables for MicroTimer	
	double bias_;
	
private:
	
	//  ---------------------------------------------------------------  //
	//  A MicroTimer creates Timer objects. A Timer object needs to be   //
	//  the smallest and fastest part of the implementation.             //
	//  ---------------------------------------------------------------  //
	
	struct Timer {
		
		Timer() {
			start_ = std::chrono::steady_clock::now();
		}
		Timer(const Timer& rhs) = default;
		Timer& operator=(const Timer& rhs) = default;
		
		void stop() {
			stop_ = std::chrono::steady_clock::now();
		}
		
		std::chrono::time_point<std::chrono::steady_clock> start_;
		std::chrono::time_point<std::chrono::steady_clock> stop_;
		double micros_; // usecs

	}; // end Timer

	// private members for MicroTimer
	
	std::map<std::string, std::unique_ptr<Timer>> timers_;
	
};

