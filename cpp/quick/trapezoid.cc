
/*
double exp3t(double x) {
	return std::exp(3.0 * x);
}
*/

struct exp3t {
	double operator() (double x) {
		return std::exp(3.0 * x);
	}
};


/*
Suppose we want to integrate the function f over interval [a,b].
Split the interval into n intervals [xi, xi+1] each of length: dx = (b-a)/n
integral [a,b] f(x) dx = dx * sum(i=1..n-1) f(a + i*dx) + dx/2*f(a) + dx/2*f(b)

Integrate exp(3x) for x in [0, 4]

*/

// Trapezoid Integral Solver
template <typename F>
class Trapezoid {

public:
	Trapezoid(F& f) : f_(f) {
		std::cout << "\n A Trapezoid was constructed!" << "\n";
	};

	void say_hello() {
		std::cout << "\n Qbert* was here!!" << "\n";
	}
	
	inline double foo(double a, double b) {
		return a + b;
	}
	double operator() (double a, double b) {
		
	}
		
private:
	F& f_;  // the function to integrate
};

__main__

exp3t myfunction;

Trapezoid<exp3t> t(myfunction);

t.say_hello();
double s = t.foo(3.0, 9.0);
std::cout << "\n result = " << s;












