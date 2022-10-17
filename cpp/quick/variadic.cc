
//  ---------------------------------------------------------------  //
//  Variadic Template example showing how to handle a variable       //
//  number of arguments of different types.                          //
//  ---------------------------------------------------------------  //

//  ---------------------------------------------------------------  //
//  base case of recursion when we're down to a single parameter.    //
//  ---------------------------------------------------------------  //
	
template <typename T>
void MyAlgo(T t) {
	std::cout << "\n last param = " << t;
}

//  ---------------------------------------------------------------  //
//  Version with variadic template (multiple arguments) the          //
//  variable M is known as the "parameter pack".                     //
//  ---------------------------------------------------------------  //

template <typename T, typename ...M>
void MyAlgo(T t, M ...m) {

	std::cout << "\n num params left: " << sizeof...(m);
	std::cout << "\n param = " << t;
	MyAlgo(m...);

}

__main__

MyAlgo("friction", 2.032f,
			 "fuel", 3.141f,
			 "gravity", -20.5);

