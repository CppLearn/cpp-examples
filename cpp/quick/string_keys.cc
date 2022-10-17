
//  ---------------------------------------------------------------  //
//  These are valid keys (parameter names). Each value parameter     //
//  must come after one of these keys in the call to the function.   //
//  ---------------------------------------------------------------  //

using ParamKeys = std::list<std::string>;
ParamKeys keys = {"friction", "init_velocity", "gravity"};


//  ---------------------------------------------------------------  //
//  Small function to make sure a key is in our list.                //
//  ---------------------------------------------------------------  //

bool InKeys(ParamKeys keys, std::string k) {
	return (std::find(begin(keys), end(keys), k) != end(keys));
}
