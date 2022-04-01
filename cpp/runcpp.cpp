#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <list>

std::string list_to_str(std::list<std::string> lines)
{
  std::string new_string = "";
  for(const auto& l : lines) {
    new_string += l;
		new_string += "\n";
  }
  return new_string;
}


int main( int argc, char *argv[] )
{
  std::string cpp_fname;
  std::string cpp_exe = "quick_cpp.exe";
  bool keep_file = false;
  
  if ( argc == 2 || argc == 3 ) {
    cpp_fname = argv[1];
    keep_file = ( argc == 3 ) ? true : false;
  } else {
    std::cout << "Usage: " << argv[0] << " <cpp filename> \n";
    exit(0);
  }
  
  std::string tmp_path = std::tmpnam(nullptr);
  tmp_path = "." + tmp_path; // make tmpfile local in ./tmp
                             // (tmp is intrisic to this function.)
  tmp_path = tmp_path + ".cpp";
  
  std::ofstream cpp_src;
  cpp_src.open( tmp_path );
	
  const std::string header;

  std::list<std::string> code;

  code.push_back("#include <algorithm>");
	code.push_back("#include <array>");
	code.push_back("#include <chrono>");
	code.push_back("#include <cstdio>");
	code.push_back("#include <cstdlib>");
  code.push_back("#include <fstream>");
	code.push_back("#include <functional>");
	code.push_back("#include <iomanip>");
  code.push_back("#include <iostream>");
  code.push_back("#include <list>");
	code.push_back("#include <map>");
	code.push_back("#include <numeric>");
	code.push_back("#include <set>");
	code.push_back("#include <sstream>");
  code.push_back("#include <string>");
	code.push_back("#include <thread>");
  code.push_back("#include <vector>");
  code.push_back("\n");
       
  std::ifstream cpp_source; // file containing cpp source code.
  std::string line;
  cpp_source.open( cpp_fname );

	std::size_t loc;
  while ( !cpp_source.eof() ) { // read cpp source file.
    getline( cpp_source, line );
		loc = line.find("__main__");
		if (loc != std::string::npos) {  // string not found,
			code.push_back("\nint main(int argc, char *argv[])");
			code.push_back("\n{\n");
		} else {
			code.push_back(line);
		}
  }
	
  code.push_back("\n");
  code.push_back("}\n");

	// We are now ready to write out code to tmp source file.
	
	cpp_src << list_to_str(code);
	
	cpp_source.close();  // close C++ snipped file.
	cpp_src.close();     // close generated source file.
	
  std::string cmd = "g++ -std=c++11 ";
  cmd = cmd + tmp_path + " -o " + cpp_exe;
  std::cout << "\n Executing => " << cmd << "\n";
  system( cmd.c_str() );
  
  cmd = "./" + cpp_exe;
  system( cmd.c_str() );
  std::cout << "\n ... \n";

	// remove temp exe
	cmd = "rm -rfv " + cpp_exe;
	system( cmd.c_str() );

	// remove temp source (unless argc == 3)
	if ( !keep_file ) {
    cmd = "rm -rfv " + tmp_path;
    system( cmd.c_str() );
  }
	
	
}

