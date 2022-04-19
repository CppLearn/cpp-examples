#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <string>

std::string list_to_str(std::list<std::string> lines)
{
  std::string new_string = "";
  for(const auto& l : lines) {
    new_string += l;
    new_string += "\n";
  }
  return new_string;
}

namespace ui {
  std::map<std::string, std::string> color = { {"red", "\033[1;31m"},
                                               {"ured", "\033[4;31m"},
                                               {"bkred", "\033[41m"},
                                               //
                                               {"green", "\033[1;32m"},
                                               {"ugreen", "\033[4;32m"},
                                               {"bkgreen", "\033[42m"},
                                               //
                                               {"yellow", "\033[1;33m"},
                                               {"uyellow", "\033[4;33m"},
                                               {"bkyellow", "\033[43m"},
                                               //
                                               {"blue", "\033[1;34m"},
                                               {"ublue", "\033[4;34m"},
                                               {"bkblue", "\033[44m"},
                                               //
                                               {"magenta", "\033[1;35m"},
                                               {"umagenta", "\033[4;35m"},
                                               {"bkmagenta", "\033[45m"},
                                               //
                                               {"cyan", "\033[1;36m"},
                                               {"ucyan", "\033[4;36m"},
                                               {"bkcyan", "\033[46m"},
                                               //
                                               {"white", "\033[1;37m"},
                                               {"uwhite", "\033[4;37m"},
                                               {"bkwhite", "\033[47m"},
                                               {"reset", "\033[0m"} };
}  // end namespace ui

void add_utils(std::list<std::string>& code)
{
  code.push_back( "namespace util {" );
	code.push_back("");
  code.push_back( "  template <typename T>" );
  code.push_back( "  static void print_vector(const T& vec, const std::string& title = \"\")" );
  code.push_back( "  {"  );
  code.push_back( "    std::cout << \" ----- [\" << title << \"] -----\";" );
	code.push_back( "    std::cout << std::endl;" );
  code.push_back( "    for( const auto& v : vec ) {" );
  code.push_back( "      std::cout << v << \" \";" );
  code.push_back( "    }" );
  code.push_back( "    std::cout << std::endl;" );
  code.push_back( "  }" );
	code.push_back("");
	code.push_back( "  template<typename K, typename V>"  );
	code.push_back( "  static void print_map(const std::map<K, V>& m, const std::string& title = \"\" ) {"  );
  code.push_back( "    std::cout << \" ----- [\" << title << \"] -----\";" );
	code.push_back( "  std::cout << std::endl; " );
	code.push_back( "  for ( const auto& pair : m ) {"  );
	code.push_back( "    std::cout << \" {\" << pair.first << \": \" << pair.second << \"}\"; " );
	code.push_back( "    std::cout << std::endl; " );
	code.push_back( "  }" );
	code.push_back( " }" );
  code.push_back( "}  // end namespace util" );
}

void add_colormap(std::list<std::string>& code)
{
  code.push_back("namespace ui {");
	code.push_back("");
  code.push_back("  std::map<std::string, std::string> color = { {\"red\", \"\033[1;31m\"},");
  code.push_back("                                               {\"ured\", \"\033[4;31m\"},");
  code.push_back("                                               {\"bkred\", \"\033[41m\"},");
  code.push_back("                                               //");
  code.push_back("                                               {\"green\", \"\033[1;32m\"},");
  code.push_back("                                               {\"ugreen\", \"\033[4;32m\"},");
  code.push_back("                                               {\"bkgreen\", \"\033[42m\"},");
  code.push_back("                                               //");
  code.push_back("                                               {\"yellow\", \"\033[1;33m\"},");
  code.push_back("                                               {\"uyellow\", \"\033[4;33m\"},");
  code.push_back("                                               {\"bkyellow\", \"\033[43m\"},");
  code.push_back("                                               //");
  code.push_back("                                               {\"blue\", \"\033[1;34m\"},");
  code.push_back("                                               {\"ublue\", \"\033[4;34m\"},");
  code.push_back("                                               {\"bkblue\", \"\033[44m\"},");
  code.push_back("                                               //");
  code.push_back("                                               {\"magenta\", \"\033[1;35m\"},");
  code.push_back("                                               {\"umagenta\", \"\033[4;35m\"},");
  code.push_back("                                               {\"bkmagenta\", \"\033[45m\"},");
  code.push_back("                                               //");
  code.push_back("                                               {\"cyan\", \"\033[1;36m\"},");
  code.push_back("                                               {\"ucyan\", \"\033[4;36m\"},");
  code.push_back("                                               {\"bkcyan\", \"\033[46m\"},");
  code.push_back("                                               //");
  code.push_back("                                               {\"white\", \"\033[1;37m\"},");
  code.push_back("                                               {\"uwhite\", \"\033[4;37m\"},");
  code.push_back("                                               {\"bkwhite\", \"\033[47m\"},");
  code.push_back("                                               {\"reset\", \"\033[0m\"} };");
	code.push_back("");	
  code.push_back("}  // end namespace ui");
} 

std::string bool_to_str(const bool& b) {
	std::string s = b ? "true" : "false";
	return s;
}

bool find_str( const std::string& s, const std::string& expr ) {

  auto loc = s.find(expr);
  return (loc != std::string::npos);
    
}

int main( int argc, char *argv[] )
{
  std::string cpp_fname;
	bool cpp_fname_set = false;
  std::string cpp_exe = "quick_cpp.exe";
  bool keep_src = false;
	bool keep_exe = false;

	if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <cpp filename> \n";
    exit(0);
  }
	
	int i = 1;
	while(i < argc) {		
		std::string arg = argv[i];
		if (arg == "--keep-src") {
			keep_src = true;
		} else if (arg == "--keep-exe") {
			keep_exe = true;
		} else {
			// only accept first filename;
			if (!cpp_fname_set) {
				cpp_fname = arg;
				cpp_fname_set = true;
			}
		} // end if arg.

		i++;
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
  code.push_back("#include <math.h>");
	code.push_back("#include <memory>");
  code.push_back("#include <numeric>");
	code.push_back("#include <random>");
  code.push_back("#include <set>");
  code.push_back("#include <sstream>");
  code.push_back("#include <string>");
  code.push_back("#include <thread>");
	code.push_back("#include <type_traits>");
  code.push_back("#include <vector>");

  // We make ui::color available by adding it to the generated code
  // for convenience for our quick c++ code in case they want to use colors.
	code.push_back("");  
  add_colormap(code);
	
  // We also add common utils:: namespace for common display utils.
	code.push_back("");  
  add_utils(code);
	code.push_back("");
	code.push_back("");
	
  std::ifstream cpp_source; // file containing cpp source code.
  std::string line;
  cpp_source.open( cpp_fname );
	
  std::size_t loc;
  while ( !cpp_source.eof() ) { // read cpp source file.

    getline( cpp_source, line );
    
    // Look for our special instructions here.
    if ( find_str( line, "__stop__" ) ) {
      break;
    } else if ( find_str( line, "__main__" ) ) {
      code.push_back("int main(int argc, char *argv[])");
      code.push_back("{");
    } else {
      code.push_back(line);
    }
  }

  // Finish up the C++ file!
	code.push_back("std::cout << std::endl;");
  code.push_back("}");

  // We are now ready to write out code to tmp source file.
  
  cpp_src << list_to_str(code);
  
  cpp_source.close();  // close C++ snipped file.
  cpp_src.close();     // close generated source file.
	
  // We are ready to compile and start showing output.

	std::string keep_src_str = bool_to_str(keep_src);
	std::string keep_exe_str = bool_to_str(keep_exe);
	
  std::cout << ui::color["yellow"];
  std::cout << "\n == Running C++ ==  " << ui::color["blue"] << cpp_fname << "\n";
  std::cout << ui::color["yellow"];
	std::cout << "keep src [" << keep_src_str << "] keep exe [" << keep_exe_str << "]\n";
  std::cout << ui::color["reset"];
  
  std::string cmd = "g++ -std=c++11 ";
  cmd = cmd + tmp_path + " -o " + cpp_exe;
  
  std::cout << ui::color["green"];
  std::cout << "\n Executing => " << cmd << "\n";
  std::cout << ui::color["white"] << std::flush;
  
  system( cmd.c_str() );
  cmd = "./" + cpp_exe;
  system( cmd.c_str() );
  
  std::cout << ui::color["green"] << std::flush;
	std::cout << "\n\n";
	
  // remove tmp src, unless flag.
	if ( !keep_src ) {
    cmd = "rm -rf " + tmp_path;
    std::cout << " " << cmd << "\n" << std::flush;
    system( cmd.c_str() );
  }
	
  // remove exe, unless flag.
	if ( !keep_exe ) {
		cmd = "rm -rf " + cpp_exe;
		std::cout << ui::color["red"] << " " << cmd << "\n" << std::flush;
		system( cmd.c_str() );
	}
	
  std::cout << ui::color["reset"] << "\n" << std::flush;
}

