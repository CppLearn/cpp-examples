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
  std::string flags = "";

  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <cpp filename> \n";
    exit(0);
  }

  // Process args.
  
  int i = 1;
  while(i < argc) {		
    std::string arg = argv[i];
    if (arg == "--flags") {
      flags = argv[++i];
    } else if (arg == "--keep-src") {
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
  
  code.push_back("");  
	
  // We add common utils:: namespace for common display utils.
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
      code.push_back("\n");
      code.push_back("  std::vector<std::string> args;");
      code.push_back("  // use a strange var name to reduce");
      code.push_back("  // chance of collision with user var.");
      code.push_back("  int arg_index_00_ = 1;" );
      code.push_back("  while(arg_index_00_ < argc) {" );
      code.push_back("    args.push_back( argv[arg_index_00_] );" );
      code.push_back("    arg_index_00_++;" );
      code.push_back("  }" );			
      code.push_back(" ");
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
	
  std::cout << "\n == Running C++ ==  " << cpp_fname << "\n";
  std::cout << "keep src [" << keep_src_str << "] keep exe [" << keep_exe_str << "]\n";
  
  std::string cmd = "g++ -O0 -pg -std=c++11 ";
	if (flags != "") {
		cmd = cmd + flags + " ";
	}
  cmd = cmd + tmp_path + " -o " + cpp_exe;
  std::cout << "\n Executing => " << cmd << "\n";
  
  system( cmd.c_str() );
  cmd = "./" + cpp_exe;
  system( cmd.c_str() );
  
  std::cout << "\n\n";
	
  // remove tmp src, unless flag.
  if ( !keep_src ) {
    cmd = "rm -rf " + tmp_path;
    std::cout << " " << cmd << "\n" << std::flush;
    system( cmd.c_str() );
  }
	
  // save exe if --keep_exe
  if ( keep_exe ) {
    cmd = "cp -p " + cpp_exe + " ./" + cpp_fname + ".exe";
    std::cout << " " << cmd << "\n" << std::flush;
    system( cmd.c_str() );
  }

  // remove tmp exe
  cmd = "rm -rf " + cpp_exe;
  std::cout << " " << cmd << "\n" << std::flush;
  system( cmd.c_str() );
  
  std::cout << "\n" << std::flush;
}

