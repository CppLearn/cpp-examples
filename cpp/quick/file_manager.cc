#include <exception>
#include <fstream>
#include <sstream>
#include <map>

class FileManager {
public:
  void add_file(int file_no, std::string filepath)
  {
    std::ifstream *new_file = new std::ifstream();
		
    try {
      new_file->open(filepath, std::ios::in);
			if (!new_file->is_open()) {
				std::ostringstream msg;
				msg << " [error] could not open file: " << filepath << std::endl;
				throw std::runtime_error(msg.str());
			}					
    } catch (const std::exception& e) {
      std::cerr << e.what() << std::endl;
      exit(1);
    } catch (...) {
      std::cerr << "Unexpected error: " << std::endl;
      exit(1);
    }
    file_map_[file_no] = new_file;
  }

  void dump_file(const int file_no)
  {
    if (file_map_[file_no]->is_open()) {
      std::string line;
      try {
        while ( std::getline(*file_map_[file_no], line) ) {
          std::cout << "\n [dump] " << line;
        }
      } catch (const std::exception& e) {
        std::cerr << "EXCEPTION!: " << e.what() << std::endl;
        exit(1);
      }
    }
  }
    
  void close_file(const int file_no)
  {
    if (file_map_[file_no]->is_open()) {
      file_map_[file_no]->close();
    }
    if (file_map_[file_no]) {
      delete file_map_[file_no];
    }
    file_map_[file_no] = nullptr;
  }

	std::string read_next(const int file_no)
	{
		std::string value = "";
		std::string line;
		std::string token;

		while (value == "") {
			std::getline(*file_map_[file_no], line);
			std::istringstream istream(line);
			while (istream >> token) {
				if (token.find("!") != std::string::npos) {
					break;
				} else {
					value = token;
				}
			}
		}
		
		return value;
	}
	
	//  ---------------------------------------------------------------  //
	//  The following functions would use templates however the string   //
	//  conversion function is different.                                //
	//  ---------------------------------------------------------------  //
	
	double read_real(const int file_no)
	{
		double value;
		try {
			std::string value_str = read_next(file_no);
			value = std::stod(value_str);
		} catch (const std::exception& e) {
			std::cerr << "[exception] reading real from file unit: " << file_no << " exception: " << e.what() << std::endl;
		}
		
		return value;
	}

	double read_int(const int file_no)
	{
		int value;
		try {
			std::string value_str = read_next(file_no);
			value = std::stoi(value_str);
		} catch (const std::exception& e) {
			std::cerr << "[exception] reading int from file unit: " << file_no << " exception: " << e.what() << std::endl;
		}
		
		return value;
	}
	
private:
  std::map<int, std::ifstream*> file_map_;
};

__main__

std::string value;

fm.add_file(300, "./data/fortune.dat");
value = fm.read_next(300);
std::cout << "\n read value: " << value;
double dval = fm.read_real(300);
std::cout << "\n read value: " << dval;
 
fm.close_file(300);
fm.close_file(100);
fm.close_file(200);

