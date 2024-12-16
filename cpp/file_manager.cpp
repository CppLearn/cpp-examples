#include <exception>
#include <fstream>
#include <map>

class FileManager {
public:
  void add_file(int file_no, std::string filepath)
  {
    std::ifstream *new_file = new std::ifstream();
    try {
      // new_file->exceptions(std::ifstream::failbit | std::ifstream::badbit);
      new_file->open(filepath, std::ios::in);
    } catch (const std::ios_base::failure& e) {
      std::cerr << "Error opening/reading file: " << e.what() << std::endl;
      exit(1);
    } catch (const std::exception& e) {
      std::cerr << "Unexpected error: " << e.what() << std::endl;
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

private:
  std::map<int, std::ifstream*> file_map_;
};

