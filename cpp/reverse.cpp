#include <iostream>
#include <string>

using namespace std;

// reverse a C++ string.
std::string reverse_string(const std::string& s)
{
  int size = s.size();
  std::string rev_string;
  rev_string.resize(size);

  for(int i = 0; i < size; i++) {
    rev_string[i] = s[(size-i) - 1];
  }

  return rev_string;
}

int main(int argc, char *argv[])
{
  std::string s = "hello world!";
  std::string rs = reverse_string(s);

  std::cout << "\n size of original: " << s.size();
  std::cout << "\n size of reversed: " << rs.size(); 
  
  cout << "\n reversed: " << rs.c_str();
  cout << "\n\n";
}
