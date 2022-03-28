#include <iostream>
#include <string>
#include <iterator>

std::string rev_string(std::string& s) {
  std::string new_str;
  new_str.resize(s.size());

  int i = 0;
  for (std::string::reverse_iterator ri = s.rbegin(); ri != s.rend(); ri++) {
    new_str[i] = *ri;
    i++;
  }

  return new_str;
}

int main(int argc, char *argv[])
{
  std::string input;
  
  std::cout << "\n enter string: ";
  std::cin >> input;
  std::cout << "\n you entered: " << input << std::endl;
  std::string rev_str = rev_string(input);
  std::cout << "\n\n the reversed string is: " << rev_str << std::endl; 
}

