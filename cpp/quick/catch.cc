
int clipper(int n) {

  if (n > 10) {
    throw std::runtime_error("Error n is > 10");
  } else {
    return n;
  }

}

int wrapper(int n) {

  int result;
  
  try {
    
    result = clipper(n);
      
  } catch(std::exception& e) {

    std::ostringstream console;
    
    console  << "System exception caught by wrapper():\n";
    console  << e.what();
    
    throw std::runtime_error(console.str());
    
  }
  
  return result;  
}

__main__


try {
  
  int n;

  std::cout << "\n Enter n: ";
  std::cin >> n;
  std::cout << "\n n is: " << n << "\n";
  std::cout << "\n calling clipper()";
  
  int x = wrapper(n);
  
  std::cout << "\n x = " << x << "\n";

 } catch(std::exception& e) {

  std::cout << "\nmain() exception): ";
  std::cout << "\nwhat: " << e.what();
  std::cout << "\n";

  exit(1);

 } // end try/catch.

