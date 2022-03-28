#include <iostream>
#include <string>

using std::cin;
using std::cout;
using std::endl;
using std::string;

int main()
{

  cout << "Please enter your first name: ";
  
  string name;
  cin >> name;

  const string greeting = "Hello, " + name + "!";
  
  // The number of blanks surrounding the greeting.

  const int pad = 4;

  // The number of rows and columns to write.
  // pad on either side gives us *2
  // the greeting plus the 2 borders gives us + 3.

  const int rows = (pad * 2) + 3;

  // The padding on either size gives us *2
  // The borders gives us +2.

  const string::size_type cols = greeting.size() + (pad * 2) + 2;

  cout << endl;

  // Loop over # of rows.

  for(int r = 0; r < rows; ++r){
    
    string::size_type c = 0;
    
    // invariant: We ave written c chars so far
    while(c != cols) {
      
      // is it time to write the greeting ?
      
      if( (r == pad + 1) && (c == pad + 1) ) {
	cout << greeting;
	c += greeting.size();
      } else {
	
	// are we on the border ?
	
	if ((r == 0) || (r == rows - 1) || (c == 0) || (c == cols-1)) {	  
	  cout << "*";
	} else {
	  cout << " ";
	}

	c++;  // We wrote out one character.

      }	// end else not greeting

    } // end while c != cols

    cout << endl; // write newline between rows.

  } // end for rows
  
  return 0;

}
      
  
  
