#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

using namespace std;

typedef vector<string>::size_type vec_sz;

int main()
{

// create empty vector for strings
  vector<string> sentence;
  
// reserve memory for five elements to avoid reallocation

  sentence.reserve(5);

// append some elements

  sentence.push_back( "Hello," );
  sentence.push_back( "how" );
  sentence.push_back( "are" );
  sentence.push_back( "you" );
  sentence.push_back( "?" );

// print elements separated with spaces

  copy (sentence.begin(), sentence.end(),
	ostream_iterator<string>( cout," " ) );
  cout << endl;

  vec_sz size = sentence.size();

// print "technical data"

  cout << "  max_size(): " << sentence.max_size() << endl;
  cout << "  size():     " << size << endl;
  cout << "  capacity(): " << sentence.capacity() << endl;



  // swap second and fourth element

  swap( sentence[1], sentence[3] );
  
  // insert element "always" before element "?"

  sentence.insert ( find( sentence.begin(), sentence.end(), "?" ),
		    "always" );

  // assign "!" to the last element

  sentence.back() = "!";

  // print elements separated with spaces

  copy ( sentence.begin(), sentence.end(),
	 ostream_iterator<string>( cout, " " ) );
  cout << endl;

  // print "technical data" again

  cout << "  max_size( ): " << sentence.max_size() << endl;
  cout << "  size():      " << sentence.size()     << endl;
  cout << "  capacity():  " << sentence.capacity() << endl;



}

