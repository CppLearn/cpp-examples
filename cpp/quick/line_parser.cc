
#include <sstream>
#include <iostream>
#include <string>
#include <fstream>
#include <vector>

class CLineParser {

public:

  CLineParser( string file ) {

    m_dataFile.open( file.c_str() );
    if (!m_dataFile) {
      throw("Error->CLineParser: Cannot open file.");
    } else {
			std::cout << "\n file = " << file << endl;
    }

  } // end CLineParser() 

  ~CLineParser() { 
    m_dataFile.close(); 
  }

  vector<string>& read_file();
  void parse_string(string& s);
  void report();

private:
  
  string	   m_filename;		// data file name.
  ifstream	   m_dataFile;   	// data input stream
  vector<string>   m_lines;             // vector of lines.

};

vector<string>& CLineParser::read_file()
{
  string line;				// Temporary to store current line.

  while( m_dataFile >> line ) {
    cout << line << endl;
    m_lines.push_back( line );
  }

  return m_lines;
}

void CLineParser::parse_string(string& s)
{
  istringstream is(s);

  cout << "Blasted fools!" << endl;
  cout << "String = " << s << endl;

	/*
  is >> _run 
     >> _sequence 
     >> _scan 
     >> _obs_view 
     >> _resolution 
     >> _direction
     >> _filter_1A
     >> _filter_1B
     >> _filter_2A
     >> _filter_2B;
	*/

}

__main__

parser = CLineParser("rocks.cc");


