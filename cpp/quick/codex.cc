
__main__

std::ifstream codex;
std::string fname("codex.txt");
codex.open(fname.c_str());
std::string token;

if (codex.fail()) {
	std::cout << "\n I couldn't open: " << fname << " ! Aborting..." << "\n";
	exit(1);
 } else {	
	while(codex >> token) {
		std::cout << "\n next token => " << token;
	}
	codex.close();
 }







 
