using namespace std;

bool token_empty( const std::string& s ) {
	return s.empty();
}

const int max_length = 70;

std::string get_next_line( std::list<std::string>& tokens )
{
	std::string next_line = "";
	while ( next_line.length() <= max_length && tokens.size() > 0 ) {

		if ( tokens.size() >= 1 ) { // peek ahead if there's another token.
			int future_len = next_line.length() + tokens.front().length() + 1;
			if ( future_len > max_length ) {
				break;
			} // next token will make us exceed max length so quit loop.

			// if we're still good then add enxt token to line.
			next_line = next_line + tokens.front() + " ";
			tokens.pop_front();
		}
	}

	return next_line;
}

__main__

std::string token;
std::list<std::string> tokens;

while( !std::cin.eof() ) {
	token.clear();
	std::cin >> token;
	tokens.push_back( token );
 }

tokens.remove_if( token_empty );

// Now output back as justified comment.

std::cout << "\n # ------------------------------------------------------------------- ";
while( tokens.size() > 0 ) {
	std::cout << "\n # " << get_next_line( tokens );
 }
std::cout << "\n # ------------------------------------------------------------------- ";




