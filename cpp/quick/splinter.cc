
__main__

std::string message = "{ CIRCLE 608 240 10 0 255 0 0 }";
std::list<std::string> tokens;
std::string token;

std::cout << "\n length of message: " << message.length();

unsigned i = 0;
std::string s = "";
char c;

while (i < message.length()) {
	c = static_cast<char>(message[i]);
	if (c != ' ') {
		s += c;
	} else break;
	i++;
 }

std::cout << "\n";
std::cout << "partial string: " << s << "\n";



