
__main__

std::ostringstream console;

console << "\n Hi. Am I writing to the console?";
console << "\n --- This is the next line.";
console << "\n --- This is the third line.";

std::cout << "\n This is the output of console:\n";
std::cout << console.str();

