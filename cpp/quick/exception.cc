
void cause_FPE()
{
	int n = 4 % 0;

	// Floating point exception (core dumped)
}

void print_blocks()
{	
	int n;	

	std::cout << "\n Enter n: ";
	std::cin >> n;

	if (n == 0) {
 		throw std::overflow_error( "[print_blocks] Divide by zero exception" );
	}
	
	for(int i = 0; i < 2000; i++) {
		if(i % n == 0) {
			std::cout << "\n " << i;
		}
	}
	
}

void outer_block()
{
	
	try {		
		print_blocks();		
	} catch (std::overflow_error& e) {
		std::string msg = "[outer_block]: ";
		msg += e.what();		
		throw std::overflow_error(msg.c_str());
	}
	
}

__main__

try {

	std::cout << "\n I'm inside a try block..." << "\n";
	
	outer_block();
	
 } catch (std::exception& e) {
	std::cerr << "\n [simulatte]: " << e.what() << "\n\n";
	exit(1);
 }

cause_FPE();

















