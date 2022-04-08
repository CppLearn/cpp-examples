
__main__

std::default_random_engine generator;
std::uniform_int_distribution<int> distribution(1,100);

std::vector<int> rocks;

for( int i = 0; i < 25; i++ ) {
	rocks.push_back( distribution(generator)  );
 }

util::print_vector( rocks, "rocks" );

std::cout << ui::color["yellow"];
while( !rocks.empty() ) {
	util::print_vector( rocks, "rocks" );
	rocks.pop_back();
 }
std::cout << ui::color["reset"];	




	


