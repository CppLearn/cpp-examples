
__main__

std::vector<char> v = {'a', 'b', 'c', 'd'};
util::print_vector( v );

for( int i = 0; i < 10; i++ ) {
  v.insert( v.begin(), 'z' );
}

util::print_vector( v );
v.erase( v.begin(), v.begin( ) + 10 );

util::print_vector( v );


