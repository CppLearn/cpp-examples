
template <class... T>
void board_ship(T... gremlins) {
	std::cout << "\n num gremlins: " << sizeof...(gremlins) << "\n";
}

__main__

board_ship( 1, 2, 3, 4 );
