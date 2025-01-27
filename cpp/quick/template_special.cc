
template <typename T>
struct BookCase {

  void ListBooks() {
    for( const T& thing : bookcase_ ) {
      std::cout << "\n item: " << thing;
    }
    std::cout << "\n";
  }

  void AddBook(const T& book) {
    bookcase_.push_back( book );
  }
  
private:
  std::vector<T> bookcase_;

};

template<>
struct BookCase<std::string> {

  void ListBooks() {
    for( const std::string& thing : bookcase_ ) {
      std::cout << "\n special item: " << thing;
    }
    std::cout << "\n";
  }

	void AddBook( const std::string& book ) {
		std::cout << "\n Adding special book...";
		bookcase_.push_back( book );
	}

private:
	std::vector<std::string> bookcase_;
};
  
__main__

BookCase<int> books;

books.AddBook( 5 );
books.AddBook( 7 );
books.AddBook( 4 );

BookCase<std::string> fiction;
std::string fiction1( "Dracula" );

fiction.AddBook( fiction1 );

books.ListBooks();
fiction.ListBooks();


