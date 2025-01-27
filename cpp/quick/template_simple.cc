
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
  
__main__

BookCase<int> books;

books.AddBook( 5 );
books.AddBook( 7 );
books.AddBook( 4 );

books.ListBooks();


