#include <iostream>
#include <vector>
#include <cassert>
#include <type_traits>


namespace run_time
{
    using type = int ;
    using type_list = std::vector<type> ;

    type_list tail( type_list types )
    {
        if( types.empty() ) return types ;
        else return { types.begin()+1, types.end() } ;
    }

    bool contains( type T, type_list types )
    {
        if( types.empty() ) return false ;
        else if( types.front() == T ) return true ;
        else return contains( T, tail(types) ) ;
    }
}


namespace compile_time
{
    template < typename... > struct type_list {} ; // vector<type>

    /////////////////////////////////////////////////////////////////////////

    // type_list tail( type_list types )
    template < typename...  > struct tail ;

    // if( types.empty() ) return types ;
    template <> struct tail< type_list<> > { using type =  type_list<> ; };

    // else return { types.begin()+1, types.end() } ;
    template < typename FIRST, typename... REST >
    struct tail< type_list<FIRST,REST...> > { using type =  type_list<REST...> ; };

    ////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////

    // bool contains( type T, type_list types )
    template < typename T, typename...  > struct contains ;

    // if( types.empty() ) return false ;
    template < typename T > struct contains< T, type_list<> > : std::false_type {} ;

    // else if( types.front() == T ) return true ;
    template < typename T, typename... REST >
    struct contains< T, type_list<T,REST...> > : std::true_type {} ;

    // else return contains( T, tail(types) ) ;
    template < typename T, typename NOT_T, typename... REST >
    struct contains< T, type_list<NOT_T,REST...> > : contains< T, type_list<REST...> > {};

    /////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////
    template < typename T, typename U > struct equals : std::false_type {} ;
    template < typename T > struct equals<T,T> : std::true_type {} ;
}

int main()
{
    {
        using namespace run_time ;
        
        std::vector<int> seq{ 1, 2, 4, 8 } ;
        assert( contains( 4, seq ) ) ;
        assert( ( tail(seq) == std::vector<int>{ 2, 4, 8 } ) ) ;
    }

    {
        using namespace compile_time ;
        
        using seq = type_list< char, short, int, double> ;
        static_assert( contains< int, seq >::value, "" ) ;
        static_assert( equals< tail<seq>::type, type_list< short, int, double > >::value, "" ) ;
    }
}


/*

g++ -std=c++14 -O2 -Wall -Wextra -pedantic-errors main.cpp && ./a.out && echo ok
clang++ -std=c++14 -stdlib=libc++ -O2 -Wall -Wextra -pedantic-errors main.cpp && ./a.out && echo ok

*/

