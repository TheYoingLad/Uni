#include <iostream>

//void f( const int *a, const int& b ){}

/*class X{
    public:
        X( int i ) { }
};*/

/*struct X
{
  X( int i = 3 ) { std::cout << this << ' ' << i << '\n'; }
  ~X() { std::cout << this << ' ' << 2 << '\n'; }
};*/

bool f()
{
std::cout << 'f';
return false;
}
bool g()
{
std::cout << 'f';
return true;
}
bool h()
{
std::cout << 'h';
return false;
}

int main(){
    //X a( 5 );
    //X* p = new X( 4 );
    
    
    
    
    
    /*{// A válasz
     const int s = 2;
     const int t = 1;

     f( &s, &t );}


    {// B válasz
     int s = 2;
     const int t = 1;

     f( 0, s );}


    {// C válasz
     int s = 2;
     int t = 1;

     f( const &s, const &t );}


    {// D válasz
     const int s = 2;
     int t = 1;

     f( &s, &t );}*/
     
    // A válasz
    


if ( f() == g() == h() )
{
std::cout << ":-)";
}
else
{
std::cout << ":-(";
}

}

/*void f()
{
  X s;
}

// B válasz
void f()
{
  X s = new X( 3 );
}

// C válasz
void f()
{
  X s = null;
}

// D válasz
void f()
{
  X s( 3 );
}*/