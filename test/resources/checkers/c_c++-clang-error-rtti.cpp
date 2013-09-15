#include <iostream>

using namespace std;

class Base {};
class Derived : public Base {};

int main()
{
    Derived *d = new Derived;
    Base *b = dynamic_cast<Base*>(d);
    cout << b;
    return 0;
}
