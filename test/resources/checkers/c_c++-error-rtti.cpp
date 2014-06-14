#include <iostream>

using namespace std;

class Base { virtual ~Base() {} };
class Derived : public Base {};

int main()
{
    Base *b = new Base;
    Derived *d = dynamic_cast<Derived*>(b);
    cout << d;
    return 0;
}
