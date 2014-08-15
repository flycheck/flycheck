class Base { virtual ~Base() {} };
class Derived : public Base {};

void     foo(Base* b) {        dynamic_cast<Derived*>(b); }
