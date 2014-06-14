class Base { virtual ~Base() {} };
class Derived : public Base {};

Derived* foo(Base* b) { return dynamic_cast<Derived*>(b); }
