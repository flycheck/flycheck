template <typename T>
void foo(T& x) { x.bar(); }

struct A {};

void bar() { A a; foo(a); }
