class A {
    A::~A();
};

typedef std::vector<int> IntVec;
void f(const IntVec& v)
{
    int unused;
    for (IntVec::const_iterator it = v.begin(); it != v.end(); it++) { }
}
