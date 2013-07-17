typedef std::vector<int> IntVec;
void f(const IntVec& v)
{
     for (IntVec::const_iterator it = v.begin(); it != v.end(); it++) { }
}
