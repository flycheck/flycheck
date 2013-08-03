template<bool> struct test;
template<> struct test<true> {};

int main(void) {
     test<false> t;
     return 0;
}
