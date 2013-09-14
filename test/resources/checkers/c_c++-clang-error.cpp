template<bool> struct test;
template<> struct test<true> {};

int main(void) {
     test<false> t;
     int *foo = nullptr;
     return 0;
}
