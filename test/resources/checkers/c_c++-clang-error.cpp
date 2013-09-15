template<bool> struct test;
template<> struct test<true> {};

int main(void) {
#if !(defined(FOO) && defined(BAR))
    test<false> t;
#endif
    int *foo = nullptr;
    return 0;
}
