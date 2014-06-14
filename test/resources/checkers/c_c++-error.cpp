#include "c_c++-local-header.h"

template<bool> struct test;
template<> struct test<true> {};

int main(void) {
#if !(defined(FLYCHECK_LOCAL) && defined(FLYCHECK_LIBRARY))
    test<false> t;
#endif
    int *foo = nullptr;
    return 0;
}
