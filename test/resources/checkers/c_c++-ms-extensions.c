#include <stdio.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmicrosoft"

typedef struct {
  double a;
} inner_s;

typedef struct {
  union {
    inner_s;
    inner_s inner;
  };
  double b;
} outer_s;

double inner_fn(inner_s p);
double inner_fn(inner_s p) { return p.a * p.a; }

double outer_fn(outer_s p);
double outer_fn(outer_s p) { return p.a * p.a + p.b * p.b; }

int main(void) {
  outer_s demo = { .a = 3, .b = 0 };
  printf("%g\n", outer_fn(demo));
  printf("%g\n", inner_fn(demo.inner));
}

#pragma clang diagnostic pop
