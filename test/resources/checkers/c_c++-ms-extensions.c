typedef struct {
  int a;
} inner_s;

typedef struct {
  union {
    inner_s;
    inner_s inner;
  };
  int b;
} outer_s;

int main(void) {
  outer_s example = { .a = 3, .b = 0 };
  return example.b;
}
