#include <stdio.h>

typedef void (^my_block_t)();

int main(void)
{
    my_block_t p = ^ { printf("hello world\n"); };
}
