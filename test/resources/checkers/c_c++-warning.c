#include <stdio.h>

int f(int x)
{
     int unused;
     unsigned int y = 10;
     return x < y ? ++x : x;
     #warning
}
