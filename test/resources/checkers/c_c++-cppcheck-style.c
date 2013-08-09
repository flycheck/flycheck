int f(int x)
{
    int unused;
    // cppcheck-suppress unusedVariable
    int unused2;
    return x * 2;
}
