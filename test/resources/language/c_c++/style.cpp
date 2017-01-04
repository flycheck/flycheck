int f(int x)
{
    int null = 0;

    int unused;
    // cppcheck-suppress unusedVariable
    int unused2;

    return x * 2 / null;
}

std::string foobar(const std::string foo) {
    return foo + "bar";
}
