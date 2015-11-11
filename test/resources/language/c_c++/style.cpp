int f(int x)
{
    int null = 0;

    int unused;
    // cppcheck-suppress unusedVariable
    int unused2;

    return x * 2 / null;
}

bool foo(bool a, bool b) { return a & b; }

std::string foobar(const std::string foo) {
    return foo + "bar";
}
