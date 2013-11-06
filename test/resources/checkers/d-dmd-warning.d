module d_dmd_warning;

auto foo(int a)
{
    return a;
    return a;
}

deprecated
auto bar(int a)
{
    return a;
}

void main()
{
    auto x = bar(1);
}
