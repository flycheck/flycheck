module d_dmd_deprecated;

deprecated
auto foo(int a)
{
    return a;
}

void main()
{
    auto bar = foo(1);
}
