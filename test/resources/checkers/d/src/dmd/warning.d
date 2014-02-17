module dmd.warning;

import dmd.library;
import external_library;

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
