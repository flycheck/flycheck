// Test continuation lines for template instantiations

template T()
{
	alias T = invalid;
}

template U()
{
	alias U = T!();
}

U!() u;
