#include <stdio.h>

extern "C" void PrintFloat(float g)
{
	printf("%f\n", g);
}

extern "C" void PrintInt(int n)
{
	printf("%d\n", n);
}

extern "C" void PrintByte(char n)
{
	printf("%d\n", n);
}

extern "C" void PrintBool(bool f)
{
	printf("%s\n", (f) ? "true" : "false");
}