#include <stdio.h>

extern "C" void PrintFloat(float g)
{
	printf("%f\n", g);
}

extern "C" void PrintInt(int n)
{
	printf("%d\n", n);
}