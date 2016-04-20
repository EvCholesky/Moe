#include <stdio.h>
#include <math.h>

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


extern "C" void PrintPointer(unsigned char * pB)
{
	printf("%p\n", pB);
}

extern "C" void PrintString(unsigned char * pChz)
{
	if (!pChz)
	{
		printf("<null>\n");
		return;
	}

	printf("%s\n", pChz);
}

extern "C" float cosf_JAI(float g)
{
	return cosf(g);
}

extern "C" float sinf_JAI(float g)
{
	return sinf(g);
}

extern "C" float sqrtf_JAI(float g)
{
	return sqrtf(g);
}