#include <cstdarg>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern "C" void * PVMalloc(size_t cB)
{
	return malloc((size_t)cB);
}

extern "C" void FreeMalloc(void * pV)
{
	free(pV);
}

extern "C" void DebugBreak()
{
	__debugbreak();
}

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

extern "C" float cosf_MOE(float g)
{
	return cosf(g);
}

extern "C" float sinf_MOE(float g)
{
	return sinf(g);
}

extern "C" float sqrtf_MOE(float g)
{
	return sqrtf(g);
}

extern "C" float GSign(float g)
{
	return g < 0 ? -1.0f : 1.0f; 
}

extern "C" float GAbs(float x)
{ 
	return fabsf(x);
}

extern "C" float GSqrt(float g)
{
	return sqrtf(g);
}

extern "C" float GMod(float x, float y)
{ 
	return fmodf(x, y); 
}

extern "C" int32_t NTrunc(float g)
{
	return static_cast<int32_t>(g);
}

extern "C" int32_t NCeil(float g)
{ 
	return static_cast<int32_t>(ceil(g)); 
}

extern "C" int32_t NRound(float g)
{
	float gSign = GSign(g); 

	return static_cast<int32_t>((GAbs(g)+0.5f) * gSign); 
}

void EnsureTerminatedCopy(char * pCozBegin, char * pCozAppend, size_t cBMax, char ch)
{
	auto pCozMax = &pCozBegin[cBMax];
	size_t iB = pCozAppend - pCozBegin;

	if (cBMax <= 0)
		return;

	iB = (cBMax - 1 < iB) ? cBMax-1 : iB;

	auto pCozEnd = &pCozBegin[iB];
	auto pCozBackup = pCozEnd; // - 1;	// skip the spot our terminator will go

	while (pCozBackup != pCozBegin)
	{
		--pCozBackup;

		bool fIsBasicChar = (*pCozBackup & 0x80) == 0;
		bool fIsStarterChar = (*pCozBackup & 0xC0) == 0xC0;
		if (fIsBasicChar || fIsStarterChar)
			break;
	}

	size_t cBBackup;
	if ((*pCozBackup & 0xF8) == 0xF0)		cBBackup = 4;
	else if ((*pCozBackup & 0xF0) == 0xE0)	cBBackup = 3;
	else if ((*pCozBackup & 0xE0) == 0xC0)	cBBackup = 2;
	else									cBBackup = 1;

	if (cBBackup > size_t(pCozEnd - pCozBackup))
	{
		*pCozBackup = ch;
	}
	else
	{
		*pCozEnd = ch;
	}
}

extern "C" ptrdiff_t snprintf_MOE(char * aCh, size_t cBMax, const char * pCozFormat, ...)
{
	// BB - could just use EWC::FormatCoz if there was a vararg version 
	// BB - Actually, trying to use EWC functions here causes a mess of duplicate symbols, so I guess I'll just copy this for now.

	char * pCozAppend = nullptr;
	if (cBMax > 1)
	{
		va_list ap;
		va_start(ap, pCozFormat);
		ptrdiff_t cCh = vsnprintf_s(aCh, cBMax, _TRUNCATE, pCozFormat, ap);
		va_end(ap);

		if (cCh == -1)
		{
			cCh = cBMax-1;
		}
		pCozAppend = aCh + cCh;
	}

	// handle truncation within utf8 multibyte char
	EnsureTerminatedCopy(aCh, pCozAppend, cBMax, '\0');
	return pCozAppend - aCh;
}