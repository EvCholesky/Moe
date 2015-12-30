/* Copyright (C) 2015 Evan Christensen
|
| Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
| documentation files (the "Software"), to deal in the Software without restriction, including without limitation the 
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
| persons to whom the Software is furnished to do so, subject to the following conditions:
| 
| The above copyright notice and this permission notice shall be included in all copies or substantial portions of the 
| Software.
| 
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
| WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
| COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
| OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */

#define EWC_TYPES_IMPLEMENTATION
#include "CodeGen.h"
#include "JaiLex.h"
#include "JaiParse.h"
#include "Workspace.h"

using namespace EWC;

extern void TestLexing();
extern void TestParse();
extern void TestTypeCheck();
extern void TestCodeGen();

class CCommandLine // tag = comline
{
public:
	struct SCommand // tag = com
	{
		HV		m_hvName;
		HV		m_hvValue;
	};

	CCommandLine()
	:m_aryCom()
	,m_pChzFilename(nullptr)
		{ ; }

	void Parse(int cpChzArg, const char * apChzArg[])
	{
		const char * pChzFilename = nullptr;
		for (int ipChz = 1; ipChz < cpChzArg; ++ipChz)
		{
			const char * pChzArg = apChzArg[ipChz];
			if (pChzArg[0] == '-')
			{
				if (m_aryCom.C() == m_aryCom.CMax())
				{
					printf("Too many arguments. ignoring %s\n", pChzArg);
				}
				else
				{
					SCommand * pCom = m_aryCom.AppendNew();
					pCom->m_hvName = HvFromPChz(pChzArg);
				}
			}
			else
			{
				if (pChzFilename)
					printf("Expected one filename argument.");

				pChzFilename = pChzArg;
			}
		}
		m_pChzFilename = pChzFilename;
	}

	bool FHasCommand(const char * pChzCommand)
	{
		HV hvCommand = HvFromPChz(pChzCommand);

		for (int iCom = 0; iCom < m_aryCom.C(); ++iCom)
		{
			if (m_aryCom[iCom].m_hvName == hvCommand)
				return true;
		}
		return false;
	}

	CFixAry<SCommand, 32>	m_aryCom;
	const char *			m_pChzFilename;
};

void PrintCommandLineOptions()
{
	printf("jailang [options] [filename]\n");
	printf("  options:\n");
	printf("	-help  : Print this message\n");
	printf("    -test  : Run compiler unit tests\n");
}

int main(int cpChzArg, const char * apChzArg[])
{
	if (cpChzArg < 2)
	{
		PrintCommandLineOptions();
		return 0;
	}

	CCommandLine comline;
	comline.Parse(cpChzArg, apChzArg);

	if (comline.FHasCommand("-help"))
	{
		PrintCommandLineOptions();
	}

	if (comline.m_pChzFilename)
	{
		u8 aBString[1024 * 100];
		CAlloc allocString(aBString, sizeof(aBString));
		StaticInitStrings(&allocString);

		u8 aB[1024 * 100];
		CAlloc alloc(aB, sizeof(aB));

		SErrorManager errman;
		CWorkspace work(&alloc, &errman);

		InitLLVM();
		CompileModule(&work, comline.m_pChzFilename);
		ShutdownLLVM();

		StaticShutdownStrings(&allocString);
	}

	if (comline.FHasCommand("-test"))
	{
		TestLexing();
		TestParse();
		TestTypeCheck();
		TestCodeGen();
		return 0;
	}

	return 0;
}