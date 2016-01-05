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

#include "CodeGen.h"
#include "JaiLex.h"
#include "JaiParse.h"
#include "Workspace.h"
#include <cstdarg>
#include <stdio.h>

using namespace EWC;

void EmitError(SErrorManager * pErrman, SLexerLocation * pLexloc, const char * pChz, ...)
{
	s32 iLine;
	s32 iCodepoint;
	CalculateLinePosition(pLexloc, &iLine, &iCodepoint);

	printf("%s(%d) error:", pLexloc->m_strFilename.PChz(), iLine);
	++pErrman->m_cError;
	
	if (pChz)
	{
		va_list ap;
		va_start(ap, pChz);
		vprintf(pChz, ap);
		printf("\n");
	}
}

void EmitError(CWorkspace * pWork, SLexerLocation * pLexloc, const char * pChz, ...)
{
	s32 iLine;
	s32 iCodepoint;
	CalculateLinePosition(pLexloc, &iLine, &iCodepoint);

	if (pLexloc)
	{
		printf("%s(%d) error:", pLexloc->m_strFilename.PChz(), iLine);
	}
	else
	{
		printf("internal error:");
	}
	++pWork->m_pErrman->m_cError;
	
	if (pChz)
	{
		va_list ap;
		va_start(ap, pChz);
		vprintf(pChz, ap);
		printf("\n");
	}
}



CWorkspace::CWorkspace(CAlloc * pAlloc, SErrorManager * pErrman)
:m_pAlloc(pAlloc)
,m_pParctx(nullptr)
,m_aryEntry(pAlloc)
,m_aryiEntryChecked(pAlloc) 
,m_hashHvIPFileSource(pAlloc)
,m_hashHvIPFileLibrary(pAlloc)
,m_arypFile(pAlloc)
,m_pSymtab(nullptr)
,m_pErrman(pErrman)
,m_cbFreePrev(-1)
{
}

void CWorkspace::AppendEntry(CSTNode * pStnod, CSymbolTable * pSymtab)
{
	EWC_ASSERT(pStnod, "null entry point");
	SEntry * pEntry = m_aryEntry.AppendNew();
	pEntry->m_pStnod = pStnod;
	pEntry->m_pSymtab = pSymtab;
	pEntry->m_pProc = nullptr;
}

CSymbolTable * CWorkspace::PSymtabNew()
{
	CSymbolTable * pSymtabNew = EWC_NEW(m_pAlloc, CSymbolTable) CSymbolTable(m_pAlloc);
	if (m_pSymtab)
	{
		m_pSymtab->AddManagedSymtab(pSymtabNew);
	}

	return pSymtabNew;
}

void CWorkspace::EnsureFile(const char * pChzFile, FILEK filek)
{
	EWC::CHash<HV, int> * phashHvIPFile = PHashHvIPFile(filek);
	EWC::CString strFilename(pChzFile);

	int * pipFile = nullptr;
	FINS fins = phashHvIPFile->FinsEnsureKey(strFilename.Hv(), &pipFile);
	if (fins == EWC::FINS_Inserted)
	{
		SFile * pFile = EWC_NEW(m_pAlloc, SFile) SFile(strFilename, filek);
		*pipFile = (int)m_arypFile.C();
		m_arypFile.Append(pFile);

		pFile->m_strFilename = strFilename;
	}
}

CWorkspace::SFile * CWorkspace::PFileLookup(HV hv, FILEK filek)
{
	EWC::CHash<HV, int> * phashHvIPFile = PHashHvIPFile(filek);
	int * pipFile = phashHvIPFile->Lookup(hv);
	if (pipFile)
	{
		if (EWC_FVERIFY(*pipFile >= 0) & (*pipFile < m_arypFile.C()), "bad file index")
		{
			return m_arypFile[*pipFile];
		}
	}

	return nullptr;
}



void BeginWorkspace(CWorkspace * pWork)
{
	CAlloc * pAlloc = pWork->m_pAlloc;

	pWork->m_aryiEntryChecked.Clear();
	pWork->m_aryEntry.Clear();

	pWork->m_arypFile.Clear();
	pWork->m_hashHvIPFileSource.Clear(0);
	pWork->m_hashHvIPFileLibrary.Clear(0);
	pWork->m_cbFreePrev = pAlloc->CB();

	pWork->m_pSymtab = pWork->PSymtabNew();
	pWork->m_pSymtab->m_grfsymtab.Clear(FSYMTAB_Ordered);
	pWork->m_pSymtab->AddBuiltInSymbols();
}

void BeginParse(CWorkspace * pWork, SJaiLexer * pJlex, const char * pChzIn)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	CParseContext * pParctx = EWC_NEW(pAlloc, CParseContext) CParseContext(pAlloc, pWork);
	pWork->m_pParctx = pParctx;

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->EWC_ALLOC(cChStorage, 4);
	InitJaiLexer(pJlex, pChzIn, &pChzIn[CCh(pChzIn)], aChStorage, cChStorage);

	SLexerLocation lexloc(pJlex);
	PushSymbolTable(pParctx, pWork->m_pSymtab, lexloc);
}

void EndParse(CWorkspace * pWork, SJaiLexer * pJlex)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	pAlloc->EWC_FREE(pJlex->m_aChStorage);

	CSymbolTable * pSymtabPop = PSymtabPop(pWork->m_pParctx);
	EWC_ASSERT(pSymtabPop == pWork->m_pSymtab, "symbol table push/pop mismatch");
	
	pAlloc->EWC_DELETE(pWork->m_pParctx);
	pWork->m_pParctx = nullptr;

	pWork->m_aryiEntryChecked.EnsureSize(pWork->m_aryEntry.C());
}

void EndWorkspace(CWorkspace * pWork)
{
	CAlloc * pAlloc = pWork->m_pAlloc;

	if (pWork->m_pSymtab)
	{
		CSymbolTable * pSymtabIt = pWork->m_pSymtab;
		while (pSymtabIt)
		{
			CSymbolTable * pSymtab = pSymtabIt;
			pSymtabIt = pSymtab->m_pSymtabNextManaged;
			pAlloc->EWC_DELETE(pSymtab);
		}

		pWork->m_pSymtab = nullptr;
	}

	CWorkspace::SEntry * pEntryMac = pWork->m_aryEntry.PMac();
	for (CWorkspace::SEntry * pEntry = pWork->m_aryEntry.A(); pEntry != pEntryMac; ++pEntry)
	{
		pAlloc->EWC_DELETE(pEntry->m_pStnod);
		pEntry->m_pStnod = nullptr;
		pEntry->m_pSymtab = nullptr;

		if (pEntry->m_pProc)
		{
			pAlloc->EWC_DELETE(pEntry->m_pProc);
			pEntry->m_pProc = nullptr;
		}
	}
	pWork->m_aryEntry.Clear();
	pWork->m_aryiEntryChecked.Clear();
	pWork->m_hashHvIPFileSource.Clear(0);
	pWork->m_hashHvIPFileLibrary.Clear(0);

	size_t cipFile = pWork->m_arypFile.C();
	for (size_t ipFile = 0; ipFile < cipFile; ++ipFile)
	{
		if (pWork->m_arypFile[ipFile])
		{
			pAlloc->EWC_DELETE(pWork->m_arypFile[ipFile]);
			pWork->m_arypFile[ipFile] = nullptr;
		}
	}
	pWork->m_arypFile.Clear();

	size_t cbFreePost = pAlloc->CB();
	if (pWork->m_cbFreePrev != cbFreePost)
	{
		pAlloc->PrintAllocations();
		EWC_ASSERT(pWork->m_cbFreePrev == cbFreePost, "failed to free all bytes");
	}
}

char * CWorkspace::PChzLoadFile(const EWC::CString & strFilename, EWC::CAlloc * pAlloc)
{
	SLexerLocation lexloc(strFilename);
#if defined( _MSC_VER )
	FILE * pFile;
	fopen_s(&pFile, strFilename.PChz(), "rb");
#else
	FILE * pFile = fopen(strFilename.PChz(), "rb");
#endif
	if (!pFile)
	{
		EmitError(m_pErrman, &lexloc, "Failed opening file %s", strFilename.PChz());
		return nullptr;
	}

	fseek(pFile, 0, SEEK_END);
	size_t cB = ftell(pFile);
	fseek(pFile, 0, SEEK_SET);

	char * pChzFile = (char *)pAlloc->EWC_ALLOC(cB + 1, 4);
	size_t cBRead = fread(pChzFile, 1, cB, pFile);
	fclose(pFile);

	if (cB != cBRead)
	{
		EmitError(m_pErrman, &lexloc, "Failed reading file %s", strFilename.PChz());
		pAlloc->EWC_FREE(pChzFile);
		return nullptr;
	}

	pChzFile[cB] = '\0';
	return pChzFile;
}