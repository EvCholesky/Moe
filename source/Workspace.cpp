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

	printf("%s(%d) error:", pLexloc->m_strFilename.PChz(), iLine);
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



void BeginWorkspace(CWorkspace * pWork)
{
	CAlloc * pAlloc = pWork->m_pAlloc;

	pWork->m_aryEntry.Clear();
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

	// force our stacks to have zero allocated memory before we take our prev cbFree measurement
	pWork->m_aryEntry.Clear();

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
	}
	pWork->m_aryEntry.Clear();

	size_t cbFreePost = pAlloc->CB();
	EWC_ASSERT(pWork->m_cbFreePrev == cbFreePost, "failed to free all bytes");
}