#include "JaiLex.h"
#include "JaiParse.h"
#include "Workspace.h"
#include <cstdarg>
#include <stdio.h>

using namespace EWC;

void EmitError(SErrorManager * pErrman, SLexerLocation * pLexloc, const char * pChz, ...)
{
	printf("%s(%d) error:", pLexloc->m_strFilename.PChz(), pLexloc->m_iLine);
	++pErrman->m_cError;
	
	if (pChz)
	{
		va_list ap;
		va_start(ap, pChz);
		vprintf(pChz, ap);
		printf("\n");
	}
}

void EmitError(CWorkspace * pWork, EWC::SLexerLocation * pLexloc, const char * pChz, ...)
{
	printf("%s(%d) error:", pLexloc->m_strFilename.PChz(), pLexloc->m_iLine);
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
,m_arypStnodEntry(pAlloc)
,m_pSymtab(nullptr)
,m_pErrman(pErrman)
,m_cbFreePrev(-1)
{
}



void BeginWorkspace(CWorkspace * pWork)
{
	CAlloc * pAlloc = pWork->m_pAlloc;

	pWork->m_arypStnodEntry.Clear();
	pWork->m_cbFreePrev = pAlloc->CB();

	pWork->m_pSymtab = EWC_NEW(pAlloc, CSymbolTable) CSymbolTable(pAlloc);
	pWork->m_pSymtab->AddBuiltInSymbols();
}

void BeginParse(CWorkspace * pWork, SJaiLexer * pJlex, const char * pChzIn)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	CParseContext * pParctx = EWC_NEW(pAlloc, CParseContext) CParseContext(pAlloc);
	pWork->m_pParctx = pParctx;

	static const int cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->EWC_ALLOC(cChStorage, 4);
	InitJaiLexer(pJlex, pChzIn, &pChzIn[CCh(pChzIn)], aChStorage, cChStorage);

	// force our stacks to have zero allocated memory before we take our prev cbFree measurement
	pWork->m_arypStnodEntry.Clear();
	pParctx->m_arypStnodOperator.Clear();	
	pParctx->m_arypStnodOperand.Clear();

	PushSymbolTable(pParctx, pWork->m_pSymtab);
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
		pAlloc->EWC_DELETE(pWork->m_pSymtab);
		pWork->m_pSymtab = nullptr;
	}

	CSTNode ** ppStnodMac = pWork->m_arypStnodEntry.PMac();
	for (CSTNode ** ppStnod = pWork->m_arypStnodEntry.A(); ppStnod != ppStnodMac; ++ppStnod)
	{
		pAlloc->EWC_DELETE(*ppStnod);
	}
	pWork->m_arypStnodEntry.Clear();

	size_t cbFreePost = pAlloc->CB();
	EWC_ASSERT(pWork->m_cbFreePrev == cbFreePost, "failed to free all bytes");
}
