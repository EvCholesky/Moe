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
#include "Lexer.h"
#include "Parser.h"
#include "Workspace.h"
#include <cstdarg>
#include <stdio.h>

using namespace EWC;

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, va_list ap)
{
	if (pLexloc && pLexloc->FIsValid())
	{
		s32 iLine;
		s32 iCol;
		CalculateLinePosition(pErrman->m_pWork, pLexloc, &iLine, &iCol);

		printf("%s(%d,%d) Warning: ", pLexloc->m_strFilename.PCoz(), iLine, iCol);
	}
	else
	{
		printf("Internal Warning: ");
	}
	++pErrman->m_cWarning;
	
	if (pCoz)
	{
		vprintf(pCoz, ap);
		printf("\n");
	}
}

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	EmitWarning(pErrman, pLexloc, pCoz, ap);
}

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, va_list ap)
{
	SError error(pErrman);
	PrintErrorLine(&error, "Error:", pLexloc, pCoz, ap);
}

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	EmitError(pErrman, pLexloc, pCoz, ap);
}

void EmitError(CWorkspace * pWork, const SLexerLocation * pLexloc, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	EmitError(pWork->m_pErrman, pLexloc, pCoz, ap);
}



SError::SError(SErrorManager * pErrman)
:m_pErrman(pErrman)
{
	++pErrman->m_cError;
}

void PrintErrorLine(SError * pError, const char * pChzPrefix, const SLexerLocation * pLexloc, const char * pCoz, va_list ap)
{
	if (pLexloc && pLexloc->FIsValid())
	{
		s32 iLine;
		s32 iCol;
		CalculateLinePosition(pError->m_pErrman->m_pWork, pLexloc, &iLine, &iCol);

		printf("%s(%d,%d) %s ", pLexloc->m_strFilename.PCoz(), iLine, iCol, pChzPrefix);
	}
	else
	{
		printf("Internal %s ", pChzPrefix);
	}
	
	if (pCoz)
	{
		vprintf(pCoz, ap);
		printf("\n");
	}
}

void PrintErrorLine(SError * pError, const char * pChzPrefix, const SLexerLocation * pLexloc, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	PrintErrorLine(pError, pChzPrefix, pLexloc, pCoz, ap);
}

inline void CalculateLinePositionRaw(const char * pChBegin, s32 dBLoc, s32 * piLine, s32 * piCol)
{
	int iLine = 0;
	int iCol = 0;

	for (const char * pCh = pChBegin; *pCh != '\0'; ++pCh)
	{
		s32 dB = s32(pCh - pChBegin);
		if (dB >= dBLoc)
			break;

		if (*pCh == '\n')
		{
			++iLine;
			iCol = 0;
		}
		else if (*pCh == '\t')
		{
			iCol += 4;
		}
		else
		{
			++iCol;
		}
	}
	*piLine = iLine;
	*piCol = iCol;
}

void CalculateLinePosition(CWorkspace * pWork, const SLexerLocation * pLexloc, s32 * piLine, s32 * piCol)
{
	auto pFile = pWork->PFileLookup(pLexloc->m_strFilename.PCoz(), CWorkspace::FILEK_Source);
	if (!pFile)
	{
		*piLine = -1;
		*piCol = -1;
		return;
	}

	const char * pChBegin = pFile->m_pChzFileBody;
	int iLine;
	int iCol;
	auto dBWarm = pFile->m_dBWarm;
	if (pLexloc->m_dB < dBWarm)
	{
		CalculateLinePositionRaw(pChBegin, pLexloc->m_dB, &iLine, &iCol);
	}
	else
	{
		CalculateLinePositionRaw(pChBegin + dBWarm, pLexloc->m_dB - dBWarm, &iLine, &iCol);
		iCol += (iLine == 0) ? pFile->m_iColumnWarm : 0;	// columns reset on line increments
		iLine += pFile->m_iLineWarm;
	}

	/* // Debug Warm Start
	s32 iLineOld = 0;
	s32 iColumnOld = 0;
	CalculateLinePositionRaw(pChBegin, pLexloc->m_dB, &iLineOld, &iColumnOld);
	EWC_ASSERT(iLine == iLineOld && iCol == iColumnOld, "bad warm start");
	*/

	pFile->m_dBWarm = pLexloc->m_dB;
	pFile->m_iLineWarm = iLine;
	pFile->m_iColumnWarm = iCol;

	*piLine = iLine + 1;	// 1 relative
	*piCol = iCol + 1;		// 1 relative
}

const char * CWorkspace::s_pCozSourceExtension = ".moe";

CWorkspace::CWorkspace(CAlloc * pAlloc, SErrorManager * pErrman)
:m_pAlloc(pAlloc)
,m_pParctx(nullptr)
,m_aryEntry(pAlloc, EWC::BK_Workspace)
,m_aryiEntryChecked(pAlloc, EWC::BK_Workspace) 
,m_arypValManaged(pAlloc, EWC::BK_WorkspaceVal, 0)
,m_hashHvIPFileSource(pAlloc)
,m_hashHvIPFileLibrary(pAlloc)
,m_arypFile(pAlloc, EWC::BK_WorkspaceFile, 200)
,m_pChzObjectFilename(nullptr)
,m_pSymtab(nullptr)
,m_pErrman(pErrman)
,m_cbFreePrev(-1)
,m_optlevel(OPTLEVEL_Debug)
,m_globmod(GLOBMOD_Normal)
{
	m_pErrman->m_pWork = this;
}

void CWorkspace::AppendEntry(CSTNode * pStnod, CSymbolTable * pSymtab)
{
	EWC_ASSERT(pStnod, "null entry point");
	SEntry * pEntry = m_aryEntry.AppendNew();
	pEntry->m_pStnod = pStnod;
	pEntry->m_pSymtab = pSymtab;
	pEntry->m_pProc = nullptr;
}

CSymbolTable * CWorkspace::PSymtabNew(const EWC::CString & strName)
{
	CSymbolTable * pSymtabNew = EWC_NEW(m_pAlloc, CSymbolTable) CSymbolTable(strName, m_pAlloc);
	if (m_pSymtab)
	{
		m_pSymtab->AddManagedSymtab(pSymtabNew);
	}

	return pSymtabNew;
}

CWorkspace::SFile * CWorkspace::PFileEnsure(const char * pCozFile, FILEK filek)
{
	EWC::CHash<HV, int> * phashHvIPFile = PHashHvIPFile(filek);
	EWC::CString strFilename(pCozFile);

	int * pipFile = nullptr;
	FINS fins = phashHvIPFile->FinsEnsureKey(EWC::HvFromPCozLowercase(strFilename.PCoz()), &pipFile);
	if (fins == EWC::FINS_Inserted)
	{
		SFile * pFile = EWC_NEW(m_pAlloc, SFile) SFile(strFilename, filek);
		*pipFile = (int)m_arypFile.C();
		m_arypFile.Append(pFile);

		pFile->m_strFilename = strFilename;
	}
	return m_arypFile[*pipFile];
}

CWorkspace::SFile * CWorkspace::PFileLookup(const char * pCozFile, FILEK filek)
{
	int hv = EWC::HvFromPCozLowercase(pCozFile);
	EWC::CHash<HV, int> * phashHvIPFile = PHashHvIPFile(filek);
	int * pipFile = phashHvIPFile->Lookup(hv);
	if (pipFile)
	{
		if (EWC_FVERIFY(*pipFile >= 0) & (*pipFile < (int)m_arypFile.C()), "bad file index")
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

	EWC_ASSERT(pWork->m_arypValManaged.C() == 0, "Unexpected managed values in workspace");

	pWork->m_arypFile.Clear();
	pWork->m_hashHvIPFileSource.Clear(0);
	pWork->m_hashHvIPFileLibrary.Clear(0);
	pWork->m_cbFreePrev = pAlloc->CB();

	pWork->m_pSymtab = pWork->PSymtabNew("global");
	pWork->m_pSymtab->m_grfsymtab.Clear(FSYMTAB_Ordered);
	pWork->m_pSymtab->AddBuiltInSymbols(pWork->m_pErrman);
}

void BeginParse(CWorkspace * pWork, SJaiLexer * pJlex, const char * pCozIn, const char * pCozFilename)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	CParseContext * pParctx = EWC_NEW(pAlloc, CParseContext) CParseContext(pAlloc, pWork);
	pWork->m_pParctx = pParctx;

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->EWC_ALLOC(cChStorage, 4);
	InitJaiLexer(pJlex, pCozIn, &pCozIn[CBCoz(pCozIn)-1], aChStorage, cChStorage);

	if (pCozFilename)
	{
		pJlex->m_pCozFilename = pCozFilename;
	}

	SLexerLocation lexloc(pJlex);
	PushSymbolTable(pParctx, pWork->m_pSymtab, lexloc);
}

void EndParse(CWorkspace * pWork, SJaiLexer * pJlex)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	pAlloc->EWC_FREE(pJlex->m_aChScratch);

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

	size_t cpVal = pWork->m_arypValManaged.C();
	for (size_t ipVal = 0; ipVal < cpVal; ++ipVal)
	{
		pAlloc->EWC_DELETE(pWork->m_arypValManaged[ipVal]);
	}
	pWork->m_arypValManaged.Clear();

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

	if (pWork->m_pChzObjectFilename)
	{
		pWork->m_pAlloc->EWC_FREE((void*)pWork->m_pChzObjectFilename);
		pWork->m_pChzObjectFilename = nullptr;
	}

	size_t cbFreePost = pAlloc->CB();
	if (pWork->m_cbFreePrev != cbFreePost)
	{
		printf("\nWARNING: failed to free all bytes during compilation. (%d -> %d)\n", pWork->m_cbFreePrev, cbFreePost);
		printf("----------------------------------------------------------------------\n");
		pAlloc->PrintAllocations();
	}
}

void CWorkspace::SetObjectFilename(const char * pChzObjectFilename, size_t cB)
{
	if (!cB)
	{
		cB = CBCoz(pChzObjectFilename);
	}

	if (EWC_FVERIFY(m_pChzObjectFilename == nullptr, "expected null object filename"))
	{
		char * pCoz = (char*)m_pAlloc->EWC_ALLOC(sizeof(char) * cB, EWC_ALIGN_OF(char));

		EWC::SStringBuffer strbuf(pCoz, cB);
		AppendCoz(&strbuf, pChzObjectFilename);
		m_pChzObjectFilename = pCoz;
	}
}

char * CWorkspace::PChzLoadFile(const EWC::CString & strFilename, EWC::CAlloc * pAlloc)
{
	SLexerLocation lexloc(strFilename);
#if defined( _MSC_VER )
	FILE * pFile;
	fopen_s(&pFile, strFilename.PCoz(), "rb");
#else
	FILE * pFile = fopen(strFilename.PCoz(), "rb");
#endif
	if (!pFile)
	{
		EmitError(m_pErrman, &lexloc, "Failed opening file %s", strFilename.PCoz());
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
		EmitError(m_pErrman, &lexloc, "Failed reading file %s", strFilename.PCoz());
		pAlloc->EWC_FREE(pChzFile);
		return nullptr;
	}

	pChzFile[cB] = '\0';
	return pChzFile;
}