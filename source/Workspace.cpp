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
#include "TypeInfo.h"
#include "Workspace.h"
#include <cstdarg>
#include <stdio.h>

using namespace EWC;



SErrorManager::SErrorManager(CAlloc * pAlloc)
:m_pWork(nullptr)
,m_aryErrid(pAlloc, BK_Workspace, 0)
,m_paryErrcExpected(nullptr)
,m_arypGenmapContext(pAlloc, BK_Workspace, 64)
{ 

}

void SErrorManager::SetWorkspace(CWorkspace * pWork)
{
	m_pWork = pWork;
} 

bool SErrorManager::FHasHiddenErrors()
{
	if (!m_paryErrcExpected)
		return false;

	auto pErrcMax = m_paryErrcExpected->PMac();
	for (auto pErrc = m_paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
	{
		if (pErrc->m_c)
			return true;
	}
	return false;
}

void SErrorManager::ComputeErrorCounts(int * pCError, int * pCWarning)
{
	int cError = 0;
	int cWarning = 0;
	auto pErridMax = m_aryErrid.PMac();
	for (auto pErrid = m_aryErrid.A(); pErrid != pErridMax; ++pErrid)
	{
		if (*pErrid < ERRID_ErrorMax)
			++cError;
		else
			++cWarning;

	}
	*pCError = cError;
	*pCWarning = cWarning;
}

bool SErrorManager::FTryHideError(ERRID errid)
{
	if (!m_paryErrcExpected)
		return false;

	auto pErrcMax = m_paryErrcExpected->PMac();
	for (auto pErrc = m_paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
	{
		if (pErrc->m_errid == errid)
		{
			++pErrc->m_c;
			return true;
		}
	}
	return false;
}

void SErrorManager::PushGenmapContext(SGenericMap * pGenmap)
{
	m_arypGenmapContext.Append(pGenmap);
}

void SErrorManager::PopGenmapContext(SGenericMap * pGenmap)
{
	if (!EWC_FVERIFY(!m_arypGenmapContext.FIsEmpty(), "instantiate context underflow in error manager"))
		return;

	auto pGenmapTop = m_arypGenmapContext.TPopLast();
	if (!EWC_FVERIFY(pGenmapTop == pGenmap, "push/pop mismatch for instantate context"))
		return;
}



SError::SError(SErrorManager * pErrman, ERRID errid)
:m_pErrman(pErrman)
,m_errid(errid)
,m_errs(ERRS_Unreported)
{
}

void PrintGenericInstantiateContext(SErrorManager * pErrman)
{
	for (SGenericMap ** ppGenmap = pErrman->m_arypGenmapContext.A(); ppGenmap != pErrman->m_arypGenmapContext.PMac(); ++ppGenmap)
	{
		const char * pCozName = "unknown";
		auto pGenmap = *ppGenmap;
		if (pGenmap->m_pSymDefinition)
		{
			pCozName = pGenmap->m_pSymDefinition->m_strName.PCoz();
		}

		printf("  while instantiating generic '%s': ", pCozName);	
		EWC::CHash<EWC::CString, SAnchor>::CIterator iter(&pGenmap->m_mpStrAnc);

		CString * pStr;
		SAnchor * pAnc;
		while ((pAnc = iter.Next(&pStr)))
		{
			if (pAnc->FIsNull())
				continue;

			CString str;
			if (pAnc->m_pTin)
			{
				str = StrFromTypeInfo(pAnc->m_pTin);
			}
			else 
			{
				EWC_ASSERT(pAnc->m_pStnodBaked, "no bake value for '%s'", pStr->PCoz());
				str = StrFromSTNode(pAnc->m_pStnodBaked);
			}

			printf("$%s %s, ", pStr->PCoz(), str.PCoz());
		}
		s32 iLine;
		s32 iCol;
		for (SLexerLocation * pLexloc = pGenmap->m_aryLexlocSrc.A(); pLexloc != pGenmap->m_aryLexlocSrc.PMac(); ++pLexloc)
		{
			CalculateLinePosition(pErrman->m_pWork, pLexloc, &iLine, &iCol);

			printf("\n  at %s(%d, %d)\n", pLexloc->m_strFilename.PCoz(), iLine, iCol);
		}
	}
}

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, va_list ap)
{
	bool fHidden = pErrman->FTryHideError(errid);
	if (fHidden)
		return;

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
	pErrman->m_aryErrid.Append(errid);
	
	if (pCoz)
	{
		vprintf(pCoz, ap);
		printf("\n");
	}

	PrintGenericInstantiateContext(pErrman);
}

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	EmitWarning(pErrman, pLexloc, errid, pCoz, ap);
}

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, va_list ap)
{
	SError error(pErrman, errid);
	PrintErrorLine(&error, "Error:", pLexloc, pCoz, ap);

	if (error.m_errs != ERRS_Hidden)
	{
		PrintGenericInstantiateContext(pErrman);
	}
}

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	EmitError(pErrman, pLexloc, errid, pCoz, ap);
}

void EmitError(CWorkspace * pWork, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, ...)
{
	va_list ap;
	va_start(ap, pCoz);
	EmitError(pWork->m_pErrman, pLexloc, errid, pCoz, ap);
}

void PrintErrorLine(SError * pError, const char * pChzPrefix, const SLexerLocation * pLexloc, const char * pCoz, va_list ap)
{
	auto pErrman = pError->m_pErrman;
	if (pError->m_errs == ERRS_Unreported)
	{
		if (pErrman->FTryHideError(pError->m_errid))
		{
			pError->m_errs = ERRS_Hidden;
		}
		else
		{
			pErrman->m_aryErrid.Append(pError->m_errid);
			pError->m_errs = ERRS_Reported;
		}
	}

	if (pError->m_errs == ERRS_Hidden)
		return;

	if (pLexloc && pLexloc->FIsValid())
	{
		s32 iLine;
		s32 iCol;
		CalculateLinePosition(pErrman->m_pWork, pLexloc, &iLine, &iCol);

		printf("%s(%d,%d) %s ", pLexloc->m_strFilename.PCoz(), iLine, iCol, pChzPrefix);
	}
	else
	{
		printf("Internal %s ", pChzPrefix);
	}

	ERRID errid = pError->m_errid;
	if (errid != ERRID_Nil && errid != ERRID_UnknownError && errid != ERRID_UnknownWarning)
	{
		printf("#%d: ", errid);
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
	auto pFile = pWork->PFileLookup(pLexloc->m_strFilename.PCoz(), CWorkspace::FILEK_Nil);
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
const char * CWorkspace::s_pCozUnitTestExtension = ".moetest";

CWorkspace::CWorkspace(CAlloc * pAlloc, SErrorManager * pErrman)
:m_pAlloc(pAlloc)
,m_pParctx(nullptr)
,m_blistEntry(pAlloc, EWC::BK_Workspace)
,m_arypEntryChecked(pAlloc, EWC::BK_Workspace) 
,m_arypValManaged(pAlloc, EWC::BK_WorkspaceVal, 0)
,m_arypFile(pAlloc, EWC::BK_WorkspaceFile, 200)
,m_pChzObjectFilename(nullptr)
,m_pSymtab(nullptr)
,m_pUntyper(nullptr)
,m_unset(pAlloc, EWC::BK_Workspace, 0)
,m_unsetTin(pAlloc, EWC::BK_Workspace, 0)
,m_pErrman(pErrman)
,m_cbFreePrev(-1)
,m_targetos(TARGETOS_Nil)
,m_optlevel(OPTLEVEL_Debug)
,m_globmod(GLOBMOD_Normal)
{
	m_pErrman->SetWorkspace(this);

	for (int filek = FILEK_Min; filek < FILEK_Max; ++filek)
	{
		m_mpFilekPHashHvIPFile[filek] = EWC_NEW(m_pAlloc, HashHvIPFile) HashHvIPFile(pAlloc, EWC::BK_Workspace);
	}
}

void CWorkspace::AppendEntry(CSTNode * pStnod, CSymbolTable * pSymtab)
{
	EWC_ASSERT(pStnod, "null entry point");
	SWorkspaceEntry * pEntry = m_blistEntry.AppendNew();
	pEntry->m_pStnod = pStnod;
	pEntry->m_pSymtab = pSymtab;
}

CSymbolTable * PSymtabNew(
	CAlloc * pAlloc,
	CSymbolTable * pSymtabParent,
	const EWC::CString & strNamespace,
	CUniqueTypeRegistry * pUntyper,
	SUniqueNameSet * pUnsetTin)
{
	CSymbolTable * pSymtabNew = EWC_NEW(pAlloc, CSymbolTable) CSymbolTable(strNamespace, pAlloc, pUntyper, pUnsetTin);
	if (pSymtabParent)
	{
		pSymtabParent->AddManagedSymtab(pSymtabNew);
	}

	return pSymtabNew;
}

CSymbolTable * PSymtabNew(CAlloc * pAlloc, CSymbolTable * pSymtabParent, const EWC::CString & strNamespace)
{
	if (!EWC_FVERIFY(pSymtabParent, "Null parent passed into pSymtabNew, use other overload for root."))
		return nullptr;

	return PSymtabNew(pAlloc, pSymtabParent, strNamespace, pSymtabParent->m_pUntyper, pSymtabParent->m_pUnset);
}

void GenerateUniqueName(SUniqueNameSet * pUnset, const char * pCozIn, char * pCozOut, size_t cBOutMax)
{
	size_t iCh = CBCoz(pCozIn) - 2;

	// not handling whitespace...
	u32 nIn = 0;
	u32 nMultiple = 1;
	while ((pCozIn[iCh] >= '0') & (pCozIn[iCh] <= '9'))
	{
		nIn = (pCozIn[iCh] - '0') * nMultiple + nIn;
		nMultiple *= 10;
		--iCh;
	}

	HV hv = 0;
	hv = HvFromPCoz(pCozIn, iCh+1);

	u32 * pN = nullptr;
	FINS fins = pUnset->m_hashHvNUnique.FinsEnsureKey(hv, &pN);
	EWC::SStringBuffer strbufOut(pCozOut, cBOutMax);
	if (fins == FINS_Inserted)
	{
		*pN = nIn;
		AppendCoz(&strbufOut, pCozIn);
	}
	else
	{
		*pN = ewcMax(nIn, *pN + 1);
		AppendCoz(&strbufOut, pCozIn);

		strbufOut.m_pCozAppend = &strbufOut.m_pCozBegin[iCh+1];
		FormatCoz(&strbufOut, "%d", *pN); 
	}
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

EWC::CHash<HV, int> * CWorkspace::PHashHvIPFile(FILEK filek) 
{
	if (!EWC_FVERIFY(filek > FILEK_Nil && filek < FILEK_Max, "bad filek"))
		return nullptr;
	
	return m_mpFilekPHashHvIPFile[filek];
}

CWorkspace::SFile * CWorkspace::PFileLookup(const char * pCozFile, FILEK filek)
{
	int filekMin = filek;
	int filekMax = filek + 1;
	if (filek == FILEK_Nil)
	{
		filekMin = FILEK_Min;
		filekMax = FILEK_Max;
	}
	int hv = EWC::HvFromPCozLowercase(pCozFile);
	for (int filekIt = filekMin; filekIt < filekMax; ++filekIt)
	{
		EWC::CHash<HV, int> * phashHvIPFile = PHashHvIPFile((FILEK)filekIt);
		int * pipFile = phashHvIPFile->Lookup(hv);
		if (pipFile)
		{
			if (EWC_FVERIFY((*pipFile >= 0) & (*pipFile < (int)m_arypFile.C()), "bad file index"))
			{
				return m_arypFile[*pipFile];
			}
		}
	}

	return nullptr;
}

const char * PCozSkipUnicodeBOM(const char * pCozFile)
{
	// utf8 BOM
	const u8 * pBFile = (const u8 *)pCozFile;
	if (pBFile[0] == 0xEF && pBFile[1] == 0xBB && pBFile[2] == 0xBF)
	{
		pCozFile += 3;
	}
	return pCozFile;
}




void BeginWorkspace(CWorkspace * pWork)
{
	CAlloc * pAlloc = pWork->m_pAlloc;

	pWork->m_arypEntryChecked.Clear();
	pWork->m_blistEntry.Clear();

	EWC_ASSERT(pWork->m_arypValManaged.C() == 0, "Unexpected managed values in workspace");

	pWork->m_arypFile.Clear();
	for (int filek = CWorkspace::FILEK_Min; filek < CWorkspace::FILEK_Max; ++filek)
	{
		pWork->m_mpFilekPHashHvIPFile[filek]->Clear(0);
	}
	pWork->m_cbFreePrev = pAlloc->CB();

	pWork->m_unset.Clear(0);
	pWork->m_unsetTin.Clear(0);
	
	pWork->m_pUntyper = EWC_NEW(pAlloc, CUniqueTypeRegistry) CUniqueTypeRegistry(pAlloc);
	pWork->m_pSymtab = PSymtabNew(pAlloc, nullptr, "global", pWork->m_pUntyper, &pWork->m_unsetTin);
	pWork->m_pSymtab->AddBuiltInSymbols(pWork);
}

void BeginParse(CWorkspace * pWork, SLexer * pLex, const char * pCozIn, const char * pCozFilename)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	CParseContext * pParctx = EWC_NEW(pAlloc, CParseContext) CParseContext(pAlloc, pWork);
	pWork->m_pParctx = pParctx;

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->EWC_ALLOC(cChStorage, 4);
	InitLexer(pLex, pCozIn, &pCozIn[CBCoz(pCozIn)-1], aChStorage, cChStorage);

	if (pCozFilename)
	{
		pLex->m_pCozFilename = pCozFilename;
	}

	SLexerLocation lexloc(pLex);
	PushSymbolTable(pParctx, pWork->m_pSymtab, lexloc);
}

void EndParse(CWorkspace * pWork, SLexer * pLex)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	pAlloc->EWC_FREE(pLex->m_aChScratch);

	CSymbolTable * pSymtabPop = PSymtabPop(pWork->m_pParctx);
	EWC_ASSERT(pSymtabPop == pWork->m_pSymtab, "symbol table push/pop mismatch");
	
	pAlloc->EWC_DELETE(pWork->m_pParctx);
	pWork->m_pParctx = nullptr;

	pWork->m_arypEntryChecked.EnsureSize(pWork->m_blistEntry.C());
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

	BlockListEntry::CIterator iter(&pWork->m_blistEntry);
	while (SWorkspaceEntry * pEntry = iter.Next())
	{
		pAlloc->EWC_DELETE(pEntry->m_pStnod);
		pEntry->m_pStnod = nullptr;
		pEntry->m_pSymtab = nullptr;
	}

	auto ppValMac = pWork->m_arypValManaged.PMac();
	for (auto ppVal = pWork->m_arypValManaged.A(); ppVal != ppValMac; ++ppVal)
	{
		auto pVal = *ppVal;

		// I don't want vTable pointers in CIRValue, so faux-virtual destructor
		switch(pVal->m_valk)
		{
		case VALK_Procedure:
			{
				pAlloc->EWC_DELETE((CIRProcedure *)pVal);
				break;
			} break;
		default:
			pAlloc->EWC_DELETE(pVal);
			break;
		}
	}

	pWork->m_arypValManaged.Clear();

	pWork->m_blistEntry.Clear();
	pWork->m_arypEntryChecked.Clear();
	for (int filek = CWorkspace::FILEK_Min; filek < CWorkspace::FILEK_Max; ++filek)
	{
		pWork->m_mpFilekPHashHvIPFile[filek]->Clear(0);
	}

	pWork->m_pAlloc->EWC_DELETE(pWork->m_pUntyper);
	pWork->m_pUntyper = nullptr;

	pWork->m_unset.Clear(0);
	pWork->m_unsetTin.Clear(0);

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
	pWork->m_pErrman->Clear();

	if (pWork->m_pChzObjectFilename)
	{
		pWork->m_pAlloc->EWC_FREE((void*)pWork->m_pChzObjectFilename);
		pWork->m_pChzObjectFilename = nullptr;
	}

	size_t cbFreePost = pAlloc->CB();
	if (pWork->m_cbFreePrev != cbFreePost)
	{
		printf("\nWARNING: failed to free all bytes during compilation. (%zd -> %zd)\n", pWork->m_cbFreePrev, cbFreePost);
		printf("----------------------------------------------------------------------\n");
		pAlloc->PrintAllocations();
	}
}

void CWorkspace::CopyUnitTestFiles(CWorkspace * pWorkOther)
{
	SFile ** ppFileMax = pWorkOther->m_arypFile.PMac();
	for (auto ppFile = pWorkOther->m_arypFile.A(); ppFile != ppFileMax; ++ppFile)
	{
		auto pFileOld = *ppFile;
		if (pFileOld->m_filek != FILEK_UnitTest)
			continue; 

		auto pFileNew = PFileEnsure(pFileOld->m_strFilename.PCoz(), pFileOld->m_filek);
		*pFileNew = *pFileOld;
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
		EmitError(m_pErrman, &lexloc, ERRID_FailedOpeningFile, "Failed opening file %s", strFilename.PCoz());
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
		EmitError(m_pErrman, &lexloc, ERRID_UnknownError, "Failed reading file %s", strFilename.PCoz());
		pAlloc->EWC_FREE(pChzFile);
		return nullptr;
	}

	pChzFile[cB] = '\0';
	return pChzFile;
}

void AssertHandlerLoc( const char* pChzFile, u32 line, const char* pChzCondition, CWorkspace * pWork, SLexerLocation * pLexloc, const char* pChzMessage, ...)
{
	s32 iLine;
	s32 iCol;
	CalculateLinePosition(pWork, pLexloc, &iLine, &iCol);
	
	printf("Assertion failed: \"%s\" at %s:%u\n", pChzCondition, pChzFile, line);
	printf("   while compiling %s:%u,%u\n", pLexloc->m_strFilename.PCoz(), iLine, iCol);

	if (pChzMessage)
	{
		va_list ap;
		va_start(ap, pChzMessage);
		vprintf(pChzMessage, ap);
		printf("\n");
	}
}
