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

#pragma once
#include "Error.h"
#include "EwcArray.h"
#include "EwcHash.h"
#include "EwcString.h"
#include <cstdarg> // for va_list forward decl

namespace EWC
{
	class CAlloc;
}

class CIRProcedure;
class CIRValue;
class CParseContext;
class CSTNode;
class CSymbolTable;
class CWorkspace;
struct SDIFile;
struct SGenericMap;
struct SLexerLocation;
struct STypeInfo;
struct STypeInfoProcedure;

struct SErrorCount	// tag = errc
{
			SErrorCount(ERRID errid)
			:m_errid(errid)
			,m_c(0)
				{ ; }

	ERRID	m_errid;
	int		m_c;
};

struct SErrorManager	//  // tag = errman
{
				SErrorManager(EWC::CAlloc * pAlloc);

	void		SetWorkspace(CWorkspace * pWork);

	void		Clear()
					{ 
						m_aryErrid.Clear();
					}
	void		AddChildErrors(const SErrorManager * pErrmanOther)
					{
						auto paryErridOther = &pErrmanOther->m_aryErrid;
						m_aryErrid.Append(paryErridOther->A(), paryErridOther->C());
					}
	bool		FHasHiddenErrors();
	bool		FHasErrors()
					{
						return CError() != 0;
					}
	int			CError()
					{	
						int cError, cWarning; 
						ComputeErrorCounts(&cError, &cWarning);
						return cError;
					}
	int			CWarning()
					{	
						int cError, cWarning; 
						ComputeErrorCounts(&cError, &cWarning);
						return cWarning;
					}
	void		ComputeErrorCounts(int * pCError, int * pCWarning);
	bool		FTryHideError(ERRID errid);

	void		PushGenmapContext(SGenericMap * pGenmap);
	void		PopGenmapContext(SGenericMap * pGenmap);

	CWorkspace *				m_pWork;				// back pointer for SFile lookup inside EmitError
	EWC::CDynAry<ERRID>			m_aryErrid;				// numbered errors (for expected unit test errors)

	EWC::CDynAry<SErrorCount> * m_paryErrcExpected;
	EWC::CDynAry<SGenericMap *> m_arypGenmapContext;	// instantiate context
};

struct SGenericMapScope // tag = genscope
{
							SGenericMapScope(SErrorManager * pErrman, SGenericMap * pGenmap)
							:m_pErrman(pErrman)
							,m_pGenmap(pGenmap)
								{
									if (pGenmap)
									{
										pErrman->PushGenmapContext(pGenmap);
									}
								}

							~SGenericMapScope()
								{
									if (m_pGenmap)
									{
										m_pErrman->PopGenmapContext(m_pGenmap);
									}
								}

	SErrorManager *			m_pErrman;
	SGenericMap * 			m_pGenmap;
};



enum ERRS
{
	ERRS_Unreported,
	ERRS_Hidden,
	ERRS_Reported,
};

struct SError 
{
						SError(SErrorManager * pErrman, ERRID errid = ERRID_UnknownError);

	SErrorManager *		m_pErrman;
	ERRID				m_errid;
	ERRS				m_errs;
};

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, va_list ap);
void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, ...);

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, va_list ap);
void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, ...);
void EmitError(CWorkspace * pWork, const SLexerLocation * pLexloc, ERRID errid, const char * pCoz, ...);

void PrintErrorLine(SError * pError, const char * pChzPrefix, const SLexerLocation * pLexloc, const char * pCoz, va_list ap);
void PrintErrorLine(SError * pError, const char * pChzPrefix, const SLexerLocation * pLexloc, const char * pCoz, ...);



class CNameMangler // tag = mang
{
public:
					CNameMangler(EWC::CAlloc * pAlloc, size_t cBStartingMax=1024);
					~CNameMangler();

	void			Resize(size_t cBStartingMax);
	void			AppendName(const char * pCoz);
	void			AppendType(STypeInfo * pTin);

	EWC::CString	StrMangleMethodName(STypeInfoProcedure * pTinproc);
	STypeInfoProcedure * 
					PTinprocDemangle(const EWC::CString & strName, CSymbolTable * pSymtab);

	EWC::CAlloc *		m_pAlloc;
	EWC::SStringBuffer	m_strbuf;
};



enum OPTLEVEL
{
	OPTLEVEL_Debug,
	OPTLEVEL_Release,
};

enum TARGETOS
{
	TARGETOS_Nil = -1,
	TARGETOS_Windows,
};

enum FUNT	// Flag UNit Tests
{
	FUNT_ImplicitProc		= 0x1,		// wrap the code in an implicit procedure for testing
										//   if this is not set it will test as a global
	FUNT_ResolveAllSymbols	= 0x2,		// mark all symbols as in use, don't search for main()

	FUNT_None				= 0x0,
	FUNT_All				= 0x3,

	GRFUNT_Default			= FUNT_ResolveAllSymbols,
	GRFUNT_DefaultTest      = FUNT_ImplicitProc | FUNT_ResolveAllSymbols,
};

EWC_DEFINE_GRF(GRFUNT, FUNT, u32)



struct SUniqueNameSet // tag = unset
{
							SUniqueNameSet(EWC::CAlloc * pAlloc, EWC::BK bk, u32 cCapacityStarting = 32)
							:m_hashHvNUnique(pAlloc, bk, cCapacityStarting)
								{ ; }
	void					Clear(u32 cCapacity)
								{ m_hashHvNUnique.Clear(cCapacity); }

	EWC::CHash<HV, u32>		m_hashHvNUnique;		// map for generating unique strings
};

extern void			GenerateUniqueName(SUniqueNameSet * pUnset, const char * pCozIn, char * pCozOut, size_t cBOutMax);



struct SWorkspaceEntry // tag = entry
{
							SWorkspaceEntry()
							:m_pStnod(nullptr)
							,m_pSymtab(nullptr)
							,m_fHideDebugString(false)
								{ ; }

	CSTNode *				m_pStnod;
	CSymbolTable *		 	m_pSymtab;	// symbol table for this entry, local symbols for lambdas 

	bool					m_fHideDebugString;	// don't print during WriteDebugStringForEntries()
};

typedef EWC::CBlockList<SWorkspaceEntry, 128> BlockListEntry;

class CWorkspace	// tag = work
{
public:
	enum FILEK
	{
		FILEK_Source,
		FILEK_UnitTest,
		FILEK_Library,
		FILEK_StaticLibrary,
		FILEK_DynamicLibrary,

		FILEK_Max,
		FILEK_Min = 0,
		FILEK_Nil = -1,
	};

	enum FILES
	{
		FILES_Nil,
		FILES_Requested,
		FILES_Processing,
		FILES_Complete,
	};

	static const int	s_cBFilenameMax = 1024;
	static const char * s_pCozSourceExtension;
	static const char * s_pCozUnitTestExtension;

	struct SFile // tag = file
	{
						SFile(const EWC::CString & strFilename, FILEK filek)
						:m_strFilename(strFilename)
						,m_pChzFileBody(nullptr)
						,m_pDif(nullptr)
						,m_filek(filek)
						,m_files(FILES_Requested)
						,m_dBWarm(0)
						,m_iLineWarm(0)
						,m_iColumnWarm(0)
							{ ; }

		EWC::CString	m_strFilename;	// full filename;
		const char *	m_pChzFileBody;	// contents of the file
		SDIFile *		m_pDif;			
		FILEK			m_filek;
		FILES			m_files;

		s32				m_dBWarm;		// byte delta for warm start (previous lookup)
		s32				m_iLineWarm;
		s32				m_iColumnWarm;

	};

							CWorkspace(EWC::CAlloc * pAlloc, SErrorManager * pErrman);

	char *					PChzLoadFile(const EWC::CString & strFilename, EWC::CAlloc * pAlloc);
	void					AppendEntry(CSTNode * pStnod, CSymbolTable * pSymtab);

	SFile *					PFileEnsure(const char * pCozFile, FILEK filek);
	EWC::CHash<HV, int> *	PHashHvIPFile(FILEK filek);

	void					CopyUnitTestFiles(CWorkspace * pWorkOther);
	int						CFile(FILEK filek)
								{ return PHashHvIPFile(filek)->C(); }
	SFile *					PFileLookup(const char * pCozFile, FILEK filek);
	void					SetObjectFilename(const char * pChzObjectFilename, size_t cB = 0);

	EWC::CAlloc *						m_pAlloc;
	CParseContext *						m_pParctx;
	BlockListEntry 						m_blistEntry;
	EWC::CDynAry<SWorkspaceEntry *> 	m_arypEntryChecked;		// order in which entry points were successfully type checked
	EWC::CDynAry<CIRValue *>			m_arypValManaged;

	typedef EWC::CHash<HV, int> HashHvIPFile;
	EWC::CHash<HV, int> *			m_mpFilekPHashHvIPFile[FILEK_Max];
	EWC::CDynAry<SFile *> 			m_arypFile;
	const char *					m_pChzObjectFilename;

	CSymbolTable *					m_pSymtab;				// top level symbols
	CUniqueTypeRegistry *			m_pUntyper;
	SUniqueNameSet					m_unset;
	SUniqueNameSet					m_unsetTin;				// unique names used by types

	SErrorManager *					m_pErrman;
	size_t							m_cbFreePrev;

	TARGETOS						m_targetos;
	OPTLEVEL						m_optlevel;
	GRFUNT							m_grfunt;
};


class CFileSearch // tag = filser
{
public:
	struct SFile
	{
		const char *	m_pChzDirectory;
		SFile *			m_pFileNext;
	};
					CFileSearch(EWC::CAlloc * pAlloc);
					~CFileSearch();

	void			AddDirectory(const char * pChzDir);

	SFile *			PFileFind(const char * pChzFileAndExt);	// filename with extension
	void			AddFile(const char * pChzFilenameFull, const char * pChzDir);


	EWC::CAlloc *			m_pAlloc;
	EWC::CDynAry<SFile *>	m_aryPFile;
	EWC::CHash<HV, int>		m_hashHvIFile;

};

void BeginWorkspace(CWorkspace * pWork);
void BeginParse(CWorkspace * pWork, SLexer * pLex, const char * pCozIn, const char * pCozFilename = nullptr);
void EndParse(CWorkspace * pWork, SLexer * pLex);
void EndWorkspace(CWorkspace * pWork);

CSymbolTable * PSymtabNew(EWC::CAlloc * pAlloc, CSymbolTable * pSymtabParent, const EWC::CString & strNamespace);

const char * PCozSkipUnicodeBOM(const char * pCozFile);

void CalculateLinePosition(CWorkspace * pWork, const SLexerLocation * pLexloc, s32 * piLine, s32 * piCodepoint);
size_t CChConstructFilename(const char * pChzFilenameIn, const char * pChzExtension, char * pChzFilenameOut, size_t cChOutMax);
void SplitFilename(const char * pChzFilename, size_t * piBFile, size_t * piBExtension, size_t * piBEnd);

void PerformTypeCheck(
	EWC::CAlloc * pAlloc,
	SErrorManager * pErrman, 
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	EWC::CDynAry<SWorkspaceEntry *> * parypEntryChecked,
	GRFUNT grfunt);


void SwapDoubleHashForPlatformBits(const char * pChInput, char * aChOut, size_t cB);

void AssertHandlerLoc( const char* pChzFile, u32 line, const char* pChzCondition, CWorkspace * pWork, SLexerLocation * pLexloc, const char* pChzMessage = 0, ...);
#define EWC_FVERIFY_LOC(PREDICATE, PWORK, PLEXLOC, ...) \
	EWC_FVERIFY_PROC (PREDICATE, AssertHandlerLoc, __FILE__, __LINE__, PWORK, PLEXLOC,  __VA_ARGS__ )
