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
#include "EwcArray.h"
#include "EwcHash.h"
#include "EwcString.h"

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
struct SLexerLocation;
struct STypeInfo;
struct STypeInfoProcedure;

struct SErrorManager	//  // tag = errman
{
				SErrorManager(CWorkspace * pWork = nullptr)
				:m_pWork(pWork)
				,m_cError(0)
				,m_cWarning(0)
					{ ; }


	void		Clear()
					{ m_cError = 0; m_cWarning = 0;}
	void		AddChildErrors(const SErrorManager * pErrmanOther)
					{
						m_cError += pErrmanOther->m_cError;
						m_cWarning += pErrmanOther->m_cWarning;
					}

	CWorkspace *	m_pWork;		// back pointer for SFile lookup inside EmitError
	int				m_cError;
	int				m_cWarning;
};

struct SError 
{
						SError(SErrorManager * pErrman);

	SErrorManager *		m_pErrman;
};

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, va_list ap);
void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, ...);

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, va_list ap);
void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pCoz, ...);
void EmitError(CWorkspace * pWork, const SLexerLocation * pLexloc, const char * pCoz, ...);

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

enum GLOBMOD
{
	GLOBMOD_UnitTest,	// Global statements are placed in an implicit method for unit testing convenience.
	GLOBMOD_Normal,		// Globals declarations become global variables, global statements are errors.
};


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
extern EWC::CString	StrUniqueName(SUniqueNameSet * pUnset, const EWC::CString & strIn);



struct SWorkspaceEntry // tag = entry
{
	CSTNode *				m_pStnod;
	CSymbolTable *		 	m_pSymtab;	// symbol table for this entry, local symbols for lambdas 

	// BB - could remove? (just use CSTNode::m_pVal for PARK_ProcedureDefinition?)
	CIRProcedure *			m_pProc;
	bool					m_fHideDebugString;	// don't print during WriteDebugStringForEntries()
};

	

class CWorkspace	// tag = work
{
public:
	enum FILEK
	{
		FILEK_Source,
		FILEK_UnitTest,
		FILEK_Library,

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
	CSymbolTable *			PSymtabNew(const EWC::CString & strNamespace, SUniqueNameSet * pUnset);

	SFile *					PFileEnsure(const char * pCozFile, FILEK filek);
	EWC::CHash<HV, int> *	PHashHvIPFile(FILEK filek);

	void					CopyUnitTestFiles(CWorkspace * pWorkOther);
	int						CFile(FILEK filek)
								{ return PHashHvIPFile(filek)->C(); }
	SFile *					PFileLookup(const char * pCozFile, FILEK filek);
	void					SetObjectFilename(const char * pChzObjectFilename, size_t cB = 0);

	EWC::CAlloc *					m_pAlloc;
	CParseContext *					m_pParctx;
	EWC::CDynAry<SWorkspaceEntry> 	m_aryEntry;
	EWC::CDynAry<int> 				m_aryiEntryChecked;		// order in which entry points were successfully type checked
	EWC::CDynAry<CIRValue *>		m_arypValManaged;

	typedef EWC::CHash<HV, int> HashHvIPFile;
	EWC::CHash<HV, int> *			m_mpFilekPHashHvIPFile[FILEK_Max];
	EWC::CDynAry<SFile *> 			m_arypFile;
	const char *					m_pChzObjectFilename;

	CSymbolTable *					m_pSymtab;				// top level symbols
	EWC::CHash<HV, STypeInfo *>		m_hashHvPTin;			// global unique typeinfo table
	SUniqueNameSet					m_unset;
	SUniqueNameSet					m_unsetTin;				// unique names used by types

	SErrorManager *					m_pErrman;
	size_t							m_cbFreePrev;

	TARGETOS						m_targetos;
	OPTLEVEL						m_optlevel;
	GLOBMOD							m_globmod;
};

void BeginWorkspace(CWorkspace * pWork);
void BeginParse(CWorkspace * pWork, SLexer * pLex, const char * pCozIn, const char * pCozFilename = nullptr);
void EndParse(CWorkspace * pWork, SLexer * pLex);
void EndWorkspace(CWorkspace * pWork);

const char * PCozSkipUnicodeBOM(const char * pCozFile);

void CalculateLinePosition(CWorkspace * pWork, const SLexerLocation * pLexloc, s32 * piLine, s32 * piCodepoint);
size_t CChConstructFilename(const char * pChzFilenameIn, const char * pChzExtension, char * pChzFilenameOut, size_t cChOutMax);

void PerformTypeCheck(
	EWC::CAlloc * pAlloc,
	SErrorManager * pErrman, 
	CSymbolTable * pSymtabTop,
	EWC::CAry<SWorkspaceEntry> * paryEntry,
	EWC::CAry<int> * paryiEntryChecked,
	GLOBMOD globmod);


void SwapDoubleHashForPlatformBits(const char * pChInput, char * aChOut, size_t cB);