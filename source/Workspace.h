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
class CParseContext;
class CSTNode;
class CWorkspace;
struct SDIFile;
struct SLexerLocation;

struct SErrorManager	//  // tag = errman
{
				SErrorManager()
				:m_pWork(nullptr)
				,m_cError(0)
				,m_cWarning(0)
					{ ; }

	void		Clear()
					{ m_cError = 0; m_cWarning = 0;}

	CWorkspace *	m_pWork;		// back pointer for SFile lookup inside EmitError
	int				m_cError;
	int				m_cWarning;
};

void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pChz, va_list ap);
void EmitWarning(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pChz, ...);

void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pChz, va_list ap);
void EmitError(SErrorManager * pErrman, const SLexerLocation * pLexloc, const char * pChz, ...);
void EmitError(CWorkspace * pWork, const SLexerLocation * pLexloc, const char * pChz, ...);



enum OPTLEVEL
{
	OPTLEVEL_Debug,
	OPTLEVEL_Release,
};



class CWorkspace	// tag = work
{
public:
	struct SEntry // tag = entry
	{
		CSTNode *				m_pStnod;
		CSymbolTable *		 	m_pSymtab;	// symbol table for this entry, local symbols for lambdas 

		// BB - could remove? (just use CSTNode::m_pVal for PARK_ProcedureDefinition?)
		CIRProcedure *			m_pProc;
	};
	
	enum FILEK
	{
		FILEK_Nil = -1,
		FILEK_Source,
		FILEK_Library
	};

	enum FILES
	{
		FILES_Nil,
		FILES_Requested,
		FILES_Processing,
		FILES_Complete,
	};

	struct SFile // tag = file
	{
						SFile(const EWC::CString & strFilename, FILEK filek)
						:m_strFilename(strFilename)
						,m_pChzFileBody(nullptr)
						,m_filek(filek)
						,m_files(FILES_Requested)
							{ ; }

		EWC::CString	m_strFilename;	// full filename;
		char *			m_pChzFileBody;	// contents of the file
		SDIFile *		m_pDif;			
		FILEK			m_filek;
		FILES			m_files;
	};

							CWorkspace(EWC::CAlloc * pAlloc, SErrorManager * pErrman);

	char *					PChzLoadFile(const EWC::CString & strFilename, EWC::CAlloc * pAlloc);
	void					AppendEntry(CSTNode * pStnod, CSymbolTable * pSymtab);
	CSymbolTable *			PSymtabNew(const EWC::CString & strName);

	void					EnsureFile(const char * pChzFile, FILEK filek);
	EWC::CHash<HV, int> *	PHashHvIPFile(FILEK filek) 
								{ return (filek == FILEK_Source) ? &m_hashHvIPFileSource :  &m_hashHvIPFileLibrary; }
	int						CFile(FILEK filek)
								{ return PHashHvIPFile(filek)->C(); }
	SFile *					PFileLookup(HV hv, FILEK filek);
	void					SetObjectFilename(const char * pChzObjectFilename, size_t cCh = 0);

	EWC::CAlloc *					m_pAlloc;
	CParseContext *					m_pParctx;
	EWC::CDynAry<SEntry> 			m_aryEntry;
	EWC::CDynAry<int> 				m_aryiEntryChecked;		// order in which entry points were successfully type checked

	EWC::CHash<HV, int>				m_hashHvIPFileSource;	// imported (and root) source files
	EWC::CHash<HV, int>				m_hashHvIPFileLibrary;	// requested foreign library files
	EWC::CDynAry<SFile *> 			m_arypFile;
	const char *					m_pChzObjectFilename;

	CSymbolTable *					m_pSymtab;				// top level symbols

	SErrorManager *					m_pErrman;
	size_t							m_cbFreePrev;
	OPTLEVEL						m_optlevel;
};

void BeginWorkspace(CWorkspace * pWork);
void BeginParse(CWorkspace * pWork, SJaiLexer * pJlex, const char * pChzIn);
void EndParse(CWorkspace * pWork, SJaiLexer * pJlex);
void EndWorkspace(CWorkspace * pWork);

void CalculateLinePosition(CWorkspace * pWork, const SLexerLocation * pLexloc, s32 * piLine, s32 * piCodepoint);

void PerformTypeCheck(
	EWC::CAlloc * pAlloc,
	SErrorManager * pErrman, 
	CSymbolTable * pSymtabTop,
	EWC::CAry<CWorkspace::SEntry> * paryEntry,
	EWC::CAry<int> * paryiEntryChecked);

