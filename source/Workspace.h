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
struct SLexerLocation;

struct SErrorManager	//  // tag = errman
{
	int			m_cError;
};

void EmitError(SErrorManager * pErrman, SLexerLocation * pLexloc, const char * pChz, ...);
void EmitError(CWorkspace * pWork, SLexerLocation * pLexloc, const char * pChz, ...);

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
	
	struct SFile // tag = file
	{
		EWC::CString	m_strFilename;
		char *			m_pChzFile;
	};

							CWorkspace(EWC::CAlloc * pAlloc, SErrorManager * pErrman);

	char *					PChzLoadFile(const EWC::CString & strFilename, EWC::CAlloc * pAlloc);
	void					AppendEntry(CSTNode * pStnod, CSymbolTable * pSymtab);
	CSymbolTable *			PSymtabNew();

	EWC::CAlloc *					m_pAlloc;
	CParseContext *					m_pParctx;
	EWC::CDynAry<SEntry> 			m_aryEntry;
	EWC::CDynAry<int> 				m_aryiEntryChecked;		// order in which entry points were successfully type checked
	EWC::CHash<HV, SFile *>			m_hashHvPFile;
	CSymbolTable *					m_pSymtab;				// top level symbols

	SErrorManager *					m_pErrman;
	size_t							m_cbFreePrev;
};

void BeginWorkspace(CWorkspace * pWork);
void BeginParse(CWorkspace * pWork, SJaiLexer * pJlex, const char * pChzIn);
void EndParse(CWorkspace * pWork, SJaiLexer * pJlex);
void EndWorkspace(CWorkspace * pWork);

void PerformTypeCheck(
	EWC::CAlloc * pAlloc,
	CSymbolTable * pSymtabTop,
	EWC::CAry<CWorkspace::SEntry> * paryEntry,
	EWC::CAry<int> * paryiEntryChecked);

