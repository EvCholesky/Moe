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

#include "EwcArray.h"

namespace EWC
{
	class CAlloc;
	class CParseContext;
	class CSTNode;
	struct SLexerLocation;
}

class CWorkspace;

struct SErrorManager	//  // tag = errman
{
	int			m_cError;
};

void EmitError(SErrorManager * pErrman, EWC::SLexerLocation * pLexloc, const char * pChz, ...);
void EmitError(CWorkspace * pWork, EWC::SLexerLocation * pLexloc, const char * pChz, ...);

class CWorkspace	// tag = work
{
public:

							CWorkspace(EWC::CAlloc * pAlloc, SErrorManager * pErrman);

	EWC::CAlloc *					m_pAlloc;
	EWC::CParseContext *			m_pParctx;
	EWC::CDynAry<EWC::CSTNode *> 	m_arypStnodEntry;
	EWC::CSymbolTable *				m_pSymtab;

	SErrorManager *					m_pErrman;
	size_t							m_cbFreePrev;
};

void BeginWorkspace(CWorkspace * pWork);
void BeginParse(CWorkspace * pWork, EWC::SJaiLexer * pJlex, const char * pChzIn);
void EndParse(CWorkspace * pWork, EWC::SJaiLexer * pJlex);
void EndWorkspace(CWorkspace * pWork);
