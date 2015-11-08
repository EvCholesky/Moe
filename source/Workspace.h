// copyright (C) 2015 Evan Christensen

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
