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

#include "BigMath.h"
#include "Parser.h"
#include "TypeInfo.h"
#include "Util.h"
#include "Workspace.h"
#include <cstdarg>
#include <limits.h>
#include <stdio.h>

using namespace EWC;

enum TCCTX // tag = Type Check Context
{
	TCCTX_Normal,
	TCCTX_TypeSpecification, // type checking is inside type spec, walking the tree to do literal op evaluation
};

struct STypeCheckStackEntry // tag = tcsent
{
	int				m_nState;
	CSTNode *		m_pStnod;
	CSymbolTable *	m_pSymtab;			// BB - Could omit this pointer with careful handling of stack pops?
										//  maybe swap out for fPushedStack?
	CSTNode *		m_pStnodProcedure;	// definition node for current procedure
	GRFSYMLOOK		m_grfsymlook;
	bool			m_fAllowForwardDecl;
	TCCTX			m_tcctx;
};

struct STypeCheckFrame // tag = tcfram
{
	int								m_ipTcframQueue;	// index in the pending/waiting queue
	CDynAry<STypeCheckStackEntry>	m_aryTcsent;
};

struct SUnknownType // tag = untype
{
	CDynAry<int>	m_aryiTcframDependent;		// id for frames dependent on this type
};



#define VALIDATE_NAME_MANGLING 1

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



struct STypeCheckWorkspace // tag = tcwork
{
					STypeCheckWorkspace(CAlloc * pAlloc, SErrorManager * pErrman, int cTcfram)
					:m_pAlloc(pAlloc)
					,m_pErrman(pErrman)
					,m_mang(pAlloc)
					,m_nIdNext(0)
					,m_aryTcfram()
					,m_hashPSymUntype(pAlloc)
					,m_arypTcframPending()
					,m_arypTcframWaiting()
						{
							m_aryTcfram.AllocArray(m_pAlloc, cTcfram);
							m_arypTcframPending.AllocArray(m_pAlloc, cTcfram);
							m_arypTcframWaiting.AllocArray(m_pAlloc, cTcfram);
						}

					~STypeCheckWorkspace()
						{
							m_pAlloc->EWC_FREE(m_arypTcframWaiting.A());
							m_pAlloc->EWC_FREE(m_arypTcframPending.A());
							m_pAlloc->EWC_FREE(m_aryTcfram.A());
						}
	CAlloc *								m_pAlloc;
	SErrorManager *							m_pErrman;
	int										m_nIdNext;
	CNameMangler							m_mang;
	CAry<STypeCheckFrame>					m_aryTcfram;
	CHash<const SSymbol *, SUnknownType>	m_hashPSymUntype;

	CAry<STypeCheckFrame *>					m_arypTcframPending;	// frames ready to be run again (may stop 
																	//  during check, not guaranteed to have all types)
	CAry<STypeCheckFrame *>					m_arypTcframWaiting;	// frames waiting for one specific symbol
};


extern bool FDoesOperatorExist(JTOK jtok, const SOpTypes * pOptype);
bool FCanExplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst);
void OnTypeComplete(STypeCheckWorkspace * pTcwork, const SSymbol * pSym);
STypeInfo * PTinPromoteUntypedDefault(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, CSTNode * pStnodLit);
bool FDoesOperatorReturnBool(PARK park);

CNameMangler::CNameMangler(EWC::CAlloc * pAlloc, size_t cBStartingMax)
:m_pAlloc(pAlloc)
,m_strbuf()
{
	Resize(cBStartingMax);
}

CNameMangler::~CNameMangler()
{
	Resize(0);
}

void CNameMangler::Resize(size_t cBNew)
{
	char * aBNew = nullptr;
	if (cBNew)
	{
		aBNew = (char *)m_pAlloc->EWC_ALLOC_TYPE_ARRAY(char, cBNew);

		if (m_strbuf.m_cBMax)
		{
			(void) CBCopyCoz(m_strbuf.m_pCozBegin, aBNew, m_strbuf.m_cBMax);
		}
	}

	if (m_strbuf.m_cBMax)
	{
		m_pAlloc->EWC_DELETE(m_strbuf.m_pCozBegin);
	}

	m_strbuf = EWC::SStringBuffer(aBNew, cBNew);
}

void CNameMangler::AppendName(const char * pCoz)
{
	size_t cCh = CCh(pCoz);
	FormatCoz(&m_strbuf, "%d%s", cCh, pCoz);
}

void CNameMangler::AppendType(STypeInfo * pTin)
{
	switch (pTin->m_tink)
	{
    case TINK_Integer:
		{
			// BB - doesn't respect typedefs, including int - will mangle just based on integer size
			char aChz[4] = "Bxx";
			auto pTinint = (STypeInfoInteger *)pTin;
			aChz[1] = (pTinint->m_fIsSigned) ? 'i' : 'u';
			switch (pTinint->m_cBit)
			{
			case 8:		aChz[2] = 'c';	break;	// char
			case 16:	aChz[2] = 's';	break;	// short
			case 32:	aChz[2] = 'w';	break;	// word
			case 64:	aChz[2] = 'd';	break;	// double
			}
			AppendCoz(&m_strbuf, aChz);
		} break;
    case TINK_Float:
		{
			char aChz[3] = "Bx";
			auto pTinfloat = (STypeInfoFloat *)pTin;
			switch (pTinfloat->m_cBit)
			{
			case 32:	aChz[1] = 'g';	break;	// float
			case 64:	aChz[1] = 'd';	break;	// double
			}
			AppendCoz(&m_strbuf, aChz);
		} break;
	case TINK_Bool:		AppendCoz(&m_strbuf, "Bf");	break;
	case TINK_String:	AppendCoz(&m_strbuf, "Bs");	break;
	case TINK_Void:		AppendCoz(&m_strbuf, "Bv");	break;
    case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;
			AppendCoz(&m_strbuf, "P");
			AppendType(pTinptr->m_pTinPointedTo);
		} break;
    case TINK_Struct:	AppendName(pTin->m_strName.PCoz());	break;
    case TINK_Enum:		AppendName(pTin->m_strName.PCoz()); break;
    case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
				{
					FormatCoz(&m_strbuf, "A%d", pTinary->m_c);
					AppendType(pTinary->m_pTin);
				} break;
			case ARYK_Reference:
				{
					AppendCoz(&m_strbuf, "AR");
					AppendType(pTinary->m_pTin);
				} break;
			default: EWC_ASSERT(false, "unhandled array type");
			}
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;
			FormatCoz(&m_strbuf, "F%d_", pTinproc->m_arypTinReturns.C());

			if (pTinproc->m_fHasVarArgs)
			{
				AppendCoz(&m_strbuf, "VA");
			}

			size_t ipTinMax = pTinproc->m_arypTinParams.C();
			for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
			{
				auto pTin = pTinproc->m_arypTinParams[ipTin];
				AppendType(pTin);
			}

			AppendCoz(&m_strbuf, "_"); // return types
			ipTinMax = pTinproc->m_arypTinReturns.C();
			for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
			{
				auto pTin = pTinproc->m_arypTinReturns[ipTin];
				AppendType(pTin);
			}
		} break;
	default: 
		EWC_ASSERT(false, "unexpected type encountered while name mangling a procedure");
	}
}

static inline s64 NReadNumber(const char ** ppCoz)
{
	const char * pCoz = *ppCoz;
	if ((*pCoz < '0') | (*pCoz > '9'))
		return -1;
	
	s64 n = 0;
	while (1)
	{
		if ((*pCoz>= '0') & (*pCoz<= '9'))
			n = n*10 + (*pCoz- '0');
		else
			break;
		++pCoz;
    }

	*ppCoz = pCoz;
	return n;
}

static inline EWC::CString StrReadName(const char ** ppCoz)
{
	auto cCh = NReadNumber(ppCoz);
	if (cCh < 0)
		return EWC::CString("");

	const char * pCozName = *ppCoz;
	*ppCoz = pCozName + cCh;
	return EWC::CString(pCozName, (size_t)cCh);
}

static inline bool FMatchString(const char * pCozRef, const char ** ppCoz)
{
	auto pCozRefIt = pCozRef;
	const char * pCozIt = *ppCoz;
	while (*pCozRefIt != '\0')
	{
		if (*pCozRefIt != *pCozIt)
			return false;
		++pCozRefIt;
		++pCozIt;
	}

	*ppCoz = pCozIt;
	return true;
}

static STypeInfoProcedure * PTinprocAlloc(EWC::CAlloc * pAlloc, size_t cParams, size_t cReturns, const char * pCozName)
{
	size_t cBAlloc = CBAlign(sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));
	cBAlloc = cBAlloc +	(cParams + cReturns) * sizeof(STypeInfo *);

	u8 * pB = (u8 *)pAlloc->EWC_ALLOC(cBAlloc,8);
	STypeInfoProcedure * pTinproc = new(pB) STypeInfoProcedure(pCozName);
	STypeInfo ** ppTin = (STypeInfo**)PVAlign( pB + sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));

	pTinproc->m_arypTinParams.SetArray(ppTin, 0, cParams);
	pTinproc->m_arypTinReturns.SetArray(&ppTin[cParams], 0, cReturns);
	return pTinproc;
}

STypeInfo * PTinReadType(const char ** ppCoz, CSymbolTable * pSymtab)
{
	int chFirst = **ppCoz;
	if (chFirst == 'B')	// built-in type
	{
		++(*ppCoz);
		char chBuiltIn = *(*ppCoz)++;
		switch(chBuiltIn)
		{
		case 'i':
		case 'u':
			{
				char aCh[4];
				EWC::SStringBuffer strbufScratch(aCh, EWC_DIM(aCh));
				*strbufScratch.m_pCozAppend++ = (chBuiltIn == 'i') ? 's' : 'u';
				switch(*(*ppCoz)++)
				{
					case 'c':	AppendCoz(&strbufScratch, "8");		break;
					case 's':	AppendCoz(&strbufScratch, "16");	break;
					case 'w':	AppendCoz(&strbufScratch, "32");	break;
					case 'd':	AppendCoz(&strbufScratch, "64");	break;
				}

				return pSymtab->PTinBuiltin(aCh);
			} 
		case 'g':	return pSymtab->PTinBuiltin("f32");
		case 'd':	return pSymtab->PTinBuiltin("f64");
		case 'f':	return pSymtab->PTinBuiltin("bool");
		case 's':	return pSymtab->PTinBuiltin("string");
		case 'v':	return pSymtab->PTinBuiltin("void");
		default: EWC_ASSERT(false, "unknown built-in type during de-mangling");
		}
	}
	else if (chFirst == 'P') // Pointer
	{
		++(*ppCoz);
		auto pTinPointedTo = PTinReadType(ppCoz, pSymtab);
		if (!pTinPointedTo)
			return nullptr;
		return pSymtab->PTinptrAllocReference(pTinPointedTo);
	}
	else if (chFirst == 'A') // Array
	{
		++(*ppCoz);
		STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();

		if (**ppCoz == 'R') // ARYK_Reference
		{
			++(*ppCoz);
			pTinary->m_aryk = ARYK_Reference;
		}
		else
		{
			pTinary->m_aryk = ARYK_Fixed;
			pTinary->m_c = NReadNumber(ppCoz);
		}

		pTinary->m_pTin = PTinReadType(ppCoz, pSymtab);

		if (pTinary->m_pTin == nullptr || pTinary->m_c < 0)
		{
			pSymtab->m_pAlloc->EWC_DELETE(pTinary);
			return nullptr;
		}
		pSymtab->AddManagedTin(pTinary);
		return pTinary;
	}
	else if (chFirst == 'F') // procedure reference
	{
		++(*ppCoz);
		auto cpTinReturn = NReadNumber(ppCoz);
		if (!FMatchString("_", ppCoz))
			return nullptr;

		bool fHasVarArgs = FMatchString("VA", ppCoz);
		EWC::CDynAry<STypeInfo *> arypTinParams(pSymtab->m_pAlloc, EWC::BK_Stack);
		EWC::CDynAry<STypeInfo *> arypTinReturns(pSymtab->m_pAlloc, EWC::BK_Stack);

		while (!FMatchString("_", ppCoz) && *ppCoz != '\0')
		{
			arypTinParams.Append(PTinReadType(ppCoz, pSymtab));
			if (arypTinParams.Last() == nullptr)
				return nullptr;
		}

		for (int ipTinReturn = 0; ipTinReturn < cpTinReturn; ++ipTinReturn)
		{
			arypTinReturns.Append(PTinReadType(ppCoz, pSymtab));
			if (arypTinReturns.Last() == nullptr)
				return nullptr;
		}

		auto pTinproc = PTinprocAlloc(pSymtab->m_pAlloc, arypTinParams.C(), arypTinReturns.C(), "");
		pTinproc->m_fHasVarArgs = fHasVarArgs;

		int cpTin = arypTinParams.C();
		for (int ipTin = 0; ipTin < cpTin; ++ipTin)
		{
			pTinproc->m_arypTinParams.Append(arypTinParams[ipTin]);
		}

		cpTin = arypTinReturns.C();
		for (int ipTin = 0; ipTin < cpTin; ++ipTin)
		{
			pTinproc->m_arypTinReturns.Append(arypTinReturns[ipTin]);
		}

		pSymtab->AddManagedTin(pTinproc);
		return pTinproc;
	}
	else
	{
		// BB - need to handle namespacing and nesting.

		auto strName = StrReadName(ppCoz);
		SLexerLocation lexloc;
		auto pSym = pSymtab->PSymLookup(strName, lexloc);
		return (pSym) ? pSym->m_pTin : nullptr;
	}

	return nullptr;
}

CString	CNameMangler::StrMangleMethodName(STypeInfoProcedure * pTinproc)
{
	m_strbuf.m_pCozAppend = m_strbuf.m_pCozBegin;
	AppendCoz(&m_strbuf, "__F"); // function

	auto strPunyName = StrPunyEncode(pTinproc->m_strName.PCoz());
	AppendName(strPunyName.PCoz());

	AppendCoz(&m_strbuf, "_"); // arguments

	if (pTinproc->m_fHasVarArgs)
	{
		AppendCoz(&m_strbuf, "VA");
	}

	size_t ipTinMax = pTinproc->m_arypTinParams.C();
	for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
	{
		auto pTin = pTinproc->m_arypTinParams[ipTin];
		AppendType(pTin);
	}

	AppendCoz(&m_strbuf, "_"); // return types
	ipTinMax = pTinproc->m_arypTinReturns.C();
	for (size_t ipTin = 0; ipTin < ipTinMax; ++ipTin)
	{
		auto pTin = pTinproc->m_arypTinReturns[ipTin];
		AppendType(pTin);
	}
	return CString(m_strbuf.m_pCozBegin, m_strbuf.m_pCozAppend - m_strbuf.m_pCozBegin);
}

STypeInfoProcedure * CNameMangler::PTinprocDemangle(const CString & strName, CSymbolTable * pSymtab)
{
	const char * pCoz = strName.PCoz();
	if (!FMatchString("__F", &pCoz))
		return nullptr;

	CString strProcNamePuny = StrReadName(&pCoz);
	CString strProcName = StrPunyDecode(strProcNamePuny.PCoz());

	if (!FMatchString("_", &pCoz))
		return nullptr;

	bool fHasVarArgs = FMatchString("VA", &pCoz);
	EWC::CDynAry<STypeInfo *> arypTinParams(pSymtab->m_pAlloc, EWC::BK_Stack);
	EWC::CDynAry<STypeInfo *> arypTinReturns(pSymtab->m_pAlloc, EWC::BK_Stack);

	while (!FMatchString("_", &pCoz) && pCoz != '\0')
	{
		arypTinParams.Append(PTinReadType(&pCoz, pSymtab));
		if (arypTinParams.Last() == nullptr)
			return nullptr;
	}

	while (*pCoz != '\0')
	{
		arypTinReturns.Append(PTinReadType(&pCoz, pSymtab));
		if (arypTinReturns.Last() == nullptr)
			return nullptr;
	}

	auto pTinproc = PTinprocAlloc(pSymtab->m_pAlloc, arypTinParams.C(), arypTinReturns.C(), strProcName.PCoz());
	pSymtab->AddManagedTin(pTinproc);
	pTinproc->m_fHasVarArgs = fHasVarArgs;

	int cpTin = arypTinParams.C();
	for (int ipTin = 0; ipTin < cpTin; ++ipTin)
	{
		pTinproc->m_arypTinParams.Append(arypTinParams[ipTin]);
	}

	cpTin = arypTinReturns.C();
	for (int ipTin = 0; ipTin < cpTin; ++ipTin)
	{
		pTinproc->m_arypTinReturns.Append(arypTinReturns[ipTin]);
	}

	return pTinproc;
}

// by the time we get to type checking we've parsed the whole program, we should have a tree of symbol tables containing
//	1. names of every global and struct nested types (but not sizing, const values, typedefs)
//  2. all named global identifiers (potentially with some typenames? not sure that helps)

// Note: all declarations within a procedure are expected in-order (for now) so we just add them to the symbol table
//  during type checking.

// Type checking walks over all entry frames, pausing to wait for the type-checking/sizing of types used in any 
//  definitions.

// My (half-cooked) strategy for handling arbitrary ordering within non-global scopes is to insert TypeDefinitions 
//  before any statements in their containing scope.

void EmitError(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, const char * pCozFormat, ...)
{
	// BB - need to do file/line lookup from pStnod
	//printf("%s(%d) Error:", pJlex->m_pChzFilename, NLine(pJlex));
	
	const SLexerLocation & lexloc = pStnod->m_lexloc;

	va_list ap;
	va_start(ap, pCozFormat);
	EmitError(pTcwork->m_pErrman, &lexloc, pCozFormat, ap);
}

void AllocateOptype(CSTNode * pStnod)
{
	CAlloc * pAlloc = pStnod->m_arypStnodChild.m_pAlloc;
	pStnod->m_pOptype = EWC_NEW(pAlloc, SOpTypes) SOpTypes();
}

CString StrTypenameFromTypeSpecification(CSTNode * pStnod)
{
	char aCh[2048];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	CSTNode * pStnodIt = pStnod;
	while (pStnodIt)
	{
		switch (pStnodIt->m_park)
		{
			case PARK_Identifier:
			{
				if (!EWC_FVERIFY(pStnod->m_pStval, "identifier without value string detected"))
					break;
				AppendCoz(&strbuf, StrFromIdentifier(pStnod).PCoz()); 
				pStnodIt = nullptr;
			}break;
			case PARK_ReferenceDecl:

				AppendCoz(&strbuf, "* "); 

				EWC_ASSERT(pStnodIt->CStnodChild() == 1);
				pStnodIt = pStnodIt->PStnodChild(0);
				break;
			case PARK_ArrayDecl:
				// BB - should follow the [], [..], [c] convention
				EWC_ASSERT(false, "not type-checking asserts yet");
				pStnodIt=  nullptr;

				break;
			default:
				AppendCoz(&strbuf, "<BadPark> "); 
				pStnod = nullptr;
				break;
		}
	}

	return CString(aCh);
}

CString StrFromTypeInfo(STypeInfo * pTin)
{
	char aCh[1024];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	PrintTypeInfo(&strbuf, pTin, PARK_Nil);
	return CString(aCh);
}

CString StrFullyQualifiedSymbol(SSymbol * pSym)
{
	char aCh[256];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
	FormatCoz(&strbuf, "TBD::TBD::%s",pSym->m_strName.PCoz());
	return CString(aCh);
}

SUnknownType * PUntypeEnsure(STypeCheckWorkspace * pTcwork, const SSymbol * pSym)
{
	SUnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
	{
		pTcwork->m_hashPSymUntype.FinsEnsureKey(pSym, &pUntype);
		pUntype->m_aryiTcframDependent.SetAlloc(pTcwork->m_pAlloc, EWC::BK_TypeCheck);
	}
	return pUntype;
}

void PushTcsent(STypeCheckFrame * pTcfram, STypeCheckStackEntry ** ppTcsentTop, CSTNode * pStnod)
{
	// update ppTcsentTop to handle times when the dynArray reallocs.
	size_t iTcsentTop = pTcfram->m_aryTcsent.IFromP(*ppTcsentTop);

	EWC_ASSERT(pStnod->m_strees < STREES_TypeChecked, "Pushing syntax tree node that was already checked");
	size_t cPrev = pTcfram->m_aryTcsent.C()-1;
	STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
	*pTcsent = pTcfram->m_aryTcsent[cPrev];

	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnod;

	*ppTcsentTop = &pTcfram->m_aryTcsent[iTcsentTop];
}

void PopTcsent(STypeCheckFrame * pTcfram, STypeCheckStackEntry ** ppTcsentTop, CSTNode * pStnodDebug)
{
	*ppTcsentTop = nullptr;
	EWC_ASSERT(pTcfram->m_aryTcsent.PLast()->m_pStnod == pStnodDebug || pStnodDebug == nullptr);

	pTcfram->m_aryTcsent.PopLast();
}
enum TCRET
{
	TCRET_Complete,
	TCRET_StoppingError,
	TCRET_WaitingForSymbolDefinition,
};

inline u64 NUnsignedLiteralCast(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, const CSTValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:
		return pStval->m_nUnsigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
	case STVALK_SignedInt:
		{
			if (pStval->m_nSigned < 0)
			{
				EmitError(pTcwork, pStnod, "Implicit cast will discard negative value");
			}
			return (u64)pStval->m_nSigned;
		}
	case STVALK_ReservedWord:
		{
			if (EWC_FVERIFY(pStval->m_rword == RWORD_LineDirective, "unexpected reserved word"))
			{
				return pStval->m_nUnsigned;
			}
			return 0;
		}
	default:
		EWC_ASSERT(false, "bad literal cast to unsigned int");
		return 0;
	}
}

inline s64 NSignedLiteralCast(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, const CSTValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:
		{
			if (pStval->m_nUnsigned > LLONG_MAX)
			{
				EmitError(pTcwork, pStnod, "Literal is too large for implicit signed int cast.");
			}
			return (s64)pStval->m_nUnsigned;
		}
	case STVALK_SignedInt:
		return pStval->m_nSigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
	case STVALK_ReservedWord:
		{
			if (EWC_FVERIFY(pStval->m_rword == RWORD_LineDirective, "unexpected reserved word"))
			{
				return pStval->m_nUnsigned;
			}
			return 0;
		}
	default:
		EWC_ASSERT(false, "bad literal cast to signed int");
		return 0;
	}
}

inline F64 GLiteralCast(const CSTValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_UnsignedInt:	return (F64)pStval->m_nUnsigned;
	case STVALK_SignedInt:		return (F64)pStval->m_nSigned;
	case STVALK_Float:			return pStval->m_g;
	default: EWC_ASSERT(false, "expected number");
	}
	return 0.0;
}

SBigInt BintFromStval(CSTValue * pStval)
{
	switch (pStval->m_stvalk)
	{
	case STVALK_SignedInt:		return BintFromInt(pStval->m_nSigned);
	case STVALK_UnsignedInt:	return BintFromUint(pStval->m_nUnsigned, false);
	case STVALK_ReservedWord:
		{

			EWC_ASSERT(pStval->m_litkLex == LITK_Integer || pStval->m_litkLex == LITK_Bool, "Can't create Bint from non integer reserved word");
			return BintFromUint(pStval->m_nUnsigned, false);
		}
	default:
		EWC_ASSERT(false, "Can't create Bint from non integer value");
		return SBigInt();
	}
}

STypeInfo * PTinFromBint(
	STypeCheckWorkspace *pTcwork,
	CSymbolTable * pSymtab,
	SBigInt bint)
{
	if (!bint.m_fIsNegative)
	{
		s64 nUnsigned = bint.U64Coerce();
		if (nUnsigned <= UCHAR_MAX)	return pSymtab->PTinBuiltin("u8");
		if (nUnsigned <= USHRT_MAX)	return pSymtab->PTinBuiltin("u16");
		if (nUnsigned <= UINT_MAX)	return pSymtab->PTinBuiltin("u32");
		return pSymtab->PTinBuiltin("u64");
	}
	
	s64 nSigned = bint.S64Coerce();
	if ((nSigned <= SCHAR_MAX) & (nSigned > SCHAR_MIN))	return pSymtab->PTinBuiltin("s8");
	if ((nSigned <= SHRT_MAX) & (nSigned > SHRT_MIN))	return pSymtab->PTinBuiltin("s16");
	if ((nSigned <= INT_MAX) & (nSigned > INT_MIN))		return pSymtab->PTinBuiltin("s32");
	return pSymtab->PTinBuiltin("s64");
}

inline void SetIntegerValue(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, CSTValue * pStval, const SBigInt bint)
{
	if (bint.m_fIsNegative)
	{
		if (bint.m_nAbs > LLONG_MAX)
		{
			EmitError(pTcwork, pStnod, "Literal value overflow. Value is too large for signed int.");
		}
		SetSignedIntValue(pStval, bint.S64Coerce());
	}
	else
	{
		SetUnsignedIntValue(pStval, bint.U64Coerce());
	}
}

inline bool FComputeUnaryOpOnLiteral(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodOperator,
	CSymbolTable * pSymtab,
	CSTNode * pStnodOperand,
	STypeInfoLiteral ** ppTinOperand,
	STypeInfoLiteral ** ppTinReturn,
	CSTValue ** ppStval)
{
	STypeInfo * pTinOperand = pStnodOperand->m_pTin;

	if ((pTinOperand->m_tink != TINK_Literal) | (pStnodOperand->m_pStval == nullptr))
		return false;

	STypeInfoLiteral * pTinlitOperand = (STypeInfoLiteral *)pTinOperand;
	const SLiteralType & littyOperand = pTinlitOperand->m_litty;
	CSTValue * pStvalOperand = pStnodOperand->m_pStval;

	bool fOperandIsNumber = (littyOperand.m_litk == LITK_Float) | (littyOperand.m_litk == LITK_Integer) | (littyOperand.m_litk == LITK_Enum);
	if (!fOperandIsNumber)
		return false;

	int n = +(-3);

	JTOK jtokOperator = pStnodOperator->m_jtok;
	bool fIsBoolOp = FDoesOperatorReturnBool(pStnodOperator->m_park);

	if (littyOperand.m_litk == LITK_Float)
	{
		bool f;
		F64 g = GLiteralCast(pStvalOperand);
		switch (jtokOperator)
		{
		case JTOK('-'):         g = -g; break;
		case JTOK('!'):         f = !g; break;
		default: return false;
		}

		CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
		*ppStval = pStvalStnod;

		if (fIsBoolOp)
		{
			SetBoolValue(pStvalStnod, f);
	
			STypeInfoLiteral * pTinBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			EWC_ASSERT(!pTinBool || pTinBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinBool;
			*ppTinOperand = pTinlitOperand;
		}
		else
		{
			SetFloatValue(pStvalStnod, g);

			EWC_ASSERT(pTinlitOperand->m_litty.m_litk == LITK_Float);
			*ppTinReturn = pTinlitOperand;
			*ppTinOperand = pTinlitOperand;
		}
		return true;
	} 
	else // LITK_Integer
	{
		EWC_ASSERT(littyOperand.m_cBit == -1, "expected unsized literal here");

		SBigInt bintOperand(BintFromStval(pStvalOperand));

		bool f;
		switch (jtokOperator)
		{
		case JTOK('-'):         bintOperand.m_fIsNegative = !bintOperand.m_fIsNegative; break;
		// BB - We're not really handling unsized literals correctly here - we'll just promote to a 64 bit type
		case JTOK('~'):       bintOperand = BintBitwiseNot(bintOperand); break;

		case JTOK('!'):         f = bintOperand.m_nAbs == 0; break;
		default: return false;
		}

		CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
		*ppStval = pStvalStnod;

		if (fIsBoolOp)
		{
			SetBoolValue(pStvalStnod, f);
	
			STypeInfoLiteral * pTinBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			EWC_ASSERT(!pTinBool || pTinBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinBool;
			*ppTinOperand = pTinlitOperand;
		}
		else
		{
			SetIntegerValue(pTcwork, pStnodOperand, pStvalStnod, bintOperand);

			if (pTinlitOperand->m_litty.m_litk == LITK_Enum)
			{
				// We need to make an unfinalized integer literal
				auto pTinlitInt = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
				pTinlitInt->m_litty.m_litk = LITK_Integer;
				pSymtab->AddManagedTin(pTinlitInt);

				*ppTinReturn = pTinlitInt;
				*ppTinOperand = pTinlitOperand;
			}
			else
			{
				EWC_ASSERT(pTinlitOperand->m_litty.m_litk == LITK_Integer);
				*ppTinReturn = pTinlitOperand;
				*ppTinOperand = pTinlitOperand;
			}
		}
		return true;
	}
}

inline bool FComputeBinaryOpOnLiterals(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodOperator,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLhs,
	CSTNode * pStnodRhs, 
	STypeInfoLiteral ** ppTinOperand,
	STypeInfoLiteral ** ppTinReturn,
	CSTValue ** ppStval)
{
	STypeInfo * pTinLhs = pStnodLhs->m_pTin;
	STypeInfo * pTinRhs = pStnodRhs->m_pTin;

	if ((pTinLhs->m_tink != TINK_Literal) | (pTinRhs->m_tink != TINK_Literal) |
		(pStnodLhs->m_pStval == nullptr) | (pStnodRhs->m_pStval == nullptr))
		return false;

	STypeInfoLiteral * pTinlitLhs = (STypeInfoLiteral *)pTinLhs;
	STypeInfoLiteral * pTinlitRhs = (STypeInfoLiteral *)pTinRhs;
	const SLiteralType & littyLhs = pTinlitLhs->m_litty;
	const SLiteralType & littyRhs = pTinlitRhs->m_litty;
	CSTValue * pStvalLhs = pStnodLhs->m_pStval;
	CSTValue * pStvalRhs = pStnodRhs->m_pStval;

	bool fLhsIsNumber = (littyLhs.m_litk == LITK_Float) | (littyLhs.m_litk == LITK_Integer) | (littyLhs.m_litk == LITK_Bool) | (littyLhs.m_litk == LITK_Enum);
	bool fRhsIsNumber = (littyRhs.m_litk == LITK_Float) | (littyRhs.m_litk == LITK_Integer) | (littyRhs.m_litk == LITK_Bool) | (littyRhs.m_litk == LITK_Enum);
	if ((fLhsIsNumber == false) | (fRhsIsNumber == false))
		return false;

	JTOK jtokOperator = pStnodOperator->m_jtok;
	bool fIsBoolOp = FDoesOperatorReturnBool(pStnodOperator->m_park);

	// NOTE: the *RIGHT* thing to do here is to use arbitrary precision floats, otherwise we'll lose some
	//  precision if the constants are ever turned into float before assignment

	// if lhs or rhs are float, upcast to float
	if ((littyLhs.m_litk == LITK_Float) | (littyRhs.m_litk == LITK_Float))
	{
		F64 g;
		bool f;
		F64 gLhs = GLiteralCast(pStvalLhs);
		F64 gRhs = GLiteralCast(pStvalRhs);
		switch (jtokOperator)
		{
		case JTOK('+'):         g = gLhs + gRhs; break;
		case JTOK('-'):         g = gLhs - gRhs; break;
		case JTOK('*'):         g = gLhs * gRhs; break;
		case JTOK('/'):         g = gLhs / gRhs; break;
		case JTOK('>'):         f = gLhs > gRhs; break;
		case JTOK('<'):         f = gLhs < gRhs; break;
		case JTOK_EqualEqual:   f = gLhs == gRhs; break;
		case JTOK_NotEqual:     f = gLhs != gRhs; break;
		case JTOK_LessEqual:    f = gLhs <= gRhs; break;
		case JTOK_GreaterEqual:	f = gLhs >= gRhs; break;
		default: return false;
		}

		CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
		*ppStval = pStvalStnod;

		if (fIsBoolOp)
		{
			SetBoolValue(pStvalStnod, f);
	
			STypeInfoLiteral * pTinBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			EWC_ASSERT(!pTinBool || pTinBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinBool;
			*ppTinOperand = pTinlitLhs;
		}
		else
		{
			SetFloatValue(pStvalStnod, g);

			auto pTinlitFloat = (pTinlitLhs->m_litty.m_litk == LITK_Float) ? pTinlitLhs : pTinlitRhs;
			*ppTinReturn = pTinlitFloat;
			*ppTinOperand = pTinlitFloat;
		}
		return true;
	} 
	else // both LITK_Integer
	{
		EWC_ASSERT(littyLhs.m_cBit == -1, "expected unsized literal here");
		EWC_ASSERT(littyRhs.m_cBit == -1, "expected unsized literal here");

		SBigInt bintLhs(BintFromStval(pStvalLhs));
		SBigInt bintRhs(BintFromStval(pStvalRhs));

		SBigInt bintOut;
		bool f;
		switch (jtokOperator)
		{
		case JTOK('+'):         bintOut = BintAdd(bintLhs, bintRhs); break;
		case JTOK('-'):         bintOut = BintSub(bintLhs, bintRhs); break;
		case JTOK('*'):         bintOut = BintMul(bintLhs, bintRhs); break;
		case JTOK('/'):         bintOut = BintDiv(bintLhs, bintRhs); break;
		case JTOK('%'):         bintOut = BintRemainder(bintLhs, bintRhs); break;
		case JTOK('|'):         bintOut = BintBitwiseOr(bintLhs, bintRhs); break;
		case JTOK('&'):         bintOut = BintBitwiseAnd(bintLhs, bintRhs); break;
		case JTOK('^'):         bintOut = BintBitwiseXor(bintLhs, bintRhs); break;
		case JTOK_ShiftRight:	bintOut = BintShiftRight(bintLhs, bintRhs); break;
		case JTOK_ShiftLeft:	bintOut = BintShiftLeft(bintLhs, bintRhs); break;
		case JTOK('>'):         f = bintLhs > bintRhs; break;
		case JTOK('<'):         f = bintLhs < bintRhs; break;
		case JTOK_EqualEqual:   f = bintLhs == bintRhs; break;
		case JTOK_NotEqual:     f = bintLhs != bintRhs; break;
		case JTOK_LessEqual:    f = bintLhs <= bintRhs; break;
		case JTOK_GreaterEqual:	f = bintLhs >= bintRhs; break;
		default: return false;
		}

		CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
		*ppStval = pStvalStnod;

		if (fIsBoolOp)
		{
			SetBoolValue(pStvalStnod, f);
			STypeInfoLiteral * pTinlitBool = pSymtab->PTinlitFromLitk(LITK_Bool);
			EWC_ASSERT(!pTinlitBool || pTinlitBool->m_tink == TINK_Literal, "expected literal type");

			*ppTinReturn = pTinlitBool;
			*ppTinOperand = pTinlitLhs;
		}
		else
		{
			SetIntegerValue(pTcwork, pStnodLhs, pStvalStnod, bintOut);

			if (pTinlitLhs->m_litty.m_litk == LITK_Enum)
			{
				// We need to make an unfinalized integer literal
				auto pTinlitInt = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
				pTinlitInt->m_litty.m_litk = LITK_Integer;
				pSymtab->AddManagedTin(pTinlitInt);

				*ppTinReturn = pTinlitInt;
				*ppTinOperand = pTinlitLhs;
			}
			else
			{
				EWC_ASSERT(pTinlitLhs->m_litty.m_litk == LITK_Integer || pTinlitLhs->m_litty.m_litk == LITK_Bool);
				*ppTinReturn = pTinlitLhs;
				*ppTinOperand = pTinlitLhs;
			}
		}
		return true;
	}
}

void FinalizeLiteralType(CSymbolTable * pSymtab, STypeInfo * pTinDst, CSTNode * pStnodLit)
{
	if (!pStnodLit->m_pTin || pStnodLit->m_pTin->m_tink != TINK_Literal)
		return;

	auto pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;

	// we've found the place the literal will become 'typed' - flush that type back down into the literal
	// Note: we may re-finalize finalized literals here when using a typed constant (ie "SomeConst : s8 : 2;" )

	EWC_ASSERT(pTinDst->m_tink != TINK_Literal, "cannot finalize literal with literal");
	switch (pTinDst->m_tink)
	{
	case TINK_Integer:
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTinDst;
			pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Integer, pTinint->m_cBit, pTinint->m_fIsSigned);
		}break;
    case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTinDst;
			pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Float, pTinfloat->m_cBit, true);
		}break;
	case TINK_Bool:		pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Bool);	break;
    case TINK_String:	pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_String);	break;
	case TINK_Procedure:
		{
			LITK litkPrev = LITK_Nil;
			STypeInfoLiteral * pTinlitPrev = (STypeInfoLiteral *)pStnodLit->m_pTin;
			STypeInfoLiteral * pTinlit = nullptr;
			
			switch (pTinlitPrev->m_litty.m_litk)
			{
			case LITK_Null:		
				{
					pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
					pSymtab->AddManagedTin(pTinlit);
					pTinlit->m_litty.m_litk = LITK_Null;
					pTinlit->m_litty.m_cBit = -1;
					pTinlit->m_litty.m_fIsSigned = false;
					pTinlit->m_fIsFinalized = true;
					pTinlit->m_pTinSource = (STypeInfoPointer*)pTinDst;
				} break;
			default: EWC_ASSERT(false, "unexpected literal type");
			}

			if (pTinlit)
			{
				pStnodLit->m_pTin = pTinlit;
			}
		} break;
    case TINK_Pointer:
		{
			LITK litkPrev = LITK_Nil;
			STypeInfoLiteral * pTinlitPrev = (STypeInfoLiteral *)pStnodLit->m_pTin;
			STypeInfoLiteral * pTinlit = nullptr;
			
			switch (pTinlitPrev->m_litty.m_litk)
			{
			case LITK_String:	pTinlit = pSymtab->PTinlitFromLitk(LITK_String);	break;
			case LITK_Null:		
				{
					pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
					pSymtab->AddManagedTin(pTinlit);
					pTinlit->m_litty.m_litk = LITK_Null;
					pTinlit->m_litty.m_cBit = -1;
					pTinlit->m_litty.m_fIsSigned = false;
					pTinlit->m_fIsFinalized = true;
					pTinlit->m_pTinSource = (STypeInfoPointer*)pTinDst;
				} break;
			case LITK_Integer:	
				{
					CSTValue * pStval = pStnodLit->m_pStval;
					pTinlit = pSymtab->PTinlitFromLitk(LITK_Integer, 64, pStval->m_stvalk == STVALK_SignedInt);
				} break;
			default: EWC_ASSERT(false, "unexpected literal type");
			}

			if (pTinlit)
			{
				pStnodLit->m_pTin = pTinlit;
			}
		} break;
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTinDst;

			auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinenum->m_pTinLoose);
			if (EWC_FVERIFY(pTinint, "Expected integer 'loose' type for enum"))
			{
				pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Integer, pTinint->m_cBit, pTinint->m_fIsSigned);
			}
		} break;
	case TINK_Array:
		{
			if (!EWC_FVERIFY(pTinlit->m_litty.m_litk == LITK_Array, "finalizing array with non-array literal"))
				break;
			
			CSTNode * pStnodDef = pStnodLit;
			if (EWC_FVERIFY(pTinlit->m_pStnodDefinition, "bad array literal definition"))
			{
				pStnodDef = pTinlit->m_pStnodDefinition;
			}
			
			auto pStdecl = pStnodDef->m_pStdecl;
			if (!pStdecl)
				break;

			auto pStnodList = pStnodDef->PStnodChildSafe(pStdecl->m_iStnodInit);
			if (pStnodList)
			{
				for (int iStnod = 0; iStnod < pStnodList->CStnodChild(); ++iStnod)
				{
					FinalizeLiteralType(pSymtab, pTinlit->m_pTinSource, pStnodList->PStnodChild(iStnod));
				}
			}

		} break;
	case TINK_Null: // fall through
	case TINK_Void: // fall through
	default:
		EWC_ASSERT(false, "unexpected type");
	}
}

STypeInfo *PTinPromoteVarArg(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, STypeInfo * pTinIn)
{
	// C99 requires that all floats are promoted to double and all integers < 32 bit are promoted to 32 bit.

	switch (pTinIn->m_tink)
	{
	case TINK_Integer:
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger*)pTinIn;
			if (pTinint->m_cBit < 32)
			{
				return pSymtab->PTinBuiltin((pTinint->m_fIsSigned) ? "s32" : "u32");
			}
			return pTinIn;
		}
	case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTinIn;
			if (pTinfloat->m_cBit < 64)
			{
				return pSymtab->PTinBuiltin("f64");
			}
			return pTinIn;
		}
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTinIn;
			return PTinPromoteVarArg(pTcwork, pSymtab, pTinenum->m_pTinLoose);
		}
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTinIn;
			return pSymtab->PTinptrAllocReference(pTinary->m_pTin);
		}
	default: return pTinIn;
	}
}

inline STypeInfo * PTinFromLiteralFinalized(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	const STypeInfoLiteral * pTinlit)
{
	EWC_ASSERT(pTinlit->m_fIsFinalized, "Expected finalized literal type");

	const SLiteralType & litty = pTinlit->m_litty;
	switch (litty.m_litk)
	{
	case LITK_Integer:
		{
			if (litty.m_fIsSigned)
			{
				switch (litty.m_cBit)
				{
				case 8:		return pSymtab->PTinBuiltin("s8");
				case 16:	return pSymtab->PTinBuiltin("s16");
				case 32:	return pSymtab->PTinBuiltin("s32");
				case 64:	return pSymtab->PTinBuiltin("s64");
				}
			}
			else
			{
				switch (litty.m_cBit)
				{
				case 8:		return pSymtab->PTinBuiltin("u8");
				case 16:	return pSymtab->PTinBuiltin("u16");
				case 32:	return pSymtab->PTinBuiltin("u32");
				case 64:	return pSymtab->PTinBuiltin("u64");
				}
			}
		}break;
	case LITK_Float:
		{ 
			switch (litty.m_cBit)
			{
			case 32:	return pSymtab->PTinBuiltin("f32");
			case 64:	return pSymtab->PTinBuiltin("f64");
			}
		} break;
	case LITK_Char:		return pSymtab->PTinBuiltin("char");
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_String:
		{
			// right now string literals just promote to *u8, but will eventually promote to string
			auto pTinU8 = pSymtab->PTinBuiltin("u8");
			return pSymtab->PTinptrAllocReference(pTinU8);
		} break;
	case LITK_Enum:
		{
			EWC_ASSERT(false, "enum literals should not be finalized");
		} break;
	}

	EWC_ASSERT(false, "Unknown literal kind");
	return nullptr;
}

static inline STypeInfo * PTinPromoteUntypedCommon(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, bool * pFWasHandled, CSTNode * pStnodLit)
{
	*pFWasHandled = true;
	STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	if (!pTinlit)
		return pStnodLit->m_pTin;

	if (pTinlit->m_tink != TINK_Literal)
		return pStnodLit->m_pTin;

	if (pTinlit->m_litty.m_litk == LITK_Array)
	{
		// if this is a constant we need to look up the source STNode
		if (EWC_FVERIFY(pTinlit->m_pStnodDefinition, "bad array literal definition"))
		{
			pStnodLit = pTinlit->m_pStnodDefinition;
		}

		auto pDecl = pStnodLit->m_pStdecl;
		if (!EWC_FVERIFY(pDecl, "bad array literal"))
			return nullptr;

		auto pStnodValues = pStnodLit->PStnodChildSafe(pDecl->m_iStnodInit);

		if (!pTinlit->m_pTinSource &&
			EWC_FVERIFY(pStnodValues && pStnodValues->CStnodChild(), "Array literal has no child literals"))
		{
			pTinlit->m_pTinSource = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodValues->PStnodChild(0));
		}

		STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
		pSymtab->AddManagedTin(pTinary);
		pTinary->m_pTin = pTinlit->m_pTinSource;
		pTinary->m_c = pTinlit->m_c;
		pTinary->m_aryk = ARYK_Fixed;

		return pTinary;
	}

	if (pTinlit->m_fIsFinalized)
		return PTinFromLiteralFinalized(pTcwork, pSymtab, pTinlit);

	const CSTValue * pStval = pStnodLit->m_pStval;
	if (!EWC_FVERIFY(pStval, "literal without value"))
		return nullptr;

	*pFWasHandled = false;
	return nullptr;
}


STypeInfo * PTinPromoteUntypedDefault(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, CSTNode * pStnodLit)
{
	if (pStnodLit->m_park == PARK_Cast)
	{
		auto pStdecl = pStnodLit->m_pStdecl;
		bool fIsAutoCast = pStdecl && pStdecl->m_iStnodType < 0;
		if (fIsAutoCast)
		{
			EmitError(pTcwork, pStnodLit, "Cannot resolve acast when the left hand side is untyped.");
			return PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit->PStnodChildSafe(pStdecl->m_iStnodInit));
		}
	}

	bool fWasHandled;
	STypeInfo * pTinReturn = PTinPromoteUntypedCommon(pTcwork, pSymtab, &fWasHandled, pStnodLit);
	if (fWasHandled)
		return pTinReturn;

	STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	const SLiteralType & litty = pTinlit->m_litty;

	switch (litty.m_litk)
	{
	case LITK_Integer:
		{
			bool fIsSigned = litty.m_fIsSigned;
			if (fIsSigned == false)
			{
				const CSTValue * pStval = pStnodLit->m_pStval;
				s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStnodLit, pStval);
				fIsSigned = (nUnsigned < LLONG_MAX);
			}
			return pSymtab->PTinBuiltin((fIsSigned) ? "int" : "uint");
		}
	case LITK_Float:	return pSymtab->PTinBuiltin("float");
	case LITK_Char:		return pSymtab->PTinBuiltin("char");
	case LITK_String:
	{
		// right now string literals just promote to *u8, but will eventually promote to string
		auto pTinU8 = pSymtab->PTinBuiltin("u8");
		return pSymtab->PTinptrAllocReference(pTinU8);
	}
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:
		{
			EmitError(pTcwork, pStnodLit, "Cannot infer type for null");
		}break;
	case LITK_Enum:
		{
			auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinlit->m_pTinSource);
			EWC_ASSERT(pTinenum, "Failed to infer type for enum literal");
			return pTinenum;
		}
	case LITK_Nil: 
		EWC_ASSERT(false, "Cannot infer type for LITK_Nil");
	}
	return nullptr;
}

STypeInfo * PTinPromoteUntypedArgument(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,
	STypeInfo * pTinArgument)
{
	STypeInfoLiteral * pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodLit->m_pTin);
	if (pTinlit)
	{
		const SLiteralType & litty = pTinlit->m_litty;
		if (litty.m_litk == LITK_Null)
		{
			if (pTinArgument && pTinArgument->m_tink == TINK_Pointer )
				return pTinArgument;

			return pSymtab->PTinptrAllocReference(pSymtab->PTinBuiltin("void"));
		}
	}

	return PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit);
}

inline STypeInfo * PTinPromoteUntypedTightest(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,	
	STypeInfo * pTinDst)
{
	if (pStnodLit->m_park == PARK_Cast)
	{
		auto pStdecl = pStnodLit->m_pStdecl;
		bool fIsAutoCast = pStdecl && pStdecl->m_iStnodType < 0;
		if (fIsAutoCast)
		{
			auto pStnodInit = pStnodLit->PStnodChildSafe(pStdecl->m_iStnodInit);
			STypeInfo * pTinInit = (pStnodInit) ? pStnodInit->m_pTin : nullptr;

			if (EWC_FVERIFY(pStnodInit, "finalizing auto cast with no target type"))
			{
				if (FCanExplicitCast(pTinInit, pTinDst))
				{
					pStnodLit->m_pTin = pTinDst;
					(void) PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pTinDst);
				}
				else
				{
					EmitError(pTcwork, pStnodLit, "Cannot auto cast type '%s' to '%s'",
						StrFromTypeInfo(pTinInit).PCoz(),
						StrFromTypeInfo(pTinDst).PCoz());
				}
				return pStnodLit->m_pTin;
			}
		}
	}

	bool fWasHandled;
	STypeInfo * pTinReturn = PTinPromoteUntypedCommon(pTcwork, pSymtab, &fWasHandled, pStnodLit);
	if (fWasHandled)
		return pTinReturn;

	STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	const SLiteralType & litty = pTinlit->m_litty;
	switch (litty.m_litk)
	{
	case LITK_Enum:
		{
			if (pTinDst->m_tink == TINK_Enum)
			{
				auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinlit->m_pTinSource);
				if (EWC_FVERIFY(pTinenum, "bad enum literal"))
				{
					return pTinenum;
				}
			}

			SBigInt bintEnum = BintFromStval(pStnodLit->m_pStval);

			bool fDestIsSigned = pTinDst->m_tink != TINK_Integer || ((STypeInfoInteger*)pTinDst)->m_fIsSigned;
			bintEnum.m_fIsNegative |= fDestIsSigned;

			return PTinFromBint(pTcwork, pSymtab, bintEnum);
		}
	case LITK_Integer:
		{
			// NOTE: We're casting the value to fit the type info here, not letting the value determine the type.

			if (pTinDst->m_tink == TINK_Float)
			{
				// integer literals can be used to initialize floating point numbers
				return pSymtab->PTinBuiltin("f32");
			}

			const CSTValue * pStval = pStnodLit->m_pStval;
			bool fDestIsSigned = pTinDst->m_tink != TINK_Integer || ((STypeInfoInteger*)pTinDst)->m_fIsSigned;
			bool fIsValNegative = pStval->m_stvalk == STVALK_SignedInt && pStval->m_nSigned < 0;

			if (fDestIsSigned == false && fIsValNegative == false)
			{
				s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStnodLit, pStval);
				if (nUnsigned <= UCHAR_MAX)	return pSymtab->PTinBuiltin("u8");
				if (nUnsigned <= USHRT_MAX)	return pSymtab->PTinBuiltin("u16");
				if (nUnsigned <= UINT_MAX)	return pSymtab->PTinBuiltin("u32");
				return pSymtab->PTinBuiltin("u64");
			}

			// NOTE - if this value isn't explicitly negative, allow code to initialize it with 
			//  values large enough to cause it to be negative. ie. n:s32=0xFFFFFFFF;
			s64 nSigned = NSignedLiteralCast(pTcwork, pStnodLit, pStval);
			if ((nSigned & ~0x00000000000000FF) == 0)	return pSymtab->PTinBuiltin("s8");
			if ((nSigned & ~0x000000000000FFFF) == 0)	return pSymtab->PTinBuiltin("s16");
			if ((nSigned & ~0x00000000FFFFFFFF) == 0)	return pSymtab->PTinBuiltin("s32");
			return pSymtab->PTinBuiltin("s64");
		}
	case LITK_Float:	return pSymtab->PTinBuiltin("float");
	case LITK_Char:
		{
			const CSTValue * pStval = pStnodLit->m_pStval;
			bool fDestIsSigned = pTinDst->m_tink == TINK_Integer && ((STypeInfoInteger*)pTinDst)->m_fIsSigned;
			if (fDestIsSigned)
			{
				s64 nSigned = NSignedLiteralCast(pTcwork, pStnodLit, pStval);
				if ((nSigned <= SCHAR_MAX) & (nSigned > SCHAR_MIN))	return pSymtab->PTinBuiltin("s8");
				if ((nSigned <= SHRT_MAX) & (nSigned > SHRT_MIN))	return pSymtab->PTinBuiltin("s16");
				return pSymtab->PTinBuiltin("s32");
			}

			s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStnodLit, pStval);
			if (nUnsigned <= UCHAR_MAX)	return pSymtab->PTinBuiltin("u8");
			if (nUnsigned <= USHRT_MAX)	return pSymtab->PTinBuiltin("u16");
			return pSymtab->PTinBuiltin("char");
		}
	case LITK_String:
	{
		// right now string literals just promote to *u8, but will eventually promote to string
		auto pTinU8 = pSymtab->PTinBuiltin("u8");
		return pSymtab->PTinptrAllocReference(pTinU8);
	}
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:		
		{
			if (pTinDst && (pTinDst->m_tink == TINK_Pointer || pTinDst->m_tink == TINK_Procedure))
				return pTinDst;
			EmitError(pTcwork, pStnodLit, "Trying to initialize non pointer type with null value");
		} break;
	case LITK_Nil: 
		EWC_ASSERT(false, "Cannot infer type for LITK_Nil");
	}
	return nullptr;
}

bool FTypesAreSame(STypeInfo * pTinLhs, STypeInfo * pTinRhs)
{
	if (pTinLhs == pTinRhs)
		return true;

	if (!pTinLhs || !pTinRhs || pTinLhs->m_tink != pTinRhs->m_tink)
		return false;
	
	switch(pTinLhs->m_tink)
	{
		// BB - We'll need to be a bit more explicit here if we're going to support some kind of explicit typedefs
	case TINK_Float:	return ((STypeInfoFloat *)pTinLhs)->m_cBit == ((STypeInfoFloat *)pTinRhs)->m_cBit;
	case TINK_Integer:	
		{
			STypeInfoInteger * pTinintLhs = (STypeInfoInteger *)pTinLhs;
			STypeInfoInteger * pTinintRhs = (STypeInfoInteger *)pTinRhs;
			return (pTinintLhs->m_cBit == pTinintRhs->m_cBit) & (pTinintLhs->m_fIsSigned == pTinintRhs->m_fIsSigned);
		}

	case TINK_Pointer:	return FTypesAreSame(
								((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo, 
								((STypeInfoPointer *)pTinRhs)->m_pTinPointedTo);
	case TINK_Array:	
		{
			auto pTinaryLhs = (STypeInfoArray *)pTinLhs;
			auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
			return (pTinaryLhs->m_aryk == pTinaryRhs->m_aryk) & (pTinaryLhs->m_c == pTinaryRhs->m_c) &&
				FTypesAreSame(((STypeInfoArray *)pTinLhs)->m_pTin, ((STypeInfoArray *)pTinRhs)->m_pTin);
		}
	case TINK_Struct:
		{
			auto pTinstructLhs = (STypeInfoStruct *)pTinLhs;
			auto pTinstructRhs = (STypeInfoStruct *)pTinRhs;

			return pTinstructLhs->m_pStnodStruct == pTinstructRhs->m_pStnodStruct;
		}
	case TINK_Procedure:
		{
			auto pTinprocLhs = (STypeInfoProcedure *)pTinLhs;
			auto pTinprocRhs = (STypeInfoProcedure *)pTinRhs;
			if (pTinprocLhs->m_arypTinParams.C() != pTinprocRhs->m_arypTinParams.C() ||
				pTinprocLhs->m_arypTinReturns.C() != pTinprocRhs->m_arypTinReturns.C() ||
				pTinprocLhs->m_fHasVarArgs != pTinprocRhs->m_fHasVarArgs)
				return false;

			STypeInfo ** ppTinLhs = pTinprocLhs->m_arypTinParams.A();
			STypeInfo ** ppTinRhs = pTinprocRhs->m_arypTinParams.A();
			for (STypeInfo ** ppTinLhsMax = pTinprocLhs->m_arypTinParams.PMac() ; ppTinLhs != ppTinLhsMax; ++ppTinLhs, ++ppTinRhs)
			{
				if (!FTypesAreSame(*ppTinLhs, *ppTinRhs))
					return false;
			}

			ppTinLhs = pTinprocLhs->m_arypTinReturns.A();
			ppTinRhs = pTinprocRhs->m_arypTinReturns.A();
			for (STypeInfo ** ppTinLhsMax = pTinprocLhs->m_arypTinReturns.PMac() ; ppTinLhs != ppTinLhsMax; ++ppTinLhs, ++ppTinRhs)
			{
				if (!FTypesAreSame(*ppTinLhs, *ppTinRhs))
					return false;
			}
			return true;
		}
	default :			return false;
	}
}

inline STypeInfo * PTinElement(STypeInfo * pTin)
{
	switch(pTin->m_tink)
	{
		case TINK_Pointer:	return ((STypeInfoPointer *)pTin)->m_pTinPointedTo;
		case TINK_Array:	return ((STypeInfoArray *)pTin)->m_pTin;
		default :			return nullptr;
	}
}

STypeInfoPointer * PTinptrAlloc(CSymbolTable * pSymtab, STypeInfo * pTinPointedTo)
{
	auto pTinptr = EWC_NEW(pSymtab->m_pAlloc, STypeInfoPointer) STypeInfoPointer();
	pSymtab->AddManagedTin(pTinptr);
	pTinptr->m_pTinPointedTo = pTinPointedTo;
	return pTinptr;
}

bool FDoesOperatorReturnBool(PARK park)
{
	// return if operator returns a bool (rather than the operand type)
	return  (park == PARK_RelationalOp) | (park == PARK_EqualityOp) | (park == PARK_LogicalAndOrOp);
}

inline STypeInfo * PTinResult(PARK park, CSymbolTable * pSymtab, STypeInfo * pTinOp)
{
	if (FDoesOperatorReturnBool(park))
		return pSymtab->PTinBuiltin("bool");
	return pTinOp;
}

SOpTypes OptypeFromPark(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	JTOK jtok,
	PARK parkOperator,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs)
{
	if (parkOperator == PARK_LogicalAndOrOp)
	{
		auto pTinBool = pSymtab->PTinBuiltin("bool");
		return SOpTypes(pTinBool, pTinBool, pTinBool);
	}

	bool fLhsIsReference = (pTinLhs->m_tink == TINK_Pointer) | (pTinLhs->m_tink == TINK_Array);
	bool fRhsIsReference = (pTinRhs->m_tink == TINK_Pointer) | (pTinRhs->m_tink == TINK_Array);

	// BB - Could this be cleaner with a table?
	if (fLhsIsReference | fRhsIsReference)
	{
		STypeInfo * pTinMin = pTinLhs;
		STypeInfo * pTinMax = pTinRhs;
		if (pTinMin->m_tink > pTinMax->m_tink)
		{
			ewcSwap(pTinMin, pTinMax);
		}
		TINK tinkMin = pTinMin->m_tink;
		TINK tinkMax = pTinMax->m_tink;

		if (fLhsIsReference & fRhsIsReference)
		{
			STypeInfo * pTinRefMax;
			if (tinkMax == TINK_Array)
			{
				if (tinkMin != TINK_Pointer) // no operand for array & array
					return SOpTypes();

				pTinRefMax = ((STypeInfoArray *)pTinMax)->m_pTin;
			}
			else if (EWC_FVERIFY(tinkMax == TINK_Pointer, "unexpected reference type info"))
			{
				pTinRefMax = ((STypeInfoPointer *)pTinMax)->m_pTinPointedTo;
			}

			auto pTinRefMin = ((STypeInfoPointer*)pTinMin)->m_pTinPointedTo;
			bool fAreRefTypesSame = FTypesAreSame(pTinRefMin, pTinRefMax);

			if (parkOperator == PARK_AssignmentOp)
			{
				if (jtok == JTOK('='))
				{
					if (pTinLhs->m_tink == TINK_Array)
						return SOpTypes();

					if (!fAreRefTypesSame && 
						(pTinLhs->m_tink != TINK_Pointer || ((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo->m_tink != TINK_Void))
						return SOpTypes();

					return SOpTypes(pTinLhs, pTinRhs, pSymtab->PTinBuiltin("bool"));
				}
			}

			bool fIsOneTypeVoid = (pTinRefMin->m_tink == TINK_Void) | (pTinRefMax->m_tink == TINK_Void);
			if (parkOperator == PARK_EqualityOp && (fAreRefTypesSame | fIsOneTypeVoid))
			{
				return SOpTypes(pTinLhs, pTinRhs, pSymtab->PTinBuiltin("bool"));
			}

			if (parkOperator == PARK_AdditiveOp)
			{
				if (jtok == JTOK('-') && fAreRefTypesSame)
				{
					return SOpTypes(pTinLhs, pTinRhs, pSymtab->PTinBuiltin("sSize"));
				}
			}

			return SOpTypes();
		}

		STypeInfo * pTinPtr = pTinLhs;
		STypeInfo * pTinOther = pTinRhs;
		if (pTinOther->m_tink == TINK_Pointer)
		{
			pTinPtr = pTinRhs;
			pTinOther = pTinLhs;
		}

		if (((STypeInfoPointer *)pTinPtr)->m_pTinPointedTo->m_tink == TINK_Void)
		{
			return SOpTypes();
		}
		
		PARK parkOperatorAdj = parkOperator;
		if (parkOperator == PARK_AssignmentOp && ((jtok == JTOK_PlusEqual) | (jtok == JTOK_MinusEqual)))
		{
			parkOperatorAdj = PARK_AdditiveOp;
		}

		switch (parkOperatorAdj)
		{
		case PARK_AdditiveOp:
			{
				if (pTinOther->m_tink == TINK_Integer)
				{
					return SOpTypes(pTinLhs, pTinRhs, pTinPtr);
				}
				else
				{
					EWC_ASSERT(false, "unexpected Additive operator");
				}
			} break;
		}
	}

	if (pTinLhs->m_tink == pTinRhs->m_tink)
	{
		switch(pTinLhs->m_tink)
		{
		case TINK_Float:
			{
				STypeInfoFloat * pTinfloatA = (STypeInfoFloat *)pTinLhs;
				STypeInfoFloat * pTinfloatB = (STypeInfoFloat *)pTinRhs;

				auto pTinOp = (pTinfloatA->m_cBit >= pTinfloatB->m_cBit) ? pTinLhs : pTinRhs;
				return SOpTypes(pTinOp, pTinOp, PTinResult(parkOperator, pSymtab, pTinOp));
			}
		case TINK_Integer:
			{
				STypeInfoInteger * pTinintA = (STypeInfoInteger *)pTinLhs;
				STypeInfoInteger * pTinintB = (STypeInfoInteger *)pTinRhs;

				if (pTinintA->m_fIsSigned != pTinintB->m_fIsSigned)
					return SOpTypes();
			
				auto pTinOp = (pTinintA->m_cBit >= pTinintB->m_cBit) ? pTinLhs : pTinRhs;
				return SOpTypes(pTinOp, pTinOp, PTinResult(parkOperator, pSymtab, pTinOp));
			}
		case TINK_Array:
			return SOpTypes();
		}

		if (FTypesAreSame(pTinLhs, pTinRhs))
		{
			return SOpTypes(pTinLhs, pTinLhs, PTinResult(parkOperator, pSymtab, pTinLhs));
		}
	}

	if (pTinLhs->m_tink == TINK_Enum || pTinRhs->m_tink == TINK_Enum)
	{
		STypeInfo * pTinEnum = pTinLhs;
		STypeInfo * pTinOther = pTinRhs;
		if (pTinOther->m_tink == TINK_Enum)
		{
			pTinEnum = pTinRhs;
			pTinOther = pTinLhs;
		}

		if (parkOperator == PARK_AdditiveOp && pTinOther->m_tink == TINK_Integer)
		{
			return SOpTypes(pTinEnum, pTinEnum, pTinEnum);
		}
	}

	if (pTinLhs->m_tink == TINK_Bool || pTinRhs->m_tink == TINK_Integer)
	{
		if (parkOperator == PARK_AssignmentOp)
		{
			return SOpTypes(pTinLhs, pTinLhs, pTinLhs);
		}
	}
	return SOpTypes();
}

inline bool FIsNumericTink(TINK tink)
{
	switch (tink)
	{
	case TINK_Integer:	return true;
	case TINK_Bool:		return true;
	case TINK_Enum:		return true;
	case TINK_Float:	return true;
	default: return false;
	}
}

inline bool FCanImplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
	if (!pTinSrc)
		return false;	 // NOTE: this can happen after an error has occurred, don't assert - just return.

	EWC_ASSERT(pTinSrc->m_tink != TINK_Literal, "literals should be promoted before calling FCanImplicitCast()");

	if (pTinSrc->m_tink == pTinDst->m_tink)
	{
		// Note - can't just compare pointers directly as tins are not unique. (but they should be)
		switch (pTinSrc->m_tink)
		{
		case TINK_Integer:
			{
				auto pTinintSrc = (STypeInfoInteger *)pTinSrc;
				auto pTinintDst = (STypeInfoInteger *)pTinDst;

				if ((pTinintDst->m_cBit >= pTinintSrc->m_cBit) & (pTinintDst->m_fIsSigned == pTinintSrc->m_fIsSigned))
					return true;

				// Allow unsigned->signed conversions if a higher cBit
				return ((pTinintDst->m_fIsSigned == true) & (pTinintDst->m_cBit > pTinintSrc->m_cBit));
			}
		case TINK_Float:
			{
				auto pTinfloatSrc = (STypeInfoFloat *)pTinSrc;
				auto pTinfloatDst = (STypeInfoFloat *)pTinDst;
				return pTinfloatDst->m_cBit >= pTinfloatSrc->m_cBit;
			}
		case TINK_Bool: return true;
		case TINK_String: return true;
		case TINK_Pointer:
			{
				auto pTinptrSrc = (STypeInfoPointer *)pTinSrc;
				auto pTinptrDst = (STypeInfoPointer *)pTinDst;
				if (pTinptrDst->m_pTinPointedTo->m_tink == TINK_Void)
					return true;

				return FTypesAreSame(pTinptrSrc->m_pTinPointedTo, pTinptrDst->m_pTinPointedTo);	
			}
		case TINK_Array:
			{
				auto pTinaryDst = (STypeInfoArray *)pTinDst;
				if (pTinaryDst->m_aryk == ARYK_Reference)
				{
					auto pTinarySrc = (STypeInfoArray *)pTinSrc;
					return FTypesAreSame(pTinarySrc->m_pTin, pTinaryDst->m_pTin);
				}
				return FTypesAreSame(pTinSrc, pTinDst);
			} 
		case TINK_Enum: 
		case TINK_Struct:
		case TINK_Procedure:
			{
				return FTypesAreSame(pTinSrc, pTinDst);
			}
		default: return false;
		}
	}

	if ((pTinSrc->m_tink == TINK_Array) & (pTinDst->m_tink == TINK_Pointer))
	{
		auto pTinarySrc = (STypeInfoArray *)pTinSrc;
		auto pTinptrDst = (STypeInfoPointer *)pTinDst;
		if (pTinptrDst->m_pTinPointedTo->m_tink == TINK_Void)
			return true;
		return FTypesAreSame(pTinarySrc->m_pTin, pTinptrDst->m_pTinPointedTo);	
	}

	if (pTinSrc->m_tink == TINK_Enum && ((pTinDst->m_tink == TINK_Integer) | (pTinDst->m_tink == TINK_Float)))
	{
		auto pTinenum = (STypeInfoEnum *)pTinSrc;
		return FCanImplicitCast(pTinenum->m_pTinLoose, pTinDst);
	}

	if (pTinSrc->m_tink == TINK_Bool && pTinDst->m_tink == TINK_Integer)
	{
		return true;
	}

	if (pTinDst->m_tink == TINK_Bool)
	{
		switch (pTinSrc->m_tink)
		{
		case TINK_Integer:	return true;
		case TINK_Float:	return true;
		case TINK_Pointer:	return true;
		case TINK_Bool:	return true;
		default : return false;
		}
	}
	return false;
}

inline bool FCanExplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
	if (FIsNumericTink(pTinSrc->m_tink))
	{
		return FIsNumericTink(pTinDst->m_tink);
	}

	if (pTinSrc->m_tink == TINK_Pointer && pTinDst->m_tink == TINK_Pointer)
		return true;
	if (pTinSrc->m_tink == TINK_Procedure && pTinDst->m_tink == TINK_Procedure)
		return true;

	return FCanImplicitCast(pTinSrc, pTinDst);
}

bool FIsValidLhs(const CSTNode * pStnod)
{
	// BB - this is just returning the easy failures... needs a more thorough check.
	STypeInfo * pTin = pStnod->m_pTin;
	if (!pTin)
		return false;

	TINK tink = pTin->m_tink;
	if (tink == TINK_Array)
	{
		auto pTinary = (STypeInfoArray *)pTin;
		return pTinary->m_aryk == ARYK_Reference;
	}

	return (tink != TINK_Null) & (tink != TINK_Void) & (tink != TINK_Literal);
}


STypeInfo * PTinFromRange(
	STypeCheckWorkspace *pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnod,
	SBigInt bintMin,
	SBigInt bintMax)
{
	SBigInt bint = (bintMin.m_nAbs >= bintMax.m_nAbs) ? bintMin : bintMax;

	if (bintMin.m_fIsNegative)
	{
		if (bintMax.m_nAbs > LLONG_MAX)
		{
			EmitError(pTcwork, pStnod, "Range too large to wit within 64 bit type [%s%lld .. %s%lld]",
				(bintMin.m_fIsNegative) ? "-" : "", bintMin.m_nAbs,
				(bintMax.m_fIsNegative) ? "-" : "", bintMax.m_nAbs);
		}

		// pass a negative int to PTinFromBint if we need signed values
		bint.m_fIsNegative = true;
	}

	auto pTin = PTinFromBint(pTcwork, pSymtab, bint);
	return pTin;
}

bool FIsType(CSTNode * pStnod)
{
	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		return false;

	auto pSym = pStnod->m_pSym;
	if (!pSym)
	{
		EWC_ASSERT(pStnod->m_park != PARK_Identifier, "Expected identifiers to have symbol");
		return false;
	}

	return pSym->m_grfsym.FIsSet(FSYM_IsType);
}

STypeInfo * PTinFromTypeSpecification(
	STypeCheckWorkspace *pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnod,
	GRFSYMLOOK grfsymlook,
	SSymbol **ppSymType,
	bool * pFIsValidTypeSpec)
{
	*pFIsValidTypeSpec = true;

	// returns null if this is an instance of an unknown type, if this is a reference to an unknown type we will return
	//  tinptr->tin(TINK_Unknown) because we need this to handle a struct with a pointer to an instance of itself.

	// Essentially, any non-null returned from this should be enough to determine the size of this type spec and 
	//  determine target type equivalence.

	CAlloc * pAlloc = pSymtab->m_pAlloc;
	bool fAllowForwardDecl = false;

	// loop and find the concrete target type
	STypeInfo * pTinFinal = nullptr;
	auto pStnodIt = pStnod;

	EWC_ASSERT(pStnodIt->m_strees == STREES_TypeChecked, "Type specification should be type checked first, (for literal op eval)");

	while (pStnodIt)
	{
		switch(pStnodIt->m_park)
		{
		case PARK_MemberLookup:
			{
				EWC_ASSERT(pStnodIt->CStnodChild() == 2, "Expected Lhs.Rhs in PARK_MemberLookup");
				CSTNode * pStnodIdent = pStnodIt->PStnodChild(0);
				auto strIdent = StrFromIdentifier(pStnodIdent);
				auto pSym = pSymtab->PSymLookup(strIdent, pStnodIdent->m_lexloc, grfsymlook);
				EWC_ASSERT(pSym && pSym->m_pStnodDefinition, "bad outer type in type specification");

				CSTNode * pStnodDefinition = pSym->m_pStnodDefinition;
				if (EWC_FVERIFY(pStnodDefinition->m_pSymtab, "Struct without symbol table"))
				{
					pSymtab = pStnodDefinition->m_pSymtab;
				}

				pStnodIt = pStnodIt->PStnodChild(1);
			} break;
		case PARK_Identifier:
			{
				auto strIdent = StrFromIdentifier(pStnodIt);
				auto pSym = pSymtab->PSymLookup(strIdent, pStnodIt->m_lexloc, grfsymlook);

				//if (!FIsType(pStnodIt))
				if (!pSym->m_grfsym.FIsSet(FSYM_IsType))
				{
					EmitError(pTcwork, pStnodIt, "Expected type specification but encounted '%s'", strIdent.PCoz());
					*pFIsValidTypeSpec = false;
				}

				EWC_ASSERT(pSym && pSym->m_pTin, "bad type identifier in type specification");
				
				if (ppSymType)
				{
					*ppSymType = pSym;
				}

				pTinFinal = pSym->m_pTin;
				pStnodIt->m_pTin = pTinFinal;
				pStnodIt = nullptr;
			} break;
		case PARK_ArrayDecl:
			{
				// array decl's children are [type] or [m_c, type]
				pStnodIt = pStnodIt->PStnodChild(pStnodIt->CStnodChild()-1);
				EWC_ASSERT(pStnodIt, "bad array declaration");
			} break;
		case PARK_ReferenceDecl:
			{
				fAllowForwardDecl |= true;
				EWC_ASSERT(pStnodIt->CStnodChild() == 1);
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		case PARK_ProcedureReferenceDecl:
			{
				pTinFinal = pStnodIt->m_pTin;
				EWC_ASSERT(pTinFinal, "expected pTinproc before PTinFromTypeSpecification");
				pStnodIt = nullptr;
			} break;
		default: EWC_ASSERT(false, "unexpected parse node %s in PTinFromTypeSpecification", PChzFromPark(pStnod->m_park));
			break;
		}
	}

	if (!pTinFinal)
		return nullptr;

	STypeInfo * pTinReturn = nullptr;
	STypeInfo ** ppTinCur = &pTinReturn;

	// build the fully qualified type info
	pStnodIt = pStnod;
	STypeInfo * pTinPrev = nullptr;
	while (pStnodIt)
	{
		switch (pStnodIt->m_park)
		{
		case PARK_ReferenceDecl:
			{
				auto pTinptr = PTinptrAlloc(pSymtab, nullptr);
				pStnodIt->m_pTin = pTinptr;

				pTinPrev = pTinptr;
				*ppTinCur = pTinptr;
				ppTinCur = &pTinptr->m_pTinPointedTo;

				EWC_ASSERT(pStnodIt->CStnodChild() == 1);
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		case PARK_ArrayDecl:
			{
				STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
				pSymtab->AddManagedTin(pTinary);

				*ppTinCur = pTinary;
				ppTinCur = &pTinary->m_pTin;

				if (pStnodIt->CStnodChild() == 2)
				{
					CSTNode * pStnodDim = pStnodIt->PStnodChild(0);
					STypeInfoLiteral * pTinlitDim = (STypeInfoLiteral *)pStnodDim->m_pTin;
					CSTValue * pStvalDim = nullptr;
					if (!pTinlitDim || pTinlitDim->m_tink != TINK_Literal)
					{
						EmitError(pTcwork, pStnodIt, "Only static sized arrays are currently supported");
						*pFIsValidTypeSpec = false;
					}
					else
					{
						STypeInfo * pTinCount = pSymtab->PTinBuiltin("int");
						STypeInfo * pTinPromoted = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodDim, pTinCount);
						if (!FCanImplicitCast(pTinPromoted, pTinCount))
						{
							EmitError(pTcwork, pStnodIt, "static integer array size expected");
							*pFIsValidTypeSpec = false;
						}
						else
						{
							FinalizeLiteralType(pSymtab, pTinCount, pStnodDim);
							pStvalDim = pStnodDim->m_pStval;
						}
					}

					if (!pStvalDim)
						return nullptr;

					pTinary->m_c = NUnsignedLiteralCast(pTcwork, pStnodIt, pStvalDim);
					pTinary->m_aryk = ARYK_Fixed;
				}
				else
				{
					pTinary->m_aryk = (pStnodIt->m_jtok == JTOK_PeriodPeriod) ? ARYK_Dynamic : ARYK_Reference;
				}

				if (pTinary->m_aryk == ARYK_Dynamic)
				{
					EmitError(pTcwork, pStnod, "Dynamic arrays are not yet supported");
				}

				pStnodIt->m_pTin = pTinary;
				pStnodIt = pStnodIt->PStnodChild(pStnodIt->CStnodChild()-1);
			} break;
		case PARK_MemberLookup:
			{
				// don't need to update pSymtab, already have pTinFinal
				EWC_ASSERT(pStnodIt->CStnodChild() == 2, "Expected Lhs.Rhs in PARK_MemberLookup");
				pStnodIt = pStnodIt->PStnodChild(1);
			} break;
		case PARK_ProcedureReferenceDecl:
			{
				*ppTinCur = pTinFinal;
				ppTinCur = nullptr;
				pStnodIt = nullptr;
			} break;
		case PARK_Identifier:
			{
				if (pTinFinal->m_tink == TINK_ForwardDecl &&
					EWC_FVERIFY(pTinPrev != nullptr, "how did we get here without a prev type info?"))
				{
					STypeInfoForwardDecl * pTinfwd = (STypeInfoForwardDecl *)pTinFinal;
					pTinfwd->m_arypTinReferences.Append(pTinPrev);
				}

				*ppTinCur = pTinFinal;
				ppTinCur = nullptr;
				pStnodIt = nullptr;
			} break;
		}
	}
	return pTinReturn;
}

STypeInfo * PTinReturnFromStnodProcedure(CSTNode * pStnod)
{
	if (!EWC_FVERIFY(pStnod->m_park == PARK_ProcedureDefinition && pStnod->m_pStproc, "Bad procedure node"))
		return nullptr;
	CSTProcedure * pStproc = pStnod->m_pStproc;
	if (pStproc->m_iStnodReturnType < 0)
		return nullptr;
	return pStnod->PStnodChild(pStproc->m_iStnodReturnType)->m_pTin;
}

STypeStructMember * PTypemembLookup(STypeInfoStruct * pTinstruct, const CString & strMemberName)
{
	// BB - could just store the members in a contiguous array... simplify this loop
	auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
	for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb)
	{
		if (pTypememb->m_strName == strMemberName)
			return pTypememb;
	}
	return nullptr;
}

bool FVerifyIsInstance(STypeCheckWorkspace * pTcwork, CSTNode * pStnod)
{
	if (FIsType(pStnod))
	{
		CString strLhs = StrFromTypeInfo(pStnod->m_pTin);
		EmitError(pTcwork, pStnod, "Invalid use of type '%s' as an instance type", strLhs.PCoz());
		return false;
	}
	return true;
}

void SetEnumConstantValue(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, const SBigInt & bint)
{
	auto pStval = EWC_NEW(pTcwork->m_pAlloc, CSTValue) CSTValue();
	if (bint.m_fIsNegative)
	{
		pStval->m_stvalk = STVALK_SignedInt;
		pStval->m_nSigned = bint.S64Coerce();
	}
	else
	{
		pStval->m_stvalk = STVALK_UnsignedInt;
		pStval->m_nUnsigned = bint.U64Coerce();
	}
	pStnod->m_pStval = pStval;
}

void AddEnumNameValuePair(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodNames,
	CSTNode * pStnodValues,
	CSTNode * pStnodConstant,
	STypeInfo * pTinValue, 
	STypeInfo * pTinName)
{
	CAlloc * pAlloc = pTcwork->m_pAlloc;
	CSTNode * pStnodValue = EWC_NEW(pAlloc, CSTNode) CSTNode(pAlloc, pStnodValues->m_lexloc);
	pStnodValue->m_park = PARK_Literal;
	pStnodValue->m_pTin = pTinValue; // finalized literal version of enum.loose type
	pStnodValue->m_pStval = PStvalCopy(pAlloc, pStnodConstant->m_pStval);

	pStnodValues->IAppendChild(pStnodValue);

	CSTNode * pStnodName = EWC_NEW(pAlloc, CSTNode) CSTNode(pAlloc, pStnodNames->m_lexloc);
	pStnodName->m_park = PARK_Literal;
	pStnodName->m_pTin = pTinName;
	auto pStvalName = EWC_NEW(pAlloc, CSTValue) CSTValue();
	pStvalName->m_stvalk = STVALK_String;

	CSTDecl * pStdecl = pStnodConstant->m_pStdecl;
	CSTNode * pStnodIdent = (pStdecl) ? pStnodConstant->PStnodChildSafe(pStdecl->m_iStnodIdentifier) : nullptr;
	if (EWC_FVERIFY(pStnodIdent && pStnodIdent->m_pStident, "Enum constant missing name"))
	{
		pStvalName->m_str = pStnodIdent->m_pStident->m_str;
	}

	pStnodName->m_pStval = pStvalName;
	pStnodNames->IAppendChild(pStnodName);
}

void ResolveSpoofTypedef(
	STypeCheckWorkspace * pTcwork, 
	CSymbolTable * pSymtab,
	CSTNode * pStnod,
	const CString & strIdent,
	STypeInfo * pTin,
	GRFSYMLOOK grfsymlook)
{
	auto pSym = pSymtab->PSymLookup( strIdent, pStnod->m_lexloc, grfsymlook);

	if (!EWC_FVERIFY(pSym && pSym->m_pStnodDefinition == pStnod, "symbol lookup failed for '%s'", strIdent.PCoz()))
		return;
	
	EWC_ASSERT(pSym->m_pTin == nullptr, "spoof typedef already resolved");
	pSym->m_pTin = pTin;

	OnTypeComplete(pTcwork, pSym);
}

void SpoofLiteralArray(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, CSTNode * pStnodArray, int cElements, STypeInfo * pTinElement)
{
	if (!EWC_FVERIFY(pStnodArray->m_pStdecl && pStnodArray->m_pSym, "bad spoofed literal array"))
		return;

	STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
	pSymtab->AddManagedTin(pTinlit);
	pTinlit->m_c = cElements;
	pTinlit->m_litty.m_litk = LITK_Array;
	pTinlit->m_pTinSource = pTinElement;
	pTinlit->m_pStnodDefinition = pStnodArray;
	pStnodArray->m_pTin = pTinlit;
	pStnodArray->m_pSym->m_pTin = pTinlit;

	CSTNode * pStnodList = EWC_NEW(pSymtab->m_pAlloc, CSTNode) CSTNode(pSymtab->m_pAlloc, pStnodArray->m_lexloc);
	pStnodList->m_park = PARK_List;
	pStnodList->m_pTin = pTinlit;

	EWC_ASSERT(pStnodArray->m_pStdecl->m_iStnodInit == -1, "expected empty array");
	pStnodArray->m_pStdecl->m_iStnodInit = pStnodArray->IAppendChild(pStnodList);
}

TCRET TcretWaitForTypeSymbol(STypeCheckWorkspace * pTcwork, STypeCheckFrame * pTcfram, SSymbol * pSymType, CSTNode * pStnodType)
{
	if (!pSymType)
	{
		CString strTypename = StrTypenameFromTypeSpecification(pStnodType);
		EmitError(pTcwork, pStnodType, "'%s' unknown symbol detected", strTypename.PCoz());
		return TCRET_StoppingError;
	}

	if (!pSymType->m_grfsym.FIsSet(FSYM_IsType))
	{
		CString strName = StrFullyQualifiedSymbol(pSymType);
		EmitError(pTcwork, pStnodType, "%s symbol refers to instance, but was expecting type", strName.PCoz());
		return TCRET_StoppingError;
	}
	else
	{
		// wait for this type to be resolved.
		SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymType);
		pUntype->m_aryiTcframDependent.Append((s32)pTcwork->m_aryTcfram.IFromP(pTcfram));
		return TCRET_WaitingForSymbolDefinition;
	}
}

// wrapper struct to allow breaking on returning different TCRET values
struct TcretDebug
{
			TcretDebug(TCRET tcret)
			:m_tcret(tcret)
				{
					if (tcret == TCRET_StoppingError)
						DoNothing();
					if (tcret == TCRET_WaitingForSymbolDefinition)
						DoNothing();
				}

			operator TCRET()
				{ return m_tcret; }

	TCRET	m_tcret;
};


enum PROCMATCH
{
	PROCMATCH_None,
	PROCMATCH_Exact,
	PROCMATCH_ImplicitCast,
	PROCMATCH_Max
};

PROCMATCH ProcmatchCheckArguments(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	STypeInfoProcedure * pTinproc,
	CSTNode * pStnodCall,
	bool fPrintErrors)
{
	int cStnodCall = pStnodCall->CStnodChild(); 
	int iStnodArgMin = 1; // skip the identifier

	size_t cArgs = size_t(cStnodCall - iStnodArgMin);
	if (cArgs < pTinproc->m_arypTinParams.C())
	{
		if (fPrintErrors)
		{
			EmitError(pTcwork, pStnodCall, "Too few arguments to procedure '%s'. Expected %d but encountered %d",
				(pTinproc->m_strName.FIsEmpty()) ? "unnamed" : pTinproc->m_strName.PCoz(),
				pTinproc->m_arypTinParams.C(),
				cStnodCall - iStnodArgMin);
		}
		return PROCMATCH_None;
	}

	if 	(pTinproc->m_fHasVarArgs == false && cArgs > pTinproc->m_arypTinParams.C())
	{
		if (fPrintErrors)
		{
			EmitError(pTcwork, pStnodCall, "Too many arguments to procedure '%s'. Expected %d but encountered %d",
				(pTinproc->m_strName.FIsEmpty()) ? "unnamed" : pTinproc->m_strName.PCoz(),
				pTinproc->m_arypTinParams.C(),
				cStnodCall - iStnodArgMin);
		}
		return PROCMATCH_None;
	}

	PROCMATCH procmatch = PROCMATCH_Exact;
	for (int iStnodArg = iStnodArgMin; iStnodArg < cStnodCall; ++iStnodArg)
	{
		CSTNode * pStnodArg = pStnodCall->PStnodChild(iStnodArg);

		if (FIsType(pStnodArg))
		{
			if (fPrintErrors)
			{
				EmitError(pTcwork, pStnodCall, "Procedure argument %d, Expected an instance, but encountered a type.",
						iStnodArg-iStnodArgMin+1);
			}
			return PROCMATCH_None;
		}

		STypeInfo * pTinCall = pStnodArg->m_pTin;
		
		// Find the default literal promotion, as we need this to check for exact matches (which have precedence for matching)
		//  Things that can't default (void *) are problematic.
		STypeInfo * pTinParam = nullptr;
		int ipTinParam = iStnodArg - iStnodArgMin;
		bool fIsVarArg = ipTinParam >= (int)pTinproc->m_arypTinParams.C();

		if (!fIsVarArg)
		{
			pTinParam = pTinproc->m_arypTinParams[ipTinParam];
			if (!EWC_FVERIFY(pTinParam, "unknown parameter type"))
				return PROCMATCH_None;
			pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodArg, pTinParam);
		}

		STypeInfo * pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pStnodArg, pTinParam);
		if (fIsVarArg)
		{
			if (!pTinproc->m_fHasVarArgs)
			{
				if (fPrintErrors)
				{
					EmitError(pTcwork, pStnodCall, "procedure '%s' expected %d arguments but encountered %d",
						(pTinproc->m_strName.FIsEmpty()) ? "unnamed" : pTinproc->m_strName.PCoz(),
						pTinproc->m_arypTinParams.C(),
						cStnodCall - iStnodArgMin);
				}
				return PROCMATCH_None;
			}

			pTinCall = pTinCallDefault;
			pTinParam = PTinPromoteVarArg(pTcwork, pSymtab, pTinCall);
		}

		if (FTypesAreSame(pTinCallDefault, pTinParam))
			continue;

		if (FCanImplicitCast(pTinCall, pTinParam))
		{
			procmatch = PROCMATCH_ImplicitCast;
		}
		else
		{
			if (fPrintErrors)
			{
				CString strTinCall = StrFromTypeInfo(pTinCall);
				CString strTinParam = StrFromTypeInfo(pTinParam);
				EmitError(pTcwork, pStnodCall, "procedure call '%s' cannot convert argument %d from type %s to %s",
					(pTinproc->m_strName.FIsEmpty()) ? "unnamed" : pTinproc->m_strName.PCoz(),
					iStnodArg-iStnodArgMin+1,
					strTinCall.PCoz(),
					strTinParam.PCoz());
			}
			procmatch = PROCMATCH_None;
		}

	}

	return procmatch;
}

bool FIsDirectCall(CSTNode * pStnodCall)
{
	if (!EWC_FVERIFY(pStnodCall->m_park == PARK_ProcedureCall && pStnodCall->CStnodChild() >= 1,
				"Bad node passed int FIsDirectCall"))
		return false;

	auto pStnod = pStnodCall->PStnodChild(0);
	auto pSym = pStnod->m_pSym;
	if (pSym && pSym->m_pStnodDefinition && pSym->m_pStnodDefinition->m_park == PARK_ProcedureDefinition)
	{
		return true;
	}
	return false;
}

TcretDebug TcretTypeCheckSubtree(STypeCheckWorkspace * pTcwork, STypeCheckFrame * pTcfram)
{
	CDynAry<STypeCheckStackEntry> * paryTcsent = &pTcfram->m_aryTcsent;
	while (paryTcsent->C())
	{
		STypeCheckStackEntry * pTcsentTop = paryTcsent->PLast();
		CSTNode * pStnod = pTcsentTop->m_pStnod;

		switch (pStnod->m_park)
		{
			case PARK_ProcedureDefinition:
			{
				CSTProcedure * pStproc = pStnod->m_pStproc;
				if (!EWC_FVERIFY(pStproc, "missing procedure parse data"))
					return TCRET_StoppingError;

				STypeInfoProcedure * pTinproc = (STypeInfoProcedure *)pStnod->m_pTin;
				if (!EWC_FVERIFY(pTinproc && pTinproc->m_tink == TINK_Procedure, "missing procedure type info"))
					return TCRET_StoppingError;

				switch(pTcsentTop->m_nState++)
				{
				case 0:
					{	// push the parameter list
						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParamList = pStnod->PStnodChild(pStproc->m_iStnodParameterList);

							PushTcsent(pTcfram, &pTcsentTop, pStnodParamList);
							STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
							pTcsentPushed->m_pSymtab = pStnodParamList->m_pSymtab;
							EWC_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");
						}
					}break;
				case 1:
					{	// push the return type
						if (pStproc->m_iStnodReturnType >= 0)
						{
							CSTNode * pStnodReturn = pStnod->PStnodChild(pStproc->m_iStnodReturnType);

							PushTcsent(pTcfram, &pTcsentTop, pStnodReturn);
							STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
							pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;

						}
					}break;
				case 2:
					{
						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParamList = pStnod->PStnodChild(pStproc->m_iStnodParameterList);

							int cParamsExpected = pStnodParamList->CStnodChild() - pTinproc->m_fHasVarArgs;
							EWC_ASSERT(pTinproc->m_arypTinParams.C() == cParamsExpected, "parameter child mismatch");
							for (int iStnodArg = 0; iStnodArg < cParamsExpected; ++iStnodArg)
							{
								pTinproc->m_arypTinParams[iStnodArg] = pStnodParamList->PStnodChild(iStnodArg)->m_pTin;
							}
						}

						// type check the return list
						if (pStnod->m_strees < STREES_SignatureTypeChecked)
						{
							if (pStproc->m_iStnodReturnType >= 0)
							{
								CSTNode * pStnodReturn = pStnod->PStnodChild(pStproc->m_iStnodReturnType);
								bool fIsValidTypeSpec;
								STypeInfo * pTinReturn = PTinFromTypeSpecification(
															pTcwork,
															pTcsentTop->m_pSymtab,
															pStnodReturn,
															pTcsentTop->m_grfsymlook,
															nullptr, 
															&fIsValidTypeSpec);
								if (!fIsValidTypeSpec)
									return TCRET_StoppingError;

								if (!pTinReturn)
								{
									EmitError(pTcwork, pStnod, "failed to parse return type");
								}
								pStnodReturn->m_pTin = pTinReturn;

								pTinproc->m_arypTinReturns[0] = pTinReturn;
							}
							pStnod->m_strees = STREES_SignatureTypeChecked;

							// find our symbol and resolve any pending unknown types

							CSTNode * pStnodIdent = nullptr;
							if (pStproc->m_iStnodProcName >= 0)
							{
								CString strProcName = StrFromIdentifier(pStnod->PStnodChild(pStproc->m_iStnodProcName));
								pStnodIdent = pStnod->PStnodChildSafe(pStproc->m_iStnodProcName);
							}
						}

						// push the body subtree
						if (pStproc->m_iStnodBody >= 0)
						{
							CSTNode * pStnodBody = pStnod->PStnodChild(pStproc->m_iStnodBody);
							PushTcsent(pTcfram, &pTcsentTop, pStnodBody);

							STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
							pTcsentPushed->m_pStnodProcedure = pStnod;
							pTcsentPushed->m_pSymtab = pStnodBody->m_pSymtab;
						}
					}break;
				case 3:
					{
						SSymbol * pSymProc = pStnod->m_pSym;
						CString strProcName;

						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParams = pStnod->PStnodChild(pStproc->m_iStnodParameterList);
							EWC_ASSERT(pStnodParams->m_park == PARK_ParameterList, "expected parametere list");

							for (int iStnod = 0; iStnod < pStnodParams->CStnodChild(); ++iStnod)
							{
								auto pStnodParam = pStnodParams->PStnodChild(iStnod);
								if (pStnodParam->m_park == PARK_VariadicArg)
									continue;

								if (!FIsValidLhs(pStnodParam))
								{
									auto strType = StrFromTypeInfo(pStnodParam->m_pTin);
									EmitError(
										pTcwork, pStnod,
										"Argument %d is not a valid argument type. '%s' does not define an assignment operator", 
										iStnod + 1,
										strType.PCoz());
								}
							}

							if (pTcwork->m_pErrman->m_cError)
								return TCRET_StoppingError;
						}

						if (!EWC_FVERIFY(pSymProc, "failed to find procedure name symbol: %s", strProcName.PCoz()))
							return TCRET_StoppingError;
						if (!EWC_FVERIFY(pSymProc->m_pTin, "expected procedure type info to be created during parse"))
							return TCRET_StoppingError;

						pStnod->m_pSym = pSymProc;
						EWC_ASSERT(pStnod->m_pSym, "null symbol");

						// compute name mangling
						auto pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pStnod->m_pSym->m_pTin);

						if (pStproc->m_fUseUnmangledName)
						{
							pTinproc->m_strMangled = pTinproc->m_strName;
						}
						else
						{
							pTinproc->m_strMangled = pTcwork->m_mang.StrMangleMethodName(pTinproc);
#if VALIDATE_NAME_MANGLING
							char aCh[1024];
							EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
							PrintTypeInfo(&strbuf, pTinproc, PARK_Nil, FDBGSTR_UseSizedNumerics);

							STypeInfoProcedure * pTinprocDemangled = pTcwork->m_mang.PTinprocDemangle(pTinproc->m_strMangled, pTcsentTop->m_pSymtab);

							if (!pTinprocDemangled)
							{
								pTinprocDemangled = pTcwork->m_mang.PTinprocDemangle(pTinproc->m_strMangled, pTcsentTop->m_pSymtab);
							}

							if (EWC_FVERIFY(pTinprocDemangled, "Name demangling failed - null procedure type"))
							{
								char aChAfter[1024];
								EWC::SStringBuffer strbufAfter(aChAfter, EWC_DIM(aChAfter));
								PrintTypeInfo(&strbufAfter, pTinprocDemangled, PARK_Nil, FDBGSTR_UseSizedNumerics);
								EWC_ASSERT(FAreCozEqual(aCh, aChAfter), "Unmangled type info doesn't match initial info");
							}
#endif
						}

						PopTcsent(pTcfram, &pTcsentTop, pStnod);

						pStnod->m_strees = STREES_TypeChecked;
						OnTypeComplete(pTcwork, pSymProc);
					}break;
				}
			} break;
			case PARK_ProcedureReferenceDecl:
			{
				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
					break;
				}

				CSTProcedure * pStproc = pStnod->m_pStproc;
				auto pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pStnod->m_pTin);
				if (!EWC_FVERIFY(pStproc && pTinproc, "expected procedure"))
					return TCRET_StoppingError;

				CSTNode * pStnodReturn = pStnod->PStnodChildSafe(pStproc->m_iStnodReturnType);

				if (!pStnodReturn)
				{
					EmitError(pTcwork, pStnod, "Procedure reference decl missing return AST node");
					return TCRET_StoppingError;
				}

				int cParamsExpected = 0;
				CSTNode * pStnodParameterList = pStnod->PStnodChildSafe(pStproc->m_iStnodParameterList);
				if (pStnodParameterList)
				{
					cParamsExpected = pStnodParameterList->CStnodChild() - pTinproc->m_fHasVarArgs;
				}

				EWC_ASSERT(pTinproc->m_arypTinParams.C() == cParamsExpected, "parameter child mismatch");
				for (int iStnodArg = 0; iStnodArg < cParamsExpected; ++iStnodArg)
				{
					pTinproc->m_arypTinParams[iStnodArg] = pStnodParameterList->PStnodChild(iStnodArg)->m_pTin;
				}

				bool fIsValidTypeSpec;
				STypeInfo * pTinReturn = PTinFromTypeSpecification(
											pTcwork,
											pTcsentTop->m_pSymtab,
											pStnodReturn,
											pTcsentTop->m_grfsymlook,
											nullptr, 
											&fIsValidTypeSpec);
				if (!fIsValidTypeSpec)
					return TCRET_StoppingError;

				if (!pTinReturn)
				{
					EmitError(pTcwork, pStnod, "failed to parse return type");
				}
				pStnodReturn->m_pTin = pTinReturn;
				pTinproc->m_arypTinReturns[0] = pTinReturn;

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);

			} break;
			case PARK_ProcedureCall:
			{
				// The first child is either 
				//   - an identifier (who's symbol maps to a function definition or a function pointer)
				//       all identifiers should be resolved by the method overloading code.
				//   - some expression that evaluates to a function pointer

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					int ipStnodChild = pTcsentTop->m_nState++;
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(ipStnodChild));

					STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
					pTcsentPushed->m_fAllowForwardDecl = ipStnodChild == 0;
					break;
				}

				CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
				bool fIsDirectCall = false;
				STypeInfoProcedure * pTinproc = nullptr;
				CSTNode * pStnodCallee = pStnod->PStnodChild(0);
				if (pStnodCallee->m_park == PARK_Identifier)
				{
					CSTNode * pStnodIdent = pStnodCallee;
					CString strProcName = StrFromIdentifier(pStnodIdent);
					CSymbolTable::CSymbolIterator symiter;
					if (!strProcName.FIsEmpty())
					{
						symiter = CSymbolTable::CSymbolIterator(pSymtab, strProcName, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);
					}

					if (symiter.FIsDone())
					{
						EmitError(pTcwork, pStnod, "unknown procedure in type check: %s", strProcName.PCoz());
						return TCRET_StoppingError;
					}

					// the first argument is index 1, (the procedure's identifier is element zero)

					int cSymOptions = 0;
					int cArgMismatch = 0;
					int iStnodArgMin = 1;

					struct SSymMatch
					{
						SSymbol *	m_pSym;
						PROCMATCH 	m_procmatch;
					};
					CFixAry<SSymMatch, 32> arySymmatch;
					int	mpProcmatchCSymmatch[PROCMATCH_Max];
					ZeroAB(mpProcmatchCSymmatch, sizeof(mpProcmatchCSymmatch));

					SSymbol * pSymProc;
					while (pSymProc = symiter.PSymNext())
					{
						CSTNode * pStnodDefinition = pSymProc->m_pStnodDefinition;
						if (!pStnodDefinition)
							continue;

						if (pStnodDefinition->m_strees < STREES_SignatureTypeChecked)
						{
							// wait for this procedure's signature to be type checked.
							SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymProc);
							pUntype->m_aryiTcframDependent.Append((int)pTcwork->m_aryTcfram.IFromP(pTcfram));
							return TCRET_WaitingForSymbolDefinition;
						}

						if (!EWC_FVERIFY(pSymProc->m_pTin, "bad symbol in proc call lookup"))
							continue;

						if (pSymProc->m_pTin->m_tink != TINK_Procedure)
							continue;

						++cSymOptions;

						if (pSymProc)
						{
							auto pTinprocSym = PTinRtiCast<STypeInfoProcedure  *>(pSymProc->m_pTin);
							if (!EWC_FVERIFY(pTinprocSym, "expected type info procedure"))
								continue;

							auto procmatch = ProcmatchCheckArguments(pTcwork, pSymtab, pTinprocSym, pStnod, false);
							if (procmatch != PROCMATCH_None)
							{
								++mpProcmatchCSymmatch[procmatch];
								auto pSymmatch = arySymmatch.AppendNew();
								pSymmatch->m_pSym = pSymProc;
								pSymmatch->m_procmatch = procmatch;
							}
						}
					}

					PROCMATCH procmatchFinal = (mpProcmatchCSymmatch[PROCMATCH_Exact] > 0) ? PROCMATCH_Exact : PROCMATCH_ImplicitCast;

					int cSysmatch = mpProcmatchCSymmatch[procmatchFinal];
					if (cSysmatch == 0)
					{
						if (cSymOptions == 0)
						{
							EmitError(pTcwork, pStnod, "'%s' does not evaluate to a procedure.", strProcName.PCoz());
						}
						else if (cSymOptions == 1)
						{
							// print out non overloaded mismatch errors.
							SSymbol * pSymProc = pSymtab->PSymLookup(strProcName, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);
							if (pSymProc)
							{
								auto pTinprocSym = PTinRtiCast<STypeInfoProcedure  *>(pSymProc->m_pTin);
								if (EWC_FVERIFY(pTinprocSym, "expected type info procedure"))
								{
									(void) ProcmatchCheckArguments( pTcwork, pSymtab, pTinprocSym, pStnod, true);
								}
							}

							if (pTcwork->m_pErrman->m_cError == 0)
							{
								EmitError(pTcwork, pStnod, "error type matching procedure '%s'", strProcName.PCoz());
							}
						}
						else
						{
							SError error(pTcwork->m_pErrman);
							PrintErrorLine(&error, "Error:", &pStnod->m_lexloc, "No overload matches procedure call. Options are:");

							symiter = CSymbolTable::CSymbolIterator(pSymtab, strProcName, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);
							while (SSymbol * pSym = symiter.PSymNext())
							{
								auto pTinproc = PTinDerivedCast<STypeInfoProcedure  *>(pSym->m_pTin);
								CString strProc = StrFromTypeInfo(pTinproc);

								PrintErrorLine(&error, "   ", &pSym->m_pStnodDefinition->m_lexloc, "%s\n", strProc.PCoz());
							}
						}
						return TCRET_StoppingError;
					}
					else if (cSysmatch == 1)
					{
						SSymMatch * pSysmatch = nullptr;
						for (size_t iSymmatch = 0; iSymmatch < arySymmatch.C(); ++iSymmatch)
						{
							if (arySymmatch[iSymmatch].m_procmatch == procmatchFinal)
							{
								pSysmatch = &arySymmatch[iSymmatch];
								break;
							}
						}
						if (!EWC_FVERIFY(pSysmatch, "matching procedure lookup failed"))
							return TCRET_StoppingError;

						SSymbol * pSymProc = pSysmatch->m_pSym;
						pStnod->m_pSym = pSymProc;

						// Note: The callee's sym and pTin are set by type checking the identifier, this may pick the wrong
						//  overload, clean it up here.
						pStnodCallee->m_pSym = pSymProc;
						pStnodCallee->m_pTin = pSymProc->m_pTin;

						if (EWC_FVERIFY(pSymProc, "expected symbol"))
						{
							auto pTinprocSym = PTinRtiCast<STypeInfoProcedure *>(pSymProc->m_pTin);
							
							auto procmatch = ProcmatchCheckArguments(pTcwork, pSymtab, pTinprocSym, pStnod, false);
							EWC_ASSERT(procmatch != PROCMATCH_None, "overload resolution failed");
						}
					}
					else // cSymmatch > 1 
					{	

						SError error(pTcwork->m_pErrman);
						PrintErrorLine(&error, "Error:", &pStnod->m_lexloc, "Overloaded procedure is ambiguous. Options are:");

						SSymMatch * pSymmatchMac = arySymmatch.PMac();
						for (SSymMatch * pSymmatch = arySymmatch.A(); pSymmatch != pSymmatchMac; ++pSymmatch)
						{
							SSymbol * pSym = pSymmatch->m_pSym;
							auto pTinproc = PTinDerivedCast<STypeInfoProcedure  *>(pSym->m_pTin);
							CString strProc = StrFromTypeInfo(pTinproc);

							PrintErrorLine(&error, "   ", &pSym->m_pStnodDefinition->m_lexloc, "%s", strProc.PCoz());
						}
					}

					pSymProc = pStnod->m_pSym;
					pTinproc = pSymProc ? PTinRtiCast<STypeInfoProcedure *>(pSymProc->m_pTin) : nullptr;

					fIsDirectCall = FIsDirectCall(pStnod);
					if (fIsDirectCall && pSymProc)
					{
						CSTNode * pStnodDefinition = pSymProc->m_pStnodDefinition;

						EWC_ASSERT(pStnodDefinition->m_strees >= STREES_SignatureTypeChecked, "expected definition to be type checked");
						EWC_ASSERT(pStnodDefinition->m_pTin == pTinproc, "tin mysmatch");

						CSTProcedure * pStproc = pStnodDefinition->m_pStproc;

						if (EWC_FVERIFY(pStproc, "bad procedure return info"))
						{
							if (pStproc->m_iStnodReturnType >= 0)
							{
								auto pTinReturnCheck = pStnodDefinition->PStnodChild(pStproc->m_iStnodReturnType)->m_pTin;
								EWC_ASSERT(pTinproc->m_arypTinReturns[0] == pTinReturnCheck, "return type mismatch");
							}
						}
					}
				}
				else // callee is not an identifier - proc indirect call 
				{
					auto pTinprocSym = PTinRtiCast<STypeInfoProcedure  *>(pStnodCallee->m_pTin);
					if (EWC_FVERIFY(pTinprocSym, "expected type info procedure"))
					{
						auto procmatch = ProcmatchCheckArguments( pTcwork, pSymtab, pTinprocSym, pStnod, true);
						if (procmatch == PROCMATCH_None)
							return TCRET_StoppingError;
					}
				}

				if (!fIsDirectCall)
				{
					pTinproc = PTinRtiCast<STypeInfoProcedure *>(pStnodCallee->m_pTin);
					if (!pTinproc)
					{
						CString strType = StrFromTypeInfo(pStnodCallee->m_pTin);
						EmitError(pTcwork, pStnod, "Statement does not evaluate to a procedure, type is a %s", strType.PCoz());
					}
				}

				if (!pTinproc)
					return TCRET_StoppingError;

				// Revisit all the arguments to finalize literals
				int iStnodArgMin = 1;
				for (int iStnodArg = iStnodArgMin; iStnodArg < pStnod->CStnodChild(); ++iStnodArg)
				{
					CSTNode * pStnodArg = pStnod->PStnodChild(iStnodArg);
					STypeInfo * pTinCall = pStnodArg->m_pTin;
					STypeInfo * pTinParam = nullptr;
					
					int ipTinParam = iStnodArg - iStnodArgMin;
					if (ipTinParam < (int)pTinproc->m_arypTinParams.C())
					{
						pTinParam = pTinproc->m_arypTinParams[ipTinParam];
						if (!EWC_FVERIFY(pTinParam, "unknown parameter type"))
							return TCRET_StoppingError;

						pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodArg, pTinParam);
					}
					else
					{
						if (!EWC_FVERIFY(pTinproc->m_fHasVarArgs, "bad procedure match!"))
							return TCRET_StoppingError;

						pTinCall = PTinPromoteUntypedArgument(pTcwork, pSymtab, pStnodArg, nullptr);
						pTinParam = PTinPromoteVarArg(pTcwork, pSymtab, pTinCall);
					}

					// if we have a literal, just expect the finalized type to be correct, otherwise
					//  set the implicit cast type on the argument node
					if (pStnodArg->m_pTin->m_tink == TINK_Literal)
					{
						FinalizeLiteralType(pSymtab, pTinParam, pStnodArg);
					}
					else
					{
						if (!FTypesAreSame(pStnodArg->m_pTin, pTinParam))
						{
							auto pAlloc = pTcwork->m_pAlloc;
							CSTNode * pStnodCast = EWC_NEW(pAlloc, CSTNode) CSTNode(pAlloc, pStnodArg->m_lexloc);
							pStnodCast->m_park = PARK_Cast;
							pStnodCast->m_pTin = pTinParam;

							auto  pStdecl = EWC_NEW(pAlloc, CSTDecl) CSTDecl();
							pStdecl->m_iStnodInit = pStnodCast->IAppendChild(pStnodArg);
							pStnodCast->m_pStdecl = pStdecl;

							pStnod->ReplaceChild(pStnodArg, pStnodCast);

							if (!FVerifyIsInstance(pTcwork, pStnodArg))
								return TCRET_StoppingError;

							if (EWC_FVERIFY(pStnodArg->m_strees == STREES_TypeChecked, "expected arg to be type checked"))
								pStnodCast->m_strees = STREES_TypeChecked;

						}
					}
				}

				STypeInfo * pTinReturn = pTinproc->m_arypTinReturns[0];
				pStnod->m_pTin = pTinReturn;
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			}break;
			case PARK_EnumDefinition:
			{
				auto pTinenum = PTinDerivedCast<STypeInfoEnum *>(pStnod->m_pTin);
				auto pStenum = pStnod->m_pStenum;
				if (!EWC_FVERIFY(pTinenum && pStenum, "missing struct type info"))
					return TCRET_StoppingError;

				// skip identifier
				if (pTcsentTop->m_nState == pStenum->m_iStnodIdentifier)
					++pTcsentTop->m_nState;

				if (pTcsentTop->m_nState == 1)
				{
					++pTcsentTop->m_nState;

					// type spec
					if (pStenum->m_iStnodType >= 0)
					{
						PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pStenum->m_iStnodType));
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;

						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
						break;
					}
				}

				if (pTcsentTop->m_nState == 2)
				{
					if (pStenum->m_iStnodType >= 0)
					{
						auto pStnodType = pStnod->PStnodChild(pStenum->m_iStnodType);
						bool fIsValidTypeSpec;
						auto pTinLoose = PTinFromTypeSpecification(
											pTcwork,
											pTcsentTop->m_pSymtab,
											pStnodType,
											pTcsentTop->m_grfsymlook,
											nullptr,
											&fIsValidTypeSpec);

						if (fIsValidTypeSpec)
						{
							pTinenum->m_pTinLoose = pTinLoose;
						}
					}
					++pTcsentTop->m_nState;
				}

				if (pTcsentTop->m_nState == 3)
				{
					if (pStenum->m_iStnodConstantList >= 0)
					{
						pTinenum->m_bintLatest = BintFromInt(-1);  // -1 so initial incremented value is zero

						PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pStenum->m_iStnodConstantList));
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
						++pTcsentTop->m_nState;
						break;
					}
				}

				auto pTinstruct = &pTinenum->m_tinstructProduced;
				EWC_ASSERT(pTinstruct->m_aryTypemembField.C() == 0, "no fields expected in enum struct");

				int cStnodChild = 0;
				CSTNode ** ppStnodMember = nullptr;
				if (pStenum->m_iStnodConstantList >= 0)
				{
					CSTNode * pStnodConstantList = pStnod->PStnodChild(pStenum->m_iStnodConstantList);

					ppStnodMember = PPStnodChildFromPark(pStnodConstantList, &cStnodChild, PARK_List);

					if (!EWC_FVERIFY(cStnodChild >= ENUMIMP_Max, "missing implicit enum members"))
						return TCRET_StoppingError;

					// loop over our constants and find the min/max values
					CSTNode ** ppStnodIt = &ppStnodMember[ENUMIMP_Max];
					CSTNode ** ppStnodMax = &ppStnodMember[cStnodChild];
					SBigInt bintMin;
					SBigInt bintMax;
					if (ppStnodIt != ppStnodMax && (*ppStnodIt)->m_pStval)
					{
						bintMin = BintFromStval((*ppStnodIt)->m_pStval);
						bintMax = bintMin;
						++ppStnodIt;
					}

					for ( ; ppStnodIt!= ppStnodMax; ++ppStnodIt)
					{
						auto pStnodMember = *ppStnodIt;
						if (EWC_FVERIFY(pStnodMember->m_pStval, "PARK_EnumConstant type check failed to set values"))
						{
							auto bint = BintFromStval(pStnodMember->m_pStval);

							if (bint < bintMin)
								bintMin = bint;
							if (bint > bintMax)
								bintMax = bint;
						}
					}

					pTinenum->m_bintMin = bintMin;
					pTinenum->m_bintMax = bintMax;

					if (!pTinenum->m_pTinLoose)
					{
						pTinenum->m_pTinLoose = PTinFromRange(pTcwork, pTcsentTop->m_pSymtab, pStnod, bintMin, bintMax);
					}

					auto pTinLoose = pTinenum->m_pTinLoose;
					if (EWC_FVERIFY(pTinLoose && pTinLoose->m_tink == TINK_Integer, "bad enum pTinLoose"))
					{
						auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinLoose);
						SBigInt bintNil;
						if (pTinint->m_fIsSigned)
						{
							bintNil = BintFromInt(-1);
						}
						else
						{
							switch (pTinint->m_cBit)
							{
							case 8:		bintNil = BintFromInt(0xFF); break;
							case 16:	bintNil = BintFromInt(0xFFFF); break;
							case 32:	bintNil = BintFromInt(0xFFFFFFFF); break;
							case 64:	bintNil = BintFromInt(0xFFFFFFFFFFFFFFFFULL); break;
							default: EWC_ASSERT(false, "unexpected cBit");
							}
						}

						SetEnumConstantValue(pTcwork, ppStnodMember[ENUMIMP_NilConstant], bintNil);
						SetEnumConstantValue(pTcwork, ppStnodMember[ENUMIMP_MinConstant], bintMin);
						SetEnumConstantValue(pTcwork, ppStnodMember[ENUMIMP_LastConstant], bintMax);
						SetEnumConstantValue(pTcwork, ppStnodMember[ENUMIMP_MaxConstant], BintAdd(bintMax, BintFromInt(1)));
					}
				}

				if (!pTinenum->m_pTinLoose)
				{
					EmitError(pTcwork, pStnod, "Unable to determine loose type for enum %s", pTinenum->m_strName.PCoz());
					return TCRET_StoppingError;
				}
				else
				{
					CSTNode * pStnodNames = ppStnodMember[ENUMIMP_Names];
					CSTNode * pStnodValues = ppStnodMember[ENUMIMP_Values];

					int cBitLoose = 64;
					bool fIsSignedLoose = true;
					auto pTinintLoose = PTinRtiCast<STypeInfoInteger *>(pTinenum->m_pTinLoose);
					if (EWC_FVERIFY(pTinintLoose, "expected enum type to be integer"))
					{
						cBitLoose = pTinintLoose->m_cBit;
						fIsSignedLoose = pTinintLoose->m_fIsSigned;
					}

					auto pSymtab = pTcsentTop->m_pSymtab;
					STypeInfoLiteral * pTinlitValue = pSymtab->PTinlitFromLitk(LITK_Integer, cBitLoose, fIsSignedLoose);
					STypeInfoLiteral * pTinlitName = pSymtab->PTinlitFromLitk(LITK_String);

					auto pTinU8 = pSymtab->PTinBuiltin("u8");
					STypeInfo * pTinString = pSymtab->PTinptrAllocReference(pTinU8);

					SpoofLiteralArray(pTcwork, pSymtab, pStnodNames, cStnodChild - ENUMIMP_Max, pTinString);
					auto pStnodNameList = pStnodNames->PStnodChildSafe(pStnodNames->m_pStdecl->m_iStnodInit);

					SpoofLiteralArray(pTcwork, pSymtab, pStnodValues, cStnodChild - ENUMIMP_Max, pTinenum->m_pTinLoose);
					auto pStnodValueList = pStnodValues->PStnodChildSafe(pStnodValues->m_pStdecl->m_iStnodInit);

					// assign pTin and finalize literals
					for (int iStnodMember = 0; iStnodMember < cStnodChild; ++iStnodMember)
					{
						auto pStnodMember = ppStnodMember[iStnodMember];
						if ((iStnodMember == ENUMIMP_Names) | (iStnodMember == ENUMIMP_Values))
							continue;

						// just make sure the init type fits the specified one
						auto pStnodInit = pStnodMember->PStnodChildSafe(1);
						if (pStnodInit)
						{
							STypeInfo * pTinInit = pStnodMember->m_pTin;

							pTinInit = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pTinenum->m_pTinLoose);
							if (!FCanImplicitCast(pTinInit, pTinenum->m_pTinLoose))
							{
								EmitError(pTcwork, pStnodInit, "Cannot initialize constant of type %s with %s",
									StrFromTypeInfo(pTinenum->m_pTinLoose).PCoz(),
									StrFromTypeInfo(pTinInit).PCoz());
							}
						}

						auto pTinecon = pTinenum->m_aryTinecon.AppendNew();
						pTinecon->m_bintValue = BintFromStval(pStnodMember->m_pStval);

						CSTDecl * pStdecl = pStnodMember->m_pStdecl;
						CSTNode * pStnodIdent = (pStdecl) ? pStnodMember->PStnodChildSafe(pStdecl->m_iStnodIdentifier) : nullptr;
						if (EWC_FVERIFY(pStnodIdent && pStnodIdent->m_pStident, "Enum constant missing name"))
						{
							pTinecon->m_strName = pStnodIdent->m_pStident->m_str;
						}

						if (pStnodMember->m_grfstnod.FIsSet(FSTNOD_ImplicitMember))
							continue;

						AddEnumNameValuePair(pTcwork, pSymtab, pStnodNameList, pStnodValueList, pStnodMember, pTinlitValue, pTinlitName);
					}
				}

				SSymbol * pSymEnum = nullptr;
				if (pStenum->m_iStnodIdentifier >= 0)
				{
					CSTNode * pStnodIdent = pStnod->PStnodChild(0);
					CString strIdent = StrFromIdentifier(pStnodIdent);
					pSymEnum = pTcsentTop->m_pSymtab->PSymLookup(strIdent, pStnod->m_lexloc, pTcsentTop->m_grfsymlook);
				}

				if (!EWC_FVERIFY(pSymEnum, "Failed to find enum name symbol"))
					return TCRET_StoppingError;
				if (!EWC_FVERIFY(pSymEnum->m_pTin, "expected structure type info to be created during parse"))
					return TCRET_StoppingError;

				auto grfsymlook = pTcsentTop->m_grfsymlook;
				ResolveSpoofTypedef(pTcwork, pStnod->m_pSymtab, pStnod, "loose", pTinenum->m_pTinLoose, grfsymlook);
				ResolveSpoofTypedef(pTcwork, pStnod->m_pSymtab, pStnod, "strict", pTinenum, grfsymlook);

				OnTypeComplete(pTcwork, pSymEnum);
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			}break;
			case PARK_EnumConstant:
			{
				if (pTcsentTop->m_nState < 1)
					pTcsentTop->m_nState = 1;	// skip the identifier

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					++pTcsentTop->m_nState;
					break;
				}

				auto pStdecl = pStnod->m_pStdecl;
				STypeInfoEnum * pTinenum = nullptr;
				auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnod->m_pTin);
				if (pTinlit)
				{
					pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinlit->m_pTinSource);
				}
					
				if (!EWC_FVERIFY(pTinenum, "expected enum literal") ||
					!EWC_FVERIFY(pStdecl, "enum literal without syntax tree decl struct"))
					return TCRET_StoppingError;

				CSTNode * pStnodIdent = pStnod->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
				if (!EWC_FVERIFY(pStnodIdent, "constant Declaration without identifier"))				
					return TCRET_StoppingError;
					
				CString strIdent = StrFromIdentifier(pStnodIdent);
				auto pStnodInit = pStnod->PStnodChildSafe(pStdecl->m_iStnodInit);
				if (pStnodInit)
				{
					STypeInfo * pTinInit = pStnodInit->m_pTin;
					bool fHasConstInt = pTinInit->m_tink == TINK_Literal; 

					if (!fHasConstInt)
					{
						EmitError(pTcwork, pStnod, "initializing enum constant '%s' with non-constant", strIdent.PCoz());
						return TCRET_StoppingError;
					}

					pStnod->m_pStval = PStvalCopy(pTcwork->m_pAlloc, pStnodInit->m_pStval);
					pTinenum->m_bintLatest = BintFromStval(pStnod->m_pStval);
				}
				else if (!pStnod->m_grfstnod.FIsSet(FSTNOD_ImplicitMember))
				{
					pTinenum->m_bintLatest = BintAdd(pTinenum->m_bintLatest, BintFromUint(1));
					SetEnumConstantValue(pTcwork, pStnod, pTinenum->m_bintLatest);
				}

				// Find our symbol and resolve any pending unknown types - we don't have a concrete type yet
				//  but that should be ok.
				auto pSymtab = pTcsentTop->m_pSymtab;
				SSymbol * pSymIdent = pSymtab->PSymLookup(
												strIdent,
												pStnodIdent->m_lexloc,
												pTcsentTop->m_grfsymlook);

				if (EWC_FVERIFY(pSymIdent && pSymIdent->m_pStnodDefinition == pStnod, "symbol lookup failed for '%s'", strIdent.PCoz()))
				{
					pStnod->m_pSym = pSymIdent;
					if (pSymIdent->m_pTin == nullptr)
					{
						pSymIdent->m_pTin = pStnod->m_pTin;
					}
				}
				OnTypeComplete(pTcwork, pSymIdent);

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_StructDefinition:
			{
				auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pStnod->m_pTin);
				if (!EWC_FVERIFY(pTinstruct, "missing struct type info"))
					return TCRET_StoppingError;

				// Note: struct layout is just children[identifierName, DeclList]
				if (pTcsentTop->m_nState == 0)
				{
					pStnod->m_strees = STREES_SignatureTypeChecked;
					if (pStnod->CStnodChild() >= 1)
					{
						CSTNode * pStnodMembers = pStnod->PStnodChild(1);
						PushTcsent(pTcfram, &pTcsentTop, pStnodMembers);

						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
					}
					++pTcsentTop->m_nState;
					break;
				}

				auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
				for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb)
				{
					pTypememb->m_pTin = pTypememb->m_pStnod->m_pTin;
				}

				SSymbol * pSymStruct = nullptr;
				if (pStnod->CStnodChild() >= 0)
				{
					CSTNode * pStnodIdent = pStnod->PStnodChild(0);
					CString strIdent = StrFromIdentifier(pStnodIdent);
					pSymStruct = pTcsentTop->m_pSymtab->PSymLookup(strIdent, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);
				}

				if (!EWC_FVERIFY(pSymStruct, "failed to find structure name symbol"))
					return TCRET_StoppingError;
				if (!EWC_FVERIFY(pSymStruct->m_pTin, "expected structure type info to be created during parse"))
					return TCRET_StoppingError;

				PopTcsent(pTcfram, &pTcsentTop, pStnod);

				pStnod->m_strees = STREES_TypeChecked;
				OnTypeComplete(pTcwork, pSymStruct);
			}break;
			case PARK_Identifier:
			{
				// Note: we're only expecting to get here for identifiers within statements.
				//  Identifiers for function names, declaration names and types, should do their own type checking.

				CString strIdent = StrFromIdentifier(pStnod);
				if (EWC_FVERIFY(!strIdent.FIsEmpty(), "identifier node with no value"))
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					auto pSym = pSymtab->PSymLookup(strIdent, pStnod->m_lexloc, pTcsentTop->m_grfsymlook);
					if (!pSym)
					{
						EmitError(pTcwork, pStnod, "'%s' unknown identifier detected", strIdent.PCoz());
						return TCRET_StoppingError;
					}

					if (pSym->m_grfsym.FIsSet(FSYM_IsBuiltIn))
					{
						pStnod->m_pTin = pSym->m_pTin;
						pStnod->m_pSym = pSym;
					}
					else if (EWC_FVERIFY(pSym->m_pStnodDefinition, "Non-built-in types must have a STNode"))
					{
						CSTNode * pStnodDefinition = pSym->m_pStnodDefinition;
						if (pStnodDefinition->m_park == PARK_Decl ||
							pStnodDefinition->m_park == PARK_ConstantDecl ||
							pStnodDefinition->m_park == PARK_ArrayLiteral ||
							pStnodDefinition->m_park == PARK_Typedef ||
							pStnodDefinition->m_park == PARK_EnumDefinition ||
							pStnodDefinition->m_park == PARK_EnumConstant ||
							pStnodDefinition->m_park == PARK_StructDefinition ||
							pStnodDefinition->m_park == PARK_ProcedureDefinition)
						{
							if (pStnodDefinition->m_strees >= STREES_TypeChecked || 
							   ((pStnodDefinition->m_strees >= STREES_SignatureTypeChecked) && pTcsentTop->m_fAllowForwardDecl))
							{
								EWC_ASSERT(pStnodDefinition->m_pTin, "symbol definition was type checked, but has no type?");
								pStnod->m_pTin = pStnodDefinition->m_pTin;
								pStnod->m_pSym = pSym;

								if (pStnod->m_pTin && 
									(pStnod->m_pTin->m_tink == TINK_Literal || pStnod->m_pTin->m_tink == TINK_Enum) &&
									pStnodDefinition->m_pStval)
								{
									pStnod->m_pStval = PStvalCopy(pSymtab->m_pAlloc, pStnodDefinition->m_pStval);
								}
							}
							else
							{
								// set up dependency for either the definition or the type...
								
								SSymbol * pSymDepend = pSym;
								SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymDepend);
								pUntype->m_aryiTcframDependent.Append((int)pTcwork->m_aryTcfram.IFromP(pTcfram));
								return TCRET_WaitingForSymbolDefinition;
							}
						}
						else
						{
							EWC_ASSERT(false, "unexpected identifier in type check");
						}
					}
				}

				PopTcsent(pTcfram, &pTcsentTop, pStnod);
				pStnod->m_strees = STREES_TypeChecked;
			}break;

			case PARK_ArrayDecl:
			case PARK_ReferenceDecl:
			case PARK_ParameterList:
			case PARK_List:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));

				STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
				if (pStnod->m_park == PARK_List)
				{
					if (pStnod->m_pSymtab)
					{
						pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
					}
				}
				if (pStnod->m_park == PARK_ReferenceDecl)
				{
					pTcsentPushed->m_fAllowForwardDecl = true;
				}

			} break;
			case PARK_Uninitializer:
			case PARK_Nop:
			case PARK_VariadicArg:
			{
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_ConstantDecl:
			{
				if (pTcsentTop->m_nState < 1)
					pTcsentTop->m_nState = 1;	// skip the identifier

				auto * pStdecl = pStnod->m_pStdecl;
				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));

					if (pTcsentTop->m_nState == pStdecl->m_iStnodType)
					{
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					}
					
					++pTcsentTop->m_nState;
					break;
				}

				auto pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);
				CSTNode * pStnodIdent = pStnod->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
				CString strIdent = StrFromIdentifier(pStnodIdent);
				if (FIsType(pStnodInit))
				{
					EmitError(pTcwork, pStnod, "Cannot initialize constant '%s' to non-instance value.",strIdent.PCoz());
					return TCRET_StoppingError;
				}

				auto pSymtab = pTcsentTop->m_pSymtab;
				auto pStnodType = pStnod->PStnodChildSafe(pStdecl->m_iStnodType);
				if (pStnodType)
				{
					bool fIsValidTypeSpec;
					STypeInfo * pTinType = PTinFromTypeSpecification(
											pTcwork,
											pSymtab,
											pStnodType,
											pTcsentTop->m_grfsymlook,
											nullptr,
											&fIsValidTypeSpec);

					if (!fIsValidTypeSpec)
						return TCRET_StoppingError;

					// just make sure the init type fits the specified one
					STypeInfo * pTinInit = pStnodInit->m_pTin;
					pTinInit = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pTinType);
					if (FCanImplicitCast(pTinInit, pTinType))
					{
						FinalizeLiteralType(pSymtab, pTinType, pStnodInit);
					}
					else
					{
						EmitError(pTcwork, pStnod, "Cannot initialize constant of type %s with %s",
							StrFromTypeInfo(pTinType).PCoz(),
							StrFromTypeInfo(pTinInit).PCoz());
					}
				}
				else
				{
					// Promote as literal, just to error on untyped acasts. We don't actually force a type on
					//  the constant until it gets used
					(void) PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodInit);
				}

				pStnod->m_pStval = PStvalCopy(pTcwork->m_pAlloc, pStnodInit->m_pStval);
				pStnod->m_pTin = pStnodInit->m_pTin;

				// find our symbol and resolve any pending unknown types
				if (EWC_FVERIFY(pStnodIdent, "constant Declaration without identifier"))
				{
					SSymbol * pSymIdent = pSymtab->PSymLookup(
													strIdent,
													pStnodIdent->m_lexloc,
													pTcsentTop->m_grfsymlook);

					if (!pSymIdent || pSymIdent->m_pStnodDefinition != pStnod)
					{
						EmitError(pTcwork, pStnod, "symbol lookup failed for '%s'", strIdent.PCoz());
						return TCRET_StoppingError;
					}
					else
					{
						pStnod->m_pSym = pSymIdent;
						if (pSymIdent->m_pTin == nullptr)
						{
							pSymIdent->m_pTin = pStnod->m_pTin;
						}
					}
					OnTypeComplete(pTcwork, pSymIdent);
				}

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_Typedef:
			{
				if (!EWC_FVERIFY(pStnod->CStnodChild() == 2, "typedef should have 2 children (ident, typespec)"))
					return TCRET_StoppingError;

				CSTNode * pStnodType = pStnod->PStnodChild(1);
				if (pTcsentTop->m_nState == 0)
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnodType);
					STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
					pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					
					++pTcsentTop->m_nState;
					break;
				}

				SSymbol * pSymType = nullptr;
				bool fIsValidTypeSpec;
				auto pSymtab = pTcsentTop->m_pSymtab;
				pStnod->m_pTin = PTinFromTypeSpecification(
					pTcwork, 
					pSymtab,
					pStnodType,
					pTcsentTop->m_grfsymlook,
					&pSymType,
					&fIsValidTypeSpec);

				CSTNode * pStnodIdent = pStnod->PStnodChild(0);
				if (!fIsValidTypeSpec)
				{
					EmitError(pTcwork, pStnod, "Cannot determine type for typedef '%s'", 
						(pStnodIdent) ? StrFromIdentifier(pStnodIdent).PCoz() : "unknown");
					return TCRET_StoppingError;
				}

				// find our symbol and resolve any pending unknown types
				if (EWC_FVERIFY(pStnodIdent, "constant Declaration without identifier"))
				{
					CString strIdent = StrFromIdentifier(pStnodIdent);
					auto pSym = pSymtab->PSymLookup( strIdent, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);

					if (EWC_FVERIFY(pSym, "symbol lookup failed for '%s'", strIdent.PCoz()))
					{
						if (pSym->m_pStnodDefinition != pStnod)
						{
							s32 iLine;
							s32 iCol;
							auto pLexlocDefinition = &pSym->m_pStnodDefinition->m_lexloc;
							CalculateLinePosition(pTcwork->m_pErrman->m_pWork, pLexlocDefinition , &iLine, &iCol);

							EmitError(pTcwork, pStnod, "Symbol '%s' is also defined here: %s(%d,%d)",
								strIdent.PCoz(),
								pLexlocDefinition->m_strFilename.PCoz(), iLine, iCol);
							return TCRET_StoppingError;
						}
						else
						{
							pStnod->m_pSym = pSym;
							if (pSym->m_pTin == nullptr)
							{
								pSym->m_pTin = pStnod->m_pTin;
							}
						}
					}
					OnTypeComplete(pTcwork, pSym);
				}

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_Cast:
			{
				auto * pStdecl = pStnod->m_pStdecl;
				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));

					if (pTcsentTop->m_nState == pStdecl->m_iStnodType)
					{
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					}
					
					++pTcsentTop->m_nState;
					break;
				}

				if (!EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "bad explicit cast"))
					return TCRET_StoppingError;

				CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
				auto pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);

				STypeInfo * pTinInit = pStnodInit->m_pTin;

				// AutoCast will be resoved during promotion of untyped RHSs
				bool fIsAutoCast = pStdecl->m_iStnodType < 0;
				if (fIsAutoCast)
				{
					pStnod->m_pTin = pTinInit;
				}
				else
				{
					auto pStnodType = pStnod->PStnodChild(pStdecl->m_iStnodType);

					SSymbol * pSymType = nullptr;
					bool fIsValidTypeSpec;
					STypeInfo * pTinType = PTinFromTypeSpecification(
						pTcwork,
						pSymtab,
						pStnodType,
						pTcsentTop->m_grfsymlook,
						&pSymType,
						&fIsValidTypeSpec);

					if (!fIsValidTypeSpec)
						return TCRET_StoppingError;

					if (pTinType)
					{
						pStnod->m_pTin = pTinType;
					}
					else
					{
						return TcretWaitForTypeSymbol(pTcwork, pTcfram, pSymType, pStnodType);
					}

					pTinInit = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pStnod->m_pTin);
					if (FCanExplicitCast(pTinInit, pStnod->m_pTin))
					{
						FinalizeLiteralType(pSymtab, pStnod->m_pTin, pStnodInit);
						pTinInit = pStnod->m_pTin;
					}
					else
					{
						EmitError(pTcwork, pStnod, "Cannot cast type '%s' to '%s'",
							StrFromTypeInfo(pTinInit).PCoz(),
							StrFromTypeInfo(pStnod->m_pTin).PCoz());
					}
				}

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_Decl:
			{
				auto * pStdecl = pStnod->m_pStdecl;
				EWC_ASSERT(pStdecl, "missing decl parse data");

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					if (pTcsentTop->m_nState != pStdecl->m_iStnodIdentifier)
					{
						PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();

						if (pTcsentTop->m_nState == pStdecl->m_iStnodInit)
						{
							// Note: Allow forward declarations - we may be initializing to a pointer to the current procedure
							pTcsentPushed->m_fAllowForwardDecl = true;
						}
						else if (pTcsentTop->m_nState == pStdecl->m_iStnodType)
						{
							pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
						}
					}

					++pTcsentTop->m_nState;
					break;
				}

				/*
				if (pTcsentTop->m_nState == 0)	// type check initializer 
				{
					if (pStdecl->m_iStnodInit >= 0)
					{
						PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pStdecl->m_iStnodInit));

						// Note: Allow forward declarations - we may be initializing to a pointer to the current procedure
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_fAllowForwardDecl = true;
					}
					++pTcsentTop->m_nState;
				}
				else if (pTcsentTop->m_nState == 1) // type check type specification (for literal op eval)
				{
					if (pStdecl->m_iStnodType >= 0)
					{
						CSTNode * pStnodReturn = pStnod->PStnodChild(pStdecl->m_iStnodType);

						PushTcsent(pTcfram, &pTcsentTop, pStnodReturn);
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					}
					++pTcsentTop->m_nState;
				}
				else if (pTcsentTop->m_nState == 2) // resolve actual type
				*/
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					if (pStdecl->m_iStnodType >= 0)
					{
						CSTNode * pStnodType = pStnod->PStnodChild(pStdecl->m_iStnodType);

						SSymbol * pSymType = nullptr;
						bool fIsValidTypeSpec;
						STypeInfo * pTinType = PTinFromTypeSpecification(
												pTcwork,
												pSymtab,
												pStnodType,
												pTcsentTop->m_grfsymlook,
												&pSymType,
												&fIsValidTypeSpec);

						if (!fIsValidTypeSpec)
							return TCRET_StoppingError;

						if (pTinType)
						{
							pStnod->m_pTin = pTinType;
						}
						else
						{
							return TcretWaitForTypeSymbol(pTcwork, pTcfram, pSymType, pStnodType);
						}
					}

					if (pStdecl->m_iStnodInit >= 0)
					{
						CSTNode * pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);
						if (pStnod->m_pTin && pStnodInit->m_park != PARK_Uninitializer)
						{
							// just make sure the init type fits the specified one
							STypeInfo * pTinInit = pStnodInit->m_pTin;
							pTinInit = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pStnod->m_pTin);
							if (FCanImplicitCast(pTinInit, pStnod->m_pTin))
							{
								FinalizeLiteralType(pSymtab, pStnod->m_pTin, pStnodInit);
								pTinInit = pStnod->m_pTin;
							}
							else
							{
								EmitError(pTcwork, pStnod, "Cannot initialize variable of type '%s' with '%s'",
									StrFromTypeInfo(pStnod->m_pTin).PCoz(),
									StrFromTypeInfo(pTinInit).PCoz());
							}
						}
						else if (pStnodInit->m_pTin)
						{
							pStnod->m_pTin = PTinPromoteUntypedDefault(
													pTcwork,
													pTcsentTop->m_pSymtab,
													pStnodInit);

							EWC_ASSERT(pStnod->m_pTin);
							FinalizeLiteralType(pTcsentTop->m_pSymtab, pStnod->m_pTin, pStnodInit);
						}
					}

					CSTNode * pStnodIdent = pStnod->PStnodChildSafe(pStdecl->m_iStnodIdentifier);

					// would this be better if there was a PARK_CompoundDecl?
					bool fIsCompoundDecl = pStdecl->m_iStnodChildMin == -1;
					if (fIsCompoundDecl)
					{
						if (pStnod->m_pTin == nullptr)
						{
							const char * pCozIdent = (pStnodIdent) ? StrFromIdentifier(pStnodIdent).PCoz() : "declaration";
							EmitError(pTcwork, pStnod, "Unable to calculate type for %s", pCozIdent);
							return TCRET_StoppingError;
						}

						// find our symbol and resolve any pending unknown types
						if (EWC_FVERIFY(pStnodIdent, "Declaration without identifier"))
						{
							CString strIdent = StrFromIdentifier(pStnodIdent);

							// may not have symbols for a declaration if this is inside a procedure reference decl
							if (pStnodIdent->m_pSym)
							{
								auto pSymIdent = pStnodIdent->m_pSym;
								pSymIdent->m_pTin = pStnod->m_pTin;
								pStnod->m_pSym = pSymIdent;
								OnTypeComplete(pTcwork, pSymIdent);
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
				}
			}break;
			case PARK_ArrayLiteral:
			{
				if (pStnod->m_grfstnod.FIsSet(FSTNOD_ImplicitMember))
				{
					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					++pTcsentTop->m_nState;
					break;
				}

				auto pStdecl = pStnod->m_pStdecl;
				if (!EWC_FVERIFY(pStdecl, "invalid array literal"))
					return TCRET_StoppingError;

				if (EWC_FVERIFY(pStnod->m_pTin == nullptr, "STypeInfo should not be constructed before type checking"))
				{
					auto pSymtab = pTcsentTop->m_pSymtab;
					STypeInfo * pTinType = nullptr;
					if (pStdecl->m_iStnodType >= 0)
					{
						auto pStnodType = pStnod->PStnodChild(pStdecl->m_iStnodType);

						SSymbol * pSymType = nullptr;
						bool fIsValidTypeSpec;
						pTinType = PTinFromTypeSpecification(
												pTcwork,
												pSymtab,
												pStnodType,
												pTcsentTop->m_grfsymlook,
												&pSymType,
												&fIsValidTypeSpec);

						if (!fIsValidTypeSpec)
							return TCRET_StoppingError;
					}

					bool fHasValues = false;
					auto pStnodValues = pStnod->PStnodChildSafe(pStdecl->m_iStnodInit);
					if (pStnodValues && pStnodValues->CStnodChild())
					{
						auto pTinValue = pStnodValues->PStnodChild(0)->m_pTin;
						fHasValues = pTinValue && pTinValue->m_tink == TINK_Literal;
					}

					if (!fHasValues)
					{
						EmitError(pTcwork, pStnod, "Array literal without any element literals");
						return TCRET_StoppingError;
					}

					STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
					pSymtab->AddManagedTin(pTinlit);
					pTinlit->m_litty.m_litk = LITK_Array;
					pTinlit->m_pTinSource = pTinType;
					pTinlit->m_pStnodDefinition = pStnod;
					pStnod->m_pTin = pTinlit;

					if (pStdecl->m_iStnodInit >= 0)
					{
						auto pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);

						EWC_ASSERT(pStnodInit->m_park == PARK_List, "invalid ArrayLiteral");
						pTinlit->m_c = pStnodInit->CStnodChild();
					}
				}

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_Literal:
			{
				if (EWC_FVERIFY(pStnod->m_pTin == nullptr, "STypeInfoLiteral should not be constructed before type checking"))
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
					pSymtab->AddManagedTin(pTinlit);
					pStnod->m_pTin = pTinlit;

					if (EWC_FVERIFY(pStnod->m_pStval, "null value in literal"))
					{
						pTinlit->m_litty.m_litk = pStnod->m_pStval->m_litkLex;
					}
				}
				
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			}break;

			case PARK_AssignmentOp:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to assignment op"))
					{
						CSTNode * pStnodLhs = pStnod->PStnodChild(0);
						CSTNode * pStnodRhs = pStnod->PStnodChild(1);
						STypeInfo * pTinLhs = pStnodLhs->m_pTin;
						STypeInfo * pTinRhs = pStnodRhs->m_pTin;

						if (!FVerifyIsInstance(pTcwork, pStnodLhs) || !FVerifyIsInstance(pTcwork, pStnodRhs))
							return TCRET_StoppingError;

						TINK tinkLhs = TINK_Nil;
						if (EWC_FVERIFY(pTinLhs, "unexpected unknown type in assignment op LHS"))
						{
							bool fIsValidLhs = FIsValidLhs(pStnodLhs);
							if (!fIsValidLhs)
							{
								CString strLhs = StrFromTypeInfo(pTinLhs);
								EmitError(pTcwork, pStnod, "'%s' does not provide an assignment operator", strLhs.PCoz());
								return TCRET_StoppingError;
							}

							tinkLhs = pTinLhs->m_tink;
						}

						EWC_ASSERT(pTinLhs, "unexpected null type in assignment op RHS");

						STypeInfo * pTinRhsPromoted = PTinPromoteUntypedTightest(
														pTcwork,
														pTcsentTop->m_pSymtab,
														pStnodRhs,
														pTinLhs);

						CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
						SOpTypes optype = OptypeFromPark(pTcwork, pSymtab, pStnod->m_jtok, pStnod->m_park, pTinLhs, pTinRhsPromoted);

						if (!optype.FIsValid() || !FDoesOperatorExist(pStnod->m_jtok, &optype))
						{
							CString strLhs = StrFromTypeInfo(pTinLhs);
							CString strRhs = StrFromTypeInfo(pTinRhsPromoted);
							EmitError( pTcwork, pStnod,
								"operator '%s' is not defined for %s and %s",
								PCozFromJtok(pStnod->m_jtok),
								strLhs.PCoz(),
								strRhs.PCoz());
							return TCRET_StoppingError;
						}

						if (FCanImplicitCast(pTinRhsPromoted, optype.m_pTinRhs))
						{
							AllocateOptype(pStnod);
							*pStnod->m_pOptype = optype;
							FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinLhs, pStnodRhs);
						}
						else
						{
							CString strLhs = StrFromTypeInfo(pTinLhs);
							CString strRhs = StrFromTypeInfo(pTinRhsPromoted);
							EmitError( pTcwork, pStnod,
								"implicit cast from %s to %s is not allowed",
								strRhs.PCoz(),
								strLhs.PCoz());
						}
					}

					EWC_ASSERT(pStnod->m_pTin == nullptr, "assignment op has no 'return' value");

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			} break;
			case PARK_MemberLookup:
			{
				if (pTcsentTop->m_nState == 0)
				{
					++pTcsentTop->m_nState;
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(0));
					break;
				}

				CSTNode * pStnodRhs = pStnod->PStnodChildSafe(1);
				if (!pStnodRhs || pStnodRhs->m_park != PARK_Identifier)
				{
					const char * pChzRhs = (pStnodRhs) ? PChzFromPark(pStnodRhs->m_park) : "(null)";
					EmitError(pTcwork, pStnod, "Expected right hand side to identifier but it is %s", pChzRhs);
					return TCRET_StoppingError;
				}

				CSTNode * pStnodLhs = pStnod->PStnodChild(0);
				STypeInfoStruct * pTinstruct = nullptr;
				auto pTinLhs = pStnodLhs->m_pTin;
				if (pTinLhs)
				{
					if (pTinLhs->m_tink == TINK_Pointer)
					{
						pTinLhs = ((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo;
					}
					else if (pTinLhs->m_tink == TINK_Literal)
					{
						pTinLhs = PTinPromoteUntypedDefault(pTcwork, pTcsentTop->m_pSymtab, pStnodLhs);
						FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinLhs, pStnodLhs);
					}
				}

				CString strMemberName = StrFromIdentifier(pStnodRhs);
				STypeInfo * pTinMember = nullptr;
				CSTValue * pStvalMember = nullptr;
				if (pTinLhs)
				{
					switch (pTinLhs->m_tink)
					{
						case TINK_Enum:
						{
							auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinLhs);
							if (pTinenum)
							{
								pTinLhs = &pTinenum->m_tinstructProduced;
							}
						} // fall through
						case TINK_Struct:
						{
							pTinstruct = PTinRtiCast<STypeInfoStruct *>(pTinLhs);
							if (!pTinstruct)
								break;

							CSTNode * pStnodStruct = pTinstruct->m_pStnodStruct;
							if (EWC_FVERIFY(pStnodStruct && pStnodStruct->m_pSym, "Struct type missing symbol"))
							{
								if (pStnodStruct->m_strees != STREES_TypeChecked)
								{
									// wait for this type to be resolved.
									SUnknownType * pUntype = PUntypeEnsure(pTcwork, pStnodStruct->m_pSym);
									pUntype->m_aryiTcframDependent.Append((s32)pTcwork->m_aryTcfram.IFromP(pTcfram));
									return TCRET_WaitingForSymbolDefinition;
								}
							}

							auto pSymMember = pStnodStruct->m_pSymtab->PSymLookup(strMemberName, pStnodRhs->m_lexloc, FSYMLOOK_Local | FSYMLOOK_IgnoreOrder);
							if (!pSymMember)
							{
								EmitError(pTcwork, pStnod, "%s is not a member of %s", strMemberName.PCoz(), pTinstruct->m_strName.PCoz());
								return TCRET_StoppingError;
							}

							EWC_ASSERT(pSymMember->m_pTin, "expected symbol to have type");
							pTinMember = pSymMember->m_pTin;

							pStnodRhs->m_pSym = pSymMember;	

							if (pTinMember && pSymMember->m_pStnodDefinition)
							{
								if (pTinMember->m_tink == TINK_Literal)
								{
									CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
									pStvalMember = PStvalCopy(pSymtab->m_pAlloc, pSymMember->m_pStnodDefinition->m_pStval);
								}
							}
						} break;
						case TINK_Array:
						{
							CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
							auto pTinary = PTinRtiCast<STypeInfoArray *>(pTinLhs);

							ARYMEMB arymemb = ArymembLookup(strMemberName.PCoz());

							switch (arymemb)
							{
							case ARYMEMB_Count:
								{
									switch (pTinary->m_aryk)
									{
									case ARYK_Fixed:
										{
											auto pTinlitInt = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
											pTinlitInt->m_litty.m_litk = LITK_Integer;
											pSymtab->AddManagedTin(pTinlitInt);
											pTinMember = pTinlitInt;

											pStvalMember = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
											pStvalMember->m_stvalk = STVALK_SignedInt;
											pStvalMember->m_nSigned = pTinary->m_c;
										} break;
									case ARYK_Reference:
									case ARYK_Dynamic:
										{
											pTinMember = pSymtab->PTinBuiltin("s64");
										} break;
									}
								} break;
							case ARYMEMB_Data:
								{
									pTinMember = PTinptrAlloc(pSymtab, pTinary->m_pTin);
								} break;
							default: 
								EmitError(pTcwork, pStnod, "unknown array member '%s'", strMemberName.PCoz());
								return TCRET_StoppingError;
							}

						} break;
					}
				}

				if (!pTinMember)
				{
					CString strTin = StrFromTypeInfo(pTinLhs);
					EmitError(pTcwork, pStnod, "Left hand type '%s' does not contain member '%s'", strTin.PCoz(), strMemberName.PCoz());
					return TCRET_StoppingError;
				}

				pStnod->m_pTin = pTinMember;
				pStnod->m_strees = STREES_TypeChecked;

				if (pStvalMember)
				{
					pStnod->m_pStval = pStvalMember;
				}
				PopTcsent(pTcfram, &pTcsentTop, pStnod);

			} break;
			case PARK_ArrayElement:
			{
				int cStnodChild = pStnod->CStnodChild();
				if (!EWC_FVERIFY(cStnodChild == 2, "expected 2 children (array, index) for array element AST, found %d",  cStnodChild))
					return TCRET_StoppingError;

				if (pTcsentTop->m_nState < cStnodChild)
				{
					PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					++pTcsentTop->m_nState;
					break;
				}

				CSTNode * pStnodLhs = pStnod->PStnodChild(0);
				CSTNode * pStnodIndex = pStnod->PStnodChild(1);
				if (!EWC_FVERIFY(pStnodLhs && pStnodLhs->m_pTin, "Array element LHS has no type") ||
					!EWC_FVERIFY(pStnodIndex && pStnodIndex->m_pTin, "Array index has no type"))
					return TCRET_StoppingError;

				switch (pStnodLhs->m_pTin->m_tink)
				{
				case TINK_Array:
					{
						auto pTinary = (STypeInfoArray *)pStnodLhs->m_pTin;
						pStnod->m_pTin = pTinary->m_pTin;
					} break;
				case TINK_Pointer:
					{
						auto pTinptr = (STypeInfoPointer *)pStnodLhs->m_pTin;
						pStnod->m_pTin = pTinptr->m_pTinPointedTo;
					} break;
				default: 
					CString strLhs = StrFromTypeInfo(pStnodLhs->m_pTin);
					EmitError(pTcwork, pStnod, "%s cannot be indexed as an array", strLhs.PCoz());
					return TCRET_StoppingError;
				}

				auto pSymtab = pTcsentTop->m_pSymtab;

				auto pTinIndex = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodIndex);
				if (pTinIndex->m_tink == TINK_Enum)
				{
					auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinIndex);
					pTinIndex = pTinenum->m_pTinLoose;
				}

				if (pTinIndex->m_tink != TINK_Integer)
				{
					CString strTinIndex = StrFromTypeInfo(pTinIndex);
					EmitError(pTcwork, pStnod, "Cannot convert %s to integer for array index", strTinIndex.PCoz());
				}

				FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinIndex, pStnodIndex);
				
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_ReservedWord:
			{
				if (EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
				{
					RWORD rword = pStnod->m_pStval->m_rword;
					switch (rword)
					{
					case RWORD_Continue:
					case RWORD_Break:
						{
							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_Sizeof:
					case RWORD_Alignof:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							auto pStnodChild = pStnod->PStnodChild(0);
							if (!EWC_FVERIFY(pStnodChild, "sizeof/typeof missing child"))
								break;

							if (!pStnodChild->m_pTin)
							{
								EmitError(pTcwork, pStnod, "%s unable to determine target type", PCozFromRword(rword)); 
							}

							auto pSymtab = pTcsentTop->m_pSymtab;
							pStnod->m_pTin = pSymtab->PTinBuiltin("uSize");

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_For:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
								pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
								EWC_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");

								break;
							}

							if (pStnod->m_pStfor == nullptr)
							{
								EmitError(pTcwork, pStnod, "for loop was improperly parsed.");
								return TCRET_StoppingError;
							}

							STypeInfo * pTinIterator = nullptr;
							auto pStfor = pStnod->m_pStfor;
							auto pStnodDecl = pStnod->PStnodChildSafe(pStfor->m_iStnodDecl);
							if (pStnodDecl)
							{
								pTinIterator = pStnodDecl->m_pTin;
							}
							else
							{
								auto pStnodIterator = pStnod->PStnodChildSafe(pStfor->m_iStnodIterator);
								if (pStnodIterator && pStnodIterator->m_pTin)
								{
									pTinIterator = pStnodIterator->m_pTin;

									auto pStnodInit = pStnod->PStnodChildSafe(pStfor->m_iStnodInit);
									if (pStnodInit)
									{
										bool fIsValidLhs = FIsValidLhs(pStnodIterator);
										if (!fIsValidLhs)
										{
											CString strLhs = StrFromTypeInfo(pTinIterator);
											EmitError(pTcwork, pStnod, "'%s' is not a valid left-hand-side", strLhs.PCoz());
											return TCRET_StoppingError;
										}

										STypeInfo * pTinRhsPromoted = PTinPromoteUntypedTightest(
																		pTcwork,
																		pTcsentTop->m_pSymtab,
																		pStnodInit,
																		pTinIterator);
										if (FCanImplicitCast(pTinRhsPromoted, pTinIterator))
										{
											FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinIterator, pStnodInit);
										}
										else
										{
											CString strLhs = StrFromTypeInfo(pTinIterator);
											CString strRhs = StrFromTypeInfo(pTinRhsPromoted);
											EmitError( pTcwork, pStnod,
												"implicit cast from %s to %s is not allowed",
												strRhs.PCoz(),
												strLhs.PCoz());
										}
									}
								}
							}

							if (!pTinIterator)
							{
								EmitError(pTcwork, pStnod, "Cannot determine for loop iterator type");
								return TCRET_StoppingError;
							}

							auto pStnodPredicate = pStnod->PStnodChildSafe(pStfor->m_iStnodPredicate);
							if (EWC_FVERIFY(pStnodPredicate, "for loop missing predicate child"))
							{
								if (!pStnodPredicate->m_pTin || pStnodPredicate->m_pTin->m_tink != TINK_Bool)
								{
									CString strTin = StrFromTypeInfo(pStnodPredicate->m_pTin);
									EmitError(pTcwork, pStnod,
										"For loop predicate must evaluate to a bool, but evaluates to a %s",
										strTin.PCoz());
								}
							}

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_While:
					case RWORD_If:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							if (pStnod->CStnodChild() < 2)
							{
								EmitError(
									pTcwork,
									pStnod,
									"encountered %s statement without expected predicate,child",
									PCozFromRword(rword));
								return TCRET_StoppingError;
							}

							// (if (predicate) (ifCase) (else (elseCase)))
							// (while (predicate) (body))
							CSTNode * pStnodPred = pStnod->PStnodChild(0);
							STypeInfo * pTinPred = pStnodPred->m_pTin;

							auto pSymtab = pTcsentTop->m_pSymtab;
							STypeInfo * pTinBool = pSymtab->PTinBuiltin("bool");
							STypeInfo * pTinPredPromoted = PTinPromoteUntypedTightest(
															pTcwork,
															pTcsentTop->m_pSymtab,
															pStnodPred,
															pTinBool);
							if (!FCanImplicitCast(pTinPredPromoted, pTinBool))
							{
								CString strTinPred = StrFromTypeInfo(pTinPredPromoted);
								EmitError(pTcwork, pStnod, "No conversion between %s and bool", strTinPred.PCoz());
							}

							pStnod->m_pTin = pTinBool;
							FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinBool, pStnodPred);

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_Else:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_Return:
						{
							if (pTcsentTop->m_nState >= pStnod->CStnodChild())
							{
								CSTNode * pStnodProc = pTcsentTop->m_pStnodProcedure;
								if (!pStnodProc)
								{
									EmitError(pTcwork, pStnod, "Return statement encountered outside of a procedure");
									return TCRET_StoppingError;
								}

								STypeInfo * pTinReturn = PTinReturnFromStnodProcedure(pStnodProc);
								if (!EWC_FVERIFY(pTinReturn, "expected return type (implicit void should be added by now"))
								{
									return TCRET_StoppingError;
								}

								if (pStnod->CStnodChild() == 0)
								{
									if (pTinReturn->m_tink != TINK_Void)
									{
										EmitError(pTcwork, pStnod, "non void return type expected.");
										return TCRET_StoppingError;
									}
									pStnod->m_pTin = pTinReturn;
								}
								else if (pStnod->CStnodChild() == 1)
								{
									CSTNode * pStnodRhs = pStnod->PStnodChild(0);
									STypeInfo * pTinRhs = pStnodRhs->m_pTin;
									STypeInfo * pTinRhsPromoted = PTinPromoteUntypedTightest(
																	pTcwork,
																	pTcsentTop->m_pSymtab,
																	pStnodRhs,
																	pTinReturn);
									if (FCanImplicitCast(pTinRhsPromoted, pTinReturn))
									{
										pStnod->m_pTin = pTinReturn;
										FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinReturn, pStnodRhs);
									}
									else
									{
										CString strLhs = StrFromTypeInfo(pTinReturn);
										CString strRhs = StrFromTypeInfo(pTinRhs);
										EmitError( pTcwork, pStnod,
											"implicit cast from %s to %s is not allowed by return statement",
											strRhs.PCoz(),
											strLhs.PCoz());
									}
								}
								else
								{
									EWC_ASSERT(false, "multiple return types not supported (yet).");
								}

								pStnod->m_strees = STREES_TypeChecked;
								PopTcsent(pTcfram, &pTcsentTop, pStnod);
								break;
							}
							PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
						}break;
					default:
						EmitError(pTcwork, pStnod, "unhandled reserved word '%s' in type checker", PCozFromRword(rword));
						return TCRET_StoppingError;
					}
				}
			}break;
			case PARK_AdditiveOp:
			case PARK_MultiplicativeOp:
			case PARK_ShiftOp:
			case PARK_BitwiseAndOrOp:
			case PARK_RelationalOp:
			case PARK_EqualityOp:
			case PARK_LogicalAndOrOp:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to binary ops"))
					{
						CSTNode * pStnodLhs = pStnod->PStnodChild(0);
						CSTNode * pStnodRhs = pStnod->PStnodChild(1);
						STypeInfo * pTinLhs = pStnodLhs->m_pTin;
						STypeInfo * pTinRhs = pStnodRhs->m_pTin;

						if (!FVerifyIsInstance(pTcwork, pStnodLhs) || !FVerifyIsInstance(pTcwork, pStnodRhs))
							return TCRET_StoppingError;

						if (EWC_FVERIFY((pTinLhs != nullptr) & (pTinRhs != nullptr), "unknown type in binary operation"))
						{
							CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
							if ((pTinLhs->m_tink == TINK_Literal) & (pTinRhs->m_tink == TINK_Literal))
							{
								// this needs to be explicitly handled, create a new literal with the result
								if (EWC_FVERIFY(
										pStnod->m_pTin == nullptr, 
										"STypeInfoLiteral should be constructed during type checking"))
								{
									// NOTE: this is only finding the type info for the result of our binary op
									//  it will be used for type inference from a literal, but this doesn't collapse
									//  the operator into a constant. (yet)

									STypeInfoLiteral * pTinReturn;
									STypeInfoLiteral * pTinOperand;
									CSTValue * pStval;
									if (FComputeBinaryOpOnLiterals(
											pTcwork,
											pStnod,
											pSymtab,
											pStnodLhs,
											pStnodRhs,
											&pTinOperand,
											&pTinReturn,
											&pStval))
									{
										pStnod->m_pTin = pTinReturn;

										AllocateOptype(pStnod);
										*pStnod->m_pOptype = SOpTypes(pTinOperand, pTinOperand, pTinReturn);
										pStnod->m_pStval = pStval;
									}
									else
									{
										EmitError(
											pTcwork, 
											pStnod, 
											"invalid operation %s for %s literal and %s literal", 
											PCozFromJtok(pStnod->m_jtok),
											PChzFromLitk(((STypeInfoLiteral *)pTinLhs)->m_litty.m_litk),
											PChzFromLitk(((STypeInfoLiteral *)pTinRhs)->m_litty.m_litk));
										return TCRET_StoppingError;
									}
								}
							}
							else
							{
								PARK park = pStnod->m_park;
								STypeInfo * pTinUpcastLhs = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodLhs, pTinRhs);
								STypeInfo * pTinUpcastRhs = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodRhs, pTinLhs);

								SOpTypes optype = OptypeFromPark(pTcwork, pSymtab, pStnod->m_jtok, park, pTinUpcastLhs, pTinUpcastRhs);

								if (!optype.FIsValid() || !FDoesOperatorExist(pStnod->m_jtok, &optype))
								{
									CString strLhs = StrFromTypeInfo(pTinLhs);
									CString strRhs = StrFromTypeInfo(pTinRhs);
									EmitError(
										pTcwork,
										pStnod,
										"%s operator not defined for %s and %s",
										PCozFromJtok(pStnod->m_jtok),
										strLhs.PCoz(),
										strRhs.PCoz());
									return TCRET_StoppingError;
								}

								AllocateOptype(pStnod);
								*pStnod->m_pOptype = optype;
								pStnod->m_pTin = optype.m_pTinResult;

								FinalizeLiteralType(pTcsentTop->m_pSymtab, optype.m_pTinLhs, pStnodLhs);
								FinalizeLiteralType(pTcsentTop->m_pSymtab, optype.m_pTinRhs, pStnodRhs);
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
		case PARK_PostfixUnaryOp:
		case PARK_UnaryOp:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (EWC_FVERIFY(pStnod->CStnodChild() == 1, "expected one operand to unary operations"))
					{
						CSTNode * pStnodOperand = pStnod->PStnodChild(0);
						STypeInfo * pTinOperand = pStnodOperand->m_pTin;

						AllocateOptype(pStnod);
						*pStnod->m_pOptype = SOpTypes(pTinOperand, pTinOperand, pTinOperand);

						if (!FVerifyIsInstance(pTcwork, pStnodOperand))
							return TCRET_StoppingError;

						if (EWC_FVERIFY(pTinOperand != nullptr, "unknown type in unary operation"))
						{
							if ((pTinOperand->m_tink == TINK_Literal))
							{
								// this needs to be explicitly handled, create a new literal with the result
								if (EWC_FVERIFY(
										pStnod->m_pTin == nullptr, 
										"STypeInfoLiteral should be constructed during type checking"))
								{
									// NOTE: This computes the proper value and type, but will not collapse the AST
									//  The codegen pass will stop recursing when it gets to this finalized literal type

									CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
									STypeInfoLiteral * pTinReturn;
									STypeInfoLiteral * pTinlitOperand = nullptr;
									CSTValue * pStval;
									if (FComputeUnaryOpOnLiteral(
											pTcwork,
											pStnod,
											pSymtab,
											pStnodOperand,
											&pTinlitOperand,
											&pTinReturn,
											&pStval))
									{
										pStnod->m_pTin = pTinReturn;
										pStnod->m_pStval = pStval;
									}
									else
									{
										EmitError(
											pTcwork, 
											pStnod, 
											"invalid unary operand %s for %s literal", 
											PCozFromJtok(pStnod->m_jtok),
											PChzFromLitk(((STypeInfoLiteral *)pTinOperand)->m_litty.m_litk));
										return TCRET_StoppingError;
									}
								}
							}
							else
							{

								JTOK jtok = pStnod->m_jtok;
								switch (jtok)
								{
								case JTOK_Dereference:
									{
										if (pTinOperand->m_tink != TINK_Pointer)
										{
											CString strOp = StrFromTypeInfo(pTinOperand);
											EmitError(pTcwork, pStnod, "Cannot dereference type %s", strOp.PCoz());
											return TCRET_StoppingError;
										}
										else
										{
											STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTinOperand;
											pStnod->m_pTin = pTinptr->m_pTinPointedTo;
										}
									}break;
								case JTOK_Reference:
									{
										// NOTE: TINK cannot be a literal - handled above...
										// NOTE: Can take a reference if we have a symbol that is not an enum or procedure
										//  definition, but need to walk past member lookups 
										
										// Need a better method for this - this fails in lots of different ways

										bool fCanTakeReference = false;
										auto pStnodMember = pStnodOperand;

										while (1)
										{
											if (!EWC_FVERIFY(pStnodMember, "bad member lookup child"))
												break;

											if (pStnodMember->m_park == PARK_MemberLookup)
												pStnodMember = pStnodMember->PStnodChildSafe(1);
											else if (pStnodMember->m_park == PARK_ArrayElement)
												pStnodMember = pStnodMember->PStnodChildSafe(0);
											else if (pStnodMember->m_park == PARK_Cast)
											{
												auto * pStdecl = pStnodMember->m_pStdecl;
												pStnodMember = pStnodMember->PStnodChild(pStdecl->m_iStnodInit);
											}
											else
												break;
										}

										if (pStnodMember->m_pSym && pStnodMember->m_pSym->m_pStnodDefinition)
										{
											PARK parkDefinition = pStnodMember->m_pSym->m_pStnodDefinition->m_park;
											fCanTakeReference = (parkDefinition != PARK_ProcedureDefinition) | 
																(parkDefinition != PARK_EnumConstant);
										}

										if (!fCanTakeReference)
										{
											CString strOp = StrFromTypeInfo(pTinOperand);
											EmitError(pTcwork, pStnod, "Cannot take reference of constant %s", strOp.PCoz());
											return TCRET_StoppingError;
										}

										CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
										pStnod->m_pTin = pSymtab->PTinptrAllocReference(pTinOperand);
									}break;

								case JTOK('!'):
									{
										STypeInfo * pTinBool = pTcsentTop->m_pSymtab->PTinBuiltin("bool");
										if (!EWC_FVERIFY(pTinBool, "missing bool type"))
											return TCRET_StoppingError;
										if (!FCanImplicitCast(pTinOperand, pTinBool))
										{
											CString strOp = StrFromTypeInfo(pTinOperand);
											EmitError(pTcwork, pStnod, "Cannot convert type %s to bool", strOp.PCoz());
										}

										pStnod->m_pTin = pTinBool;
										pStnod->m_pOptype->m_pTinResult = pTinBool;
									}break;

								case JTOK('~'):
								case JTOK_PlusPlus:
								case JTOK_MinusMinus:
								case JTOK('+'):
								case JTOK('-'):
									{
										TINK tinkOperand = pTinOperand->m_tink;
										bool fIsInteger = tinkOperand == TINK_Integer;
										bool fIsFloat = tinkOperand == TINK_Float;
										bool fIsEnum = tinkOperand == TINK_Enum;
										if (tinkOperand == TINK_Literal && pStnodOperand->m_pStval)
										{
											LITK litk = ((STypeInfoLiteral *)pTinOperand)->m_litty.m_litk;
											fIsInteger |= litk == LITK_Integer;
											fIsFloat |= litk == LITK_Float;
											fIsEnum |= litk == LITK_Enum;
										}

										bool fIsValidPtrOp = ((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) &
											(tinkOperand == TINK_Pointer);
										bool fIsValidFloatOp = ((jtok == JTOK('+')) | (jtok == JTOK('-')) | (jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) & 
																fIsFloat;
										bool fIsValidEnumOp = ((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) & fIsEnum;
										bool fIsSupported = fIsInteger | fIsValidPtrOp | fIsValidFloatOp | fIsValidEnumOp;

										// BB - we should be checking for negating a signed literal here, but we can't really
										//  do operations on literals until we know the resolved type
										//  (Otherwise ~1 will always resolve to a u64)

										pStnod->m_pTin = pTinOperand;
										if (!fIsSupported)
										{
											CString strOp = StrFromTypeInfo(pTinOperand);
											EmitError(pTcwork, pStnod, "invalid unary operator for type %s", strOp.PCoz());
										}
										else
										{
											if (jtok == JTOK('-') && tinkOperand == TINK_Integer)
											{
												STypeInfoInteger * pTinint = (STypeInfoInteger *)pTinOperand;
												if (!pTinint->m_fIsSigned)
												{
													CString strOp = StrFromTypeInfo(pTinOperand);
													EmitError(
														pTcwork,
														pStnod,
														"negate operand not valid for unsigned type %s",
														strOp.PCoz());
												}
											}
										}
									}break;
								}
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				PushTcsent(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
		default:
			EWC_ASSERT(false, "unknown parse kind (%s) encountered during type check", PChzFromPark(pStnod->m_park));
			break;
		}
	}

	return TCRET_Complete;
}

void ValidateTcframArray(CAry<STypeCheckFrame *> * parypTcfram)
{
	for (int ipTcfram = 0; ipTcfram < (int)parypTcfram->C(); ++ipTcfram)
	{
		EWC_ASSERT((*parypTcfram)[ipTcfram]->m_ipTcframQueue == ipTcfram, "invalid type check frame array");
	}
}

void RelocateTcfram(
	STypeCheckFrame * pTcfram,
	CAry<STypeCheckFrame *> * parypTcframOld,
	CAry<STypeCheckFrame *> * parypTcframNew)
{
	EWC_ASSERT((*parypTcframOld)[pTcfram->m_ipTcframQueue] == pTcfram, "bookkeeping error");

	s32 cOld = (s32)parypTcframOld->C() - 1; 
	int ipTcfram = pTcfram->m_ipTcframQueue;
	if (cOld != ipTcfram)
	{
		STypeCheckFrame * pTcframTop = (*parypTcframOld)[cOld];
		pTcframTop->m_ipTcframQueue = ipTcfram;
		(*parypTcframOld)[ipTcfram] = pTcframTop;
	}

	parypTcframOld->PopLast();

	if (parypTcframNew)
	{
		pTcfram->m_ipTcframQueue = (s32)parypTcframNew->C();
		parypTcframNew->Append(pTcfram);
	}
}

void OnTypeComplete(STypeCheckWorkspace * pTcwork, const SSymbol * pSym)
{
	EWC_ASSERT(pSym->m_pTin, "expected type for completed symbol");

	// BB - could replace this with a function that 'pops' a key/value pair from the hash and save a second lookup.
	SUnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
		return;

	int cTcframDependent = (s32)pUntype->m_aryiTcframDependent.C();
	EWC_ASSERT(cTcframDependent > 0, "unknown type not cleaned up (empty dependent array)");

	for (int iTcfram = 0; iTcfram < cTcframDependent; ++iTcfram)
	{
		STypeCheckFrame & tcfram = pTcwork->m_aryTcfram[pUntype->m_aryiTcframDependent[iTcfram]];

		if (EWC_FVERIFY(pTcwork->m_arypTcframWaiting[tcfram.m_ipTcframQueue] == &tcfram, "bookkeeping error (onTypeComplete)"))
		{
			RelocateTcfram(&tcfram, &pTcwork->m_arypTcframWaiting, &pTcwork->m_arypTcframPending);
		}
	}
	pTcwork->m_hashPSymUntype.Remove(pSym);
}

STVALK StvalkFromTin(STypeInfo * pTin)
{
	if (pTin->m_tink == TINK_Literal)
	{
		STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTin;
		switch (pTinlit->m_litty.m_litk)
		{
		case LITK_Integer:	return (pTinlit->m_litty.m_fIsSigned) ? STVALK_SignedInt : STVALK_UnsignedInt;
		case LITK_Float:	return STVALK_Float;
		case LITK_Char:		return STVALK_String;
		case LITK_String:	return STVALK_String;
		case LITK_Bool:		return STVALK_UnsignedInt;
		case LITK_Null:		return STVALK_Nil;
		}
	}
	return STVALK_Nil;
}

PARK ParkDefinition(STypeCheckWorkspace * pTcwork, const SSymbol * pSym)
{
	if (EWC_FVERIFY(pSym->m_pStnodDefinition, "symbol without definition"))
	{
		PARK parkDef = pSym->m_pStnodDefinition->m_park;
		switch (parkDef)
		{
		case PARK_ProcedureDefinition:	return parkDef;
		case PARK_Decl:					return parkDef; 
		case PARK_StructDefinition:		return PARK_Decl; // instances and types shadow each other
		default: 
			EmitError(pTcwork, pSym->m_pStnodDefinition, "Unexpected PARK %s for symbol definition", PChzFromPark(parkDef));
		}
	}
	return PARK_Nil;
}

void PerformTypeCheck(
	CAlloc * pAlloc,
	SErrorManager * pErrman,
	CSymbolTable * pSymtabTop,
	CAry<CWorkspace::SEntry> * paryEntry,
	CAry<int> * paryiEntryChecked)
{
	auto pTcwork = EWC_NEW(pAlloc, STypeCheckWorkspace) STypeCheckWorkspace(pAlloc, pErrman, (s32)paryEntry->C());

	CWorkspace::SEntry * pEntryMax = paryEntry->PMac();
	int ipTcfram = 0;
	for (CWorkspace::SEntry * pEntry = paryEntry->A(); pEntry != pEntryMax; ++pEntry, ++ipTcfram)
	{
		EWC_ASSERT(pEntry->m_pSymtab, "entry point without symbol table");
		STypeCheckFrame * pTcfram = pTcwork->m_aryTcfram.AppendNew();
		pTcfram->m_ipTcframQueue = ipTcfram;

		pTcfram->m_aryTcsent.SetAlloc(pAlloc, EWC::BK_TypeCheck);
		STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
		pTcsent->m_nState = 0;
		pTcsent->m_pStnod = pEntry->m_pStnod;
		pTcsent->m_pSymtab = pEntry->m_pSymtab;
		pTcsent->m_pStnodProcedure = nullptr;
		pTcsent->m_grfsymlook = FSYMLOOK_Default;
		pTcsent->m_fAllowForwardDecl = false;
		pTcsent->m_tcctx = TCCTX_Normal;

		pTcwork->m_arypTcframPending.Append(pTcfram);
	}
	
	while (pTcwork->m_arypTcframPending.C())
	{
		STypeCheckFrame * pTcfram = pTcwork->m_arypTcframPending[0];
		TCRET tcret = TcretTypeCheckSubtree(pTcwork, pTcfram);

		if (tcret == TCRET_StoppingError)
		{
			// make sure we've reported at least one error.
			if (pErrman->m_cError == 0)
			{
				SLexerLocation lexloc;
				EmitError(pErrman, &lexloc, "Unknown error in type checker, quitting.");
			}

			while (pTcfram->m_aryTcsent.C())
			{
				STypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();
				PopTcsent(pTcfram, &pTcsentTop, nullptr);
			}

			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);
		}
		else if (tcret == TCRET_Complete)
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);

			size_t iEntry = pTcwork->m_aryTcfram.IFromP(pTcfram);
			paryiEntryChecked->Append(S32Coerce(iEntry));
		}
		else if (EWC_FVERIFY(tcret == TCRET_WaitingForSymbolDefinition))
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, &pTcwork->m_arypTcframWaiting);
		}

		ValidateTcframArray(&pTcwork->m_arypTcframPending);
		ValidateTcframArray(&pTcwork->m_arypTcframWaiting);
	}

	CHash<const SSymbol *, SUnknownType>::CIterator iter(&pTcwork->m_hashPSymUntype);

	const SSymbol ** ppSym;
	while (SUnknownType * pUntype = iter.Next(&ppSym))
	{
		EWC_ASSERT(pUntype->m_aryiTcframDependent.C() > 0, "unknown type not cleaned up (empty dependent array)");
		for (size_t iTcfram = 0; iTcfram < pUntype->m_aryiTcframDependent.C(); ++iTcfram)
		{
			// Note: we're assuming the top thing on the stack is the thing we're waiting for.
			const STypeCheckFrame & tcfram = pTcwork->m_aryTcfram[pUntype->m_aryiTcframDependent[iTcfram]];
			const STypeCheckStackEntry * pTcsent = tcfram.m_aryTcsent.PLast();

			EmitError(pTcwork, pTcsent->m_pStnod, "Unresolved type '%s' reference found here", (*ppSym)->m_strName.PCoz());
		}
	}

	//PerformFlushResolvedLiteralsPass(pTcwork, paryEntry);

	//check for top level collisions

	{
		EWC::CHash<HV, SSymbol *>::CIterator iterSym(&pSymtabTop->m_hashHvPSym);
		SSymbol ** ppSym;
		while (ppSym = iterSym.Next())
		{
			auto pSym = *ppSym;
			for (auto pSymSrc = pSym; pSymSrc; pSymSrc = pSymSrc->m_pSymPrev)
			{
				for (auto pSymDst = pSymSrc->m_pSymPrev; pSymDst; pSymDst = pSymDst->m_pSymPrev)
				{
					PARK parkSrc = ParkDefinition(pTcwork, pSymSrc);
					PARK parkDst = ParkDefinition(pTcwork, pSymDst);

					if (parkSrc != parkDst)
						continue;

					if (parkSrc == PARK_ProcedureDefinition)
					{
						if (!FTypesAreSame(pSymSrc->m_pTin, pSymDst->m_pTin))
							continue;
					}

					if (pSymDst->m_pStnodDefinition)
					{
						auto pLexlocDst = &pSymDst->m_pStnodDefinition->m_lexloc;

						s32 iLineDst;
						s32 iColDst;
						CalculateLinePosition(pErrman->m_pWork, pLexlocDst, &iLineDst, &iColDst);

						EmitError(
							pTcwork,
							pSym->m_pStnodDefinition, 
							"Top level symbol '%s' is also defined here %s(%d,%d)",
							pSym->m_strName.PCoz(),
							pLexlocDst->m_strFilename.PCoz(),
							iLineDst,
							iColDst);
					}
				}
			}
		}
	}

	pAlloc->EWC_DELETE(pTcwork);
}

void AssertEquals(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	bool fIsAbsSame = bintLhs.m_nAbs == bintRhs.m_nAbs;
	bool fIsSignSame = bintLhs.m_fIsNegative == bintRhs.m_fIsNegative;
	fIsSignSame |= (fIsAbsSame & (bintLhs.m_nAbs == 0));
	EWC_ASSERT(fIsAbsSame & fIsSignSame, 
		"expected %s%llu but calculated %s%llu",
		(bintLhs.m_fIsNegative) ? "-" : "", bintLhs.m_nAbs,
		(bintRhs.m_fIsNegative) ? "-" : "", bintRhs.m_nAbs);
}

void AssertTestSigned65()
{
	s64 s_aNSigned[] = { 0, 1, -1, 400000000, -400000000 }; //, LLONG_MAX, LLONG_MIN + 1

	// NOTE: This does not replicate s64 overflow exactly, Signed65 structs overflow like a u64 but maintaining sign 
	// values. It's not clear that this is the wrong behavior for literals... it should probably throw an overflow
	// error (?)

	s64 nTest = -1 >> 1;
	AssertEquals(BintShiftRight(BintFromInt(-1), BintFromInt(1)), BintFromInt(-1 >> 1));
	AssertEquals(BintSub(BintFromInt(0), BintFromInt(LLONG_MIN+1)), BintFromInt(LLONG_MAX));

	for (int iNLhs = 0; iNLhs < EWC_DIM(s_aNSigned); ++iNLhs)
	{
		for (int iNRhs = 0; iNRhs < EWC_DIM(s_aNSigned); ++iNRhs)
		{
			s64 nLhs = s_aNSigned[iNLhs];
			s64 nRhs = s_aNSigned[iNRhs];
			AssertEquals(BintAdd(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs + nRhs));
			AssertEquals(BintSub(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs - nRhs));
			AssertEquals(BintMul(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs * nRhs));

			AssertEquals(BintBitwiseOr(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs | nRhs));
			AssertEquals(BintBitwiseAnd(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs & nRhs));

			if (nRhs >= 0)
			{
				// shifting by more than the number of bits in a type results in undefined behavior
				auto nRhsClamp = (nRhs > 31) ? 31 : nRhs;
				auto bintRhsClamp = BintFromInt(nRhsClamp);
			
				AssertEquals(BintShiftRight(BintFromInt(nLhs), BintFromInt(nRhsClamp)), BintFromInt(nLhs >> nRhsClamp));
				AssertEquals(BintShiftLeft(BintFromInt(nLhs), BintFromInt(nRhsClamp)), BintFromInt(nLhs << nRhsClamp));
			}

			if (nRhs != 0)
			{
				AssertEquals(BintDiv(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs / nRhs));
				AssertEquals(BintRemainder(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs % nRhs));
			}
		}
	}

	
	u64 s_aNUnsigned[] = { 0, 1, 400000000, ULLONG_MAX };

	for (int iNLhs = 0; iNLhs < EWC_DIM(s_aNSigned); ++iNLhs)
	{
		for (int iNRhs = 0; iNRhs < EWC_DIM(s_aNSigned); ++iNRhs)
		{
			u64 nLhs = s_aNUnsigned[iNLhs];
			u64 nRhs = s_aNUnsigned[iNRhs];
			AssertEquals(BintAdd(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs + nRhs));

			AssertEquals(BintBitwiseOr(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs | nRhs));
			AssertEquals(BintBitwiseAnd(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs & nRhs));

			if (nRhs >= 0)
			{
				// shifting by more than the number of bits in a type results in undefined behavior
				auto nRhsClamp = (nRhs > 31) ? 31 : nRhs;
				auto bintRhsClamp = BintFromInt(nRhsClamp);
			
				AssertEquals(BintShiftRight(BintFromUint(nLhs), BintFromUint(nRhsClamp)), BintFromUint(nLhs >> nRhsClamp));
				AssertEquals(BintShiftLeft(BintFromUint(nLhs), BintFromUint(nRhsClamp)), BintFromUint(nLhs << nRhsClamp));
			}

			// does not replicate unsigned underflow, because the sign bit is tracked seperately
			if (nLhs >= nRhs)
				AssertEquals(BintSub(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs - nRhs));

			AssertEquals(BintMul(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs * nRhs));
			if (nRhs != 0)
			{
				AssertEquals(BintDiv(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs / nRhs));
				AssertEquals(BintRemainder(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs % nRhs));
			}
		}
	}

	AssertEquals(BintAdd(BintFromUint(ULLONG_MAX, true), BintFromUint(100)), BintFromUint(ULLONG_MAX - 100, true));
	AssertEquals(BintAdd(BintFromUint(ULLONG_MAX, true), BintFromUint(ULLONG_MAX)), BintFromUint(0));
	AssertEquals(BintSub(BintFromUint(100), BintFromUint(ULLONG_MAX)), BintFromUint(ULLONG_MAX - 100, true));

}

void AssertTestTypeCheck(
	CWorkspace * pWork,
	const char * pCozIn,
	const char * pCozExpected)
{
	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pCozIn);

	EWC_ASSERT(pWork->m_pErrman->m_cError == 0, "parse errors detected");
	pWork->m_pErrman->Clear();

	ParseGlobalScope(pWork, &jlex, true);
	EWC_ASSERT(pWork->m_aryEntry.C() > 0);

	EndParse(pWork, &jlex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pErrman, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	WriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type|FDBGSTR_LiteralSize);

#if EWC_X64
	const char * pChzWord = "64";
#else
	const char * pChzWord = "32";
#endif

	char aChExpected[1024];
	int cCh = CCh(pCozExpected);
	for (int iCh = 0; iCh < cCh+1; ++iCh)
	{
		if (iCh < cCh - 1 && pCozExpected[iCh] == '#' && pCozExpected[iCh + 1] == '#')
		{
			aChExpected[iCh] = pChzWord[0];
			++iCh;
			aChExpected[iCh] = pChzWord[1];
		}
		else
		{
			aChExpected[iCh] = pCozExpected[iCh];
		}
	}

	EWC_ASSERT(FAreCozEqual(aCh, aChExpected), "type check debug string doesn't match expected value");

	EndWorkspace(pWork);
}

void TestTypeCheck()
{		
	AssertTestSigned65();

	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);

	const char * pCozIn;
	const char * pCozOut;

	pCozIn = "pN: &s16; cB := pN - pN; pN = pN + 2; pN += 1;";
	pCozOut = "(&s16 $pN (&s16 s16)) (sSize $cB (sSize &s16 &s16)) (= &s16 (&s16 &s16 Literal:Int8)) (= &s16 Literal:Int64)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "iterMake :: (n: u8) -> u8 { return n; } "
			 "iterIsDone :: (pN: & u8) -> bool { return false; } "
			 "iterNext :: (pN: & u8) { } "
			"it: u8; for it = iterMake(2) { }";
	pCozOut = "(iterMake(u8)->u8 $iterMake (Params (u8 $n u8)) u8 ({} (u8 u8))) "
				"(iterIsDone(&u8)->bool $iterIsDone (Params (&u8 $pN (&u8 u8))) bool ({} (bool Literal:Bool8))) "
				"(iterNext(&u8)->void $iterNext (Params (&u8 $pN (&u8 u8))) void ({} (void))) "
				"(u8 $it u8) (??? u8 (u8 iterMake(u8)->u8 Literal:Int8) (bool iterIsDone(&u8)->bool (&u8 u8)) (void iterNext(&u8)->void (&u8 u8)) ({}))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "iterMake :: (n: u8) -> u8 { return n; } "
			 "iterIsDone :: (pN: & u8) -> bool { return false; } "
			 "iterNext :: (pN: & u8) { } "
			"for it := iterMake(2) { }";
	pCozOut = "(iterMake(u8)->u8 $iterMake (Params (u8 $n u8)) u8 ({} (u8 u8))) "
				"(iterIsDone(&u8)->bool $iterIsDone (Params (&u8 $pN (&u8 u8))) bool ({} (bool Literal:Bool8))) "
				"(iterNext(&u8)->void $iterNext (Params (&u8 $pN (&u8 u8))) void ({} (void))) "
				"(??? (u8 $it (u8 iterMake(u8)->u8 Literal:Int8)) (bool iterIsDone(&u8)->bool (&u8 u8)) (void iterNext(&u8)->void (&u8 u8)) ({}))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "SFunc :: () { { n:=5; n=2; } }";
	pCozOut = "(SFunc()->void $SFunc void ({} ({} (int $n Literal:Int##) (= int Literal:Int##)) (void)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ n:s32=0xFFFFFFFF; }";
	pCozOut = "({} (s32 $n s32 Literal:Int32))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ ch:= 'a'; }";
	pCozOut = "({} (char $ch Literal:Int32))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ n1: int, g1, g2: float = ---; }";
	pCozOut = "({} (??? (int $n1 int) (float $g1 float) (float $g2 float) (---)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ n1, n2: int, g: float; }";
	pCozOut = "({} (??? (int $n1 int) (int $n2 int) (float $g float)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= " SelfRef :: () { pfunc := SelfRef; }";
	pCozOut		= "(SelfRef()->void $SelfRef void ({} (SelfRef()->void $pfunc SelfRef()->void) (void)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= " { ppfunc: & (n: s32)->s32; }";
	pCozOut		= "({} (&(s32)->s32 $ppfunc (&(s32)->s32 ((s32)->s32 (Params (s32 $n s32)) s32))))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= " { n:int=2; Foo :: ()->int { n2:=n; g:=Bar(); return 1;}    Bar :: ()->float { n:=Foo(); return 1;} }";
	pCozOut		=	"(Foo()->int $Foo int ({} (int $n2 int) (float $g (float Bar()->float)) (int Literal:Int##)))"
					" (Bar()->float $Bar float ({} (int $n (int Foo()->int)) (float Literal:Float32)))"
					" ({} (int $n int Literal:Int##))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "fooFunc :: (n: s32) -> s64 { return 2; }     func: (n: s32)->s64 = fooFunc;";		// procedure reference declaration
	pCozOut = "(fooFunc(s32)->s64 $fooFunc (Params (s32 $n s32)) s64 ({} (s64 Literal:Int64))) "
			  "((s32)->s64 $func ((s32)->s64 (Params (s32 $n s32)) s64) fooFunc(s32)->s64)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "pFunc: (n: s32)->s64;   pFunc(33);";
	pCozOut = "((s32)->s64 $pFunc ((s32)->s64 (Params (s32 $n s32)) s64)) (s64 (s32)->s64 Literal:Int32)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "pG : & float = null; pN := cast(& int) pG;";
	pCozOut = "(&float $pG (&float float) Literal:Null) (&int $pN (&int (&int int) &float))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "aN := {:s32: 2, 4, 5};";
	pCozOut = "([3]s32 $aN (Literal:Array s32 ({} Literal:Int32 Literal:Int32 Literal:Int32)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "aN : [] int = {2, 4, 5};";
	pCozOut = "([]int $aN ([]int int) (Literal:Array ({} Literal:Int## Literal:Int## Literal:Int##)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "aN : [] & u8 = {\"foo\", \"bar\", \"ack\"};";
	pCozOut = "([]&u8 $aN ([]&u8 (&u8 u8)) (Literal:Array ({} Literal:String Literal:String Literal:String)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "ArrayConst :: {2, 4, 5};";
	pCozOut = "(Literal:Array $ArrayConst (Literal:Array ({} Literal:Int Literal:Int Literal:Int)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ aN : [2] int; n := aN.count; }";
	pCozOut = "({} ([2]int $aN ([2]int Literal:Int## int)) (int $n (Literal:Int## [2]int $count)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ aN : [2] int; aNUnsized : [] int = aN; }";
	pCozOut = "({} ([2]int $aN ([2]int Literal:Int## int)) ([]int $aNUnsized ([]int int) [2]int))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ ENUMK :: enum s32 { Ick : 1, Foo, Bah : 3 } enumk := ENUMK.Bah;}";
	pCozOut = "({} (ENUMK_enum $ENUMK s32 ({} (Literal:Enum $nil) (Literal:Enum $min) (Literal:Enum $last) (Literal:Enum $max) "
		"(Literal:Array $names (Literal:Array Literal:String Literal:String Literal:String)) (Literal:Array $values (Literal:Array Literal:Int32 Literal:Int32 Literal:Int32)) "
		"(Literal:Enum $Ick Literal:Int) (Literal:Enum $Foo) (Literal:Enum $Bah Literal:Int))) "
		"(ENUMK_enum $enumk (Literal:Int32 ENUMK_enum $Bah)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ EEK :: enum s16 { Ick : 1 } eek : EEK.strict = EEK.Ick; n : EEK.loose = EEK.Ick; }";
	pCozOut = "({} (EEK_enum $EEK s16 ({} (Literal:Enum $nil) (Literal:Enum $min) (Literal:Enum $last) (Literal:Enum $max) "
		"(Literal:Array $names (Literal:Array Literal:String)) (Literal:Array $values (Literal:Array Literal:Int16)) (Literal:Enum $Ick Literal:Int))) "
		"(EEK_enum $eek (EEK_enum EEK_enum EEK_enum) (Literal:Int16 EEK_enum $Ick)) "
		"(s16 $n (s16 EEK_enum s16) (Literal:Int16 EEK_enum $Ick)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "SomeConst : s16 : 0xFF; n := SomeConst;";
	pCozOut = "(Literal:Int16 $SomeConst s16 Literal:Int16) (s16 $n Literal:Int16)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "SomeConst :: 0xFF; n : s16 = SomeConst; n2 := SomeConst;";
	pCozOut = "(Literal:Int $SomeConst Literal:Int) (s16 $n s16 Literal:Int16) (int $n2 Literal:Int##)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "pUgh : & SUgh; pUgh.m_foo.m_n = 1; SUgh :: struct { m_foo : SFoo; } SFoo :: struct { m_n : s8; }";
	pCozOut = "(&SUgh_struct $pUgh (&SUgh_struct SUgh_struct)) (= (s8 (SFoo_struct &SUgh_struct $m_foo) $m_n) Literal:Int8) "
		"(SUgh_struct $SUgh ({} (SFoo_struct $m_foo SFoo_struct))) "
		"(SFoo_struct $SFoo ({} (s8 $m_n s8)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "pFoo : & SFoo; pFoo.m_n = 1; SFoo :: struct { m_n : s8; }";
	pCozOut = "(&SFoo_struct $pFoo (&SFoo_struct SFoo_struct)) (= (s8 &SFoo_struct $m_n) Literal:Int8) (SFoo_struct $SFoo ({} (s8 $m_n s8)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "foo : SFoo; foo.m_n = 1; SFoo :: struct { m_n : s8; }";
	pCozOut = "(SFoo_struct $foo SFoo_struct) (= (s8 SFoo_struct $m_n) Literal:Int8) (SFoo_struct $SFoo ({} (s8 $m_n s8)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "SFoo :: struct { m_n : s32; m_g := 1.2; } foo : SFoo;";
	pCozOut = "(SFoo_struct $SFoo ({} (s32 $m_n s32) (float $m_g Literal:Float32))) (SFoo_struct $foo SFoo_struct)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "aN : [4] s32; pN : & s32; fEq := aN.data == pN; ";
	pCozOut = "([4]s32 $aN ([4]s32 Literal:Int## s32)) (&s32 $pN (&s32 s32)) (bool $fEq (bool (&s32 [4]s32 $data) &s32))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "aN : [4] s32; paN : & [4] s32 = &aN;";
	pCozOut = "([4]s32 $aN ([4]s32 Literal:Int## s32)) (&[4]s32 $paN (&[4]s32 ([4]s32 Literal:Int## s32)) (&[4]s32 [4]s32))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "aN : [4] s32; n := aN[0];";
	pCozOut = "([4]s32 $aN ([4]s32 Literal:Int## s32)) (s32 $n (s32 [4]s32 Literal:Int##))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "n:s32=2; n++; n--;";
	pCozOut ="(s32 $n s32 Literal:Int32) (s32 s32) (s32 s32)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ ; n : s32 = ---;}";
	pCozOut ="({} (Nop) (s32 $n s32 (---)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "printf :: (pCh : & u8, ..) -> s32 #foreign;";
	pCozOut ="(printf(&u8, ..)->s32 $printf (Params (&u8 $pCh (&u8 u8)) (..)) s32)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "Vararg :: (..) #foreign; Vararg(2.2, 8,2000);";
	pCozOut ="(Vararg(..)->void $Vararg (Params (..)) void) (void Vararg(..)->void Literal:Float64 Literal:Int## Literal:Int##)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "n:=7; pN:=&n; ppN:=&pN; pN2:=@ppN; n2:=@pN2;";
	pCozOut ="(int $n Literal:Int##) (&int $pN (&int int)) (&&int $ppN (&&int &int)) "
				"(&int $pN2 (&int &&int)) (int $n2 (int &int))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "pN : & int; @pN = 2;";
	pCozOut ="(&int $pN (&int int)) (= (int &int) Literal:Int##)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "pChz:&u8=\"teststring\"; ";
	pCozOut ="(&u8 $pChz (&u8 u8) Literal:String)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "a:=2-2; n:=-2;";
	pCozOut ="(int $a (Literal:Int## Literal:Int Literal:Int)) (int $n (Literal:Int## Literal:Int))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= "foo :: (n : bool) #foreign; n:s16; ack :: ( n : s32) { test := n; n : s64; test2 := n; }"; 
	pCozOut		= "(foo(bool)->void $foo (Params (bool $n bool)) void) (s16 $n s16) "
					"(ack(s32)->void $ack (Params (s32 $n s32)) void ({} (s32 $test s32) (s64 $n s64) (s64 $test2 s64) (void)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:=5; foo:=i; g:=g_g; } g_g:=2.2;";
	pCozOut = "({} (int $i Literal:Int##) (int $foo int) (float $g float)) (float $g_g Literal:Float32)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"i:= (5-4) + (5-6); g:=2.2 + (3.3*10);";
	pCozOut = "(int $i (Literal:Int## (Literal:Int Literal:Int Literal:Int) (Literal:Int Literal:Int Literal:Int))) "
				"(float $g (Literal:Float32 Literal:Float (Literal:Float Literal:Float Literal:Int)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "n:=2; n = 100-n;";
	pCozOut ="(int $n Literal:Int##) (= int (int Literal:Int## int))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:s8=5; foo:=i; bar:s16=i; g:=g_g; } g_g : f64 = 2.2;";
	pCozOut = "({} (s8 $i s8 Literal:Int8) (s8 $foo s8) (s16 $bar s16 s8) (f64 $g f64)) (f64 $g_g f64 Literal:Float64)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:=true; foo:=i; g:=g_g; } g_g : bool = false;";
	pCozOut = "({} (bool $i Literal:Bool8) (bool $foo bool) (bool $g bool)) (bool $g_g bool Literal:Bool8)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= "ParamFunc :: (nA : s32, g : float) { foo := nA; bah := g; }";
	pCozOut		= "(ParamFunc(s32, float)->void $ParamFunc (Params (s32 $nA s32) (float $g float)) void ({} (s32 $foo s32) (float $bah float) (void)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:=\"hello\"; foo:=i; g:=g_g; } g_g : &u8 = \"huzzah\";";
	pCozOut = "({} (&u8 $i Literal:String) (&u8 $foo &u8) (&u8 $g &u8)) (&u8 $g_g (&u8 u8) Literal:String)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ pN:& s8; pNInfer:=pN; pNTest:&s8=null;}";
	pCozOut = "({} (&s8 $pN (&s8 s8)) (&s8 $pNInfer &s8) (&s8 $pNTest (&s8 s8) Literal:Null))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ ppN:&& s8; ppNInfer:=ppN; ppNTest:&&s8=null;}";
	pCozOut = "({} (&&s8 $ppN (&&s8 (&s8 s8))) (&&s8 $ppNInfer &&s8) (&&s8 $ppNTest (&&s8 (&s8 s8)) Literal:Null))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:s8=5; foo:=i; foo=6; i = foo; }";
	pCozOut = "({} (s8 $i s8 Literal:Int8) (s8 $foo s8) (= s8 Literal:Int8) (= s8 s8))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:s8=5; foo:=i; fBool:bool = foo==i; fBool = i<2; }";
	pCozOut = "({} (s8 $i s8 Literal:Int8) (s8 $foo s8) (bool $fBool bool (bool s8 s8)) (= bool (bool s8 Literal:Int8)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ i:s8; foo:s32; foo=i+foo; foo=foo<<i; }";
	pCozOut = "({} (s8 $i s8) (s32 $foo s32) (= s32 (s32 s8 s32)) (= s32 (s32 s32 s8)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ n:s8; pN:=&n; n2:=@pN; }";
	pCozOut = "({} (s8 $n s8) (&s8 $pN (&s8 s8)) (s8 $n2 (s8 &s8)))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ n:s64; pN:&s8; fN := !pN; ++n; --n;}";
	pCozOut = "({} (s64 $n s64) (&s8 $pN (&s8 s8)) (bool $fN (bool &s8)) (s64 s64) (s64 s64))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ n:s64; if n == 2 n = 5; else n = 6;}";
	pCozOut = "({} (s64 $n s64) (bool (bool s64 Literal:Int64) (= s64 Literal:Int64) (??? (= s64 Literal:Int64))))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ n:s64 = 5; while n > 0 { --n; } }";
	pCozOut = "({} (s64 $n s64 Literal:Int64) (bool (bool s64 Literal:Int64) ({} (s64 s64))))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn = "{ nA : s8; nB : s8; nC := (nA < nB) == (nB >= nA); }";
	pCozOut = "({} (s8 $nA s8) (s8 $nB s8) (bool $nC (bool (bool s8 s8) (bool s8 s8))))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn =	"{ n:s64; if n n = 5; else n = 6;}";
	pCozOut = "({} (s64 $n s64) (bool s64 (= s64 Literal:Int64) (??? (= s64 Literal:Int64))))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= "AddNums :: (a : int, b := 1)->int { return a + b;} n := AddNums(2,3);";
	pCozOut		= "(AddNums(int, int)->int $AddNums (Params (int $a int) (int $b Literal:Int##)) int ({} (int (int int int))))"
					" (int $n (int AddNums(int, int)->int Literal:Int## Literal:Int##))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= "NoReturn :: (a : int) { n := a;} NoReturn(2);";
	pCozOut		= "(NoReturn(int)->void $NoReturn (Params (int $a int)) void ({} (int $n int) (void))) (void NoReturn(int)->void Literal:Int##)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= "NoReturn :: (a : int) { n := a; return; } NoReturn(2);";
	pCozOut		= "(NoReturn(int)->void $NoReturn (Params (int $a int)) void ({} (int $n int) (void))) (void NoReturn(int)->void Literal:Int##)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);
	
	pCozIn		= "{ ovr:=2; { nNest:= ovr; ovr:float=2.2; g:=ovr; } n:=ovr; }"; 
	pCozOut		= "({} (int $ovr Literal:Int##) ({} (int $nNest int) (float $ovr float Literal:Float32) (float $g float)) (int $n int))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);
	
	pCozIn		= " { ovr:=2; Foo :: () { nNest:= ovr; ovr:float=2.2; g:=ovr; } n:=ovr; }"; 
	pCozOut		=	"(Foo()->void $Foo void ({} (int $nNest int) (float $ovr float Literal:Float32) (float $g float) (void)))"
					" ({} (int $ovr Literal:Int##) (int $n int))";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	pCozIn		= "ack :: ( n : s32) { if (n < 2) foo(true); }  foo :: (n : bool) #foreign;"; 
	pCozOut		= "(ack(s32)->void $ack (Params (s32 $n s32)) void ({} (bool (bool s32 Literal:Int32) (void foo(bool)->void Literal:Bool8)) (void))) "
					"(foo(bool)->void $foo (Params (bool $n bool)) void)";
	AssertTestTypeCheck(&work, pCozIn, pCozOut);

	StaticShutdownStrings(&allocString);
}