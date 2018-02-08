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

enum ERREP // ERror REPorting
{
	ERREP_HideErrors,
	ERREP_ReportErrors,	
};

struct STypeCheckStackEntry // tag = tcsent
{
	int				m_nState;
	CSTNode *		m_pStnod;
	CSymbolTable *	m_pSymtab;			// BB - Could omit this pointer with careful handling of stack pops?
										//  maybe swap out for fPushedStack?
	CSTNode *		m_pStnodProcedure;	// definition node for current procedure
	SSymbol	*		m_pSymContext;		// Procedure or struct 
	GRFSYMLOOK		m_grfsymlook;
	PARK 			m_parkDeclContext;
	bool			m_fAllowForwardDecl;
	TCCTX			m_tcctx;
};

struct STypeCheckFrame // tag = tcfram
{
	size_t							m_ipTcframQueue;	// index in the pending/waiting queue
	SWorkspaceEntry *				m_pEntry;
	CDynAry<STypeCheckStackEntry>	m_aryTcsent;
};

struct SUnknownType // tag = untype
{
	CDynAry<STypeCheckFrame *>	m_arypTcframDependent;		// id for frames dependent on this type
};


#define VALIDATE_NAME_MANGLING 1

struct SInstantiateRequest // tag = insreq
{
								SInstantiateRequest()
								:m_pStnodGeneric(nullptr)
								,m_pSym(nullptr)
								,m_pGenmap(nullptr)
								,m_arypInsctx(BK_TypeCheckGenerics)
									{ ; }

								~SInstantiateRequest()
								{
									EWC_ASSERT(!m_pGenmap, "genmap was not cleaned up"); 
								}

								void Cleanup(CAlloc * pAlloc)
								{
									if (m_pGenmap)
									{
										m_pGenmap->Cleanup(pAlloc);
										pAlloc->EWC_DELETE(m_pGenmap);
										m_pGenmap = nullptr;
									}
								}


	CSTNode * 					m_pStnodGeneric;
	SSymbol *					m_pSym;				// instantiated type, tin proc with resolved argument types
	SGenericMap * 				m_pGenmap;

	CDynAry<SInstantiateContext *> m_arypInsctx; // code that instantiated this generic
};

struct STypeCheckWorkspace // tag = tcwork
{
					STypeCheckWorkspace(CAlloc * pAlloc, SErrorManager * pErrman, BlockListEntry * pblistEntry)
					:m_pAlloc(pAlloc)
					,m_pErrman(pErrman)
					,m_mang(pAlloc)
					,m_pblistEntry(pblistEntry)
					,m_blistTcfram(pAlloc, EWC::BK_TypeCheck)
					,m_hashPSymUntype(pAlloc, EWC::BK_TypeCheck)
					,m_arypTcframPending(pAlloc, EWC::BK_TypeCheck, pblistEntry->C())
					,m_arypTcframWaiting(pAlloc, EWC::BK_TypeCheck, pblistEntry->C())
					,m_aryInsreq(pAlloc, EWC::BK_TypeCheck)
					,m_arypInsctxManaged(pAlloc, EWC::BK_TypeCheck)
						{ ; }

					~STypeCheckWorkspace()
						{
							for (auto pInsreq = m_aryInsreq.A(); pInsreq != m_aryInsreq.PMac(); ++pInsreq)
							{
								pInsreq->Cleanup(m_pAlloc);
							}

							for (auto ppInsctx = m_arypInsctxManaged.A(); ppInsctx != m_arypInsctxManaged.PMac(); ++ppInsctx)
							{
								m_pAlloc->EWC_DELETE(*ppInsctx);
							}
							m_arypInsctxManaged.Clear();
						}
	CAlloc *								m_pAlloc;
	SErrorManager *							m_pErrman;
	CNameMangler							m_mang;
	BlockListEntry * 						m_pblistEntry;
	CBlockList<STypeCheckFrame, 128>		m_blistTcfram;
	CHash<const SSymbol *, SUnknownType>	m_hashPSymUntype;

	CDynAry<STypeCheckFrame *>				m_arypTcframPending;	// frames ready to be run again (may stop 
																	//  during check, not guaranteed to have all types)
	CDynAry<STypeCheckFrame *>				m_arypTcframWaiting;	// frames waiting for one specific symbol

	CDynAry<SInstantiateRequest>			m_aryInsreq;			// generics that need to be instantiated -> typechecked
	CDynAry<SInstantiateContext *>			m_arypInsctxManaged;	// instantiate contexts that need to be deleted
};

enum PROCMATCH
{
	PROCMATCH_None,
	PROCMATCH_Exact,
	PROCMATCH_ImplicitCast,

	EWC_MAX_MIN_NIL(PROCMATCH)
};

enum ARGORD // ARGument ORDer
{
	ARGORD_Normal,
	ARGORD_Reversed,	// argument order reversed (used for checking comutative procedures)

	EWC_MAX_MIN_NIL(ARGORD)
};

enum FARG
{
	FARG_DefaultArg			= 0x1,	// default argument needs to copy syntax tree
	FARG_NamedLabelChild	= 0x2,	// argument was specified with a label - pStnod points at label parent
	FARG_BakedValue			= 0x4,	// this stnod will be removed from the argument list (but not deleted until end of typecheck) 

	FARG_None = 0x0,
	FARG_All  = 0x7,
};

EWC_DEFINE_GRF(GRFARG, FARG, u8);



bool FVerifyIvalk(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, IVALK ivalkExpected);
extern bool FDoesOperatorExist(TOK tok, const SOpTypes * pOptype);
bool FCanExplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst, CSymbolTable * pSymtab);
void OnTypeResolve(STypeCheckWorkspace * pTcwork, const SSymbol * pSym);
STypeInfo * PTinPromoteUntypedDefault(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,
	ERREP errep = ERREP_ReportErrors);
bool FDoesOperatorReturnBool(PARK park);
void FinalizeLiteralType(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, STypeInfo * pTinDst, CSTNode * pStnodLit);

void RemapGenericStnodCopy(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGen,
	CSTNode * pStnodNew,
	SGenericMap * pGenmap,
	EWC::CHash<SSymbol *, SSymbol *> * pmpPSymGenericPSymRemapped,
	EWC::CHash<SSymbol *, CSTNode *> * pmpPSymSrcPStnodConstant,
	EWC::CHash<CSTNode *, CSTNode *> * pmpPStnodGenPStnodNew,
	CSymbolTable * pSymtabSrc,
	CSymbolTable * pSymtabNew);

SInstantiateRequest * PInsreqLookup(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodDefinition,
	SGenericMap * pGenmap,
	SInstantiateContext * pInsctx);

ERRID ErridComputeDefinedGenerics(
	STypeCheckWorkspace * pTcwork,
	ERREP errep,
	STypeInfo * pTinRef,
	CSTNode * pStnodDef,
	SGenericMap * pGenmap,
	CHash<HV, CSTNode *> * pmpHvPStnodAnchor);

void PrintTypeStack(STypeCheckWorkspace * pTcwork, STypeCheckFrame * pTcfram)
{
	for (int iTcsent = 0; iTcsent < pTcfram->m_aryTcsent.C(); ++iTcsent)
	{
		auto pTcsent = &pTcfram->m_aryTcsent[iTcsent];

		s32 iLine;
		s32 iCol;
		auto pLexloc = &pTcsent->m_pStnod->m_lexloc;
		CalculateLinePosition(pTcwork->m_pErrman->m_pWork, pLexloc, &iLine, &iCol);
		
		printf("%d) PARK_%s, state = %d, sym = %s,     %s (%d, %d)\n", 
			iTcsent,
			PChzFromPark(pTcsent->m_pStnod->m_park),
			pTcsent->m_nState,
			(pTcsent->m_pStnod->m_pSym) ? pTcsent->m_pStnod->m_pSym->m_strName.PCoz() : "none",
			pLexloc->m_strFilename.PCoz(), iLine, iCol);
	}
}

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
	case TINK_Void:		AppendCoz(&m_strbuf, "Bv");	break;
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;

			AppendCoz(&m_strbuf, "Q");
			if (pTinqual->m_grfqualk.FIsSet(FQUALK_Const)) 
				AppendCoz(&m_strbuf, "c");
			if (pTinqual->m_grfqualk.FIsSet(FQUALK_InArg)) 
				AppendCoz(&m_strbuf, "i");

			AppendType(pTinqual->m_pTin);

		} break;
    case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;
			AppendCoz(&m_strbuf, "P");
			AppendType(pTinptr->m_pTinPointedTo);
		} break;
    case TINK_Struct:
		{
			AppendName(pTin->m_strName.PCoz()); 
		}break;
    case TINK_Enum:
		{
			AppendName(pTin->m_strName.PCoz()); 
		}break;
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

			if (pTinproc->FHasVarArgs())
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
		CString strTin = StrFromTypeInfo(pTin);
		EWC_ASSERT(false, "unexpected type encountered while name mangling a procedure '%s'", strTin.PCoz());
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

STypeInfo * PTinQualifyAfterAssignment(STypeInfo * pTin, CSymbolTable * pSymtab)
{
	if (pTin->m_tink != TINK_Qualifier)
		return pTin;
	
	auto pTinqual = (STypeInfoQualifier *)pTin;
	if (!pTinqual->m_grfqualk.FIsSet(FQUALK_Const))
	{
		// just strip inArg

		return pTinqual->m_pTin;
	}

	// strip off the top level const, but make sure there's a const qualifier one level below

	STypeInfo * pTinRef = pTinqual->m_pTin;
	switch (pTinRef->m_tink)
	{
	case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTinRef;
			auto pTinqualChild = pSymtab->PTinqualEnsure(pTinptr->m_pTinPointedTo, pTinqual->m_grfqualk);
			pTinptr->m_pTinPointedTo = pTinqualChild;
		}break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTinRef;
			auto pTinqualChild = pSymtab->PTinqualEnsure(pTinary->m_pTin, pTinqual->m_grfqualk);
			pTinary->m_pTin = pTinqualChild;
		}break;
	default: break;
	}

	return pTinRef;
}

STypeInfo * PTinAfterRValueAssignment(STypeCheckWorkspace * pTcwork, SLexerLocation * pLexloc, STypeInfo * pTin, CSymbolTable * pSymtab)
{
	if (pTin->m_tink == TINK_Procedure)
	{
		auto pTinproc = (STypeInfoProcedure *)pTin;
		if (pTinproc->FHasGenericArgs())	
		{
			EmitError(pTcwork->m_pErrman, pLexloc, ERRID_NoGenericRValue,
				"cannot make a reference to a generic procedure definition '%s'",
				pTinproc->m_strName.PCoz());
		}
	}

	return PTinQualifyAfterAssignment(pTin, pSymtab);
}


STypeInfo * PTinStripQualifiers(STypeInfo * pTin)
{
	int cQualifiers = 0;
	while (pTin->m_tink == TINK_Qualifier)
	{
		auto pTinqual = (STypeInfoQualifier *)pTin;
		pTin = pTinqual->m_pTin;
		EWC_ASSERT(++cQualifiers < 2, "STypeInfoQualifiers should not be directly nested");
	}
	return pTin;
}

STypeInfoStruct * PTinstructAlloc(
	CSymbolTable * pSymtab,
	CString strIdent,
	size_t cField,
	size_t cGenericParam)
{
	EWC::CAlloc * pAlloc = pSymtab->m_pAlloc;

	size_t cBAlloc = CBAlign(sizeof(STypeInfoStruct), EWC_ALIGN_OF(STypeStructMember)) + 
					cField * sizeof(STypeStructMember) + 
					cGenericParam  * sizeof (STypeInfo *);
	u8 * pB = (u8 *)pAlloc->EWC_ALLOC(cBAlloc, 8);

	STypeInfoStruct * pTinstruct = new(pB) STypeInfoStruct(strIdent, StrUniqueName(pSymtab->m_pUnsetTin, strIdent));
	pSymtab->AddManagedTin(pTinstruct);

	auto aTypememb = (STypeStructMember*)PVAlign(
											pB + sizeof(STypeInfoStruct), 
											EWC_ALIGN_OF(STypeStructMember));
	pTinstruct->m_aryTypemembField.SetArray(aTypememb, 0, cField);

	auto aTinParam = (STypeInfo**)PVAlign(&aTypememb[cField], EWC_ALIGN_OF(STypeInfo *));
	pTinstruct->m_arypTinGenericParam.SetArray(aTinParam, 0, cGenericParam);
	pTinstruct->m_arypTinGenericParam.AppendFill(cGenericParam, nullptr);
	return pTinstruct;
}

STypeInfoProcedure * PTinprocAlloc(
	CSymbolTable * pSymtab,
	size_t cParam,
	size_t cReturn,
	const char * pCozName)
{
	EWC::CAlloc * pAlloc = pSymtab->m_pAlloc;

	size_t cBAlloc = CBAlign(sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));
	cBAlloc = cBAlloc +	(cParam + cReturn) * sizeof(STypeInfo *) + (cParam * sizeof(GRFPARMQ));

	u8 * pB = (u8 *)pAlloc->EWC_ALLOC(cBAlloc,8);
	CString strName(pCozName);
	STypeInfoProcedure * pTinproc = new(pB) STypeInfoProcedure(strName, StrUniqueName(pSymtab->m_pUnsetTin, strName));
	STypeInfo ** ppTin = (STypeInfo**)PVAlign( pB + sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));

	pTinproc->m_arypTinParams.SetArray(ppTin, 0, cParam);
	pTinproc->m_arypTinReturns.SetArray(&ppTin[cParam], 0, cReturn);

	auto pGrfparmq = (GRFPARMQ*)&ppTin[cParam + cReturn];
	pTinproc->m_mpIptinGrfparmq.SetArray(pGrfparmq, cParam, cParam);
	ZeroAB(pTinproc->m_mpIptinGrfparmq.A(), pTinproc->m_mpIptinGrfparmq.C() * sizeof(GRFPARMQ));

	pSymtab->AddManagedTin(pTinproc);
	return pTinproc;
}

STypeInfoProcedure * PTinprocCopy(CSymbolTable * pSymtab, STypeInfoProcedure * pTinprocSrc)
{
	STypeInfoProcedure * pTinprocNew = PTinprocAlloc(
										pSymtab,
										pTinprocSrc->m_arypTinParams.C(),
										pTinprocSrc->m_arypTinReturns.C(),
										pTinprocSrc->m_strName.PCoz());

	pTinprocNew->m_pStnodDefinition = pTinprocSrc->m_pStnodDefinition;
	pTinprocNew->m_grftinproc = pTinprocSrc->m_grftinproc;
	pTinprocNew->m_inlinek = pTinprocSrc->m_inlinek;
	pTinprocNew->m_callconv = pTinprocSrc->m_callconv;

	pTinprocNew->m_arypTinParams.Append(pTinprocSrc->m_arypTinParams.A(), pTinprocSrc->m_arypTinParams.C());
	pTinprocNew->m_arypTinReturns.Append(pTinprocSrc->m_arypTinReturns.A(), pTinprocSrc->m_arypTinReturns.C());

	pTinprocNew->m_mpIptinGrfparmq.Clear();
	pTinprocNew->m_mpIptinGrfparmq.Append(pTinprocSrc->m_mpIptinGrfparmq.A(), pTinprocSrc->m_mpIptinGrfparmq.C());
	return pTinprocNew;
}

STypeInfoArray * PTinaryCopy(CSymbolTable * pSymtab, STypeInfoArray * pTinarySrc)
{
	auto pTinaryNew = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
	pSymtab->AddManagedTin(pTinaryNew);

	*pTinaryNew = *pTinarySrc;	
	return pTinaryNew;
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
	else if (chFirst == 'Q') // Qualifier
	{
		++(*ppCoz);
		GRFQUALK grfqualk = FQUALK_None;
		if (**ppCoz == 'c')
		{
			grfqualk.AddFlags(FQUALK_Const);
			++(*ppCoz);
		}
		if (**ppCoz == 'i')
		{
			grfqualk.AddFlags(FQUALK_InArg);
			++(*ppCoz);
		}

		auto pTinPointedTo = PTinReadType(ppCoz, pSymtab);
		if (!pTinPointedTo)
			return nullptr;
		return pSymtab->PTinqualEnsure(pTinPointedTo, grfqualk);
	}
	else if (chFirst == 'P') // Pointer
	{
		++(*ppCoz);
		auto pTinPointedTo = PTinReadType(ppCoz, pSymtab);
		if (!pTinPointedTo)
			return nullptr;
		return pSymtab->PTinptrAllocate(pTinPointedTo);
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

		while (!FMatchString("_", ppCoz) && **ppCoz != '\0')
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

		auto pTinproc = PTinprocAlloc(pSymtab, arypTinParams.C(), arypTinReturns.C(), "");
		pTinproc->m_grftinproc.AssignFlags(FTINPROC_HasVarArgs, fHasVarArgs);

		size_t cpTin = arypTinParams.C();
		for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
		{
			pTinproc->m_arypTinParams.Append(arypTinParams[ipTin]);
		}

		cpTin = arypTinReturns.C();
		for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
		{
			pTinproc->m_arypTinReturns.Append(arypTinReturns[ipTin]);
		}

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

	if (pTinproc->FHasVarArgs())
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

	while (!FMatchString("_", &pCoz) && *pCoz != '\0')
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

	auto pTinproc = PTinprocAlloc(pSymtab, arypTinParams.C(), arypTinReturns.C(), strProcName.PCoz());
	pTinproc->m_grftinproc.AssignFlags(FTINPROC_HasVarArgs, fHasVarArgs);

	size_t cpTin = arypTinParams.C();
	for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
	{
		pTinproc->m_arypTinParams.Append(arypTinParams[ipTin]);
	}

	cpTin = arypTinReturns.C();
	for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
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
	//printf("%s(%d) Error:", pLex->m_pChzFilename, NLine(pLex));
	
	const SLexerLocation & lexloc = pStnod->m_lexloc;

	va_list ap;
	va_start(ap, pCozFormat);
	EmitError(pTcwork->m_pErrman, &lexloc, ERRID_UnknownError, pCozFormat, ap);
}

void EmitError(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, ERRID errid, const char * pCozFormat, ...)
{
	// BB - need to do file/line lookup from pStnod
	//printf("%s(%d) Error:", pLex->m_pChzFilename, NLine(pLex));
	
	const SLexerLocation & lexloc = pStnod->m_lexloc;

	va_list ap;
	va_start(ap, pCozFormat);
	EmitError(pTcwork->m_pErrman, &lexloc, errid, pCozFormat, ap);
}

void AllocateOptype(CSTNode * pStnod)
{
	EWC_ASSERT(pStnod->m_pOptype == nullptr, "expected null");

	CAlloc * pAlloc = pStnod->m_arypStnodChild.m_pAlloc;
	pStnod->m_pOptype = EWC_NEW(pAlloc, SOpTypes) SOpTypes();
}


void AppendTypenameFromTypeSpecification(CSTNode * pStnodArg, EWC::SStringBuffer * pStrbuf)
{
	// BB - This routine should reproduce a type specification, but it's not really up to the task
	//  (especially for generic types)
	CSTNode * pStnodIt = pStnodArg;
	while (pStnodIt)
	{
		switch (pStnodIt->m_park)
		{
			case PARK_Identifier:
			{
				if (!EWC_FVERIFY(pStnodIt->m_pStident, "identifier without identifier string detected"))
					break;
				AppendCoz(pStrbuf, StrFromIdentifier(pStnodIt).PCoz()); 
				pStnodIt = nullptr;
			}break;
			case PARK_Literal:
			{
				PrintLiteral(pStrbuf, pStnodIt);
				pStnodIt = nullptr;
			}break;
			case PARK_GenericStructInst:
			{
				auto pStnodIdent = pStnodIt->PStnodChildSafe(0);
				if (!EWC_FVERIFY(pStnodIdent, "GenericStructInst with no identifier"))
					break;

				AppendTypenameFromTypeSpecification(pStnodIdent, pStrbuf);
				AppendCoz(pStrbuf, "(");
				auto cStnodChild = pStnodIt->CStnodChild();
				if (cStnodChild > 1)
				{
					int iStnod = 1;
					while (1)
					{
						auto pStnodChild = pStnodIt->PStnodChildSafe(iStnod);
						AppendTypenameFromTypeSpecification(pStnodChild, pStrbuf);
						++iStnod;
						if (iStnod >= cStnodChild)
							break;
						AppendCoz(pStrbuf, ",");
					}
				}
				AppendCoz(pStrbuf, ")");
				pStnodIt=  nullptr;

			}break;
			case PARK_QualifierDecl:
			{
				if (!EWC_FVERIFY(pStnodIt->m_pStval, "qualifier without value string detected"))
					break;
				AppendCoz(pStrbuf, PCozFromRword(pStnodIt->m_pStval->m_rword)); 
				AppendCoz(pStrbuf, " "); 
			}break;
			case PARK_ReferenceDecl:
				AppendCoz(pStrbuf, "* "); 

				EWC_ASSERT(pStnodIt->CStnodChild() == 1, "expected one child");
				pStnodIt = pStnodIt->PStnodChild(0);
				break;
			case PARK_ArrayDecl:
				// BB - should follow the [], [..], [c] convention
				EWC_ASSERT(false, "not type-checking asserts yet");
				pStnodIt=  nullptr;

				break;
			default:
				AppendCoz(pStrbuf, "<unexpected PARK_"); 
				AppendCoz(pStrbuf, PChzFromPark(pStnodIt->m_park));
				AppendCoz(pStrbuf, "> "); 

				pStnodIt = nullptr;
				break;
		}
	}
}

CString StrTypenameFromTypeSpecification(CSTNode * pStnod)
{
	char aCh[2048];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	AppendTypenameFromTypeSpecification(pStnod, &strbuf);
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
		pUntype->m_arypTcframDependent.SetAlloc(pTcwork->m_pAlloc, EWC::BK_TypeCheck);
	}
	return pUntype;
}

STypeCheckStackEntry * PTcsentPush(STypeCheckFrame * pTcfram, STypeCheckStackEntry ** ppTcsentTop, CSTNode * pStnod)
{
	if (pStnod->m_strees >= STREES_TypeChecked)
	{
		// we can have type checked nodes here when we're dealing with baked constants
		return nullptr;
	}

	// update ppTcsentTop to handle times when the dynArray reallocs.
	size_t iTcsentTop = pTcfram->m_aryTcsent.IFromP(*ppTcsentTop);

	size_t cPrev = pTcfram->m_aryTcsent.C()-1;
	STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
	*pTcsent = pTcfram->m_aryTcsent[cPrev];

	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnod;

	*ppTcsentTop = &pTcfram->m_aryTcsent[iTcsentTop];
	return pTcsent;
}

void PopTcsent(STypeCheckFrame * pTcfram, STypeCheckStackEntry ** ppTcsentTop, CSTNode * pStnodDebug)
{
	*ppTcsentTop = nullptr;
	EWC_ASSERT(pTcfram->m_aryTcsent.PLast()->m_pStnod == pStnodDebug || pStnodDebug == nullptr, "type check entry pop mismatch");

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
			switch (pStval->m_rword)
			{
			case RWORD_LineDirective:	return pStval->m_nUnsigned;
			case RWORD_True:			return 1;
			case RWORD_False:			return 0;
			default:

				EWC_ASSERT(false, "unexpected reserved word %s", PCozFromRword(pStval->m_rword));
				return 0;
			}
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
			switch (pStval->m_rword)
			{
			case RWORD_LineDirective:	return pStval->m_nUnsigned;
			case RWORD_True:			return 1;
			case RWORD_False:			return 0;
			default:

				EWC_ASSERT(false, "unexpected reserved word %s", PCozFromRword(pStval->m_rword));
				return 0;
			}
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
		u64 nUnsigned = bint.U64Coerce();

		if (nUnsigned >  LLONG_MAX)
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

	TOK tokOperator = pStnodOperator->m_tok;
	bool fIsBoolOp = FDoesOperatorReturnBool(pStnodOperator->m_park);

	if (littyOperand.m_litk == LITK_Float)
	{
		bool f;
		F64 g = GLiteralCast(pStvalOperand);
		switch ((u32)tokOperator)
		{
		case '-':         g = -g; break;
		case '!':         f = !g; break;
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

			EWC_ASSERT(pTinlitOperand->m_litty.m_litk == LITK_Float, "expected float literal");
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
		switch ((u32)tokOperator)
		{
		case TOK('-'):         bintOperand.m_fIsNegative = !bintOperand.m_fIsNegative; break;
		// BB - We're not really handling unsized literals correctly here - we'll just promote to a 64 bit type
		case TOK('~'):       bintOperand = BintBitwiseNot(bintOperand); break;

		case TOK('!'):         f = bintOperand.m_nAbs == 0; break;
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
				EWC_ASSERT(pTinlitOperand->m_litty.m_litk == LITK_Integer, "expected integer literal");
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

	TOK tokOperator = pStnodOperator->m_tok;
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
		switch ((u32)tokOperator)
		{
		case TOK('+'):         g = gLhs + gRhs; break;
		case TOK('-'):         g = gLhs - gRhs; break;
		case TOK('*'):         g = gLhs * gRhs; break;
		case TOK('/'):         g = gLhs / gRhs; break;
		case TOK('>'):         f = gLhs > gRhs; break;
		case TOK('<'):         f = gLhs < gRhs; break;
		case TOK_EqualEqual:   f = gLhs == gRhs; break;
		case TOK_NotEqual:     f = gLhs != gRhs; break;
		case TOK_LessEqual:    f = gLhs <= gRhs; break;
		case TOK_GreaterEqual:	f = gLhs >= gRhs; break;
		case TOK_AndAnd:		f = (gLhs != 0.0f) && (gRhs != 0.0f); break;
		case TOK_OrOr:			f = (gLhs != 0.0f) || (gRhs != 0.0f); break;
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
		// may not be unsized literal in compound expressions, ie a == b == c
		// turns into (a == b) == c which ends up being bool == unsized comparison

		SBigInt bintLhs(BintFromStval(pStvalLhs));
		SBigInt bintRhs(BintFromStval(pStvalRhs));

		SBigInt bintOut;
		bool f;
		switch ((u32)tokOperator)
		{
		case TOK('+'):         bintOut = BintAdd(bintLhs, bintRhs); break;
		case TOK('-'):         bintOut = BintSub(bintLhs, bintRhs); break;
		case TOK('*'):         bintOut = BintMul(bintLhs, bintRhs); break;
		case TOK('/'):         bintOut = BintDiv(bintLhs, bintRhs); break;
		case TOK('%'):         bintOut = BintRemainder(bintLhs, bintRhs); break;
		case TOK('|'):         bintOut = BintBitwiseOr(bintLhs, bintRhs); break;
		case TOK('&'):         bintOut = BintBitwiseAnd(bintLhs, bintRhs); break;
		case TOK('^'):         bintOut = BintBitwiseXor(bintLhs, bintRhs); break;
		case TOK_ShiftRight:	bintOut = BintShiftRight(bintLhs, bintRhs); break;
		case TOK_ShiftLeft:	bintOut = BintShiftLeft(bintLhs, bintRhs); break;
		case TOK('>'):         f = bintLhs > bintRhs; break;
		case TOK('<'):         f = bintLhs < bintRhs; break;
		case TOK_EqualEqual:   f = bintLhs == bintRhs; break;
		case TOK_NotEqual:     f = bintLhs != bintRhs; break;
		case TOK_LessEqual:    f = bintLhs <= bintRhs; break;
		case TOK_GreaterEqual:	f = bintLhs >= bintRhs; break;
		case TOK_AndAnd:	
		{
			f = bintLhs.m_nAbs && bintRhs.m_nAbs; 
		}	break;
		case TOK_OrOr:			f = bintLhs.m_nAbs || bintRhs.m_nAbs; break;
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
				EWC_ASSERT(pTinlitLhs->m_litty.m_litk == LITK_Integer || pTinlitLhs->m_litty.m_litk == LITK_Bool, "unexpected literal kind");
				*ppTinReturn = pTinlitLhs;
				*ppTinOperand = pTinlitLhs;
			}
		}
		return true;
	}
}


void FinalizeArrayLiteralType(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, STypeInfoLiteral * pTinlit, CSTNode * pStnodLit)
{
	if (!EWC_FVERIFY(pTinlit->m_litty.m_litk == LITK_Array, "finalizing array with non-array literal"))
		return;
	
	CSTNode * pStnodDef = pStnodLit;
	if (EWC_FVERIFY(pTinlit->m_pStnodDefinition, "bad array literal definition"))
	{
		pStnodDef = pTinlit->m_pStnodDefinition;
	}
	
	auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDef->m_pStmap);
	if (!pStdecl)
		return;

	auto pStnodList = pStnodDef->PStnodChildSafe(pStdecl->m_iStnodInit);
	if (pStnodList)
	{
		for (int iStnod = 0; iStnod < pStnodList->CStnodChild(); ++iStnod)
		{
			FinalizeLiteralType(pTcwork, pSymtab, pTinlit->m_pTinSource, pStnodList->PStnodChild(iStnod));
		}
	}
}

void FinalizeLiteralType(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, STypeInfo * pTinDst, CSTNode * pStnodLit)
{
	if (!pStnodLit->m_pTin || pStnodLit->m_pTin->m_tink != TINK_Literal)
		return;

	pTinDst = PTinStripQualifiers(pTinDst);

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
	case TINK_Procedure:
		{
			STypeInfoLiteral * pTinlitPrev = (STypeInfoLiteral *)pStnodLit->m_pTin;
			STypeInfoLiteral * pTinlit = nullptr;
			
			switch (pTinlitPrev->m_litty.m_litk)
			{
			case LITK_Null:		
				{
					pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
					pTinlit->m_litty.m_litk = LITK_Null;
					pTinlit->m_litty.m_cBit = -1;
					pTinlit->m_litty.m_fIsSigned = false;
					pTinlit->m_fIsFinalized = true;
					pTinlit->m_pTinSource = (STypeInfoPointer*)pTinDst;
					pSymtab->AddManagedTin(pTinlit);

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
			STypeInfoLiteral * pTinlitPrev = PTinDerivedCast<STypeInfoLiteral *>(pStnodLit->m_pTin);
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
			case LITK_Array:
				{
					FinalizeArrayLiteralType(pTcwork, pSymtab, pTinlitPrev, pStnodLit);
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
			STypeInfoLiteral * pTinlitPrev = PTinDerivedCast<STypeInfoLiteral *>(pStnodLit->m_pTin);
			FinalizeArrayLiteralType(pTcwork, pSymtab, pTinlitPrev, pStnodLit);

		} break;
	case TINK_Generic:
		{
			(void) PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit);
		}break;
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
			return pSymtab->PTinptrAllocate(pTinary->m_pTin);
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
			return pSymtab->PTinptrAllocate(pTinU8);
		} break;
	case LITK_Enum:
		{
			EWC_ASSERT(false, "enum literals should not be finalized");
		} break;
	case LITK_Null:
		{
			return pTinlit->m_pTinSource;
		} break;
	default:
		break;
	}

	EWC_ASSERT(false, "Unknown literal kind %d", litty.m_litk);
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

		auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodLit->m_pStmap);
		if (!EWC_FVERIFY(pStdecl, "bad array literal"))
			return nullptr;

		auto pStnodValues = pStnodLit->PStnodChildSafe(pStdecl->m_iStnodInit);

		if (!pTinlit->m_pTinSource &&
			EWC_FVERIFY(pStnodValues && pStnodValues->CStnodChild(), "Array literal has no child literals"))
		{
			pTinlit->m_pTinSource = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodValues->PStnodChild(0));
		}

		STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
		pTinary->m_pTin = pTinlit->m_pTinSource;
		pTinary->m_c = pTinlit->m_c;
		pTinary->m_aryk = ARYK_Fixed;
		pSymtab->AddManagedTin(pTinary);
		pTinary = pSymtab->PTinMakeUnique(pTinary);

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


STypeInfo * PTinPromoteUntypedDefault(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,
	ERREP errep)
{
	if (pStnodLit->m_park == PARK_Cast)
	{
		auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodLit->m_pStmap);
		bool fIsAutoCast = pStdecl && pStdecl->m_iStnodType < 0;
		if (fIsAutoCast)
		{
			if (errep == ERREP_ReportErrors)
			{
				EmitError(pTcwork, pStnodLit, "Cannot resolve acast when the left hand side is untyped.");
			}
			return PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit->PStnodChildSafe(pStdecl->m_iStnodInit), errep);
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
		auto pTinqual = pSymtab->PTinqualEnsure(pTinU8, FQUALK_Const);
		return pSymtab->PTinptrAllocate(pTinqual);
	}
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:
		{
			auto pTinU8 = pSymtab->PTinBuiltin("void");
			return pSymtab->PTinptrAllocate(pTinU8);
		}break;
	case LITK_Enum:
		{
			auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinlit->m_pTinSource);
			EWC_ASSERT(pTinenum, "Failed to infer type for enum literal");
			return pTinenum;
		}
	case LITK_Nil: 
		EWC_ASSERT(false, "Cannot infer type for LITK_Nil");
	default:
		EWC_ASSERT(false, "Cannot infer type for unexpected literal kind");
		break;
	}
	return nullptr;
}

STypeInfo * PTinPromoteUntypedArgument(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,
	STypeInfo * pTinArgument,
	ERREP errep)
{
	STypeInfoLiteral * pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodLit->m_pTin);
	if (pTinlit)
	{
		const SLiteralType & litty = pTinlit->m_litty;
		if (litty.m_litk == LITK_Null)
		{
			if (pTinArgument && pTinArgument->m_tink == TINK_Pointer )
				return pTinArgument;

			return pSymtab->PTinptrAllocate(pSymtab->PTinBuiltin("void"));
		}
	}

	return PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLit, errep);
}

inline STypeInfo * PTinPromoteUntypedTightest(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,	
	STypeInfo * pTinDst,
	ERREP errep = ERREP_ReportErrors)
{
	if (pStnodLit->m_park == PARK_Cast)
	{
		auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodLit->m_pStmap);
		bool fIsAutoCast = pStdecl && pStdecl->m_iStnodType < 0;
		if (fIsAutoCast)
		{
			auto pStnodInit = pStnodLit->PStnodChildSafe(pStdecl->m_iStnodInit);
			STypeInfo * pTinInit = (pStnodInit) ? pStnodInit->m_pTin : nullptr;

			if (EWC_FVERIFY(pStnodInit, "finalizing auto cast with no target type"))
			{
				if (FCanExplicitCast(pTinInit, pTinDst, pSymtab))
				{
					pStnodLit->m_pTin = pTinDst;
					(void) PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodInit, pTinDst);
				}
				else if (errep == ERREP_ReportErrors)
				{
					EmitError(pTcwork, pStnodLit, "Cannot auto cast type '%s' to '%s'",
						StrFromTypeInfo(pTinInit).PCoz(),
						StrFromTypeInfo(pTinDst).PCoz());
				}
				return pStnodLit->m_pTin;
			}
		}
	}

	pTinDst = PTinStripQualifiers(pTinDst); 

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

			s64 nSigned = NSignedLiteralCast(pTcwork, pStnodLit, pStval);
			if (fIsValNegative)
			{
				if (nSigned >= SCHAR_MIN)	return pSymtab->PTinBuiltin("s8");
				if (nSigned >= SHRT_MIN)	return pSymtab->PTinBuiltin("s16");
				if (nSigned >= INT_MIN)	return pSymtab->PTinBuiltin("s32");
				return pSymtab->PTinBuiltin("s64");
			}

			// NOTE - if this value isn't explicitly negative, allow code to initialize it with 
			//  values large enough to cause it to be negative. ie. n:s32=0xFFFFFFFF;

			if (nSigned <= UCHAR_MAX)	return pSymtab->PTinBuiltin("s8");
			if (nSigned <= USHRT_MAX)	return pSymtab->PTinBuiltin("s16");
			if (nSigned <= UINT_MAX)	return pSymtab->PTinBuiltin("s32");
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
		auto pTinqual = pSymtab->PTinqualEnsure(pTinU8, FQUALK_Const);
		return pSymtab->PTinptrAllocate(pTinqual);
	}
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:		
		{
			if (pTinDst && (pTinDst->m_tink == TINK_Pointer || pTinDst->m_tink == TINK_Procedure))
				return pTinDst;
			if (errep == ERREP_ReportErrors)
			{
				EmitError(pTcwork, pStnodLit, "Trying to initialize non pointer type with null value");
			}
		} break;
	case LITK_Nil: 
		EWC_ASSERT(false, "Cannot infer type for LITK_Nil");
	default:
		EWC_ASSERT(false, "Cannot infer type for unknown literal kind");
	}
	return nullptr;
}

inline STypeInfo * PTinPromoteUntypedRvalueTightest(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,	
	STypeInfo * pTinDst)
{
	auto pTinPromoted = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodLit, pTinDst);

	return PTinAfterRValueAssignment(pTcwork, &pStnodLit->m_lexloc, pTinPromoted, pSymtab);
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
	case TINK_Qualifier:
		{
			auto pTinqualLhs = (STypeInfoQualifier *)pTinLhs;
			auto pTinqualRhs = (STypeInfoQualifier *)pTinRhs;
			return pTinqualLhs->m_grfqualk == pTinqualRhs->m_grfqualk &&
				FTypesAreSame(pTinqualLhs->m_pTin, pTinqualRhs->m_pTin);
		}
	case TINK_Pointer:	return FTypesAreSame(
								((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo, 
								((STypeInfoPointer *)pTinRhs)->m_pTinPointedTo);
	case TINK_Array:	
		{
			auto pTinaryLhs = (STypeInfoArray *)pTinLhs;
			auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
			return (pTinaryLhs->m_aryk == pTinaryRhs->m_aryk) & (pTinaryLhs->m_c == pTinaryRhs->m_c) &&
				FTypesAreSame(pTinaryLhs->m_pTin, pTinaryRhs->m_pTin);
		}
	case TINK_Struct:
		{
			auto pTinstructLhs = (STypeInfoStruct *)pTinLhs;
			auto pTinstructRhs = (STypeInfoStruct *)pTinRhs;

			return pTinstructLhs->m_pStnodStruct == pTinstructRhs->m_pStnodStruct;
		}
	case TINK_Enum:
		{
			// if we're not the same enum, return false
			return false;
		}
	case TINK_Procedure:
		{
			auto pTinprocLhs = (STypeInfoProcedure *)pTinLhs;
			auto pTinprocRhs = (STypeInfoProcedure *)pTinRhs;
			if (pTinprocLhs->m_arypTinParams.C() != pTinprocRhs->m_arypTinParams.C() ||
				pTinprocLhs->m_arypTinReturns.C() != pTinprocRhs->m_arypTinReturns.C() ||
				pTinprocLhs->m_grftinproc != pTinprocRhs->m_grftinproc)
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
	default:			
		EWC_ASSERT(false, "unhandled TINK");
		return false;
	}

}

bool FIsGenericType(STypeInfo * pTin)
{
	// This may happen with a type that's not done typechecking...
	if (!EWC_FVERIFY(pTin != nullptr, "null type in FIsGenericType"))
		return false;

	switch (pTin->m_tink)
	{
	case TINK_Generic:
		return true;
    case TINK_Integer:
    case TINK_Float:
    case TINK_Bool:
	case TINK_Literal:
    case TINK_Null:
    case TINK_Enum:
    case TINK_Any:
    case TINK_Void:
    	return false;
    case TINK_Pointer: 		return FIsGenericType(((STypeInfoPointer *)pTin)->m_pTinPointedTo);
    case TINK_Array:	 	return FIsGenericType(((STypeInfoArray *)pTin)->m_pTin);
	case TINK_Qualifier:	return FIsGenericType(((STypeInfoQualifier *)pTin)->m_pTin);
    case TINK_Procedure:
    	{
    		auto pTinproc = (STypeInfoProcedure *)pTin;
    		return pTinproc->FHasGenericArgs();
    	}
    case TINK_Struct:
    	{
    		auto pTinstruct = (STypeInfoStruct *)pTin;
			EWC::CAry<STypeStructMember>	m_aryTypemembField;

			return pTinstruct->FHasGenericParams();
    	}
	default:
		EWC_ASSERT(false, "unhandled TINK");
		return false;
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
	TOK tok,
	PARK parkOperator,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs)
{
	if (parkOperator == PARK_LogicalAndOrOp)
	{
		auto pTinBool = pSymtab->PTinBuiltin("bool");
		return SOpTypes(pTinBool, pTinBool, pTinBool);
	}

	GRFQUALK grfqualkLhs = FQUALK_None;
	GRFQUALK grfqualkRhs = FQUALK_None;
	if (auto pTinqualLhs = PTinRtiCast<STypeInfoQualifier *>(pTinLhs))
		{ grfqualkLhs = pTinqualLhs->m_grfqualk; }
	if (auto pTinqualRhs = PTinRtiCast<STypeInfoQualifier *>(pTinRhs))
		{ grfqualkRhs = pTinqualRhs->m_grfqualk; }
	auto pTinUnqualLhs = PTinStripQualifiers(pTinLhs);
	auto pTinUnqualRhs = PTinStripQualifiers(pTinRhs);

	bool fLhsIsReference = (pTinUnqualLhs->m_tink == TINK_Pointer) | (pTinUnqualLhs->m_tink == TINK_Array);
	bool fRhsIsReference = (pTinUnqualRhs->m_tink == TINK_Pointer) | (pTinUnqualRhs->m_tink == TINK_Array);

	// BB - Could this be cleaner with a table?
	if (fLhsIsReference | fRhsIsReference)
	{
		STypeInfo * pTinMin = pTinUnqualLhs;
		STypeInfo * pTinMax = pTinUnqualRhs;
		if (pTinMin->m_tink > pTinMax->m_tink)
		{
			ewcSwap(pTinMin, pTinMax);
		}
		TINK tinkMin = pTinMin->m_tink;
		TINK tinkMax = pTinMax->m_tink;

		if (fLhsIsReference & fRhsIsReference)
		{
			bool fLhsIsArrayRef = pTinUnqualLhs->m_tink == TINK_Array && 
									((STypeInfoArray*)pTinLhs)->m_aryk == ARYK_Reference;
			STypeInfo * pTinRefMax = nullptr;
			if (tinkMax == TINK_Array)
			{
				if (tinkMin != TINK_Pointer && !fLhsIsArrayRef) // no operand for array & array
					return SOpTypes();

				pTinRefMax = ((STypeInfoArray *)pTinMax)->m_pTin;
			}
			else if (EWC_FVERIFY(tinkMax == TINK_Pointer, "unexpected reference type info"))
			{
				pTinRefMax = ((STypeInfoPointer *)pTinMax)->m_pTinPointedTo;
				pTinRefMax = PTinStripQualifiers(pTinRefMax);
			}

			auto pTinRefMin = ((STypeInfoPointer*)pTinMin)->m_pTinPointedTo;
			pTinRefMin = PTinStripQualifiers(pTinRefMin);
			bool fAreRefTypesSame = FTypesAreSame(pTinRefMin, pTinRefMax);

			if (parkOperator == PARK_AssignmentOp)
			{
				if (tok == TOK('='))
				{
					if (pTinLhs->m_tink == TINK_Array && !fLhsIsArrayRef)
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
				// cast the array to a pointer before comparing
				return SOpTypes(pTinMin, pTinMin, pSymtab->PTinBuiltin("bool"));
			}

			if (parkOperator == PARK_AdditiveOp)
			{
				if (tok == TOK('-') && fAreRefTypesSame)
				{
					return SOpTypes(pTinLhs, pTinRhs, pSymtab->PTinBuiltin("sSize"));
				}
			}

			return SOpTypes();
		}

		STypeInfo * pTinRef = pTinLhs;
		STypeInfo * pTinOther = pTinRhs;
		if (pTinOther->m_tink == TINK_Pointer || pTinOther->m_tink == TINK_Array)
		{
			pTinRef = pTinRhs;
			pTinOther = pTinLhs;
		}

		if (pTinRef->m_tink == TINK_Pointer && ((STypeInfoPointer *)pTinRef)->m_pTinPointedTo->m_tink == TINK_Void)
		{
			return SOpTypes();
		}
		
		PARK parkOperatorAdj = parkOperator;
		if (parkOperator == PARK_AssignmentOp && ((tok == TOK_PlusEqual) | (tok == TOK_MinusEqual)))
		{
			parkOperatorAdj = PARK_AdditiveOp;
		}

		if (parkOperatorAdj == PARK_AdditiveOp)
		{
			if (pTinOther->m_tink == TINK_Integer)
			{
				return SOpTypes(pTinLhs, pTinRhs, pTinRef);
			}
			else
			{
				EWC_ASSERT(false, "unexpected Additive operator");
			}
		}
	}

	if (pTinLhs->m_tink == pTinRhs->m_tink)
	{
		switch(pTinLhs->m_tink)
		{
		case TINK_Float:
			{
				STypeInfoFloat * pTinfloatLhs = (STypeInfoFloat *)pTinLhs;
				STypeInfoFloat * pTinfloatRhs = (STypeInfoFloat *)pTinRhs;

				auto pTinOp = pTinLhs;
				if (pTinfloatLhs->m_cBit < pTinfloatRhs->m_cBit)
				{
					if (parkOperator == PARK_AssignmentOp)
					{
						return SOpTypes();
					}
					pTinOp = pTinRhs;
				}

				return SOpTypes(pTinOp, pTinOp, PTinResult(parkOperator, pSymtab, pTinOp));
			}
		case TINK_Integer:
			{
				STypeInfoInteger * pTinintLhs = (STypeInfoInteger *)pTinLhs;
				STypeInfoInteger * pTinintRhs = (STypeInfoInteger *)pTinRhs;

				if (pTinintRhs->m_fIsSigned != pTinintRhs->m_fIsSigned)
					return SOpTypes();

				auto pTinOp = pTinLhs;
				if (pTinintLhs->m_cBit < pTinintRhs->m_cBit)
				{
					if (parkOperator == PARK_AssignmentOp)
					{
						return SOpTypes();
					}
					pTinOp = pTinRhs;
				}
			
				return SOpTypes(pTinOp, pTinOp, PTinResult(parkOperator, pSymtab, pTinOp));
			}
		case TINK_Array:
			return SOpTypes();
		default:
			break;
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
		else if (parkOperator == PARK_ShiftOp && pTinOther->m_tink == TINK_Integer)
		{

			EWC_ASSERT(pTinEnum->m_tink == TINK_Enum && ((STypeInfoEnum*)pTinEnum)->m_pTinLoose, "expected loose type");
			return SOpTypes(pTinLhs, pTinRhs, ((STypeInfoEnum*)pTinEnum)->m_pTinLoose);
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
		case TINK_Pointer:
			{
				auto pTinptrSrc = (STypeInfoPointer *)pTinSrc;
				auto pTinptrDst = (STypeInfoPointer *)pTinDst;
				if (pTinptrDst->m_pTinPointedTo->m_tink == TINK_Void)
					return true;

				STypeInfo * pTinChildSrc = pTinptrSrc->m_pTinPointedTo;
				STypeInfo * pTinChildDst = pTinptrDst->m_pTinPointedTo;
				GRFQUALK grfqualkSrc;
				GRFQUALK grfqualkDst;
				if (pTinChildSrc->m_tink == TINK_Qualifier)
				{
					auto pTinqualSrc = (STypeInfoQualifier *)pTinChildSrc;
					grfqualkSrc = pTinqualSrc->m_grfqualk;
					pTinChildSrc = pTinqualSrc->m_pTin;
				}
				if (pTinChildDst->m_tink == TINK_Qualifier)
				{
					auto pTinqualDst = (STypeInfoQualifier *)pTinChildDst;
					grfqualkDst = pTinqualDst->m_grfqualk;
					pTinChildDst = pTinqualDst->m_pTin;
				}

				// can upcast to const/inarg, but not downcast
				grfqualkSrc.AddFlags(grfqualkDst);
				if (grfqualkDst != grfqualkSrc)
					return false;

				return FTypesAreSame(pTinChildSrc, pTinChildDst);	
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
		auto pTinPointedTo = PTinStripQualifiers(pTinptrDst->m_pTinPointedTo);
		if (pTinPointedTo->m_tink == TINK_Void)
			return true;
		return FTypesAreSame(pTinarySrc->m_pTin, pTinPointedTo);	
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

static inline bool FIsMutableType(STypeInfo * pTin)
{
	auto pTinqual = PTinRtiCast<STypeInfoQualifier *>(pTin);
	if (pTinqual)
	{
		return !pTinqual->m_grfqualk.FIsAnySet(FQUALK_Const | FQUALK_InArg);
	}
	return !pTin || pTin->m_tink != TINK_Literal;
}

inline bool FCanExplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst, CSymbolTable * pSymtab)
{
	if (pTinSrc->m_tink == TINK_Pointer && pTinDst->m_tink == TINK_Pointer)
	{
		auto pTinptrSrc = (STypeInfoPointer *)pTinSrc;
		auto pTinptrDst = (STypeInfoPointer *)pTinDst;

		return FIsMutableType(pTinptrSrc->m_pTinPointedTo) == true || FIsMutableType(pTinptrDst->m_pTinPointedTo) == false;
	}
	if (pTinSrc->m_tink == TINK_Procedure && pTinDst->m_tink == TINK_Procedure)
		return true;

	// Result of this cast is an RValue, we can step the const down a level
	pTinSrc = PTinQualifyAfterAssignment(pTinSrc, pSymtab);

	if (FIsNumericTink(pTinSrc->m_tink))
	{
		return FIsNumericTink(pTinDst->m_tink);
	}

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
		// BB - The only current exception to his is spoofed array members
		EWC_ASSERT(pStnod->m_park != PARK_Identifier, "Expected identifiers to have symbol");
		return false;
	}

	return pSym->m_grfsym.FIsSet(FSYM_IsType);
}

QUALK QualkFromRword(RWORD rword)
{
	switch (rword)
	{
	case RWORD_Const: return QUALK_Const;
	case RWORD_InArg: return QUALK_InArg;
	default: 
		EWC_ASSERT(false, "unexpected RWORD for qualk");
		return QUALK_Nil;
	}
}

bool FIsCompileTimeConstant(STypeInfo * pTin)
{
	// This just checks for a literal now, but will need something more elaborate once
	//  compile time code execution comes online.

	if (pTin && pTin->m_tink == TINK_Literal)
		return true;

	return false;
}


inline bool FFillOrderdAndNamedArgs(
	STypeCheckWorkspace * pTcwork,
	size_t cArgDefinition,
	CAry<CSTNode *> * pmpIArgPStnod,
	CAry<GRFARG> * pmpIArgGrfarg,
	ERREP errep,
	CSTNode * pStnodDefParamList,
	CSTNode ** ppStnodCall,
	size_t cpStnodCall, 
	SLexerLocation * pLexloc,
	const char * pChzOwner,
	const char * pChzStructOrProc)
{
	CDynAry<CString> mpIArgStrName(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cArgDefinition);
	mpIArgStrName.AppendFill(cArgDefinition, "_");

	// Fill out default arguments and build list of names
	if (pStnodDefParamList)
	{
		int cpStnodParamList = pStnodDefParamList->CStnodChild();
		for (int iArg = 0; iArg < cpStnodParamList; ++iArg)
		{
			CSTNode * pStnodParamDef = pStnodDefParamList->PStnodChildSafe(iArg);
			if (!pStnodParamDef)
				continue;

			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParamDef->m_pStmap);
			if (!pStdecl)
				continue;

			auto pStnodInit = pStnodParamDef->PStnodChildSafe(pStdecl->m_iStnodInit);
			if (pStnodInit)
			{
				(*pmpIArgPStnod)[iArg] = pStnodInit;
				(*pmpIArgGrfarg)[iArg].AddFlags(FARG_DefaultArg);
			}
			mpIArgStrName[iArg] = StrFromIdentifier(pStnodParamDef->PStnodChildSafe(pStdecl->m_iStnodIdentifier));
		}
	}
	// fill in ordered args and search for named arguments
	for (int iArg = 0; iArg < cpStnodCall; ++iArg)
	{
		int iArgDest = iArg;
		GRFARG grfarg;
		auto pStnodExp = ppStnodCall[iArg];

		if (pStnodExp->m_park == PARK_ArgumentLabel && 
			EWC_FVERIFY(pStnodExp->CStnodChild() == 2, "argument label node children should be (name, arg)"))
		{
			CSTNode * pStnodIdentifier = pStnodExp->PStnodChild(0);
			CString strIdentifier(StrFromIdentifier(pStnodIdentifier));

			int iArgNamed = -1;
			for (int iArgIt = 0; iArgIt < mpIArgStrName.C(); ++iArgIt)
			{
				if (mpIArgStrName[iArgIt] == strIdentifier)
				{
					iArgNamed = iArgIt;
					break;
				}
			}

			if (iArgNamed < 0)
			{
				if (errep == ERREP_ReportErrors)
				{
					EmitError(pTcwork->m_pErrman, pLexloc, ERRID_NamedArgumentNotFound,
						"Cannot find argument named %s for %s %s",
						strIdentifier.PCoz(),
						pChzStructOrProc,
						pChzOwner);
				}
				return false;
			}
			else
			{
				iArgDest = iArgNamed;
				grfarg.AddFlags(FARG_NamedLabelChild);
			}
		}

		if ((*pmpIArgPStnod)[iArgDest] != nullptr && !(*pmpIArgGrfarg)[iArgDest].FIsSet(FARG_DefaultArg))
		{
			if (errep == ERREP_ReportErrors)
			{
				EmitError(pTcwork->m_pErrman, pLexloc, ERRID_ArgumentSuppliedTwice,
					"Argument %d '%s' to %s %s was supplied twice: as an ordered argument and specified by name.",
					iArgDest + 1,
					mpIArgStrName[iArgDest].PCoz(),
					pChzStructOrProc,
					pChzOwner);
			}
			return false;
		}

		(*pmpIArgPStnod)[iArgDest] = pStnodExp; 
		(*pmpIArgGrfarg)[iArgDest] = grfarg;
	}

	for (int iArg = 0; iArg < cArgDefinition; ++iArg)
	{
		if ((*pmpIArgPStnod)[iArg] != nullptr)
			continue;
		if (errep == ERREP_ReportErrors)
		{
			EmitError(pTcwork->m_pErrman, pLexloc, ERRID_TooFewArgs,
				"Too few arguments to %s '%s'. cannot find value for parameter #%d: '%s'",
				pChzStructOrProc,
				pChzOwner,
				iArg + 1,
				mpIArgStrName[iArg].PCoz());
		}
		return false;
	}

	return true;
}

STypeInfo * PTinSubstituteGenerics(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	SLexerLocation * pLexloc,
	STypeInfo * pTinUnsub,
	SGenericMap * pGenmap,
	ERREP errep)
{
	// given known generics and un-substituted type, generate an instantiated type
	switch (pTinUnsub->m_tink)
	{
		case TINK_Generic:
			{
				auto pTingen = (STypeInfoGeneric *)pTinUnsub;
				SBakeValue * pBakval = pGenmap->m_mpPSymBakval.Lookup(pTingen->m_pStnodDefinition->m_pSym);
				if (!pBakval)
				{
					if (errep == ERREP_ReportErrors)
					{
						EmitError(pTcwork->m_pErrman, pLexloc, ERRID_GenericLookupFail,
							"Unable to pattern match instance type for generic value '$%s'", pTingen->m_strName.PCoz());
					}
					return pTingen;
				}

				EWC_ASSERT(pBakval->m_pTin, "expected baked type (not value)");
				return pBakval->m_pTin;
			}
    	case TINK_Integer:
    	case TINK_Float:
    	case TINK_Bool:
    	case TINK_Void:
    	case TINK_Null:
    	case TINK_Enum:
	    		return pTinUnsub;
	    case TINK_Pointer:
		    {
		    	auto pTinptr = (STypeInfoPointer *)pTinUnsub;
		    	auto pTinTarget = PTinSubstituteGenerics(pTcwork, pSymtab, pLexloc, pTinptr->m_pTinPointedTo, pGenmap, errep);
				return pSymtab->PTinptrAllocate(pTinTarget);
		    }
	    case TINK_Procedure:
		    {
		    	auto pTinprocUnsub = (STypeInfoProcedure *)pTinUnsub;
				auto pTinproc = PTinprocCopy(pSymtab, pTinprocUnsub);
				pTinproc->m_grftinproc.Clear(FTINPROC_HasGenericArgs);

				// collapse compile-time baked values

		    	auto cpTinParams = pTinproc->m_arypTinParams.C();
				int ipTinDst = 0;
				for (int ipTinSrc = 0; ipTinSrc < cpTinParams; ++ipTinSrc)
				{
					if (pTinprocUnsub->m_mpIptinGrfparmq[ipTinSrc].FIsSet(FPARMQ_BakedValue))
						continue;

					pTinproc->m_arypTinParams[ipTinDst] = PTinSubstituteGenerics(pTcwork, pSymtab, pLexloc, pTinproc->m_arypTinParams[ipTinSrc], pGenmap, errep);
					pTinproc->m_mpIptinGrfparmq[ipTinDst] = pTinprocUnsub->m_mpIptinGrfparmq[ipTinSrc];
					++ipTinDst;
				}

				while (pTinproc->m_arypTinParams.C() > ipTinDst)
				{
					pTinproc->m_arypTinParams.PopLast();
				}

		    	auto cpTinReturn = pTinproc->m_arypTinReturns.C();
				for (int ipTin = 0; ipTin < cpTinReturn; ++ipTin)
				{
					pTinproc->m_arypTinReturns[ipTin] = PTinSubstituteGenerics(pTcwork, pSymtab, pLexloc, pTinproc->m_arypTinReturns[ipTin], pGenmap, errep);
				}

				return pTinproc;
			}
	    case TINK_Struct:
		    {
				auto pTinstructUnsub = (STypeInfoStruct *)pTinUnsub;

				auto cTypememb = pTinstructUnsub->m_aryTypemembField.C();
				size_t cBAlloc = CBAlign(sizeof(STypeInfoStruct), EWC_ALIGN_OF(STypeStructMember)) + 
								cTypememb * sizeof(STypeStructMember);
				u8 * pB = (u8 *)pTcwork->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoStruct * pTinstruct = new(pB) STypeInfoStruct(pTinUnsub->m_strName, pTinUnsub->m_strUnique);
				pSymtab->AddManagedTin(pTinstruct);

				pTinstruct->m_pStnodStruct = pTinstructUnsub->m_pStnodStruct;
				auto aTypememb = (STypeStructMember*)PVAlign(
														pB + sizeof(STypeInfoStruct), 
														EWC_ALIGN_OF(STypeStructMember));
				pTinstruct->m_aryTypemembField.SetArray(aTypememb, 0, cTypememb);

				for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
				{
					STypeStructMember * pTypemembUnsub = &pTinstructUnsub->m_aryTypemembField[iTypememb];
					pTinstruct->m_aryTypemembField.Append(*pTypemembUnsub);

					if (!EWC_FVERIFY(pTypemembUnsub->m_pTin, "struct member type has not been determined"))
						continue;
					STypeStructMember * pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];
					pTypememb->m_pTin = PTinSubstituteGenerics(pTcwork, pSymtab, pLexloc, pTypemembUnsub->m_pTin, pGenmap, errep);
				}

		    	return pTinstruct;
		    }
	    case TINK_Array:
		    {
		    	auto pTinaryUnsub = (STypeInfoArray *)pTinUnsub;

		    	auto pTinaryNew = PTinaryCopy(pSymtab, pTinaryUnsub);
		    	pTinaryNew->m_pTin = PTinSubstituteGenerics(pTcwork, pSymtab, pLexloc, pTinaryUnsub->m_pTin, pGenmap, errep);

		    	return pTinaryNew;
		    }
		case TINK_Qualifier:
		    {
		    	auto pTinqual = (STypeInfoQualifier *)pTinUnsub;
		    	auto pTinTarget = PTinSubstituteGenerics(pTcwork, pSymtab, pLexloc, pTinqual->m_pTin, pGenmap, errep);
				return pSymtab->PTinqualEnsure(pTinTarget, pTinqual->m_grfqualk);
		    }
		default:
			EWC_ASSERT(false, "unhandled type info.");
			break;
	}

	return nullptr;
}

SInstantiateContext * PInsctxNew(STypeCheckWorkspace * pTcwork)
{
	SInstantiateContext * pInsctx = EWC_NEW(pTcwork->m_pAlloc, SInstantiateContext) SInstantiateContext();
	pTcwork->m_arypInsctxManaged.Append(pInsctx);
	return pInsctx;
}

SInstantiateRequest * PInsreqInstantiateGenericStruct(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGeneric,
	SGenericMap * pGenmap, 
	SInstantiateContext * pInsctx)
{
	auto pStstructSrc = PStmapRtiCast<CSTStruct *>(pStnodGeneric->m_pStmap);
	if (!EWC_FVERIFY(pStstructSrc, "expected procedure def"))
		return nullptr;

	// remap the types for the argument list and build symbols for them
	//CSTNode * pStnodDecl = pStnodGeneric->PStnodChildSafe(pStstructSrc->m_iStnodDeclList);

	EWC::CHash<CSTNode *, CSTNode *> mpPStnodGenPStnodCopy(pTcwork->m_pAlloc, BK_TypeCheckGenerics);

	auto pStnodStructCopy = PStnodCopy(pTcwork->m_pAlloc, pStnodGeneric, &mpPStnodGenPStnodCopy);
	pStnodStructCopy->m_grfstnod.Clear(FSTNOD_NoCodeGeneration);

	EWC_ASSERT(pStnodGeneric->m_strees = STREES_TypeChecked, "generic struct definition should be type checked prior to instantiation");
	pStnodStructCopy->m_strees = STREES_Parsed;

	auto pInsreq = pTcwork->m_aryInsreq.AppendNew();
	pInsreq->m_pGenmap = pGenmap;
	pInsreq->m_pStnodGeneric = pStnodGeneric;
	pInsreq->m_arypInsctx.SetAlloc(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	pInsreq->m_arypInsctx.Append(pInsctx);

	SInstantiateContextScope insctxscope(pTcwork->m_pErrman, pInsctx);

	// copy the symbol table, but replace generic types from the map

	// BB - is this really the right way to get the proc's symtab? might have a param list, might not.
	CSymbolTable * pSymtabSrc = pStnodGeneric->m_pSymtab;
	if (!EWC_FVERIFY(pSymtabSrc, "generic structure source has no symbol table"))
		return nullptr;

	CSymbolTable * pSymtabNew = PSymtabNew(pTcwork->m_pAlloc, pSymtabSrc, pSymtabSrc->m_strNamespace);	
	pSymtabNew->m_pSymtabParent = pSymtabSrc->m_pSymtabParent;

	// Remap the top level symbol table
	EWC::CHash<SSymbol *, CSTNode *> mpPSymSrcPStnodValue(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	EWC::CHash<SSymbol *, SSymbol *> mpPSymGenericPSymRemapped(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	EWC::CHash<HV, SSymbol *>::CIterator iterSrc(&pSymtabSrc->m_hashHvPSym);
	SSymbol ** ppSymSrc;
	while ((ppSymSrc = iterSrc.Next()))
	{
		SSymbol * pSymSrc = *ppSymSrc;
		EWC_ASSERT(pSymSrc->m_pSymPrev == nullptr, "not handing shadowed symbols"); // see PSymtabCopy

		if (EWC_FVERIFY(pSymSrc->m_pStnodDefinition, "symbol without defining syntax tree node"))
		{
			auto pStnodDefinition = pSymSrc->m_pStnodDefinition;
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pSymSrc->m_pStnodDefinition->m_pStmap);
			if (pStdecl && pStdecl->m_fIsBakedConstant)
			{
				auto pBakval = pGenmap->m_mpPSymBakval.Lookup(pSymSrc);
				if (!pBakval)
				{
					EmitError(pTcwork, pStnodGeneric, 
						"Couldn't find baked value for compile time constant '%s'", 
						pSymSrc->m_strName.PCoz());
				}
				else
				{
					EWC_ASSERT(pBakval->m_pStnod, "expected baked value (not type)");
					mpPSymSrcPStnodValue.Insert(pSymSrc, pBakval->m_pStnod);
						continue;
				}
			}
		}

		auto pSymNew = pSymtabNew->PSymEnsure(
			pTcwork->m_pErrman,
			pSymSrc->m_strName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pSymNew->m_pStnodDefinition, 
				"cannot look up definition stNode for symbol %s", 
				pSymNew->m_strName.PCoz());
		}
		else
		{
			pSymNew->m_pStnodDefinition = *ppStnodCopy;
		}

		if (pSymSrc->m_pTin)
		{
			pSymNew->m_pTin = PTinSubstituteGenerics(
								pTcwork,
								pSymtabNew,
								&pSymSrc->m_pStnodDefinition->m_lexloc,
								pSymSrc->m_pTin,
								pGenmap,
								ERREP_ReportErrors);
		}
		mpPSymGenericPSymRemapped.Insert(pSymSrc, pSymNew);
	}

	// build pTinstruct for the instantiated procedure

	auto pTinstructSrc = PTinDerivedCast<STypeInfoStruct *>(pStnodGeneric->m_pTin);

	auto cTypememb = pTinstructSrc->m_aryTypemembField.C();
	auto pTinstructNew = PTinstructAlloc(pSymtabNew, pTinstructSrc->m_strName, cTypememb, 0);

	for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
	{
		STypeStructMember * pTypemembUnsub = &pTinstructSrc->m_aryTypemembField[iTypememb];
		pTinstructNew->m_aryTypemembField.Append(*pTypemembUnsub);

		EWC_ASSERT(pTypemembUnsub->m_pTin == nullptr, "expected pTin to be unresolved");
	}

	for (int iTypememb = 0; iTypememb < pTinstructNew->m_aryTypemembField.C(); ++iTypememb)
	{
		auto pTypememb = &pTinstructNew->m_aryTypemembField[iTypememb];

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pTypememb->m_pStnod);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pTypememb->m_pStnod, 
				"cannot look up definition stNode for type member %s in %s", 
				pTypememb->m_strName.PCoz(),
				pTinstructSrc->m_strName.PCoz());
			continue;
		}
		pTypememb->m_pStnod = *ppStnodCopy;

	}

	pInsreq->m_pSym = pSymtabNew->PSymGenericInstantiate(pStnodGeneric->m_pSym, pTinstructNew);
	EWC_ASSERT(pInsreq->m_pSym, "null symbol");

	pStnodStructCopy->m_pTin = pTinstructNew;
	pStnodStructCopy->m_pSym = pInsreq->m_pSym;
	pInsreq->m_pSym->m_pStnodDefinition = pStnodStructCopy;
	pTinstructNew->m_pStnodStruct = pStnodStructCopy;

	auto pStstructCopy = PStmapRtiCast<CSTStruct *>(pStnodStructCopy->m_pStmap);
	pStstructCopy->m_iStnodParameterList = -1;

	for (int ipStnod = 0; ipStnod < pStnodStructCopy->CStnodChild(); ++ipStnod)
	{
		CSTNode * pStnodChild = pStnodStructCopy->PStnodChild(ipStnod);
		if (pStnodChild->m_pSymtab == pSymtabSrc)
		{
			pStnodChild->m_pSymtab = pSymtabNew;
		}
	}

	// type check the body with the new values

	if (!EWC_FVERIFY(pStstructSrc && pStstructSrc->m_iStnodDeclList >= 0, "bad pStnodGeneric"))
		return nullptr;	

	auto pStnodDeclSrc = pStnodGeneric->PStnodChild(pStstructSrc->m_iStnodDeclList);
	auto pStnodDeclCopy = pStnodGeneric->PStnodChild(pStstructCopy->m_iStnodDeclList);
	RemapGenericStnodCopy(
		pTcwork,
		pStnodGeneric,
		pStnodStructCopy,
		pGenmap,
		&mpPSymGenericPSymRemapped,
		&mpPSymSrcPStnodValue,
		&mpPStnodGenPStnodCopy,
		pSymtabSrc,
		pSymtabNew);

	STypeCheckFrame * pTcfram = pTcwork->m_blistTcfram.AppendNew();
	pTcfram->m_ipTcframQueue = pTcwork->m_arypTcframPending.C();
	pTcwork->m_arypTcframPending.Append(pTcfram);

	SWorkspaceEntry * pEntry = pTcwork->m_pblistEntry->AppendNew();
	pEntry->m_pStnod = pStnodStructCopy;
	pEntry->m_pSymtab = pSymtabNew;
	pTcfram->m_pEntry = pEntry;

	pTcfram->m_aryTcsent.SetAlloc(pTcwork->m_pAlloc, EWC::BK_TypeCheckStack);
	STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnodStructCopy;
	pTcsent->m_pSymtab = pSymtabNew;
	pTcsent->m_pStnodProcedure = nullptr;	// BB - how to find pStnodProcedure for pStnodGen
	pTcsent->m_pSymContext = pInsreq->m_pSym;
	pTcsent->m_grfsymlook = FSYMLOOK_Default;
	pTcsent->m_parkDeclContext = PARK_Nil;
	pTcsent->m_fAllowForwardDecl = false;
	pTcsent->m_tcctx = TCCTX_Normal;

	return pInsreq;
}

struct SMatchTypeInfo // tag = mtin
{
					SMatchTypeInfo()
					:m_pTinCall(nullptr)
					,m_pTinCallDefault(nullptr)
					,m_pTinParam(nullptr)
					,m_pStnodArg(nullptr)
						{ ; }

	STypeInfo *		m_pTinCall;
	STypeInfo *		m_pTinCallDefault;
	STypeInfo *		m_pTinParam;
	CSTNode *		m_pStnodArg;
	CSTNode *		m_pStnodLabel;
};

 bool FTryComputeMatchTypeInfo(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	SMatchTypeInfo * pMtin, 
	CSTNode * pStnodArg,
	STypeInfo * pTinParam,
	int	iStnodArg,
	GRFARG grfarg,
	GRFPARMQ grfparmq,
	ERREP errep)
{
	CSTNode * pStnodLabel = pStnodArg;
	if (grfarg.FIsSet(FARG_NamedLabelChild))
	{
		pStnodArg = pStnodArg->PStnodChildSafe(1);
	}

	if (FIsType(pStnodArg))
	{
		if (errep == ERREP_ReportErrors)
		{
			EmitError(pTcwork->m_pErrman, &pStnodArg->m_lexloc, ERRID_NotRvalue,
				"Procedure argument %d, Expected an instance, but encountered a type.",
				iStnodArg+1);
		}
		return false;
	}

	STypeInfo * pTinCall = pStnodArg->m_pTin;
	
	// Find the default literal promotion, as we need this to check for exact matches (which have precedence for matching)
	//  Things that can't default (void *) are problematic.

	if (pTinParam)
	{
		if (grfparmq.FIsSet(FPARMQ_ImplicitRef))
		{
			if (pTinParam->m_tink != TINK_Pointer)
				return false;

			if (!FVerifyIvalk(pTcwork, pStnodArg, IVALK_LValue))
			{
				EmitError(pTcwork->m_pErrman, &pStnodArg->m_lexloc, ERRID_NotLvalue,
					"Argument %d, must be an LValue for implicit conversion to pointer.",
					iStnodArg+1);
			}
			pTinParam = ((STypeInfoPointer*)pTinParam)->m_pTinPointedTo;
		}

		pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pStnodArg, pTinParam, errep);
	}

	STypeInfo * pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pStnodArg, pTinParam, errep);

	pMtin->m_pTinCall = pTinCall;
	pMtin->m_pTinCallDefault = pTinCallDefault;
	pMtin->m_pStnodArg = pStnodArg;
	pMtin->m_pStnodLabel = pStnodLabel;

	// we'll need to rebuild pTinParam once we know what all the generic types are
	pMtin->m_pTinParam = pTinParam;
	return true;
}

SGenericMap * PGenmapFromStructParameters(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	STypeInfoStruct * pTinstruct,
	CSTNode * pStnodStructInst,
	int ipStnodMin)
{
	auto pStnodStruct = pTinstruct->m_pStnodStruct;
	auto pStstruct = PStmapRtiCast<CSTStruct *>(pStnodStruct->m_pStmap);
	if (!EWC_FVERIFY(pStstruct, "expected ststruct") || pStstruct->m_iStnodParameterList < 0)
		return nullptr;

	auto pStnodDefParamList = pStnodStruct->PStnodChild(pStstruct->m_iStnodParameterList);
	int cParam = pStnodDefParamList->CStnodChild();

	// build a list of expected arguments and names
	CDynAry<CSTNode *> mpIArgPStnod(pTcwork->m_pAlloc, BK_TypeCheckGenerics, cParam);
	CDynAry<GRFARG> mpIArgGrfarg(pTcwork->m_pAlloc, BK_TypeCheckGenerics, cParam);
	CDynAry<CString> aryStrArgName(pTcwork->m_pAlloc, BK_TypeCheckGenerics, cParam);
	aryStrArgName.AppendFill(cParam, "_");
	mpIArgPStnod.AppendFill(cParam, nullptr);
	mpIArgGrfarg.AppendFill(cParam, FARG_None);

	CSTNode ** ppStnodCall = &pStnodStructInst->m_arypStnodChild[ipStnodMin];
	size_t cpStnodCall = pStnodStructInst->CStnodChild() - ipStnodMin; 

	if (!FFillOrderdAndNamedArgs(
		pTcwork,
		cParam,
		&mpIArgPStnod,
		&mpIArgGrfarg,
		ERREP_ReportErrors,
		pStnodDefParamList,
		ppStnodCall,
		cpStnodCall,
		&pStnodStruct->m_lexloc,
		pTinstruct->m_strName.PCoz(),
		"struct"))
	{
		return nullptr;
	}
		
	CDynAry<SMatchTypeInfo> aryMtin(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cpStnodCall);

	SGenericMap genmap(pTcwork->m_pAlloc, pStnodStruct->m_pSym);
	for (int iArg = 0; iArg < cParam; ++iArg)
	{
		CSTNode * pStnodArg = mpIArgPStnod[iArg];
		auto pMtin = aryMtin.AppendNew();

		STypeInfo * pTinParam = pTinstruct->m_arypTinGenericParam[iArg];
		if (!EWC_FVERIFY(pTinParam, "unknown parameter type"))
			return nullptr;

		if (!FTryComputeMatchTypeInfo(
			pTcwork,
			pSymtab,
			pMtin,
			pStnodArg,
			pTinParam,
			iArg,
			mpIArgGrfarg[iArg],
			FPARMQ_None,
			ERREP_ReportErrors))
		{
			return nullptr;
		}
	}

	CHash<HV, CSTNode *> mpHvPStnodAnchor(pTcwork->m_pAlloc, BK_TypeCheckProcmatch);
	if (pTinstruct->FHasGenericParams())
	{
		// compute types for generic type 'anchors'

		for (int ipStnodParam = 0; ipStnodParam < cpStnodCall; ++ipStnodParam)
		{
			auto pStnodDecl = pStnodDefParamList->PStnodChild(ipStnodParam);
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);

			if (pStnodDecl->m_park != PARK_Decl || pStdecl == nullptr)
				continue;

			if (pStdecl->m_fIsBakedConstant && EWC_FVERIFY(pStnodDecl->m_pSym, "expected symbol for baked value"))
			{
				auto pStnodArg = aryMtin[ipStnodParam].m_pStnodArg;
				if (!FIsCompileTimeConstant(pStnodArg->m_pTin))
				{
					auto pStnodIdent = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodIdentifier);

					EmitError(pTcwork->m_pErrman, &pStnodStruct->m_lexloc, ERRID_BakingNonLiteralValue,
						"passing non-constant to argument %d of procedure '%s'. '%s' must be a compile-time constant",
						ipStnodParam + 1,
						pTinstruct->m_strName.PCoz(),
						StrFromIdentifier(pStnodIdent).PCoz());
					return nullptr;
				}

				mpIArgPStnod[ipStnodParam] = aryMtin[ipStnodParam].m_pStnodLabel;
				mpIArgGrfarg[ipStnodParam].AddFlags(FARG_BakedValue);
				genmap.m_mpPSymBakval.Insert(pStnodDecl->m_pSym, SBakeValue(pStnodArg));
			}
			else
			{
				auto pStnodIdent = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
				EmitError(pTcwork->m_pErrman, &pStnodStruct->m_lexloc, ERRID_StructParamsMustBeBaked,
					"generic structure %s has non compile-time constant parameter '%s'",
					pTinstruct->m_strName.PCoz(),
					StrFromIdentifier(pStnodIdent).PCoz());
			}

			auto pStnodType = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodType);
			if (!EWC_FVERIFY(pStnodType, "Encountered decl without type syntax tree node"))
				continue;

			// given a reference type and a generic type specification compute the anchored genric types
			ERRID errid = ErridComputeDefinedGenerics(
				pTcwork,
				ERREP_ReportErrors,
				aryMtin[ipStnodParam].m_pTinCallDefault,
				pStnodType,
				&genmap,
				&mpHvPStnodAnchor);
			if (errid != ERRID_Nil)
				return nullptr;
		}

		int cMtin = (int)aryMtin.C();
		for (int iMtin = 0; iMtin < cMtin; ++iMtin)
		{
			auto pMtin = &aryMtin[iMtin];
			pMtin->m_pTinParam = PTinSubstituteGenerics(pTcwork, pSymtab, &pStnodStruct->m_lexloc, aryMtin[iMtin].m_pTinParam, &genmap, ERREP_ReportErrors);
			pMtin->m_pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam);
			pMtin->m_pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam, ERREP_ReportErrors);
		}
	}

	for (int iStnodArg = 0; iStnodArg < cpStnodCall; ++iStnodArg)
	{
		// This behavior can be a bit confusing when we're calling an overloaded function with a numeric literal
		//  we consider the overload an exact match when the default promotion matches exactly, we can't use the tightest
		//  promotion because that would exact match all implicit numeric conversions (ie. 2 tightest matches to both int and float)

		auto pMtin = &aryMtin[iStnodArg];
		if (FTypesAreSame(pMtin->m_pTinCallDefault, pMtin->m_pTinParam))
			continue;

		if (!FCanImplicitCast(pMtin->m_pTinCall, pMtin->m_pTinParam))
		{
			SInstantiateContext insctx;
			insctx.m_pGenmap = &genmap;
			insctx.m_lexlocCall = pStnodStruct->m_lexloc;
			SInstantiateContextScope insctxscope(pTcwork->m_pErrman, (genmap.FIsEmpty()) ? nullptr : &insctx);

			CString strTinCall = StrFromTypeInfo(pMtin->m_pTinCall);
			CString strTinParam = StrFromTypeInfo(pMtin->m_pTinParam);
			EmitError(pTcwork->m_pErrman, &pStnodStruct->m_lexloc, ERRID_BadImplicitConversion,
				"generic structure '%s' cannot convert argument %d from type %s to %s",
				pTinstruct->m_strName.PCoz(),
				iStnodArg + 1,
				strTinCall.PCoz(),
				strTinParam.PCoz());

			return nullptr;
		}
	}

	if (genmap.FIsEmpty())
		return nullptr;

	SGenericMap * pGenmapReturn = EWC_NEW(pTcwork->m_pAlloc, SGenericMap) SGenericMap(pTcwork->m_pAlloc, nullptr);
	pGenmapReturn->Swap(&genmap);
	return pGenmapReturn;
}



SSymbol * PSymInstantiateGenericStruct(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodStructInst,
	GRFSYMLOOK grfsymlook)
{
	CSTNode * pStnodIdent = pStnodStructInst->PStnodChildSafe(0);
	SSymbol * pSymGen = nullptr;
	if (EWC_FVERIFY(pStnodIdent, "bad generic struct instantiation"))
	{
		auto strIdent = StrFromIdentifier(pStnodIdent);
		pSymGen = pSymtab->PSymLookup(strIdent, pStnodIdent->m_lexloc, grfsymlook);
		if (!pSymGen)
		{
			EmitError(pTcwork, pStnodStructInst, "failed looking up generic struct '%s'", strIdent.PCoz());
		}
	}
	
	if (!pSymGen)
		return nullptr;

	if (!pSymGen->m_grfsym.FIsSet(FSYM_IsType))
	{
		EmitError(pTcwork, pStnodStructInst, "Expected type specification but encountered '%s'", pSymGen->m_strName.PCoz());
		return nullptr;
	}

	EWC_ASSERT(pSymGen->m_pTin, "generic struct symbol has no type info");
	STypeInfoStruct * pTinstruct = PTinRtiCast<STypeInfoStruct *>(pSymGen->m_pTin);
	if (!pTinstruct || !pTinstruct->FHasGenericParams())
	{
		EmitError(pTcwork, pStnodStructInst, "'%s' is not a generic struct and cannot be instantiated with arguments", pSymGen->m_strName.PCoz());
		return nullptr;
	}

	auto pGenmap = PGenmapFromStructParameters(pTcwork, pSymtab, pTinstruct, pStnodStructInst, 1); 
	if (!pGenmap)
		return nullptr;

	auto pInsctxNew = PInsctxNew(pTcwork);
	pInsctxNew->m_pGenmap = pGenmap;
	pInsctxNew->m_lexlocCall = pStnodStructInst->m_lexloc;

	auto pInsreq = PInsreqLookup(pTcwork, pSymGen->m_pStnodDefinition, pGenmap, pInsctxNew);
	if (pInsreq)
	{
		pGenmap->Cleanup(pTcwork->m_pAlloc);
		pTcwork->m_pAlloc->EWC_DELETE(pGenmap);
		pGenmap = nullptr;
	}
	else
	{
		pInsreq = PInsreqInstantiateGenericStruct(
					pTcwork,
					pSymGen->m_pStnodDefinition,
					pGenmap,
					pInsctxNew);

		// 'unhook' the genmap because it will be referenced by the insreq 
		pGenmap = nullptr;
	}

	if (!pInsreq)
		return nullptr;

	return pInsreq->m_pSym;
}

STypeInfo * PTinFromTypeSpecification(
	STypeCheckWorkspace * pTcwork,
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
		case PARK_GenericStructInst:
			{
				EWC_ASSERT(pStnodIt->m_strees == STREES_TypeChecked, "generic inst needs to by type checked first");

				pTinFinal = nullptr;
				if (pStnodIt->m_pSym && pStnodIt->m_pSym->m_pTin)
				{
					pTinFinal = pStnodIt->m_pSym->m_pTin;
				}
				else
				{
					*pFIsValidTypeSpec = false;
				}
				
				pStnodIt->m_pTin = pTinFinal;
				pStnodIt = nullptr;
			} break;
		case PARK_Identifier:
			{
				auto strIdent = StrFromIdentifier(pStnodIt);
				auto pSym = pSymtab->PSymLookup(strIdent, pStnodIt->m_lexloc, grfsymlook);

				if (!pSym->m_grfsym.FIsSet(FSYM_IsType))
				{
					EmitError(pTcwork, pStnodIt, "Expected type specification but encountered '%s'", strIdent.PCoz());
					*pFIsValidTypeSpec = false;
				}

				EWC_ASSERT(pSym && pSym->m_pTin, "bad type identifier in type specification");

				auto pTinstruct = PTinRtiCast<STypeInfoStruct *>(pSym->m_pTin);
				if (pTinstruct && pTinstruct->FHasGenericParams())
				{
					EmitError(pTcwork, pStnodIt, "Generic struct '%s' needs argument list for instantiation", strIdent.PCoz());
					*pFIsValidTypeSpec = false;
				}
				
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
		case PARK_QualifierDecl:
			{
				EWC_ASSERT(pStnodIt->CStnodChild() == 1, "expected one child");
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		case PARK_ReferenceDecl:
			{
				fAllowForwardDecl |= true;
				EWC_ASSERT(pStnodIt->CStnodChild() == 1, "expected one child");
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		case PARK_ProcedureReferenceDecl:
			{
				pTinFinal = pStnodIt->m_pTin;
				EWC_ASSERT(pTinFinal, "Expected pTinproc before PTinFromTypeSpecification");
				pStnodIt = nullptr;
			} break;
		case PARK_GenericDecl:
			{ 
				pTinFinal = pStnodIt->m_pTin;
				EWC_ASSERT(pTinFinal, "Expected pTingen before PTinFromTypeSpecification");
				pStnodIt = nullptr;
			} break;
		default: EWC_ASSERT(false, "Unexpected parse node %s in PTinFromTypeSpecification", PChzFromPark(pStnod->m_park));
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
		case PARK_QualifierDecl:
			{
				QUALK qualk = QualkFromRword(PStvalExpected(pStnodIt)->m_rword);
				GRFQUALK grfqualk = 0x1 << qualk;
				
				if (auto pTinqualPrev = PTinRtiCast<STypeInfoQualifier *>(pTinPrev))
				{
					pTinqualPrev->m_grfqualk.AddFlags(grfqualk);
					pStnodIt->m_pTin = pTinqualPrev;
				}
				else
				{
					auto pTinqual = pSymtab->PTinqualEnsure(nullptr, grfqualk);
					pStnodIt->m_pTin = pTinqual;

					pTinPrev = pTinqual;
					*ppTinCur = pTinqual;
					ppTinCur = &pTinqual->m_pTin;
				}

				EWC_ASSERT(pStnodIt->CStnodChild() == 1, "expected one child");
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		case PARK_ReferenceDecl:
			{
				auto pTinptr = pSymtab->PTinptrAllocate(nullptr);
				pStnodIt->m_pTin = pTinptr;

				pTinPrev = pTinptr;
				*ppTinCur = pTinptr;
				ppTinCur = &pTinptr->m_pTinPointedTo;

				EWC_ASSERT(pStnodIt->CStnodChild() == 1, "expected one child");
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		case PARK_ArrayDecl:
			{
				STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();

				*ppTinCur = pTinary;
				ppTinCur = &pTinary->m_pTin;

				if (pStnodIt->CStnodChild() == 2)
				{
					CSTNode * pStnodDim = pStnodIt->PStnodChild(0);
					STypeInfoLiteral * pTinlitDim = (STypeInfoLiteral *)pStnodDim->m_pTin;
					CSTValue * pStvalDim = nullptr;
					if (!FIsCompileTimeConstant(pTinlitDim))
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
							FinalizeLiteralType(pTcwork, pSymtab, pTinCount, pStnodDim);
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
					pTinary->m_aryk = (pStnodIt->m_tok == TOK_PeriodPeriod) ? ARYK_Dynamic : ARYK_Reference;
				}

				if (pTinary->m_aryk == ARYK_Dynamic)
				{
					EmitError(pTcwork, pStnod, ERRID_NotYetSupported, "Dynamic arrays are not yet supported");
				}

				pSymtab->AddManagedTin(pTinary);

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
		case PARK_GenericDecl:
		case PARK_GenericStructInst:
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
		default:
			{
				EWC_ASSERT(false, "unexpected parse node kind. PARK_%s", PChzFromPark(pStnodIt->m_park));
			} break;
		}
	}

	pTinReturn = pSymtab->PTinMakeUnique(pTinReturn);
	return pTinReturn;
}

STypeInfo * PTinReturnFromStnodProcedure(CSTNode * pStnod)
{
	auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnod->m_pStmap);
	if (!EWC_FVERIFY(pStnod->m_park == PARK_ProcedureDefinition && pStproc, "Bad procedure node"))
		return nullptr;
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

const char * PChzFromIvalk(IVALK ivalk)
{
	static const char * s_mpIvalkPChz[] =
	{
		"Error",
		"Type",
		"R-Value",
		"L-Value",
	};
	EWC_CASSERT(EWC_DIM(s_mpIvalkPChz) == IVALK_Max, "missing IVALK string");
	if (ivalk == IVALK_Nil)
		return "Nil";

	if ((ivalk < IVALK_Nil) | (ivalk >= IVALK_Max))
		return "Unknown IVALK";

	return s_mpIvalkPChz[ivalk];
}

IVALK IvalkCompute(CSTNode * pStnod)
{
	if (pStnod->m_park == PARK_MemberLookup)
	{
		// if the lhs is a type this is not an lvalue, check for constant rvalues
		CSTNode * pStnodLhs = pStnod->PStnodChildSafe(0);
		CSTNode * pStnodRhs = pStnod->PStnodChildSafe(1);
		if (!EWC_FVERIFY((pStnodLhs != nullptr) & (pStnodRhs != nullptr), "invalid member lookup"))
			return IVALK_Error;

		// BB - we should have symbol tables for arrays and this should work like any other symbol
		STypeInfo * pTinLhs = pStnodLhs->m_pTin;

		bool lhsIsArray = pTinLhs && pTinLhs->m_tink == TINK_Array;
		if (auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pTinLhs))
		{
			lhsIsArray = pTinlit->m_litty.m_litk == LITK_Array;
		}

		if (lhsIsArray)
		{
			return IVALK_RValue;
		}

		// We currently allow using lvalues to specify an R-Value as it gets tricky to specify array R-Values otherwise
		// ie. SType.m_inst.kConstant is the same as SType.SInstType.kConstant, so we can say SType.m_aN.count
		auto ivalkLhs = IvalkCompute(pStnodLhs);
		auto ivalkRhs = IvalkCompute(pStnodRhs);
		if ((ivalkLhs == IVALK_Type && ivalkRhs == IVALK_LValue) ||
			(ivalkLhs == IVALK_Error && ivalkRhs != IVALK_RValue))
		{
			// (type, (inst, m_val)) -> IVALK_Error
			return IVALK_Error;
		}
		return ivalkRhs;
	}
	else if (pStnod->m_park == PARK_Cast)
	{
		if (!pStnod->m_pTin || pStnod->m_pTin->m_tink != TINK_Pointer)
		{
			return IVALK_RValue;
		}
	}
	else if (pStnod->m_park == PARK_ArrayElement)
	{
		auto pStnodArray = pStnod->PStnodChild(0);
		if (!FIsMutableType(pStnodArray->m_pTin))
		{
			return IVALK_RValue;
		}
		return (pStnodArray->m_pTin->m_tink == TINK_Literal) ? IVALK_RValue : IVALK_LValue;
	}
	else if (pStnod->m_park == PARK_UnaryOp && pStnod->m_tok == TOK_Dereference)
	{

		return (FIsMutableType(pStnod->m_pTin)) ? IVALK_LValue : IVALK_RValue;
	}

	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		return IVALK_RValue;

	auto pSym = pStnod->m_pSym;
	if (!pSym)
	{
		EWC_ASSERT(pStnod->m_park != PARK_Identifier, "Expected identifiers to have symbol");
		return IVALK_RValue;
	}
	else if (pSym->m_grfsym.FIsSet(FSYM_IsType))
	{
		return IVALK_Type;
	}
	else if (pSym->m_grfsym.FIsSet(FSYM_VisibleWhenNested))
	{
		return IVALK_RValue;
	}

	return (FIsMutableType(pStnod->m_pTin)) ? IVALK_LValue : IVALK_RValue;
}


CString StrFromStnod(CAlloc * pAlloc, CSTNode * pStnod)
{
	switch (pStnod->m_park)
	{
	case PARK_MemberLookup:
		{
			EWC::SStringEditBuffer seb(pAlloc);
			CSTNode * pStnodLhs = pStnod->PStnodChildSafe(0);
			CSTNode * pStnodRhs = pStnod->PStnodChildSafe(1);
			seb.AppendCoz(StrFromStnod(pAlloc, pStnodLhs).PCoz());
			seb.AppendCoz(".");
			seb.AppendCoz(StrFromIdentifier(pStnodRhs).PCoz());
			return CString(seb.PCoz());
		}
	case PARK_Identifier:
		return StrFromIdentifier(pStnod);
	case PARK_ProcedureCall:
		{
			EWC::SStringEditBuffer seb(pAlloc);
			seb.AppendCoz("Procedure Call");
			CSTNode * pStnodName = pStnod->PStnodChildSafe(0);
			if (pStnodName && pStnodName->m_park == PARK_Identifier)
			{
				seb.AppendCoz(" '");
				seb.AppendCoz(StrFromIdentifier(pStnodName).PCoz());
				seb.AppendCoz("'");
			}
			return CString(seb.PCoz());
		}
	default:
		break;
	}

	return CString("");
}

bool FVerifyIvalk(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, IVALK ivalkExpected)
{
	auto ivalkActual = IvalkCompute(pStnod);
	if (ivalkActual < ivalkExpected)
	{
		const char * pChzIvalk = PChzFromIvalk(ivalkExpected);
		CString strLhs = StrFromStnod(pTcwork->m_pAlloc, pStnod);
		CString strTin = StrFromTypeInfo(pStnod->m_pTin);
		EmitError(pTcwork, pStnod, ERRID_IncorrectIvalk, "'%s%s%s' is not a valid %s", 
			strLhs.PCoz(), 
			(strLhs.FIsEmpty()) ? "" : ": ",
			strTin.PCoz(), 
			pChzIvalk);

		return false;
	}

	return true;
}

void SetEnumConstantValue(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, const SBigInt & bint)
{
	if (!EWC_FVERIFY(pStnod, "expected pStnod for enum constant"))
		return;

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

	auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodConstant->m_pStmap);
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

	OnTypeResolve(pTcwork, pSym);
}

void SpoofLiteralArray(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, CSTNode * pStnodArray, int cElements, STypeInfo * pTinElement)
{
	auto pStdeclArray = PStmapRtiCast<CSTDecl *>(pStnodArray->m_pStmap);
	if (!EWC_FVERIFY(pStdeclArray && pStnodArray->m_pSym, "bad spoofed literal array"))
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
	pStnodList->m_park = PARK_ExpressionList;
	pStnodList->m_pTin = pTinlit;

	EWC_ASSERT(pStdeclArray->m_iStnodInit == -1, "expected empty array");
	pStdeclArray->m_iStnodInit = pStnodArray->IAppendChild(pStnodList);
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
		pUntype->m_arypTcframDependent.Append(pTcfram);
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

void AddSymbolReference(SSymbol * pSymContext, SSymbol * pSymTarget)
{
	if (pSymContext == pSymTarget)
		return;

	SSymbol ** ppSymMac = pSymTarget->m_aryPSymReferencedBy.PMac();
	for (SSymbol ** ppSymIt = pSymTarget->m_aryPSymReferencedBy.A(); ppSymIt != ppSymMac; ++ppSymIt)
	{
		if (*ppSymIt == pSymContext)
			return;
	}

	if (EWC_FVERIFY(pSymContext, "missing symbol context for %s", pSymTarget->m_strName.PCoz()))
	{
		pSymTarget->m_aryPSymReferencedBy.Append(pSymContext);
	}
}



void AdjustArgumentOrder(ARGORD argord, CSTNode * pStnod, SOpTypes * pOptype)
{
	if (argord != ARGORD_Reversed)
		return;

	pStnod->m_grfstnod.AddFlags(FSTNOD_CommutativeCall);
	auto pTin = pOptype->m_pTinLhs;
	pOptype->m_pTinLhs = pOptype->m_pTinRhs;
	pOptype->m_pTinRhs = pTin;
}

struct SProcMatchFit // tag pmfit
{
						SProcMatchFit(CAlloc * pAlloc)
						:m_mpIArgPStnod(pAlloc, BK_TypeCheckProcmatch)
						,m_mpIArgGrfarg(pAlloc, BK_TypeCheckProcmatch)
						,m_pGenmap(nullptr)
							{ ; }

						~SProcMatchFit()
						{
							CAlloc * pAlloc = m_mpIArgPStnod.m_pAlloc;
							if (m_pGenmap)
							{
								pAlloc->EWC_DELETE(m_pGenmap);	
								m_pGenmap = nullptr;
							}
						}

	CDynAry<CSTNode *>	m_mpIArgPStnod;		// calling arguments with named arg and defaults resolved
	CDynAry<GRFARG>		m_mpIArgGrfarg;
	SGenericMap *		m_pGenmap;
};

struct SProcMatchParam // tag = pmparam
{
						SProcMatchParam(CAlloc * pAlloc, SLexerLocation * pLexloc)
						:m_pAlloc(pAlloc)
						,m_pLexloc(pLexloc)
						,m_ppStnodCall(nullptr)
						,m_cpStnodCall(0)
						,m_pPmfit(nullptr)
						,m_fMustFindMatch(false)
							{ ; }

						~SProcMatchParam()
							{
								if (m_pPmfit)
								{
									m_pAlloc->EWC_DELETE(m_pPmfit);	
									m_pPmfit = nullptr;
								}
							}

	CAlloc *			m_pAlloc;
	SLexerLocation *	m_pLexloc;
	CSTNode **			m_ppStnodCall;		// actual arguments passed to the call, no default/named args
	size_t				m_cpStnodCall;		// no default/named args

	SProcMatchFit *		m_pPmfit;

	bool				m_fMustFindMatch;
};

inline const char * PChzProcName(STypeInfo * pTin, SSymbol * pSym)
{
	if (!pTin->m_strName.FIsEmpty())
		return pTin->m_strName.PCoz();

	if (pSym)
		return pSym->m_strName.PCoz();
	return "unnamed";
}

void PrintHexDigit(u8 b)
{
	const char * str = "0123456789ABCDEF";
	printf("%c", str[(b & 0x0F)]);
}

void PrintMem(void * pV, size_t cB)
{
	u8 * pB = (u8 *)pV;
	u8 * pBEnd = pB + cB;
	while (pB != pBEnd)
	{
		//BB - this is completely backwards, reversed byte order, reversed word order
		printf(" 0x%p:", pB);
		for (int iWord = 0; iWord < 4 && pB != pBEnd; ++iWord)
		{
			printf(" ");
			for (int iByte = 0; iByte < 4 && pB != pBEnd; ++iByte)
			{
				u8 b = *pB++;
				PrintHexDigit((b & 0xF0)>>4);
				PrintHexDigit(b);
			}
		}
		printf("\n");
	}
}

ERRID ErridComputeDefinedGenerics(
	STypeCheckWorkspace * pTcwork,
	ERREP errep,
	STypeInfo * pTinRef,
	CSTNode * pStnodDef,
	SGenericMap * pGenmap,
	CHash<HV, CSTNode *> * pmpHvPStnodAnchor)
{
	// given a reference type and a generic type specification compute the anchored genric types

	struct SGenericFrame // tag = genfram
	{
		CSTNode * 		m_pStnod;
		STypeInfo *		m_pTinRef;
	};

	CDynAry<SGenericFrame> aryGenfram(pTcwork->m_pAlloc, BK_TypeCheckGenerics, 16);

	{
		auto pGenfram = aryGenfram.AppendNew();
		pGenfram->m_pStnod = pStnodDef;
		pGenfram->m_pTinRef = pTinRef;
	}

	EWC::CHash<SSymbol *, STypeInfo *> mpPSymPTinRemapped(pTcwork->m_pAlloc, BK_TypeCheckGenerics);

	// walk through 
	while (aryGenfram.C())
	{
		auto genfram = aryGenfram.TPopLast();
		auto pStnodIt = genfram.m_pStnod;

		EWC_ASSERT(pStnodIt->m_strees == STREES_TypeChecked, "Type specification should be type checked first, (for literal op eval)");
		while (pStnodIt)
		{

			CSTNode * pStnodCur = pStnodIt;
			pStnodIt = nullptr;	

			switch(pStnodCur->m_park)
			{
			case PARK_GenericDecl:
				{ 
					auto pTingen = PTinDerivedCast<STypeInfoGeneric *>(pStnodCur->m_pTin);

					auto pSym = pStnodCur->m_pSym;
					if (!EWC_FVERIFY(pSym->m_pTin && pSym->m_pTin->m_tink == TINK_Generic, "expected generic type"))
						break;

					CSTNode ** ppStnodAnchor;
					auto fins = pmpHvPStnodAnchor->FinsEnsureKey(pTingen->m_strName.Hv(), &ppStnodAnchor);

					if (fins == FINS_AlreadyExisted)
					{
						EWC_ASSERT(false, "multiple anchor points should have been found during parse");
						return ERRID_MultipleAnchorDef;
					}
					else
					{
						*ppStnodAnchor = pStnodCur;
					}

					mpPSymPTinRemapped.Insert(pSym, genfram.m_pTinRef);
				} break;
			case PARK_MemberLookup:
				{
					// I *think* there aren't any member lookup cases that can contain another type decl
					/*
					EWC_ASSERT(pStnodCur->CStnodChild() == 2, "Expected Lhs.Rhs in PARK_MemberLookup");
					CSTNode * pStnodIdent = pStnodCur->PStnodChild(0);
					auto strIdent = StrFromIdentifier(pStnodIdent);
					auto pSym = pSymtab->PSymLookup(strIdent, pStnodIdent->m_lexloc, grfsymlook);
					EWC_ASSERT(pSym && pSym->m_pStnodDefinition, "bad outer type in type specification");

					CSTNode * pStnodDefinition = pSym->m_pStnodDefinition;
					if (EWC_FVERIFY(pStnodDefinition->m_pSymtab, "Struct without symbol table"))
					{
						pSymtab = pStnodDefinition->m_pSymtab;
					}

					pStnodCur = pStnodIt->PStnodChild(1);
					*/
				} break;
			case PARK_Identifier:
				{
				} break;
			case PARK_Decl:
				{ 
					auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodCur->m_pStmap);
					if (!EWC_FVERIFY(pStdecl, "expected declaration"))
						break;

					pStnodIt = pStnodCur->PStnodChildSafe(pStdecl->m_iStnodType);
				} break;
			case PARK_ArrayDecl:
				{
					auto pTinaryRef = PTinRtiCast<STypeInfoArray *>(genfram.m_pTinRef);
					if (pTinaryRef)
					{
						genfram.m_pTinRef = pTinaryRef->m_pTin;

						// array decl's children are [type] or [m_c, type]
						pStnodIt = pStnodCur->PStnodChild(pStnodCur->CStnodChild()-1);
						EWC_ASSERT(pStnodIt, "bad array declaration");
					}
				} break;
			case PARK_QualifierDecl:
				{
					auto pTinqualRef = PTinRtiCast<STypeInfoQualifier *>(genfram.m_pTinRef);
					if (pTinqualRef)
					{
						genfram.m_pTinRef = pTinqualRef->m_pTin;
						EWC_ASSERT(pStnodCur->CStnodChild() == 1, "expected one child");
						pStnodIt = pStnodCur->PStnodChild(0);
					}
				} break;
			case PARK_ReferenceDecl:
				{
					auto pTinptrRef = PTinRtiCast<STypeInfoPointer *>(genfram.m_pTinRef);
					if (pTinptrRef)
					{
						genfram.m_pTinRef = pTinptrRef->m_pTinPointedTo;

						EWC_ASSERT(pStnodCur->CStnodChild() == 1, "expected one child");
						pStnodIt = pStnodCur->PStnodChild(0);
					}
				} break;
			case PARK_ProcedureReferenceDecl:
				{
					auto pTinprocRef = PTinRtiCast<STypeInfoProcedure *>(pTinRef);
					auto pTinprocGen = PTinRtiCast<STypeInfoProcedure *>(pStnodCur->m_pTin);

					if (!pTinprocRef)
						break;
				
					if (pTinprocRef->FHasGenericArgs())
					{
						if (errep == ERREP_ReportErrors)
						{
							EmitError(pTcwork, pStnodCur, ERRID_NoGenericRValue,
								"cannot make a reference to a generic procedure definition '%s'",
								pTinprocRef->m_strName.PCoz());
						}

						return ERRID_NoGenericRValue;
					}

					auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnodCur->m_pStmap);
					if (EWC_FVERIFY(pStproc && pTinprocGen, "bad PARK_ProcedureReferenceDecl") &&
						pTinprocRef && 
						pTinprocGen->FHasGenericArgs() &&
  						pStproc->m_iStnodParameterList >= 0)
					{
						if (pTinprocRef->m_arypTinParams.C() == pTinprocGen->m_arypTinParams.C() &&
							pTinprocRef->m_arypTinReturns.C() == pTinprocGen->m_arypTinReturns.C() &&
							pTinprocRef->FHasVarArgs() == pTinprocGen->FHasVarArgs()) 
						{

							CSTNode * pStnodParameterList = pStnodCur->PStnodChild(pStproc->m_iStnodParameterList);
							for (int ipStnod = 0; ipStnod < pStnodParameterList->CStnodChild(); ++ipStnod)
							{
								CSTNode * pStnodParam = pStnodParameterList->PStnodChild(ipStnod);
								if (pStnodParam->m_park == PARK_VariadicArg)
									continue;

								auto pGenfram = aryGenfram.AppendNew();
								pGenfram->m_pStnod = pStnodParam;
								pGenfram->m_pTinRef = pTinprocRef->m_arypTinParams[ipStnod];
							}
						}
					}
				} break;
			default: EWC_ASSERT(false, "Unexpected parse node PARK_%s in FComputeDefinedGenerics", PChzFromPark(pStnodCur->m_park));
				break;
			}
		}
	}

	EWC::CHash<SSymbol *, STypeInfo *>::CIterator iter(&mpPSymPTinRemapped);
	SSymbol ** ppSym;
	STypeInfo ** ppTin;
	while ((ppTin = iter.Next(&ppSym)))
	{
		pGenmap->m_mpPSymBakval.Insert(*ppSym, SBakeValue(*ppTin));
	}
	return ERRID_Nil;
}
PROCMATCH ProcmatchCheckArguments(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	STypeInfoProcedure * pTinproc,
	SProcMatchParam * pPmparam,
	ERREP errep,
	ARGORD argord,
	SSymbol * pSymProc)
{
	size_t cArgCall = pPmparam->m_cpStnodCall;
	if 	(!pTinproc->FHasVarArgs() && cArgCall > pTinproc->m_arypTinParams.C())
	{
		if (errep == ERREP_ReportErrors)
		{
			EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_TooManyArgs,
				"Too many arguments to procedure '%s'. Expected %d but encountered %d",
				PChzProcName(pTinproc, pSymProc),
				pTinproc->m_arypTinParams.C(),
				cArgCall);
		}
		return PROCMATCH_None;
	}

	auto pStnodDefinition = pTinproc->m_pStnodDefinition;
	SGenericMap genmap(pTcwork->m_pAlloc, pStnodDefinition->m_pSym);

	CSTNode * pStnodDefParamList = nullptr;
	CSTProcedure * pStproc = nullptr;
	if (EWC_FVERIFY(pTinproc->m_pStnodDefinition, "expected procedure definition node"))
	{
		pStproc = PStmapRtiCast<CSTProcedure *>(pTinproc->m_pStnodDefinition->m_pStmap);

		if (pStproc && pStproc->m_iStnodParameterList >= 0)
		{
			pStnodDefParamList = pTinproc->m_pStnodDefinition->PStnodChild(pStproc->m_iStnodParameterList);
		}
	}

	size_t cArgTinproc = pTinproc->m_arypTinParams.C();
	size_t cArg = ewcMax(cArgCall, cArgTinproc); 
	CDynAry<CSTNode *> mpIArgPStnod(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cArg);
	CDynAry<GRFARG> mpIArgGrfarg(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cArg);
	mpIArgPStnod.AppendFill(cArg, nullptr);
	mpIArgGrfarg.AppendFill(cArg, FARG_None);
	if (!FFillOrderdAndNamedArgs(
		pTcwork,
		cArgTinproc,
		&mpIArgPStnod,
		&mpIArgGrfarg,
		errep,
		pStnodDefParamList,
		pPmparam->m_ppStnodCall,
		pPmparam->m_cpStnodCall,
		pPmparam->m_pLexloc,
		PChzProcName(pTinproc, pSymProc),
		"procedure"))
	{
		return PROCMATCH_None;
	}

	CDynAry<SMatchTypeInfo> aryMtin(pTcwork->m_pAlloc, BK_TypeCheckProcmatch, cArg);

	//computes arypTinCallDefault and arypTinParam
	for (int iStnodArg = 0; iStnodArg < cArg; ++iStnodArg)
	{
		int iStnodArgAdj = (argord == ARGORD_Reversed) ? ((int)cArg - 1 - iStnodArg) : iStnodArg;
		CSTNode * pStnodArg = mpIArgPStnod[iStnodArgAdj];

		auto pMtin = aryMtin.AppendNew();
		STypeInfo * pTinParam = nullptr;
		GRFPARMQ grfparmq = FPARMQ_None;

		bool fIsArgVariadic = iStnodArg >= (int)pTinproc->m_arypTinParams.C();
		if (!fIsArgVariadic)
		{
			pTinParam = pTinproc->m_arypTinParams[iStnodArg];
			if (!EWC_FVERIFY(pTinParam, "unknown parameter type"))
				return PROCMATCH_None;
			grfparmq = pTinproc->m_mpIptinGrfparmq[iStnodArg];
		}

		if (!FTryComputeMatchTypeInfo(
			pTcwork,
			pSymtab,
			pMtin,
			pStnodArg,
			pTinParam,
			iStnodArg,
			mpIArgGrfarg[iStnodArgAdj],
			grfparmq,
			errep))
		{
			return PROCMATCH_None;
		}

		if (fIsArgVariadic)
		{
			if (!pTinproc->FHasVarArgs())
			{
				if (errep == ERREP_ReportErrors)
				{
					EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_TooManyArgs,
						"procedure '%s' expected %d arguments but encountered %d",
						PChzProcName(pTinproc, pSymProc),
						pTinproc->m_arypTinParams.C(),
						cArg);
				}
				return PROCMATCH_None;
			}

			pMtin->m_pTinCall = pMtin->m_pTinCallDefault;
			pMtin->m_pTinParam = PTinPromoteVarArg(pTcwork, pSymtab, pMtin->m_pTinCall);
		}
	}

	CHash<HV, CSTNode *> mpHvPStnodAnchor(pTcwork->m_pAlloc, BK_TypeCheckProcmatch);
	if (pTinproc->FHasGenericArgs())
	{
		// compute types for generic type 'anchors'
		if (pStproc && pStproc->m_iStnodParameterList >= 0)
		{
			CSTNode * pStnodParamList = pTinproc->m_pStnodDefinition->PStnodChild(pStproc->m_iStnodParameterList);

			auto cpStnodParam = pStnodParamList->CStnodChild();

			for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
			{
				auto pStnodDecl = pStnodParamList->PStnodChild(ipStnodParam);
				auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);

				if (pStnodDecl->m_park != PARK_Decl || pStdecl == nullptr)
					continue;

				if (pStdecl->m_fIsBakedConstant && EWC_FVERIFY(pStnodDecl->m_pSym, "expected symbol for baked value"))
				{
					auto pStnodArg = aryMtin[ipStnodParam].m_pStnodArg;
					if (!FIsCompileTimeConstant(pStnodArg->m_pTin))
					{
						if (errep == ERREP_ReportErrors)
						{
							auto pStnodIdent = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
							auto strParamName = StrFromIdentifier(pStnodIdent);

							EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_BakingNonLiteralValue,
								"passing non-constant to argument %d of procedure '%s'. '%s' must be a compile-time constant",
								ipStnodParam+1,
								PChzProcName(pTinproc, pSymProc),
								strParamName.PCoz());
						}
						return PROCMATCH_None;
					}

					mpIArgPStnod[ipStnodParam] = aryMtin[ipStnodParam].m_pStnodLabel; 
					mpIArgGrfarg[ipStnodParam].AddFlags(FARG_BakedValue);
					genmap.m_mpPSymBakval.Insert(pStnodDecl->m_pSym, SBakeValue(pStnodArg));
				}

				auto pStnodType = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodType);
				if (!EWC_FVERIFY(pStnodType, "Encountered decl without type syntax tree node"))
					continue;

				// given a reference type and a generic type specification compute the anchored genric types
				ERRID errid = ErridComputeDefinedGenerics(
					pTcwork,
					errep,
					aryMtin[ipStnodParam].m_pTinCallDefault,
					pStnodType,
					&genmap,
					&mpHvPStnodAnchor);
				if (errid != ERRID_Nil)
					return PROCMATCH_None;
			}
		}

		/*
		EWC::CHash<STypeInfoGeneric *, STypeInfo *>::CIterator iterPrint(&genmap.m_mpPTingenPTinRemapped);
		STypeInfoGeneric ** ppTingenPrint;
		STypeInfo ** ppTinPrint;
		while ((ppTinPrint = iterPrint.Next(&ppTingenPrint)))
		{
			CString strMapped = StrFromTypeInfo(*ppTinPrint);
			printf("'%s' mapped $%s to %s\n", pTinproc->m_strName.PCoz(), (*ppTingenPrint)->m_strName.PCoz(), strMapped.PCoz());
		}*/

		// given known generic type anchors and unsubstituted types, generate instantiated types
		int cMtin = (int)aryMtin.C();
		for (int iMtin = 0; iMtin < cMtin; ++iMtin)
		{
			auto pMtin = &aryMtin[iMtin];
			pMtin->m_pTinParam = PTinSubstituteGenerics(pTcwork, pSymtab, pPmparam->m_pLexloc, aryMtin[iMtin].m_pTinParam, &genmap, errep);

			bool fIsArgOutsideRange = iMtin >= (int)pTinproc->m_arypTinParams.C();
			if (!fIsArgOutsideRange)
			{
				pMtin->m_pTinCall = PTinPromoteUntypedTightest(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam);
				pMtin->m_pTinCallDefault = PTinPromoteUntypedArgument(pTcwork, pSymtab, pMtin->m_pStnodArg, pMtin->m_pTinParam, errep);
			}
		}
	}

	PROCMATCH procmatch = PROCMATCH_Exact;
	for (int iStnodArg = 0; iStnodArg < cArg; ++iStnodArg)
	{
		// This behavior can be a bit confusing when we're calling an overloaded function with a numeric literal
		//  we consider the overload an exact match when the default promotion matches exactly, we can't use the tightest
		//  promotion because that would exact match all implicit numeric conversions (ie. 2 tightest matches to both int and float)

		auto pMtin = &aryMtin[iStnodArg];

		if (FTypesAreSame(pMtin->m_pTinCallDefault, pMtin->m_pTinParam))
			continue;

		if (FCanImplicitCast(pMtin->m_pTinCall, pMtin->m_pTinParam))
		{
			procmatch = PROCMATCH_ImplicitCast;
		}
		else
		{
			if (errep == ERREP_ReportErrors)
			{
				SInstantiateContext insctx;
				insctx.m_pGenmap = &genmap;
				insctx.m_lexlocCall = *pPmparam->m_pLexloc;
				SInstantiateContextScope insctxscope(pTcwork->m_pErrman, (genmap.FIsEmpty()) ? nullptr : &insctx);

				CString strTinCall = StrFromTypeInfo(pMtin->m_pTinCall);
				CString strTinParam = StrFromTypeInfo(pMtin->m_pTinParam);
				EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_BadImplicitConversion,
					"procedure call '%s' cannot convert argument %d from type %s to %s",
					PChzProcName(pTinproc, pSymProc),
					iStnodArg+1,
					strTinCall.PCoz(),
					strTinParam.PCoz());
			}
			procmatch = PROCMATCH_None;
			break;
		}
	}

	if (procmatch == PROCMATCH_Exact || procmatch == PROCMATCH_ImplicitCast)
	{
		EWC_ASSERT(pPmparam->m_pPmfit == nullptr, "leaking proc match fit struct");
		auto pPmfit = EWC_NEW(pPmparam->m_pAlloc, SProcMatchFit) SProcMatchFit(pPmparam->m_pAlloc);

		pPmparam->m_pPmfit = pPmfit;
		pPmfit->m_mpIArgPStnod.Swap(&mpIArgPStnod);
		pPmfit->m_mpIArgGrfarg.Swap(&mpIArgGrfarg);

		if (!genmap.FIsEmpty())
		{
			pPmfit->m_pGenmap = EWC_NEW(pTcwork->m_pAlloc, SGenericMap) SGenericMap(pPmparam->m_pAlloc, nullptr);
			pPmfit->m_pGenmap->Swap(&genmap);
		}
	}

	return procmatch;
}

// replace exlicit argument list with named/default arg resolved values
void ResolveProcCallArguments(STypeCheckWorkspace * pTcwork, CSTNode * pStnodCall, SProcMatchFit * pPmfit, int ipStnodCallMin, size_t ipStnodCallMax)
{
	CAlloc * pAlloc = pTcwork->m_pAlloc; 
	if (!EWC_FVERIFY(pStnodCall->m_park == PARK_ProcedureCall, "node passed to ResolveProcCallArguments is not a procedure call"))
		return;

	// we can't be agnostic about how the arguments are stored in pStnodCall, we're gonna modify pStnodCall's children

	// clean up any explicit arguments that aren't used (should have emitted errors about this earlier, but we can't leak them)
	size_t cArg = pPmfit->m_mpIArgPStnod.C();
	for (int ipStnod = ipStnodCallMin; ipStnod < ipStnodCallMax; ++ipStnod)
	{
		CSTNode * pStnodExplicit = pStnodCall->PStnodChild(ipStnod);

		bool fFound = false;
		for (int iArg = 0; iArg < cArg; ++iArg)
		{
			if (pPmfit->m_mpIArgPStnod[iArg] == pStnodExplicit)
			{
				fFound = true;
				break;
			}
		}

		if (!fFound)
		{
			pAlloc->EWC_DELETE(pStnodExplicit);
			pStnodCall->m_arypStnodChild[ipStnod] = nullptr;
		}
	}

	size_t cpStnod = cArg + ipStnodCallMin;
	pStnodCall->m_arypStnodChild.AppendFill(cpStnod - pStnodCall->m_arypStnodChild.C(), nullptr);

	for (int iArg = 0; iArg < cArg; ++iArg)
	{
		GRFARG grfarg = pPmfit->m_mpIArgGrfarg[iArg];
		CSTNode * pStnodFit = pPmfit->m_mpIArgPStnod[iArg];

		if (grfarg.FIsSet(FARG_NamedLabelChild))
		{
			CSTNode * pStnodLabel = pPmfit->m_mpIArgPStnod[iArg];
			EWC_ASSERT(pStnodLabel->m_park == PARK_ArgumentLabel, "expected argument label");

			pStnodFit = pStnodLabel->PStnodChildSafe(1);
			pPmfit->m_mpIArgPStnod[iArg] = pStnodFit;
			pStnodLabel->m_arypStnodChild.RemoveFastByI(1);

			pAlloc->EWC_DELETE(pStnodLabel);
		}

		if (grfarg.FIsSet(FARG_DefaultArg))
		{
			pStnodFit = PStnodCopy(pAlloc, pStnodFit);
		}

		if (grfarg.FIsSet(FARG_BakedValue))
		{
			// We can't delete the pStnod here as it is referred to by the baked value map, hand it off 
			//  to the genmap for bookkeeping.
			if (EWC_FVERIFY(pPmfit->m_pGenmap, "baked value with no genmap"))
			{
				pPmfit->m_pGenmap->m_aryPStnodManaged.Append(pStnodFit);
			}
			pStnodFit = nullptr;
		}

		pStnodCall->m_arypStnodChild[iArg + ipStnodCallMin] = pStnodFit;
	}

	int ipStnodDst = ipStnodCallMin;
	for (int ipStnodSrc = ipStnodCallMin; ipStnodSrc < cpStnod; ++ipStnodSrc)
	{
		auto pStnod = pStnodCall->m_arypStnodChild[ipStnodSrc];
		if (pStnod)
		{
			pStnodCall->m_arypStnodChild[ipStnodDst++] = pStnod;
		}
	}

	while (pStnodCall->m_arypStnodChild.C() > ipStnodDst)
	{
		pStnodCall->m_arypStnodChild.PopLast();
	}
}

bool FLiteralsAreSame(CSTNode * pStnodA, CSTNode * pStnodB)
{
	CSTValue * pStvalA = pStnodA->m_pStval;
	CSTValue * pStvalB = pStnodB->m_pStval;
	STypeInfoLiteral * pTinlitA = (STypeInfoLiteral *)pStnodA->m_pTin;
	STypeInfoLiteral * pTinlitB = (STypeInfoLiteral *)pStnodB->m_pTin;

	if (pTinlitA->m_litty.m_litk != pTinlitB->m_litty.m_litk)
		return false;

	switch (pTinlitA->m_litty.m_litk)
	{
	case LITK_Integer:
		{
			if ((pTinlitA->m_litty.m_cBit != pTinlitB->m_litty.m_cBit) | 
				(pTinlitA->m_litty.m_fIsSigned != pTinlitB->m_litty.m_fIsSigned))
				return false;
		
			return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
		}
	case LITK_Float:
		{
			if (pTinlitA->m_litty.m_cBit != pTinlitB->m_litty.m_cBit)
				return false;

			return pStvalA->m_g == pStvalB->m_g;
		}
	case LITK_Char:
			return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
	case LITK_String:
			return pStvalA->m_str == pStvalB->m_str;
	case LITK_Bool:
		return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
	case LITK_Null:
		return true;
	case LITK_Enum:
		{
			if (pTinlitA->m_pTinSource != pTinlitB->m_pTinSource)
				return false;

			return pStvalA->m_nUnsigned == pStvalB->m_nUnsigned;
		}
	case LITK_Array:
		{
			CSTDecl * pStdeclA = PStmapRtiCast<CSTDecl *>(pStnodA->m_pStmap);
			CSTDecl * pStdeclB = PStmapRtiCast<CSTDecl *>(pStnodB->m_pStmap);
			if (!EWC_FVERIFY(pStdeclA && pStdeclA->m_iStnodInit >= 0, "array literal with no values") ||
				!EWC_FVERIFY(pStdeclA && pStdeclA->m_iStnodInit >= 0, "array literal with no values"))
			{
				return false;
			}

			auto pStnodListA = pStnodA->PStnodChild(pStdeclA->m_iStnodInit);
			auto pStnodListB = pStnodB->PStnodChild(pStdeclB->m_iStnodInit);

			if (pStnodListA->CStnodChild() != pStnodListB->CStnodChild())
				return false;

			for (int ipStnod = 0; ipStnod < pStnodListA->CStnodChild(); ++ipStnod)
			{
				if (!FLiteralsAreSame(pStnodListA->PStnodChild(ipStnod), pStnodListB->PStnodChild(ipStnod)))
					return false;
			}

			return true;
		}
	}
	return false;
}

bool FBakvalAreSame(SBakeValue * pBakvalA, SBakeValue * pBakvalB)
{
	if (pBakvalA->m_pTin != nullptr)
	{
		if (!FTypesAreSame(pBakvalA->m_pTin, pBakvalB->m_pTin))
			return false;
	}

	auto pStnodA = pBakvalA->m_pStnod;
	if (pStnodA != nullptr)
	{
		auto pStnodB = pBakvalB->m_pStnod;
		if (pStnodB == nullptr)
			return false;

		//check if the constant value of two syntax trees are equal
		bool fIsLiteralA = pStnodA->m_pTin && pStnodA->m_pTin->m_tink == TINK_Literal;
		bool fIsLiteralB = pStnodB->m_pTin && pStnodB->m_pTin->m_tink == TINK_Literal;
		
		if (fIsLiteralA != fIsLiteralB)
			return false;
		if (fIsLiteralA)
		{
			return FLiteralsAreSame(pStnodA, pStnodB);
		}
	}

	return true;
}

SInstantiateRequest * PInsreqLookup(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodDefinition,
	SGenericMap * pGenmap,
	SInstantiateContext * pInsctx)
{
	auto pmpPSymBakvalRemappedArg = &pGenmap->m_mpPSymBakval;
	for (SInstantiateRequest * pInsreq = pTcwork->m_aryInsreq.A(); pInsreq != pTcwork->m_aryInsreq.PMac(); ++pInsreq)
	{
		if (pInsreq->m_pStnodGeneric != pStnodDefinition)
			continue;

		auto pmpPSymBakvalRemappedIt = &pInsreq->m_pGenmap->m_mpPSymBakval;
		if (pmpPSymBakvalRemappedIt->C() != pmpPSymBakvalRemappedArg->C())
			continue;

		EWC::CHash<SSymbol *, SBakeValue>::CIterator iterIt(pmpPSymBakvalRemappedIt);
		EWC::CHash<SSymbol *, SBakeValue>::CIterator iterArg(pmpPSymBakvalRemappedArg);

		bool fAreTheSame = true;
		SSymbol ** ppSymIt;
		SSymbol ** ppSymArg;
		SBakeValue * pBakvalIt;
		SBakeValue * pBakvalArg;
		while ((pBakvalIt = iterIt.Next(&ppSymIt)))
		{
			pBakvalArg = iterArg.Next(&ppSymArg);
			if (!EWC_FVERIFY(pBakvalArg, "hash value mismatch"))
				break;	

			fAreTheSame &= (*ppSymIt == *ppSymArg && FBakvalAreSame(pBakvalIt, pBakvalArg));
			if (!fAreTheSame)
				break;
		}

		if (fAreTheSame)
		{
			pInsreq->m_arypInsctx.Append(pInsctx);
			return pInsreq;
		}
	}

	return nullptr;
}

static inline SSymbol * PSymRemapGeneric(
	STypeCheckWorkspace * pTcwork,
	SLexerLocation * pLexloc,
	SSymbol * pSymSrc,
	EWC::CHash<SSymbol *, SSymbol *> * pmpPSymGenericPSymRemapped)
{
	if (!pSymSrc)
		return nullptr;

	SSymbol ** ppSymRemapped = pmpPSymGenericPSymRemapped->Lookup(pSymSrc);
	if (ppSymRemapped)
	{
		return *ppSymRemapped;
	}
	return pSymSrc;
}

void InstantiateSymbolsFromTable(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtabSrc, 
	CSymbolTable * pSymtabNew, 
	SGenericMap * pGenmap, 
	EWC::CHash<SSymbol *, CSTNode *> * pmpPSymSrcPStnodValue,
	EWC::CHash<SSymbol *, SSymbol *> * pmpPSymGenericPSymRemapped,
	EWC::CHash<CSTNode *, CSTNode *> * pmpPStnodGenPStnodNew)
{
	// create a duplicate copy of the symbols in this table 
	//  (Even if they aren't generic symbols they need a pStnodDefinition pointer that points into the instantiated 
	//  Syntax tree)

	EWC::CHash<HV, SSymbol *>::CIterator iterSrc(&pSymtabSrc->m_hashHvPSym);
	SSymbol ** ppSymSrc;
	while ((ppSymSrc = iterSrc.Next()))
	{
		SSymbol * pSymSrc = *ppSymSrc;
		EWC_ASSERT(pSymSrc->m_pSymPrev == nullptr, "not handing shadowed symbols"); // see PSymtabCopy

		if (EWC_FVERIFY(pSymSrc->m_pStnodDefinition, "symbol without defining syntax tree node"))
		{
			auto pStnodDefinition = pSymSrc->m_pStnodDefinition;
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pSymSrc->m_pStnodDefinition->m_pStmap);
			if (pStdecl && pStdecl->m_fIsBakedConstant)
			{
				auto pBakval = pGenmap->m_mpPSymBakval.Lookup(pSymSrc);
				if (!pBakval)
				{
					EmitError(pTcwork, pStnodDefinition, 
						"Couldn't find baked value for compile time constant '%s'", 
						pSymSrc->m_strName.PCoz());
				}
				else
				{
					EWC_ASSERT(pBakval->m_pStnod, "expected baked value (not type)");
					pmpPSymSrcPStnodValue->Insert(pSymSrc, pBakval->m_pStnod);
						continue;
				}
			}
		}

		auto pSymNew = pSymtabNew->PSymEnsure(
			pTcwork->m_pErrman,
			pSymSrc->m_strName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = pmpPStnodGenPStnodNew->Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pSymNew->m_pStnodDefinition, 
				"cannot look up definition stNode for symbol %s", 
				pSymNew->m_strName.PCoz());
		}
		else
		{
			pSymNew->m_pStnodDefinition = *ppStnodCopy;
		}

		if (pSymSrc->m_pTin)
		{
			pSymNew->m_pTin = PTinSubstituteGenerics(
								pTcwork,
								pSymtabNew,
								&pSymSrc->m_pStnodDefinition->m_lexloc,
								pSymSrc->m_pTin,
								pGenmap,
								ERREP_ReportErrors);
		}
		pmpPSymGenericPSymRemapped->Insert(pSymSrc, pSymNew);
	}
}

void RemapGenericStnodCopy(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGen,
	CSTNode * pStnodNew,
	SGenericMap * pGenmap,
	EWC::CHash<SSymbol *, SSymbol *> * pmpPSymGenericPSymRemapped,
	EWC::CHash<SSymbol *, CSTNode *> * pmpPSymSrcPStnodConstant,
	EWC::CHash<CSTNode *, CSTNode *> * pmpPStnodGenPStnodNew,
	CSymbolTable * pSymtabSrc,
	CSymbolTable * pSymtabNew)
{
	CDynAry<CSTNode *> arypStnodStackGen(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	CDynAry<CSTNode *> arypStnodStackNew(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	arypStnodStackGen.Append(pStnodGen);
	arypStnodStackNew.Append(pStnodNew);

	CHash<CSymbolTable *, CSymbolTable *>	mpPSymtabSrcPSymtabNew(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	mpPSymtabSrcPSymtabNew.Insert(pSymtabSrc, pSymtabNew);

	while (arypStnodStackGen.C())
	{
		EWC_ASSERT(arypStnodStackNew.C(),"remap stack mismatch!");
		auto pStnodGen	= arypStnodStackGen.TPopLast();
		auto pStnodNew	= arypStnodStackNew.TPopLast();

		if (pStnodNew->m_pSym)
		{
			bool fHasSymbolTin = pStnodNew->m_pTin == pStnodNew->m_pSym->m_pTin;
			auto pSymNew = PSymRemapGeneric(pTcwork, &pStnodNew->m_lexloc, pStnodNew->m_pSym, pmpPSymGenericPSymRemapped);
			if (fHasSymbolTin)
			{
				pStnodNew->m_pTin = pSymNew->m_pTin;
			}

			pStnodNew->m_pSym = pSymNew;
			if (pSymNew)
			{
				if (pStnodNew->m_pTin)
				{
					EWC_ASSERT(!FIsGenericType(pStnodNew->m_pTin), "pTin was not remapped. pSym = %s", pSymNew->m_strName.PCoz());
				}

				if (pSymNew->m_pTin)
				{
					EWC_ASSERT(!FIsGenericType(pSymNew->m_pTin), "symbol pTin was not remapped! PARK_%s sym = %s, %s",
						PChzFromPark(pStnodNew->m_park),
						pSymNew->m_strName.PCoz());
				}
			}
		}

		EWC_ASSERT(pStnodNew->CStnodChild() == pStnodGen->CStnodChild(), "copy child mismatch");
		for (int iStnod = 0; iStnod < pStnodNew->CStnodChild(); ++iStnod)
		{
			auto pStnodChildGen = pStnodGen->PStnodChild(iStnod);
			auto pStnodChildNew = pStnodNew->PStnodChild(iStnod);

			SSymbol * pSymGen = pStnodChildNew->m_pSym;
			if (!pSymGen && pStnodChildGen->m_park == PARK_Identifier)
			{
				CString strIdent = StrFromIdentifier(pStnodChildGen);
				pSymGen = pSymtabSrc->PSymLookup(strIdent, pStnodChildGen->m_lexloc, FSYMLOOK_Local);
			}

			if (pSymGen)
			{
				CSTNode ** ppStnodBaked = pmpPSymSrcPStnodConstant->Lookup(pSymGen);
				if (ppStnodBaked)
				{
					auto pStnodCopy = PStnodCopy(pTcwork->m_pAlloc, *ppStnodBaked);

					pStnodNew->ReplaceChild(pStnodChildNew, pStnodCopy);

					// Don't delete this stnod - it's children may be referenced by another generic parameter.
					//   ie ($BAKE: $T)
					pGenmap->m_aryPStnodManaged.Append(pStnodChildNew);
					continue;
				}
			}

			arypStnodStackGen.Append(pStnodChildGen);
			arypStnodStackNew.Append(pStnodChildNew);
		}

		if (pStnodNew->m_pSymtab)
		{
			// replace any symbol tables that are (or are descended from) the generic symbol table
			CSymbolTable * pSymtabParentNew = nullptr;
			CSymbolTable * pSymtabIt = pStnodNew->m_pSymtab;
			int cCopy = 0;
			while (pSymtabIt)
			{
				auto ppSymtabNew = mpPSymtabSrcPSymtabNew.Lookup(pSymtabIt);
				if (ppSymtabNew)
				{
					pSymtabParentNew = *ppSymtabNew;
					break;
				}

				++cCopy;
				pSymtabIt = pSymtabIt->m_pSymtabParent;
			}

			if (pSymtabParentNew)
			{
				CSymbolTable * pSymtabCopySrc = pStnodNew->m_pSymtab;
				CSymbolTable ** ppSymtabNew = &pStnodNew->m_pSymtab;
				for (int iCopy = 0; iCopy < cCopy; ++iCopy)
				{
					auto pSymtabNew = PSymtabNew(pTcwork->m_pAlloc, pSymtabCopySrc, pSymtabCopySrc->m_strNamespace);	

					*ppSymtabNew = pSymtabNew;
					ppSymtabNew = &pSymtabNew->m_pSymtabParent;

					InstantiateSymbolsFromTable(
						pTcwork,
						pSymtabCopySrc,
						pSymtabNew,
						pGenmap,
						pmpPSymSrcPStnodConstant,
						pmpPSymGenericPSymRemapped,
						pmpPStnodGenPStnodNew);
				}

				*ppSymtabNew = pSymtabParentNew;
			}
		}
	}
}

CString StrComputeMangled(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, CSymbolTable * pSymtab)
{
	auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnod->m_pStmap);
	STypeInfoProcedure * pTinproc = nullptr;
	if (pStnod->m_pSym)
	{
		pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pStnod->m_pSym->m_pTin);
	}

	if (!EWC_FVERIFY(pStproc && pTinproc, "bad procedure definition in StrComputeMangled"))
		return CString();

	if (pStproc->m_fUseUnmangledName)
	{
		return pTinproc->m_strName;
	}
	else
	{
		auto strMangled = pTcwork->m_mang.StrMangleMethodName(pTinproc);

#if VALIDATE_NAME_MANGLING
		char aCh[1024];
		EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
		PrintTypeInfo(&strbuf, pTinproc, PARK_Nil, FDBGSTR_UseSizedNumerics);

		STypeInfoProcedure * pTinprocDemangled = pTcwork->m_mang.PTinprocDemangle(strMangled, pSymtab);
		if (EWC_FVERIFY(pTinprocDemangled, "Name demangling failed - null procedure type"))
		{
			char aChAfter[1024];
			EWC::SStringBuffer strbufAfter(aChAfter, EWC_DIM(aChAfter));
			PrintTypeInfo(&strbufAfter, pTinprocDemangled, PARK_Nil, FDBGSTR_UseSizedNumerics);
			EWC_ASSERT(FAreCozEqual(aCh, aChAfter), "Unmangled type info doesn't match initial info");
		}
#endif
		return strMangled;
	}
}


SInstantiateRequest * PInsreqInstantiateGenericProcedure(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGeneric,
	SGenericMap * pGenmap, 
	SInstantiateContext * pInsctx)
{
	auto pStprocSrc = PStmapRtiCast<CSTProcedure *>(pStnodGeneric->m_pStmap);
	if (!EWC_FVERIFY(pStprocSrc, "expected procedure def"))
		return nullptr;

	if (pStprocSrc->m_fIsForeign)
	{
		EmitError(pTcwork, pStnodGeneric, "generic procedures cannot be marked foreign '%s'", pStnodGeneric->m_pTin->m_strName.PCoz());
		return nullptr;
	}

	// remap the types for the argument list and build symbols for them
	CSTNode * pStnodBody = pStnodGeneric->PStnodChildSafe(pStprocSrc->m_iStnodBody);

	EWC::CHash<CSTNode *, CSTNode *> mpPStnodGenPStnodCopy(pTcwork->m_pAlloc, BK_TypeCheckGenerics);

	auto pStnodProcCopy = PStnodCopy(pTcwork->m_pAlloc, pStnodGeneric, &mpPStnodGenPStnodCopy);
	pStnodProcCopy->m_grfstnod.Clear(FSTNOD_NoCodeGeneration);

	auto pInsreq = pTcwork->m_aryInsreq.AppendNew();
	pInsreq->m_pGenmap = pGenmap;
	pInsreq->m_pStnodGeneric = pStnodGeneric;
	pInsreq->m_arypInsctx.SetAlloc(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	pInsreq->m_arypInsctx.Append(pInsctx);

	SInstantiateContextScope insctxscope(pTcwork->m_pErrman, pInsctx);

	// copy the symbol table, but replace generic types from the map

	// BB - is this really the right way to get the proc's symtab? might have a param list, might not.
	CSymbolTable * pSymtabSrc = nullptr;
	if (EWC_FVERIFY(pStnodBody, "no body? %s\n", pStnodGeneric->m_pTin->m_strName.PCoz()))
	{
		pSymtabSrc = pStnodBody->m_pSymtab;
	}

	if (!EWC_FVERIFY(pSymtabSrc, "generic procedure source has no symbol table"))
		return nullptr;

	CSymbolTable * pSymtabNew = PSymtabNew(pTcwork->m_pAlloc, pSymtabSrc, pSymtabSrc->m_strNamespace);	
	pSymtabNew->m_pSymtabParent = pSymtabSrc->m_pSymtabParent;

	// Remap the top level symbol table
	EWC::CHash<SSymbol *, CSTNode *> mpPSymSrcPStnodValue(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	EWC::CHash<SSymbol *, SSymbol *> mpPSymGenericPSymRemapped(pTcwork->m_pAlloc, BK_TypeCheckGenerics);
	EWC::CHash<HV, SSymbol *>::CIterator iterSrc(&pSymtabSrc->m_hashHvPSym);
	SSymbol ** ppSymSrc;
	while ((ppSymSrc = iterSrc.Next()))
	{
		SSymbol * pSymSrc = *ppSymSrc;
		EWC_ASSERT(pSymSrc->m_pSymPrev == nullptr, "not handing shadowed symbols"); // see PSymtabCopy

		if (EWC_FVERIFY(pSymSrc->m_pStnodDefinition, "symbol without defining syntax tree node"))
		{
			auto pStnodDefinition = pSymSrc->m_pStnodDefinition;
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pSymSrc->m_pStnodDefinition->m_pStmap);
			if (pStdecl && pStdecl->m_fIsBakedConstant)
			{
				auto pBakval = pGenmap->m_mpPSymBakval.Lookup(pSymSrc);
				if (!pBakval)
				{
					EmitError(pTcwork, pStnodGeneric, 
						"Couldn't find baked value for compile time constant '%s'", 
						pSymSrc->m_strName.PCoz());
				}
				else
				{
					EWC_ASSERT(pBakval->m_pStnod, "expected baked value (not type)");
					mpPSymSrcPStnodValue.Insert(pSymSrc, pBakval->m_pStnod);
						continue;
				}
			}
		}

		auto pSymNew = pSymtabNew->PSymEnsure(
			pTcwork->m_pErrman,
			pSymSrc->m_strName,
			pSymSrc->m_pStnodDefinition,
			pSymSrc->m_grfsym);

		auto ppStnodCopy = mpPStnodGenPStnodCopy.Lookup(pSymNew->m_pStnodDefinition);
		if (!ppStnodCopy)
		{
			EmitError(pTcwork, pSymNew->m_pStnodDefinition, 
				"cannot look up definition stNode for symbol %s", 
				pSymNew->m_strName.PCoz());
		}
		else
		{
			pSymNew->m_pStnodDefinition = *ppStnodCopy;
		}

		if (pSymSrc->m_pTin)
		{
			pSymNew->m_pTin = PTinSubstituteGenerics(
								pTcwork,
								pSymtabNew,
								&pSymSrc->m_pStnodDefinition->m_lexloc,
								pSymSrc->m_pTin,
								pGenmap,
								ERREP_ReportErrors);
		}
		mpPSymGenericPSymRemapped.Insert(pSymSrc, pSymNew);
	}

	// build pTinproc for the instantiated procedure

	auto pTinprocSrc = PTinDerivedCast<STypeInfoProcedure *>(pStnodGeneric->m_pTin);

	auto pTinNew = 	PTinSubstituteGenerics(pTcwork, pSymtabNew, &pStnodGeneric->m_lexloc, pTinprocSrc, pGenmap, ERREP_ReportErrors);
	auto pTinprocNew = PTinDerivedCast<STypeInfoProcedure *>(pTinNew);

	pInsreq->m_pSym = pSymtabNew->PSymGenericInstantiate(pStnodGeneric->m_pSym, pTinprocNew);
	EWC_ASSERT(pInsreq->m_pSym, "null symbol");

	pStnodProcCopy->m_pTin = pTinprocNew;
	pStnodProcCopy->m_pSym = pInsreq->m_pSym;
	pInsreq->m_pSym->m_pStnodDefinition = pStnodProcCopy;

	for (int ipStnod = 0; ipStnod < pStnodProcCopy->CStnodChild(); ++ipStnod)
	{
		CSTNode * pStnodChild = pStnodProcCopy->PStnodChild(ipStnod);
		if (pStnodChild->m_pSymtab == pSymtabSrc)
		{
			pStnodChild->m_pSymtab = pSymtabNew;
		}
	}

	// Disconnect the symbol from baked constant parameters so they won't be remapped.
	//  We'll delete the baked constant parameters after remapping (the remapper doesn't handle cChild mismatches)
	auto pStprocCopy = PStmapRtiCast<CSTProcedure *>(pStnodProcCopy->m_pStmap);
	if (pStprocCopy->m_iStnodParameterList >= 0)
	{
		CSTNode * pStnodParamList = pStnodProcCopy->PStnodChild(pStprocCopy->m_iStnodParameterList);
		int ipStnodDst = 0;
		for (int ipStnodSrc = 0; ipStnodSrc < pStnodParamList->CStnodChild(); ++ipStnodSrc)
		{
			auto pStnodChild = pStnodParamList->m_arypStnodChild[ipStnodSrc];
			auto pStdeclChild = PStmapRtiCast<CSTDecl *>(pStnodChild->m_pStmap);
			if (pStdeclChild && pStdeclChild->m_fIsBakedConstant)
			{
				pStnodChild->m_pSym = nullptr;
			}
		}
	}

	pTinprocNew->m_strMangled = StrComputeMangled(pTcwork, pStnodProcCopy, pSymtabSrc->m_pSymtabParent);

	EWC_ASSERT(pTinprocNew->m_strMangled.PCoz(), "failed computing mangled name");
	pTinprocNew->m_grftinproc.Clear(FTINPROC_HasGenericArgs);

	// type check the body with the new values

	if (!EWC_FVERIFY(pStprocSrc && pStprocSrc->m_iStnodBody >= 0, "bad pStnodGeneric"))
		return nullptr;	

	CSTNode * pStnodBodyCopy = pStnodProcCopy->PStnodChildSafe(pStprocSrc->m_iStnodBody);
	pStnodBodyCopy->m_pSymtab = pSymtabNew;

	RemapGenericStnodCopy(
		pTcwork,
		pStnodGeneric,
		pStnodProcCopy,
		pGenmap,
		&mpPSymGenericPSymRemapped,
		&mpPSymSrcPStnodValue,
		&mpPStnodGenPStnodCopy,
		pSymtabSrc,
		pSymtabNew);


	// remove the stnodes for baked constants from the parameter list
	if (pStprocCopy->m_iStnodParameterList >= 0)
	{
		CSTNode * pStnodParamList = pStnodProcCopy->PStnodChild(pStprocCopy->m_iStnodParameterList);
		int ipStnodDst = 0;
		for (int ipStnodSrc = 0; ipStnodSrc < pStnodParamList->CStnodChild(); ++ipStnodSrc)
		{
			auto pStnodChild = pStnodParamList->m_arypStnodChild[ipStnodSrc];
			auto pStdeclChild = PStmapRtiCast<CSTDecl *>(pStnodChild->m_pStmap);
			if (pStdeclChild && pStdeclChild->m_fIsBakedConstant)
			{
				pTcwork->m_pAlloc->EWC_DELETE(pStnodChild);
				continue;
			}
			pStnodParamList->m_arypStnodChild[ipStnodDst] = pStnodChild;
			++ipStnodDst;
		}

		while (pStnodParamList->m_arypStnodChild.C() > ipStnodDst)
		{
			pStnodParamList->m_arypStnodChild.PopLast();
		}
	}

	STypeCheckFrame * pTcfram = pTcwork->m_blistTcfram.AppendNew();
	pTcfram->m_ipTcframQueue = pTcwork->m_arypTcframPending.C();
	pTcwork->m_arypTcframPending.Append(pTcfram);

	SWorkspaceEntry * pEntry = pTcwork->m_pblistEntry->AppendNew();
	pEntry->m_pStnod = pStnodProcCopy;
	pEntry->m_pSymtab = pSymtabNew;
	pTcfram->m_pEntry = pEntry;

	pTcfram->m_aryTcsent.SetAlloc(pTcwork->m_pAlloc, EWC::BK_TypeCheckStack);
	STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnodBodyCopy;
	pTcsent->m_pSymtab = pSymtabNew;
	pTcsent->m_pStnodProcedure = pStnodProcCopy;
	pTcsent->m_pSymContext = pInsreq->m_pSym;
	pTcsent->m_grfsymlook = FSYMLOOK_Default;
	pTcsent->m_parkDeclContext = PARK_Nil;
	pTcsent->m_fAllowForwardDecl = false;
	pTcsent->m_tcctx = TCCTX_Normal;

	return pInsreq;
}

TCRET TcretTryFindMatchingProcedureCall(
	STypeCheckWorkspace * pTcwork, 
	STypeCheckFrame * pTcfram,
	CString strProcName,
	CSymbolTable * pSymtab,
	SProcMatchParam * pPmparam,
	SSymbol ** ppSym,
	ARGORD * pArgord,
	GRFSYMLOOK grfsymlook)
{
	CSymbolTable::CSymbolIterator symiter;
	if (!strProcName.FIsEmpty())
	{
		symiter = CSymbolTable::CSymbolIterator(pSymtab, strProcName, *pPmparam->m_pLexloc, grfsymlook);
	}

	if (symiter.FIsDone())
	{
		if (pPmparam->m_fMustFindMatch)
		{
			EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_CantFindProc,
				"unknown procedure in type check: %s", strProcName.PCoz());
		}
		return TCRET_StoppingError;
	}

	// the first argument is index 1, (the procedure's identifier is element zero)

	int cSymOptions = 0;

	struct SSymMatch
	{
		SSymbol *	m_pSym;
		PROCMATCH 	m_procmatch;
		ARGORD		m_argord;
	};
	CFixAry<SSymMatch, 32> arySymmatch;
	int	mpProcmatchCSymmatch[PROCMATCH_Max];
	SProcMatchFit * mpProcmatchPPmfit[PROCMATCH_Max];
	ZeroAB(mpProcmatchCSymmatch, sizeof(mpProcmatchCSymmatch));
	ZeroAB(mpProcmatchPPmfit, sizeof(mpProcmatchPPmfit));

	SSymbol * pSymProc;
	while ((pSymProc = symiter.PSymNext()))
	{
		CSTNode * pStnodDefinition = pSymProc->m_pStnodDefinition;
		if (!pStnodDefinition)
			continue;

		if (pStnodDefinition->m_strees < STREES_SignatureTypeChecked)
		{
			for (auto ppPmfit = mpProcmatchPPmfit; ppPmfit != EWC_PMAC(mpProcmatchPPmfit); ++ppPmfit)
			{
				if (*ppPmfit)
				{
					pPmparam->m_pAlloc->EWC_DELETE(*ppPmfit);
				}
			}

			// wait for this procedure's signature to be type checked.
			*ppSym = pSymProc;
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

			int argordMax = ARGORD_Normal+1;
			if (pTinprocSym->m_grftinproc.FIsSet(FTINPROC_IsCommutative))
			{
				argordMax = ARGORD_Max;
				++cSymOptions;
			}

			for (int argord = ARGORD_Min; argord < argordMax; ++argord)
			{
				auto procmatch = ProcmatchCheckArguments(pTcwork, pSymtab, pTinprocSym, pPmparam, ERREP_HideErrors, (ARGORD)argord, pSymProc);
				if (procmatch != PROCMATCH_None)
				{
					++mpProcmatchCSymmatch[procmatch];
					auto pSymmatch = arySymmatch.AppendNew();
					pSymmatch->m_pSym = pSymProc;
					pSymmatch->m_procmatch = procmatch;
					pSymmatch->m_argord = (ARGORD)argord;

					if (mpProcmatchPPmfit[procmatch] == nullptr)
					{
						mpProcmatchPPmfit[procmatch] = pPmparam->m_pPmfit;
					}
					else
					{
						// we won't use this struct if there's more than one procmatch, so just delete the extra fit struct
						pPmparam->m_pAlloc->EWC_DELETE(pPmparam->m_pPmfit);
					}
					pPmparam->m_pPmfit = nullptr;
				}
			}
		}
	}

	PROCMATCH procmatchFinal = (mpProcmatchCSymmatch[PROCMATCH_Exact] > 0) ? PROCMATCH_Exact : PROCMATCH_ImplicitCast;

	// set pPmparam to point to the final SProcMatchFit struct and clean up the rest
	for (int procmatch = PROCMATCH_Min; procmatch < PROCMATCH_Max; ++procmatch)
	{
		if (procmatch == (int)procmatchFinal)
		{
			pPmparam->m_pPmfit = mpProcmatchPPmfit[procmatch];	
		}
		else if (mpProcmatchPPmfit[procmatch])	
		{
			pPmparam->m_pAlloc->EWC_DELETE(mpProcmatchPPmfit[procmatch]);
		}
		mpProcmatchPPmfit[procmatch] = nullptr;
	}

	int cSysmatch = mpProcmatchCSymmatch[procmatchFinal];
	if (cSysmatch == 0)
	{
		if (cSymOptions == 0)
		{
			if (pPmparam->m_fMustFindMatch)
			{
				EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_CantFindProc, "'%s' does not evaluate to a procedure.", strProcName.PCoz());
			}
		}
		else if (cSymOptions == 1)
		{
			// print out non overloaded mismatch errors.
			SSymbol * pSymProc = pSymtab->PSymLookup(strProcName, *pPmparam->m_pLexloc, grfsymlook);
			if (pPmparam->m_fMustFindMatch)
			{
				if (pSymProc)
				{
					auto pTinprocSym = PTinRtiCast<STypeInfoProcedure *>(pSymProc->m_pTin);
					if (EWC_FVERIFY(pTinprocSym, "expected type info procedure"))
					{
						(void) ProcmatchCheckArguments(pTcwork, pSymtab, pTinprocSym, pPmparam, ERREP_ReportErrors, ARGORD_Normal, pSymProc);
					}
				}

				if (!pTcwork->m_pErrman->FHasErrors() && !pTcwork->m_pErrman->FHasHiddenErrors())
				{
					EmitError(pTcwork->m_pErrman, pPmparam->m_pLexloc, ERRID_UnknownError,
						"error type matching procedure '%s'", strProcName.PCoz());
				}
			}
		}
		else if (pPmparam->m_fMustFindMatch)
		{
			SError error(pTcwork->m_pErrman);
			PrintErrorLine(&error, "Error:", pPmparam->m_pLexloc, "No overload matches procedure call. Options are:");

			symiter = CSymbolTable::CSymbolIterator(pSymtab, strProcName, *pPmparam->m_pLexloc, grfsymlook);
			while (SSymbol * pSym = symiter.PSymNext())
			{
				auto pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pSym->m_pTin);
				CString strProc = StrFromTypeInfo(pTinproc);

				PrintErrorLine(&error, "   ", &pSym->m_pStnodDefinition->m_lexloc, "%s", strProc.PCoz());
			}
		}
		return TCRET_StoppingError;
	}
	else if (cSysmatch == 1)
	{
		SSymMatch * pSymmatch = nullptr;
		for (size_t iSymmatch = 0; iSymmatch < arySymmatch.C(); ++iSymmatch)
		{
			if (arySymmatch[iSymmatch].m_procmatch == procmatchFinal)
			{
				pSymmatch = &arySymmatch[iSymmatch];
				break;
			}
		}
		if (!EWC_FVERIFY(pSymmatch, "matching procedure lookup failed"))
			return TCRET_StoppingError;

		SSymbol * pSymProc = pSymmatch->m_pSym;
		STypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();

		// mark the symbol used if not a generic (generics will be marked used during instantiation)
		if (!pPmparam->m_pPmfit->m_pGenmap)
		{
			AddSymbolReference(pTcsentTop->m_pSymContext, pSymProc);
		}

		*ppSym = pSymProc;
		*pArgord = pSymmatch->m_argord;
	}
	else // cSymmatch > 1 
	{	

		SError error(pTcwork->m_pErrman, ERRID_AmbiguousOverload);
		PrintErrorLine(&error, "Error:", pPmparam->m_pLexloc, "Overloaded procedure is ambiguous. Options are:");

		SSymMatch * pSymmatchMac = arySymmatch.PMac();
		for (SSymMatch * pSymmatch = arySymmatch.A(); pSymmatch != pSymmatchMac; ++pSymmatch)
		{
			SSymbol * pSym = pSymmatch->m_pSym;
			auto pTinproc = PTinDerivedCast<STypeInfoProcedure  *>(pSym->m_pTin);
			CString strProc = StrFromTypeInfo(pTinproc);

			if (pSymmatch->m_argord == ARGORD_Reversed)
			{
				PrintErrorLine(&error, "   ", &pSym->m_pStnodDefinition->m_lexloc, "%s (reversed)", strProc.PCoz());
			}
			else
			{
				PrintErrorLine(&error, "   ", &pSym->m_pStnodDefinition->m_lexloc, "%s", strProc.PCoz());
			}
		}

		// just return any one of the ambiguous symbols so it doesn't error claiming "doesn't evaluate to a procedure"
		auto pSymmatch = &arySymmatch[0];
		*ppSym = pSymmatch->m_pSym;
		*pArgord = pSymmatch->m_argord;
	}

	return TCRET_Complete;
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

struct SOverloadCheck // tag ovcheck
{
			SOverloadCheck(STypeInfoProcedure * pTinproc, TCRET tcret = TCRET_StoppingError, ARGORD argord = ARGORD_Normal)
			:m_pTinproc(pTinproc)
			,m_tcret(tcret)
			,m_argord(argord)
				{ ; }

	STypeInfoProcedure *	m_pTinproc;
	TCRET					m_tcret;
	ARGORD					m_argord;
};


SOverloadCheck OvcheckTryCheckOverload(
	STypeCheckWorkspace * pTcwork,
	STypeCheckFrame * pTcfram,
	CSTNode * pStnod,
	SProcMatchParam * pPmparam)
{
	CDynAry<STypeCheckStackEntry> * paryTcsent = &pTcfram->m_aryTcsent;
	STypeCheckStackEntry * pTcsentTop = paryTcsent->PLast();

	CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
	const char * pCozOverload = PCozOverloadNameFromTok(pStnod->m_tok);
	if (!pCozOverload)
		return SOverloadCheck(nullptr);

	CString strProcName(pCozOverload);
	SSymbol * pSymProc = nullptr;
	ARGORD argord = ARGORD_Normal;
	TCRET tcret = TcretTryFindMatchingProcedureCall(
					pTcwork, pTcfram, strProcName, pSymtab, pPmparam, &pSymProc, &argord, pTcsentTop->m_grfsymlook);

	auto pTinproc = (pSymProc) ? PTinRtiCast<STypeInfoProcedure*>(pSymProc->m_pTin) : nullptr;

	if (pTinproc && tcret == TCRET_WaitingForSymbolDefinition)
	{
		// wait for this procedure's signature to be type checked.
		SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymProc);
		pUntype->m_arypTcframDependent.Append(pTcfram);

		return SOverloadCheck(pTinproc, tcret, argord);
	}
	else if (tcret != TCRET_Complete)
	{
		return SOverloadCheck(nullptr);
	}

	if (pTinproc)
	{
		CSTNode * pStnodDefinition = pSymProc->m_pStnodDefinition;

		EWC_ASSERT(pStnodDefinition->m_strees >= STREES_SignatureTypeChecked, "expected definition to be type checked");
		EWC_ASSERT(pStnodDefinition->m_pTin == pTinproc, "tin mismatch");
	}

	EWC_ASSERT(pStnod->m_pTin == nullptr, "assignment op has no 'return' value");

	return SOverloadCheck(pTinproc, TCRET_Complete, argord);
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
				auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnod->m_pStmap);
				if (!EWC_FVERIFY(pStproc, "missing procedure parse data"))
					return TCRET_StoppingError;

				STypeInfoProcedure * pTinproc = (STypeInfoProcedure *)pStnod->m_pTin;
				if (!EWC_FVERIFY(pTinproc && pTinproc->m_tink == TINK_Procedure, "missing procedure type info"))
					return TCRET_StoppingError;

				switch(pTcsentTop->m_nState++)
				{
				case 0:
					{	
						EWC_ASSERT(pStnod->m_pSym, "expected procedure symbol");
						pTcsentTop->m_pSymContext = pStnod->m_pSym;

						// push the parameter list
						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParamList = pStnod->PStnodChild(pStproc->m_iStnodParameterList);

							auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodParamList);
							if (pTcsentPushed)
							{
								pTcsentPushed->m_pSymtab = pStnodParamList->m_pSymtab;
								EWC_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");
							}
						}
					}break;
				case 1:
					{	// push the return type
						if (pStproc->m_iStnodReturnType >= 0)
						{
							CSTNode * pStnodReturn = pStnod->PStnodChild(pStproc->m_iStnodReturnType);

							auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodReturn);

							// if we have a parameter list, use it's symbol table so we can return generic types 
							//  defined in the arguments
							CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
							CSTNode * pStnodParamList = pStnod->PStnodChildSafe(pStproc->m_iStnodParameterList);
							if (pStnodParamList && pStnodParamList->m_pSymtab)
							{
								pSymtab = pStnodParamList->m_pSymtab;
							}

							pStnodReturn->m_pSymtab = pSymtab;
							if (pTcsentPushed)
							{
								pTcsentPushed->m_pSymtab = pSymtab;
								pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
							}
						}
					}break;
				case 2:
					{
						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParamList = pStnod->PStnodChild(pStproc->m_iStnodParameterList);

							int cParamsExpected = pStnodParamList->CStnodChild() - pTinproc->FHasVarArgs();
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
															pStnodReturn->m_pSymtab,
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

							if (pStnodIdent->m_tok != TOK_Identifier) // must be op overloaded procedure
							{
								auto errid = ErridCheckOverloadSignature(pStnodIdent->m_tok, pTinproc, pTcwork->m_pErrman, &pStnod->m_lexloc);
								if (errid != ERRID_Nil)
									return TCRET_StoppingError;
							}
						}

						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParams = pStnod->PStnodChild(pStproc->m_iStnodParameterList);
							EWC_ASSERT(pStnodParams->m_park == PARK_ParameterList, "expected parameter list");

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

							if (pTcwork->m_pErrman->FHasErrors())
							{
								return TCRET_StoppingError;
							}
						}

						SSymbol * pSymProc = pStnod->m_pSym;
						if (!EWC_FVERIFY(pSymProc, "failed to find procedure name symbol: %s", pTinproc->m_strName.PCoz()))
							return TCRET_StoppingError;
						if (!EWC_FVERIFY(pSymProc->m_pTin, "expected procedure type info to be created during parse"))
							return TCRET_StoppingError;

						EWC_ASSERT(pStnod->m_pSym, "null symbol");

						if (pTinproc && FIsGenericType(pTinproc))
						{
							SSymbol * pSymProc = pStnod->m_pSym;
							auto pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pSymProc->m_pTin);

							OnTypeResolve(pTcwork, pSymProc);

							for (int ipTin = 0; ipTin < pTinproc->m_arypTinParams.C(); ++ipTin)
							{
								EWC_ASSERT(pTinproc->m_arypTinParams[ipTin], "null parameter type? arg %d", ipTin);
							}

							pStnod->m_grfstnod.AddFlags(FSTNOD_NoCodeGeneration);
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
							return TCRET_Complete;
						}

						// push the body subtree
						if (pStproc->m_iStnodBody >= 0)
						{
							CSTNode * pStnodBody = pStnod->PStnodChild(pStproc->m_iStnodBody);
							auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodBody);

							if (pTcsentPushed)
							{
								pTcsentPushed->m_pStnodProcedure = pStnod;
								pTcsentPushed->m_pSymtab = pStnodBody->m_pSymtab;
							}
						}
					}break;
				case 3:
					{
						pTinproc->m_strMangled = StrComputeMangled(pTcwork, pStnod, pTcsentTop->m_pSymtab);
						PopTcsent(pTcfram, &pTcsentTop, pStnod);

						SSymbol * pSymProc = pStnod->m_pSym;
						pStnod->m_strees = STREES_TypeChecked;
						OnTypeResolve(pTcwork, pSymProc);
					}break;
				}
			} break;
			case PARK_ProcedureReferenceDecl:
			{
				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
					break;
				}

				auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnod->m_pStmap);
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
					cParamsExpected = pStnodParameterList->CStnodChild() - pTinproc->FHasVarArgs();
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
			case PARK_ArgumentLabel:
			{
				// children should be (identifier, argument)

				if (pTcsentTop->m_nState == 0)
				{
					if (pStnod->CStnodChild() != 2)
					{
						EmitError(pTcwork, pStnod, 
							"Invalid argument label in type checker, expected 2 children (label, arg) but encountered %d", 
							pStnod->CStnodChild());
					}
					else
					{
						auto pStnodIdent = pStnod->PStnodChild(0);
						if (pStnodIdent->m_park != PARK_Identifier)
						{
							EmitError(pTcwork, pStnod, 
								"Argument label was parsed incorrectly, expected label identifier but encountered %s", 
								PChzFromPark(pStnodIdent->m_park));
						}

						int ipStnodArg = 1; // actual argument
						(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(ipStnodArg));
						++pTcsentTop->m_nState;
						break;
					}
				}

				auto pStnodArg = pStnod->PStnodChild(1);
				pStnod->m_pTin = pStnodArg->m_pTin;
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
					auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(ipStnodChild));
					if (pTcsentPushed)
					{
						pTcsentPushed->m_fAllowForwardDecl = true; //ipStnodChild == 0;
					}

					break;
				}

				CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
				bool fIsDirectCall = false;
				STypeInfoProcedure * pTinproc = nullptr;
				CSTNode * pStnodCallee = pStnod->PStnodChild(0);
				if (pStnodCallee->m_park == PARK_Identifier)
				{
					SProcMatchParam pmparam(pTcwork->m_pAlloc, &pStnodCallee->m_lexloc);
					pmparam.m_cpStnodCall = pStnod->m_arypStnodChild.C() - 1;
					pmparam.m_ppStnodCall = (pmparam.m_cpStnodCall) ? &pStnod->m_arypStnodChild[1] : nullptr;
					pmparam.m_fMustFindMatch = true;

					CSTNode * pStnodIdent = pStnodCallee;
					CString strProcName = StrFromIdentifier(pStnodIdent);
					SSymbol * pSymProc = nullptr;
					ARGORD argord;
					TCRET tcret = TcretTryFindMatchingProcedureCall(
									pTcwork, pTcfram, strProcName, pSymtab, &pmparam, &pSymProc, &argord, pTcsentTop->m_grfsymlook);

					switch (tcret)
					{
					case TCRET_WaitingForSymbolDefinition:
						{
							// wait for this procedure's signature to be type checked.
							// BB - We'll redo the work to find our matching procedure once this definition is checked. 
							SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymProc);
							pUntype->m_arypTcframDependent.Append(pTcfram);
							return tcret;
						}
					case TCRET_StoppingError:
						return tcret;
					case TCRET_Complete:
						{
							pStnod->m_pSym = pSymProc;

							ResolveProcCallArguments(pTcwork, pStnod, pmparam.m_pPmfit, 1, pStnod->m_arypStnodChild.C());

							if (pmparam.m_pPmfit->m_pGenmap)
							{
								auto pInsctxNew = PInsctxNew(pTcwork);
								pInsctxNew->m_pGenmap = pmparam.m_pPmfit->m_pGenmap;
								pInsctxNew->m_lexlocCall = pStnod->m_lexloc;

								auto pInsreq = PInsreqLookup(pTcwork, pSymProc->m_pStnodDefinition, pmparam.m_pPmfit->m_pGenmap, pInsctxNew);
								if (pInsreq)
								{
									pmparam.m_pPmfit->m_pGenmap->Cleanup(pTcwork->m_pAlloc);
								}
								else
								{
									pInsreq = PInsreqInstantiateGenericProcedure(
												pTcwork,
												pSymProc->m_pStnodDefinition,
												pmparam.m_pPmfit->m_pGenmap,
												pInsctxNew);

									// 'unhook' the genmap because it will be referenced by the insreq 
									pmparam.m_pPmfit->m_pGenmap = nullptr;
								}

								if (!pInsreq)
									return TCRET_StoppingError;

								pSymProc = pInsreq->m_pSym;
								AddSymbolReference(pTcsentTop->m_pSymContext, pSymProc);

								EWC_ASSERT(!FIsGenericType(pSymProc->m_pTin), "remap failed");
							}

							// Note: The callee's symbol and pTin are set by type checking the identifier, this may pick the wrong
							//  overload, clean it up here.
							pStnodCallee->m_pSym = pSymProc;

							pTinproc = (pSymProc) ? PTinRtiCast<STypeInfoProcedure*>(pSymProc->m_pTin) : nullptr;
							pStnodCallee->m_pTin = pTinproc;

							// make sure the instanced symbol is pushed to the PARK_ProcedureCall node
							pStnod->m_pSym = pSymProc;
							pStnod->m_pTin = pTinproc;

							fIsDirectCall = FIsDirectCall(pStnod);
							if (fIsDirectCall && pSymProc)
							{
								CSTNode * pStnodDefinition = pSymProc->m_pStnodDefinition;

								EWC_ASSERT(
									pStnodDefinition->m_strees >= STREES_SignatureTypeChecked,
									"expected definition to be type checked");

								//EWC_ASSERT(pStnodDefinition->m_pTin == pTinproc, "tin mismatch");

								auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnodDefinition->m_pStmap);
								if (EWC_FVERIFY(pStproc, "bad procedure return info"))
								{
									if (pStproc->m_iStnodReturnType >= 0)
									{
										auto pTinReturnCheck = pStnodDefinition->PStnodChild(pStproc->m_iStnodReturnType)->m_pTin;
										EWC_ASSERT(pTinproc->m_arypTinReturns[0] == pTinReturnCheck, "return type mismatch");
									}
								}
							}
						} break;
					}
				}
				else // callee is not an identifier - proc indirect call 
				{
					auto pTinprocSym = PTinRtiCast<STypeInfoProcedure  *>(pStnodCallee->m_pTin);
					if (EWC_FVERIFY(pTinprocSym, "expected type info procedure"))
					{
						SProcMatchParam pmparam(pTcwork->m_pAlloc, &pStnodCallee->m_lexloc);
						pmparam.m_ppStnodCall = &pStnod->m_arypStnodChild[1];
						pmparam.m_cpStnodCall = pStnod->m_arypStnodChild.C() - 1;
						pmparam.m_fMustFindMatch = true;

						auto procmatch = ProcmatchCheckArguments(pTcwork, pSymtab, pTinprocSym, &pmparam, ERREP_ReportErrors, ARGORD_Normal, nullptr);
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

						pTinCall = PTinPromoteUntypedRvalueTightest(pTcwork, pSymtab, pStnodArg, pTinParam);
					}
					else
					{
						if (!EWC_FVERIFY(pTinproc->FHasVarArgs(), "bad procedure match!"))
							return TCRET_StoppingError;

						pTinCall = PTinPromoteUntypedArgument(pTcwork, pSymtab, pStnodArg, nullptr, ERREP_ReportErrors);
						pTinParam = PTinPromoteVarArg(pTcwork, pSymtab, pTinCall);
					}

					// if we have a literal, just expect the finalized type to be correct, otherwise
					//  set the implicit cast type on the argument node
					if (pStnodArg->m_pTin->m_tink == TINK_Literal)
					{
						FinalizeLiteralType(pTcwork, pSymtab, pTinParam, pStnodArg);
					}
					else
					{
						if (!FTypesAreSame(pStnodArg->m_pTin, pTinParam))
						{
							auto pAlloc = pTcwork->m_pAlloc;
							CSTNode * pStnodCast = EWC_NEW(pAlloc, CSTNode) CSTNode(pAlloc, pStnodArg->m_lexloc);
							pStnodCast->m_park = PARK_Cast;
							pStnodCast->m_pTin = pTinParam;

							auto pStdecl = pStnodCast->PStmapEnsure<CSTDecl>(pAlloc);
							pStdecl->m_iStnodInit = pStnodCast->IAppendChild(pStnodArg);

							pStnod->ReplaceChild(pStnodArg, pStnodCast);

							if (!FVerifyIvalk(pTcwork, pStnodArg, IVALK_RValue))
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
				auto pStenum = PStmapRtiCast<CSTEnum *>(pStnod->m_pStmap);
				if (!EWC_FVERIFY(pTinenum && pStenum, "missing struct type info"))
					return TCRET_StoppingError;

				// skip identifier
				if (pTcsentTop->m_nState == pStenum->m_iStnodIdentifier)
					++pTcsentTop->m_nState;

				if (pTcsentTop->m_nState == 1)
				{
					++pTcsentTop->m_nState;

					EWC_ASSERT(pStnod->m_pSym, "expected enum symbol");
					pTcsentTop->m_pSymContext = pStnod->m_pSym;

					// type spec
					if (pStenum->m_iStnodType >= 0)
					{
						auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pStenum->m_iStnodType));
						if (pTcsentPushed)
						{
							pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
							pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
						}
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
						// initial incremented value is 1 (flagEnum) or 0 (basic enums) 
						int nEnumInitial = (pTinenum->m_enumk == ENUMK_FlagEnum) ? 0 : -1;
						pTinenum->m_bintLatest = BintFromInt(nEnumInitial);

						auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pStenum->m_iStnodConstantList));
						if (pTcsentPushed)
						{
							pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
						}
						++pTcsentTop->m_nState;
						break;
					}
				}

				auto pTinstruct = &pTinenum->m_tinstructProduced;
				EWC_ASSERT(pTinstruct->m_aryTypemembField.C() == 0, "no fields expected in enum struct");

				CSTNode * mpEnumimpPStnod[ENUMIMP_Max];
				ZeroAB(mpEnumimpPStnod, sizeof(mpEnumimpPStnod));

				CSTNode * pStnodConstantList = pStnod->PStnodChild(pStenum->m_iStnodConstantList);

				int cStnodChildImplicit = 0;
				int cStnodChild = pStnodConstantList->CStnodChild();
				CDynAry<ENUMIMP> mpIStnodChildEnumimp(pTcwork->m_pAlloc, BK_TypeCheck, cStnodChild);
				mpIStnodChildEnumimp.AppendFill(cStnodChild, ENUMIMP_Nil);

				if (pStenum->m_iStnodConstantList >= 0)
				{
					// loop over our constants and find the min/max values

					bool fIsFirst = true;
					SBigInt bintMin;
					SBigInt bintLast;
					SBigInt bintAll;
					for (int ipStnod = 0; ipStnod < pStnodConstantList->CStnodChild(); ++ipStnod)
					{
						int enumimp;
						for (enumimp = ENUMIMP_Min; enumimp < ENUMIMP_Max; ++enumimp)
						{
							if (pStenum->m_mpEnumimpIstnod[enumimp] == ipStnod)
								break;
						}

						auto pStnodConstant = pStnodConstantList->PStnodChild(ipStnod);
						if (enumimp < ENUMIMP_Max)
						{
							mpEnumimpPStnod[enumimp] = pStnodConstant;
							mpIStnodChildEnumimp[ipStnod] = (ENUMIMP)enumimp;
							++cStnodChildImplicit;
							continue;
						}
						
						if (!EWC_FVERIFY(pStnodConstant->m_pStval, "PARK_EnumConstant type check failed to set values"))
							continue;

						auto bint = BintFromStval(pStnodConstant->m_pStval);
						bintAll = BintBitwiseOr(bintAll, bint);
						if (fIsFirst)
						{
							fIsFirst = false;
							bintMin = bint;
							bintLast = bint;
						}
						else
						{
							if (bint < bintMin)
								bintMin = bint;
							if (bint > bintLast)
								bintLast = bint;
						}
					}

					SBigInt bintMax = BintAdd(bintLast, BintFromInt(1));
					pTinenum->m_bintMin = bintMin;
					pTinenum->m_bintMax = bintMax;

					if (!pTinenum->m_pTinLoose)
					{
						pTinenum->m_pTinLoose = PTinFromRange(pTcwork, pTcsentTop->m_pSymtab, pStnod, bintMin, bintLast);
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

						if (pStenum->m_enumk == ENUMK_Basic)
						{
							SetEnumConstantValue(pTcwork, mpEnumimpPStnod[ENUMIMP_NilConstant], bintNil);
							SetEnumConstantValue(pTcwork, mpEnumimpPStnod[ENUMIMP_MinConstant], bintMin);
							SetEnumConstantValue(pTcwork, mpEnumimpPStnod[ENUMIMP_LastConstant], bintLast);
							SetEnumConstantValue(pTcwork, mpEnumimpPStnod[ENUMIMP_MaxConstant], bintMax);
						}
						else
						{
							SetEnumConstantValue(pTcwork, mpEnumimpPStnod[ENUMIMP_None], BintFromInt(0));
							SetEnumConstantValue(pTcwork, mpEnumimpPStnod[ENUMIMP_All], bintAll);
						}
					}
				}

				if (!pTinenum->m_pTinLoose)
				{
					EmitError(pTcwork, pStnod, "Unable to determine loose type for enum %s", pTinenum->m_strName.PCoz());
					return TCRET_StoppingError;
				}
				else
				{
					CSTNode * pStnodNames = mpEnumimpPStnod[ENUMIMP_Names];
					CSTNode * pStnodValues = mpEnumimpPStnod[ENUMIMP_Values];

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
					STypeInfo * pTinString = pSymtab->PTinptrAllocate(pTinU8);

					SpoofLiteralArray(pTcwork, pSymtab, pStnodNames, cStnodChild - cStnodChildImplicit, pTinString);
					auto pStdeclNames = PStmapDerivedCast<CSTDecl *>(pStnodNames->m_pStmap);
					auto pStnodNameList = pStnodNames->PStnodChildSafe(pStdeclNames->m_iStnodInit);

					SpoofLiteralArray(pTcwork, pSymtab, pStnodValues, cStnodChild - cStnodChildImplicit, pTinenum->m_pTinLoose);
					auto pStdeclValues = PStmapDerivedCast<CSTDecl *>(pStnodValues->m_pStmap);
					auto pStnodValueList = pStnodValues->PStnodChildSafe(pStdeclValues->m_iStnodInit);

					// assign pTin and finalize literals
					for (int iStnodMember = 0; iStnodMember < cStnodChild; ++iStnodMember)
					{
						auto pStnodMember = pStnodConstantList->PStnodChild(iStnodMember);

						ENUMIMP enumimp = mpIStnodChildEnumimp[iStnodMember];
						if ((enumimp == ENUMIMP_Names) | (enumimp == ENUMIMP_Values))
							continue;

						// just make sure the init type fits the specified one
						auto pStnodInit = pStnodMember->PStnodChildSafe(1);
						if (pStnodInit)
						{
							STypeInfo * pTinInit = pStnodMember->m_pTin;

							pTinInit = PTinPromoteUntypedRvalueTightest(pTcwork, pSymtab, pStnodInit, pTinenum->m_pTinLoose);
							if (!FCanImplicitCast(pTinInit, pTinenum->m_pTinLoose))
							{
								EmitError(pTcwork, pStnodInit, ERRID_InitTypeMismatch, "Cannot initialize constant of type %s with %s",
									StrFromTypeInfo(pTinenum->m_pTinLoose).PCoz(),
									StrFromTypeInfo(pTinInit).PCoz());
							}
						}

						auto pTinecon = pTinenum->m_aryTinecon.AppendNew();
						pTinecon->m_bintValue = BintFromStval(pStnodMember->m_pStval);

						auto pStdecl = PStmapDerivedCast<CSTDecl *>(pStnodMember->m_pStmap);
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

				OnTypeResolve(pTcwork, pSymEnum);
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			}break;
			case PARK_EnumConstant:
			{
				if (pTcsentTop->m_nState < 1)
					pTcsentTop->m_nState = 1;	// skip the identifier

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					++pTcsentTop->m_nState;
					break;
				}

				auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
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
					if (pTinenum->m_enumk == ENUMK_FlagEnum)
					{
						pTinenum->m_bintLatest = BintNextPowerOfTwo(pTinenum->m_bintLatest);
					}
					else
					{
						pTinenum->m_bintLatest = BintAdd(pTinenum->m_bintLatest, BintFromUint(1));
					}
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
					AddSymbolReference(pTcsentTop->m_pSymContext, pSymIdent);

					pStnod->m_pSym = pSymIdent;
					if (pSymIdent->m_pTin == nullptr)
					{
						pSymIdent->m_pTin = pStnod->m_pTin;
					}
				}
				OnTypeResolve(pTcwork, pSymIdent);

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_StructDefinition:
			{
				auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pStnod->m_pTin);
				if (!EWC_FVERIFY(pTinstruct, "missing struct type info"))
					return TCRET_StoppingError;

				auto pStstruct = PStmapRtiCast<CSTStruct *>(pStnod->m_pStmap);
				if (!EWC_FVERIFY(pStstruct, "expected ststruct"))
					return TCRET_StoppingError;

				if (pTcsentTop->m_nState == pStstruct->m_iStnodIdentifier)
					++pTcsentTop->m_nState;

				// Don't try to typecheck the decl list if we haven't replaced our generic params yet.
				if (pTinstruct->FHasGenericParams() && pTcsentTop->m_nState == pStstruct->m_iStnodDeclList)
				{
					++pTcsentTop->m_nState;
				}

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					pStnod->m_strees = STREES_SignatureTypeChecked;

					auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					if (pTcsentPushed)
					{
						pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
					}

					++pTcsentTop->m_nState;
					break;
				}

				if (pStstruct->m_iStnodParameterList >= 0)
				{
					pStnod->m_grfstnod.AddFlags(FSTNOD_NoCodeGeneration);
					auto pStnodParamList = pStnod->PStnodChild(pStstruct->m_iStnodParameterList);

					EWC_ASSERT(pTinstruct->m_arypTinGenericParam.CMax() == pStnodParamList->CStnodChild(), "missized array");
					for (int ipStnodChild = 0; ipStnodChild < pStnodParamList->CStnodChild(); ++ ipStnodChild)
					{
						auto pStnodChild = pStnodParamList->PStnodChild(ipStnodChild);
						pTinstruct->m_arypTinGenericParam[ipStnodChild] = pStnodChild->m_pTin;
					}
				}

				auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
				for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb)
				{
					pTypememb->m_pTin = pTypememb->m_pStnod->m_pTin;
				}

				SSymbol * pSymStruct = pStnod->m_pSym;
				if (!EWC_FVERIFY(pSymStruct, "struct symbol should be created during parse"))
					return TCRET_StoppingError;
				if (!EWC_FVERIFY(pSymStruct->m_pTin, "expected structure type info to be created during parse"))
					return TCRET_StoppingError;


				pStnod->m_strees = STREES_TypeChecked;

				PopTcsent(pTcfram, &pTcsentTop, pStnod);
				OnTypeResolve(pTcwork, pSymStruct);
			}break;
			case PARK_Identifier:
			{
				// Note: we're only expecting to get here for identifiers within statements.
				//  Identifiers for function names, declaration names*, should do their own type checking.
				//      * declaration type identifiers will be type checked here

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
						if (pStnodDefinition->m_park == PARK_GenericDecl)
						{
							// We're type checking the uninstantiated generic, don't wait for a symbol definition
							//  this symbol will be replaced later

							pStnod->m_pTin = pStnodDefinition->m_pTin;
							pStnod->m_pSym = pSym;
							AddSymbolReference(pTcsentTop->m_pSymContext, pSym);

						}
						else if (pStnodDefinition->m_park == PARK_Decl ||
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

								AddSymbolReference(pTcsentTop->m_pSymContext, pSym);

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
								pUntype->m_arypTcframDependent.Append(pTcfram);
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

			case PARK_GenericStructInst:
			{
				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					++pTcsentTop->m_nState;
					break;
				}

				auto pSymInst = pStnod->m_pSym;
				if (!pSymInst)
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					pSymInst = PSymInstantiateGenericStruct(pTcwork, pSymtab, pStnod, pTcsentTop->m_grfsymlook);
				}

				if (!pSymInst)
					return TCRET_StoppingError;

				STypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();
				AddSymbolReference(pTcsentTop->m_pSymContext, pSymInst);
				pStnod->m_pSym = pSymInst;

				EWC_ASSERT(!FIsGenericType(pSymInst->m_pTin), "remap failed");

				if (pSymInst->m_pStnodDefinition->m_strees < STREES_TypeChecked)
				{
					SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymInst);
					pUntype->m_arypTcframDependent.Append(pTcfram);
					return TCRET_WaitingForSymbolDefinition;
				}

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;

			case PARK_GenericDecl:
			{
				// Don't push the identifier

				OnTypeResolve(pTcwork, pStnod->m_pSym);

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;

			case PARK_ArrayDecl:
			case PARK_ReferenceDecl:
			case PARK_QualifierDecl:
			case PARK_ParameterList:
			case PARK_List:
			case PARK_ExpressionList:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (pStnod->m_park == PARK_List)
					{
						int cStnodChild = pStnod->CStnodChild();
						for (int iStnod = 0; iStnod < cStnodChild; ++iStnod)
						{
							auto pStnodChild = pStnod->PStnodChild(iStnod);
							switch (pStnodChild->m_park)
							{
							case PARK_Identifier:
							case PARK_Cast:
							case PARK_Literal:
								{
									EmitError(pTcwork, pStnod, 
										"%s is left hand side, has no effect",
										PChzFromPark(pStnodChild->m_park));
								} break;
							default:
								break;
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
				if (pTcsentPushed)
				{
					if (pStnod->m_park == PARK_List || pStnod->m_park == PARK_ExpressionList)
					{
						if (pStnod->m_pSymtab)
						{
							pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
						}
					}
					if (pStnod->m_park == PARK_ParameterList)
					{
						pTcsentPushed->m_parkDeclContext = pStnod->m_park;
						pTcsentPushed->m_fAllowForwardDecl = true;
					}
					if (pStnod->m_park == PARK_ReferenceDecl)
					{
						pTcsentPushed->m_fAllowForwardDecl = true;
					}
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
				auto pStdecl = PStmapDerivedCast<CSTDecl *>(pStnod->m_pStmap);
				if (pTcsentTop->m_nState < 1)
				{
					pTcsentTop->m_nState = 1;	// skip the identifier

					auto pStnodIdent = pStnod->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
					if (EWC_FVERIFY(pStnodIdent, "constant missing identifier"))
					{
						EWC_ASSERT(pStnodIdent->m_pSym, "expected symbol for declaration");
						pTcsentTop->m_pSymContext = pStnodIdent->m_pSym;
					}
				}

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					if (pTcsentPushed && pTcsentTop->m_nState == pStdecl->m_iStnodType)
					{
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
					EmitError(pTcwork, pStnod, ERRID_InitTypeMismatch, "Cannot initialize constant '%s' to non-instance value.",strIdent.PCoz());
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
						FinalizeLiteralType(pTcwork, pSymtab, pTinType, pStnodInit);
					}
					else
					{
						EmitError(pTcwork, pStnod, ERRID_InitTypeMismatch, "Cannot initialize constant of type %s with %s",
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
					OnTypeResolve(pTcwork, pSymIdent);
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
					auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnodType);
					if (pTcsentPushed)
					{
						EWC_ASSERT(pStnod->m_pSym, "expected typedef symbol");
						pTcsentPushed->m_pSymContext = pStnod->m_pSym;

						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					}
					
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
					OnTypeResolve(pTcwork, pSym);
				}

				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			} break;
			case PARK_Cast:
			{
				auto pStdecl = PStmapDerivedCast<CSTDecl *>(pStnod->m_pStmap);
				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					if (pTcsentPushed && pTcsentTop->m_nState == pStdecl->m_iStnodType)
					{
						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					}
					
					++pTcsentTop->m_nState;
					break;
				}

				if (!EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "bad explicit cast"))
					return TCRET_StoppingError;

				CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
				auto pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);

				if (!FVerifyIvalk(pTcwork, pStnodInit, IVALK_RValue))
					return TCRET_StoppingError;

				STypeInfo * pTinInit = pStnodInit->m_pTin;

				// AutoCast will be resolved during promotion of untyped RHSs
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
					if (FCanExplicitCast(pTinInit, pStnod->m_pTin, pSymtab))
					{
						FinalizeLiteralType(pTcwork, pSymtab, pStnod->m_pTin, pStnodInit);
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
				auto pStdecl = PStmapDerivedCast<CSTDecl *>(pStnod->m_pStmap);

				auto pStnodIdent = pStnod->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
				if (pStnodIdent && pTcsentTop->m_pSymContext == nullptr)
				{
					EWC_ASSERT(pStnodIdent->m_pSym, "expected symbol for declaration");
					pTcsentTop->m_pSymContext = pStnodIdent->m_pSym;
				}

				if (pTcsentTop->m_nState < pStnod->CStnodChild())
				{
					if (pTcsentTop->m_nState != pStdecl->m_iStnodIdentifier)
					{
						auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
						if (pTcsentPushed)
						{
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
					}

					++pTcsentTop->m_nState;
					break;
				}

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
						if (!FVerifyIvalk(pTcwork, pStnodInit, IVALK_RValue))
						{
							pStnodInit->m_park = PARK_Uninitializer;
						}

						STypeInfo * pTinInitDefault = pStnod->m_pTin;
						if (!pTinInitDefault && pStnodInit->m_park != PARK_Uninitializer)
						{
							CString strIdent = StrFromIdentifier(pStnodIdent);
							CSymbolTable::CSymbolIterator symiter(pSymtab, strIdent, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);
							auto pSymPrior = symiter.PSymNext();

							while (!symiter.FIsDone() && LexlocFromSym(pStnodIdent->m_pSym) <= LexlocFromSym(pSymPrior))
							{
								pSymPrior = symiter.PSymNext();
							}

							if (pSymPrior && LexlocFromSym(pSymPrior) < LexlocFromSym(pStnodIdent->m_pSym))
							{
								if (!pSymPrior->m_pStnodDefinition)
								{
									EmitError(pTcwork, pStnod, ERRID_InitTypeMismatch,
										"'%s' is already declared. \n"
										"Type inference is not allowed on overloaded variables. Did you type ':=' but meant '='?",
										strIdent.PCoz());
								}
								else
								{
									s32 iLine;
									s32 iCol;
									auto pLexlocPrior = &pSymPrior->m_pStnodDefinition->m_lexloc;
									CalculateLinePosition(pTcwork->m_pErrman->m_pWork, pLexlocPrior, &iLine, &iCol);

									EmitError(pTcwork, pStnod, ERRID_InitTypeMismatch,
										"'%s' is already declared at %s (%d, %d). \n"
										"Type inference is not allowed on overloaded variables. Did you type ':=' but meant '='?",
										strIdent.PCoz(),
										pLexlocPrior->m_strFilename.PCoz(),
										iLine,
										iCol);
								}
							}

							if (!pStnodInit->m_pTin)
							{
								EmitError(pTcwork, pStnod, "trying to initialize %s with a 'void' type", strIdent.PCoz());
								return TCRET_StoppingError;
							}

							// BB - This won't allow an override of operator:= to return a different type
							// I'm planning on coming back to it when I handle return types values as regular LValues
							pTinInitDefault = PTinPromoteUntypedDefault(pTcwork, pTcsentTop->m_pSymtab, pStnodInit);
							pTinInitDefault = PTinAfterRValueAssignment(pTcwork, &pStnodInit->m_lexloc, pTinInitDefault, pTcsentTop->m_pSymtab);

							EWC_ASSERT(pTinInitDefault, "failed to compute default init type");
						}

						SOverloadCheck ovcheck(nullptr);

						auto pStnodOpLhs = pStnodIdent;
						if (!pStnodOpLhs)
						{
							pStnodOpLhs = pStnod->PStnodChildSafe(pStdecl->m_iStnodChildMin);
						}

						if (pTinInitDefault)
						{
							EWC_ASSERT(pStnodOpLhs && pStnodOpLhs->m_pTin == nullptr, "expected null identifier type");
							pStnodOpLhs->m_pTin = pTinInitDefault;

							// Check for overloads on the ':=' operator
							//   What happens with constant init?
							//   What about globals?

							pStnod->m_tok = TOK_ColonEqual;

							CSTNode * apStnod[2];
							apStnod[0] = pStnodOpLhs;
							apStnod[1] = pStnodInit;
							
							SProcMatchParam pmparam(pTcwork->m_pAlloc, &pStnod->m_lexloc);
							pmparam.m_cpStnodCall = 2;
							pmparam.m_ppStnodCall = apStnod;

							ovcheck = OvcheckTryCheckOverload(pTcwork, pTcfram, pStnod, &pmparam);
							pStnodOpLhs->m_pTin = nullptr;
						}

						if (ovcheck.m_pTinproc)
						{
							if (ovcheck.m_tcret != TCRET_Complete)
								return ovcheck.m_tcret;

							auto pTinproc = ovcheck.m_pTinproc;
							EWC_ASSERT(pTinproc->m_arypTinParams.C() == 2 && pTinproc->m_arypTinReturns.C() == 1, "bad operator overload signature");

							AllocateOptype(pStnod);
							pStnod->m_pOptype->m_pTinLhs = pTinproc->m_arypTinParams[0];
							pStnod->m_pOptype->m_pTinRhs = pTinproc->m_arypTinParams[1];
							pStnod->m_pOptype->m_pTinResult = pTinproc->m_arypTinReturns[0];
							pStnod->m_pOptype->m_pTinprocOverload = pTinproc;
							EWC_ASSERT(ovcheck.m_argord == ARGORD_Normal, "Decl arguments cannot be commutative");

							pStnod->m_pTin = pTinInitDefault;
							FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pOptype->m_pTinLhs, pStnodInit);
						}
						else
						{
							// don't finalize the literal for init here if it's actually a default argument.
							//  enum values finalize to the loose int types and it causes the default argument type checking to fail

							bool fAllowFinalizing = pTcsentTop->m_parkDeclContext != PARK_ParameterList;

							if (pStnod->m_pTin && pStnodInit->m_park != PARK_Uninitializer)
							{
								// just make sure the init type fits the specified one
								STypeInfo * pTinInit = pStnodInit->m_pTin;
								pTinInit = PTinPromoteUntypedRvalueTightest(pTcwork, pSymtab, pStnodInit, pStnod->m_pTin);

								// Strip the top level const, as we're declaring a new instance
								//auto pTinInstance = PTinQualifyAfterAssignment(pStnod->m_pTin, pSymtab);
								auto pTinInstance = PTinAfterRValueAssignment(pTcwork, &pStnod->m_lexloc, pStnod->m_pTin, pSymtab);
								if (FCanImplicitCast(pTinInit, pTinInstance))
								{
									if (fAllowFinalizing)
									{
										FinalizeLiteralType(pTcwork, pSymtab, pStnod->m_pTin, pStnodInit);
									}
									pTinInit = pStnod->m_pTin;
								}
								else
								{
									const char * pChzFormat = (pTcsentTop->m_parkDeclContext == PARK_ParameterList) ? 
									"parameter '%s' is type '%s', but default argument is '%s'" :
									"Cannot initialize variable '%s' of type '%s' with '%s'";

									CString strIdent = StrFromIdentifier(pStnodIdent);
									EmitError(pTcwork, pStnod, ERRID_InitTypeMismatch, 
										pChzFormat,
										strIdent.PCoz(),
										StrFromTypeInfo(pStnod->m_pTin).PCoz(),
										StrFromTypeInfo(pTinInit).PCoz());
								}
							}
							else if (pTinInitDefault)
							{
								pStnod->m_pTin = pTinInitDefault;
								if (fAllowFinalizing)
								{
									FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pTin, pStnodInit);
								}
							}
						}
					}

					// would this be better if there was a PARK_CompoundDecl?
					bool fIsCompoundDecl = pStdecl->m_iStnodChildMin != -1;
					if (!fIsCompoundDecl)
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
								OnTypeResolve(pTcwork, pSymIdent);
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
					(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					++pTcsentTop->m_nState;
					break;
				}

				auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
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
					pTinlit->m_litty.m_litk = LITK_Array;
					pTinlit->m_pTinSource = pTinType;
					pTinlit->m_pStnodDefinition = pStnod;

					if (pStdecl->m_iStnodInit >= 0)
					{
						auto pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);

						EWC_ASSERT(pStnodInit->m_park == PARK_ExpressionList, "invalid ArrayLiteral");
						pTinlit->m_c = pStnodInit->CStnodChild();
					}

					pSymtab->AddManagedTin(pTinlit);
					pTinlit = pSymtab->PTinMakeUnique(pTinlit);
					pStnod->m_pTin = pTinlit;
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

					if (EWC_FVERIFY(pStnod->m_pStval, "null value in literal"))
					{
						pTinlit->m_litty.m_litk = pStnod->m_pStval->m_litkLex;
					}

					pSymtab->AddManagedTin(pTinlit);
					pTinlit = pSymtab->PTinMakeUnique(pTinlit);
					pStnod->m_pTin = pTinlit;
				}
				
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram, &pTcsentTop, pStnod);
			}break;

			case PARK_AssignmentOp:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (!EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to assignment op"))
						return TCRET_StoppingError;

					CSTNode * pStnodLhs = pStnod->PStnodChild(0);
					CSTNode * pStnodRhs = pStnod->PStnodChild(1);
					STypeInfo * pTinLhs = pStnodLhs->m_pTin;

					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;

					if (!FVerifyIvalk(pTcwork, pStnodLhs, IVALK_LValue) || !FVerifyIvalk(pTcwork, pStnodRhs, IVALK_RValue))
						return TCRET_StoppingError;

					SProcMatchParam pmparam(pTcwork->m_pAlloc, &pStnod->m_lexloc);
					pmparam.m_cpStnodCall = pStnod->m_arypStnodChild.C();
					pmparam.m_ppStnodCall = (pmparam.m_cpStnodCall) ? &pStnod->m_arypStnodChild[0] : nullptr;

					SOverloadCheck ovcheck = OvcheckTryCheckOverload(pTcwork, pTcfram, pStnod, &pmparam);
					if (ovcheck.m_pTinproc)
					{
						auto pTinproc = ovcheck.m_pTinproc;
						EWC_ASSERT(pTinproc->m_arypTinParams.C() == 2 && pTinproc->m_arypTinReturns.C() == 1, "bad operator overload signature");

						if (ovcheck.m_tcret != TCRET_Complete)
							return ovcheck.m_tcret;

						AllocateOptype(pStnod);
						pStnod->m_pOptype->m_pTinLhs = pTinproc->m_arypTinParams[0];
						pStnod->m_pOptype->m_pTinRhs = pTinproc->m_arypTinParams[1];
						pStnod->m_pOptype->m_pTinResult = pTinproc->m_arypTinReturns[0];
						pStnod->m_pOptype->m_pTinprocOverload = pTinproc;

						EWC_ASSERT(ovcheck.m_argord == ARGORD_Normal, "Assignment arguments cannot be commutative");
						FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pOptype->m_pTinLhs, pStnodRhs);

						pStnod->m_strees = STREES_TypeChecked;
						PopTcsent(pTcfram, &pTcsentTop, pStnod);
						break;
					}

					{
						AllocateOptype(pStnod);
						if (EWC_FVERIFY(pTinLhs, "unexpected unknown type in assignment op LHS"))
						{
							bool fIsValidLhs = FIsValidLhs(pStnodLhs);
							if (!fIsValidLhs)
							{
								CString strLhs = StrFromTypeInfo(pTinLhs);
								EmitError(pTcwork, pStnod, "'%s' does not provide an assignment operator", strLhs.PCoz());
								return TCRET_StoppingError;
							}
						}

						EWC_ASSERT(pTinLhs, "unexpected null type in assignment op RHS");

						STypeInfo * pTinRhsPromoted = PTinPromoteUntypedRvalueTightest(pTcwork, pTcsentTop->m_pSymtab, pStnodRhs, pTinLhs);

						SOpTypes optype = OptypeFromPark(pTcwork, pSymtab, pStnod->m_tok, pStnod->m_park, pTinLhs, pTinRhsPromoted);

						if (!optype.FIsValid() || !FDoesOperatorExist(pStnod->m_tok, &optype))
						{
							(void)OptypeFromPark(pTcwork, pSymtab, pStnod->m_tok, pStnod->m_park, pTinLhs, pTinRhsPromoted);
							CString strLhs = StrFromTypeInfo(pTinLhs);
							CString strRhs = StrFromTypeInfo(pTinRhsPromoted);
							EmitError( pTcwork, pStnod,
								"operator '%s' is not defined for '%s' and '%s'",
								PCozFromTok(pStnod->m_tok),
								strLhs.PCoz(),
								strRhs.PCoz());
							return TCRET_StoppingError;
						}

						if (!FCanImplicitCast(pTinRhsPromoted, optype.m_pTinRhs))
						{
							CString strLhs = StrFromTypeInfo(optype.m_pTinRhs);
							CString strRhs = StrFromTypeInfo(pTinRhsPromoted);
							EmitError( pTcwork, pStnod,
								"implicit cast from %s to %s is not allowed",
								strRhs.PCoz(),
								strLhs.PCoz());
							return TCRET_StoppingError;
						}

						*pStnod->m_pOptype = optype;
					}

					FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pOptype->m_pTinLhs, pStnodRhs);

					EWC_ASSERT(pStnod->m_pTin == nullptr, "assignment op has no 'return' value");

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			} break;
			case PARK_MemberLookup:
			{
				if (pTcsentTop->m_nState == 0)
				{
					(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(0));
					++pTcsentTop->m_nState;
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
						FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinLhs, pStnodLhs);
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
									pUntype->m_arypTcframDependent.Append(pTcfram);
									return TCRET_WaitingForSymbolDefinition;
								}
							}

							auto pSymMember = pStnodStruct->m_pSymtab->PSymLookup(strMemberName, pStnodRhs->m_lexloc, FSYMLOOK_Local | FSYMLOOK_IgnoreOrder);
							if (!pSymMember)
							{
								EmitError(pTcwork, pStnod, ERRID_BadMemberLookup, 
									"%s is not a member of %s", strMemberName.PCoz(), pTinstruct->m_strName.PCoz());
								return TCRET_StoppingError;
							}

							AddSymbolReference(pTcsentTop->m_pSymContext, pSymMember);
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
											pTinlitInt = pSymtab->PTinMakeUnique(pTinlitInt);
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
									default:
										EWC_ASSERT(false, "unknown array kind");
										break;
									}
								} break;
							case ARYMEMB_Data:
								{
									// TODO: think about what to do here, aN must be mutable 
									// because it points at mutable data - inarg maybe?

									//auto pTinptr = pSymtab->PTinptrAllocate(pTinary->m_pTin);
									//pTinMember = pSymtab->PTinqualEnsure(pTinptr, FQUALK_Const);
									pTinMember = pSymtab->PTinptrAllocate(pTinary->m_pTin);
								} break;
							default: 
								EmitError(pTcwork, pStnod, "unknown array member '%s'", strMemberName.PCoz());
								return TCRET_StoppingError;
							}

						} break;
						default:
							EWC_ASSERT(false, "unknown type info kind");
							break;
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
					(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState));
					++pTcsentTop->m_nState;
					break;
				}

				auto pSymtab = pTcsentTop->m_pSymtab;

				auto pStnodLhs = pStnod->PStnodChild(0);

				auto pTinLhs = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodLhs);
				FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinLhs, pStnodLhs);

				CSTNode * pStnodIndex = pStnod->PStnodChild(1);
				if (!EWC_FVERIFY(pStnodLhs && pStnodLhs->m_pTin, "Array element LHS has no type") ||
					!EWC_FVERIFY(pStnodIndex && pStnodIndex->m_pTin, "Array index has no type"))
					return TCRET_StoppingError;

				switch (pTinLhs->m_tink)
				{
				case TINK_Array:
					{
						auto pTinary = PTinDerivedCast<STypeInfoArray *>(pTinLhs);
						pStnod->m_pTin = pTinary->m_pTin;
					} break;
				case TINK_Pointer:
					{
						auto pTinptr = PTinDerivedCast<STypeInfoPointer *>(pTinLhs);
						pStnod->m_pTin = pTinptr->m_pTinPointedTo;
					} break;
				default: 
					CString strLhs = StrFromTypeInfo(pTinLhs);
					EmitError(pTcwork, pStnod, "%s cannot be indexed as an array", strLhs.PCoz());
					return TCRET_StoppingError;
				}

				auto pTinIndex = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodIndex);
				if (pTinIndex->m_tink == TINK_Enum)
				{
					auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinIndex);
					pTinIndex = pTinenum->m_pTinLoose;
				}

				if (pTinIndex->m_tink != TINK_Integer)
				{
					CString strTinIndex = StrFromTypeInfo(pTinIndex);
					EmitError(pTcwork, pStnod, ERRID_BadArrayIndex, "Cannot convert %s to integer for array index", strTinIndex.PCoz());
				}

				FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinIndex, pStnodIndex);
				
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
					case RWORD_Sizeof:
					case RWORD_Alignof:
					case RWORD_Typeinfo:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							auto pStnodChild = pStnod->PStnodChild(0);
							if (!EWC_FVERIFY(pStnodChild, "%s missing child", PCozFromRword(rword)))
								break;

							if (!pStnodChild->m_pTin)
							{
								EmitError(pTcwork, pStnod, "%s unable to determine target type", PCozFromRword(rword)); 
							}

							auto pSymtab = pTcsentTop->m_pSymtab;

							auto pTinDefault = PTinPromoteUntypedDefault(pTcwork, pSymtab, pStnodChild);
							if (pStnodChild->m_pTin->m_tink == TINK_Literal)
							{
								pStnodChild->m_pTin = pTinDefault;
							}

							pStnodChild->m_pTin = pSymtab->PTinMakeUnique(pStnodChild->m_pTin);

							if (rword == RWORD_Typeinfo)
							{
								// Make sure the type table is already resoved, this ensures that it will codegen before
								//  this typeInfo statement is generated
								auto pSymTinTable = pSymtab->PSymLookup(STypeInfo::s_pChzGlobalTinTable, SLexerLocation());
								if (!pSymTinTable )
								{
									return TcretWaitForTypeSymbol(pTcwork, pTcfram, pSymTinTable , pStnod);
								}

								// lookup STypeInfo, or return waiting for type
								auto pSymTypeinfo = pSymtab->PSymLookup("STypeInfo", SLexerLocation());

								if (!pSymTypeinfo)
								{
									return TcretWaitForTypeSymbol(pTcwork, pTcfram, pSymTypeinfo, pStnod);
								}
								pStnod->m_pTin = pSymtab->PTinptrAllocate(pSymTypeinfo->m_pTin);
							}
							else
							{
								pStnod->m_pTin = pSymtab->PTinBuiltin("uSize");
							}

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_For:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								if (pTcsentPushed)
								{
									pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
									EWC_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");
								}

								break;
							}

							auto pStfor = PStmapRtiCast<CSTFor *>(pStnod->m_pStmap);
							if (pStfor == nullptr)
							{
								EmitError(pTcwork, pStnod, "for loop was improperly parsed.");
								return TCRET_StoppingError;
							}

							auto pStnodPredicate = pStnod->PStnodChildSafe(pStfor->m_iStnodPredicate);
							if (pStnodPredicate)
							{
								auto pSymtab = pTcsentTop->m_pSymtab;
								STypeInfo * pTinBool = pSymtab->PTinBuiltin("bool");
								STypeInfo * pTinPredPromoted = PTinPromoteUntypedRvalueTightest(pTcwork, pTcsentTop->m_pSymtab, pStnodPredicate, pTinBool);

								if (!FCanImplicitCast(pTinPredPromoted, pTinBool))
								{
									CString strTin = StrFromTypeInfo(pTinPredPromoted);
									EmitError(pTcwork, pStnod, "Cannot convert predicate from %s to bool", strTin.PCoz());
								}
								FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinBool, pStnodPredicate);
							}

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);

						} break;
					case RWORD_ForEach:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								auto pTcsentPushed = PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								if (pTcsentPushed)
								{
									pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
									EWC_ASSERT(pTcsentPushed->m_pSymtab, "null symbol table");
								}

								break;
							}

							auto pStfor = PStmapRtiCast<CSTFor *>(pStnod->m_pStmap);
							if (pStfor == nullptr)
							{
								EmitError(pTcwork, pStnod, "for_each loop was improperly parsed.");
								return TCRET_StoppingError;
							}

							STypeInfo * pTinIterator = nullptr;
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

										STypeInfo * pTinRhsPromoted = PTinPromoteUntypedRvalueTightest(
																		pTcwork,
																		pTcsentTop->m_pSymtab,
																		pStnodInit,
																		pTinIterator);
										if (FCanImplicitCast(pTinRhsPromoted, pTinIterator))
										{
											FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinIterator, pStnodInit);
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
							if (EWC_FVERIFY(pStnodPredicate, "for_each loop missing predicate child"))
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
					case RWORD_Fallthrough:
						{
							EWC_ASSERT(pTcfram->m_aryTcsent.PLast()->m_pStnod == pStnod, "expected this node");
							CSTNode * pStnodChild = pStnod;
							for (int iTcsent = (int)pTcfram->m_aryTcsent.C()-2; iTcsent >= 0; --iTcsent)
							{
								auto pStnodIt = pTcfram->m_aryTcsent[iTcsent].m_pStnod;
								bool fIsValidPosition = false;
								switch (pStnodIt->m_park)
								{
									case PARK_List:
									{
										if (pStnodIt->PStnodChildSafe(pStnodIt->CStnodChild()-1) == pStnodChild)
										{
											fIsValidPosition = true;
										}
									} break;
									case PARK_ReservedWord:
									{
										if (!EWC_FVERIFY(pStnodIt->m_pStval, "bad reserved word."))
											break;
										RWORD rword = pStnodIt->m_pStval->m_rword;
										if ((rword == RWORD_Case) | (rword == RWORD_Else))
										{
											pStnodIt->m_grfstnod.AddFlags(FSTNOD_Fallthrough);
											fIsValidPosition = true;
											iTcsent = -1;
										}
									}break;
									default:
										EWC_ASSERT(false, "unexpected parse kind");
										break;
								}

								if (!fIsValidPosition)
								{
									EmitError(pTcwork, pStnod, "fallthrough keyword should always be the last statement in a switch case");
								}
								pStnodChild = pStnodIt;
							}

						} // fallthrough
					case RWORD_Continue:
					case RWORD_Break:
						{
							EWC_ASSERT(pStnod->CStnodChild() == 0, "did not expect child nodes");
							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_Switch:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							auto pStnodExp = pStnod->PStnodChildSafe(0);
							if (!pStnod)
							{
								EmitError(pTcwork, pStnod, "switch missing expression");
								return TCRET_StoppingError;
							}

							STypeInfo * pTinExpPromoted = PTinPromoteUntypedDefault(pTcwork, pTcsentTop->m_pSymtab, pStnodExp);

							// BB - should make pTinExpPromoted const?
							pTinExpPromoted = PTinStripQualifiers(pTinExpPromoted);
							if (pTinExpPromoted->m_tink != TINK_Integer && 
								pTinExpPromoted->m_tink != TINK_Bool && 
								pTinExpPromoted->m_tink != TINK_Enum)
							{
								EmitError(pTcwork, pStnod, "switch expression must evaluate to an integer type");
								return TCRET_StoppingError;
							}

							EWC::CDynAry<CSTNode *> aryPStnod(pTcwork->m_pAlloc, BK_TypeCheck, pStnod->CStnodChild());
							EWC::CDynAry<SBigInt> aryBint(pTcwork->m_pAlloc, BK_TypeCheck, pStnod->CStnodChild());

							int iStnodDefault = -1;
							for (int iStnodIt = 1; iStnodIt < pStnod->CStnodChild(); ++iStnodIt)
							{
								auto pStnodIt = pStnod->PStnodChild(iStnodIt);
			
								if (!EWC_FVERIFY(pStnodIt->m_park == PARK_ReservedWord, "expected switch case"))
									continue;

								RWORD rword = pStnodIt->m_pStval->m_rword;
								if (rword == RWORD_Else)
								{
									if (iStnodDefault >= 0)
									{
										auto pStnodPrev = pStnod->PStnodChild(iStnodDefault);
										auto pLexloc = &pStnodPrev->m_lexloc;
										s32 iLine;
										s32 iCol;
										CalculateLinePosition(pTcwork->m_pErrman->m_pWork, pLexloc, &iLine, &iCol);

										EmitError(
											pTcwork, pStnodIt,
											"switch statement else case already defined. %s (%d,%d). ",
											pLexloc->m_strFilename.PCoz(),
											iLine,
											iCol);
										continue;
									}
									iStnodDefault = iStnodIt;
								}
								else
								{
									int cStnodLit = pStnodIt->CStnodChild()-1;
									for (int iStnodLit = 0; iStnodLit < cStnodLit; ++iStnodLit)
									{
										auto pStnodLit = pStnodIt->PStnodChildSafe(iStnodLit);
										if (!EWC_FVERIFY(pStnodLit, "missing case literal"))
											continue;

										STypeInfo * pTinCase = PTinPromoteUntypedTightest(
																	pTcwork,
																	pTcsentTop->m_pSymtab,
																	pStnodLit,
																	pTinExpPromoted);

										if (!FCanImplicitCast(pTinCase, pTinExpPromoted))
										{
											CString strTinCase = StrFromTypeInfo(pTinCase);
											CString strTinExp = StrFromTypeInfo(pTinExpPromoted);
											EmitError(pTcwork, pStnod, "No conversion between %s and %s", strTinCase.PCoz(), strTinExp.PCoz());
										}

										FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinExpPromoted, pStnodLit);

										auto pTinLit = pStnodLit->m_pTin;
										if (!pTinLit || pTinLit->m_tink != TINK_Literal)
										{
											EmitError(pTcwork, pStnod, "case literal does not evaluate to a constant");
											continue;
										}

										if (!EWC_FVERIFY(pStnodLit->m_pStval, "case literal missing value"))
											continue;

										SBigInt bint = BintFromStval(pStnodLit->m_pStval);
										aryPStnod.Append(pStnodLit);
										aryBint.Append(bint);
									}
								}
							}

							for (int iBintLhs = 0; iBintLhs < aryBint.C(); ++iBintLhs)
							{
								SBigInt bintLhs = aryBint[iBintLhs];
								for (int iBintRhs = iBintLhs+1; iBintRhs < aryBint.C(); ++iBintRhs)
								{
									if (FAreEqual(bintLhs, aryBint[iBintRhs]))
									{
										auto pStnodLhs = aryPStnod[iBintLhs];
										auto pStnodRhs = aryPStnod[iBintLhs];

										auto pLexlocLhs = &pStnodLhs->m_lexloc;
										s32 iLineLhs;
										s32 iColLhs;
										CalculateLinePosition(pTcwork->m_pErrman->m_pWork, pLexlocLhs, &iLineLhs, &iColLhs);

										EmitError(
											pTcwork, pStnodRhs,
											"case value %s%lld already used. %s(%d, %d):",
											(bintLhs.m_fIsNegative) ? "-" : "",
											bintLhs.m_nAbs,
											pLexlocLhs->m_strFilename.PCoz(),
											iLineLhs,
											iColLhs);
									}
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
								(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
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

							auto pSymtab = pTcsentTop->m_pSymtab;
							STypeInfo * pTinBool = pSymtab->PTinBuiltin("bool");
							STypeInfo * pTinPredPromoted = PTinPromoteUntypedRvalueTightest(
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
							FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinBool, pStnodPred);

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram, &pTcsentTop, pStnod);
						} break;
					case RWORD_Else:
					case RWORD_Case:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
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
									if (!FVerifyIvalk(pTcwork, pStnodRhs, IVALK_RValue))
										return TCRET_StoppingError;

									STypeInfo * pTinRhs = pStnodRhs->m_pTin;
									STypeInfo * pTinRhsPromoted = PTinPromoteUntypedRvalueTightest(
																	pTcwork,
																	pTcsentTop->m_pSymtab,
																	pStnodRhs,
																	pTinReturn);

									// Strip the top level const, as we're declaring a new instance
									auto pTinInstance = PTinAfterRValueAssignment(pTcwork, &pStnodRhs->m_lexloc, pTinReturn, pTcsentTop->m_pSymtab);
									if (FCanImplicitCast(pTinRhsPromoted, pTinInstance))
									{
										pStnod->m_pTin = pTinReturn;
										FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pTinReturn, pStnodRhs);
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
							(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
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
					if (!EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to binary ops"))
						return TCRET_StoppingError;

					CSTNode * pStnodLhs = pStnod->PStnodChild(0);
					CSTNode * pStnodRhs = pStnod->PStnodChild(1);
					STypeInfo * pTinLhs = pStnodLhs->m_pTin;
					STypeInfo * pTinRhs = pStnodRhs->m_pTin;

					if (!FVerifyIvalk(pTcwork, pStnodLhs, IVALK_RValue) || !FVerifyIvalk(pTcwork, pStnodRhs, IVALK_RValue))
						return TCRET_StoppingError;

					if (!EWC_FVERIFY((pTinLhs != nullptr) & (pTinRhs != nullptr), "unknown type in binary operation"))
						return TCRET_StoppingError;

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
									PCozFromTok(pStnod->m_tok),
									PChzFromLitk(((STypeInfoLiteral *)pTinLhs)->m_litty.m_litk),
									PChzFromLitk(((STypeInfoLiteral *)pTinRhs)->m_litty.m_litk));
								return TCRET_StoppingError;
							}
						}
					}
					else
					{
						SProcMatchParam pmparam(pTcwork->m_pAlloc, &pStnod->m_lexloc);
						pmparam.m_cpStnodCall = pStnod->m_arypStnodChild.C();
						pmparam.m_ppStnodCall = (pmparam.m_cpStnodCall) ? &pStnod->m_arypStnodChild[0] : nullptr;

						SOverloadCheck ovcheck = OvcheckTryCheckOverload(pTcwork, pTcfram, pStnod, &pmparam);
						if (ovcheck.m_pTinproc)
						{
							if (ovcheck.m_tcret != TCRET_Complete)
								return ovcheck.m_tcret;

							AllocateOptype(pStnod);

							STypeInfoProcedure * pTinproc = ovcheck.m_pTinproc;
							if (EWC_FVERIFY(pTinproc->m_arypTinParams.C() == 2 && pTinproc->m_arypTinReturns.C() == 1,
										"bad operator overload signature"))
							{
								pStnod->m_pOptype->m_pTinLhs = pTinproc->m_arypTinParams[0];
								pStnod->m_pOptype->m_pTinRhs = pTinproc->m_arypTinParams[1];
								pStnod->m_pOptype->m_pTinResult = pTinproc->m_arypTinReturns[0];
							}

							pStnod->m_pOptype->m_pTinprocOverload = ovcheck.m_pTinproc;
							pStnod->m_pTin = pStnod->m_pOptype->m_pTinResult;

							AdjustArgumentOrder(ovcheck.m_argord, pStnod, pStnod->m_pOptype);
						}

						if (!pStnod->m_pOptype)
						{
							AllocateOptype(pStnod);

							PARK park = pStnod->m_park;
							STypeInfo * pTinUpcastLhs = PTinPromoteUntypedRvalueTightest(pTcwork, pSymtab, pStnodLhs, pTinRhs);
							STypeInfo * pTinUpcastRhs = PTinPromoteUntypedRvalueTightest(pTcwork, pSymtab, pStnodRhs, pTinLhs);

							SOpTypes optype = OptypeFromPark(pTcwork, pSymtab, pStnod->m_tok, park, pTinUpcastLhs, pTinUpcastRhs);

							if (!optype.FIsValid() || !FDoesOperatorExist(pStnod->m_tok, &optype))
							{
								CString strLhs = StrFromTypeInfo(pTinLhs);
								CString strRhs = StrFromTypeInfo(pTinRhs);
								EmitError(
									pTcwork,
									pStnod,
									"%s operator not defined for %s and %s",
									PCozFromTok(pStnod->m_tok),
									strLhs.PCoz(),
									strRhs.PCoz());
								return TCRET_StoppingError;
							}
							*pStnod->m_pOptype = optype;
						}

						pStnod->m_pTin = pStnod->m_pOptype->m_pTinResult;

						FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pOptype->m_pTinLhs, pStnodLhs);
						FinalizeLiteralType(pTcwork, pTcsentTop->m_pSymtab, pStnod->m_pOptype->m_pTinRhs, pStnodRhs);
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram, &pTcsentTop, pStnod);
					break;
				}
				(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
		case PARK_PostfixUnaryOp:
		case PARK_UnaryOp:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (!EWC_FVERIFY(pStnod->CStnodChild() == 1, "expected one operand to unary operations"))
						return TCRET_StoppingError;

					CSTNode * pStnodOperand = pStnod->PStnodChild(0);
					STypeInfo * pTinOperand = pStnodOperand->m_pTin;

					SProcMatchParam pmparam(pTcwork->m_pAlloc, &pStnod->m_lexloc);
					pmparam.m_cpStnodCall = pStnod->m_arypStnodChild.C();
					pmparam.m_ppStnodCall = (pmparam.m_cpStnodCall) ? &pStnod->m_arypStnodChild[0] : nullptr;

					SOverloadCheck ovcheck = OvcheckTryCheckOverload(pTcwork, pTcfram, pStnod, &pmparam);
					if (ovcheck.m_pTinproc)
					{
						EWC_ASSERT(ovcheck.m_argord == ARGORD_Normal, "unary arguments cannot be commutative");
						if (ovcheck.m_tcret != TCRET_Complete)
							return ovcheck.m_tcret;

						AllocateOptype(pStnod);

						STypeInfoProcedure * pTinproc = ovcheck.m_pTinproc;
						if (EWC_FVERIFY(pTinproc->m_arypTinParams.C() == 1 && pTinproc->m_arypTinReturns.C() == 1,
									"bad operator overload signature"))
						{
							*pStnod->m_pOptype = SOpTypes(pTinOperand, pTinOperand, pTinproc->m_arypTinReturns[0]);
						}

						pStnod->m_pOptype->m_pTinprocOverload = ovcheck.m_pTinproc;
						pStnod->m_pTin = pStnod->m_pOptype->m_pTinResult;

						pStnod->m_strees = STREES_TypeChecked;
						PopTcsent(pTcfram, &pTcsentTop, pStnod);
						break;
					}

					{
						AllocateOptype(pStnod);
						*pStnod->m_pOptype = SOpTypes(pTinOperand, pTinOperand, pTinOperand);

						if (!FVerifyIvalk(pTcwork, pStnodOperand, IVALK_RValue))
							return TCRET_StoppingError;

						if (EWC_FVERIFY(pTinOperand != nullptr, "unknown type in unary operation"))
						{
							if (pTinOperand->m_tink == TINK_Literal)
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
											PCozFromTok(pStnod->m_tok),
											PChzFromLitk(((STypeInfoLiteral *)pTinOperand)->m_litty.m_litk));
										return TCRET_StoppingError;
									}
								}
							}
							else
							{
								TOK tok = pStnod->m_tok;
								switch ((u32)tok)
								{
								case TOK_Dereference:
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
								case TOK_Reference:
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
												auto pStdecl = PStmapDerivedCast<CSTDecl *>(pStnodMember->m_pStmap);
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
										pStnod->m_pTin = pSymtab->PTinptrAllocate(pTinOperand);
									}break;

								case TOK('!'):
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

								case TOK('~'):
								case TOK_PlusPlus:
								case TOK_MinusMinus:
								case TOK('+'):
								case TOK('-'):
									{
										TINK tinkOperand = pTinOperand->m_tink;
										bool fIsInteger = tinkOperand == TINK_Integer;
										bool fIsFloat = tinkOperand == TINK_Float;

										bool fIsBasicEnum = false;
										bool fIsFlagEnum = false;
										auto pTinenum = PTinRtiCast<STypeInfoEnum *>(pTinOperand);
										if (pTinenum)
										{
											fIsBasicEnum = pTinenum->m_enumk == ENUMK_Basic;
											fIsFlagEnum = pTinenum->m_enumk == ENUMK_FlagEnum;
										}

										if (tinkOperand == TINK_Literal && pStnodOperand->m_pStval)
										{
											LITK litk = ((STypeInfoLiteral *)pTinOperand)->m_litty.m_litk;
											fIsInteger |= litk == LITK_Integer;
											fIsFloat |= litk == LITK_Float;

											EWC_ASSERT(litk != LITK_Enum || pTinenum, "literal without enum type?");
										}

										bool fIsValidPtrOp = ((tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) &
											(tinkOperand == TINK_Pointer);
										bool fIsValidFloatOp = ((tok == TOK('+')) | (tok == TOK('-')) | (tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) & 
																fIsFloat;
										bool fIsValidBasicEnumOp = ((tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) & fIsBasicEnum;
										bool fIsValidFlagEnumOp = (tok == TOK('~')) & fIsFlagEnum;
										bool fIsSupported = fIsInteger | fIsValidPtrOp | fIsValidFloatOp | fIsValidBasicEnumOp | fIsValidFlagEnumOp;

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
											if (tok == TOK('-') && tinkOperand == TINK_Integer)
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
				(void) PTcsentPush(pTcfram, &pTcsentTop, pStnod->PStnodChild(pTcsentTop->m_nState++));
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
	size_t ipTcfram = pTcfram->m_ipTcframQueue;
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

void OnTypeResolve(STypeCheckWorkspace * pTcwork, const SSymbol * pSym)
{
	EWC_ASSERT(pSym->m_pTin, "expected type for completed symbol");

	// BB - could replace this with a function that 'pops' a key/value pair from the hash and save a second lookup.
	SUnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
		return;

	int cTcframDependent = (s32)pUntype->m_arypTcframDependent.C();
	EWC_ASSERT(cTcframDependent > 0, "unknown type not cleaned up (empty dependent array)");

	for (int iTcfram = 0; iTcfram < cTcframDependent; ++iTcfram)
	{
		STypeCheckFrame * pTcfram = pUntype->m_arypTcframDependent[iTcfram];

		if (EWC_FVERIFY(pTcwork->m_arypTcframWaiting[pTcfram->m_ipTcframQueue] == pTcfram, "bookkeeping error (OnTypeResolve)"))
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframWaiting, &pTcwork->m_arypTcframPending);
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
		default:
			break;
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


void MarkAllSymbolsUsed(CSymbolTable * pSymtab)
{
	EWC::CHash<HV, SSymbol *>::CIterator iterSym(&pSymtab->m_hashHvPSym);

	SSymbol ** ppSym;
	while ((ppSym = iterSym.Next()))
	{
		SSymbol * pSymIt = nullptr;
		if (ppSym)
			pSymIt = *ppSym;

		while (pSymIt)
		{
			pSymIt->m_symdep = SYMDEP_Used;
			pSymIt = pSymIt->m_pSymPrev;
		}
	}

	for (ppSym = pSymtab->m_arypSymGenerics.A(); ppSym != pSymtab->m_arypSymGenerics.PMac(); ++ppSym)
	{
		SSymbol * pSymIt = *ppSym;
		while (pSymIt)
		{
			pSymIt->m_symdep = SYMDEP_Used;
			pSymIt = pSymIt->m_pSymPrev;
		}
	}
}

inline void ComputeSymbolUsage(SSymbol * pSym, CDynAry<SSymbol *> * parypSym)
{
	if (pSym->m_symdep != SYMDEP_Nil)
		return;

	parypSym->Append(pSym);
	while (parypSym->C())
	{
		SSymbol * pSym = parypSym->Last();
		size_t ipSym = parypSym->C() - 1;

		int cSymdepNil = 0;
		int cSymdepUsed = 0;
		if (pSym->m_symdep == SYMDEP_Nil)
		{
			SSymbol ** ppSymMac = pSym->m_aryPSymReferencedBy.PMac();
			for (SSymbol ** ppSymIt = pSym->m_aryPSymReferencedBy.A(); ppSymIt != ppSymMac; ++ppSymIt)
			{
				SSymbol * pSymRef = *ppSymIt;

				switch (pSymRef->m_symdep)
				{
					case SYMDEP_Nil:	
					{
						parypSym->Append(pSymRef);
						++cSymdepNil;
						
					} break;
					case SYMDEP_Used:
					{
						++cSymdepUsed;
					} break;
					default:
						break;
				}
			}

			if (cSymdepUsed == 0 && cSymdepNil != 0)
				break;

			pSym->m_symdep = (cSymdepUsed > 0) ? SYMDEP_Used: SYMDEP_Unused;
		}

		parypSym->RemoveFastByI(ipSym);
	}
}

void ComputeSymbolDependencies(CAlloc * pAlloc, SErrorManager * pErrman, CSymbolTable * pSymtabRoot)
{
	CDynAry<SSymbol *> arypSym(pAlloc, BK_Dependency, 1024);

	SLexerLocation lexloc;
	auto pSym = pSymtabRoot->PSymLookup("main", lexloc);
	if (!pSym)
	{
		EmitError(pErrman, &lexloc, ERRID_CantFindMain, "Failed to find global 'main' procedure");
		return;
	}

	pSym->m_symdep = SYMDEP_Used;

	CSymbolTable * pSymtabIt = pSymtabRoot;
	while (pSymtabIt)
	{
		EWC::CHash<HV, SSymbol *>::CIterator iterSym(&pSymtabIt->m_hashHvPSym);
		SSymbol ** ppSym;
		while ((ppSym = iterSym.Next()))
		{
			ComputeSymbolUsage(*ppSym, &arypSym);
		}

		SSymbol ** ppSymMac = pSymtabIt->m_arypSymGenerics.PMac();
		for (ppSym = pSymtabIt->m_arypSymGenerics.A(); ppSym != ppSymMac; ++ppSym)
		{
			EWC_ASSERT(*ppSym, "null symbol");
			ComputeSymbolUsage(*ppSym, &arypSym);
		}

		pSymtabIt = pSymtabIt->m_pSymtabNextManaged;
	}
}

void PerformTypeCheck(
	CAlloc * pAlloc,
	SErrorManager * pErrman,
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	CDynAry<SWorkspaceEntry *> * parypEntryChecked, 
	GLOBMOD globmod)
{
	auto pTcwork = EWC_NEW(pAlloc, STypeCheckWorkspace) STypeCheckWorkspace(pAlloc, pErrman, pblistEntry);

	SSymbol * pSymRoot = nullptr;
	// if we're in a unit test we spoof a top level implicit function symbol
	if (globmod == GLOBMOD_UnitTest)
	{
		pSymRoot = pSymtabTop->PSymEnsure(pErrman, "__ImplicitMethod", nullptr);
	}

	int ipTcfram = 0;
	BlockListEntry::CIterator iterEntry(pblistEntry);
	while (SWorkspaceEntry * pEntry = iterEntry.Next())
	{
		EWC_ASSERT(pEntry->m_pSymtab, "entry point without symbol table");
		STypeCheckFrame * pTcfram = pTcwork->m_blistTcfram.AppendNew();
		pTcfram->m_ipTcframQueue = ipTcfram;
		pTcfram->m_pEntry = pEntry;

		pTcfram->m_aryTcsent.SetAlloc(pAlloc, EWC::BK_TypeCheckStack);
		STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
		pTcsent->m_nState = 0;
		pTcsent->m_pStnod = pEntry->m_pStnod;
		pTcsent->m_pSymtab = pEntry->m_pSymtab;
		pTcsent->m_pStnodProcedure = nullptr;
		pTcsent->m_pSymContext = pSymRoot;
		pTcsent->m_grfsymlook = FSYMLOOK_Default;
		pTcsent->m_parkDeclContext = PARK_Nil;
		pTcsent->m_fAllowForwardDecl = false;
		pTcsent->m_tcctx = TCCTX_Normal;

		pTcwork->m_arypTcframPending.Append(pTcfram);
		++ipTcfram;
	}

	int cStoppingError = 0;
	while (pTcwork->m_arypTcframPending.C())
	{
		STypeCheckFrame * pTcfram = pTcwork->m_arypTcframPending[0];
		TCRET tcret = TcretTypeCheckSubtree(pTcwork, pTcfram);

		if (tcret == TCRET_StoppingError)
		{
			// make sure we've reported at least one error.
			if (!pErrman->FHasErrors() && !pErrman->FHasHiddenErrors())
			{
				SLexerLocation lexloc;
				EmitError(pErrman, &lexloc, ERRID_UnknownError, "Unknown error in type checker, quitting.");
			}

			while (pTcfram->m_aryTcsent.C())
			{
				STypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();
				PopTcsent(pTcfram, &pTcsentTop, nullptr);
			}

			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);
			++cStoppingError;
		}
		else if (tcret == TCRET_Complete)
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);
			if (EWC_FVERIFY(pTcfram->m_pEntry, "type check frame missing workspace entry"))
			{
				parypEntryChecked->Append(pTcfram->m_pEntry);
			}
		}
		else if (tcret == TCRET_WaitingForSymbolDefinition)
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, &pTcwork->m_arypTcframWaiting);
		}
		else
		{
			EWC_ASSERT(false, "Unhandled type check return value.")	
		}

		ValidateTcframArray(&pTcwork->m_arypTcframPending);
		ValidateTcframArray(&pTcwork->m_arypTcframWaiting);
	}

	CHash<const SSymbol *, SUnknownType>::CIterator iter(&pTcwork->m_hashPSymUntype);

	// NOTE: Don't report unresolved type errors if we stopped typechecking any entry points early, odds
	//  are that our missing type is in there and it makes for very confusing error messages...
	//  The *right* thing to do is have some kind of graph check to see if the type was actually skipped.
	const SSymbol ** ppSym;
	while (SUnknownType * pUntype = iter.Next(&ppSym))
	{
		EWC_ASSERT(pUntype->m_arypTcframDependent.C() > 0, "unknown type not cleaned up (empty dependent array)");

		int cTcframDependent = (s32)pUntype->m_arypTcframDependent.C();
		for (size_t iTcfram = 0; iTcfram < cTcframDependent; ++iTcfram)
		{
			auto pTcfram = pUntype->m_arypTcframDependent[iTcfram];
			if (cStoppingError == 0)
			{
				// Note: we're assuming the top thing on the stack is the thing we're waiting for.
				const STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.PLast();

				EmitError(pTcwork, pTcsent->m_pStnod, "Unresolved type '%s' reference found here", (*ppSym)->m_strName.PCoz());
			}

			// clean up the tcsent array for frames that are waiting on a symbol hidden by an error
			while (pTcfram->m_aryTcsent.C())
			{
				STypeCheckStackEntry * pTcsentTop = pTcfram->m_aryTcsent.PLast();
				PopTcsent(pTcfram, &pTcsentTop, nullptr);
			}
		}
	}

	//PerformFlushResolvedLiteralsPass(pTcwork, paryEntry);

	//check for top level collisions
	{
		EWC::CHash<HV, SSymbol *>::CIterator iterSym(&pSymtabTop->m_hashHvPSym);
		SSymbol ** ppSym;
		while ((ppSym = iterSym.Next()))
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
	
	if (globmod == GLOBMOD_UnitTest)
	{
		CSymbolTable * pSymtabIt = pSymtabTop;
		while(pSymtabIt)
		{
			MarkAllSymbolsUsed(pSymtabIt);
			pSymtabIt = pSymtabIt->m_pSymtabNextManaged;
		}
	}
	else
	{
		ComputeSymbolDependencies(pAlloc, pErrman, pSymtabTop);
	}

	pAlloc->EWC_DELETE(pTcwork);
}

#define EWC_ASSERT_EQUALS(LHS, RHS) AssertEquals(LHS, RHS, __FILE__, __LINE__)

void AssertEquals(const SBigInt & bintLhs, const SBigInt & bintRhs, const char * pChzFile, u32 nLine)
{
	EWC_ASSERT(FAreEqual(bintLhs, bintRhs),
		"%s (%d): expected %s%llu but calculated %s%llu",
		pChzFile, nLine,
		(bintLhs.m_fIsNegative) ? "-" : "", bintLhs.m_nAbs,
		(bintRhs.m_fIsNegative) ? "-" : "", bintRhs.m_nAbs);
}

bool FTestSigned65()
{
	/* Failing on x64, shifting by 64 bits is undefined behavior
	s64 x = 0xFFFFFFFFFFFFFFFF << 64;

	s64 nL = -400000000;
	s64 nR = 0;
	s64 shift = nL >> nR;
	EWC_ASSERT_EQUALS(BintShiftRight(BintFromInt(nL), BintFromInt(nR)), BintFromInt(nL >> nR));
	*/

	s64 s_aNSigned[] = { 0, 1, -1, 400000000, -400000000 }; //, LLONG_MAX, LLONG_MIN + 1

	// NOTE: This does not replicate s64 overflow exactly, Signed65 structs overflow like a u64 but maintaining sign 
	// values. It's not clear that this is the wrong behavior for literals... it should probably throw an overflow
	// error (?)

	EWC_ASSERT_EQUALS(BintShiftRight(BintFromInt(-1), BintFromInt(1)), BintFromInt(-1 >> 1));
	EWC_ASSERT_EQUALS(BintSub(BintFromInt(0), BintFromInt(LLONG_MIN+1)), BintFromInt(LLONG_MAX));

	for (int iNLhs = 0; iNLhs < EWC_DIM(s_aNSigned); ++iNLhs)
	{
		for (int iNRhs = 0; iNRhs < EWC_DIM(s_aNSigned); ++iNRhs)
		{
			s64 nLhs = s_aNSigned[iNLhs];
			s64 nRhs = s_aNSigned[iNRhs];
			EWC_ASSERT_EQUALS(BintAdd(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs + nRhs));
			EWC_ASSERT_EQUALS(BintSub(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs - nRhs));
			EWC_ASSERT_EQUALS(BintMul(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs * nRhs));

			EWC_ASSERT_EQUALS(BintBitwiseOr(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs | nRhs));
			EWC_ASSERT_EQUALS(BintBitwiseAnd(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs & nRhs));

			if (nRhs >= 0)
			{
				// shifting by more than the number of bits in a type results in undefined behavior
				auto nRhsClamp = (nRhs > 31) ? 31 : nRhs;
			
				EWC_ASSERT_EQUALS(BintShiftRight(BintFromInt(nLhs), BintFromInt(nRhsClamp)), BintFromInt(nLhs >> nRhsClamp));
				EWC_ASSERT_EQUALS(BintShiftLeft(BintFromInt(nLhs), BintFromInt(nRhsClamp)), BintFromInt(nLhs << nRhsClamp));
			}

			if (nRhs != 0)
			{
				EWC_ASSERT_EQUALS(BintDiv(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs / nRhs));
				EWC_ASSERT_EQUALS(BintRemainder(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs % nRhs));
			}
		}
	}

	
	u64 s_aNUnsigned[] = { 0, 1, 400000000, ULLONG_MAX };

	for (int iNLhs = 0; iNLhs < EWC_DIM(s_aNUnsigned); ++iNLhs)
	{
		for (int iNRhs = 0; iNRhs < EWC_DIM(s_aNUnsigned); ++iNRhs)
		{
			u64 nLhs = s_aNUnsigned[iNLhs];
			u64 nRhs = s_aNUnsigned[iNRhs];
			EWC_ASSERT_EQUALS(BintAdd(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs + nRhs));

			EWC_ASSERT_EQUALS(BintBitwiseOr(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs | nRhs));
			EWC_ASSERT_EQUALS(BintBitwiseAnd(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs & nRhs));

			// shifting by more than the number of bits in a type results in undefined behavior
			auto nRhsClamp = (nRhs > 31) ? 31 : nRhs;
		
			EWC_ASSERT_EQUALS(BintShiftRight(BintFromUint(nLhs), BintFromUint(nRhsClamp)), BintFromUint(nLhs >> nRhsClamp));
			EWC_ASSERT_EQUALS(BintShiftLeft(BintFromUint(nLhs), BintFromUint(nRhsClamp)), BintFromUint(nLhs << nRhsClamp));

			// does not replicate unsigned underflow, because the sign bit is tracked seperately
			if (nLhs >= nRhs)
			{
				EWC_ASSERT_EQUALS(BintSub(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs - nRhs));
			}

			EWC_ASSERT_EQUALS(BintMul(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs * nRhs));
			if (nRhs != 0)
			{
				EWC_ASSERT_EQUALS(BintDiv(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs / nRhs));
				EWC_ASSERT_EQUALS(BintRemainder(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs % nRhs));
			}
		}
	}

	EWC_ASSERT_EQUALS(BintAdd(BintFromUint(ULLONG_MAX, true), BintFromUint(100)), BintFromUint(ULLONG_MAX - 100, true));
	EWC_ASSERT_EQUALS(BintAdd(BintFromUint(ULLONG_MAX, true), BintFromUint(ULLONG_MAX)), BintFromUint(0));
	EWC_ASSERT_EQUALS(BintSub(BintFromUint(100), BintFromUint(ULLONG_MAX)), BintFromUint(ULLONG_MAX - 100, true));

	return true;
}

void SwapDoubleHashForPlatformBits(const char * pChInput, char * aChOut, size_t cB)
{
#if EWC_X64
	const char * pChzWord = "64";
#else
	const char * pChzWord = "32";
#endif

	//size_t cCh = CCh(pCozExpected);
	for (size_t iCh = 0; iCh < cB; ++iCh)
	{
		if (iCh < cB - 2 && pChInput[iCh] == '#' && pChInput[iCh + 1] == '#')
		{
			aChOut[iCh] = pChzWord[0];
			++iCh;
			aChOut[iCh] = pChzWord[1];
		}
		else
		{
			aChOut[iCh] = pChInput[iCh];
		}
	}
}
