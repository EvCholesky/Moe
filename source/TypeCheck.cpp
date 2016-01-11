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

#include "JaiParse.h"
#include "JaiTypes.h"
#include "Workspace.h"
#include <cstdarg>
#include <limits.h>
#include <stdio.h>

using namespace EWC;

struct STypeCheckStackEntry // tag = tcsent
{
	int				m_nState;
	CSTNode *		m_pStnod;
	CSymbolTable *	m_pSymtab;			// BB - Could omit this pointer with careful handling of stack pops?
										//  maybe swap out for fPushedStack?
	CSTNode *		m_pStnodProcedure;	// definition node for current procedure
	GRFSYMLOOK		m_grfsymlook;
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

struct STypeCheckWorkspace // tag = tcwork
{
					STypeCheckWorkspace(CAlloc * pAlloc, int cTcfram)
					:m_pAlloc(pAlloc)
					,m_nIdNext(0)
					,m_cError(0)
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
	int										m_nIdNext;
	int										m_cError;
	CAry<STypeCheckFrame>					m_aryTcfram;
	CHash<const SSymbol *, SUnknownType>	m_hashPSymUntype;

	CAry<STypeCheckFrame *>					m_arypTcframPending;	// frames ready to be run again (may stop 
																	//  during check, not guaranteed to have all types)
	CAry<STypeCheckFrame *>					m_arypTcframWaiting;	// frames waiting for one specific symbol
};



void OnTypeComplete(STypeCheckWorkspace * pTcwork, const SSymbol * pSym);


// by the time we get to type checking we've parsed the whole program, we should have a tree of symbol tables containing
//	1. names of every global and struct nested types (but not sizing, const values, typedefs)
//  2. all named global identifiers (potentially with some typenames? not sure that helps)

// Note: all declarations within a procedure are expected in-order (for now) so we just add them to the symbol table
//  during type checking.

// Type checking walks over all entry frames, pausing to wait for the type-checking/sizing of types used in any 
//  definitions.

// My (half-cooked) strategy for handling arbitrary ordering within non-global scopes is to insert TypeDefinitions 
//  before any statements in their containing scope.

void EmitError(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, const char * pChz, ...)
{
	// BB - need to do file/line lookup from pStnod
	//printf("%s(%d) error:", pJlex->m_pChzFilename, NLine(pJlex));
	
	const SLexerLocation & lexloc = pStnod->m_lexloc;
	printf ("%s(%d) error: ", lexloc.m_strFilename.PChz(), -1);
	++pTcwork->m_cError;
	
	if (pChz)
	{
		va_list ap;
		va_start(ap, pChz);
		vprintf(pChz, ap);
		printf("\n");
	}
}

CString StrTypenameFromTypeSpecification(CSTNode * pStnod)
{
	char aCh[2048];
	char * pCh = aCh;
	char * pChEnd = EWC_PMAC(aCh);

	CSTNode * pStnodIt = pStnod;
	while (pStnodIt)
	{
		switch (pStnodIt->m_park)
		{
			case PARK_Identifier:
			{
				if (!EWC_FVERIFY(pStnod->m_pStval, "identifier without value string detected"))
					break;
				pCh += CChCopy(pStnod->m_pStval->m_str.PChz(), pCh, pChEnd - pCh); 
				pStnodIt = nullptr;
			}break;
			case PARK_Reference:
				pCh += CChCopy("* ", pCh, pChEnd - pCh); 

				EWC_ASSERT(pStnodIt->CStnodChild() == 1);
				pStnodIt = pStnodIt->PStnodChild(0);
				break;
			case PARK_ArrayDecl:
				// BB - should follow the [], [..], [c] convention
				EWC_ASSERT(false, "not type-checking asserts yet");
				pStnodIt=  nullptr;

				break;
			default:
				pCh += CChCopy("<BadPark> ", pCh, pChEnd - pCh); 
				pStnod = nullptr;
				break;
		}
	}

	return CString(aCh);
}

STypeInfo * PTinFromTypeSpecification(CSymbolTable * pSymtab, CSTNode * pStnod, GRFSYMLOOK grfsymlook, SSymbol **ppSymType)
{
	// returns null if this is an instance of an unknown type, if this is a reference to an unknown type we will return
	//  tinptr->tin(TINK_Unknown) because we need this to handle a struct with a pointer to an instance of itself.

	// Essentially, any non-null returned from this should be enough to determine the size of this type spec and 
	//  determine target type equivalence.

	CAlloc * pAlloc = pSymtab->m_pAlloc;

	STypeInfo * pTinReturn = nullptr;
	STypeInfo ** ppTinCur = &pTinReturn;
	bool fAllowForwardDecl = false;

	// loop and find the concrete target type
	STypeInfo * pTinFinal = nullptr;
	CSTNode * pStnodIt = pStnod;
	while (pStnodIt)
	{
		if (pStnodIt->m_park == PARK_Identifier)
		{
			if (!EWC_FVERIFY(pStnodIt->m_pStval, "identifier without value string detected"))
				break;

			pTinFinal = pSymtab->PTinLookup(pStnodIt->m_pStval->m_str, pStnodIt->m_lexloc, grfsymlook, ppSymType);

			if ((pTinFinal == nullptr) & fAllowForwardDecl)
			{
				pTinFinal = pSymtab->PTinfwdLookup(pStnodIt->m_pStval->m_str, grfsymlook);
				// NOTE: we're not setting pStnodIt->m_pTin to point at the forward decl because we won't know to update
				//  the pointer
			}
			else
			{
				pStnodIt->m_pTin = pTinFinal;
			}
			break;
		}
		else
		{
			fAllowForwardDecl |= pStnodIt->m_park == PARK_Reference;
			EWC_ASSERT(pStnodIt->m_park == PARK_Reference || pStnodIt->m_park == PARK_ArrayDecl);
			EWC_ASSERT(pStnodIt->CStnodChild() == 1);
			pStnodIt = pStnodIt->PStnodChild(0);
		}
	}

	if (!pTinFinal)
		return nullptr;

	// build the fully qualified type info
	pStnodIt = pStnod;
	STypeInfo * pTinPrev = nullptr;
	while (pStnodIt)
	{
		if (pStnodIt->m_park == PARK_Reference)
		{
			STypeInfoPointer * pTinptr = EWC_NEW(pAlloc, STypeInfoPointer) STypeInfoPointer();
			pSymtab->AddManagedTin(pTinptr);
			pStnodIt->m_pTin = pTinptr;

			pTinPrev = pTinptr;
			*ppTinCur = pTinptr;
			ppTinCur = &pTinptr->m_pTinPointedTo;

			EWC_ASSERT(pStnodIt->CStnodChild() == 1);
			pStnodIt = pStnodIt->PStnodChild(0);
		}
		else if (pStnodIt->m_park == PARK_ArrayDecl)
		{
			EWC_ASSERT(false, "not handling arrays in typecheck yet");
			//STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
			//*ppTinCur = pTinary;
			//ppTinCur = &pTinary->m_pTin;

			// Need a way to pass the [..] vs [] vs [constExpr] distinction 

			// if this is a pointer an array of an unknown type, the array is the unknown. I guess that's TBD
		}
		if (pStnodIt->m_park == PARK_Identifier)
		{
			if (pTinFinal->m_tink == TINK_ForwardDecl &&
				EWC_FVERIFY(pTinPrev != nullptr, "how did we get here without a prev type info?"))
			{
				STypeInfoForwardDecl * pTinfwd = (STypeInfoForwardDecl *)pTinFinal;
				pTinfwd->m_arypTinReferences.Append(pTinPrev);
			}

			*ppTinCur = pTinFinal;
			ppTinCur = nullptr;
			break;
		}
	}
	return pTinReturn;
}

CString StrFromTypeInfo(STypeInfo * pTin)
{
	char aCh[1024];

	CChPrintTypeInfo(pTin, PARK_Nil, aCh, EWC_PMAC(aCh));
	return CString(aCh);
}

CString StrFullyQualifiedSymbol(SSymbol * pSym)
{
	char aCh[256];
	CChFormat(aCh, EWC_DIM(aCh), "TBD::TBD::%s",pSym->m_strName.PChz());
	return CString(aCh);
}

SUnknownType * PUntypeEnsure(STypeCheckWorkspace * pTcwork, const SSymbol * pSym)
{
	SUnknownType * pUntype = pTcwork->m_hashPSymUntype.Lookup(pSym);
	if (!pUntype)
	{
		pTcwork->m_hashPSymUntype.FinsEnsureKey(pSym, &pUntype);
		pUntype->m_aryiTcframDependent.SetAlloc(pTcwork->m_pAlloc);
	}
	return pUntype;
}

void PushTcsent(STypeCheckFrame * pTcfram, CSTNode * pStnod)
{
	STypeCheckStackEntry * pTcsentPrev = pTcfram->m_aryTcsent.PLast();
	STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
	*pTcsent = *pTcsentPrev;

	pTcsent->m_nState = 0;
	pTcsent->m_pStnod = pStnod;
}

void PopTcsent(STypeCheckFrame * pTcfram)
{
	pTcfram->m_aryTcsent.PopLast();
}
enum TCRET
{
	TCRET_Complete,
	TCRET_StoppingError,
	TCRET_WaitingForSymbolDefinition,
};

STypeInfo * PTinPromoteLiteralDefault(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, CSTNode * pStnodLit)
{
	STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	if (!pTinlit || pTinlit->m_tink != TINK_Literal)
		return pTinlit;

	const CSTValue & stval = pTinlit->m_stval;
	const SLiteralType & litty = stval.m_litty;
	switch (litty.m_litk)
	{
	case LITK_Integer:
		{
			TFN tfnSigned = stval.m_tfnSigned;
			if ((tfnSigned != TFN_True) & (stval.m_nUnsigned >= LLONG_MAX))
			{
				tfnSigned = TFN_False;
			}
			if (tfnSigned == TFN_False)		return pSymtab->PTinBuiltin("uint");
			else							return pSymtab->PTinBuiltin("int");
		}
	case LITK_Float:	return pSymtab->PTinBuiltin("float");
	case LITK_Char:		return pSymtab->PTinBuiltin("char");
	case LITK_String:	return pSymtab->PTinBuiltin("string");
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:
		{
			EmitError(pTcwork, pStnodLit, "Cannot infer type for null");
		}
	}
	return nullptr;
}

inline s64 NSignedLiteralCast(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, CSTValue * pStval)
{
	if (!pStval->m_litty.m_fIsSigned)
	{
		if (pStval->m_nUnsigned >= LLONG_MAX)
		{
			EmitError(pTcwork, pStnod, "Literal is too large for implicitly cast to signed int.");
		}
		return (s64)pStval->m_nUnsigned;
	}
	return pStval->m_nSigned;
}

inline F64 GLiteralCast(CSTValue * pStval)
{
	switch (pStval->m_litty.m_litk)
	{
	case LITK_Integer:	return (pStval->m_litty.m_fIsSigned) ? (F64)pStval->m_nSigned : (F64)pStval->m_nUnsigned;
	case LITK_Float:	return pStval->m_g;
	default: EWC_ASSERT(false, "expected number");
	}
	return 0.0;
}

inline bool FComputeBinaryOpOnLiterals(
	STypeCheckWorkspace * pTcwork,
	JTOK jtokOperand,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLhs,
	CSTNode * pStnodRhs, 
	STypeInfoLiteral ** ppTinlit,
	CSTValue ** ppStval)
{
	STypeInfo * pTinLhs = pStnodLhs->m_pTin;
	STypeInfo * pTinRhs = pStnodRhs->m_pTin;

	if ((pTinLhs->m_tink != TINK_Literal) | (pTinRhs->m_tink != TINK_Literal) |
		(pStnodLhs->m_pStval == nullptr) | (pStnodRhs->m_pStval == nullptr))
		return false;

	STypeInfoLiteral * pTinlitLhs = (STypeInfoLiteral *)pTinLhs;
	STypeInfoLiteral * pTinlitRhs = (STypeInfoLiteral *)pTinRhs;
	CSTValue * pStvalLhs = pStnodLhs->m_pStval;
	CSTValue * pStvalRhs = pStnodRhs->m_pStval;
	const SLiteralType & littyLhs = pStvalLhs->m_litty;
	const SLiteralType & littyRhs = pStvalRhs->m_litty;

	bool fLhsIsNumber = (littyLhs.m_litk == LITK_Float) | (littyLhs.m_litk == LITK_Integer);
	bool fRhsIsNumber = (littyRhs.m_litk == LITK_Float) | (littyRhs.m_litk == LITK_Integer);
	if ((fLhsIsNumber == false) | (fRhsIsNumber == false))
		return false;

	if ((littyLhs.m_litk == LITK_Float) | (littyRhs.m_litk == LITK_Float))
	{
		F64 g;
		F64 gLhs = GLiteralCast(pStvalLhs);
		F64 gRhs = GLiteralCast(pStvalRhs);
		switch (jtokOperand)
		{
		case JTOK('+'): g = gLhs + gRhs; break;
		case JTOK('-'): g = gLhs - gRhs; break;
		case JTOK('*'): g = gLhs * gRhs; break;
		case JTOK('/'): g = gLhs / gRhs; break;
		default: return false;
		}

		CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
		*pStvalStnod = *pStvalLhs;
		pStvalStnod->m_g = g;
		pStvalStnod->m_litty.m_litk = LITK_Float;
		*ppStval = pStvalStnod;

		STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
		pSymtab->AddManagedTin(pTinlit);
		pTinlit->m_stval = *pStvalStnod;
		*ppTinlit = pTinlit;
		return true;
	} 
	else // both LITK_Integer
	{
		EWC_ASSERT(littyLhs.m_cBit == -1, "expected unsized literal here");
		EWC_ASSERT(littyRhs.m_cBit == -1, "expected unsized literal here");

		bool fMixedSign = littyLhs.m_fIsSigned != littyRhs.m_fIsSigned;
		if (!fMixedSign)
		{
			// consider it mixed sign if the result will be negative
			fMixedSign |= ((jtokOperand == JTOK('-')) & (pStvalRhs->m_nUnsigned > pStvalLhs->m_nUnsigned));
		}

		if (fMixedSign | littyLhs.m_fIsSigned)
		{
			s64 nLhs = NSignedLiteralCast(pTcwork, pStnodLhs, pStvalLhs);
			s64 nRhs = NSignedLiteralCast(pTcwork, pStnodRhs, pStvalRhs);

			s64 n;
			switch (jtokOperand)
			{
			case JTOK('+'): n = nLhs + nRhs; break;
			case JTOK('-'): n = nLhs - nRhs; break;
			case JTOK('*'): n = nLhs * nRhs; break;
			case JTOK('/'): n = nLhs / nRhs; break;
			default: return false;
			}

			CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
			*pStvalStnod = *pStvalLhs;
			pStvalStnod->m_nSigned = n;
			pStvalStnod->m_litty.m_fIsSigned = (n < 0);
			*ppStval = pStvalStnod;

			STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
			pSymtab->AddManagedTin(pTinlit);
			pTinlit->m_stval = *pStvalStnod;
			*ppTinlit = pTinlit;
		}
		else // unsigned
		{
			u64 n;
			switch (jtokOperand)
			{
			case JTOK('+'): n = pStvalLhs->m_nUnsigned + pStvalRhs->m_nUnsigned; break;
			case JTOK('-'): n = pStvalLhs->m_nUnsigned - pStvalRhs->m_nUnsigned; break;
			case JTOK('*'): n = pStvalLhs->m_nUnsigned * pStvalRhs->m_nUnsigned; break;
			case JTOK('/'): n = pStvalLhs->m_nUnsigned / pStvalRhs->m_nUnsigned; break;
			default: return false;
			}

			CSTValue * pStvalStnod = EWC_NEW(pSymtab->m_pAlloc, CSTValue) CSTValue();
			*pStvalStnod = *pStvalLhs;
			pStvalStnod->m_nUnsigned = n;
			*ppStval = pStvalStnod;

			STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
			pSymtab->AddManagedTin(pTinlit);
			pTinlit->m_stval = *pStvalStnod;
			*ppTinlit = pTinlit;
		}
		return true;
	}
}

inline STypeInfo * PTinPromoteLiteralTightest(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,	
	STypeInfo * pTinDest)
{
	STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	if (!pTinlit || pTinlit->m_tink != TINK_Literal)
		return pTinlit;

	const CSTValue & stval = pTinlit->m_stval;
	const SLiteralType & litty = stval.m_litty;

	switch (litty.m_litk)
	{
	case LITK_Integer:
		{
			TFN tfnSigned = stval.m_tfnSigned;
			if ((tfnSigned != TFN_True) & (stval.m_nUnsigned >= LLONG_MAX))
			{
				tfnSigned = TFN_False;
			}
			if (tfnSigned == TFN_False)
			{
				if (stval.m_nUnsigned < UCHAR_MAX)		return pSymtab->PTinBuiltin("u8");
				if (stval.m_nUnsigned < USHRT_MAX)		return pSymtab->PTinBuiltin("u16");
				if (stval.m_nUnsigned < UINT_MAX)		return pSymtab->PTinBuiltin("u32");
				else									return pSymtab->PTinBuiltin("u64");
			}
			else
			{
				if ((stval.m_nSigned < SCHAR_MAX) & (stval.m_nSigned > SCHAR_MIN))	return pSymtab->PTinBuiltin("s8");
				if ((stval.m_nSigned < SHRT_MAX) & (stval.m_nSigned > SHRT_MIN))	return pSymtab->PTinBuiltin("s16");
				if ((stval.m_nSigned < INT_MAX) & (stval.m_nSigned > INT_MIN))		return pSymtab->PTinBuiltin("s32");
				else																return pSymtab->PTinBuiltin("s64");
			}
		}
	case LITK_Float:	return pSymtab->PTinBuiltin("float");
	case LITK_Char:		return pSymtab->PTinBuiltin("char");
	case LITK_String:	return pSymtab->PTinBuiltin("string");
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:		
		{
			if (pTinDest && pTinDest->m_tink == TINK_Pointer)
			{
				return pTinDest;
			}
			EmitError(pTcwork, pStnodLit, "Trying to initialize non pointer type with null value");
		}
	}
	return nullptr;
}

STypeInfo * PTinFindUpcast(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	STypeInfo * pTinA,
	STypeInfo * pTinB)
{
	if (pTinA->m_tink == pTinB->m_tink)
	{
		if (pTinA->m_tink == TINK_Float)
		{
			STypeInfoFloat * pTinfloatA = (STypeInfoFloat *)pTinA;
			STypeInfoFloat * pTinfloatB = (STypeInfoFloat *)pTinB;
			return (pTinfloatA->m_cBit >= pTinfloatB->m_cBit) ? pTinA : pTinB;
		}
		if (pTinA->m_tink == TINK_Integer)
		{
			STypeInfoInteger * pTinintA = (STypeInfoInteger *)pTinA;
			STypeInfoInteger * pTinintB = (STypeInfoInteger *)pTinB;

			if (pTinintA->m_fIsSigned != pTinintB->m_fIsSigned)
				return nullptr;
		
			return (pTinintA->m_cBit >= pTinintB->m_cBit) ? pTinA : pTinB;
		}
		if (pTinA == pTinB)
			return pTinA;
	}
	return nullptr;
}

bool FCanImplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
	EWC_ASSERT(pTinSrc->m_tink != TINK_Literal, "literals should be promoted before calling FCanImplicitCast()");

	if (pTinSrc->m_tink == pTinDst->m_tink)
	{
		// Note - can't just compare pointers directly as tins are not unique. (but they should be)
		switch (pTinSrc->m_tink)
		{
		case TINK_Integer:
			{
				STypeInfoInteger * pTinintSrc = (STypeInfoInteger *)pTinSrc;
				STypeInfoInteger * pTinintDst = (STypeInfoInteger *)pTinDst;

				// BB - this could be more forgiving... allow signed/unsigned conversions if a higher cBit
				return (pTinintDst->m_cBit >= pTinintSrc->m_cBit) & (pTinintDst->m_fIsSigned == pTinintSrc->m_fIsSigned);
			} break;
		case TINK_Float:
			{
				STypeInfoFloat * pTinfloatSrc = (STypeInfoFloat *)pTinSrc;
				STypeInfoFloat * pTinfloatDst = (STypeInfoFloat *)pTinDst;
				return pTinfloatDst->m_cBit >= pTinfloatSrc->m_cBit;
			} break;
		case TINK_Bool: return true;
		case TINK_String: return true;
		case TINK_Pointer: return pTinSrc == pTinDst;	// BB - not safe, type infos are not unique
		case TINK_Enum: return pTinSrc == pTinDst;	// BB - not safe, type infos are not unique
		default: return false;
		}
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

bool FIsValidLhs(const CSTNode * pStnod)
{
	// BB - this is just returning the easy failures... needs a more thorough check.
	STypeInfo * pTin = pStnod->m_pTin;
	if (!pTin)
		return false;

	TINK tink = pTin->m_tink;
	return (tink != TINK_Null) & (tink != TINK_Void) & (tink != TINK_Literal);
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

TCRET TypeCheckSubtree(STypeCheckWorkspace * pTcwork, STypeCheckFrame * pTcfram)
{
	int i = 2;
	i = - + 2;
	int b = ~i;

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
					{	// type check the parameter list
						if (pStproc->m_iStnodParameterList >= 0)
						{
							PushTcsent(pTcfram, pStnod->PStnodChild(pStproc->m_iStnodParameterList));
						}
					}break;
				case 1:
					{
						if (pStproc->m_iStnodParameterList >= 0)
						{
							CSTNode * pStnodParamList = pStnod->PStnodChild(pStproc->m_iStnodParameterList);

							EWC_ASSERT(pTinproc->m_arypTinParams.C() == pStnodParamList->CStnodChild(), "parameter child mismatch");
							for (int iStnodArg = 0; iStnodArg < pStnodParamList->CStnodChild(); ++iStnodArg)
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
								STypeInfo * pTinReturn = PTinFromTypeSpecification(
									pTcsentTop->m_pSymtab,
									pStnodReturn,
									pTcsentTop->m_grfsymlook,
									nullptr);
								if (!pTinReturn)
								{
									EmitError(pTcwork, pStnod, "failed to parse return type");
								}
								pStnodReturn->m_pTin = pTinReturn;
							}
							pStnod->m_strees = STREES_SignatureTypeChecked;

							// find our symbol and resolve any pending unknown types

							CSTNode * pStnodIdent = nullptr;
							if (pStproc->m_iStnodProcName >= 0)
							{
								CString strProcName = StrFromIdentifier(pStnod->PStnodChild(pStproc->m_iStnodProcName));
								pStnodIdent = pStnod->PStnodChildSafe(pStproc->m_iStnodProcName);
							}

							if (EWC_FVERIFY(pStnodIdent && pStnodIdent->m_pStval, "Procedure without identifier"))
							{
								SSymbol * pSymIdent = pTcsentTop->m_pSymtab->PSymLookup(
																				pStnodIdent->m_pStval->m_str,
																				pStnodIdent->m_lexloc, 
																				pTcsentTop->m_grfsymlook);
								OnTypeComplete(pTcwork, pSymIdent);
							}
						}

						// type check the body list
						if (pStproc->m_iStnodBody >= 0)
						{
							CSTNode * pStnodBody = pStnod->PStnodChild(pStproc->m_iStnodBody);
							PushTcsent(pTcfram, pStnodBody);

							STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
							pTcsentPushed->m_pStnodProcedure = pStnod;
							pTcsentPushed->m_pSymtab = pStnodBody->m_pSymtab;
						}
					}break;
				case 2:
					{
						SSymbol * pSymProc = nullptr;
						CString strProcName;
						if (pStproc->m_iStnodProcName >= 0)
						{
							strProcName = StrFromIdentifier(pStnod->PStnodChild(pStproc->m_iStnodProcName));
							if (!strProcName.FIsEmpty())
							{
								pSymProc = pTcsentTop->m_pSymtab->PSymLookup(
																	strProcName,
																	pStnod->m_lexloc,
																	pTcsentTop->m_grfsymlook);
							}
						}

						if (!EWC_FVERIFY(pSymProc, "failed to find procedure name symbol: %s", strProcName.PChz()))
							return TCRET_StoppingError;
						if (!EWC_FVERIFY(pSymProc->m_pTin, "expected procedure type info to be created during parse"))
							return TCRET_StoppingError;

						pStnod->m_pSym = pSymProc;
						PopTcsent(pTcfram);

						pStnod->m_strees = STREES_TypeChecked;
						OnTypeComplete(pTcwork, pSymProc);
					}break;
				}
			}break;
			case PARK_ProcedureCall:
			{
				//BB - this expects the argument list to be preceded by an identifier, we're not handling calling
				// procedures by pointer yet.

				if (pTcsentTop->m_nState == 0)
				{
					// skip type checking the identifier
					++pTcsentTop->m_nState;
				}

				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					CSTNode * pStnodIdent = pStnod->PStnodChild(0);

					SSymbol * pSymProc = nullptr;
					CString strProcName = StrFromIdentifier(pStnodIdent);
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					if (!strProcName.FIsEmpty())
					{
						pSymProc = pSymtab->PSymLookup(strProcName, pStnod->m_lexloc, pTcsentTop->m_grfsymlook);
					}
					pStnod->m_pSym = pSymProc;

					if (!pSymProc || !pSymProc->m_pStnodDefinition)
					{
						EmitError(pTcwork, pStnod, "unknown procedure in type check: %s", strProcName.PChz());
						return TCRET_StoppingError;
					}

					CSTNode * pStnodDefinition = pSymProc->m_pStnodDefinition;
					if (pStnodDefinition->m_strees < STREES_SignatureTypeChecked)
					{
						// wait for this procedure's signature to be type checked.
						SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymProc);
						pUntype->m_aryiTcframDependent.Append((int)pTcwork->m_aryTcfram.IFromP(pTcfram));
						return TCRET_WaitingForSymbolDefinition;
					}

					STypeInfoProcedure * pTinproc = (STypeInfoProcedure *)pStnodDefinition->m_pTin;
					if (!EWC_FVERIFY(pTinproc && pTinproc->m_tink == TINK_Procedure, "bad procedure type info"))
						return TCRET_StoppingError;

					for (int iStnodArg = 1; iStnodArg < pStnod->CStnodChild(); ++iStnodArg)
					{
						CSTNode * pStnodArg = pStnod->PStnodChild(iStnodArg);
						STypeInfo * pTinCall = pStnodArg->m_pTin;
						STypeInfo * pTinParam = pTinproc->m_arypTinParams[iStnodArg - 1];
						if (!EWC_FVERIFY(pTinParam, "unknown parameter type"))
							continue;

						pTinCall = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodArg, pTinParam);
						if (!FCanImplicitCast(pTinCall, pTinParam))
						{
							//BB - need fullyQualifiedTypenames
							CString strTinCall = StrFromTypeInfo(pTinCall);
							CString strTinParam = StrFromTypeInfo(pTinParam);
							EmitError(pTcwork, pStnod, "no implicit conversion from  type %s to %s",
								strTinCall.PChz(),
								strTinParam.PChz());
						}
					}

					STypeInfo * pTinReturn = nullptr;
					CSTProcedure * pStproc = pStnodDefinition->m_pStproc;
					if (EWC_FVERIFY(pStproc, "bad procedure return info"))
					{
						if (pStproc->m_iStnodReturnType >= 0)
						{
							pTinReturn = pStnodDefinition->PStnodChild(pStproc->m_iStnodReturnType)->m_pTin;
						}
					}

					pStnod->m_pTin = pTinReturn;
					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
			case PARK_EnumDefinition:
			{
				EWC_ASSERT(false, "WIP Enum typeCheck not finished");
				return TCRET_StoppingError;
			}break;
			case PARK_EnumConstant:
			{
				EWC_ASSERT(false, "WIP Enum typeCheck not finished");
				return TCRET_StoppingError;
			}break;
			case PARK_StructDefinition:
			{
				// Note: there isn't struct value data, layout is just children[identifierName, DeclList]
				SSymbol * pSymStruct = nullptr;
				CString strStructName;
				if (pStnod->CStnodChild() >= 0)
				{
					strStructName = StrFromIdentifier(pStnod->PStnodChild(0));
					if (!strStructName.FIsEmpty())
					{
						pSymStruct = pTcsentTop->m_pSymtab->PSymLookup(
																strStructName,
																pStnod->m_lexloc,
																pTcsentTop->m_grfsymlook);
					}
				}

				if (!EWC_FVERIFY(pSymStruct, "failed to find structure name symbol"))
					return TCRET_StoppingError;
				if (!EWC_FVERIFY(pSymStruct->m_pTin, "expected structure type info to be created during parse"))
					return TCRET_StoppingError;

				if (pTcsentTop->m_nState == 0)
				{
					if (pStnod->CStnodChild() >= 1)
					{
						PushTcsent(pTcfram, pStnod->PStnodChild(1));
					}
					++pTcsentTop->m_nState;
				}
				else
				{
					PopTcsent(pTcfram);

					pStnod->m_strees = STREES_TypeChecked;
					OnTypeComplete(pTcwork, pSymStruct);
				}
			}break;

			case PARK_Identifier:
			{
				// Note: we're only expecting to get here for identifiers within statements.
				//  Identifiers for function names, declaration names and types, should do their own type checking.

				CSTValue * pStval = pStnod->m_pStval;
				if (EWC_FVERIFY(pStval, "identifier node with no value"))
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					SSymbol * pSym = pSymtab->PSymLookup(pStval->m_str, pStnod->m_lexloc, pTcsentTop->m_grfsymlook);
					if (!pSym || !pSym->m_pStnodDefinition)
					{
						EmitError(pTcwork, pStnod, "'%s' unknown identifier detected", pStval->m_str.PChz());
						return TCRET_StoppingError;
					}


					CSTNode * pStnodDefinition = pSym->m_pStnodDefinition;
					if (pStnodDefinition->m_park == PARK_Decl)
					{
						if (pStnodDefinition->m_strees >= STREES_TypeChecked)
						{
							EWC_ASSERT(pStnodDefinition->m_pTin, "symbol definition was type checked, but has no type?");
							pStnod->m_pTin = pStnodDefinition->m_pTin;
							pStnod->m_pSym = pSym;
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
						// TODO: handle function, enum, and struct definitions
					}
				}

				PopTcsent(pTcfram);
				pStnod->m_strees = STREES_TypeChecked;
			}break;

			case PARK_ParameterList:
			case PARK_List:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));

				if (pStnod->m_park == PARK_List)
				{
					STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
					EWC_ASSERT(pStnod->m_pSymtab, "null symbol table");
					pTcsentPushed->m_pSymtab = pStnod->m_pSymtab;
				}

			}break;
			case PARK_Decl:
			{
				CSTDecl * pStdecl = pStnod->m_pStdecl;
				EWC_ASSERT(pStdecl, "missing decl parse data");

				if (pTcsentTop->m_nState == 0)	// type check initializer 
				{
					if (pStdecl->m_iStnodInit >= 0)
					{
						PushTcsent(pTcfram, pStnod->PStnodChild(pStdecl->m_iStnodInit));
					}
					++pTcsentTop->m_nState;
				}
				else if (pTcsentTop->m_nState == 1) // resolve actual type
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					if (pStdecl->m_iStnodType >= 0)
					{
						CSTNode * pStnodType = pStnod->PStnodChild(pStdecl->m_iStnodType);

						SSymbol * pSymType = nullptr;
						STypeInfo * pTinType = PTinFromTypeSpecification(
												pSymtab,
												pStnodType,
												pTcsentTop->m_grfsymlook,
												&pSymType);

						if (pTinType)
						{
							pStnod->m_pTin = pTinType;
						}
						else
						{
							if (!pSymType)
							{
								CString strTypename = StrTypenameFromTypeSpecification(pStnodType);
								EmitError(pTcwork, pStnod, "'%s' unknown symbol detected", strTypename.PChz());
								return TCRET_StoppingError;
							}
							if (!pSymType->m_grfsym.FIsSet(FSYM_IsType))
							{
								CString strName = StrFullyQualifiedSymbol(pSymType);
								EmitError(pTcwork, pStnod, "%s symbol refers to instance, but was expecting type", strName.PChz());
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
					}

					if (pStdecl->m_iStnodInit >= 0)
					{
						CSTNode * pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);
						if (pStnod->m_pTin)
						{
							// just make sure the init type fits the specified one
							STypeInfo * pTinInit = pStnodInit->m_pTin;
							pTinInit = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodInit, pStnod->m_pTin);
							if (FCanImplicitCast(pTinInit, pStnod->m_pTin))
							{
								pTinInit = pStnod->m_pTin;
							}
							else
							{
								//BB - need fullyQualifiedTypenames
								EmitError(pTcwork, pStnod, "Cannot initialize variable of type %s with %s",
									pStnod->m_pTin->m_strName.PChz(),
									pTinInit->m_strName.PChz());
							}
						}
						else if (pStnodInit->m_pTin)
						{
							pStnod->m_pTin = PTinPromoteLiteralDefault(
													pTcwork,
													pTcsentTop->m_pSymtab,
													pStnodInit);

							//pStnod->m_pTin = pStnodInit->m_pTin;
						}
					}

					if (pStnod->m_pTin == nullptr)
					{
						EmitError(pTcwork, pStnod, "Unable to calculate type for declaration");
						return TCRET_StoppingError;
					}

					// find our symbol and resolve any pending unknown types

					CSTNode * pStnodIdent = pStnod->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
					if (EWC_FVERIFY(pStnodIdent && pStnodIdent->m_pStval, "Declaration without identifier"))
					{
						SSymbol * pSymIdent = pSymtab->PSymLookup(
														pStnodIdent->m_pStval->m_str,
														pStnodIdent->m_lexloc,
														pTcsentTop->m_grfsymlook);
						pStnod->m_pSym = pSymIdent;
						OnTypeComplete(pTcwork, pSymIdent);
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
				}
			}break;
			case PARK_Literal:
			{
				if (EWC_FVERIFY(pStnod->m_pTin == nullptr, "STypeInfoLiteral should be constructed during type checking"))
				{
					CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
					STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
					pSymtab->AddManagedTin(pTinlit);
					pStnod->m_pTin = pTinlit;

					if (EWC_FVERIFY(pStnod->m_pStval, "null value in literal"))
					{
						pTinlit->m_stval = *pStnod->m_pStval;
					}
				}
				
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram);
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

						TINK tinkLhs = TINK_Nil;
						if (EWC_FVERIFY(pTinLhs, "unexpected unknown type in assignment op LHS"))
						{
							bool fIsValidLhs = FIsValidLhs(pStnodLhs);
							if (!fIsValidLhs)
							{
								(void) FIsValidLhs(pStnod);
								CString strLhs = StrFromTypeInfo(pTinLhs);
								EmitError(pTcwork, pStnod, "%s does not provide an assignment operator", strLhs.PChz());
								return TCRET_StoppingError;
							}

							tinkLhs = pTinLhs->m_tink;
						}

						EWC_ASSERT(pTinLhs, "unexpected null type in assignment op RHS");

						STypeInfo * pTinRhsPromoted = PTinPromoteLiteralTightest(
														pTcwork,
														pTcsentTop->m_pSymtab,
														pStnodRhs,
														pTinLhs);
						if (FCanImplicitCast(pTinRhsPromoted, pTinLhs))
						{
							pStnod->m_pTin = pTinLhs;
						}
						else
						{
							CString strLhs = StrFromTypeInfo(pTinLhs);
							CString strRhs = StrFromTypeInfo(pTinRhs);
							EmitError( pTcwork, pStnod,
								"implicit cast from %s to %s is not allowed",
								strRhs.PChz(),
								strLhs.PChz());
						}

						JTOK jtokOp = pStnod->m_jtok;
						bool fIsArithmeticOp =	(jtokOp == JTOK_MulEqual) | (jtokOp == JTOK_DivEqual) |
												(jtokOp == JTOK_PlusEqual) | (jtokOp == JTOK_MinusEqual) |
												(jtokOp == JTOK_ModEqual);

						bool fIsBitwiseOp =		(jtokOp == JTOK_AndEqual) | (jtokOp == JTOK_OrEqual) |
												(jtokOp == JTOK_XorEqual);

						bool fSupportsArithmetic = (tinkLhs == TINK_Integer) | (tinkLhs == TINK_Float);
						bool fSupportsBitwise = tinkLhs == TINK_Integer;

						if (fIsArithmeticOp & (fSupportsArithmetic == false))
						{
							CString strLhs = StrFromTypeInfo(pTinLhs);
							EmitError(pTcwork, pStnod, "%s does not support arithmetic operations", strLhs.PChz());
							return TCRET_StoppingError;
						}

						if (fIsBitwiseOp & (fSupportsBitwise == false))
						{
							CString strLhs = StrFromTypeInfo(pTinLhs);
							EmitError(pTcwork, pStnod, "%s does not support bitwise operations", strLhs.PChz());
							return TCRET_StoppingError;
						}

					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
			case PARK_ReservedWord:
			{
				if (EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
				{
					RWORD rword = pStnod->m_pStval->m_rword;
					switch (rword)
					{
					case RWORD_If:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							if (pStnod->CStnodChild() < 2)
							{
								EmitError(pTcwork, pStnod, "encountered if statement without expected predicate,child");
								return TCRET_StoppingError;
							}

							// (if (predicate) (ifCase) (else (elseCase)))
							CSTNode * pStnodPred = pStnod->PStnodChild(0);
							STypeInfo * pTinPred = pStnodPred->m_pTin;

							auto pSymtab = pTcsentTop->m_pSymtab;
							STypeInfo * pTinBool = pSymtab->PTinBuiltin("bool");
							STypeInfo * pTinPredPromoted = PTinPromoteLiteralTightest(
															pTcwork,
															pTcsentTop->m_pSymtab,
															pStnodPred,
															pTinBool);
							if (!FCanImplicitCast(pTinPredPromoted, pTinBool))
							{
								CString strTinPred = StrFromTypeInfo(pTinPredPromoted);
								EmitError(pTcwork, pStnod, "No conversion between %s and bool", strTinPred.PChz());
							}

							pStnod->m_pTin = pTinBool;

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram);
						} break;
					case RWORD_Else:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							pStnod->m_strees = STREES_TypeChecked;
							PopTcsent(pTcfram);
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
									STypeInfo * pTinRhsPromoted = PTinPromoteLiteralTightest(
																	pTcwork,
																	pTcsentTop->m_pSymtab,
																	pStnodRhs,
																	pTinReturn);
									if (FCanImplicitCast(pTinRhsPromoted, pTinReturn))
									{
										pStnod->m_pTin = pTinReturn;
									}
									else
									{
										CString strLhs = StrFromTypeInfo(pTinReturn);
										CString strRhs = StrFromTypeInfo(pTinRhs);
										EmitError( pTcwork, pStnod,
											"implicit cast from %s to %s is not allowed by return statement",
											strRhs.PChz(),
											strLhs.PChz());
									}
								}
								else
								{
									EWC_ASSERT(false, "multiple return types not supported (yet).");
								}

								pStnod->m_strees = STREES_TypeChecked;
								PopTcsent(pTcfram);
								break;
							}
							PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
						}break;
					default:
						EmitError(pTcwork, pStnod, "unhandled reserved word '%s' in type checker", PChzFromRword(rword));
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

						CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
						if (EWC_FVERIFY((pTinLhs != nullptr) & (pTinRhs != nullptr), "unknown type in binary operation"))
						{
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
									CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
									STypeInfoLiteral * pTinlit;
									CSTValue * pStval;
									if (FComputeBinaryOpOnLiterals(
											pTcwork,
											pStnod->m_jtok,
											pSymtab,
											pStnodLhs,
											pStnodRhs,
											&pTinlit,
											&pStval))
									{
										pStnod->m_pTin = pTinlit;
										pStnod->m_pTinOperand = pTinlit;
										pStnod->m_pStval = pStval;
									}
									else
									{
										EWC_ASSERT(false, "failed to execute binary op on literals");
									}
								}
							}
							else
							{
								STypeInfo * pTinUpcastLhs = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodLhs, pTinRhs);
								STypeInfo * pTinUpcastRhs = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodRhs, pTinLhs);
								STypeInfo * pTinUpcast = PTinFindUpcast(pTcwork, pSymtab, pTinUpcastRhs, pTinUpcastLhs);
								if (!pTinUpcast)
								{
									CString strLhs = StrFromTypeInfo(pTinLhs);
									CString strRhs = StrFromTypeInfo(pTinRhs);
									EmitError(
										pTcwork,
										pStnod,
										"%s operator not defined for %s and %s",
										PChzFromJtok(pStnod->m_jtok),
										strLhs.PChz(),
										strRhs.PChz());
								}

								PARK park = pStnod->m_park;
								bool fIsLogicalOp = (park == PARK_RelationalOp) |
									(park == PARK_EqualityOp) |
									(park == PARK_LogicalAndOrOp);

								pStnod->m_pTin = (fIsLogicalOp) ? pSymtab->PTinBuiltin("bool") : pTinUpcast;
								pStnod->m_pTinOperand = pTinUpcast;
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
		case PARK_UnaryOp:
			{
				if (pTcsentTop->m_nState >= pStnod->CStnodChild())
				{
					if (EWC_FVERIFY(pStnod->CStnodChild() == 1, "expected one operand to unary operations"))
					{
						CSTNode * pStnodOperand = pStnod->PStnodChild(0);
						STypeInfo * pTinOperand = pStnodOperand->m_pTin;

						JTOK jtok = pStnod->m_jtok;
						switch (jtok)
						{
							case JTOK('&'):		// dereference
							{
								if (pTinOperand->m_tink != TINK_Pointer)
								{
									CString strOp = StrFromTypeInfo(pTinOperand);
									EmitError(pTcwork, pStnod, "Cannot dereference type %s", strOp.PChz());
									return TCRET_StoppingError;
								}
								else
								{
									STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTinOperand;
									pStnod->m_pTin = pTinptr->m_pTinPointedTo;
								}
							}break;
							case JTOK('*'):		// add reference
							{
								bool FCanTakeReference = pTinOperand->m_tink != TINK_Literal;
								if (!FCanTakeReference)
								{
									CString strOp = StrFromTypeInfo(pTinOperand);
									EmitError(pTcwork, pStnod, "Cannot take reference of type %s", strOp.PChz());
									return TCRET_StoppingError;
								}

								CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
								STypeInfoPointer * pTinptr = EWC_NEW(pSymtab->m_pAlloc, STypeInfoPointer) STypeInfoPointer();
								pTinptr->m_pTinPointedTo = pTinOperand;
								pSymtab->AddManagedTin(pTinptr);
								pStnod->m_pTin = pTinptr;
							}break;

							case JTOK('!'):
							{
								STypeInfo * pTinBool = pTcsentTop->m_pSymtab->PTinBuiltin("bool");
								if (!EWC_FVERIFY(pTinBool, "missing bool type"))
									return TCRET_StoppingError;
								if (!FCanImplicitCast(pTinOperand, pTinBool))
								{
									CString strOp = StrFromTypeInfo(pTinOperand);
									EmitError(pTcwork, pStnod, "Cannot convert type %s to bool", strOp.PChz());
								}

								pStnod->m_pTin = pTinBool;
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
								if (tinkOperand == TINK_Literal && pStnodOperand->m_pStval)
								{
									LITK litk = pStnodOperand->m_pStval->m_litty.m_litk;
									fIsInteger |= litk == LITK_Integer;
									fIsFloat |= litk == LITK_Float;
								}

								bool fIsValidPtrOp =	((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) & 
														(tinkOperand == TINK_Pointer);
								bool fIsValidFloatOp =  ((jtok == JTOK('+')) | (jtok == JTOK('-'))) & fIsFloat;
								bool fIsSupported = fIsInteger | fIsValidPtrOp | fIsValidFloatOp;

								// BB - we should be checking for negating a signed literal here, but we can't really
								//  do operations on literals until we know the resolved type
								//  (Otherwise ~1 will always resolve to a u64)

								pStnod->m_pTin = pTinOperand;
								if (!fIsSupported)
								{
									CString strOp = StrFromTypeInfo(pTinOperand);
									EmitError(pTcwork, pStnod, "invalid unary operator for type %s", strOp.PChz());
								}
								else
								{
									if (jtok == JTOK('-') && tinkOperand == TINK_Integer)
									{
										STypeInfoInteger * pTinint = (STypeInfoInteger *)pTinOperand;
										if (!pTinint->m_fIsSigned)
										{
											CString strOp = StrFromTypeInfo(pTinOperand);
											EmitError(pTcwork, pStnod, "negate operand not valid for unsigned type %s", strOp.PChz());
										}
									}
								}
							}break;
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
			}break;
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

enum LITSTOR
{
	LITSTOR_Nil = -1,
	LITSTOR_Float,
	LITSTOR_SInt,
	LITSTOR_UInt,
};

static inline LITSTOR LitstoreFromLitk(LITK litk, bool fSigned)
{
	switch (litk)
	{
	case LITK_Integer:	return (fSigned) ? LITSTOR_SInt : LITSTOR_UInt;
	case LITK_Float:	return LITSTOR_Float;
	case LITK_Bool:		return LITSTOR_UInt;
	default:			return LITSTOR_Nil;
	}
}

static inline LITK LitkFromTink(TINK tink)
{
	// BB - LITK and TINK should merge
	switch (tink)
	{
	case TINK_Integer:	return LITK_Integer;
	case TINK_Float:	return LITK_Float;
	case TINK_Bool:		return LITK_Bool;
	case TINK_String:	return LITK_String;
	default:			return LITK_Nil;
	}
}

void FlushResolvedLiterals(STypeCheckWorkspace * pTcwork, CSTNode * pStnod, STypeInfo * pTinResolve)
{
	switch (pStnod->m_park)
	{
	case PARK_Literal:
		{
			CSTValue * pStval = pStnod->m_pStval;
			if (EWC_FVERIFY(pStval && pTinResolve, "trying to flush resolved literal without type"))
			{
				LITK litkNew = LitkFromTink(pTinResolve->m_tink);
				bool fIsTinResolveSigned = (pTinResolve->m_tink != TINK_Bool);
				if (pTinResolve->m_tink == TINK_Integer)
					fIsTinResolveSigned |= ((STypeInfoInteger *)pTinResolve)->m_fIsSigned;

				SLiteralType littyOld = pStval->m_litty.m_litk;
				LITSTOR litstorNew = LitstoreFromLitk(litkNew, fIsTinResolveSigned);
				LITSTOR litstorOld = LitstoreFromLitk(littyOld.m_litk, pStval->m_tfnSigned == TFN_True);

				if (litkNew != LITK_Nil && litstorOld != litstorNew)
				{
					// we need to 'cast' literals when you init something with a different type of literal:
					// fTest : bool = 3; gTest : float = 10;

					pStval->m_litty.m_litk = litkNew;
					if (litstorOld == LITSTOR_Float && litkNew == LITK_Integer)
					{
						EmitError(pTcwork, pStnod, "Initializing integer with float literal");
					}

					switch (litstorNew)
					{
					case LITSTOR_Float:
						{
							pStval->m_g = (litstorOld == LITSTOR_UInt) ? (F64)(pStval->m_nUnsigned) : (F64)(pStval->m_nSigned);
							pStval->m_litty.m_fIsSigned = true;
						}break;
					case LITSTOR_SInt:
						{
							pStval->m_litty.m_fIsSigned = true;
							switch (litstorOld)
							{
							case LITSTOR_Float: 
								pStval->m_nSigned = (s64)pStval->m_g;
								break;
							case LITSTOR_UInt:
								if (pStval->m_nUnsigned > LLONG_MAX)
								{
									EmitError(pTcwork, pStnod, "Signed int constant too large for s64");
								}
								pStval->m_nSigned = (s64)pStval->m_nUnsigned;
								break;
							}
						}break;
					case LITSTOR_UInt:
						{
							pStval->m_litty.m_fIsSigned = false;
							switch (litstorOld)
							{
							case LITSTOR_Float: 
								pStval->m_nUnsigned = (u64)pStval->m_g;
								break;
							case LITSTOR_SInt:
								if (pStval->m_nSigned < 0)
								{
									EmitError(pTcwork, pStnod, "Unsigned int constant with negative value");
								}
								pStval->m_nUnsigned = (u64)pStval->m_nSigned;
								break;
							}
						}
					}
				}

				switch (pStval->m_litty.m_litk)
				{
				case LITK_Integer:
					{
						if(!EWC_FVERIFY(pTinResolve && pTinResolve->m_tink == TINK_Integer, "expected integer type"))
							return;

						STypeInfoInteger * pTinint = (STypeInfoInteger *)pTinResolve;
						pStval->m_litty.m_cBit = pTinint->m_cBit;
					}break;
				case LITK_Float:
					{
						if(!EWC_FVERIFY(pTinResolve && pTinResolve->m_tink == TINK_Float, "expected float type"))
							return;

						STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTinResolve;
						pStval->m_litty.m_cBit = pTinfloat->m_cBit;
					}break;
				}
			}
		}break;
	case PARK_Identifier:
		return;
	case PARK_StructDefinition:
	case PARK_ParameterList:
	case PARK_List:
		{
			for (int iStnod = 0; iStnod < pStnod->CStnodChild(); ++iStnod)
			{
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(iStnod), nullptr);
			}
		}break;
	case PARK_ProcedureDefinition:
		{
			CSTProcedure * pStproc = pStnod->m_pStproc;
			if (!EWC_FVERIFY(pStproc, "missing procedure parse data"))
				return;

			if (pStproc->m_iStnodParameterList >= 0)
			{
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(pStproc->m_iStnodParameterList), nullptr);
			}

			if (pStproc->m_iStnodBody >= 0)
			{
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(pStproc->m_iStnodBody), nullptr);
			}
		}break;
	case PARK_ProcedureCall:
		{
			if (!EWC_FVERIFY(pStnod->m_pSym && pStnod->m_pSym->m_pStnodDefinition, "Unknown procedure in call"))
				return;

			CSTNode * pStnodDefinition = pStnod->m_pSym->m_pStnodDefinition;
			STypeInfoProcedure * pTinproc = (STypeInfoProcedure *)pStnodDefinition->m_pTin;
			if (!EWC_FVERIFY(pTinproc && pTinproc->m_tink == TINK_Procedure, "expected procedure type"))
				return;

			for (int iStnodArg = 1; iStnodArg < pStnod->CStnodChild(); ++iStnodArg)
			{
				CSTNode * pStnodArg = pStnod->PStnodChild(iStnodArg);
				STypeInfo * pTinParam = pTinproc->m_arypTinParams[iStnodArg - 1];

				FlushResolvedLiterals(pTcwork, pStnodArg, pTinParam);
			}
		}break;
	case PARK_EnumConstant:
	case PARK_EnumDefinition:
		{
			EWC_ASSERT(false, "WIP Enum typeCheck not finished");
			return;
		}break;
	case PARK_Decl:
		{
			CSTDecl * pStdecl = pStnod->m_pStdecl;
			if (!EWC_FVERIFY(pStdecl, "missing decl parse data"))
				return;
			if (pStdecl->m_iStnodInit >= 0)
			{
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(pStdecl->m_iStnodInit), pStnod->m_pTin);
			}
		}break;
	case PARK_AssignmentOp:
		{
			if (EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to assignment op"))
			{
				CSTNode * pStnodRhs = pStnod->PStnodChild(1);
				FlushResolvedLiterals(pTcwork, pStnodRhs, pStnod->m_pTin);
			}
		}break;
	case PARK_ReservedWord:
		{
			if (EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
			{
				RWORD rword = pStnod->m_pStval->m_rword;
				switch (rword)
				{
				case RWORD_If:
					{
						if (pStnod->CStnodChild() < 2)
						{
							EmitError(pTcwork, pStnod, "encountered if statement without expected predicate,child,[else]");
							return;
						}

						STypeInfo * pTinBool = pStnod->m_pTin;
						if (EWC_FVERIFY(pTinBool->m_tink == TINK_Bool, "expected bool type for predicate"))
						{
							FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(0), pTinBool);
						}

						for (int iStnodArg = 1; iStnodArg < pStnod->CStnodChild(); ++iStnodArg)
						{
							FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(iStnodArg), nullptr);
						}
					} break;
				case RWORD_Else:
					{
						for (int iStnod = 0; iStnod < pStnod->CStnodChild(); ++iStnod)
						{
							FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(iStnod), nullptr);
						}
					} break;
				case RWORD_Return:
					{
						for (int iStnod = 0; iStnod < pStnod->CStnodChild(); ++iStnod)
						{
							FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(iStnod), pStnod->m_pTin);
						}
					}break;
				default:
					EmitError(pTcwork, pStnod, "unhandled reserved word '%s' in FlushResolvedLiterals", PChzFromRword(rword));
					return;
				}
			}
		}break;
	case PARK_AdditiveOp:
	case PARK_MultiplicativeOp:
	case PARK_ShiftOp:
	case PARK_BitwiseAndOrOp:
		{
			// binary ops where the return type is the same as the operands
			if (EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to binary ops"))
			{
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(0), pTinResolve);
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(1), pTinResolve);
			}
		}break;
	case PARK_RelationalOp:
	case PARK_EqualityOp:
	case PARK_LogicalAndOrOp:
		{
			// binary ops where the return type is bool
			if (EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected two operands to binary ops"))
			{
				EWC_ASSERT(pStnod->m_pTinOperand, "expected operand type info");
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(0), pStnod->m_pTinOperand);
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(1), pStnod->m_pTinOperand);
			}
		}break;
	case PARK_UnaryOp:
		{
			if (EWC_FVERIFY(pStnod->CStnodChild() == 1, "expected one operand to unary operations"))
			{
				FlushResolvedLiterals(pTcwork, pStnod->PStnodChild(0), pTinResolve);
			}
		}break;
	}
}

void PerformFlushResolvedLiteralsPass(
	STypeCheckWorkspace * pTcwork,
	CAry<CWorkspace::SEntry> * paryEntry)
{
	// Type checking has done a bottom up pass resolving literals (as tightly as possible, or using default rules)
	// Now we need to push those resolved literal types down the tree. This could be avoided (maybe?) once the
	// code to collapse literal expressions also collapses the AST nodes into a single constant.

	// Note: this only sets CSTNode::m_pStval->m_litty's type info, not the STypeInfoLiteral::m_litty 
	//  as they should be unique-ified literal values (?) (yuck!)

	CWorkspace::SEntry * pEntryMac = paryEntry->PMac();
	for (CWorkspace::SEntry * pEntry = paryEntry->A(); pEntry != pEntryMac; ++pEntry)
	{
		FlushResolvedLiterals(pTcwork, pEntry->m_pStnod, nullptr);
	}
}

void PerformTypeCheck(
	CAlloc * pAlloc,
	CSymbolTable * pSymtabTop,
	CAry<CWorkspace::SEntry> * paryEntry,
	CAry<int> * paryiEntryChecked)
{
	STypeCheckWorkspace * pTcwork = EWC_NEW(pAlloc, STypeCheckWorkspace) STypeCheckWorkspace(pAlloc, (s32)paryEntry->C());

	CWorkspace::SEntry * pEntryMax = paryEntry->PMac();
	int ipTcfram = 0;
	for (CWorkspace::SEntry * pEntry = paryEntry->A(); pEntry != pEntryMax; ++pEntry, ++ipTcfram)
	{
		STypeCheckFrame * pTcfram = pTcwork->m_aryTcfram.AppendNew();
		pTcfram->m_ipTcframQueue = ipTcfram;

		pTcfram->m_aryTcsent.SetAlloc(pAlloc);
		STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
		pTcsent->m_nState = 0;
		pTcsent->m_pStnod = pEntry->m_pStnod;
		pTcsent->m_pSymtab = pEntry->m_pSymtab;
		pTcsent->m_pStnodProcedure = nullptr;
		pTcsent->m_grfsymlook = FSYMLOOK_Default;

		pTcwork->m_arypTcframPending.Append(pTcfram);
	}
	
	while (pTcwork->m_arypTcframPending.C())
	{
		STypeCheckFrame * pTcfram = pTcwork->m_arypTcframPending[0];
		TCRET tcret = TypeCheckSubtree(pTcwork, pTcfram);

		if (tcret == TCRET_StoppingError || tcret == TCRET_Complete)
		{
			RelocateTcfram(pTcfram, &pTcwork->m_arypTcframPending, nullptr);

			if (tcret == TCRET_Complete)
			{
				size_t iEntry = pTcwork->m_aryTcfram.IFromP(pTcfram);
				paryiEntryChecked->Append(S32Coerce(iEntry));
			}
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

			EmitError(pTcwork, pTcsent->m_pStnod, "Unresolved type (%s) reference found here", (*ppSym)->m_strName.PChz());
		}
	}

	PerformFlushResolvedLiteralsPass(pTcwork, paryEntry);

	pAlloc->EWC_DELETE(pTcwork);
}

void AssertTestTypeCheck(
	CWorkspace * pWork,
	const char * pChzIn,
	const char * pChzOut)
{
	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pChzIn);

	EWC_ASSERT(pWork->m_pParctx->m_cError == 0, "parse errors detected");
	pWork->m_pParctx->m_cError = 0;

	ParseGlobalScope(pWork, &jlex, true);
	EWC_ASSERT(pWork->m_aryEntry.C() > 0);

	EndParse(pWork, &jlex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type|FDBGSTR_LiteralSize);

	EWC_ASSERT(FAreSame(aCh, pChzOut), "type check debug string doesn't match expected value");

	EndWorkspace(pWork);
}

void TestTypeCheck()
{
	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);

	const char * pChzIn;
	const char * pChzOut;
	pChzIn =	"{ i:=5; foo:=i; g:=g_g; } g_g:=2.2;";
	pChzOut = "({} (int @i Literal:Int64) (int @foo int) (float @g float)) (float @g_g Literal:Float32)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"i:= (5-4) + (5-6); g:=2.2 + (3.3*10);";
	pChzOut = "(int @i (Literal (Literal Literal:Int64 Literal:Int64) (Literal Literal:Int64 Literal:Int64))) "
				"(float @g (Literal Literal:Float32 (Literal Literal:Float32 Literal:Float32)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "n:=2; n = 100-n;";
	pChzOut ="(int @n Literal:Int64) (int int (int Literal:Int64 int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; bar:s16=i; g:=g_g; } g_g : float64 = 2.2;";
	pChzOut = "({} (s8 @i s8 Literal:Int8) (s8 @foo s8) (s16 @bar s16 s8) (float64 @g float64)) (float64 @g_g float64 Literal:Float64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=true; foo:=i; g:=g_g; } g_g : bool = false;";
	pChzOut = "({} (bool @i Literal:Bool) (bool @foo bool) (bool @g bool)) (bool @g_g bool Literal:Bool)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "ParamFunc :: (nA : s32, g : float) { foo := nA; bah := g; }";
	pChzOut		= "(ParamFunc() @ParamFunc (params (s32 @nA s32) (float @g float)) void ({} (s32 @foo s32) (float @bah float) (void)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=\"hello\"; foo:=i; g:=g_g; } g_g : string = \"huzzah\";";
	pChzOut = "({} (string @i Literal:String) (string @foo string) (string @g string)) (string @g_g string Literal:String)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ pN:* s8; pNInfer:=pN; pNTest:*s8=null;}";
	pChzOut = "({} (*s8 @pN (*s8 s8)) (*s8 @pNInfer *s8) (*s8 @pNTest (*s8 s8) Literal:Null))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ pN:** s8; pNInfer:=pN; pNTest:**s8=null;}";
	pChzOut = "({} (**s8 @pN (**s8 (*s8 s8))) (**s8 @pNInfer **s8) (**s8 @pNTest (**s8 (*s8 s8)) Literal:Null))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; foo=6; i = foo; }";
	pChzOut = "({} (s8 @i s8 Literal:Int8) (s8 @foo s8) (s8 s8 Literal:Int8) (s8 s8 s8))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; fBool:bool = foo==i; fBool = i<2; }";
	pChzOut = "({} (s8 @i s8 Literal:Int8) (s8 @foo s8) (bool @fBool bool (bool s8 s8)) (bool bool (bool s8 Literal:Int8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8; foo:s32; foo=i+foo; foo=foo<<i; }";
	pChzOut = "({} (s8 @i s8) (s32 @foo s32) (s32 s32 (s32 s8 s32)) (s32 s32 (s32 s32 s8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s8; pN:=*n; n2:=&pN; }";
	pChzOut = "({} (s8 @n s8) (*s8 @pN (*s8 s8)) (s8 @n2 (s8 *s8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; pN:*s8; fN := !pN; ++n; --n;}";
	pChzOut = "({} (s64 @n s64) (*s8 @pN (*s8 s8)) (bool @fN (bool *s8)) (s64 s64) (s64 s64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; if n == 2 n = 5; else n = 6;}";
	pChzOut = "({} (s64 @n s64) (bool (bool s64 Literal:Int64) (s64 s64 Literal:Int64) (??? (s64 s64 Literal:Int64))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "{ nA : s8; nB : s8; nC := (nA < nB) == (nB >= nA); }";
	pChzOut = "({} (s8 @nA s8) (s8 @nB s8) (bool @nC (bool (bool s8 s8) (bool s8 s8))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; if n n = 5; else n = 6;}";
	pChzOut = "({} (s64 @n s64) (bool s64 (s64 s64 Literal:Int64) (??? (s64 s64 Literal:Int64))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "AddNums :: (a : int, b := 1) -> int { return a + b;} n := AddNums(2,3);";
	pChzOut		= "(AddNums() @AddNums (Params (int @a int) (int @b Literal:Int64)) int ({} (int (int int int))))"
					" (int @n (int @AddNums Literal:Int64 Literal:Int64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "NoReturn :: (a : int) { n := a;} NoReturn(2);";
	pChzOut		= "(NoReturn() @NoReturn (Params (int @a int)) void ({} (int @n int) (void))) (void @NoReturn Literal:Int64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "NoReturn :: (a : int) { n := a; return; } NoReturn(2);";
	pChzOut		= "(NoReturn() @NoReturn (Params (int @a int)) void ({} (int @n int) (void))) (void @NoReturn Literal:Int64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);
	
	pChzIn		= " { n:int=2; Foo :: () -> int { n2:=n; g:=Bar();}    Bar :: () -> float { n:=Foo(); } }";
	pChzOut		=	"(Foo() @Foo int ({} (int @n2 int) (float @g (float @Bar))))"
					" (Bar() @Bar float ({} (int @n (int @Foo))))"
					" ({} (int @n int Literal:Int64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "{ ovr:=2; { nNest:= ovr; ovr:float=2.2; g:=ovr; } n:=ovr; }"; 
	pChzOut		= "({} (int @ovr Literal:Int64) ({} (int @nNest int) (float @ovr float Literal:Float32) (float @g float)) (int @n int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);
	
	pChzIn		= " { ovr:=2; Foo :: () { nNest:= ovr; ovr:float=2.2; g:=ovr; } n:=ovr; }"; 
	pChzOut		=	"(Foo() @Foo void ({} (int @nNest int) (float @ovr float Literal:Float32) (float @g float) (void)))"
					" ({} (int @ovr Literal:Int64) (int @n int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	StaticShutdownStrings(&allocString);
}