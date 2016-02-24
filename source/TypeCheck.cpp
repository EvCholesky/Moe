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
#include "JaiParse.h"
#include "JaiTypes.h"
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
			case PARK_ReferenceDecl:
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
			if (pStval->m_nUnsigned >= LLONG_MAX)
			{
				EmitError(pTcwork, pStnod, "Literal is too large for implicit signed int cast.");
			}
			return (s64)pStval->m_nUnsigned;
		}
	case STVALK_SignedInt:
		return pStval->m_nSigned;
	case STVALK_Float:
		return (s64)pStval->m_g;
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
			switch (pStval->m_rword)
			{
			case RWORD_False:	return BintFromUint(0, false);
			case RWORD_True:	return BintFromUint(1, false);
			default:
				EWC_ASSERT(false, "Can't create Bint from non reserved word");
				return SBigInt();
			}
		}
	default:
		EWC_ASSERT(false, "Can't create Bint from non integer value");
		return SBigInt();
	}
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

	bool fOperandIsNumber = (littyOperand.m_litk == LITK_Float) | (littyOperand.m_litk == LITK_Integer);
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
	else // both LITK_Integer
	{
		EWC_ASSERT(littyOperand.m_cBit == -1, "expected unsized literal here");

		SBigInt bintOperand(BintFromStval(pStvalOperand));

		bool f;
		switch (jtokOperator)
		{
		case JTOK('-'):         bintOperand.m_fIsNegative = !bintOperand.m_fIsNegative; break;
		// we're not currently handling ~ Doing this properly requires a bunch of work on sign extending unsized literals
		//case JTOK('~'):       bintOperand = ~bintOperand; break;
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

			EWC_ASSERT(pTinlitOperand->m_litty.m_litk == LITK_Integer);
			*ppTinReturn = pTinlitOperand;
			*ppTinOperand = pTinlitOperand;
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

	bool fLhsIsNumber = (littyLhs.m_litk == LITK_Float) | (littyLhs.m_litk == LITK_Integer) | (littyLhs.m_litk == LITK_Bool);
	bool fRhsIsNumber = (littyRhs.m_litk == LITK_Float) | (littyRhs.m_litk == LITK_Integer) | (littyRhs.m_litk == LITK_Bool);
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

			EWC_ASSERT(pTinlitLhs->m_litty.m_litk == LITK_Float);
			*ppTinReturn = pTinlitLhs;
			*ppTinOperand = pTinlitLhs;
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

			EWC_ASSERT(pTinlitLhs->m_litty.m_litk == LITK_Integer || pTinlitLhs->m_litty.m_litk == LITK_Bool);
			*ppTinReturn = pTinlitLhs;
			*ppTinOperand = pTinlitLhs;
		}
		return true;
	}
}

void FinalizeLiteralType(CSymbolTable * pSymtab, STypeInfo * pTin, CSTNode * pStnodLit)
{
	if (!pStnodLit->m_pTin || pStnodLit->m_pTin->m_tink != TINK_Literal)
		return;

	// we've found the place the literal will become 'typed' - flush that type back down into the literal

	EWC_ASSERT(pTin->m_tink != TINK_Literal, "cannot finalize literal with literal");
	switch (pTin->m_tink)
	{
	case TINK_Integer:
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Integer, pTinint->m_cBit, pTinint->m_fIsSigned);
		}break;
    case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTin;
			pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Float, pTinfloat->m_cBit, true);
		}break;
	case TINK_Bool:		pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_Bool);	break;
    case TINK_String:	pStnodLit->m_pTin = pSymtab->PTinlitFromLitk(LITK_String);	break;
    case TINK_Pointer:
		{
			LITK litkPrev = LITK_Nil;
			STypeInfoLiteral * pTinlitPrev = (STypeInfoLiteral *)pStnodLit->m_pTin;
			STypeInfoLiteral * pTinlit = nullptr;
			
			switch (pTinlitPrev->m_litty.m_litk)
			{
			case LITK_String:	pTinlit = pSymtab->PTinlitFromLitk(LITK_String);	break;
			case LITK_Null:		pTinlit = pSymtab->PTinlitFromLitk(LITK_Null);	break;
			case LITK_Integer:	
				{
					CSTValue * pStval = pStnodLit->m_pStval;
					pTinlit = pSymtab->PTinlitFromLitk(LITK_Integer, 64, pStval->m_stvalk == STVALK_SignedInt);
				} break;
			default: EWC_ASSERT(false, "unexpected literal type");
			}
			if (pTinlit)
			{
				pTinlit->m_pTinptrNull = (STypeInfoPointer*)pTin;
				pStnodLit->m_pTin = pTinlit;
			}
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
				return pSymtab->PTinBuiltin((pTinint->m_fIsSigned) ? "int" : "uint");
			}
			return pTinIn;
		}
	case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTinIn;
			if (pTinfloat->m_cBit < 64)
			{
				return pSymtab->PTinBuiltin("float64");
			}
			return pTinIn;
		}
	default: return pTinIn;
	}
}

STypeInfo * PTinPromoteLiteralDefault(STypeCheckWorkspace * pTcwork, CSymbolTable * pSymtab, CSTNode * pStnodLit)
{
	const STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	if (!pTinlit || pTinlit->m_tink != TINK_Literal)
		return pStnodLit->m_pTin;

	const CSTValue * pStval = pStnodLit->m_pStval;
	if (!EWC_FVERIFY(pStval, "literal without value"))
		return nullptr;

	const SLiteralType & litty = pTinlit->m_litty;
	switch (litty.m_litk)
	{
	case LITK_Integer:
		{
			bool fIsSigned = litty.m_fIsSigned;
			if (fIsSigned == false)
			{
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
		return pSymtab->PTinptrGetReference(pTinU8);
	}
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:
		{
			EmitError(pTcwork, pStnodLit, "Cannot infer type for null");
		}break;
	case LITK_Nil: 
		EWC_ASSERT(false, "Cannot infer type for LITK_Nil");
	}
	return nullptr;
}

inline STypeInfo * PTinPromoteLiteralTightest(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	CSTNode * pStnodLit,	
	STypeInfo * pTinDest)
{
	const STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodLit->m_pTin;
	if (!pTinlit || pTinlit->m_tink != TINK_Literal)
		return pStnodLit->m_pTin;

	const SLiteralType & litty = pTinlit->m_litty;

	switch (litty.m_litk)
	{
	case LITK_Integer:
		{
			// NOTE: We're casting the value to fit the type info here, not letting the value determine the type.

			const CSTValue * pStval = pStnodLit->m_pStval;
			if (!EWC_FVERIFY(pStval, "literal without value"))
				return nullptr;

			bool fDestIsSigned = pTinDest->m_tink != TINK_Integer || ((STypeInfoInteger*)pTinDest)->m_fIsSigned;
			bool fIsValNegative = pStval->m_stvalk == STVALK_SignedInt && pStval->m_nSigned < 0;

			if (fDestIsSigned == false && fIsValNegative == false)
			{
				s64 nUnsigned = NUnsignedLiteralCast(pTcwork, pStnodLit, pStval);
				if (nUnsigned < UCHAR_MAX)	return pSymtab->PTinBuiltin("u8");
				if (nUnsigned < USHRT_MAX)	return pSymtab->PTinBuiltin("u16");
				if (nUnsigned < UINT_MAX)	return pSymtab->PTinBuiltin("u32");
				return pSymtab->PTinBuiltin("u64");
			}
			
			s64 nSigned = NSignedLiteralCast(pTcwork, pStnodLit, pStval);
			if ((nSigned < SCHAR_MAX) & (nSigned > SCHAR_MIN))	return pSymtab->PTinBuiltin("s8");
			if ((nSigned < SHRT_MAX) & (nSigned > SHRT_MIN))	return pSymtab->PTinBuiltin("s16");
			if ((nSigned < INT_MAX) & (nSigned > INT_MIN))		return pSymtab->PTinBuiltin("s32");
			return pSymtab->PTinBuiltin("s64");
		}
	case LITK_Float:	return pSymtab->PTinBuiltin("float");
	case LITK_Char:		return pSymtab->PTinBuiltin("char");
	case LITK_String:
	{
		// right now string literals just promote to *u8, but will eventually promote to string
		auto pTinU8 = pSymtab->PTinBuiltin("u8");
		return pSymtab->PTinptrGetReference(pTinU8);
	}
	case LITK_Bool:		return pSymtab->PTinBuiltin("bool");
	case LITK_Null:		
		{
			if (pTinDest && pTinDest->m_tink == TINK_Pointer)
				return pTinDest;
			EmitError(pTcwork, pStnodLit, "Trying to initialize non pointer type with null value");
		}
	case LITK_Nil: 
		EWC_ASSERT(false, "Cannot infer type for LITK_Nil");
	}
	return nullptr;
}

inline bool FTypesAreSame(STypeInfo * pTinLhs, STypeInfo * pTinRhs)
{
	if (pTinLhs == pTinRhs)
		return true;

	if (pTinLhs->m_tink != pTinRhs->m_tink)
		return false;
	
	switch(pTinLhs->m_tink)
	{
	case TINK_Pointer:	return FTypesAreSame(
								((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo, 
								((STypeInfoPointer *)pTinRhs)->m_pTinPointedTo);
	case TINK_Array:	return FTypesAreSame(((STypeInfoArray *)pTinLhs)->m_pTin, ((STypeInfoArray *)pTinRhs)->m_pTin);
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

STypeInfo * PTinOperandFromPark(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	PARK parkOperator,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs)
{
	if (pTinLhs->m_tink == pTinRhs->m_tink)
	{
		switch(pTinLhs->m_tink)
		{
		case TINK_Float:
			{
				STypeInfoFloat * pTinfloatA = (STypeInfoFloat *)pTinLhs;
				STypeInfoFloat * pTinfloatB = (STypeInfoFloat *)pTinRhs;
				return (pTinfloatA->m_cBit >= pTinfloatB->m_cBit) ? pTinLhs : pTinRhs;
			}
		case TINK_Integer:
			{
				STypeInfoInteger * pTinintA = (STypeInfoInteger *)pTinLhs;
				STypeInfoInteger * pTinintB = (STypeInfoInteger *)pTinRhs;

				if (pTinintA->m_fIsSigned != pTinintB->m_fIsSigned)
					return nullptr;
			
				return (pTinintA->m_cBit >= pTinintB->m_cBit) ? pTinLhs : pTinRhs;
			}
		}

		if (FTypesAreSame(pTinLhs, pTinRhs))
			return pTinLhs;
	}

	switch(pTinLhs->m_tink)
	{
	case TINK_Array:
	case TINK_Pointer:
		{
			switch (parkOperator)
			{
				case PARK_EqualityOp:
				{
					auto pTinElemLhs = PTinElement(pTinLhs);
					auto pTinElemRhs = PTinElement(pTinRhs);
					if (FTypesAreSame(pTinElemLhs, pTinElemRhs))
					{
						if (pTinLhs->m_tink == TINK_Pointer)
							return pTinLhs;
						if (pTinRhs->m_tink == TINK_Pointer)
							return pTinRhs;

						auto pTinary = PTinDerivedCast<STypeInfoArray *>(pTinLhs);
						auto pTinptr = EWC_NEW(pSymtab->m_pAlloc, STypeInfoPointer) STypeInfoPointer();
						pSymtab->AddManagedTin(pTinptr);
						pTinptr->m_pTinPointedTo = pTinary->m_pTin;
						return pTinptr;
					}
				} break;
				case PARK_AdditiveOp:
				{
					if (pTinRhs->m_tink != TINK_Integer)
						return nullptr;
					if (pTinLhs->m_tink == TINK_Pointer)
						return pTinLhs;

					auto pTinary = PTinDerivedCast<STypeInfoArray *>(pTinLhs);
					auto pTinptr = EWC_NEW(pSymtab->m_pAlloc, STypeInfoPointer) STypeInfoPointer();
					pSymtab->AddManagedTin(pTinptr);
					pTinptr->m_pTinPointedTo = pTinary->m_pTin;
					return pTinptr;
				}
			}
		}
	}

	return nullptr;
}

inline bool FCanImplicitCast(STypeInfo * pTinSrc, STypeInfo * pTinDst)
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
		case TINK_Pointer:
			{
				STypeInfoPointer * pTinptrSrc = (STypeInfoPointer *)pTinSrc;
				STypeInfoPointer * pTinptrDst = (STypeInfoPointer *)pTinDst;

				return FTypesAreSame(pTinptrSrc->m_pTinPointedTo, pTinptrDst->m_pTinPointedTo);	
			} break;
		case TINK_Enum: 
			return FTypesAreSame(pTinSrc, pTinDst);
		default: return false;
		}
	}

	if ((pTinSrc->m_tink == TINK_Array) & (pTinDst->m_tink == TINK_Pointer))
	{
		auto pTinarySrc = (STypeInfoArray *)pTinSrc;
		auto pTinptrDst = (STypeInfoPointer *)pTinDst;
		return FTypesAreSame(pTinarySrc->m_pTin, pTinptrDst->m_pTinPointedTo);	
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

	STypeInfo * pTinReturn = nullptr;
	STypeInfo ** ppTinCur = &pTinReturn;
	bool fAllowForwardDecl = false;

	// loop and find the concrete target type
	STypeInfo * pTinFinal = nullptr;
	CSTNode * pStnodIt = pStnod;

	EWC_ASSERT(pStnodIt->m_strees == STREES_TypeChecked, "Type specification should be type checked first, (for literal op eval)");

	while (pStnodIt)
	{
		switch(pStnodIt->m_park)
		{
		case PARK_Identifier:
			{
				if (!EWC_FVERIFY(pStnodIt->m_pStval, "identifier without value string detected"))
					break;

				pTinFinal = pSymtab->PTinLookup(pStnodIt->m_pStval->m_str, pStnodIt->m_lexloc, grfsymlook, ppSymType);

				if ((pTinFinal == nullptr) & fAllowForwardDecl)
				{
					pTinFinal = pSymtab->PTinfwdLookup(pStnodIt->m_pStval->m_str, grfsymlook);

					// NOTE: we're not setting pStnodIt->m_pTin to point at the forward decl because we won't know
					//  to update the pointer
				}
				else
				{
					pStnodIt->m_pTin = pTinFinal;
				}
				pStnodIt = nullptr;
			} break;
		case PARK_ArrayDecl:
			{
				if (pStnodIt->CStnodChild() != 2)
				{
					EmitError(pTcwork, pStnodIt, "Encounted type spec with no array size specified");
					*pFIsValidTypeSpec = false;
					return nullptr;
				}
				pStnodIt = pStnodIt->PStnodChild(1);
			} break;
		case PARK_ReferenceDecl:
			{
				fAllowForwardDecl |= true;
				EWC_ASSERT(pStnodIt->CStnodChild() == 1);
				pStnodIt = pStnodIt->PStnodChild(0);
			} break;
		default: EWC_ASSERT(false, "unexpected parse node %s in PTinFromTypeSpecification", PChzFromPark(pStnod->m_park));
			break;
		}
	}

	if (!pTinFinal)
		return nullptr;

	// build the fully qualified type info
	pStnodIt = pStnod;
	STypeInfo * pTinPrev = nullptr;
	while (pStnodIt)
	{
		if (pStnodIt->m_park == PARK_ReferenceDecl)
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
			STypeInfoArray * pTinary = EWC_NEW(pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
			pSymtab->AddManagedTin(pTinary);
			pStnodIt->m_pTin = pTinary;

			*ppTinCur = pTinary;
			ppTinCur = &pTinary->m_pTin;

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
				STypeInfo * pTinCount = pSymtab->PTinBuiltin("u64");
				STypeInfo * pTinPromoted = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodDim, pTinCount);
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
			pTinary->m_aryk = ARYK_Static;

			pStnodIt = pStnodIt->PStnodChild(1);
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

STypeInfo * PTinReturnFromStnodProcedure(CSTNode * pStnod)
{
	if (!EWC_FVERIFY(pStnod->m_park == PARK_ProcedureDefinition && pStnod->m_pStproc, "Bad procedure node"))
		return nullptr;
	CSTProcedure * pStproc = pStnod->m_pStproc;
	if (pStproc->m_iStnodReturnType < 0)
		return nullptr;
	return pStnod->PStnodChild(pStproc->m_iStnodReturnType)->m_pTin;
}

bool FDoesOperatorReturnBool(PARK park)
{
	// return if operator returns a bool (rather than the operand type)
	return  (park == PARK_RelationalOp) | (park == PARK_EqualityOp) | (park == PARK_LogicalAndOrOp);
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
							CSTNode * pStnodParamList = pStnod->PStnodChild(pStproc->m_iStnodParameterList);

							PushTcsent(pTcfram, pStnodParamList);
							STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
							pTcsentPushed->m_pSymtab = pStnodParamList->m_pSymtab;
						}
					}break;
				case 1:
					{	// type check the return type
						if (pStproc->m_iStnodReturnType >= 0)
						{
							CSTNode * pStnodReturn = pStnod->PStnodChild(pStproc->m_iStnodReturnType);

							PushTcsent(pTcfram, pStnodReturn);
							STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
							pTcsentPushed->m_pSymtab = pStnodReturn->m_pSymtab;
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
				case 3:
					{
						SSymbol * pSymProc = nullptr;
						CString strProcName;
						if (pStproc->m_iStnodProcName >= 0)
						{
							CSTNode * pStnodIdent = pStnod->PStnodChild(pStproc->m_iStnodProcName);
							strProcName = StrFromIdentifier(pStnodIdent);
							if (!strProcName.FIsEmpty())
							{
								pSymProc = pTcsentTop->m_pSymtab->PSymLookup(
																	strProcName,
																	pStnodIdent->m_lexloc,
																	pTcsentTop->m_grfsymlook);
								EWC_ASSERT(pSymProc && pSymProc->m_pStnodDefinition == pStnod, "symbol lookup failed");
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
						pSymProc = pSymtab->PSymLookup(strProcName, pStnodIdent->m_lexloc, pTcsentTop->m_grfsymlook);
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

					// the first argument is index 1, (the procedure's identifier is element zero)
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
								continue;

							pTinCall = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodArg, pTinParam);
						}
						else
						{
							if (!pTinproc->m_fHasVarArgs)
							{
								EmitError(pTcwork, pStnod, "expected %d arguments but encountered %d",
									pTinproc->m_arypTinParams.C(),
									pStnod->CStnodChild() - iStnodArgMin);
								break;
							}

							pTinCall = PTinPromoteLiteralDefault(pTcwork, pSymtab, pStnodArg);
							pTinParam = PTinPromoteVarArg(pTcwork, pSymtab, pTinCall);
						}

						if (!FCanImplicitCast(pTinCall, pTinParam))
						{
							//BB - need fullyQualifiedTypenames
							CString strTinCall = StrFromTypeInfo(pTinCall);
							CString strTinParam = StrFromTypeInfo(pTinParam);
							EmitError(pTcwork, pStnod, "no implicit conversion from  type %s to %s",
								strTinCall.PChz(),
								strTinParam.PChz());
						}

						// if we have a literal, just expect the finalized type to be correct, otherwise
						//  set the implicit cast type on the argument node
						if (pStnodArg->m_pTin->m_tink == TINK_Literal)
						{
							FinalizeLiteralType(pSymtab, pTinParam, pStnodArg);
							pStnodArg->m_pTinOperand = pStnodArg->m_pTin;
						}
						else
						{
							// BB - this should probably be replaced with an explicit cast AST node
							pStnodArg->m_pTinOperand = pStnodArg->m_pTin;
							pStnodArg->m_pTin = pTinParam;
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
					CSTNode * pStnodIdent = pStnod->PStnodChild(0);
					strStructName = StrFromIdentifier(pStnodIdent);
					if (!strStructName.FIsEmpty())
					{
						pSymStruct = pTcsentTop->m_pSymtab->PSymLookup(
																strStructName,
																pStnodIdent->m_lexloc,
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
				if (pTcsentTop->m_tcctx == TCCTX_Normal)
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
				}

				PopTcsent(pTcfram);
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
			case PARK_Nop:
			case PARK_VariadicArg:
			{
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram);
				break;
			}
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
				else if (pTcsentTop->m_nState == 1) // type check type specification (for literal op eval)
				{
					if (pStdecl->m_iStnodType >= 0)
					{
						CSTNode * pStnodReturn = pStnod->PStnodChild(pStdecl->m_iStnodType);

						PushTcsent(pTcfram, pStnodReturn);
						STypeCheckStackEntry * pTcsentPushed = paryTcsent->PLast();
						pTcsentPushed->m_tcctx = TCCTX_TypeSpecification;
					}
					++pTcsentTop->m_nState;
				}
				else if (pTcsentTop->m_nState == 2) // resolve actual type
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
								FinalizeLiteralType(pSymtab, pStnod->m_pTin, pStnodInit);
								pTinInit = pStnod->m_pTin;
							}
							else
							{
								CString strLhs = StrFromTypeInfo(pStnod->m_pTin);
								CString strInit = StrFromTypeInfo(pTinInit);
								EmitError(pTcwork, pStnod, "Cannot initialize variable of type %s with %s",
									strLhs.PChz(),
									strInit.PChz());
							}
						}
						else if (pStnodInit->m_pTin)
						{
							pStnod->m_pTin = PTinPromoteLiteralDefault(
													pTcwork,
													pTcsentTop->m_pSymtab,
													pStnodInit);

							EWC_ASSERT(pStnod->m_pTin);
							FinalizeLiteralType(pTcsentTop->m_pSymtab, pStnod->m_pTin, pStnodInit);
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
						if (!pSymIdent || pSymIdent->m_pStnodDefinition != pStnod)
						{
							SSymbol * pSymIdentTest = pSymtab->PSymLookup(
															pStnodIdent->m_pStval->m_str,
															pStnodIdent->m_lexloc,
															pTcsentTop->m_grfsymlook);
						}

						EWC_ASSERT(pSymIdent && pSymIdent->m_pStnodDefinition == pStnod, "symbol lookup failed");
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
						pTinlit->m_litty.m_litk = pStnod->m_pStval->m_litkLex;
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
							FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinLhs, pStnodRhs);
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
			} break;
			case PARK_ArrayElement:
			{
				int cStnodChild = pStnod->CStnodChild();
				if (!EWC_FVERIFY(cStnodChild == 2, "expected 2 children (array, index) for array element AST, found %d",  cStnodChild))
					return TCRET_StoppingError;

				if (pTcsentTop->m_nState < cStnodChild)
				{
					PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState));
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
					EmitError(pTcwork, pStnod, "%s cannot be indexed as an array", strLhs.PChz());
					return TCRET_StoppingError;
				}

				auto pSymtab = pTcsentTop->m_pSymtab;

				auto pTinIndex = PTinPromoteLiteralDefault(pTcwork, pSymtab, pStnodIndex);
				if (pTinIndex->m_tink != TINK_Integer &&  pTinIndex->m_tink != TINK_Integer)
				{
					CString strTinIndex = StrFromTypeInfo(pTinIndex);
					EmitError(pTcwork, pStnod, "Cannot convert %s to integer for array index", strTinIndex.PChz());
				}

				FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinIndex, pStnodIndex);
				
				pStnod->m_strees = STREES_TypeChecked;
				PopTcsent(pTcfram);
			} break;
			case PARK_ReservedWord:
			{
				if (EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
				{
					RWORD rword = pStnod->m_pStval->m_rword;
					switch (rword)
					{
					case RWORD_While:
					case RWORD_If:
						{
							if (pTcsentTop->m_nState < pStnod->CStnodChild())
							{
								PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
								break;
							}

							if (pStnod->CStnodChild() < 2)
							{
								EmitError(
									pTcwork,
									pStnod,
									"encountered %s statement without expected predicate,child",
									PChzFromRword(rword));
								return TCRET_StoppingError;
							}

							// (if (predicate) (ifCase) (else (elseCase)))
							// (while (predicate) (body))
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
							FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinBool, pStnodPred);

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
										FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinReturn, pStnodRhs);
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
										pStnod->m_pTinOperand = pTinOperand;
										pStnod->m_pStval = pStval;
									}
									else
									{
										EmitError(
											pTcwork, 
											pStnod, 
											"invalid opcode %s for %s literal and %s literal", 
											PChzFromJtok(pStnod->m_jtok),
											PChzFromLitk(((STypeInfoLiteral *)pTinLhs)->m_litty.m_litk),
											PChzFromLitk(((STypeInfoLiteral *)pTinRhs)->m_litty.m_litk));
										return TCRET_StoppingError;
									}
								}
							}
							else
							{
								PARK park = pStnod->m_park;
								STypeInfo * pTinUpcastLhs = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodLhs, pTinRhs);
								STypeInfo * pTinUpcastRhs = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodRhs, pTinLhs);
								STypeInfo * pTinOperand = PTinOperandFromPark(pTcwork, pSymtab, park, pTinUpcastLhs, pTinUpcastRhs);

								if (!pTinOperand)
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
									return TCRET_StoppingError;
								}

								pStnod->m_pTin = (FDoesOperatorReturnBool(park)) ? 
													pSymtab->PTinBuiltin("bool") :
													pTinOperand;
								pStnod->m_pTinOperand = pTinOperand;

								FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinOperand, pStnodLhs);
								FinalizeLiteralType(pTcsentTop->m_pSymtab, pTinOperand, pStnodRhs);
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
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
						pStnod->m_pTinOperand = pTinOperand;

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
									STypeInfoLiteral * pTinOperand;
									CSTValue * pStval;
									if (FComputeUnaryOpOnLiteral(
											pTcwork,
											pStnod,
											pSymtab,
											pStnodOperand,
											&pTinOperand,
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
											PChzFromJtok(pStnod->m_jtok),
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
											EmitError(pTcwork, pStnod, "Cannot dereference type %s", strOp.PChz());
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
										bool FCanTakeReference = pTinOperand->m_tink != TINK_Literal;
										if (!FCanTakeReference)
										{
											CString strOp = StrFromTypeInfo(pTinOperand);
											EmitError(pTcwork, pStnod, "Cannot take reference of type %s", strOp.PChz());
											return TCRET_StoppingError;
										}

										CSymbolTable * pSymtab = pTcsentTop->m_pSymtab;
										pStnod->m_pTin = pSymtab->PTinptrGetReference(pTinOperand);
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
										pStnod->m_pTinOperand = pTinBool;
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
											LITK litk = ((STypeInfoLiteral *)pTinOperand)->m_litty.m_litk;
											fIsInteger |= litk == LITK_Integer;
											fIsFloat |= litk == LITK_Float;
										}

										bool fIsValidPtrOp = ((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) &
											(tinkOperand == TINK_Pointer);
										bool fIsValidFloatOp = ((jtok == JTOK('+')) | (jtok == JTOK('-')) | (jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) & fIsFloat;
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
													EmitError(
														pTcwork,
														pStnod,
														"negate operand not valid for unsigned type %s",
														strOp.PChz());
												}
											}
										}
									}break;
								}
							}
						}
					}

					pStnod->m_strees = STREES_TypeChecked;
					PopTcsent(pTcfram);
					break;
				}
				PushTcsent(pTcfram, pStnod->PStnodChild(pTcsentTop->m_nState++));
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

/* enum LITSTOR
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
}*/

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
		pTcsent->m_tcctx = TCCTX_Normal;

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

	//PerformFlushResolvedLiteralsPass(pTcwork, paryEntry);

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
				AssertEquals(BintShiftRight(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs >> nRhs));
				AssertEquals(BintShiftLeft(BintFromInt(nLhs), BintFromInt(nRhs)), BintFromInt(nLhs << nRhs));
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
				AssertEquals(BintShiftRight(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs >> nRhs));
				AssertEquals(BintShiftLeft(BintFromUint(nLhs), BintFromUint(nRhs)), BintFromUint(nLhs << nRhs));
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
	AssertTestSigned65();

	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);

	const char * pChzIn;
	const char * pChzOut;

	pChzIn = "aN : [4] s32; pN : * s32; fEq := aN == pN; ";
	pChzOut = "([]s32 $aN ([]s32 Literal:Int64 s32)) (*s32 $pN (*s32 s32)) (bool $fEq (bool []s32 *s32))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "aN : [4] s32; paN : * [4] s32 = *aN;";
	pChzOut = "([]s32 $aN ([]s32 Literal:Int64 s32)) (*[]s32 $paN (*[]s32 ([]s32 Literal:Int64 s32)) (*[]s32 []s32))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "aN : [4] s32; n := aN[0];";
	pChzOut = "([]s32 $aN ([]s32 Literal:Int64 s32)) (s32 $n (s32 []s32 Literal:Int64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "n:s32=2; n++; n--;";
	pChzOut ="(s32 $n s32 Literal:Int32) (s32 s32) (s32 s32)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "{ ; }";
	pChzOut ="({} (Nop))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "printf :: (pCh : * u8, ..) -> s32 #foreign;";
	pChzOut ="(printf() $printf (Params (*u8 $pCh (*u8 u8)) (..)) s32)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "Vararg :: (..) #foreign; Vararg(2.2, 8,2000);";
	pChzOut ="(Vararg() $Vararg (Params (..)) void) (void $Vararg Literal:Float64 Literal:Int64 Literal:Int64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "n:=7; pN:=*n; ppN:=*pN; pN2:=@ppN; n2:=@pN2;";
	pChzOut ="(int $n Literal:Int64) (*int $pN (*int int)) (**int $ppN (**int *int)) "
				"(*int $pN2 (*int **int)) (int $n2 (int *int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "pN : * int; @pN = 2;";
	pChzOut ="(*int $pN (*int int)) (int (int *int) Literal:Int64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "pChz:*u8=\"teststring\"; ";
	pChzOut ="(*u8 $pChz (*u8 u8) Literal:String)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "a:=2-2; n:=-2;";
	pChzOut ="(int $a (Literal:Int64 Literal:Int Literal:Int)) (int $n (Literal:Int64 Literal:Int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "foo :: (n : bool) #foreign; n:s16; ack :: ( n : s32) { test := n; n : s64; test2 := n; }"; 
	pChzOut		= "(foo() $foo (Params (bool $n bool)) void) (s16 $n s16) "
					"(ack() $ack (Params (s32 $n s32)) void ({} (s32 $test s32) (s64 $n s64) (s64 $test2 s64) (void)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=5; foo:=i; g:=g_g; } g_g:=2.2;";
	pChzOut = "({} (int $i Literal:Int64) (int $foo int) (float $g float)) (float $g_g Literal:Float32)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"i:= (5-4) + (5-6); g:=2.2 + (3.3*10);";
	pChzOut = "(int $i (Literal:Int64 (Literal:Int Literal:Int Literal:Int) (Literal:Int Literal:Int Literal:Int))) "
				"(float $g (Literal:Float32 Literal:Float (Literal:Float Literal:Float Literal:Int)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "n:=2; n = 100-n;";
	pChzOut ="(int $n Literal:Int64) (int int (int Literal:Int64 int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; bar:s16=i; g:=g_g; } g_g : float64 = 2.2;";
	pChzOut = "({} (s8 $i s8 Literal:Int8) (s8 $foo s8) (s16 $bar s16 s8) (float64 $g float64)) (float64 $g_g float64 Literal:Float64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=true; foo:=i; g:=g_g; } g_g : bool = false;";
	pChzOut = "({} (bool $i Literal:Bool8) (bool $foo bool) (bool $g bool)) (bool $g_g bool Literal:Bool8)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "ParamFunc :: (nA : s32, g : float) { foo := nA; bah := g; }";
	pChzOut		= "(ParamFunc() $ParamFunc (params (s32 $nA s32) (float $g float)) void ({} (s32 $foo s32) (float $bah float) (void)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=\"hello\"; foo:=i; g:=g_g; } g_g : *u8 = \"huzzah\";";
	//pChzOut = "({} (string $i Literal:String) (string $foo string) (string $g string)) (string $g_g string Literal:String)";
	pChzOut = "({} (*u8 $i Literal:String) (*u8 $foo *u8) (*u8 $g *u8)) (*u8 $g_g (*u8 u8) Literal:String)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ pN:* s8; pNInfer:=pN; pNTest:*s8=null;}";
	pChzOut = "({} (*s8 $pN (*s8 s8)) (*s8 $pNInfer *s8) (*s8 $pNTest (*s8 s8) Literal:Null))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ pN:** s8; pNInfer:=pN; pNTest:**s8=null;}";
	pChzOut = "({} (**s8 $pN (**s8 (*s8 s8))) (**s8 $pNInfer **s8) (**s8 $pNTest (**s8 (*s8 s8)) Literal:Null))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; foo=6; i = foo; }";
	pChzOut = "({} (s8 $i s8 Literal:Int8) (s8 $foo s8) (s8 s8 Literal:Int8) (s8 s8 s8))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; fBool:bool = foo==i; fBool = i<2; }";
	pChzOut = "({} (s8 $i s8 Literal:Int8) (s8 $foo s8) (bool $fBool bool (bool s8 s8)) (bool bool (bool s8 Literal:Int8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8; foo:s32; foo=i+foo; foo=foo<<i; }";
	pChzOut = "({} (s8 $i s8) (s32 $foo s32) (s32 s32 (s32 s8 s32)) (s32 s32 (s32 s32 s8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s8; pN:=*n; n2:=@pN; }";
	pChzOut = "({} (s8 $n s8) (*s8 $pN (*s8 s8)) (s8 $n2 (s8 *s8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; pN:*s8; fN := !pN; ++n; --n;}";
	pChzOut = "({} (s64 $n s64) (*s8 $pN (*s8 s8)) (bool $fN (bool *s8)) (s64 s64) (s64 s64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; if n == 2 n = 5; else n = 6;}";
	pChzOut = "({} (s64 $n s64) (bool (bool s64 Literal:Int64) (s64 s64 Literal:Int64) (??? (s64 s64 Literal:Int64))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64 = 5; while n > 0 { --n; } }";
	pChzOut = "({} (s64 $n s64 Literal:Int64) (bool (bool s64 Literal:Int64) ({} (s64 s64))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	//pChzIn =	"{ n:int; if (n < 2) n = 5; else n = 2; }";
	//pChzIn =	"PrintIf :: (n : int) { if (n < 2) n = 5; else if (n == 2) n = 0; else n = 1; }";
	//pChzOut = "({} (int $n int) (bool (bool int Literal:Int64) (int int Literal:Int64) (??? (int int Literal:Int64))))";
	//AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn = "{ nA : s8; nB : s8; nC := (nA < nB) == (nB >= nA); }";
	pChzOut = "({} (s8 $nA s8) (s8 $nB s8) (bool $nC (bool (bool s8 s8) (bool s8 s8))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; if n n = 5; else n = 6;}";
	pChzOut = "({} (s64 $n s64) (bool s64 (s64 s64 Literal:Int64) (??? (s64 s64 Literal:Int64))))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "AddNums :: (a : int, b := 1) -> int { return a + b;} n := AddNums(2,3);";
	pChzOut		= "(AddNums() $AddNums (Params (int $a int) (int $b Literal:Int64)) int ({} (int (int int int))))"
					" (int $n (int $AddNums Literal:Int64 Literal:Int64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "NoReturn :: (a : int) { n := a;} NoReturn(2);";
	pChzOut		= "(NoReturn() $NoReturn (Params (int $a int)) void ({} (int $n int) (void))) (void $NoReturn Literal:Int64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "NoReturn :: (a : int) { n := a; return; } NoReturn(2);";
	pChzOut		= "(NoReturn() $NoReturn (Params (int $a int)) void ({} (int $n int) (void))) (void $NoReturn Literal:Int64)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);
	
	pChzIn		= " { n:int=2; Foo :: () -> int { n2:=n; g:=Bar();}    Bar :: () -> float { n:=Foo(); } }";
	pChzOut		=	"(Foo() $Foo int ({} (int $n2 int) (float $g (float $Bar))))"
					" (Bar() $Bar float ({} (int $n (int $Foo))))"
					" ({} (int $n int Literal:Int64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "{ ovr:=2; { nNest:= ovr; ovr:float=2.2; g:=ovr; } n:=ovr; }"; 
	pChzOut		= "({} (int $ovr Literal:Int64) ({} (int $nNest int) (float $ovr float Literal:Float32) (float $g float)) (int $n int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);
	
	pChzIn		= " { ovr:=2; Foo :: () { nNest:= ovr; ovr:float=2.2; g:=ovr; } n:=ovr; }"; 
	pChzOut		=	"(Foo() $Foo void ({} (int $nNest int) (float $ovr float Literal:Float32) (float $g float) (void)))"
					" ({} (int $ovr Literal:Int64) (int $n int))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn		= "ack :: ( n : s32) { if (n < 2) foo(true); }  foo :: (n : bool) #foreign;"; 
	pChzOut		= "(ack() $ack (Params (s32 $n s32)) void ({} (bool (bool s32 Literal:Int32) (void $foo Literal:Bool8)) (void))) "
					"(foo() $foo (Params (bool $n bool)) void)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	StaticShutdownStrings(&allocString);
}