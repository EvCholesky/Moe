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
	CSymbolTable *	m_pSymtab;		// BB - Could omit this pointer with careful handling of stack pops?
									//  maybe swap out for fPushedStack?
	GRFSYMLOOK		m_grfsymlook;
};

struct STypeCheckFrame // tag = tcfram
{
	int								m_iTcfram;			// index in tcwork::m_aryTcfram 
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
	
	printf (" xx(xx) error: ");
	++pTcwork->m_cError;
	
	if (pChz)
	{
		va_list ap;
		va_start(ap, pChz);
		vprintf(pChz, ap);
		printf("\n");
	}
}

CString StrFromIdentifier(CSTNode * pStnod)
{
	if (EWC_FVERIFY(pStnod->m_park == PARK_Identifier, "expected identifier") && 
		EWC_FVERIFY(pStnod->m_pStval, "identifier encountered without string value"))
	{
		return pStnod->m_pStval->m_str;
	}

	return CString();
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
			}break;
			case PARK_Reference:
				pCh += CChCopy("* ", pCh, pChEnd - pCh); 
				break;
			case PARK_ArrayDecl:
				// BB - should follow the [], [..], [c] convention
				pCh += CChCopy("[] ", pCh, pChEnd - pCh); 
				break;
			default:
				pCh += CChCopy("<BadPark> ", pCh, pChEnd - pCh); 
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

			pTinFinal = pSymtab->PTinLookup(pStnodIt->m_pStval->m_str, grfsymlook, ppSymType);

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
	TCRET_WaitingForTypeDefinition,
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
	case LITK_Int:
		{
			if (stval.m_n >= LLONG_MAX)		return pSymtab->PTinLookup("u64");
			else							return pSymtab->PTinLookup("s64");
		}
	case LITK_Float:	return pSymtab->PTinLookup("float");
	case LITK_Char:		return pSymtab->PTinLookup("char");
	case LITK_String:	return pSymtab->PTinLookup("string");
	case LITK_Bool:		return pSymtab->PTinLookup("bool");
	case LITK_Null:
		{
			EmitError(pTcwork, pStnodLit, "Cannot infer type for null");
		}
	}
	return nullptr;
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
	case LITK_Int:
		{
			if (stval.m_n < SCHAR_MAX)		return pSymtab->PTinLookup("s8");
			if (stval.m_n < SHRT_MAX)		return pSymtab->PTinLookup("s16");
			if (stval.m_n < INT_MAX)		return pSymtab->PTinLookup("s32");
			if (stval.m_n < LLONG_MAX)		return pSymtab->PTinLookup("s64");
			else							return pSymtab->PTinLookup("u64");
		}
	case LITK_Float:	return pSymtab->PTinLookup("float");
	case LITK_Char:		return pSymtab->PTinLookup("char");
	case LITK_String:	return pSymtab->PTinLookup("string");
	case LITK_Bool:		return pSymtab->PTinLookup("bool");
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

			if (pTinintA->m_fSigned != pTinintB->m_fSigned)
				return nullptr;
		
			return (pTinintA->m_cBit >= pTinintB->m_cBit) ? pTinA : pTinB;
		}
		if (pTinA == pTinB)
			return pTinA;
	}
	return nullptr;
}

bool FCanImplicitCast(STypeInfo * pTinFrom, STypeInfo * pTinTo)
{
	EWC_ASSERT(pTinFrom->m_tink != TINK_Literal, "literals should be promoted before calling FCanImplicitCast()");

	if (pTinFrom->m_tink == pTinTo->m_tink)
	{
		if (pTinFrom->m_tink == TINK_Float)
		{
			STypeInfoFloat * pTinfloatFrom = (STypeInfoFloat *)pTinFrom;
			STypeInfoFloat * pTinfloatTo = (STypeInfoFloat *)pTinTo;
			return pTinfloatTo->m_cBit >= pTinfloatFrom->m_cBit;
		}
		if (pTinFrom->m_tink == TINK_Integer)
		{
			STypeInfoInteger * pTinintFrom = (STypeInfoInteger *)pTinFrom;
			STypeInfoInteger * pTinintTo = (STypeInfoInteger *)pTinTo;

			// BB - this could be more forgiving... allow signed/unsigned conversions if a higher cBit
			return (pTinintTo->m_cBit >= pTinintFrom->m_cBit) & (pTinintTo->m_fSigned == pTinintFrom->m_fSigned);
		}
		return pTinFrom == pTinTo;
	}
	else if (pTinFrom->m_tink == TINK_Pointer)
	{
		if (pTinTo->m_tink == TINK_Bool)
			return true;
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

TCRET TypeCheckSubtree(STypeCheckWorkspace * pTcwork, STypeCheckFrame * pTcfram)
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

				SSymbol * pSymProc = nullptr;
				CString strProcName;
				if (pStproc->m_iStnodProcName >= 0)
				{
					strProcName = StrFromIdentifier(pStnod->PStnodChild(pStproc->m_iStnodProcName));
					if (!strProcName.FIsEmpty())
					{
						pSymProc = pTcsentTop->m_pSymtab->PSymLookup(strProcName, pTcsentTop->m_grfsymlook);
					}
				}

				if (!EWC_FVERIFY(pSymProc, "failed to find procedure name symbol"))
					return TCRET_StoppingError;
				if (!EWC_FVERIFY(pSymProc->m_pTin, "expected procedure type info to be created during parse"))
					return TCRET_StoppingError;

				// Can't I just push a child per state?
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
					{	// type check the return list
						if (pStproc->m_iStnodReturnType >= 0)
						{
							PushTcsent(pTcfram, pStnod->PStnodChild(pStproc->m_iStnodBody));
						}
					}break;
				case 2:
					{	// type check the body list
						if (pStproc->m_iStnodBody >= 0)
						{
							PushTcsent(pTcfram, pStnod->PStnodChild(pStproc->m_iStnodBody));
						}
					}break;
				case 3:
					{
						PopTcsent(pTcfram);

						pStnod->m_strees = STREES_TypeChecked;
						OnTypeComplete(pTcwork, pSymProc);
					}
				}
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
						pSymStruct = pTcsentTop->m_pSymtab->PSymLookup(strStructName, pTcsentTop->m_grfsymlook);
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
					SSymbol * pSym = pSymtab->PSymLookup(pStval->m_str, pTcsentTop->m_grfsymlook);
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
						}
						else
						{
							// Try to find the name of the type, if we can find it we're good, but if there's 
							// type inference the definition needs to be done with type checking

							CSTDecl * pStdecl = pStnodDefinition->m_pStdecl;
							STypeInfo * pTinDef = pStnodDefinition->m_pTin;
							SSymbol * pSymDepend = pSym;
							if (!pTinDef && pStdecl && pStdecl->m_iStnodType >= 0)
							{
								CSTNode * pStnodType = pStnodDefinition->PStnodChild(pStdecl->m_iStnodType);

								pTinDef = PTinFromTypeSpecification(
											pSymtab,
											pStnodType,
											pTcsentTop->m_grfsymlook,
											&pSymDepend);
							}

							if (!pTinDef)
							{
								// set up dependency for either the definition or the type...
								SUnknownType * pUntype = PUntypeEnsure(pTcwork, pSymDepend);
								pUntype->m_aryiTcframDependent.Append(pTcfram->m_iTcfram);
								return TCRET_WaitingForTypeDefinition;
							}
							pStnod->m_pTin = pTinDef;
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
								pUntype->m_aryiTcframDependent.Append(pTcfram->m_iTcfram);
								return TCRET_WaitingForTypeDefinition;
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
							pStnodInit->m_pTin = PTinPromoteLiteralDefault(
													pTcwork,
													pTcsentTop->m_pSymtab,
													pStnodInit);

							pStnod->m_pTin = pStnodInit->m_pTin;
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
						SSymbol * pSymIdent = pSymtab->PSymLookup(pStnodIdent->m_pStval->m_str, pTcsentTop->m_grfsymlook);
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
							STypeInfo * pTinUpcast = nullptr;
							if ((pTinLhs->m_tink == TINK_Literal) & (pTinLhs->m_tink == TINK_Literal))
							{
								// this needs to be explicitly handled, create a new literal with the result
								EWC_ASSERT(false, "TODO: support binary ops on two literals");
							}
							else
							{
								STypeInfo * pTinUpcastLhs = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodLhs, pTinRhs);
								STypeInfo * pTinUpcastRhs = PTinPromoteLiteralTightest(pTcwork, pSymtab, pStnodRhs, pTinLhs);
								pTinUpcast = PTinFindUpcast(pTcwork, pSymtab, pTinUpcastRhs, pTinUpcastLhs);
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
							}

							PARK park = pStnod->m_park;
							bool fIsLogicalOp = (park == PARK_RelationalOp) |
								(park == PARK_EqualityOp) |
								(park == PARK_LogicalAndOrOp);

							pStnod->m_pTin = (fIsLogicalOp) ? pSymtab->PTinLookup("bool") : pTinUpcast;
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
								STypeInfo * pTinBool = pTcsentTop->m_pSymtab->PTinLookup("bool");
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
								bool fIsValidPtrOp =	((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) & 
														(tinkOperand == TINK_Pointer);
								bool fIsValidFloatOp =  ((jtok == JTOK('+')) | (jtok == JTOK('-'))) & 
														(tinkOperand == TINK_Float);
								bool fIsSupported = fIsInteger | fIsValidPtrOp | fIsValidFloatOp;

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
										if (!pTinint->m_fSigned)
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

void PerformTypeCheck(CAlloc * pAlloc, CSymbolTable * pSymtabTop, CAry<CSTNode *> * parypStnodEntry)
{
	STypeCheckWorkspace * pTcwork = EWC_NEW(pAlloc, STypeCheckWorkspace) STypeCheckWorkspace(pAlloc, (s32)parypStnodEntry->C());

	CSTNode ** ppStnodMax = parypStnodEntry->PMac();
	int ipTcfram = 0;
	for (CSTNode ** ppStnod = parypStnodEntry->A(); ppStnod != ppStnodMax; ++ppStnod, ++ipTcfram)
	{
		STypeCheckFrame * pTcfram = pTcwork->m_aryTcfram.AppendNew();
		pTcfram->m_iTcfram = ipTcfram;
		pTcfram->m_ipTcframQueue = ipTcfram;

		pTcfram->m_aryTcsent.SetAlloc(pAlloc);
		STypeCheckStackEntry * pTcsent = pTcfram->m_aryTcsent.AppendNew();
		pTcsent->m_nState = 0;
		pTcsent->m_pStnod = *ppStnod;
		pTcsent->m_pSymtab = pSymtabTop;
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
		}
		else if (EWC_FVERIFY(tcret == TCRET_WaitingForTypeDefinition))
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
	EWC_ASSERT(pWork->m_arypStnodEntry.C() > 0);


	EndParse(pWork, &jlex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_arypStnodEntry);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type);

	EWC_ASSERT(FAreSame(aCh, pChzOut), "parse debug string doesn't match expected value");

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

	const char * pChzIn =	"{ i:=5; foo:=i; g:=g_g; } g_g:=2.2;";
	const char * pChzOut = "({} (s64 ??? s64) (s64 ??? s64) (float ??? float)) (float ??? float)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; bar:s16=i; g:=g_g; } g_g : float64 = 2.2;";
	pChzOut = "({} (s8 ??? s8 IntLiteral) (s8 ??? s8) (s16 ??? s16 s8) (float64 ??? float64)) (float64 ??? float64 FloatLiteral)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=true; foo:=i; g:=g_g; } g_g : bool = false;";
	pChzOut = "({} (bool ??? bool) (bool ??? bool) (bool ??? bool)) (bool ??? bool BoolLiteral)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:=\"hello\"; foo:=i; g:=g_g; } g_g : string = \"huzzah\";";
	pChzOut = "({} (string ??? string) (string ??? string) (string ??? string)) (string ??? string StringLiteral)";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ pN:* s8; pNInfer:=pN; pNTest:*s8=null;}";
	pChzOut = "({} (*s8 ??? (*s8 s8)) (*s8 ??? *s8) (*s8 ??? (*s8 s8) NullLiteral))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ pN:** s8; pNInfer:=pN; pNTest:**s8=null;}";
	pChzOut = "({} (**s8 ??? (**s8 (*s8 s8))) (**s8 ??? **s8) (**s8 ??? (**s8 (*s8 s8)) NullLiteral))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; foo=6; i = foo; }";
	pChzOut = "({} (s8 ??? s8 IntLiteral) (s8 ??? s8) (s8 s8 IntLiteral) (s8 s8 s8))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8=5; foo:=i; fBool:bool = foo==i; fBool = i<2; }";
	pChzOut = "({} (s8 ??? s8 IntLiteral) (s8 ??? s8) (bool ??? bool (bool s8 s8)) (bool bool (bool s8 IntLiteral)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ i:s8; foo:s32; foo=i+foo; foo=foo<<i; }";
	pChzOut = "({} (s8 ??? s8) (s32 ??? s32) (s32 s32 (s32 s8 s32)) (s32 s32 (s32 s32 s8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s8; pN:=*n; n2:=&pN; }";
	pChzOut = "({} (s8 ??? s8) (*s8 ??? (*s8 s8)) (s8 ??? (s8 *s8)))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	pChzIn =	"{ n:s64; pN:*s8; fN := !pN; ++n; --n;}";
	pChzOut = "({} (s64 ??? s64) (*s8 ??? (*s8 s8)) (bool ??? (bool *s8)) (s64 s64) (s64 s64))";
	AssertTestTypeCheck(&work, pChzIn, pChzOut);

	StaticShutdownStrings(&allocString);
}