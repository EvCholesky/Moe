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
#include <stdio.h>



using namespace EWC;

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SJaiLexer * pJlex, CSymbolTable * pSymtab);
CSTNode * PStnodParseDefinition(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseLogicalAndOrExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseStatement(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SJaiLexer * pJlex);

const char * PChzFromPark(PARK park)
{
	static const char * s_mpParkPChz[] =
	{
		"Error",
		"Identifier",
		"Reserved Word",
		"Nop",
		"Literal",
		"Additive Operator",
		"Multiplicative Operator",
		"Shift Operator",
		"Equality Operator",
		"Relational Operator",
		"BitwiseAndOr Operator",
		"LogicalAndOr Operator",
		"Assignment Operator",
		"Unary Operator",
		"Postfix Unary Operator",
		"Uninitializer",
		"Cast",
		"Array Element",		// [array, index]
		"Member Lookup",		// [struct, child]
		"Argument Call",		// [procedure, arg0, arg1, ...]
		"List",
		"Parameter List",
		"Array Decl",
		"If",
		"Else",
		"Reference Decl",
		"Decl",
		"Typedef",
		"Constant Decl",
		"Procedure Definition",
		"Enum Definition",
		"Struct Definition",
		"Enum Constant",
		"Variadic Argument",
		"Array Literal",
	};
	EWC_CASSERT(EWC_DIM(s_mpParkPChz) == PARK_Max, "missing PARK string");
	if (park == PARK_Nil)
		return "Nil";

	if ((park < PARK_Nil) | (park >= PARK_Max))
		return "Unknown PARK";

	return s_mpParkPChz[park];
}

const char * PChzFromAryk(ARYK aryk)
{
	static const char * s_mpArykPChz[] =
	{
		"Fixed",
		"Dynamic",
		"Reference",
	};
	EWC_CASSERT(EWC_DIM(s_mpArykPChz) == ARYK_Max, "missing ARYK string");
	if (aryk == ARYK_Nil)
		return "Nil";

	if ((aryk < ARYK_Nil) | (aryk >= ARYK_Max))
		return "Unknown ARYK";

	return s_mpArykPChz[aryk];
}


const char * PChzFromArymemb(ARYMEMB arymemb)
{
	static const char * s_mpArymembPChz[] =
	{
		"count",
		"data",
	};
	EWC_CASSERT(EWC_DIM(s_mpArymembPChz) == ARYMEMB_Max, "missing ARYMEMB string");
	if (arymemb == ARYMEMB_Nil)
		return "Nil";

	if ((arymemb < ARYMEMB_Nil) | (arymemb >= ARYMEMB_Max))
		return "Unknown ARYMEMB";

	return s_mpArymembPChz[arymemb];
}

ARYMEMB ArymembLookup(const char * pChzMember)
{
	for (int arymemb = ARYMEMB_Min; arymemb < ARYMEMB_Max; ++arymemb)
	{
		if (FAreSame(PChzFromArymemb((ARYMEMB)arymemb), pChzMember))
			return (ARYMEMB)arymemb;
	}
	return ARYMEMB_Nil;
}

const char * PChzFromLitk(LITK litk)
{
	static const char * s_mpLitkPChz[] =
	{
		"Int",			// int with unassigned type
		"Float",
		"Char",
		"String",
		"Bool",
		"Null",
		"Enum",
		"Array",
	};
	EWC_CASSERT(EWC_DIM(s_mpLitkPChz) == LITK_Max, "missing LITK string");
	if (litk == LITK_Nil)
		return "Nil";

	if ((litk < LITK_Nil) | (litk >= LITK_Max))
		return "Unknown LITK";

	return s_mpLitkPChz[litk];
}

const char * PChzFromTink(TINK tink)
{
	static const char * s_mpTinkPChz[] =
	{
		"integer",
		"float",
		"bool",
		"string",
		"pointer",
		"procedure",
		"void",
		"struct",
		"array",
		"null",
		"any",
		"enum",
		"forwardDecl",
		"literal",
	};
	EWC_CASSERT(EWC_DIM(s_mpTinkPChz) == TINK_Max, "missing TINK string");
	if (tink == TINK_Nil)
		return "Nil";

	if ((tink < TINK_Nil) | (tink >= TINK_Max))
		return "Unknown TINK";

	return s_mpTinkPChz[tink];
}

const char * PChzFromEnumimp(ENUMIMP enumimp)
{
	static const char * s_mpEnumimpPChz[] =
	{
		"nil",		//ENUMIMP_NilConstant,
		"min",		//ENUMIMP_MinConstant,
		"last",		//ENUMIMP_LastConstant,
		"max",		//ENUMIMP_MaxConstant,
		"names",	//ENUMIMP_Names,
		"values",	//ENUMIMP_Values,
	};
	EWC_CASSERT(EWC_DIM(s_mpEnumimpPChz) == ENUMIMP_Max, "missing ENUMIMP string");
	if (enumimp == ENUMIMP_Max)
		return "(Nil)";

	if ((enumimp < ENUMIMP_Nil) | (enumimp >= ENUMIMP_Max))
		return "unknown";

	return s_mpEnumimpPChz[enumimp];
}


void ParseError(CParseContext * pParctx, SJaiLexer * pJlex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pJlex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pParctx->m_pWork->m_pErrman, &lexloc, pChzFormat, ap);
}

void Expect(CParseContext * pParctx, SJaiLexer * pJlex, JTOK jtokExpected, const char * pChzInfo = nullptr, ...)
{
	if (pJlex->m_jtok != jtokExpected)
	{
		char aB[1024] = {0};
		if (pChzInfo)
		{
			va_list ap;
			va_start(ap, pChzInfo);
			vsprintf_s(aB, EWC_DIM(aB), pChzInfo, ap);
		}

		CString strIdent;
		const char * pChzFound = PChzCurrentToken(pJlex);
		if (pJlex->m_jtok == JTOK_Identifier)
		{
			strIdent = CString(pJlex->m_pChString, pJlex->m_cChString);
			pChzFound = strIdent.PChz();
		}

		ParseError(pParctx, pJlex, "Expected '%s' before '%s' %s", PChzFromJtok(jtokExpected), pChzFound, aB);
	}

	JtokNextToken(pJlex);
};

CSTNode * PStnodAllocateIdentifier(CParseContext * pParctx, const SLexerLocation & lexloc, const CString & strIdent)
{
	CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

	pStnod->m_park = PARK_Identifier;

	auto pStident = EWC_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
	pStident->m_str = strIdent;
	pStnod->m_pStident = pStident;

	return pStnod;
}

CSTNode * PStnodParseIdentifier(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok != JTOK_Identifier)
		return nullptr;

	SLexerLocation lexloc(pJlex);

	CString strIdent(pJlex->m_pChString, pJlex->m_cChString);
	auto pStnod = PStnodAllocateIdentifier(pParctx, lexloc, strIdent);
	pStnod->m_jtok = JTOK(pJlex->m_jtok);

	if (strIdent.FIsEmpty())
	{
		ParseError(pParctx, pJlex, "Identifier with no string");
	}
	else if (pJlex->m_pChString[0] == '#')
	{
		ParseError(pParctx, pJlex, "Unknown directive encountered %s", pJlex->m_pChString);
	}

	JtokNextToken(pJlex);
	return pStnod;
}

CSTNode * PStnodParseReservedWord(CParseContext * pParctx, SJaiLexer * pJlex, RWORD rwordExpected = RWORD_Nil)
{
	if (pJlex->m_jtok != JTOK_ReservedWord)
		return nullptr;

	RWORD rwordLookup = RwordLookup(pJlex);
	if ((rwordExpected != RWORD_Nil) & (rwordExpected != rwordLookup))
	{
		ParseError(pParctx, pJlex, "Expected %s before %s", PChzFromRword(rwordExpected), PChzCurrentToken(pJlex));
		return nullptr;
	}

	SLexerLocation lexloc(pJlex);
	CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

	pStnod->m_jtok = JTOK(pJlex->m_jtok);
	pStnod->m_park = PARK_ReservedWord;

	CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
	pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
	pStval->m_rword = rwordLookup;
	pStnod->m_pStval = pStval;

	JtokNextToken(pJlex);
	return pStnod;
}

STypeInfo * PTinForFloat(CSymbolTable * pSymtab, F64 gMin, F64 gLast)
{
	return nullptr;
}

STypeInfo * PTinForInt(CSymbolTable * pSymtab, u64 uMin, u64 nLast)
{
	return nullptr;
}

CSTNode * PStnodParseExpressionList(
	CParseContext * pParctx,
	SJaiLexer * pJlex)
{
	SLexerLocation lexloc(pJlex);

	CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
	CSTNode * pStnodList = nullptr;

	if (pStnodExp)
	{
		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_park = PARK_List;
		pStnodList->IAppendChild(pStnodExp);

		while (FConsumeToken(pJlex, JTOK(',')))
		{
			pStnodExp = PStnodParseExpression(pParctx, pJlex);

			if (!pStnodExp)
			{
				ParseError(pParctx, pJlex, "Expected expression before %s", PChzCurrentToken(pJlex));
				break;
			}
			pStnodList->IAppendChild(pStnodExp);
		}
	}

	return pStnodList;
}


CSTNode * PStnodParsePrimaryExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	switch(pJlex->m_jtok)
	{
		case JTOK_Identifier:
			{
				CSTNode * pStnod = PStnodParseIdentifier(pParctx, pJlex);
				return pStnod;
			}
		case JTOK_ReservedWord:
			{
				RWORD rword = RwordLookup(pJlex);
				switch (rword)
				{
				case RWORD_True:
				case RWORD_False:
				case RWORD_Null:
					{
						SLexerLocation lexloc(pJlex);
						CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

						pStnod->m_jtok = JTOK(pJlex->m_jtok);
						pStnod->m_park = PARK_Literal;

						CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
						pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
						pStval->m_stvalk = STVALK_ReservedWord;
						pStval->m_nUnsigned = (rword == RWORD_True) ? 1 : 0;
						pStval->m_rword = rword;
						pStval->m_litkLex = (rword == RWORD_Null) ? LITK_Null : LITK_Bool;
						pStnod->m_pTin = nullptr;

						pStnod->m_pStval = pStval;

						JtokNextToken(pJlex);
						return pStnod;
					}
				}
				return nullptr;
			}
		case JTOK_Literal:
			{
				// NOTE - Negative literals don't exist until the type checking phase folds in the unary '-' operator

				SLexerLocation lexloc(pJlex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_jtok = JTOK(pJlex->m_jtok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
				pStval->m_litkLex = pJlex->m_litk;

				if (pJlex->m_litk == LITK_Float)
				{
					SetFloatValue(pStval, pJlex->m_g);
				}
				else if ((pJlex->m_litk == LITK_String) || (pJlex->m_litk == LITK_Char))
				{
					pStval->m_stvalk = STVALK_String;
				}
				else
				{
					SetUnsignedIntValue(pStval, pJlex->m_n);
				}

				pStnod->m_pStval = pStval;

				JtokNextToken(pJlex);
				return pStnod;
			} 
		case JTOK('{'): // array literals
			{
				SLexerLocation lexloc(pJlex);
				JtokNextToken(pJlex); // consume '{'

				CSTNode * pStnodLit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodLit->m_jtok = JTOK(pJlex->m_jtok);
				pStnodLit->m_park = PARK_ArrayLiteral;

				// We're using a decl here... may need a custom value structure
				CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
				pStnodLit->m_pStdecl = pStdecl;

				if (FConsumeToken(pJlex, JTOK(':')))
				{
					auto pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
					pStdecl->m_iStnodType = pStnodLit->IAppendChild(pStnodType);

					Expect(pParctx, pJlex, JTOK(':'));
				}

				CSTNode * pStnodValues = PStnodParseExpressionList(pParctx, pJlex);
				pStdecl->m_iStnodInit = pStnodLit->IAppendChild(pStnodValues);

				Expect(pParctx, pJlex, JTOK('}'));
				return pStnodLit;
			} break;
		case '(':	// ( Expression )
			{
				JtokNextToken(pJlex); // consume '('

				CSTNode * pStnodReturn = PStnodParseExpression(pParctx, pJlex);
				Expect(pParctx, pJlex, JTOK(')'));
				return pStnodReturn;
			}

		default: return nullptr;
	}
}

CSTNode * PStnodParsePostfixExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParsePrimaryExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch(pJlex->m_jtok)
		{
		case JTOK('['):		// [ expression ]
			{
				SLexerLocation lexloc(pJlex);
				JtokNextToken(pJlex); // consume '('

				CSTNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArray->m_jtok = JTOK(pJlex->m_jtok);
				pStnodArray->m_park = PARK_ArrayElement;
				pStnodArray->IAppendChild(pStnod);

				CSTNode * pStnodElement = PStnodParseExpression(pParctx, pJlex);
				pStnodArray->IAppendChild(pStnodElement);

				pStnod = pStnodArray;
				Expect(pParctx, pJlex, JTOK(']'));
			} break;
		case JTOK('('):		// ( )
			{				// ( ArgumentExpressionList )
				SLexerLocation lexloc(pJlex);
				JtokNextToken(pJlex); // consume '('

				CSTNode * pStnodIdent = nullptr;
				if (pStnod->m_park == PARK_Identifier)
				{
					// clear out the identifier's type info
					pStnod->m_pTin = nullptr;
					pStnodIdent = pStnod;
				}

				CSTNode * pStnodArgList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArgList->m_jtok = JTOK(pJlex->m_jtok);
				pStnodArgList->m_park = PARK_ProcedureCall;
				pStnodArgList->IAppendChild(pStnod);
				pStnod = pStnodArgList;

				// parsing this with LogicalAndOrExpression even though ISO c uses assignmentExpression
				//  need to change this if we expect assignments to return the assigned value (x := a = b; )

				while (1)
				{
					CSTNode * pStnodArg = PStnodParseLogicalAndOrExpression(pParctx, pJlex);
					pStnodArgList->IAppendChild(pStnodArg);

					if ((pStnodArg==nullptr) | (pJlex->m_jtok != JTOK(',')))
						break;
					Expect(pParctx, pJlex, JTOK(','));
				}

				Expect(
					pParctx,
					pJlex,
					JTOK(')'),
					"while parsing procedure call '%s'", 
					pStnodIdent ? StrFromIdentifier(pStnodIdent).PChz() : "unknown");
			}break;
		case JTOK('.'):		// . identifier
			{
				JtokNextToken(pJlex); // consume '.'
				SLexerLocation lexloc(pJlex);

				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
				if (!pStnodIdent)
				{
					ParseError(pParctx, pJlex, "Expected identifier after '.' before %s", PChzFromJtok(jtokPrev));
				}
				else
				{
					CSTNode * pStnodMember = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodMember->m_jtok = jtokPrev;
					pStnodMember->m_park = PARK_MemberLookup;
					pStnodMember->IAppendChild(pStnod);
					pStnodMember->IAppendChild(pStnodIdent);
					pStnod = pStnodMember;
				}
			} break;
		case JTOK_PlusPlus:
		case JTOK_MinusMinus:
			{
				SLexerLocation lexloc(pJlex);

				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume '++' or '--'

				CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodUnary->m_jtok = jtokPrev;
				pStnodUnary->m_park = PARK_PostfixUnaryOp;
				pStnodUnary->IAppendChild(pStnod);

				pStnod = pStnodUnary;
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseUnaryExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK_DoubleReference)
	{
		SplitToken(pJlex, JTOK_Reference);
	}

	switch(pJlex->m_jtok)
	{
	case JTOK_Dereference:
	case JTOK_Reference:
	case JTOK('+'):
	case JTOK('-'):
	case JTOK('~'):
	case JTOK('!'):
	case JTOK_PlusPlus:
	case JTOK_MinusMinus:
		{
			JTOK jtokPrev = JTOK(pJlex->m_jtok);	
			SLexerLocation lexloc(pJlex);

			JtokNextToken(pJlex); // consume unary operator

			CSTNode * pStnodExp = PStnodParseUnaryExpression(pParctx, pJlex);
			if (!pStnodExp)
			{
				ParseError(
					pParctx,
					pJlex,
					"Unary operator '%s' missing operand before %s",
					PChzFromJtok(jtokPrev),
					PChzCurrentToken(pJlex));
				return nullptr;
			}

			CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodUnary->m_jtok = jtokPrev;
			pStnodUnary->m_park = PARK_UnaryOp;
			pStnodUnary->IAppendChild(pStnodExp);

			return pStnodUnary;
		}
	default: return PStnodParsePostfixExpression(pParctx, pJlex);
	}
}

CSTNode * PStnodParseCastExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnodCast = nullptr;
	CSTDecl * pStdecl = nullptr;
	if (RwordLookup(pJlex) == RWORD_Cast)
	{
		JtokNextToken(pJlex);

		SLexerLocation lexloc(pJlex);
		pStnodCast = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodCast->m_park = PARK_Cast;

		pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
		pStnodCast->m_pStdecl = pStdecl;

		Expect(pParctx, pJlex, JTOK('('));

		auto pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
		pStdecl->m_iStnodType = pStnodCast->IAppendChild(pStnodType);

		Expect(pParctx, pJlex, JTOK(')'));
	}

	auto pStnodChild = PStnodParseUnaryExpression(pParctx, pJlex);
	if (!pStnodCast)
		return pStnodChild;

	pStdecl->m_iStnodInit = pStnodCast->IAppendChild(pStnodChild);

	if (pStdecl->m_iStnodInit < 0)
	{
		EmitError(pParctx->m_pWork->m_pErrman, &pStnodCast->m_lexloc, "Cast statement missing right hand side");
	}

	if (pStdecl->m_iStnodType < 0)
	{
		EmitError(pParctx->m_pWork->m_pErrman, &pStnodCast->m_lexloc, "Cast statement missing type");
	}
	return pStnodCast;
}

CSTNode * PStnodHandleExpressionRHS(
	CParseContext * pParctx,
	SJaiLexer * pJlex,
	const SLexerLocation & lexloc,
	JTOK jtokExpression,
	PARK parkExpression,
	CSTNode * pStnodLhs,
	CSTNode * pStnodRhs)
{
	if (!pStnodRhs)
	{
		ParseError(
			pParctx,
			pJlex,
			"operator '%s' missing right hand side before %s",
			PChzFromJtok(jtokExpression),
			PChzCurrentToken(pJlex));
		return pStnodLhs;
	}

	CSTNode * pStnodExp = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodExp->m_jtok = jtokExpression;
	pStnodExp->m_park = parkExpression;
	pStnodExp->IAppendChild(pStnodLhs);
	pStnodExp->IAppendChild(pStnodRhs);
	return pStnodExp;
}

CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseCastExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK('*'):
		case JTOK('/'):
		case JTOK('%'):
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseCastExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_MultiplicativeOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseAdditiveExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseMultiplicativeExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK('+'):
		case JTOK('-'):
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseMultiplicativeExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_AdditiveOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseShiftExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseAdditiveExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK_ShiftLeft:
		case JTOK_ShiftRight:
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseAdditiveExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_ShiftOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseRelationalExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseShiftExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK('<'):
		case JTOK('>'):
		case JTOK_LessEqual:
		case JTOK_GreaterEqual:
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseShiftExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_RelationalOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseEqualityExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseRelationalExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK_EqualEqual:
		case JTOK_NotEqual:
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseRelationalExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_EqualityOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseBitwiseAndOrExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	// BB - This is a little different than ISO C precedence rules, we're treating all bitwise operators the same
	//  rather than inclusiveOr < exclusiveOr < bitwiseAnd

	CSTNode * pStnod = PStnodParseEqualityExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK('|'):
		case JTOK('&'):
		case JTOK('^'):
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseEqualityExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_BitwiseAndOrOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseLogicalAndOrExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseBitwiseAndOrExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK_OrOr:
		case JTOK_AndAnd:
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseBitwiseAndOrExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseAssignmentExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseLogicalAndOrExpression(pParctx, pJlex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pJlex->m_jtok)
		{
		case JTOK('='):
		case JTOK_MulEqual:
		case JTOK_DivEqual:
		case JTOK_ModEqual:
		case JTOK_PlusEqual:
		case JTOK_MinusEqual:
		case JTOK_AndEqual:
		case JTOK_OrEqual:
		case JTOK_XorEqual:
			{
				SLexerLocation lexloc(pJlex);
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseLogicalAndOrExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, lexloc, jtokPrev, PARK_AssignmentOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseAssignmentExpression(pParctx, pJlex);

	// TODO: handle Expression > AssignmentExpression , AssignmentExpression

	return pStnod;
}

CSTNode * PStnodParseArrayDecl(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (FConsumeToken(pJlex, JTOK('[')))
	{
		SLexerLocation lexloc(pJlex);
		CSTNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodArray->m_jtok = (JTOK)pJlex->m_jtok;
		pStnodArray->m_park = PARK_ArrayDecl;

		if (FConsumeToken(pJlex, JTOK_PeriodPeriod))
		{
			;
		}
		else if (pJlex->m_jtok != JTOK(']'))
		{
			CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
			if (pStnodExp)
			{
				pStnodArray->IAppendChild(pStnodExp);
			}
		}

		Expect(pParctx, pJlex, JTOK(']'));
		return pStnodArray;
	}
	return nullptr;
}

CSTNode * PStnodParsePointerDecl(CParseContext * pParctx, SJaiLexer * pJlex)
{
	// handle the mis-lexing of '&&' as one token here
	if (pJlex->m_jtok == JTOK_DoubleReference)
	{
		SplitToken(pJlex, JTOK_Reference);
	}

	if (FConsumeToken(pJlex, JTOK_Reference))
	{
		SLexerLocation lexloc(pJlex);
		CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnod->m_jtok = (JTOK)pJlex->m_jtok;
		pStnod->m_park = PARK_ReferenceDecl;
		return pStnod;
	}

	return nullptr;
}

CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseIdentifier(pParctx, pJlex);
	if (pStnod)
	{
		while (FConsumeToken(pJlex, JTOK('.')))
		{
			SLexerLocation lexloc(pJlex);

			JTOK jtokPrev = JTOK(pJlex->m_jtok);	
			CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
			if (!pStnodIdent)
			{
				ParseError(pParctx, pJlex, "Expected identifier after '.' before %s", PChzFromJtok(jtokPrev));
			}
			else
			{
				CSTNode * pStnodMember = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodMember->m_jtok = jtokPrev;
				pStnodMember->m_park = PARK_MemberLookup;
				pStnodMember->IAppendChild(pStnod);
				pStnodMember->IAppendChild(pStnodIdent);
				pStnod = pStnodMember;
			}
		}
		return pStnod;
	}

	pStnod = PStnodParsePointerDecl(pParctx, pJlex);
	if (!pStnod)
	{
		pStnod = PStnodParseArrayDecl(pParctx, pJlex);
	}
	if (!pStnod)
		return nullptr;

	CSTNode * pStnodChild = PStnodParseTypeSpecifier(pParctx, pJlex);
	if (!pStnodChild)
	{
		ParseError(pParctx, pJlex, "Expected type identifier before '%s'", PChzFromJtok(JTOK(pJlex->m_jtok)));
		// BB - Assume int to try to continue compilation? ala C?
	}
	else
	{
		pStnod->IAppendChild(pStnodChild);

		if (STypeInfoPointer * pTinptr = PTinRtiCast<STypeInfoPointer *>(pStnod->m_pTin))
		{
			pTinptr->m_pTinPointedTo = pStnodChild->m_pTin;
		}
		else if (STypeInfoArray * pTinary = PTinRtiCast<STypeInfoArray *>(pStnod->m_pTin))
		{
			pTinary->m_pTin = pStnodChild->m_pTin;
		}
	}
	return pStnod;
}

CSTNode * PStnodParseParameter(CParseContext * pParctx, SJaiLexer * pJlex, PARK parkContext)
{
	SLexerLocation lexloc(pJlex);
	if (pJlex->m_jtok == JTOK_PeriodPeriod && parkContext == PARK_ParameterList)
	{
		JtokNextToken(pJlex);

		CSTNode * pStnodVarArgs = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodVarArgs->m_jtok = (JTOK)pJlex->m_jtok;
		pStnodVarArgs->m_park = PARK_VariadicArg;
		return pStnodVarArgs;
	}

	if (pJlex->m_jtok != JTOK_Identifier)
		return nullptr;

	SJaiLexer jlexPeek = *pJlex;
	JtokNextToken(&jlexPeek);

	if ((jlexPeek.m_jtok != JTOK(':')) & (jlexPeek.m_jtok != JTOK_ColonEqual))
		return nullptr;

	CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
	if (!pStnodIdent)
		return nullptr;

	CSTNode * pStnodDecl = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodDecl->m_park = PARK_Decl;

	CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodDecl->m_pStdecl = pStdecl;

	pStdecl->m_iStnodIdentifier = pStnodDecl->IAppendChild(pStnodIdent);
	
	CSTNode * pStnodType = nullptr;
	CSTNode * pStnodInit = nullptr;
	if (FConsumeToken(pJlex, JTOK_ColonEqual))
	{
		pStnodInit = PStnodParseExpression(pParctx, pJlex);
	}
	else if (FConsumeToken(pJlex, JTOK(':')))
	{
		pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
		pStdecl->m_iStnodType = pStnodDecl->IAppendChild(pStnodType);

		// NOTE - I'm not propagating the type here as it may be unknown, leave this for the 
		//  type checking/inference pass.

		if (FConsumeToken(pJlex, JTOK('=')))
		{
			if (FConsumeToken(pJlex, JTOK_TripleMinus))
			{
				if (parkContext == PARK_ParameterList)
				{
					ParseError(pParctx, pJlex, "--- uninitializer not allowed in parameter lists");
				}
				else
				{
					SLexerLocation lexloc(pJlex);
					pStnodInit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodInit->m_jtok = JTOK_TripleMinus;
					pStnodInit->m_park = PARK_Uninitializer;

					if (!pStnodType)
					{
						ParseError(pParctx, pJlex, "Uninitializer not allowed without specified type");
					}
				}
			}
			else
			{
				pStnodInit = PStnodParseExpression(pParctx, pJlex);
				if (!pStnodInit)
					ParseError(pParctx, pJlex, "initial value expected before %s", PChzCurrentToken(pJlex));
			}
		}
		else if (FConsumeToken(pJlex, JTOK(':')))
		{
			pStnodDecl->m_park = PARK_ConstantDecl;
			pStnodInit = PStnodParseExpression(pParctx, pJlex);
			if (!pStnodInit)
				ParseError(pParctx, pJlex, "initial value expected before %s", PChzCurrentToken(pJlex));
		}
	}

	pStdecl->m_iStnodInit = pStnodDecl->IAppendChild(pStnodInit);
	if ((pStdecl->m_iStnodType == -1) & (pStdecl->m_iStnodInit == -1))
	{
		ParseError(pParctx, pJlex, "Expected type specifier or initialization");
	}

	CSymbolTable * pSymtab = pParctx->m_pSymtab;
	pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, StrFromIdentifier(pStnodIdent), pStnodDecl);

	// check to see if our type is known
	if (pStdecl->m_iStnodType >= 0)
	{
		pStnodDecl->m_pTin = pStnodType->m_pTin;
	}

	return pStnodDecl;
}

CSTNode * PStnodParseDecl(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseParameter(pParctx, pJlex, PARK_Decl);
	if (!pStnod)
		return nullptr;

	Expect(pParctx, pJlex, JTOK(';'));
	return pStnod;
}

CSTNode * PStnodParseMemberDeclList(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnodList = nullptr;

	while (1)
	{
		SLexerLocation lexloc(pJlex);
		CSTNode * pStnod = PStnodParseDecl(pParctx, pJlex);
		if (!pStnod)
		{
			pStnod = PStnodParseDefinition(pParctx, pJlex);
		}
		if (!pStnod)
			break;

		if (pStnodList == nullptr)
		{
			pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodList->m_jtok = JTOK('{');
			pStnodList->m_park = PARK_List;
			pStnodList->IAppendChild(pStnod);
		}
		else
		{
			pStnodList->IAppendChild(pStnod);
		}
	}
	return pStnodList;
}

CSTNode * PStnodParseParameterList(
	CParseContext * pParctx,
	SJaiLexer * pJlex,
	CSymbolTable * pSymtabProc)
{
	SLexerLocation lexloc(pJlex);
	PushSymbolTable(pParctx, pSymtabProc, lexloc);

	CSTNode * pStnodParam = PStnodParseParameter(pParctx, pJlex, PARK_ParameterList);
	CSTNode * pStnodList = nullptr;
	bool fHasVarArgs = pStnodParam && pStnodParam->m_park == PARK_VariadicArg;

	if (pStnodParam)
	{
		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_park = PARK_ParameterList;
		pStnodList->m_pSymtab = pSymtabProc;
		pStnodList->IAppendChild(pStnodParam);

		while (FConsumeToken(pJlex, JTOK(',')))
		{
			pStnodParam = PStnodParseParameter(pParctx, pJlex, PARK_ParameterList);
			fHasVarArgs |= pStnodParam->m_park == PARK_VariadicArg;

			if (!pStnodParam)
			{
				ParseError(pParctx, pJlex, "Expected parameter before %s", PChzCurrentToken(pJlex));
				break;
			}
			pStnodList->IAppendChild(pStnodParam);
		}
	}

	if (fHasVarArgs)
	{
		CSTNode * pStnodLast = pStnodList->PStnodChild(pStnodList->CStnodChild()-1);
		if (pStnodLast->m_park != PARK_VariadicArg)
		{
			ParseError(pParctx, pJlex, "Variadic function argument found before the end of the argument list");
		}
	}

	CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
	EWC_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (list)");

	return pStnodList;
}

CSTNode * PStnodSpoofEnumConstant(CParseContext * pParctx, const SLexerLocation & lexloc, const CString & strIdent, PARK park)
{
	CSTNode * pStnodConstant = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodConstant->m_park = park;
	pStnodConstant->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);

	CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodConstant->m_pStdecl = pStdecl;

	auto pStnodIdent = PStnodAllocateIdentifier(pParctx, lexloc, strIdent);
	pStnodIdent->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);
	pStdecl->m_iStnodIdentifier = pStnodConstant->IAppendChild(pStnodIdent);

	pStnodConstant->m_pSym = pParctx->m_pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, strIdent, pStnodConstant);
	return pStnodConstant;
}

CSTNode * PStnodParseEnumConstant(CParseContext * pParctx, SJaiLexer * pJlex)
{
	SLexerLocation lexloc(pJlex);
	CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
	if (!pStnodIdent)
		return nullptr;

	CSTNode * pStnodConstant = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodConstant->m_park = PARK_EnumConstant;

	CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodConstant->m_pStdecl = pStdecl;
	pStdecl->m_iStnodIdentifier = pStnodConstant->IAppendChild(pStnodIdent);

	CSymbolTable * pSymtab = pParctx->m_pSymtab;
	CString strIdent = StrFromIdentifier(pStnodIdent);

	// BB - we should change the interface here so we don't do multiple lookups
	auto pErrman = pParctx->m_pWork->m_pErrman;
	auto pSym = pSymtab->PSymLookup(strIdent, pStnodConstant->m_lexloc);
	if (pSym)
	{
		s32 iLine;
		s32 iCol;
		CalculateLinePosition(pErrman->m_pWork, &pSym->m_pStnodDefinition->m_lexloc, &iLine, &iCol);

		EmitError(pErrman, &pStnodConstant->m_lexloc, "Enum constant name '%s' has already been defined at (%d, %d)", strIdent.PChz(), iLine, iCol);
	}

	pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodConstant);
	pStnodConstant->m_pSym = pSym;

	if (FConsumeToken(pJlex, JTOK(':')))
	{
		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
		if (pStnodExp)
		{
			pStdecl->m_iStnodInit = pStnodConstant->IAppendChild(pStnodExp);
		}
	}
	return pStnodConstant;
}

CSTNode ** PPStnodChildFromPark(CSTNode * pStnod, int * pCStnodChild, PARK park)
{
	if (!pStnod || pStnod->m_park != park)
	{
		*pCStnodChild = 0;
		return nullptr;
	}

	*pCStnodChild = pStnod->CStnodChild();
	return pStnod->m_arypStnodChild.A();
}

CSTNode * PStnodParseEnumConstantList(CParseContext * pParctx, SJaiLexer * pJlex)
{
	SLexerLocation lexloc(pJlex);
	auto pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodList->m_jtok = JTOK('{');
	pStnodList->m_park = PARK_List;

	// add STNodes for implicit members: nil, min, max, etc.

	for (int enumimp = ENUMIMP_Min; enumimp < ENUMIMP_Max; ++enumimp)
	{
		PARK park = ((enumimp == ENUMIMP_Names) | (enumimp == ENUMIMP_Values)) ? PARK_ArrayLiteral : PARK_EnumConstant;
		auto pStnodImplicit = PStnodSpoofEnumConstant(pParctx, lexloc, PChzFromEnumimp((ENUMIMP)enumimp), park);
		pStnodList->IAppendChild(pStnodImplicit);
	}

	while (1)
	{
		CSTNode * pStnod = PStnodParseEnumConstant(pParctx, pJlex);
		if (!pStnod)
			break;

		pStnodList->IAppendChild(pStnod);

		if (!FConsumeToken(pJlex, JTOK(',')))
			break;
	}
	return pStnodList;
}

CSTNode * PStnodParseDefinition(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK_Identifier)
	{
		SJaiLexer jlexPeek = *pJlex;
		JtokNextToken(&jlexPeek);

		if (jlexPeek.m_jtok == JTOK_ColonColon)
		{
			SLexerLocation lexloc(pJlex);
			CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);

			*pJlex = jlexPeek;
			JtokNextToken(pJlex);
			
			// function definition
			if (FConsumeToken(pJlex, JTOK('(')))
			{
				CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodProc->m_park = PARK_ProcedureDefinition;
				pStnodProc->m_grfstnod.AddFlags(FSTNOD_EntryPoint);

				CSTProcedure * pStproc = EWC_NEW(pParctx->m_pAlloc, CSTProcedure) CSTProcedure();
				pStnodProc->m_pStproc = pStproc;
				pStproc->m_iStnodProcName = pStnodProc->IAppendChild(pStnodIdent);

				const CString & strName = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabProc = pParctx->m_pWork->PSymtabNew(strName);

				CSTNode * pStnodParams = PStnodParseParameterList(pParctx, pJlex, pSymtabProc);
				pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
				Expect(pParctx, pJlex, JTOK(')'));

				CSTNode * pStnodReturns = nullptr;
				bool fHasArrow = pJlex->m_jtok == JTOK_Arrow;
				if (FConsumeToken(pJlex, JTOK_Arrow))
				{
					// TODO : handle multiple return types

					pStnodReturns = PStnodParseTypeSpecifier(pParctx, pJlex);
					pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);
				}
				else
				{
					SLexerLocation lexloc(pJlex);
					CSTNode * pStnodVoid = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

					pStnodVoid->m_jtok = JTOK_Identifier;
					pStnodVoid->m_park = PARK_Identifier;

					auto pStident = EWC_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
					pStident->m_str = CString("void");
					pStnodVoid->m_pStident = pStident;
					pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodVoid);

					pStnodReturns = pStnodVoid;
				}

				pStproc->m_iStnodBody = -1;
				if (pJlex->m_jtok == JTOK_ReservedWord)
				{
					RWORD rwordLookup = RwordLookup(pJlex);
					if (rwordLookup != RWORD_ForeignDirective)
					{
						ParseError(
							pParctx,
							pJlex,
							"Unexpected token following procedure declaration %s\n",
							PChzFromRword(rwordLookup));
					}
					else
					{
						pStproc->m_fIsForeign = true;
					}

					JtokNextToken(pJlex);

					if (pJlex->m_jtok == JTOK_Identifier)
					{
						auto pStnodAlias = PStnodParseIdentifier(pParctx, pJlex);
						pStproc->m_iStnodForeignAlias = pStnodProc->IAppendChild(pStnodAlias);
					}

					// TODO: add support for foreign function aliasing (ie. Ack :: () -> int #foreign foo;)
					Expect(pParctx, pJlex, JTOK(';'), "While parsing foreign directive");
				}

				if (pJlex->m_jtok == JTOK('{'))
				{
					CSTNode * pStnodBody = PStnodParseCompoundStatement(pParctx, pJlex, pSymtabProc);
					pStproc->m_iStnodBody = pStnodProc->IAppendChild(pStnodBody);
				}

				if (pStproc->m_fIsForeign)
				{
					if (pStproc->m_iStnodBody != -1)
					{
						ParseError(
							pParctx,
							pJlex,
							"Procedure '%s' is marked foreign, but defines a procedure body.",
							strName.PChz());
						pStproc->m_iStnodBody = -1;
					}
				}
				else if (pStproc->m_iStnodBody == -1)
				{
					ParseError(pParctx, pJlex, "Procedure definition for '%s' has no body", strName.PChz());
				}

				int cStnodParams;
				CSTNode ** ppStnodParams = PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

				//int cStnodReturns = CChildrenInList(pStnodReturns, ppStnodReturns, PARK_Uhhhh);
				CSTNode ** ppStnodReturns = &pStnodReturns;
				int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;

				size_t cBAlloc = CBAlign(sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));
				cBAlloc = cBAlloc +	(cStnodParams + cStnodReturns) * sizeof(STypeInfo *);

				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc,8);

				STypeInfoProcedure * pTinproc = new(pB) STypeInfoProcedure(strName.PChz());
				STypeInfo ** ppTin = (STypeInfo**)PVAlign( pB + sizeof(STypeInfoProcedure), 
																		EWC_ALIGN_OF(STypeInfo *));
				pTinproc->m_arypTinParams.SetArray(ppTin, 0, cStnodParams);
				pTinproc->m_arypTinReturns.SetArray(&ppTin[cStnodParams], 0, cStnodReturns);
				pStnodProc->m_pTin = pTinproc;
				pTinproc->m_pStnodDefinition = pStnodProc;

				CSTNode ** ppStnodParamMax = &ppStnodParams[cStnodParams];
				for ( ; ppStnodParams != ppStnodParamMax; ++ppStnodParams)
				{
					CSTNode * pStnodParam = *ppStnodParams;
					if (pStnodParam->m_park == PARK_VariadicArg)
					{
						pTinproc->m_fHasVarArgs = true;
					}
					else if (EWC_FVERIFY(pStnodParam->m_park == PARK_Decl, "Expected decl"))
					{
						pTinproc->m_arypTinParams.Append(pStnodParam->m_pTin);
					}
				}

				CSTNode ** ppStnodReturnMax = &ppStnodReturns[cStnodReturns];
				for ( ; ppStnodReturns != ppStnodReturnMax; ++ppStnodReturns)
				{
					pTinproc->m_arypTinReturns.Append((*ppStnodReturns)->m_pTin);
				}

				if (pStproc->m_iStnodBody >= 0)
				{
					bool fReturnsVoid = false;
					if (EWC_FVERIFY(pStproc->m_iStnodReturnType != -1, "implicit return should be set"))
					{
						CSTNode * pStnodReturn = pStnodProc->PStnodChild(pStproc->m_iStnodReturnType);
						fReturnsVoid = FIsIdentifier(pStnodReturn, "void");
					}

					CSTNode * pStnodBody = pStnodProc->PStnodChild(pStproc->m_iStnodBody);

					if (EWC_FVERIFY( pStnodBody->m_park == PARK_List, "Expected body list"))
					{
						CSTNode * pStnodLast = pStnodBody->PStnodChildSafe(pStnodBody->CStnodChild()-1);

						bool fHasReturn = pStnodLast && FIsReservedWord(pStnodLast, RWORD_Return);
						if (!fHasReturn)
						{
							if (fReturnsVoid)
							{
								SLexerLocation lexloc(pJlex);
								CSTNode * pStnodReturn = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

								pStnodReturn->m_jtok = JTOK_ReservedWord;
								pStnodReturn->m_park = PARK_ReservedWord;

								auto pStvalReturn = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
								pStvalReturn->m_str = CString("return");
								pStvalReturn->m_rword = RWORD_Return;
								pStnodReturn->m_pStval = pStvalReturn;

								(void) pStnodBody->IAppendChild(pStnodReturn);
							}
							else
							{
								ParseError(pParctx, pJlex, "Procedure '%s' is missing return statement", strName.PChz());
							}
						}
					}
				}

				auto pErrman = pParctx->m_pWork->m_pErrman;
				SSymbol * pSymProc = pSymtabParent->PSymEnsure(pErrman, StrFromIdentifier(pStnodIdent), pStnodProc);
				pSymProc->m_pTin = pTinproc;

				pSymtabParent->AddManagedTin(pTinproc);

				return pStnodProc;
			}

			RWORD rword = RwordLookup(pJlex);
			if (rword == RWORD_Enum)
			{
				SLexerLocation lexloc(pJlex);
				JtokNextToken(pJlex);
				CSTNode * pStnodEnum = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodEnum->m_park = PARK_EnumDefinition;

				CSTEnum * pStenum = EWC_NEW(pParctx->m_pAlloc, CSTEnum) CSTEnum();
				pStnodEnum->m_pStenum = pStenum;
				pStenum->m_iStnodIdentifier = pStnodEnum->IAppendChild(pStnodIdent);

				CSTNode * pStnodType = PStnodParseIdentifier(pParctx, pJlex);
				pStenum->m_iStnodType = pStnodEnum->IAppendChild(pStnodType);
				
				const CString & strIdent = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabEnum = pParctx->m_pWork->PSymtabNew(strIdent);
				CSTNode * pStnodConstantList = nullptr;

				auto pErrman = pParctx->m_pWork->m_pErrman;
				(void) pSymtabEnum->PSymEnsure(pErrman, "loose", pStnodEnum);
				(void) pSymtabEnum->PSymEnsure(pErrman, "strict", pStnodEnum);

				if (FConsumeToken(pJlex, JTOK('{')))
				{
					SLexerLocation lexloc(pJlex);

					PushSymbolTable(pParctx, pSymtabEnum, lexloc);
					pStnodEnum->m_pSymtab = pSymtabEnum;

					pStnodConstantList = PStnodParseEnumConstantList(pParctx, pJlex);
					pStenum->m_iStnodConstantList = pStnodEnum->IAppendChild(pStnodConstantList);

					CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
					EWC_ASSERT(pSymtabEnum == pSymtabPop, "CSymbol table push/pop mismatch (enum)");

					Expect(pParctx, pJlex, JTOK('}'));
				}

				// type info enum
				int cStnodChild;
				CSTNode ** ppStnodMember = PPStnodChildFromPark(pStnodConstantList, &cStnodChild, PARK_List);
				int cStnodMember = cStnodChild; // cStnodArray;


				int cConstant = cStnodChild - (ENUMIMP_Max - ENUMIMP_Min) + ENUMIMP_CConstant;
				size_t cBAlloc = CBAlign(sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeInfoEnumConstant)) + 
								cConstant * sizeof(STypeInfoEnumConstant);
				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoEnum * pTinenum = new(pB) STypeInfoEnum(strIdent.PChz());
				auto aTinecon = (STypeInfoEnumConstant *)PVAlign( pB + sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeInfoEnumConstant));
				pTinenum->m_aryTinecon.SetArray(aTinecon, 0, cConstant);

				pTinenum->m_tinstructProduced.m_pStnodStruct = pStnodEnum;

				CSTNode ** ppStnodMemberMax = &ppStnodMember[cStnodMember];
				for ( ; ppStnodMember != ppStnodMemberMax; ++ppStnodMember)
				{
					CSTNode * pStnodMember = *ppStnodMember;

					switch (pStnodMember->m_park)
					{
					case PARK_EnumConstant:
						{
							auto pTinlit = EWC_NEW(pSymtabParent->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
							pSymtabParent->AddManagedTin(pTinlit);
							pTinlit->m_litty.m_litk = LITK_Enum;
							pTinlit->m_pTinSource = pTinenum;
							pStnodMember->m_pTin = pTinlit;
						} break;
					case PARK_ArrayLiteral:
						{
						} break;
					default: EWC_ASSERT(false, "Expected enum child value");
					}
				}

				pSymtabParent->AddManagedTin(pTinenum);

				SSymbol * pSymEnum = pSymtabParent->PSymEnsure(pErrman, strIdent, pStnodEnum, FSYM_IsType);
				pSymEnum->m_grfsym.AddFlags(FSYM_IsType);
				pSymEnum->m_pTin = pTinenum;

				pStnodEnum->m_pSym = pSymEnum;
				pStnodEnum->m_pTin = pTinenum;

				return pStnodEnum;
			}
			else if (rword == RWORD_Struct)
			{
				JtokNextToken(pJlex);
				Expect(pParctx, pJlex, JTOK('{'));

				CSTNode * pStnodStruct = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodStruct->m_park = PARK_StructDefinition;
				pStnodStruct->IAppendChild(pStnodIdent);

				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;

				const CString & strIdent = StrFromIdentifier(pStnodIdent);
				SLexerLocation lexlocChild(pJlex);
				CSymbolTable * pSymtabStruct = pParctx->m_pWork->PSymtabNew(strIdent);

				// NOTE: struct symbol tables at the global scope should be unordered.
				if (!pParctx->m_pSymtab->m_grfsymtab.FIsSet(FSYMTAB_Ordered))
					pSymtabStruct->m_grfsymtab.Clear(FSYMTAB_Ordered);

				PushSymbolTable(pParctx, pSymtabStruct, lexlocChild);
				pStnodStruct->m_pSymtab = pSymtabStruct;

				CSTNode * pStnodDeclList = PStnodParseMemberDeclList(pParctx, pJlex);

				CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
				EWC_ASSERT(pSymtabStruct == pSymtabPop, "CSymbol table push/pop mismatch (struct)");

				if (pStnodDeclList)
				{
					pStnodStruct->IAppendChild(pStnodDeclList);
				}

				// type info struct
				int cStnodMember;
				CSTNode ** ppStnodMember = PPStnodChildFromPark(pStnodDeclList, &cStnodMember, PARK_List);

				int cStnodField = 0;
				CSTNode * const * ppStnodMemberMax = &ppStnodMember[cStnodMember];
				for (auto ppStnodMemberIt = ppStnodMember; ppStnodMemberIt != ppStnodMemberMax; ++ppStnodMemberIt)
				{
					switch ((*ppStnodMemberIt)->m_park)
					{
					case PARK_Decl:				++cStnodField; break;
					case PARK_ConstantDecl:		break;
					case PARK_StructDefinition:	break;
					case PARK_EnumDefinition:	break;
					case PARK_Typedef:			break;
					default: EWC_ASSERT(false, "Unexpected member in structure %s", strIdent.PChz());
					}
				}

				size_t cBAlloc = CBAlign(sizeof(STypeInfoStruct), EWC_ALIGN_OF(STypeStructMember)) + 
								cStnodField * sizeof(STypeStructMember);
				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoStruct * pTinstruct = new(pB) STypeInfoStruct(strIdent.PChz());
				pTinstruct->m_pStnodStruct = pStnodStruct;
				STypeStructMember * aTypememb = (STypeStructMember*)PVAlign(
																		pB + sizeof(STypeInfoStruct), 
																		EWC_ALIGN_OF(STypeStructMember));
				pTinstruct->m_aryTypemembField.SetArray(aTypememb, 0, cStnodField);

				for ( ; ppStnodMember != ppStnodMemberMax; ++ppStnodMember)
				{
					CSTNode * pStnodMember = *ppStnodMember;
					if (pStnodMember->m_park != PARK_Decl)
						continue;

					auto pTypememb = pTinstruct->m_aryTypemembField.AppendNew();
					auto pStnodMemberIdent = pStnodMember->PStnodChildSafe(pStnodMember->m_pStdecl->m_iStnodIdentifier);
					pTypememb->m_strName = StrFromIdentifier(pStnodMemberIdent);
					pTypememb->m_pStnod = pStnodMember;
					pTypememb->m_pTin = pStnodMember->m_pTin;
				}

				pParctx->m_pSymtab->AddManagedTin(pTinstruct);

				auto pErrman = pParctx->m_pWork->m_pErrman;
				SSymbol * pSymStruct = pSymtabParent->PSymEnsure(pErrman, strIdent, pStnodStruct, FSYM_IsType);
				pStnodStruct->m_pSym = pSymStruct;
				pSymStruct->m_pTin = pTinstruct;
				pStnodStruct->m_pTin = pTinstruct;

				Expect(pParctx, pJlex, JTOK('}'));

				return pStnodStruct;
			}
			else if (rword == RWORD_Typedef)
			{
				JtokNextToken(pJlex);

				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
				Expect(pParctx, pJlex, JTOK(';'));
				
				CString strIdent = StrFromIdentifier(pStnodIdent);
				if (!pStnodType)
				{
					ParseError(pParctx, pJlex, "missing type value for typedef %s", strIdent.PChz());

					pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
					return nullptr;
				}

				CSTNode * pStnodTypedef = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodTypedef->m_park = PARK_Typedef;

				(void) pStnodTypedef->IAppendChild(pStnodIdent);
				(void) pStnodTypedef->IAppendChild(pStnodType);

				CSymbolTable * pSymtab = pParctx->m_pSymtab;
				auto pErrman = pParctx->m_pWork->m_pErrman;

				auto pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodTypedef);
				pSym->m_pStnodDefinition = pStnodTypedef;
				pStnodTypedef->m_pSym = pSym;

				return pStnodTypedef;
			}

			// constant decl

			auto pStnodInit = PStnodParseExpression(pParctx, pJlex);
			Expect(pParctx, pJlex, JTOK(';'));
			
			CString strIdent = StrFromIdentifier(pStnodIdent);
			if (!pStnodInit)
			{
				ParseError(pParctx, pJlex, "missing constant value for %s", strIdent.PChz());

				pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
				return nullptr;
			}

			CSTNode * pStnodConst = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodConst->m_park = PARK_ConstantDecl;

			CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
			pStnodConst->m_pStdecl = pStdecl;

			pStdecl->m_iStnodIdentifier = pStnodConst->IAppendChild(pStnodIdent);
			pStdecl->m_iStnodInit = pStnodConst->IAppendChild(pStnodInit);

			CSymbolTable * pSymtab = pParctx->m_pSymtab;
			auto pErrman = pParctx->m_pWork->m_pErrman;
			pSymtab->PSymEnsure(pErrman, strIdent, pStnodConst);

			return pStnodConst;
		}
	}
	return nullptr;
}

CSTNode * PStnodParseExpressionStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (FConsumeToken(pJlex, JTOK(';')))
	{
		// return empty statement

		SLexerLocation lexloc(pJlex);
		CSTNode * pStnodEmpty = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnodEmpty->m_jtok = JTOK(pJlex->m_jtok);
		pStnodEmpty->m_park = PARK_Nop;

		return pStnodEmpty;
	}

	CSTNode * pStnod = PStnodParseExpression(pParctx, pJlex);
	if (pStnod)
	{
		Expect(pParctx, pJlex, JTOK(';'));
	}
	return pStnod;
}

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SJaiLexer * pJlex, CSymbolTable * pSymtab)
{
	CSTNode * pStnodList = nullptr;

	if (FConsumeToken(pJlex, JTOK('{')))
	{
		SLexerLocation lexloc(pJlex);

		if (!pSymtab)
		{
			pSymtab = pParctx->m_pWork->PSymtabNew("anon");
		}
		PushSymbolTable(pParctx, pSymtab, lexloc);

		while (pJlex->m_jtok != JTOK('}'))
		{
			CSTNode * pStnod = PStnodParseStatement(pParctx, pJlex);
			if (!pStnod)
				break;

			if (pStnod->m_grfstnod.FIsAnySet(FSTNOD_EntryPoint))
			{
				// Note - move the lexLoc for this entry to be the lexloc for the block so nested
				//  functions appear at the beginning of the containing scope (yuck!)
				pStnod->m_lexloc = lexloc;

				pParctx->m_pWork->AppendEntry(pStnod, pParctx->m_pSymtab);
			}
			else 
			{
				if (pStnodList == nullptr)
				{
					pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodList->m_jtok = JTOK('{');
					pStnodList->m_park = PARK_List;
					pStnodList->m_pSymtab = pSymtab;
				}

				pStnodList->IAppendChild(pStnod);
			}
		}

		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pSymtab == pSymtabPop, "CSymbol table push/pop mismatch (list)");
		Expect(pParctx, pJlex, JTOK('}'));
	}

	return pStnodList;
}

CSTNode * PStnodParseJumpStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{
	RWORD rword = RwordLookup(pJlex);
	switch(rword)
	{
	case RWORD_Continue:
	case RWORD_Break:
		{
			CSTNode * pStnod = PStnodParseReservedWord(pParctx, pJlex, rword);
			Expect(pParctx, pJlex, JTOK(';'));

			return pStnod;
		} break;
	case RWORD_Return:
		{
			CSTNode * pStnodReturn = PStnodParseReservedWord(pParctx, pJlex, rword);
			if (EWC_FVERIFY(pStnodReturn, "error parsing return"))
			{
				CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
				pStnodReturn->IAppendChild(pStnodExp);
			}

			Expect(pParctx, pJlex, JTOK(';'));
			return pStnodReturn;
		} break;
	default:
		return nullptr;
	}
}

CSTNode * PStnodParseSelectionStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{
	RWORD rword = RwordLookup(pJlex);
	if (rword == RWORD_If)
	{
		//if expression statement
		//if expression statement else statement

		CSTNode * pStnodIf = PStnodParseReservedWord(pParctx, pJlex, RWORD_If);
		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
		pStnodIf->IAppendChild(pStnodExp);
		
		CSTNode * pStnodStatement = PStnodParseStatement(pParctx, pJlex);
		if (pStnodStatement->m_grfstnod.FIsSet(FSTNOD_EntryPoint))
		{
			EWC_ASSERT(pStnodStatement->m_park == PARK_ProcedureDefinition, "Unknown entry point park");
			ParseError( pParctx, pJlex, "Local functions not directly allowed under conditional, add {}");
		}
		else
		{
			pStnodIf->IAppendChild(pStnodStatement);
		}

		RWORD rwordElse = RwordLookup(pJlex);
		if (rwordElse == RWORD_Else)
		{
			CSTNode * pStnodElse = PStnodParseReservedWord(pParctx, pJlex, RWORD_Else);

			CSTNode * pStnodStatement = PStnodParseStatement(pParctx, pJlex);
			if (pStnodStatement->m_grfstnod.FIsSet(FSTNOD_EntryPoint))
			{
				EWC_ASSERT(pStnodStatement->m_park == PARK_ProcedureDefinition, "Unknown entry point park");
				ParseError( pParctx, pJlex, "Local functions not directly allowed under conditional, add {}");
			}
			else
			{
				pStnodElse->IAppendChild(pStnodStatement);
				pStnodIf->IAppendChild(pStnodElse);
			}
		}
		return pStnodIf;
	}
	if (rword == RWORD_Switch)
	{
		//switch ( expression ) statement
		EWC_ASSERT(false, "switch statements are not supported yet");
	}
	return nullptr;
}

CSTNode * PStnodParseIterationStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{
	RWORD rword = RwordLookup(pJlex);
	if (rword == RWORD_For)
	{
		JtokNextToken(pJlex);

		//for decl : expression..expression statement
		EWC_ASSERT(false, "for loops statements are not supported yet");
		return nullptr;
	}
	if (rword == RWORD_While)
	{
		CSTNode * pStnodWhile = PStnodParseReservedWord(pParctx, pJlex, RWORD_While);
		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
		pStnodWhile->IAppendChild(pStnodExp);
		
		CSTNode * pStnodStatement = PStnodParseStatement(pParctx, pJlex);
		pStnodWhile->IAppendChild(pStnodStatement);
		return pStnodWhile;
	}
	return nullptr;
}

CSTNode * PStnodParseStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{

	CSTNode * pStnod = PStnodParseCompoundStatement(pParctx, pJlex, nullptr);
	if (pStnod)
		return pStnod;

	// Note - Declarations and definition checks need to come first because they peek ahead to see 
	//  if an identifier has ::, : or :=

	pStnod = PStnodParseDecl(pParctx, pJlex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseDefinition(pParctx, pJlex);
	if (pStnod)
		return pStnod;

	//TODO:labeled-statement

	pStnod = PStnodParseExpressionStatement(pParctx, pJlex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseSelectionStatement(pParctx, pJlex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseIterationStatement(pParctx, pJlex);
	if (pStnod)
		return pStnod;

	return PStnodParseJumpStatement(pParctx, pJlex);
}

bool FParseImportDirectives(CWorkspace * pWork, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK_ReservedWord)
	{
		RWORD rword = RwordLookup(pJlex);

		if ((rword == RWORD_ImportDirective) | (rword == RWORD_ForeignLibraryDirective))
		{
			JtokNextToken(pJlex);
			if (pJlex->m_jtok == JTOK_Literal && pJlex->m_litk == LITK_String)
			{
				if (rword == RWORD_ImportDirective)
				{
					(void) pWork->PFileEnsure(pJlex->m_pChString, CWorkspace::FILEK_Source);
				}
				else if (EWC_FVERIFY(rword == RWORD_ForeignLibraryDirective, "unknown directive"))
				{
					(void) pWork->PFileEnsure(pJlex->m_pChString, CWorkspace::FILEK_Library);
				}
				JtokNextToken(pJlex);
				return true;
			}
			else
			{
				ParseError(
					pWork->m_pParctx,
					pJlex,
					"expected path following %s directive",
					(rword == RWORD_ImportDirective) ? "#import" : "#foreign_library");
			}
		}
	}
	return false;
}

void ParseGlobalScope(CWorkspace * pWork, SJaiLexer * pJlex, bool fAllowIllegalEntries)
{
	CParseContext * pParctx = pWork->m_pParctx;

	// load the first token
	JtokNextToken(pJlex);

	while (pJlex->m_jtok != JTOK_Eof)
	{
		if (FParseImportDirectives(pWork, pJlex))
			continue;

		CSTNode * pStnod = PStnodParseStatement(pWork->m_pParctx, pJlex);

		if (!pStnod)
		{
			ParseError( pParctx, pJlex, "Unexpected token at global scope '%s'", PChzCurrentToken(pJlex));
			break;
		}

		pWork->AppendEntry(pStnod, pParctx->m_pSymtab);
		if (!fAllowIllegalEntries)
		{
			bool fIsDecl = pStnod->m_park == PARK_Decl;
			bool fIsDefinition = (pStnod->m_park == PARK_ProcedureDefinition) | 
								(pStnod->m_park == PARK_EnumDefinition) | 
								(pStnod->m_park == PARK_StructDefinition);
			if (!fIsDecl | fIsDefinition)
			{
				ParseError(
					pParctx,
					pJlex,
					"Unexpected statement at global scope '%s'",
					PChzFromPark(pStnod->m_park));
			}
		}
	}
}


void PushSymbolTable(CParseContext * pParctx, CSymbolTable * pSymtab, const SLexerLocation & lexloc)
{
	pSymtab->m_pSymtabParent = pParctx->m_pSymtab;
	pSymtab->m_lexlocParent = lexloc;
	pParctx->m_pSymtab = pSymtab;
}

CSymbolTable * PSymtabPop(CParseContext * pParctx)
{
	CSymbolTable * pSymtabPrev = pParctx->m_pSymtab;
	if (EWC_FVERIFY(pSymtabPrev, "Pop symbol table underflow"))
	{
		pParctx->m_pSymtab = pSymtabPrev->m_pSymtabParent;
	}
	return pSymtabPrev;
}

CSymbolTable::~CSymbolTable()
{
	CHash<HV, SSymbol *>::CIterator iterPSym(&m_hashHvPSym);
	while (SSymbol ** ppSym = iterPSym.Next())
	{
		SSymbol * pSym = *ppSym;
		while (pSym)
		{
			SSymbol * pSymDelete = pSym;
			pSym = pSym->m_pSymPrev;

			m_pAlloc->EWC_DELETE(pSymDelete);
		}

		*ppSym = nullptr;
	}

	CHash<HV, STypeInfoForwardDecl *>::CIterator iterPTinfwd(&m_hashHvPTinfwd);
	while (STypeInfoForwardDecl ** ppTinfwd = iterPTinfwd.Next())
	{
		EWC_ASSERT((*ppTinfwd)->m_arypTinReferences.C() == 0, "unresolved forward declarations");
	}

	for (STypeInfo ** ppTin = m_arypTinManaged.A(); ppTin != m_arypTinManaged.PMac(); ++ppTin)
	{
		DeleteTypeInfo(m_pAlloc, *ppTin);
		*ppTin = nullptr;
	}
}

void AddSimpleBuiltInType(SErrorManager * pErrman, CSymbolTable * pSymtab, const CString & strName, TINK tink)
{
	STypeInfo * pTin = EWC_NEW(pSymtab->m_pAlloc, STypeInfo) STypeInfo(strName.PChz(), tink);

	pSymtab->AddBuiltInType(pErrman, nullptr, pTin);
}

void AddBuiltInInteger(SErrorManager * pErrman, CSymbolTable * pSymtab, const CString & strName, u32 cBit, bool fSigned)
{
	STypeInfoInteger * pTinint = EWC_NEW(pSymtab->m_pAlloc, STypeInfoInteger) STypeInfoInteger(strName.PChz(), cBit, fSigned);
	pSymtab->AddBuiltInType(pErrman, nullptr, pTinint);
}

void AddBuiltInFloat(SErrorManager * pErrman, CSymbolTable * pSymtab, const CString & strName, u32 cBit)
{
	STypeInfoFloat * pTinfloat = EWC_NEW(pSymtab->m_pAlloc, STypeInfoFloat) STypeInfoFloat(strName.PChz(), cBit);
	pSymtab->AddBuiltInType(nullptr, nullptr, pTinfloat);
}

void AddBuiltInLiteral(SErrorManager * pErrman, CSymbolTable * pSymtab, const CString & strName, LITK litk, s8 cBit, bool fIsSigned)
{
	STypeInfoLiteral * pTinlit = EWC_NEW(pSymtab->m_pAlloc, STypeInfoLiteral) STypeInfoLiteral();
	pTinlit->m_strName = strName;
	pSymtab->AddBuiltInType(nullptr, nullptr, pTinlit);

	pTinlit->m_litty.m_litk = litk;
	pTinlit->m_litty.m_cBit = cBit;
	pTinlit->m_litty.m_fIsSigned = fIsSigned;
	pTinlit->m_fIsFinalized = true;

	EWC::CDynAry<STypeInfoLiteral *> * paryPTinlit = &pSymtab->m_mpLitkArypTinlit[litk];
	if (!paryPTinlit->m_pAlloc)
	{
		paryPTinlit->SetAlloc(pSymtab->m_pAlloc, 8);
	}
	paryPTinlit->Append(pTinlit);
}

void CSymbolTable::AddBuiltInSymbols(SErrorManager * pErrman)
{
	AddSimpleBuiltInType(pErrman, this, "bool", TINK_Bool);
	AddSimpleBuiltInType(pErrman, this, "void", TINK_Void);
	AddSimpleBuiltInType(pErrman, this, "string", TINK_String);

	AddBuiltInInteger(pErrman, this, "u8", 8, false);
	AddBuiltInInteger(pErrman, this, "u16", 16, false);
	AddBuiltInInteger(pErrman, this, "u32", 32, false);
	AddBuiltInInteger(pErrman, this, "uint", 64, false);
	AddBuiltInInteger(pErrman, this, "u64", 64, false);

	AddBuiltInInteger(pErrman, this, "s8", 8, true);
	AddBuiltInInteger(pErrman, this, "s16", 16, true);
	AddBuiltInInteger(pErrman, this, "s32", 32, true);
	AddBuiltInInteger(pErrman, this, "int", 64, true);
	AddBuiltInInteger(pErrman, this, "s64", 64, true);

	AddBuiltInFloat(pErrman, this, "float", 32);
	AddBuiltInFloat(pErrman, this, "f32", 32);
	AddBuiltInFloat(pErrman, this, "double", 64);
	AddBuiltInFloat(pErrman, this, "f64", 64);

	AddBuiltInLiteral(pErrman, this, "__bool_Literal", LITK_Bool, 8, false);
	AddBuiltInLiteral(pErrman, this, "__u8_Literal", LITK_Integer, 8, false);
	AddBuiltInLiteral(pErrman, this, "__u16_Literal", LITK_Integer, 16, false);
	AddBuiltInLiteral(pErrman, this, "__u32_Literal", LITK_Integer, 32, false);
	AddBuiltInLiteral(pErrman, this, "__u64_Literal", LITK_Integer, 64, false);
	AddBuiltInLiteral(pErrman, this, "__s8_Literal", LITK_Integer, 8, true);
	AddBuiltInLiteral(pErrman, this, "__s16_Literal", LITK_Integer, 16, true);
	AddBuiltInLiteral(pErrman, this, "__s32_Literal", LITK_Integer, 32, true);
	AddBuiltInLiteral(pErrman, this, "__s64_Literal", LITK_Integer, 64, true);
	AddBuiltInLiteral(pErrman, this, "__f32_Literal", LITK_Float, 32, true);
	AddBuiltInLiteral(pErrman, this, "__f64_Literal", LITK_Float, 64, true);
	AddBuiltInLiteral(pErrman, this, "__string_Literal", LITK_String, -1, true);
	AddBuiltInLiteral(pErrman, this, "__char_Literal", LITK_Char, 32, true);
	AddBuiltInLiteral(pErrman, this, "__void_Literal", LITK_Null, -1, true);
}

SSymbol * CSymbolTable::PSymEnsure(SErrorManager * pErrman, const CString & strName, CSTNode * pStnodDefinition, GRFSYM grfsym)
{
	SLexerLocation lexloc;
	if (pStnodDefinition)
		lexloc = pStnodDefinition->m_lexloc;

	SSymbol * pSymPrev = nullptr;
	SSymbol * pSym = nullptr;
	SSymbol ** ppSym = m_hashHvPSym.Lookup(strName.Hv()); 
	if (ppSym)
	{
		pSym = *ppSym;
		if (pSym->m_pStnodDefinition != pStnodDefinition)
		{
			pSymPrev = pSym;
			pSym = nullptr;

			// Should shadowing a symbol be an error?
			//EmitError(pErrman, &lexloc, "Shadowing symbol '%s' in unordered symbol table", strName.PChz());
		}
	}
	
	if (!pSym)
	{
		pSym = EWC_NEW(m_pAlloc, SSymbol) SSymbol;
		(void) m_hashHvPSym.FinsEnsureKeyAndValue(strName.Hv(), pSym);
	}

	pSym->m_strName = strName;
	pSym->m_pStnodDefinition = pStnodDefinition;
	pSym->m_grfsym = grfsym;
	pSym->m_pTin = nullptr;
	pSym->m_pVal = nullptr;
	pSym->m_pSymPrev = pSymPrev;

	while (pSymPrev)
	{
		SLexerLocation lexlocPrev = (pSymPrev->m_pStnodDefinition) ? pSymPrev->m_pStnodDefinition->m_lexloc : SLexerLocation();
		EWC_ASSERT(lexlocPrev <= lexloc, "expected previous symbols sorted in reverse lexical order");

		lexloc = lexlocPrev;
		pSymPrev = pSymPrev->m_pSymPrev;
	}

	return pSym;
}

SSymbol * CSymbolTable::PSymLookup(const CString & str, const SLexerLocation & lexloc, GRFSYMLOOK grfsymlook)
{
	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		SSymbol ** ppSym = m_hashHvPSym.Lookup(str.Hv()); 
		if (ppSym)
		{
			bool fIsOrdered = m_grfsymtab.FIsSet(FSYMTAB_Ordered) && !grfsymlook.FIsSet(FSYMLOOK_IgnoreOrder);
			SSymbol * pSym = *ppSym;

			while (pSym)
			{
				SLexerLocation lexlocSym = (pSym->m_pStnodDefinition) ? pSym->m_pStnodDefinition->m_lexloc : SLexerLocation();
				if ((fIsOrdered == false) | (lexlocSym <= lexloc))
				{
					return pSym;
				}
				pSym = pSym->m_pSymPrev;
			}
		}
	}

	CSymbolTable * pSymtab = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? m_pSymtabParent : nullptr;
	SLexerLocation lexlocChild = lexloc;
	while (pSymtab)
	{
		SSymbol ** ppSym = pSymtab->m_hashHvPSym.Lookup(str.Hv()); 

		if (ppSym)
		{
			bool fIsOrdered = pSymtab->m_grfsymtab.FIsSet(FSYMTAB_Ordered);
			SLexerLocation lexlocSym = ((*ppSym)->m_pStnodDefinition) ? (*ppSym)->m_pStnodDefinition->m_lexloc : SLexerLocation();
			if ((fIsOrdered == false) | (lexlocSym <= lexlocChild))
			{
				return *ppSym;
			}
		}

		pSymtab = pSymtab->m_pSymtabParent;
	}
	return nullptr; 
}

STypeInfoLiteral * CSymbolTable::PTinlitFromLitk(LITK litk)
{
	if (!FIsValid(litk))
		return nullptr;

	// built in types are always in the root symbol table 
	CSymbolTable * pSymtab = this;
	while (pSymtab->m_pSymtabParent)
		pSymtab = pSymtab->m_pSymtabParent;

	const EWC::CDynAry<STypeInfoLiteral *> & mpLitkPTinlit = pSymtab->m_mpLitkArypTinlit[litk];
	if (mpLitkPTinlit.C() == 0)
		return nullptr;

	return mpLitkPTinlit[0];
}

STypeInfoLiteral * CSymbolTable::PTinlitFromLitk(LITK litk, int cBit, bool fIsSigned)
{
	if (FIsValid(litk))
	{
		// built in types are always in the root symbol table 
		CSymbolTable * pSymtab = this;
		while (pSymtab->m_pSymtabParent)
			pSymtab = pSymtab->m_pSymtabParent;

		EWC::CDynAry<STypeInfoLiteral *> & mpLitkPTinlit = pSymtab->m_mpLitkArypTinlit[litk];
		STypeInfoLiteral ** ppTinlitMax = mpLitkPTinlit.PMac();
		for (STypeInfoLiteral ** ppTinlit = mpLitkPTinlit.A(); ppTinlit != ppTinlitMax; ++ppTinlit)
		{
			STypeInfoLiteral * pTinlit = *ppTinlit;
			if ((pTinlit->m_litty.m_cBit == cBit) & (pTinlit->m_litty.m_fIsSigned == fIsSigned))
				return pTinlit;
		}
	}

	return nullptr;
}

STypeInfo *	CSymbolTable::PTinBuiltin(const EWC::CString & str)
{
	SLexerLocation lexloc;
	auto pSym = PSymLookup(str, lexloc);
	if (pSym)
		return pSym->m_pTin;
	return nullptr;
}

STypeInfoPointer * CSymbolTable::PTinptrGetReference(STypeInfo * pTinPointedTo)
{
	// Note: I should unique'ify these

	STypeInfoPointer * pTinptr = EWC_NEW(m_pAlloc, STypeInfoPointer) STypeInfoPointer();
	AddManagedTin(pTinptr);

	pTinptr->m_pTinPointedTo = pTinPointedTo;
	return pTinptr;
}

void CSymbolTable::AddManagedTin(STypeInfo * pTin)
{
	m_arypTinManaged.Append(pTin);
}

void CSymbolTable::AddManagedSymtab(CSymbolTable * pSymtab)
{
	EWC_ASSERT(pSymtab->m_pSymtabNextManaged == nullptr, "trying to add managed symtab");

	pSymtab->m_pSymtabNextManaged = m_pSymtabNextManaged;
	m_pSymtabNextManaged = pSymtab;
}

void CSymbolTable::AddBuiltInType(SErrorManager * pErrman, SJaiLexer * pJlex, STypeInfo * pTin)
{
	// NOTE: This function is for built-in types without a lexical order, so we shouldn't be calling it on an ordered table
	EWC_ASSERT(!m_grfsymtab.FIsSet(FSYMTAB_Ordered), "Cannot add built-in types to ordered symbol table.");

	m_arypTinManaged.Append(pTin);
	const CString & strName = pTin->m_strName;
	if (!EWC_FVERIFY(!strName.FIsEmpty(), "registering unnamed type"))
	{
		return;
	}

	STypeInfo ** ppTinValue = nullptr;
	FINS fins = m_hashHvPTinBuiltIn.FinsEnsureKey(strName.Hv(), &ppTinValue);
	if (fins == FINS_Inserted)
	{
		*ppTinValue = pTin;

		auto pSym = PSymEnsure(pErrman, strName, nullptr, FSYM_IsBuiltIn | FSYM_IsType);
		pSym->m_pTin = pTin;
	}
	else
	{
		SLexerLocation lexloc = (pJlex) ? SLexerLocation(pJlex) : SLexerLocation();
		EmitError(pErrman, &lexloc, "Two types encountered with same name (%s)", strName.PChz());
	}
}

void CSymbolTable::PrintDump()
{
	printf("Symbols:\n");
	EWC::CHash<HV, SSymbol *>::CIterator iter(&m_hashHvPSym);
	while (SSymbol ** ppSym = iter.Next())
	{
		SSymbol * pSym = *ppSym;
		printf("%p: %s : '%s'\n",pSym, pSym->m_strName.PChz(), (pSym->m_pTin) ? pSym->m_pTin->m_strName.PChz() : "Nill");
	}

	printf("\n");
	if (m_pSymtabParent)
	{
		printf("parent: \n");
		m_pSymtabParent->PrintDump();
	}

	printf("\n");
}



// TypeInfo routines
void DeleteTypeInfo(CAlloc * pAlloc, STypeInfo * pTin)
{
	pAlloc->EWC_DELETE(pTin);
}



// Syntax Tree Nodes

CSTNode::CSTNode(CAlloc * pAlloc, const SLexerLocation & lexLoc)
:m_jtok(JTOK_Nil)
,m_park(PARK_Nil)
,m_strees(STREES_Parsed)
,m_grfstnod()
,m_pStval(nullptr)
,m_pStident(nullptr)
,m_pStdecl(nullptr)
,m_pStproc(nullptr)
,m_pStenum(nullptr)
,m_lexloc(lexLoc)
,m_pTin(nullptr)
,m_pTinOperand(nullptr)
,m_pSymtab(nullptr)
,m_pSym(nullptr)
,m_arypStnodChild(pAlloc)
{
}

CSTNode::~CSTNode()
{
	CAlloc * pAlloc = m_arypStnodChild.m_pAlloc;
	EWC_ASSERT(pAlloc, "missing allocator!");

	for (size_t ipStnod = m_arypStnodChild.C(); ipStnod > 0; --ipStnod)
	{
		pAlloc->EWC_DELETE(m_arypStnodChild[ipStnod-1]);
	}
	m_arypStnodChild.Clear();

	if (m_pStval)
	{
		pAlloc->EWC_DELETE(m_pStval);
		m_pStval = nullptr;
	}

	if (m_pStident)
	{
		pAlloc->EWC_DELETE(m_pStident);
		m_pStident = nullptr;
	}

	if (m_pStdecl)
	{
		pAlloc->EWC_DELETE(m_pStdecl);
		m_pStdecl = nullptr;
	}

	if (m_pStproc)
	{
		pAlloc->EWC_DELETE(m_pStproc);
		m_pStproc = nullptr;
	}

	if (m_pStenum)
	{
		pAlloc->EWC_DELETE(m_pStenum);
		m_pStenum = nullptr;
	}

	m_pTin = nullptr;
}

void CSTNode::ReplaceChild(CSTNode * pStnodOld, CSTNode * pStnodNew)
{
	// NOTE - this doesn't do anything smart to handle an abandoned child, hopefully you're reparenting or deleting it.
	for (size_t ipStnod = 0; ipStnod < m_arypStnodChild.C(); ++ipStnod)
	{
		CSTNode * pStnodChild = m_arypStnodChild[ipStnod];
		if (pStnodChild == pStnodOld)
		{
			m_arypStnodChild[ipStnod] = pStnodNew;
			return;
		}
	}

	EWC_ASSERT(false, "failed to find child during ReplaceChild");
}


size_t CChPrintTypeInfo(STypeInfo * pTin, PARK park, char * pCh, char * pChEnd)
{
	if (pTin == nullptr)
	{
		switch (park)
		{
		case PARK_List:				return CChCopy("{}", pCh, pChEnd-pCh);
		case PARK_Identifier:		return CChCopy("Ident", pCh, pChEnd-pCh);
		case PARK_ParameterList:	return CChCopy("Params", pCh, pChEnd-pCh);
		case PARK_VariadicArg:		return CChCopy("..",pCh, pChEnd-pCh);
		case PARK_Nop:				return CChCopy("Nop",pCh, pChEnd-pCh);
		case PARK_Uninitializer:	return CChCopy("---",pCh, pChEnd-pCh);
		default:					return CChCopy("???", pCh, pChEnd-pCh);
		}
	}

	switch (pTin->m_tink)
	{
	case TINK_Pointer:		
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer*)pTin;
			char * pChWork = pCh;
			pChWork += CChCopy(PChzFromJtok(JTOK_Reference), pChWork, pChEnd-pChWork);
			pChWork += CChPrintTypeInfo(pTinptr->m_pTinPointedTo, park, pChWork, pChEnd);
			return pChWork - pCh;
		}break;
	case TINK_Array:
		{
			STypeInfoArray * pTinary = (STypeInfoArray*)pTin;
			char * pChWork = pCh;

			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
				pChWork += CChFormat(pChWork, pChEnd-pChWork, "[%d]", pTinary->m_c);
				break;
			case ARYK_Dynamic:
				pChWork += CChCopy("[..]", pChWork, pChEnd-pChWork);
				break;
			case ARYK_Reference:
				pChWork += CChCopy("[]", pChWork, pChEnd-pChWork);
				break;
			}

			pChWork += CChPrintTypeInfo(pTinary->m_pTin, park, pChWork, pChEnd);
			return pChWork - pCh;
		}break;

	case TINK_Literal:		
		{
			STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTin;
			char * pChWork = pCh;
			const SLiteralType & litty = pTinlit->m_litty;

			pChWork += CChCopy("Literal", pChWork, pChEnd - pChWork);
			return pChWork - pCh;
		}
    case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;
			return CChFormat(pCh, pChEnd-pCh, "%s()", pTin->m_strName.PChz());
		}break;
    case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;
			return CChFormat(pCh, pChEnd-pCh, "%s_struct", pTin->m_strName.PChz());
		}break;
    case TINK_Enum:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;
			return CChFormat(pCh, pChEnd-pCh, "%s_enum", pTin->m_strName.PChz());
		}break;
	case TINK_Integer:		// fall through ...
    case TINK_Float:		// fall through ...
    case TINK_Bool:			// fall through ...
    case TINK_String:		// fall through ...
    case TINK_Void:			// fall through ...
    case TINK_Null:			// fall through ...
    case TINK_Any:			// fall through ...
	default:
		return CChFormat(pCh, pChEnd-pCh, "%s", pTin->m_strName.PChz());
	}
}

size_t CChPrintStnodName(CSTNode * pStnod, char * pCh, char * pChEnd)
{
	switch (pStnod->m_park)
	{
	case PARK_Identifier:			return CChFormat(pCh, pChEnd-pCh, "$%s", StrFromIdentifier(pStnod).PChz());
	case PARK_ReservedWord:			return CChCopy(PChzFromRword(pStnod->m_pStval->m_rword), pCh, pChEnd - pCh); 
	case PARK_Nop:					return CChCopy("nop", pCh, pChEnd - pCh); 
	case PARK_Literal:				
		{
			switch (pStnod->m_pStval->m_stvalk)
			{
			case STVALK_String:			return CChFormat(pCh, pChEnd - pCh, "\"%s\"", pStnod->m_pStval->m_str.PChz());
			case STVALK_UnsignedInt:	return CChFormat(pCh, pChEnd - pCh, "%llu", pStnod->m_pStval->m_nUnsigned);
			case STVALK_SignedInt:		return CChFormat(pCh, pChEnd - pCh, "%lld", pStnod->m_pStval->m_nSigned);
			case STVALK_Float:			return CChFormat(pCh, pChEnd - pCh, "%f", pStnod->m_pStval->m_g);
			default:
				EWC_ASSERT(false, "unknown literal %s", PChzFromJtok(pStnod->m_jtok));
				return 0;
			}
		}
	case PARK_AdditiveOp:		    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_MultiplicativeOp:	    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_ShiftOp:			    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_EqualityOp:		    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_RelationalOp:		    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_BitwiseAndOrOp:	    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_LogicalAndOrOp:	    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_UnaryOp:			    return CChFormat(pCh, pChEnd-pCh, "unary[%s]", PChzFromJtok(pStnod->m_jtok));
	case PARK_PostfixUnaryOp:		return CChFormat(pCh, pChEnd-pCh, "postUnary[%s]", PChzFromJtok(pStnod->m_jtok));
	case PARK_AssignmentOp:		    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_ArrayElement:		    return CChCopy("elem", pCh, pChEnd - pCh);
	case PARK_MemberLookup:		    return CChCopy("member", pCh, pChEnd - pCh);
	case PARK_ProcedureCall:		return CChCopy("procCall", pCh, pChEnd - pCh);
	case PARK_List:				    return CChCopy("{}", pCh, pChEnd - pCh);
	case PARK_ParameterList:	    return CChCopy("params", pCh, pChEnd - pCh);
	case PARK_ArrayDecl:		    return CChCopy("[]", pCh, pChEnd - pCh);
	case PARK_If:				    return CChCopy("if", pCh, pChEnd - pCh);
	case PARK_Else:				    return CChCopy("else", pCh, pChEnd - pCh);
	case PARK_ReferenceDecl:		return CChCopy("ptr", pCh, pChEnd - pCh);
	case PARK_Decl:					return CChCopy("decl", pCh, pChEnd - pCh);
	case PARK_Typedef:				return CChCopy("typedef", pCh, pChEnd - pCh);
	case PARK_ConstantDecl:			return CChCopy("const", pCh, pChEnd - pCh);
	case PARK_ProcedureDefinition:	return CChCopy("func", pCh, pChEnd - pCh);
	case PARK_EnumDefinition:		return CChCopy("enum", pCh, pChEnd - pCh);
	case PARK_StructDefinition:		return CChCopy("struct", pCh, pChEnd - pCh);
	case PARK_EnumConstant:			return CChCopy("enumConst", pCh, pChEnd - pCh);
	case PARK_VariadicArg:			return CChCopy("..", pCh, pChEnd - pCh);
	case PARK_ArrayLiteral:			return CChCopy("arrayLit", pCh, pChEnd - pCh);
	case PARK_Cast:					return CChCopy("cast", pCh, pChEnd - pCh);
	case PARK_Error:
	default:						return CChCopy("error", pCh, pChEnd-pCh);
	}
}

size_t CChPrintStnod(CSTNode * pStnod, char * pCh, char * pChEnd, GRFDBGSTR grfdbgstr)
{
	char * pChWork = pCh;
	if (grfdbgstr.FIsSet(FDBGSTR_Name))
	{
		pChWork += CChPrintStnodName(pStnod, pChWork, pChEnd);
		grfdbgstr.Clear(FDBGSTR_Name);

		if (pChEnd - pChWork > 1 && grfdbgstr != FDBGSTR_None)
		{
			*pChWork = '|';
			++pChWork;
			*pChWork = '\0';
		}
	}

	if (grfdbgstr.FIsSet(FDBGSTR_Type))
	{
		if (pStnod->m_park == PARK_Identifier && pStnod->m_pTin == nullptr)
		{
			pChWork += CChPrintStnodName(pStnod, pChWork, pChEnd);
		}
		else
		{
			pChWork += CChPrintTypeInfo(pStnod->m_pTin, pStnod->m_park, pChWork, pChEnd);
		}
		grfdbgstr.Clear(FDBGSTR_Type);
	}

	if (grfdbgstr.FIsSet(FDBGSTR_LiteralSize))
	{
		if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		{
			const SLiteralType & litty = ((STypeInfoLiteral *)pStnod->m_pTin)->m_litty;

			pChWork += CChFormat(pChWork, pChEnd - pChWork, ":%s", PChzFromLitk(litty.m_litk));
			if (litty.m_cBit >= 0)
			{
				pChWork += CChFormat(pChWork, pChEnd - pChWork, "%d", litty.m_cBit);
			}
		}
		grfdbgstr.Clear(FDBGSTR_LiteralSize);
	}
	return pChWork - pCh;
}

size_t CSTNode::CChWriteDebugString(char * pCh, char * pChEnd, GRFDBGSTR grfdbgstr)
{

	if (m_park == PARK_Literal)
	{
		EWC_ASSERT(m_pStval, "operand without value struct");
		return CChPrintStnod(this, pCh, pChEnd, grfdbgstr);
	}
	if (m_park == PARK_Identifier)
	{
		EWC_ASSERT(m_pStident, "identifier operand without string struct");
		return CChPrintStnod(this, pCh, pChEnd, grfdbgstr);
	}

	size_t cChMax = pChEnd - pCh;
	size_t cCh = CChCopy("(", pCh, cChMax);
	cCh += CChPrintStnod(this, &pCh[cCh], pChEnd, grfdbgstr);

	for (size_t ipStnod = 0; ipStnod < m_arypStnodChild.C(); ++ipStnod)
	{
		if (EWC_FVERIFY(pChEnd - &pCh[cCh] >= 1, "debug string overflow"))
		{
			pCh[cCh++] = ' ';
		}
		if (EWC_FVERIFY(cCh < cChMax, "debug string storage overflow"))
		{
			CSTNode * pStnod = m_arypStnodChild[ipStnod];
			cCh += pStnod->CChWriteDebugString(&pCh[cCh], pChEnd, grfdbgstr);
		}
	}

	if (EWC_FVERIFY(pChEnd - &pCh[cCh] >= 2, "debug string overflow"))
	{
		pCh[cCh++] = ')';
		pCh[cCh] = 0;
	}
	return cCh;
}

void CChWriteDebugStringForEntries(CWorkspace * pWork, char * pCh, char * pChMax, GRFDBGSTR grfdbgstr)
{
	for (size_t ipStnod = 0; ipStnod < pWork->m_aryEntry.C(); ++ipStnod)
	{
		CSTNode * pStnod = pWork->m_aryEntry[ipStnod].m_pStnod;
		pCh += pStnod->CChWriteDebugString(pCh, pChMax, grfdbgstr);

		if ((pCh != pChMax) & (ipStnod+1 != pWork->m_aryEntry.C()))
		{
			*pCh++ = ' ';
		}
	}

	if (pCh != pChMax)
	{
		*pCh = '\0';
	}
}

CString StrFromIdentifier(CSTNode * pStnod)
{
	if (pStnod &&
		EWC_FVERIFY(pStnod->m_park == PARK_Identifier, "expected identifier") && 
		EWC_FVERIFY(pStnod->m_pStident, "identifier encountered without string value"))
	{
		return pStnod->m_pStident->m_str;
	}

	return CString();
}

void AssertParseMatchTailRecurse(
	CWorkspace * pWork,
	const char * pChzIn,
	const char * pChzOut,
	const char * apChzExpectedImport[] = nullptr,
	const char * apChzExpectedLibrary[] = nullptr)
{

#ifdef EWC_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	pWork->m_pAlloc->SetAltrac(pAltrac);
#endif

	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pChzIn);

	EWC_ASSERT(pWork->m_pErrman->m_cError == 0, "parse errors detected");
	pWork->m_pErrman->Clear();

	ParseGlobalScope(pWork, &jlex, true);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Name);

	EWC_ASSERT(FAreSame(aCh, pChzOut), "parse debug string doesn't match expected value");

	if (apChzExpectedImport)
	{
		int ipChz;
		for (ipChz = 0; ; ++ipChz)
		{
			const char * pChz= apChzExpectedImport[ipChz];
			if (!pChz)
				break;

			HV hvImport = HvFromPChz(pChz);
			EWC_ASSERT(pWork->PFileLookup(hvImport, CWorkspace::FILEK_Source), "expected import %s", pChz);
		}
		EWC_ASSERT(pWork->CFile(CWorkspace::FILEK_Source), "missing import");
	}
	
	if (apChzExpectedLibrary)
	{
		int ipChz;
		for (ipChz = 0; ; ++ipChz)
		{
			const char * pChz= apChzExpectedLibrary[ipChz];
			if (!pChz)
				break;

			HV hvImport = HvFromPChz(pChz);
			EWC_ASSERT(pWork->PFileLookup(hvImport, CWorkspace::FILEK_Library), "expected import %s", pChz);
		}
		EWC_ASSERT(pWork->CFile(CWorkspace::FILEK_Library) == ipChz, "missing import");
	}

	EndParse(pWork, &jlex);
	EndWorkspace(pWork);

#ifdef EWC_TRACK_ALLOCATION
	DeleteAltrac(&allocAltrac, pAltrac);
	pWork->m_pAlloc->SetAltrac(nullptr);
#endif
}

void TestParse()
{
	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	const char * pChzIn;
	const char * pChzOut;
	{
		SErrorManager errman;
		CWorkspace work(&alloc, &errman);

		pChzIn = "pN = cast(& int) pG;";
		pChzOut = "(= $pN (cast (ptr $int) $pG))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "aN := {:int: 2, 4, 5}; ";
		pChzOut = "(decl $aN (arrayLit $int ({} 2 4 5)))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ ENUMK :: enum int { ENUMK_Nil : -1, ENUMK_Foo, ENUMK_Bah : 3 } enumk := ENUMK.Foo; }";
		pChzOut = "({} (enum $ENUMK $int ({} (enumConst $nil) (enumConst $min) (enumConst $last) (enumConst $max) (arrayLit $names) (arrayLit $values) (enumConst $ENUMK_Nil (unary[-] 1)) (enumConst $ENUMK_Foo) (enumConst $ENUMK_Bah 3)))"
			" (decl $enumk (member $ENUMK $Foo)))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "SOut :: struct { SIn :: struct { ConstTwo :: 2; }} n := SOut.SIn.ConstTwo; ";
		pChzOut = "(struct $SOut ({} (struct $SIn ({} (const $ConstTwo 2))))) (decl $n (member (member $SOut $sIn) $ConstTwo))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "PtrType :: typedef & s16; ArrayType :: typedef [2] s8; ";
		pChzOut = "(typedef $PtrType (ptr $s16)) (typedef $ArrayType ([] 2 $s8))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "SomeConst :: 0xFF; ";
		pChzOut = "(const $SomeConst 255)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "SomeConst : s16 : 0xFF; ";
		pChzOut = "(const $SomeConst $s16 255)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "SFoo :: struct { m_n := 2; } foo : SFoo; foo.m_n = 1; ";
		pChzOut = "(struct $SFoo ({} (decl $m_n 2))) (decl $foo $SFoo) (= (member $foo $m_n) 1)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "paN : & [4] int;";
		pChzOut = "(decl $paN (ptr ([] 4 $int)))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "aN : [4] int; n := aN[0];";
		pChzOut = "(decl $aN ([] 4 $int)) (decl $n (elem $aN 0))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "x + 3*5;";
		pChzOut = "(+ $x (* 3 5))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "(ugh + foo) / ((x + 3)*5);";
		pChzOut = "(/ (+ $ugh $foo) (* (+ $x 3) 5))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "ugh/foo/guh/ack;";
		pChzOut = "(/ (/ (/ $ugh $foo) $guh) $ack)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "(5 + -x) * -(3 / foo);";
		pChzOut = "(* (+ 5 (unary[-] $x)) (unary[-] (/ 3 $foo)))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "ick * ack * -(3 / foo);";
		pChzOut = "(* (* $ick $ack) (unary[-] (/ 3 $foo)))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "ick & (ack&&foo | 123) || guh;";
		pChzOut = "(|| (& $ick (&& $ack (| $foo 123))) $guh)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "ick == ack < foo\n != 123 >= guh;";
		pChzOut = "(!= (== $ick (< $ack $foo)) (>= 123 $guh))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		// NOTE - weird ordering shouldn't matter as we will ensure lhs is l-value
		pChzIn = "ick = 5 += foo *= guh;";
		pChzOut = "(*= (+= (= $ick 5) $foo) $guh)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "++foo.bah[23];";
		pChzOut = "(unary[++] (elem (member $foo $bah) 23))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ i=5; foo.bah = ack; }";
		pChzOut = "({} (= $i 5) (= (member $foo $bah) $ack))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ if i==foo.bar.fug ick = 3; else ick = 7; }";
		pChzOut = "({} (if (== $i (member (member $foo $bar) $fug)) (= $ick 3) (else (= $ick 7))))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ while x > 0 { --x; } }";
		pChzOut = "({} (while (> $x 0) ({} (unary[--] $x))))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ break; continue; return foo=\"test\"; }";
		pChzOut = "({} (break) (continue) (return (= $foo \"test\")))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "VarArgs :: (a : int, ..) #foreign;";
		pChzOut = "(func $VarArgs (params (decl $a $int) (..)) $void)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ AddNums :: (a : int, b := 1) -> int { return a + b; } bah := 3; }";
		pChzOut = "(func $AddNums (params (decl $a $int) (decl $b 1)) $int ({} (return (+ $a $b))))"
			" ({} (decl $bah 3))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "AddLocal :: (nA : int) -> int { nLocal := 2; return nA + nLocal; }";
		pChzOut = "(func $AddLocal (params (decl $nA $int)) $int ({} (decl $nLocal 2) (return (+ $nA $nLocal))))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ AddNums :: (a : int, b := 1) -> int { return a + b; } AddNums(2, 3); }";
		pChzOut = "(func $AddNums (params (decl $a $int) (decl $b 1)) $int ({} (return (+ $a $b))))"
			" ({} (procCall $AddNums 2 3))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ FooFunc(); n:=BarFunc(x+(ack)); }";
		pChzOut = "({} (procCall $FooFunc) (decl $n (procCall $barFunc (+ $x $ack))))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "{ NopFunc :: () { guh := 2; } wha : & int; }";
		pChzOut = "(func $NopFunc $void ({} (decl $guh 2) (return))) ({} (decl $wha (ptr $int)))";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "STest :: struct { m_a := 2; m_b : int; } boo : int = 3;";
		pChzOut = "(struct $STest ({} (decl $m_a 2) (decl $m_b $int))) (decl $boo $int 3)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "pChz := foo; guh : gur = 5; bah : s32 = woo;";
		pChzOut = "(decl $pChz $foo) (decl $guh $gur 5) (decl $bah $s32 $woo)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);

		pChzIn = "ForeignFunc :: () -> int #foreign;";
		pChzOut = "(func $ForeignFunc $int)";
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut);
			
		pChzIn = "#import \"foo/blah/ack\" #foreign_library \"foo/blah/ack\" "
				"#import \"test\\wha\\huh\" #foreign_library \"test\\wha\\huh\" "
				"#import \"basic\" ";

		pChzOut = "";
		const char * apChzExpectedImport[] = { "foo/blah/ack", "test\\wha\\huh", "basic",nullptr };
		const char * apChzExpectedLibrary[] = { "foo/blah/ack", "test\\wha\\huh", nullptr };
		AssertParseMatchTailRecurse(&work, pChzIn, pChzOut, apChzExpectedImport, apChzExpectedLibrary);

		StaticShutdownStrings(&allocString);
	}
}