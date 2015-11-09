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

namespace EWC
{

CSTNode * PStnodParseExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseLogicalAndOrExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseStatement(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseDefinition(CParseContext * pParctx, SJaiLexer * pJlex);

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
		"Uninitializer",

		"Array Element",		// [array, index]
		"Member Lookup",		// [struct, child]
		"Argument Call",		// [procedure, arg0, arg1, ...]
		"List",
		"Parameter List",
		"Array Decl",
		"If",
		"Else",
		"Reference",
		"Decl",
		"Procedure Definition",
		"Enum Definition",
		"Struct Definition",
		"Enum Constant",
	};
	EWC_CASSERT(EWC_DIM(s_mpParkPChz) == PARK_Max, "missing PARK string");
	if (park == PARK_Nil)
		return "Nil";

	if ((park < PARK_Nil) | (park >= PARK_Max))
		return "Unknown PARK";

	return s_mpParkPChz[park];
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
	};
	EWC_CASSERT(EWC_DIM(s_mpLitkPChz) == LITK_Max, "missing LITK string");
	if (litk == LITK_Nil)
		return "Nil";

	if ((litk < LITK_Nil) | (litk >= LITK_Max))
		return "Unknown LITK";

	return s_mpLitkPChz[litk];
}

void ParseError(CParseContext * pParctx, SJaiLexer * pJlex, const char * pChz, ...)
{
	printf("%s(%d) error:", pJlex->m_pChzFilename, NLine(pJlex));
	++pParctx->m_cError;
	
	if (pChz)
	{
		va_list ap;
		va_start(ap, pChz);
		vprintf(pChz, ap);
		printf("\n");
	}
}

void Expect(CParseContext * pParctx, SJaiLexer * pJlex, JTOK jtokExpected)
{
	if (pJlex->m_jtok != jtokExpected)
	{
		ParseError(
			pParctx,
			pJlex,
			"Expected '%s' before %s",
			PChzFromJtok(jtokExpected),
			PChzFromJtok(JTOK(pJlex->m_jtok)));
	}

	JtokNextToken(pJlex);
};

CSTNode * PStnodParseIdentifier(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok != JTOK_Identifier)
		return nullptr;

	CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

	pStnod->m_jtok = JTOK(pJlex->m_jtok);
	pStnod->m_park = PARK_Identifier;

	CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
	pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
	pStval->m_g = pJlex->m_g;
	pStval->m_n = pJlex->m_n;
	pStval->m_rword = RWORD_Nil;
	pStnod->m_pStval = pStval;

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
		ParseError(pParctx, pJlex, "Expected %s before %s", PChzFromRword(rwordExpected), PChzFromJtok(JTOK(pJlex->m_jtok)));
		return nullptr;
	}

	CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

	pStnod->m_jtok = JTOK(pJlex->m_jtok);
	pStnod->m_park = PARK_ReservedWord;

	CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
	pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
	pStval->m_g = pJlex->m_g;
	pStval->m_n = pJlex->m_n;
	pStval->m_rword = rwordLookup;
	pStnod->m_pStval = pStval;

	JtokNextToken(pJlex);
	return pStnod;
}

STypeInfo * PTinForFloat(CSymbolTable * pSymtab, F64 gMin, F64 gLast)
{
	return nullptr;
}

STypeInfo * PTinForInt(CSymbolTable * pSymtab, U64 uMin, U64 nLast)
{
	return nullptr;
}

CSTNode * PStnodParsePrimaryExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	switch(pJlex->m_jtok)
	{
		case JTOK_Identifier:
			{
				CSTNode * pStnod = PStnodParseIdentifier(pParctx, pJlex);

				if (EWC_FVERIFY(pStnod && pStnod->m_pStval, "failed to parse identifier in expression"))
				{
					// BB - need to look in member symbol table if it exists
					const CString & str = pStnod->m_pStval->m_str;
					SSymbol * pSym = pParctx->m_pSymtab->PSymLookup(str);
					if (pSym && pSym->m_pStnodDefinition)
					{
						pStnod->m_pTin = pSym->m_pStnodDefinition->m_pTin;
					}
					else
					{
						pParctx->m_pSymtab->AddUnknownSymbolReference(str, pStnod);
					}
				}
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
						CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

						pStnod->m_jtok = JTOK(pJlex->m_jtok);
						pStnod->m_park = PARK_Literal;

						CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
						pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
						pStval->m_g = pJlex->m_g;
						pStval->m_n = (rword == RWORD_True) ? 1 : 0;
						pStval->m_rword = rword;
						pStnod->m_pTin = nullptr;

						pStval->m_litty.m_litk = (rword == RWORD_Null) ? LITK_Null : LITK_Bool;
						pStnod->m_pStval = pStval;

						JtokNextToken(pJlex);
						return pStnod;
					}
				}
				return nullptr;
			}
		case JTOK_Literal:
			{
				// NOTE - doesn't treat negative constants as literals, need to support constant folding for this to
				//  happen. I could hack it, but would need to handle cases like -(1) 

				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

				pStnod->m_jtok = JTOK(pJlex->m_jtok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_str = CString(pJlex->m_pChString, pJlex->m_cChString);
				pStval->m_g = pJlex->m_g;
				pStval->m_n = pJlex->m_n;
				pStval->m_rword = RWORD_Nil;
				pStval->m_litty = pJlex->m_litty;

				pStnod->m_pStval = pStval;

				JtokNextToken(pJlex);
				return pStnod;
			} 
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
				JtokNextToken(pJlex); // consume '('

				CSTNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodArray->m_jtok = JTOK(pJlex->m_jtok);
				pStnodArray->m_park = PARK_ArrayElement;
				pStnodArray->IAppendChild(pStnod);

				CSTNode * pStnodElement = PStnodParseExpression(pParctx, pJlex);
				pStnodArray->IAppendChild(pStnodElement);

				pStnod = pStnodArray;
				Expect(pParctx, pJlex, JTOK(']'));
			}break;
		case JTOK('('):		// ( )
			{				// ( ArgumentExpressionList )
				JtokNextToken(pJlex); // consume '('

				CSTNode * pStnodArgList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodArgList->m_jtok = JTOK(pJlex->m_jtok);
				pStnodArgList->m_park = PARK_ArgumentCall;
				pStnodArgList->IAppendChild(pStnod);

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

				Expect(pParctx, pJlex, JTOK(')'));
			}break;
		case JTOK('.'):		// . identifier
			{
				JtokNextToken(pJlex); // consume '.'

				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
				if (!pStnodIdent)
				{
					ParseError(pParctx, pJlex, "Expected identifier after '.' before %s", PChzFromJtok(jtokPrev));
				}
				else
				{
					CSTNode * pStnodMember = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
					pStnodMember->m_jtok = jtokPrev;
					pStnodMember->m_park = PARK_MemberLookup;
					pStnodMember->IAppendChild(pStnod);
					pStnodMember->IAppendChild(pStnodIdent);
					pStnod = pStnodMember;
				}
			}break;
		// not supporting post increment
		// not supporting post decrement
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseUnaryExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	switch(pJlex->m_jtok)
	{
	case JTOK('&'):
	case JTOK('*'):
	case JTOK('+'):
	case JTOK('-'):
	case JTOK('~'):
	case JTOK('!'):
	case JTOK_PlusPlus:
	case JTOK_MinusMinus:
		{
			JTOK jtokPrev = JTOK(pJlex->m_jtok);	
			JtokNextToken(pJlex); // consume unary operator

			CSTNode * pStnodExp = PStnodParsePostfixExpression(pParctx, pJlex);
			if (!pStnodExp)
			{
				ParseError(
					pParctx,
					pJlex,
					"Unary operator '%s' missing operand before %s",
					PChzFromJtok(jtokPrev),
					PChzFromJtok(JTOK(pJlex->m_jtok)));
				return nullptr;
			}

			CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
			pStnodUnary->m_jtok = jtokPrev;
			pStnodUnary->m_park = PARK_UnaryOp;
			pStnodUnary->IAppendChild(pStnodExp);

			return pStnodUnary;
		}
	default: return PStnodParsePostfixExpression(pParctx, pJlex);
	}
}

CSTNode * PStnodHandleExpressionRHS(
	CParseContext * pParctx,
	SJaiLexer * pJlex,
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
			PChzFromJtok(JTOK(pJlex->m_jtok)));
		return pStnodLhs;
	}

	CSTNode * pStnodExp = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
	pStnodExp->m_jtok = jtokExpression;
	pStnodExp->m_park = parkExpression;
	pStnodExp->IAppendChild(pStnodLhs);
	pStnodExp->IAppendChild(pStnodRhs);
	return pStnodExp;
}

CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnod = PStnodParseUnaryExpression(pParctx, pJlex);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseUnaryExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_MultiplicativeOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseMultiplicativeExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_AdditiveOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseAdditiveExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_ShiftOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseShiftExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_RelationalOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseRelationalExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_EqualityOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseEqualityExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_BitwiseAndOrOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseBitwiseAndOrExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
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
				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				JtokNextToken(pJlex); // consume operator

				CSTNode * pStnodExp = PStnodParseLogicalAndOrExpression(pParctx, pJlex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pJlex, jtokPrev, PARK_AssignmentOp, pStnod, pStnodExp);
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
	if (pJlex->m_jtok == JTOK('['))
	{
		JtokNextToken(pJlex);
		CSTNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

		pStnodArray->m_jtok = JTOK(' ');
		pStnodArray->m_park = PARK_ArrayDecl;
		
		STypeInfoArray * pTinary = EWC_NEW(pParctx->m_pAlloc, STypeInfoArray) STypeInfoArray();
		pParctx->m_pSymtab->m_arypTinManaged.Append(pTinary);

		if (pJlex->m_jtok == JTOK_PeriodPeriod)
		{
			pTinary->m_aryk = ARYK_Dynamic;
			pStnodArray->m_jtok = JTOK_PeriodPeriod;
			JtokNextToken(pJlex);
		}
		else if (pJlex->m_jtok != JTOK(']'))
		{
			CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
			if (pStnodExp)
			{
				pStnodArray->IAppendChild(pStnodExp);
				// BB pStnodExp->m_fExpectedConstant = true;

				//pTinary->m_c = constant expression eval
			}
		}

		Expect(pParctx, pJlex, JTOK(']'));
		return pStnodArray;
	}
	return nullptr;
}

CSTNode * PStnodParsePointerDecl(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK('*'))
	{
		JtokNextToken(pJlex);
		CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

		pStnod->m_jtok = JTOK('*');
		pStnod->m_park = PARK_Reference;

	// Not doing nested pointer check, ParseTypeSpecifier will handle it
	//CSTNode * pStnodNested = PStnodParsePointerDecl(pParctx, pJlex);
	//if (pStnodNested)
	//	pStnod->IAppendChild(pStnodNested);

	// Note: We're not allocating a STypeInfoPointer here because we may not know the type it points at so we have  
	//  to handle it in the type checker anyway.

	//	STypeInfoPointer * pTinptr = EWC_NEW(pParctx->m_pAlloc, STypeInfoPointer) STypeInfoPointer();
	//	pParctx->m_pSymtab->m_arypTinManaged.Append(pTinptr);
	//	pTinptr->m_soaPacking = -1;
	//	pStnod->m_pTin = pTinptr;

	// BB - should we set STypeInfo::m_strName to a string name containing our qualifier (ie "* s16") and add this 
	//  to the symbol table? It would allow reuse of this struct...
	//
	//	CSymbolTable * pSymtab = pParctx->m_pSymtab;
	//  pSymtab->AddNamedType(pParctx, pJlex, pTin);

		return pStnod;
	}
	return nullptr;
}

CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SJaiLexer * pJlex)
{
	// TODO: not supporting nested types here

	CSTNode * pStnod = PStnodParseIdentifier(pParctx, pJlex);
	if (pStnod)
	{
		if (EWC_FVERIFY(pStnod->m_pStval, "expected string from identifier parse"))
		{
			// TODO: needs to add dependency on global stack frame?

			CSymbolTable * pSymtab = pParctx->m_pSymtab;
			const CString & str = pStnod->m_pStval->m_str;
			pStnod->m_pTin = pSymtab->PTinLookup(str);
			if (!pStnod->m_pTin)
			{
				pSymtab->AddUnknownSymbolReference(str, pStnod); 
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
		ParseError(pParctx, pJlex, "Expected type identifier before %s", PChzFromJtok(JTOK(pJlex->m_jtok)));
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
	if (pJlex->m_jtok != JTOK_Identifier)
		return nullptr;

	SJaiLexer jlexPeek = *pJlex;
	JtokNextToken(&jlexPeek);

	if ((jlexPeek.m_jtok != JTOK(':')) & (jlexPeek.m_jtok != JTOK_ColonEqual))
		return nullptr;

	CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
	if (!pStnodIdent)
		return nullptr;

	CSTNode * pStnodDecl = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

	pStnodDecl->m_jtok = JTOK_Nil;
	pStnodDecl->m_park = PARK_Decl;

	CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodDecl->m_pStdecl = pStdecl;

	pStdecl->m_iStnodIdentifier = pStnodDecl->IAppendChild(pStnodIdent);
	
	CSTNode * pStnodType = nullptr;
	CSTNode * pStnodInit = nullptr;
	if (pJlex->m_jtok == JTOK_ColonEqual)
	{
		JtokNextToken(pJlex);
		pStnodInit = PStnodParseExpression(pParctx, pJlex);
	}
	else if (pJlex->m_jtok == JTOK(':'))
	{
		JtokNextToken(pJlex);
		pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
		pStdecl->m_iStnodType = pStnodDecl->IAppendChild(pStnodType);
		if (pStnodType)
		{
			// NOTE - I'm not propagating the type here as it may be unknown, leave this for the 
			//  type checking/inference pass.

			//pStnodDecl->m_pTin = pStnodType->m_pTin;
		}

		if (pJlex->m_jtok == JTOK('='))
		{
			JtokNextToken(pJlex);
			if (pJlex->m_jtok == JTOK_TripleMinus)
			{
				if (parkContext == PARK_ParameterList)
				{
					ParseError(pParctx, pJlex, "--- uninitializer not allowed in parameter lists");
				}
				else
				{
					pStnodInit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
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
					ParseError(pParctx, pJlex, "initial value expected before %s", PChzFromJtok(JTOK(pJlex->m_jtok)));
				
				// BB - pStnodInit->m_fExpectedConstant = true;
			}

			// Should we do any type inference in the parse phase?  (My current answer is no, the only 
			//  stuff we need coming out of parse is a list of unknown types and symbols)
		}
	}

	pStdecl->m_iStnodInit = pStnodDecl->IAppendChild(pStnodInit);
	if ((pStdecl->m_iStnodType == -1) & (pStdecl->m_iStnodInit == -1))
	{
		ParseError(pParctx, pJlex, "Expected type specifier or initialization");
	}

	CSymbolTable * pSymtab = pParctx->m_pSymtab;
	pSymtab->PSymEnsure(pStnodIdent->m_pStval->m_str, pStnodDecl);

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
	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodList = nullptr;

	while (1)
	{
		CSTNode * pStnod = PStnodParseDecl(pParctx, pJlex);
		if (!pStnod)
		{
			pStnod = PStnodParseDefinition(pParctx, pJlex);
		}
		if (!pStnod)
			break;

		if (pStnodReturn == nullptr)
		{
			pStnodReturn = pStnod;
		}
		else
		{
			if (pStnodList == nullptr)
			{
				pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodList->m_jtok = JTOK('{');
				pStnodList->m_park = PARK_List;
				pStnodList->IAppendChild(pStnodReturn);
				pStnodReturn = pStnodList;
			}
			pStnodList->IAppendChild(pStnod);
		}
	}
	return pStnodReturn;
}

CSTNode * PStnodParseParameterList(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnodParam = PStnodParseParameter(pParctx, pJlex, PARK_ParameterList);
	if (!pStnodParam)
		return nullptr;

	CSTNode * pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

	pStnodList->m_jtok = JTOK_Nil;
	pStnodList->m_park = PARK_ParameterList;
	pStnodList->IAppendChild(pStnodParam);

	while (pJlex->m_jtok == JTOK(','))
	{
		JtokNextToken(pJlex);

		pStnodParam = PStnodParseParameter(pParctx, pJlex, PARK_ParameterList);
		if (!pStnodParam)
		{
			ParseError(pParctx, pJlex, "Expected parameter before %s", PChzFromJtok(JTOK(pJlex->m_jtok)));
			return pStnodList;
		}
		pStnodList->IAppendChild(pStnodParam);
	}

	return pStnodList;
}

CSTNode * PStnodParseEnumConstant(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
	if (!pStnodIdent)
		return nullptr;

	CSTNode * pStnodConstant = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
	pStnodConstant->m_jtok = JTOK_Nil;
	pStnodConstant->m_park = PARK_EnumConstant;

	CSTDecl * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodConstant->m_pStdecl = pStdecl;
	pStdecl->m_iStnodIdentifier = pStnodConstant->IAppendChild(pStnodIdent);

	if (pJlex->m_jtok == JTOK(':'))
	{
		JtokNextToken(pJlex);

		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pJlex);
		if (pStnodExp)
		{
			pStdecl->m_iStnodInit = pStnodConstant->IAppendChild(pStnodExp);
			// BB pStnodExp->m_fExpectedConstant = true;
		}
	}
	return pStnodConstant;
}

int CChildrenInList(CSTNode * pStnod, CSTNode **& ppStnodChildren, PARK parkkList)
{
	if (!pStnod)
	{
		ppStnodChildren = nullptr;
		return 0;
	}

	if (pStnod->m_park != parkkList)
	{
		// assume the list of one was collapsed down
		ppStnodChildren = &pStnod;
		return 1;
	}

	ppStnodChildren = pStnod->m_arypStnodChild.A();
	return (int)pStnod->m_arypStnodChild.C();
}

CSTNode * PStnodParseEnumConstantList(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodList = nullptr;

	while (1)
	{
		CSTNode * pStnod = PStnodParseEnumConstant(pParctx, pJlex);
		if (!pStnod)
			break;

		if (pStnodReturn == nullptr)
		{
			pStnodReturn = pStnod;
		}
		else
		{
			if (pStnodList == nullptr)
			{
				pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodList->m_jtok = JTOK('{');
				pStnodList->m_park = PARK_List;
				pStnodList->IAppendChild(pStnodReturn);
				pStnodReturn = pStnodList;
			}
			pStnodList->IAppendChild(pStnod);
		}

		if (pJlex->m_jtok != JTOK(','))
		{
			break;
		}
		JtokNextToken(pJlex);
	}
	return pStnodReturn;
}

CSTNode * PStnodParseDefinition(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK_Identifier)
	{
		SJaiLexer jlexPeek = *pJlex;
		JtokNextToken(&jlexPeek);

		if (jlexPeek.m_jtok == JTOK_ColonColon)
		{
			CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);

			*pJlex = jlexPeek;
			JtokNextToken(pJlex);
			
			// function definition
			if (pJlex->m_jtok == JTOK('('))
			{
				JtokNextToken(pJlex);

				CSTNode * pStnodFunc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodFunc->m_jtok = JTOK_Nil;
				pStnodFunc->m_park = PARK_ProcedureDefinition;

				CSTProcedure * pStproc = EWC_NEW(pParctx->m_pAlloc, CSTProcedure) CSTProcedure();
				pStnodFunc->m_pStproc = pStproc;
				pStproc->m_iStnodProcName = pStnodFunc->IAppendChild(pStnodIdent);

				CSTNode * pStnodParams = PStnodParseParameterList(pParctx, pJlex);
				pStproc->m_iStnodParameterList = pStnodFunc->IAppendChild(pStnodParams);
				Expect(pParctx, pJlex, JTOK(')'));

				CSTNode * pStnodReturns = nullptr;
				if (pJlex->m_jtok == JTOK_Arrow)
				{
					JtokNextToken(pJlex);

					// TODO : handle multiple return types

					pStnodReturns = PStnodParseTypeSpecifier(pParctx, pJlex);
					pStproc->m_iStnodReturnType = pStnodFunc->IAppendChild(pStnodReturns);
				}

				CSymbolTable * pSymtabProc = EWC_NEW(pParctx->m_pAlloc, CSymbolTable) CSymbolTable(pParctx->m_pAlloc);
				if (pJlex->m_jtok == JTOK('{'))
				{
					PushSymbolTable(pParctx, pSymtabProc);

					CSTNode * pStnodBody = PStnodParseCompoundStatement(pParctx, pJlex);
					pStproc->m_iStnodBody = pStnodFunc->IAppendChild(pStnodBody);
					
					CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
					EWC_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (proc)");
				}

				if (pStproc->m_iStnodBody == -1)
				{
					ParseError(pParctx, pJlex, "Function definition with no body");
				}

				// type info procedure

				CSTNode ** ppStnodParams;
				int cStnodParams = CChildrenInList(pStnodParams, ppStnodParams, PARK_ParameterList);
				//int cStnodReturns = CChildrenInList(pStnodReturns, ppStnodReturns, PARK_Uhhhh);
				CSTNode ** ppStnodReturns = &pStnodReturns;
				int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;

				size_t cBAlloc = CBAlign(sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));
				cBAlloc = cBAlloc +	(cStnodParams + cStnodReturns) * sizeof(STypeInfo *);

				U8 * pB = (U8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc,8);

				const CString & strName = pStnodIdent->m_pStval->m_str;
				STypeInfoProcedure * pTinproc = new(pB) STypeInfoProcedure(strName.PChz());
				STypeInfo ** ppTin = (STypeInfo**)PVAlign( pB + sizeof(STypeInfoProcedure), 
																		EWC_ALIGN_OF(STypeInfo *));
				pTinproc->m_arypTinParams.SetArray(ppTin, 0, cStnodParams);
				pTinproc->m_arypTinReturns.SetArray(&ppTin[cStnodParams], 0, cStnodReturns);
				pTinproc->m_pSymtab = pSymtabProc;

				CSTNode ** ppStnodParamMax = &ppStnodParams[cStnodParams];
				for ( ; ppStnodParams != ppStnodParamMax; ++ppStnodParams)
				{
					CSTNode * pStnodParam = *ppStnodParams;
					if (EWC_FVERIFY(pStnodParam->m_park == PARK_Decl, "Expected decl"))
					{
						pTinproc->m_arypTinParams.Append(pStnodParam->m_pTin);
					}
				}

				CSTNode ** ppStnodReturnMax = &ppStnodReturns[cStnodReturns];
				for ( ; ppStnodReturns != ppStnodReturnMax; ++ppStnodReturns)
				{
					pTinproc->m_arypTinReturns.Append((*ppStnodReturns)->m_pTin);
				}

				pParctx->m_pSymtab->AddNamedType(pParctx, pJlex, pTinproc);

				return pStnodFunc;
			}

			RWORD rword = RwordLookup(pJlex);
			if (rword == RWORD_Enum)
			{
				JtokNextToken(pJlex);
				CSTNode * pStnodEnum = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodEnum->m_jtok = JTOK_Nil;
				pStnodEnum->m_park = PARK_EnumDefinition;

				CSTEnum * pStenum = EWC_NEW(pParctx->m_pAlloc, CSTEnum) CSTEnum();
				pStnodEnum->m_pStenum = pStenum;
				pStenum->m_iStnodIdentifier = pStnodEnum->IAppendChild(pStnodIdent);

				CSTNode * pStnodType = PStnodParseIdentifier(pParctx, pJlex);
				pStenum->m_iStnodType = pStnodEnum->IAppendChild(pStnodType);
				
				CSymbolTable * pSymtabEnum = EWC_NEW(pParctx->m_pAlloc, CSymbolTable) CSymbolTable(pParctx->m_pAlloc);
				CSTNode * pStnodConstantList = nullptr;
				if (pJlex->m_jtok == JTOK('{'))
				{
					JtokNextToken(pJlex);

					PushSymbolTable(pParctx, pSymtabEnum);

					pStnodConstantList = PStnodParseEnumConstantList(pParctx, pJlex);
					pStenum->m_iStnodConstantList = pStnodEnum->IAppendChild(pStnodConstantList);

					CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
					EWC_ASSERT(pSymtabEnum == pSymtabPop, "CSymbol table push/pop mismatch (enum)");

					Expect(pParctx, pJlex, JTOK('}'));
					Expect(pParctx, pJlex, JTOK(';'));
				}

				// type info enum

				const CString & strName = pStnodIdent->m_pStval->m_str;
				CSTNode ** ppStnodMember;
				int cTypememb = CChildrenInList(pStnodConstantList, ppStnodMember, PARK_List);
				size_t cBAlloc = CBAlign(sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeStructMember)) + 
								cTypememb * sizeof(STypeStructMember);
				U8 * pB = (U8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc,8);

				STypeInfoEnum * pTinenum = new(pB) STypeInfoEnum(strName.PChz());
				STypeStructMember * aTypememb = (STypeStructMember*)PVAlign(
																		pB + sizeof(STypeInfoEnum), 
																		EWC_ALIGN_OF(STypeStructMember));
				pTinenum->m_tinstructProduced.m_aryTypememb.SetArray(aTypememb, 0, cTypememb);
				pTinenum->m_pSymtab = pSymtabEnum;

				CSTNode ** ppStnodMemberMax = &ppStnodMember[cTypememb];
				for ( ; ppStnodMember != ppStnodMemberMax; ++ppStnodMember)
				{
					STypeStructMember * pTypememb = pTinenum->m_tinstructProduced.m_aryTypememb.AppendNew();
					CSTNode * pStnodMember = *ppStnodMember;
					EWC_ASSERT(pStnodMember->m_park == PARK_EnumConstant, "Expected enum constant");

					if (EWC_FVERIFY(pStnodMember->m_pStdecl, "enum constant without decl info"))
					{
						pTypememb->m_strName = pStnodMember->m_pStdecl->StrIdentifier(pStnodMember);
					}
				}

				pParctx->m_pSymtab->AddNamedType(pParctx, pJlex, pTinenum);

				return pStnodEnum;
			}
			else if (rword == RWORD_Struct)
			{
				JtokNextToken(pJlex);
				Expect(pParctx, pJlex, JTOK('{'));

				CSTNode * pStnodStruct = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
				pStnodStruct->m_jtok = JTOK_Nil;
				pStnodStruct->m_park = PARK_StructDefinition;
				pStnodStruct->IAppendChild(pStnodIdent);

				CSymbolTable * pSymtabStruct = EWC_NEW(pParctx->m_pAlloc, CSymbolTable) CSymbolTable(pParctx->m_pAlloc);
				PushSymbolTable(pParctx, pSymtabStruct);

				CSTNode * pStnodDeclList = PStnodParseMemberDeclList(pParctx, pJlex);

				CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
				EWC_ASSERT(pSymtabStruct == pSymtabPop, "CSymbol table push/pop mismatch (struct)");

				if (pStnodDeclList)
				{
					pStnodStruct->IAppendChild(pStnodDeclList);
				}

				const CString & strName = pStnodIdent->m_pStval->m_str;

				// type info struct
				CSTNode ** ppStnodMember;
				int cTypememb = CChildrenInList(pStnodDeclList, ppStnodMember, PARK_List);
				size_t cBAlloc = CBAlign(sizeof(STypeInfoStruct), EWC_ALIGN_OF(STypeStructMember)) + 
								cTypememb * sizeof(STypeStructMember);
				U8 * pB = (U8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoStruct * pTinstruct = new(pB) STypeInfoStruct(strName.PChz());
				STypeStructMember * aTypememb = (STypeStructMember*)PVAlign(
																		pB + sizeof(STypeInfoStruct), 
																		EWC_ALIGN_OF(STypeStructMember));
				pTinstruct->m_aryTypememb.SetArray(aTypememb, 0, cTypememb);
				pTinstruct->m_pSymtab = pSymtabStruct;

				CSTNode ** ppStnodMemberMax = &ppStnodMember[cTypememb];
				for ( ; ppStnodMember != ppStnodMemberMax; ++ppStnodMember)
				{
					STypeStructMember * pTypememb = pTinstruct->m_aryTypememb.AppendNew();
					CSTNode * pStnodMember = *ppStnodMember;
					EWC_ASSERT(pStnodMember->m_park == PARK_Decl, "Expected decl");

					pTypememb->m_strName = pStnodMember->m_pStdecl->StrIdentifier(pStnodMember);
					pTypememb->m_pTin = pStnodMember->m_pTin;
				}

				pParctx->m_pSymtab->AddNamedType(pParctx, pJlex, pTinstruct);

				Expect(pParctx, pJlex, JTOK('}'));
				Expect(pParctx, pJlex, JTOK(';'));

				return pStnodStruct;
			}

			ParseError(
				pParctx,
				pJlex, 
				"Unknown token before '%s', expected struct, enum or function definition",
				PChzFromJtok(JTOK(pJlex->m_jtok)));

			pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
		}
	}
	return nullptr;
}

CSTNode * PStnodParseExpressionStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK(';'))
	{
		// return empty statement

		CSTNode * pStnodEmpty = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);

		pStnodEmpty->m_jtok = JTOK(pJlex->m_jtok);
		pStnodEmpty->m_park = PARK_Nop;

		JtokNextToken(pJlex);
		return pStnodEmpty;
	}

	CSTNode * pStnod = PStnodParseExpression(pParctx, pJlex);
	if (pStnod)
	{
		Expect(pParctx, pJlex, JTOK(';'));
	}
	return pStnod;
}

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{
	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodList = nullptr;

	if (pJlex->m_jtok == JTOK('{'))
	{
		JtokNextToken(pJlex);

		while (pJlex->m_jtok != JTOK('}'))
		{
			CSTNode * pStnod = PStnodParseStatement(pParctx, pJlex);
			if (!pStnod)
				break;

			if (pStnodReturn == nullptr)
			{
				pStnodReturn = pStnod;
			}
			else
			{
				if (pStnodList == nullptr)
				{
					pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc);
					pStnodList->m_jtok = JTOK('{');
					pStnodList->m_park = PARK_List;
					pStnodList->IAppendChild(pStnodReturn);
					pStnodReturn = pStnodList;
				}
				pStnodList->IAppendChild(pStnod);
			}
		}

		Expect(pParctx, pJlex, JTOK('}'));
	}
	return pStnodReturn;
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
		}
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
		}break;
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
		pStnodIf->IAppendChild(pStnodStatement);

		RWORD rwordElse = RwordLookup(pJlex);
		if (rwordElse == RWORD_Else)
		{
			CSTNode * pStnodElse = PStnodParseReservedWord(pParctx, pJlex, RWORD_Else);

			CSTNode * pStnodStatement = PStnodParseStatement(pParctx, pJlex);

			pStnodElse->IAppendChild(pStnodStatement);
			pStnodIf->IAppendChild(pStnodElse);
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
		JtokNextToken(pJlex);
		//while expression statement

		EWC_ASSERT(false, "while loops statements are not supported yet");
		return nullptr;
	}
	return nullptr;
}

CSTNode * PStnodParseStatement(CParseContext * pParctx, SJaiLexer * pJlex)
{

	CSTNode * pStnod = PStnodParseCompoundStatement(pParctx, pJlex);
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

void ParseGlobalScope(CWorkspace * pWork, SJaiLexer * pJlex, bool fAllowIllegalEntries)
{
	CParseContext * pParctx = pWork->m_pParctx;

	// load the first token
	JtokNextToken(pJlex);

	while (pJlex->m_jtok != JTOK_Eof)
	{
		CSTNode * pStnod = PStnodParseStatement(pWork->m_pParctx, pJlex);

		pWork->m_arypStnodEntry.Append(pStnod);
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


void PushSymbolTable(CParseContext * pParctx, CSymbolTable * pSymtab)
{
	pSymtab->m_pSymtabParent = pParctx->m_pSymtab;
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

CSymbolTable * PSymtabFromPTin(STypeInfo * pTin)
{
	switch(pTin->m_tink)
	{
		case TINK_Struct:	return ((STypeInfoStruct *)pTin)->m_pSymtab;
		case TINK_Enum:		return ((STypeInfoEnum *)pTin)->m_pSymtab;
		default:			return nullptr;
	}
}

CSymbolTable::~CSymbolTable()
{
	CHash<HV, SSymbol *>::CIterator iterPSym(&m_hashHvPSym);
	while (SSymbol ** ppSym = iterPSym.Next())
	{
		m_pAlloc->EWC_DELETE(*ppSym);
		*ppSym = nullptr;
	}

	CHash<HV, STypeInfoForwardDecl *>::CIterator iterPTinfwd(&m_hashHvPTinfwd);
	while (STypeInfoForwardDecl ** ppTinfwd = iterPTinfwd.Next())
	{
		EWC_ASSERT((*ppTinfwd)->m_arypTinReferences.C() == 0, "unresolved forward declarations");
	}

	CHash<HV, SUnknownSymbol *>::CIterator iterPUnksym(&m_hashHvPUnksym);
	while (SUnknownSymbol ** ppUnksym = iterPUnksym.Next())
	{
		m_pAlloc->EWC_DELETE(*ppUnksym);
		*ppUnksym = nullptr;
	}

	for (STypeInfo ** ppTin = m_arypTinManaged.A(); ppTin != m_arypTinManaged.PMac(); ++ppTin)
	{
		DeleteTypeInfo(m_pAlloc, *ppTin);
		*ppTin = nullptr;
	}
}

void AddBuiltInType(CSymbolTable * pSymtab, const CString & strName, TINK tink)
{
	STypeInfo * pTin = EWC_NEW(pSymtab->m_pAlloc, STypeInfo) STypeInfo(strName.PChz(), tink);

	pSymtab->AddNamedType(nullptr, nullptr, pTin);
}

void AddBuiltInInteger(CSymbolTable * pSymtab, const CString & strName, U32 cBit, bool fSigned)
{
	STypeInfoInteger * pTinint = EWC_NEW(pSymtab->m_pAlloc, STypeInfoInteger) STypeInfoInteger(strName.PChz(), cBit, fSigned);
	pSymtab->AddNamedType(nullptr, nullptr, pTinint);
}

void AddBuiltInFloat(CSymbolTable * pSymtab, const CString & strName, U32 cBit)
{
	STypeInfoFloat * pTinfloat = EWC_NEW(pSymtab->m_pAlloc, STypeInfoFloat) STypeInfoFloat(strName.PChz(), cBit);
	pSymtab->AddNamedType(nullptr, nullptr, pTinfloat);
}

void CSymbolTable::AddBuiltInSymbols()
{
	AddBuiltInType(this, "bool", TINK_Bool);
	AddBuiltInType(this, "void", TINK_Void);
	AddBuiltInType(this, "string", TINK_String);

	AddBuiltInInteger(this, "u8", 8, false);
	AddBuiltInInteger(this, "u16", 16, false);
	AddBuiltInInteger(this, "u32", 32, false);
	AddBuiltInInteger(this, "uint", 32, false);
	AddBuiltInInteger(this, "u64", 64, false);

	AddBuiltInInteger(this, "s8", 8, true);
	AddBuiltInInteger(this, "s16", 16, true);
	AddBuiltInInteger(this, "s32", 32, true);
	AddBuiltInInteger(this, "int", 32, true);
	AddBuiltInInteger(this, "s64", 64, true);

	AddBuiltInFloat(this, "float", 32);
	AddBuiltInFloat(this, "float64", 64);
}

SSymbol * CSymbolTable::PSymEnsure(const CString & strName, CSTNode * pStnodDefinition, int cB, GRFSYM grfsym)
{
	// This will shadow any other type by this name within this stack frame, is that ok?

	SSymbol * pSym;
	SSymbol ** ppSym = m_hashHvPSym.Lookup(strName.Hv()); 
	if (ppSym)
	{
		pSym = *ppSym;
	}
	else
	{
		pSym = EWC_NEW(m_pAlloc, SSymbol) SSymbol;
		(void) m_hashHvPSym.FinsEnsureKeyAndValue(strName.Hv(), pSym);
	}

	pSym->m_strName = strName;
	pSym->m_cB = cB;
	pSym->m_pStnodDefinition = pStnodDefinition;
	pSym->m_grfsym = grfsym;
	pSym->m_pTin = nullptr;
	pSym->m_pSymtab = nullptr;

	return pSym;
}

SSymbol * CSymbolTable::PSymLookup(const CString & str, GRFSYMLOOK grfsymlook)
{
	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		SSymbol ** ppSym = m_hashHvPSym.Lookup(str.Hv()); 
		if (ppSym)
			return *ppSym;
	}

	CSymbolTable * pSymtab = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? m_pSymtabParent : nullptr;
	while (pSymtab)
	{
		SSymbol ** ppSym = pSymtab->m_hashHvPSym.Lookup(str.Hv()); 
		if (ppSym)
			return *ppSym;

		pSymtab = pSymtab->m_pSymtabParent;
	}
	return nullptr; 
}

STypeInfo *	CSymbolTable::PTinLookup(const CString & str, GRFSYMLOOK grfsymlook, SSymbol ** ppSym)
{
	// look for a symbol by this name (symbols can shadow type decls)
	SSymbol * pSym = PSymLookup(str, grfsymlook);
	if (ppSym)
		*ppSym = pSym;

	if (pSym)
	{
		return pSym->m_pTin;
	}

	// look for a named type
	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		STypeInfo ** ppTin = m_hashHvPTin.Lookup(str.Hv()); 
		if (ppTin)
			return *ppTin;
	}

	CSymbolTable * pSymtab = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? m_pSymtabParent : nullptr;
	while (pSymtab)
	{
		STypeInfo ** ppTin = pSymtab->m_hashHvPTin.Lookup(str.Hv()); 
		if (ppTin)
			return *ppTin;

		pSymtab = pSymtab->m_pSymtabParent;
	}
	return nullptr;
}

STypeInfoForwardDecl * CSymbolTable::PTinfwdLookup(const CString & str, GRFSYMLOOK grfsymlook)
{
	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		STypeInfoForwardDecl ** ppTinfwd = m_hashHvPTinfwd.Lookup(str.Hv()); 
		if (ppTinfwd)
			return *ppTinfwd;
	}

	CSymbolTable * pSymtab = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? m_pSymtabParent : nullptr;
	while (pSymtab)
	{
		STypeInfoForwardDecl ** ppTinfwd = pSymtab->m_hashHvPTinfwd.Lookup(str.Hv()); 
		if (ppTinfwd)
			return *ppTinfwd;

		pSymtab = pSymtab->m_pSymtabParent;
	}
	return nullptr; 
}

STypeInfoForwardDecl * CSymbolTable::PTinfwdBegin(const CString & str)
{
	STypeInfoForwardDecl * pTinfwd = EWC_NEW(m_pAlloc, STypeInfoForwardDecl) STypeInfoForwardDecl(m_pAlloc, str.PChz());

	m_arypTinManaged.Append(pTinfwd);

	STypeInfoForwardDecl ** ppTinfwd = nullptr;
	FINS fins = m_hashHvPTinfwd.FinsEnsureKey(str.Hv(), &ppTinfwd);
	if (fins == FINS_AlreadyExisted)
	{
		if (!EWC_FASSERT(false, "trying to begin forward declaration that is already registered"))
			return *ppTinfwd;
	}

	*ppTinfwd = pTinfwd;
	return pTinfwd;
}

void CSymbolTable::EndForwardDecl(const CString & str, STypeInfoForwardDecl * pTinfwd, STypeInfo * pTinResolved)
{
	//STypeInfoForwardDecl * pTinfwd = PTinfwdLookup(str, FSYMLOOK_Local);
	//if (!EWC_FVERIFY(pTinfwd, "failed to find forward declaration for resolution"))
	//	return;

	// loop over all the references to this forward decl and redirect them to point at the actual type

	STypeInfo ** ppTinMac = pTinfwd->m_arypTinReferences.PMac();
	for (STypeInfo ** ppTin = pTinfwd->m_arypTinReferences.A(); ppTin != ppTinMac; ++ppTin)
	{
		switch ((*ppTin)->m_tink)
		{
		case TINK_Pointer:
			{
				STypeInfoPointer * pTinptr = (STypeInfoPointer *)*ppTin;
				if (EWC_FVERIFY(pTinptr->m_pTinPointedTo == pTinfwd, "bad reference in forward declaration"))
				{
					pTinptr->m_pTinPointedTo = pTinResolved;
				} 
			}break;
		case TINK_Array:
			{
				STypeInfoArray * pTinary = (STypeInfoArray *)*ppTin;
				if (EWC_FVERIFY(pTinary->m_pTin == pTinfwd, "bad reference in forward declaration"))
				{
					pTinary->m_pTin = pTinResolved;
				} 
			}break;
		default: EWC_ASSERT(false, "unexpected type info kind in EndForwardDecl()");
			break;
		}
	}
	pTinfwd->m_arypTinReferences.Clear();
}

void CSymbolTable::AddUnknownSymbolReference(const CString & str, CSTNode * pStnodRef)
{
	SUnknownSymbol * pUnksym;
	SUnknownSymbol ** ppUnksym = m_hashHvPUnksym.Lookup(str.Hv()); 
	if (ppUnksym)
	{
		pUnksym = *ppUnksym;
		EWC_ASSERT(pUnksym->m_strName == str, "name mismatch (string hash collision?)");
	}
	else
	{
		pUnksym = EWC_NEW(m_pAlloc, SUnknownSymbol) SUnknownSymbol(m_pAlloc);
		pUnksym->m_strName = str;
		(void) m_hashHvPUnksym.FinsEnsureKeyAndValue(str.Hv(), pUnksym);
	}

	pUnksym->m_arypStnodRef.Append(pStnodRef);
}

void CSymbolTable::AddManagedTin(STypeInfo * pTin)
{
	m_arypTinManaged.Append(pTin);
}

void CSymbolTable::AddNamedType(CParseContext * pParctx, SJaiLexer * pJlex, STypeInfo * pTin)
{
	m_arypTinManaged.Append(pTin);
	const CString & strName = pTin->m_strName;
	if (!EWC_FVERIFY(!strName.FIsEmpty(), "registering unnamed type"))
	{
		return;
	}

	STypeInfo ** ppTinValue = nullptr;
	FINS fins = m_hashHvPTin.FinsEnsureKey(strName.Hv(), &ppTinValue);
	if (fins == FINS_Inserted)
	{
		*ppTinValue = pTin;
	}
	else
	{
		EWC_ASSERT(pParctx && pJlex,"Two types encountered with same name (%s)", strName.PChz()); 
		if (pParctx && pJlex)
		{
			ParseError(pParctx, pJlex, "Two types encountered with same name (%s)", strName.PChz());
		}
	}
}

CString	CSTDecl::StrIdentifier(CSTNode * pStnod)
{
	if (m_iStnodIdentifier < 0)
		return CString("");

	return pStnod->PStnodChild(m_iStnodIdentifier)->m_pStval->m_str;
}



// TypeInfo routines
void DeleteTypeInfo(CAlloc * pAlloc, STypeInfo * pTin)
{
	// Note - trying out c-style destruction, and didn't want a v-table for all type infos just for this
	//  We'll see if I hate it in a week...

	// BB - could add debug only field to TypeInfo to assert this was called on destruction (?)

	CSymbolTable ** ppSymtab = nullptr;
	switch (pTin->m_tink)
	{
	case TINK_Struct:
		{
			STypeInfoStruct * pTinstruct = (STypeInfoStruct *)pTin;
			ppSymtab = &pTinstruct->m_pSymtab;
		} break;
	case TINK_Enum:
		{
			STypeInfoEnum * pTinenum = (STypeInfoEnum *)pTin;
			ppSymtab = &pTinenum->m_pSymtab;
		} break;
	case TINK_Procedure:
		{
			STypeInfoProcedure * pTinproc = (STypeInfoProcedure *)pTin;
			ppSymtab = &pTinproc->m_pSymtab;
		} break;
	default: ;
	}

	if (ppSymtab && *ppSymtab)
	{
		pAlloc->EWC_DELETE(*ppSymtab);
		*ppSymtab = nullptr;
	}
	pAlloc->EWC_DELETE(pTin);
}



// Syntax Tree Nodes

CSTNode::CSTNode(CAlloc * pAlloc)
:m_jtok(JTOK_Nil)
,m_park(PARK_Nil)
,m_strees(STREES_Parsed)
,m_pStval(nullptr)
,m_pStdecl(nullptr)
,m_pStproc(nullptr)
,m_pStenum(nullptr)
,m_lexloc()
,m_pTin(nullptr)
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


size_t CChPrintTypeInfo(STypeInfo * pTin, PARK park, char * pCh, char * pChEnd)
{
	if (pTin == nullptr)
	{
		switch (park)
		{
		case PARK_List:		return CChCopy("{}", pCh, pChEnd-pCh);
		default:			return CChCopy("???", pCh, pChEnd-pCh);
		}
	}

	switch (pTin->m_tink)
	{
	case TINK_Pointer:		
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer*)pTin;
			char * pChWork = pCh;
			pChWork += CChCopy("*", pChWork, pChEnd-pChWork);
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
				pChWork += CChFormat(pChWork, pChEnd-pChWork, "[%d] ", pTinary->m_c);
				break;
			case ARYK_Dynamic:
				pChWork += CChCopy("[..] ", pChWork, pChEnd-pChWork);
				break;
			case ARYK_Static:
				pChWork += CChCopy("[] ", pChWork, pChEnd-pChWork);
				break;

			}
			//pChWork += CChPrintStnodType(pTinary->m_pTin, pCh, pChEnd);
			return pChWork - pCh;
		}break;

	case TINK_Literal:		
		{
			STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTin;
			char * pChWork = pCh;
			pChWork += CChCopy(PChzFromLitk(pTinlit->m_stval.m_litty.m_litk), pChWork, pChEnd-pChWork);
			pChWork += CChCopy("Literal", pChWork, pChEnd-pChWork);
			return pChWork - pCh;
		}
	case TINK_Integer:		// fall through ...
    case TINK_Procedure:	// fall through ...
    case TINK_Float:		// fall through ...
    case TINK_Bool:			// fall through ...
    case TINK_String:		// fall through ...
    case TINK_Void:			// fall through ...
    case TINK_Struct:		// fall through ...
    case TINK_Null:			// fall through ...
    case TINK_Any:			// fall through ...
    case TINK_Enum:			// fall through ...
	default:
		return CChFormat(pCh, pChEnd-pCh, "%s", pTin->m_strName.PChz());
	}
}

size_t CChPrintStnodName(CSTNode * pStnod, char * pCh, char * pChEnd)
{
	switch (pStnod->m_park)
	{
	case PARK_Identifier:			return CChFormat(pCh, pChEnd-pCh, "@%s", pStnod->m_pStval->m_str.PChz());
	case PARK_ReservedWord:			return CChCopy(PChzFromRword(pStnod->m_pStval->m_rword), pCh, pChEnd - pCh); 
	case PARK_Nop:					return CChCopy("nop", pCh, pChEnd - pCh); 
	case PARK_Literal:				
		{
			switch (pStnod->m_pStval->m_litty.m_litk)
			{
			case LITK_String:		return CChFormat(pCh, pChEnd-pCh, "\"%s\"", pStnod->m_pStval->m_str.PChz());
			case LITK_Char:			return CChFormat(pCh, pChEnd-pCh, "'%s'", pStnod->m_pStval->m_str.PChz());
			case LITK_Int:			return CChFormat(pCh, pChEnd-pCh, "%d", pStnod->m_pStval->m_n);
			case LITK_Float:		return CChFormat(pCh, pChEnd-pCh, "%f", pStnod->m_pStval->m_g);
			case LITK_Bool:			return CChFormat(pCh, pChEnd-pCh, "%s", (pStnod->m_pStval->m_n) ? "true" : "false");
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
	case PARK_AssignmentOp:		    return CChFormat(pCh, pChEnd-pCh, "%s", PChzFromJtok(pStnod->m_jtok));
	case PARK_ArrayElement:		    return CChCopy("elem", pCh, pChEnd - pCh);
	case PARK_MemberLookup:		    return CChCopy("member", pCh, pChEnd - pCh);
	case PARK_ArgumentCall:		    return CChCopy("args", pCh, pChEnd - pCh);
	case PARK_List:				    return CChCopy("{}", pCh, pChEnd - pCh);
	case PARK_ParameterList:	    return CChCopy("params", pCh, pChEnd - pCh);
	case PARK_ArrayDecl:		    return CChFormat(pCh, pChEnd - pCh, "[%s]", PChzFromJtok(pStnod->m_jtok));
	case PARK_If:				    return CChCopy("if", pCh, pChEnd - pCh);
	case PARK_Else:				    return CChCopy("else", pCh, pChEnd - pCh);
	case PARK_Reference:			return CChCopy("ptr", pCh, pChEnd - pCh);
	case PARK_Decl:					return CChCopy("decl", pCh, pChEnd - pCh);
	case PARK_ProcedureDefinition:	return CChCopy("func", pCh, pChEnd - pCh);
	case PARK_EnumDefinition:		return CChCopy("enum", pCh, pChEnd - pCh);
	case PARK_StructDefinition:		return CChCopy("struct", pCh, pChEnd - pCh);
	case PARK_EnumConstant:			return CChCopy("enumConst", pCh, pChEnd - pCh);
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
		pChWork += CChPrintTypeInfo(pStnod->m_pTin, pStnod->m_park, pChWork, pChEnd);
		grfdbgstr.Clear(FDBGSTR_Type);
	}
	return pChWork - pCh;
}

size_t CSTNode::CChWriteDebugString(char * pCh, char * pChEnd, GRFDBGSTR grfdbgstr)
{
	bool fIsOperand = (m_park == PARK_Identifier) | (m_park == PARK_Literal);
	bool fIsOperator = !fIsOperand;

	if (fIsOperator)
	{
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
	else if (fIsOperand)
	{
		EWC_ASSERT(m_pStval, "operand without value struct");
		return CChPrintStnod(this, pCh, pChEnd, grfdbgstr);
	}
	return 0;
}

void CChWriteDebugStringForEntries(CWorkspace * pWork, char * pCh, char * pChMax, GRFDBGSTR grfdbgstr)
{
	for (size_t ipStnod = 0; ipStnod < pWork->m_arypStnodEntry.C(); ++ipStnod)
	{
		CSTNode * pStnod = pWork->m_arypStnodEntry[ipStnod];
		pCh += pStnod->CChWriteDebugString(pCh, pChMax, grfdbgstr);

		if (pCh != pChMax)
		{
			*pCh++ = (ipStnod+1 == pWork->m_arypStnodEntry.C()) ? '\0' : ' ';
		}
	}
}

void AssertParseMatchTailRecurse(
	CWorkspace * pWork,
	const char * pChzIn,
	const char * pChzOut,
	const char * apChzExpectedSym[] = nullptr,
	const char * apChzUnknownSym[] = nullptr)
{

#ifdef EWC_TRACK_ALLOCATION
	U8 aBAltrac[1024 * 100];
	CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	pWork->m_pAlloc->SetAltrac(pAltrac);
#endif

	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pChzIn);

	EWC_ASSERT(pWork->m_pParctx->m_cError == 0, "parse errors detected");
	pWork->m_pParctx->m_cError = 0;

	ParseGlobalScope(pWork, &jlex, true);
	EWC_ASSERT(pWork->m_arypStnodEntry.C() > 0);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Name);

	EWC_ASSERT(FAreSame(aCh, pChzOut), "parse debug string doesn't match expected value");

	CSymbolTable * pSymtab = pWork->m_pParctx->m_pSymtab;
	if (apChzExpectedSym)
	{
		for (int ipChz = 0; ; ++ipChz)
		{
			const char * pChzSym = apChzExpectedSym[ipChz];
			if (!pChzSym)
				break;

			SSymbol * pSym = pSymtab->PSymLookup(pChzSym);
			EWC_ASSERT(pSym, "Failed to find expected symbol %s", pChzSym);
		}
	}

	if (apChzUnknownSym)
	{
		for (int ipChz = 0; ; ++ipChz)
		{
			const char * pChzUnksym = apChzUnknownSym[ipChz];
			if (!pChzUnksym)
				break;

			SUnknownSymbol ** ppUnksym = pSymtab->m_hashHvPUnksym.Lookup(HvFromPchz(pChzUnksym));
			EWC_ASSERT(ppUnksym, "Missing to find unknown symbol %s", pChzUnksym);
		}
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
	U8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	U8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);

	const char * pChzIn1 =	"x + 3*5;";
	const char * pChzOut1 = "(+ @x (* 3 5))";
	AssertParseMatchTailRecurse(&work, pChzIn1, pChzOut1);

	const char * pChzIn2 =	"(ugh + foo) / ((x + 3)*5);";
	const char * pChzOut2 = "(/ (+ @ugh @foo) (* (+ @x 3) 5))";
	AssertParseMatchTailRecurse(&work, pChzIn2, pChzOut2);

	EWC_ASSERT(32/8/2 == 2, "ack");
	pChzIn2  = "ugh/foo/guh/ack;";
	pChzOut2 = "(/ (/ (/ @ugh @foo) @guh) @ack)";
	AssertParseMatchTailRecurse(&work, pChzIn2, pChzOut2);

	const char * pChzIn3 =	"(5 + -x) * -(3 / foo);";
	const char * pChzOut3 = "(* (+ 5 (unary[-] @x)) (unary[-] (/ 3 @foo)))";
	AssertParseMatchTailRecurse(&work, pChzIn3, pChzOut3);

	const char * pChzIn4 =	"ick * ack * -(3 / foo);";
	const char * pChzOut4 = "(* (* @ick @ack) (unary[-] (/ 3 @foo)))";
	AssertParseMatchTailRecurse(&work, pChzIn4, pChzOut4);

	const char * pChzIn5 =	"ick & (ack&&foo | 123) || guh;";
	const char * pChzOut5 = "(|| (& @ick (&& @ack (| @foo 123))) @guh)";
	AssertParseMatchTailRecurse(&work, pChzIn5, pChzOut5);

	const char * pChzIn6 =	"ick == ack < foo\n != 123 >= guh;";
	const char * pChzOut6 = "(!= (== @ick (< @ack @foo)) (>= 123 @guh))";
	AssertParseMatchTailRecurse(&work, pChzIn6, pChzOut6);

	// NOTE - weird ordering shouldn't matter as we will ensure lhs is l-value
	const char * pChzIn7 =	"ick = 5 += foo *= guh;";
	const char * pChzOut7 = "(*= (+= (= @ick 5) @foo) @guh)";
	AssertParseMatchTailRecurse(&work, pChzIn7, pChzOut7);

	const char * pChzIn8 =	"++foo.bah[23];";
	const char * pChzOut8 = "(unary[++] (elem (member @foo @bah) 23))";
	AssertParseMatchTailRecurse(&work, pChzIn8, pChzOut8);

	const char * pChzIn9 =	"{ i=5; foo.bah = ack; }";
	const char * pChzOut9 = "({} (= @i 5) (= (member @foo @bah) @ack))";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "{ if i==foo.bar.fug ick = 3; else ick = 7; }";
	pChzOut9	= "(if (== @i (member (member @foo @bar) @fug)) (= @ick 3) (else (= @ick 7)))";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "{ break; continue; return foo=\"test\"; }";
	pChzOut9	= "({} (break) (continue) (return (= @foo \"test\")))";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "{ AddNums :: (a : int, b := 1) -> int { return a + b; } bah := 3; }";
	pChzOut9	= "({} (func @AddNums (params (decl @a @int) (decl @b 1)) @int (return (+ @a @b)))"
					" (decl @bah 3))";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "{ NopFunc :: () { guh := 2; } wha : * int; }";
	pChzOut9	= "({} (func @NopFunc (decl @guh 2)) (decl @wha (ptr @int)))";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "{ ENUMK :: enum int { ENUMK_Nil : -1, ENUMK_Foo, ENUMK_Bah : 3 }; a = 2; }";
	pChzOut9	= "({} (enum @ENUMK @int ({} (enumConst @ENUMK_Nil (unary[-] 1)) (enumConst @ENUMK_Foo) (enumConst @ENUMK_Bah 3)))"
					" (= @a 2))";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "STest :: struct { m_a := 2; m_b : int; }; boo : int = 3;";
	pChzOut9	= "(struct @STest ({} (decl @m_a 2) (decl @m_b @int))) (decl @boo @int 3)";
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9);

	pChzIn9		= "pChz := foo; guh : gur = 5; bah : s32 = woo;";
	pChzOut9	= "(decl @pChz @foo) (decl @guh @gur 5) (decl @bah @s32 @woo)";
	const char * apChzExpectedSym[] = {"pChz", "guh", "bah", nullptr };
	const char * apChzUnknownSym[] = {"foo", "gur", "woo", nullptr };
	AssertParseMatchTailRecurse(&work, pChzIn9, pChzOut9, apChzExpectedSym, apChzUnknownSym);

	StaticShutdownStrings(&allocString);
}

} // namespace EWC