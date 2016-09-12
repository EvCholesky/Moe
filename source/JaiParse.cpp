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

enum FPDECL
{
	FPDECL_AllowCompoundDecl		= 0x1,	// allow comma-separated declaration of multiple variables.
	FPDECL_AllowVariadic		= 0x2,	// allow the list to end with variadic arguments (..)
	FPDECL_AllowUninitializer	= 0x4,	// allow decls to specify explicit uninitializers (n:int=---;}

	FPDECL_None			= 0x0,
	FPDECL_All			= 0x7,
};
EWC_DEFINE_GRF(GRFPDECL, FPDECL, u32);

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SJaiLexer * pJlex, CSymbolTable * pSymtab);
CSTNode * PStnodParseDefinition(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseLogicalAndOrExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SJaiLexer * pJlex);
CSTNode * PStnodParseParameterList(CParseContext * pParctx, SJaiLexer * pJlex, CSymbolTable * pSymtabProc);
CSTNode * PStnodParseReturnArrow(CParseContext * pParctx, SJaiLexer * pJlex);
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
		"If",
		"Else",
		"Array Decl",
		"Reference Decl",
		"Procedure Reference Decl",
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
		if (FAreCozEqual(PChzFromArymemb((ARYMEMB)arymemb), pChzMember))
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

CSTValue * PStvalCopy(CAlloc * pAlloc, CSTValue * pStval)
{
	if (!pStval)
		return nullptr;

	auto pStvalRet = EWC_NEW(pAlloc, CSTValue) CSTValue();
	*pStvalRet = *pStval;
	return pStvalRet;
}

CSTNode * PStnodCopy(CAlloc * pAlloc, CSTNode * pStnodSrc)
{
	auto pStnodDst = EWC_NEW(pAlloc, CSTNode) CSTNode(pAlloc, pStnodSrc->m_lexloc);
	*pStnodDst = *pStnodSrc;

	if (pStnodSrc->m_pStval)
	{
		pStnodDst->m_pStval = PStvalCopy(pAlloc, pStnodSrc->m_pStval);
	}

	if (pStnodSrc->m_pStident)
	{
		pStnodDst->m_pStident = EWC_NEW(pAlloc, CSTIdentifier) CSTIdentifier();
		*pStnodDst->m_pStident = *pStnodSrc->m_pStident;
	}

	if (pStnodSrc->m_pStdecl)
	{
		pStnodDst->m_pStdecl = EWC_NEW(pAlloc, CSTDecl) CSTDecl();
		*pStnodDst->m_pStdecl = *pStnodSrc->m_pStdecl;
	}

	if (pStnodSrc->m_pStproc)
	{
		pStnodDst->m_pStproc = EWC_NEW(pAlloc, CSTProcedure) CSTProcedure();
		*pStnodDst->m_pStproc = *pStnodSrc->m_pStproc;
	}

	if (pStnodSrc->m_pStfor)
	{
		pStnodDst->m_pStfor = EWC_NEW(pAlloc, CSTFor) CSTFor();
		*pStnodDst->m_pStfor = *pStnodSrc->m_pStfor;
	}

	if (pStnodSrc->m_pStenum)
	{
		pStnodDst->m_pStenum = EWC_NEW(pAlloc, CSTEnum) CSTEnum();
		*pStnodDst->m_pStenum = *pStnodSrc->m_pStenum;
	}

	auto cpStnodChild = pStnodSrc->m_arypStnodChild.C();
	for (size_t ipStnod = 0; ipStnod < cpStnodChild; ++ipStnod)
	{
		auto pStnodChild = pStnodSrc->m_arypStnodChild[ipStnod];
		CSTNode * pStnodChildCopy = nullptr;
		if (pStnodChild)
		{
			pStnodChildCopy = PStnodCopy(pAlloc, pStnodChild);
		}
		pStnodDst->m_arypStnodChild[ipStnod] = pStnodChildCopy;
	}
	return pStnodDst;
}

void ParseError(CParseContext * pParctx, SJaiLexer * pJlex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pJlex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pParctx->m_pWork->m_pErrman, &lexloc, pChzFormat, ap);
}

EWC::CString StrUnexpectedToken(SJaiLexer * pJlex)
{
	if (pJlex->m_jtok == JTOK_Identifier)
	{
		return pJlex->m_str;
	}
	return CString(PCozCurrentToken(pJlex));
}

void SkipToToken(SJaiLexer * pJlex, JTOK const * const aJtok, int cJtok)
{
	while (1)
	{
		bool fFound = false;
		JTOK jtok = (JTOK)pJlex->m_jtok;
		if (jtok == JTOK_Eof)
			break;

		for (int iJtok = 0; !fFound && iJtok < cJtok; ++iJtok)
		{
			fFound |= (jtok == aJtok[iJtok]);
		}

		if (fFound)
			break;
		JtokNextToken(pJlex);
	}
}

void Expect(CParseContext * pParctx, SJaiLexer * pJlex, JTOK jtokExpected, const char * pCozInfo = nullptr, ...)
{
	if (pJlex->m_jtok != jtokExpected)
	{
		char aB[1024] = {0};
		if (pCozInfo)
		{
			va_list ap;
			va_start(ap, pCozInfo);
			vsprintf_s(aB, EWC_DIM(aB), pCozInfo, ap);
		}

		auto strUnexpected = StrUnexpectedToken(pJlex);
		ParseError(pParctx, pJlex, "Expected '%s' before '%s' %s", PCozFromJtok(jtokExpected), strUnexpected.PCoz(), aB);
	}

	JtokNextToken(pJlex);
}

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

	CString strIdent = pJlex->m_str;
	auto pStnod = PStnodAllocateIdentifier(pParctx, lexloc, strIdent);
	pStnod->m_jtok = JTOK(pJlex->m_jtok);

	if (strIdent.FIsEmpty())
	{
		ParseError(pParctx, pJlex, "Identifier with no string");
	}
	else if (strIdent.PCoz()[0] == '#')
	{
		ParseError(pParctx, pJlex, "Unknown directive encountered %s", strIdent.PCoz());
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
		ParseError(pParctx, pJlex, "Expected %s before %s", PCozFromRword(rwordExpected), PCozCurrentToken(pJlex));
		return nullptr;
	}

	SLexerLocation lexloc(pJlex);
	CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

	pStnod->m_jtok = JTOK(pJlex->m_jtok);
	pStnod->m_park = PARK_ReservedWord;

	CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
	pStval->m_str = pJlex->m_str;
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
				ParseError(pParctx, pJlex, "Expected expression before %s", PCozCurrentToken(pJlex));
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
				bool fIsRwordLiteral = false;
				switch (rword)
				{
					case RWORD_True:				// fall through
					case RWORD_False:				// fall through
					case RWORD_FileDirective:		// fall through
					case RWORD_LineDirective:		// fall through
					case RWORD_Null:				fIsRwordLiteral = true;	 break;
					default: break;
				}

				if (!fIsRwordLiteral)
					return nullptr;

				SLexerLocation lexloc(pJlex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_jtok = JTOK(pJlex->m_jtok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_stvalk = STVALK_ReservedWord;
				pStval->m_rword = rword;
				pStnod->m_pTin = nullptr;

				pStnod->m_pStval = pStval;
				switch (rword)
				{
				case RWORD_True:
					{
						pStval->m_nUnsigned = 1;
						pStval->m_litkLex = LITK_Bool;
					} break;
				case RWORD_False:
					{
						pStval->m_nUnsigned = 0;
						pStval->m_litkLex = LITK_Bool;
					} break;
				case RWORD_Null:
					{
						pStval->m_nUnsigned = 0;
						pStval->m_litkLex = LITK_Null;
					} break;
				case RWORD_FileDirective:
					{
						pStval->m_stvalk = STVALK_String;
						pStval->m_litkLex = LITK_String;
						pStval->m_str = lexloc.m_strFilename;
					} break;
				case RWORD_LineDirective:
					{
						s32 iLine;
						s32 iCol;
						CalculateLinePosition(pParctx->m_pWork, &lexloc, &iLine, &iCol);

						pStval->m_litkLex = LITK_Integer;
						pStval->m_nUnsigned = iLine;
					} break;
				}

				if (pStval->m_str.FIsEmpty())
				{
					pStval->m_str = pJlex->m_str;
				}


				JtokNextToken(pJlex);
				return pStnod;
			}
		case JTOK_Literal:
			{
				// NOTE - Negative literals don't exist until the type checking phase folds in the unary '-' operator

				SLexerLocation lexloc(pJlex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_jtok = JTOK(pJlex->m_jtok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_str = pJlex->m_str;
				pStval->m_litkLex = pJlex->m_litk;

				if (pJlex->m_litk == LITK_Float)
				{
					SetFloatValue(pStval, pJlex->m_g);
				}
				else if (pJlex->m_litk == LITK_String)
				{
					pStval->m_stvalk = STVALK_String;
				}
				else if (pJlex->m_litk == LITK_Char)
				{
					SetUnsignedIntValue(pStval, pJlex->m_n);
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
				auto * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
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
					pStnodIdent ? StrFromIdentifier(pStnodIdent).PCoz() : "unknown");
			}break;
		case JTOK('.'):		// . identifier
			{
				JtokNextToken(pJlex); // consume '.'
				SLexerLocation lexloc(pJlex);

				JTOK jtokPrev = JTOK(pJlex->m_jtok);	
				CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
				if (!pStnodIdent)
				{
					ParseError(pParctx, pJlex, "Expected identifier after '.' before %s", PCozFromJtok(jtokPrev));
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
					PCozFromJtok(jtokPrev),
					PCozCurrentToken(pJlex));
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
	auto rword = RwordLookup(pJlex);
	if (rword != RWORD_Cast && rword != RWORD_AutoCast)
	{
		return PStnodParseUnaryExpression(pParctx, pJlex);
	}

	JtokNextToken(pJlex);

	SLexerLocation lexloc(pJlex);
	pStnodCast = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodCast->m_park = PARK_Cast;

	pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodCast->m_pStdecl = pStdecl;

	if (rword == RWORD_Cast)
	{
		Expect(pParctx, pJlex, JTOK('('));

		auto pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
		pStdecl->m_iStnodType = pStnodCast->IAppendChild(pStnodType);

		Expect(pParctx, pJlex, JTOK(')'));
	}

	auto pStnodChild = PStnodParseCastExpression(pParctx, pJlex);
	if (!pStnodCast)
		return pStnodChild;

	pStdecl->m_iStnodInit = pStnodCast->IAppendChild(pStnodChild);

	if (pStdecl->m_iStnodInit < 0)
	{
		EmitError(pParctx->m_pWork->m_pErrman, &pStnodCast->m_lexloc, "Cast statement missing right hand side");
	}

	if (pStdecl->m_iStnodType < 0 && rword != RWORD_AutoCast)
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
			PCozFromJtok(jtokExpression),
			PCozCurrentToken(pJlex));
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

CSTNode * PStnodParseProcedureReferenceDecl(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (FConsumeToken(pJlex, JTOK('(')))
	{
		SLexerLocation lexloc(pJlex);
		CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodProc->m_jtok = (JTOK)pJlex->m_jtok;
		pStnodProc->m_park = PARK_ProcedureReferenceDecl;

		CSTProcedure * pStproc = EWC_NEW(pParctx->m_pAlloc, CSTProcedure) CSTProcedure();
		pStnodProc->m_pStproc = pStproc;

		CSymbolTable * pSymtabProc = nullptr; //null symbol table as the we're a forward reference
		CSTNode * pStnodParams = PStnodParseParameterList(pParctx, pJlex, pSymtabProc);
		pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
		Expect(pParctx, pJlex, JTOK(')'));

		auto pStnodReturns = PStnodParseReturnArrow(pParctx, pJlex);
		pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);

		// allocate a PTinptr to a PTinproc
		int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;
		int cStnodParams;
		(void) PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

		size_t cBAlloc = CBAlign(sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));
		cBAlloc = cBAlloc +	(cStnodParams + cStnodReturns) * sizeof(STypeInfo *);

		u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc,8);

		STypeInfoProcedure * pTinproc = new(pB) STypeInfoProcedure("");
		STypeInfo ** ppTin = (STypeInfo**)PVAlign( pB + sizeof(STypeInfoProcedure), 
																EWC_ALIGN_OF(STypeInfo *));
		pTinproc->m_arypTinParams.SetArray(ppTin, 0, cStnodParams);
		pTinproc->m_arypTinParams.AppendFill(cStnodParams, nullptr);

		pTinproc->m_arypTinReturns.SetArray(&ppTin[cStnodParams], 0, cStnodReturns);
		pTinproc->m_arypTinReturns.AppendFill(cStnodReturns, nullptr);
		pParctx->m_pSymtab->AddManagedTin(pTinproc);
		pStnodProc->m_pTin = pTinproc;

		if (pJlex->m_jtok == JTOK_ReservedWord)
		{
			RWORD rword = RwordLookup(pJlex);
			if (rword == RWORD_StdCall)
			{
				pTinproc->m_callconv = CALLCONV_StdcallX86;
				JtokNextToken(pJlex);
			}
		}

		return pStnodProc;
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
				ParseError(pParctx, pJlex, "Expected identifier after '.' before %s", PCozFromJtok(jtokPrev));
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

	pStnod = PStnodParseProcedureReferenceDecl(pParctx, pJlex);
	if (pStnod)
	{
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
		ParseError(pParctx, pJlex, "Expected type identifier before '%s'", PCozFromJtok(JTOK(pJlex->m_jtok)));
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

// Decl structure has become a bit complicated...
// ParameterList { Decl, Decl, Decl{ childDecl[3] } }
// ParameterLists contain declarations and declarations can contain compound (ie parent/child declarations)
//   Compound declarations only allow one level deep and are there to support comma separated declarations 
//   and initialization to multiple return types.

// n1, n2:s32, g1: f32;	// would be one compound decl
// parent decls cannot have a type.
// child decls cannot have initializers - the initializer should come from the parent.

void ValidateDeclaration(CParseContext * pParctx, SJaiLexer * pJlex, CSTNode * pStnodDecl)
{
	CSTDecl * pStdecl = pStnodDecl->m_pStdecl;
	if (!EWC_FVERIFY(pStdecl, "bad declaration in ValidateDeclaration"))
		return;

	bool fMissingTypeSpecifier;
	if (pStdecl->m_iStnodChildMin == -1)
	{
		fMissingTypeSpecifier = (pStdecl->m_iStnodType == -1);
		EWC_ASSERT(pStdecl->m_iStnodIdentifier != -1, "declaration missing identifier");
	}
	else // compound decl
	{
		if (pStdecl->m_iStnodType != -1)
			ParseError(pParctx, pJlex, "Internal error, compound decl should not specify a type");

		fMissingTypeSpecifier = false;
		for (int iStnodChild = pStdecl->m_iStnodChildMin; iStnodChild != pStdecl->m_iStnodChildMax; ++iStnodChild)
		{
			auto pStdeclChild = pStnodDecl->PStnodChild(iStnodChild)->m_pStdecl;
			EWC_ASSERT(pStdeclChild->m_iStnodIdentifier != -1, "declaration missing identifier");

			if (pStdeclChild->m_iStnodInit != -1)
				ParseError(pParctx, pJlex, "Internal error, child decl should not specify an initializer");

			EWC_ASSERT(pStdeclChild->m_iStnodChildMin == -1 && pStdeclChild->m_iStnodChildMax == -1, "nested children not supported");

			fMissingTypeSpecifier |= (pStdeclChild->m_iStnodType == -1);
		}
	}
	
	auto pStnodInit = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
	if (fMissingTypeSpecifier & (pStnodInit == nullptr))
		ParseError(pParctx, pJlex, "Expected type specifier or initialization");
	if (pStnodInit && pStnodInit->m_park == PARK_Uninitializer)
	{
		if (fMissingTypeSpecifier)
		{
			ParseError(pParctx, pJlex, "Uninitializer not allowed without specified type");
		}
	}
}


CSTNode * PStnodParseParameter(
	CParseContext * pParctx,
	SJaiLexer * pJlex,
	CSymbolTable * pSymtab,
	GRFPDECL grfpdecl)
{
	SLexerLocation lexloc(pJlex);
	if (pJlex->m_jtok == JTOK_PeriodPeriod && grfpdecl.FIsSet(FPDECL_AllowVariadic))
	{
		JtokNextToken(pJlex);

		CSTNode * pStnodVarArgs = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodVarArgs->m_jtok = (JTOK)pJlex->m_jtok;
		pStnodVarArgs->m_park = PARK_VariadicArg;
		return pStnodVarArgs;
	}

	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodCompound = nullptr;
	CSTNode * pStnodInit = nullptr;
	bool fAllowCompoundDecl = grfpdecl.FIsSet(FPDECL_AllowCompoundDecl);

	SJaiLexer jlexPeek = *pJlex;
	int cIdent = 0;
	while (1)
	{
		if (jlexPeek.m_jtok != JTOK_Identifier)
			return nullptr;

		CString strIdent = jlexPeek.m_str;

		++cIdent;
		JtokNextToken(&jlexPeek);

		if (fAllowCompoundDecl && FConsumeToken(&jlexPeek, JTOK(',')))
			continue;

		if ((jlexPeek.m_jtok != JTOK(':')) & (jlexPeek.m_jtok != JTOK_ColonEqual))
		{
			return nullptr;
		}

		break;
	}

	int cTypeNeeded = 0;
	CSTNode * pStnodDecl = nullptr;
	do
	{
		if (pStnodInit)
			ParseError(pParctx, pJlex, "Initializer must come after all comma separated declarations");

		CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pJlex);
		if (!EWC_FVERIFY(pStnodIdent, "parse failed during decl peek"))
			return nullptr;

		pStnodDecl = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodDecl->m_park = PARK_Decl;

		auto * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
		pStnodDecl->m_pStdecl = pStdecl;
		++cTypeNeeded;

		if (pStnodReturn)
		{
			if (!pStnodCompound)
			{
				pStnodCompound = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodCompound->m_park = PARK_Decl;

				auto * pStdeclCompound = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
				pStnodCompound->m_pStdecl = pStdeclCompound;

				pStdeclCompound->m_iStnodChildMin = pStnodCompound->IAppendChild(pStnodReturn);
				pStnodReturn = pStnodCompound;
			}
			pStnodCompound->m_pStdecl->m_iStnodChildMax = pStnodCompound->IAppendChild(pStnodDecl) + 1;
		}
		else
		{
			pStnodReturn = pStnodDecl;
		}

		// NOTE: May not resolve symbols (symtab is null if this is a procedure reference)
		if (pSymtab)
		{
			pStnodIdent->m_pSym = pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, StrFromIdentifier(pStnodIdent), pStnodDecl);
		}

		pStdecl->m_iStnodIdentifier = pStnodDecl->IAppendChild(pStnodIdent);

		if (FConsumeToken(pJlex, JTOK_ColonEqual))
		{
			pStnodInit = PStnodParseExpression(pParctx, pJlex);
		}
		else if (FConsumeToken(pJlex, JTOK(':')))
		{
			if (pStnodCompound)
			{
				EWC_ASSERT(cTypeNeeded, "No compound children?");

				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);

				int iStnodChild = pStnodCompound->m_pStdecl->m_iStnodChildMax - cTypeNeeded;
				int iChild = 0;
				for ( ; iStnodChild < pStnodCompound->m_pStdecl->m_iStnodChildMax; ++iStnodChild)
				{
					auto pStnodChild = pStnodCompound->PStnodChild(iStnodChild);

					EWC_ASSERT(pStnodChild->m_pStdecl->m_iStnodType == -1, "shouldn't set the type child twice");

					auto pStnodTypeCopy = (iChild == 0) ? pStnodType : PStnodCopy(pParctx->m_pAlloc, pStnodType);
					++iChild;

					pStnodChild->m_pStdecl->m_iStnodType = pStnodChild->IAppendChild(pStnodTypeCopy);
				}

				cTypeNeeded = 0;
			}
			else
			{
				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pJlex);
				pStdecl->m_iStnodType = pStnodDecl->IAppendChild(pStnodType);
				cTypeNeeded = 0;
			}

			if (FConsumeToken(pJlex, JTOK('=')))
			{
				if (FConsumeToken(pJlex, JTOK_TripleMinus))
				{
					if (!grfpdecl.FIsSet(FPDECL_AllowUninitializer))
					{
						ParseError(pParctx, pJlex, "--- uninitializer not allowed in parameter lists");
					}
					else
					{
						SLexerLocation lexloc(pJlex);
						pStnodInit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
						pStnodInit->m_jtok = JTOK_TripleMinus;
						pStnodInit->m_park = PARK_Uninitializer;
					}
				}
				else
				{
					pStnodInit = PStnodParseExpression(pParctx, pJlex);
					if (!pStnodInit)
						ParseError(pParctx, pJlex, "initial value expected before %s", PCozCurrentToken(pJlex));
				}
			}
			else if (FConsumeToken(pJlex, JTOK(':')))
			{
				if (pStnodCompound)
					ParseError(pParctx, pJlex, "Comma separated declarations not supported for constants");

				pStnodDecl->m_park = PARK_ConstantDecl;
				pStnodInit = PStnodParseExpression(pParctx, pJlex);
				if (!pStnodInit)
					ParseError(pParctx, pJlex, "initial value expected before %s", PCozCurrentToken(pJlex));
			}
		}

	} while (fAllowCompoundDecl && FConsumeToken(pJlex, JTOK(',')));

	pStnodReturn->m_pStdecl->m_iStnodInit = pStnodReturn->IAppendChild(pStnodInit);

	ValidateDeclaration(pParctx, pJlex, pStnodReturn);
	return pStnodReturn;
}

CSTNode * PStnodParseDecl(CParseContext * pParctx, SJaiLexer * pJlex)
{
	// stand alone declaration statement

	GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl;
	auto * pStnod =  PStnodParseParameter(pParctx, pJlex, pParctx->m_pSymtab, grfpdecl);
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

CSTNode * PStnodParseReturnArrow(CParseContext * pParctx, SJaiLexer * pJlex)
{
	if (FConsumeToken(pJlex, JTOK_Arrow))
	{
		// TODO : handle multiple return types

		return PStnodParseTypeSpecifier(pParctx, pJlex);
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

		return pStnodVoid;
	}
}

CSTNode * PStnodParseParameterList(CParseContext * pParctx, SJaiLexer * pJlex, CSymbolTable * pSymtabProc)
{
	SLexerLocation lexloc(pJlex);
	if (pSymtabProc)
	{
		PushSymbolTable(pParctx, pSymtabProc, lexloc);
	}

	GRFPDECL grfpdecl = FPDECL_AllowVariadic;
	CSTNode * pStnodParam = PStnodParseParameter(pParctx, pJlex, pSymtabProc, grfpdecl);
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
			pStnodParam = PStnodParseParameter(pParctx, pJlex, pSymtabProc, grfpdecl);

			if (!pStnodParam)
			{
				auto strUnexpected = StrUnexpectedToken(pJlex);
				ParseError(pParctx, pJlex, "expected parameter declaration before '%s'", strUnexpected.PCoz());
				break;
			}

			fHasVarArgs |= pStnodParam->m_park == PARK_VariadicArg;
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

	if (pSymtabProc)
	{
		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (list)");
	}

	return pStnodList;
}

CSTNode * PStnodSpoofEnumConstant(CParseContext * pParctx, const SLexerLocation & lexloc, const CString & strIdent, PARK park)
{
	CSTNode * pStnodConstant = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodConstant->m_park = park;
	pStnodConstant->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);

	auto * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
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

	auto * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
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

		EmitError(pErrman, &pStnodConstant->m_lexloc, "Enum constant name '%s' has already been defined at (%d, %d)", strIdent.PCoz(), iLine, iCol);
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
			
			bool fIsProcedure = false;
			INLINEK inlinek = INLINEK_Nil;
			if (FConsumeToken(pJlex, JTOK('(')))
			{
				fIsProcedure = true;
			}
			else if (pJlex->m_jtok == JTOK_ReservedWord )
			{
				// Note: It seems wrong for inline to go before the parenthesis, but that's how jBlow did it and it's 
				// not worth the deviation.

				auto rword = RwordLookup(pJlex);
				if ((rword == RWORD_Inline) | (rword == RWORD_NoInline))
				{
					fIsProcedure = true;
					inlinek = (rword == RWORD_Inline) ? INLINEK_AlwaysInline : INLINEK_NoInline;
					JtokNextToken(pJlex);

					Expect(pParctx, pJlex, JTOK('('));
				}
			}


			// function definition
			if (fIsProcedure)
			{
				CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodProc->m_park = PARK_ProcedureDefinition;
				pStnodProc->m_grfstnod.AddFlags(FSTNOD_EntryPoint);

				CSTProcedure * pStproc = EWC_NEW(pParctx->m_pAlloc, CSTProcedure) CSTProcedure();
				pStnodProc->m_pStproc = pStproc;
				pStproc->m_iStnodProcName = pStnodProc->IAppendChild(pStnodIdent);
				pStproc->m_pStnodParentScope = pParctx->m_pStnodScope;

				const CString & strName = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabProc = pParctx->m_pWork->PSymtabNew(strName);

				// BB - don't mangle the main function so the linker can find it. yuck.
				pStproc->m_fUseUnmangledName |= (strName == "main");

				CSTNode * pStnodParams = PStnodParseParameterList(pParctx, pJlex, pSymtabProc);
				pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
				Expect(pParctx, pJlex, JTOK(')'));

				auto pStnodReturns = PStnodParseReturnArrow(pParctx, pJlex);
				pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);

				CALLCONV callconv = CALLCONV_Nil;
				pStproc->m_iStnodBody = -1;
				if (pJlex->m_jtok == JTOK_ReservedWord)
				{
					while (pJlex->m_jtok == JTOK_ReservedWord)
					{
						RWORD rwordLookup = RwordLookup(pJlex);
						JtokNextToken(pJlex);
						switch (rwordLookup)
						{
						case RWORD_ForeignDirective:
							{
								pStproc->m_fIsForeign = true;
								pStproc->m_fUseUnmangledName = true;

								if (pJlex->m_jtok == JTOK_Identifier)
								{
									auto pStnodAlias = PStnodParseIdentifier(pParctx, pJlex);
									pStproc->m_iStnodForeignAlias = pStnodProc->IAppendChild(pStnodAlias);

									//JtokNextToken(pJlex);
								}
							} break;
						case RWORD_CDecl:	callconv = CALLCONV_CX86;	break;
						case RWORD_StdCall: callconv = CALLCONV_StdcallX86;	break;
						default:
							{
								ParseError(
									pParctx,
									pJlex,
									"Unexpected token following procedure declaration %s\n",
									PCozFromRword(rwordLookup));
							} break;
						}
					}

					Expect(pParctx, pJlex, JTOK(';'), "While parsing foreign directive");
				}

				if (pJlex->m_jtok == JTOK('{'))
				{
					auto pStnodScopePrev = pParctx->m_pStnodScope;
					pParctx->m_pStnodScope = pStnodProc;
					CSTNode * pStnodBody = PStnodParseCompoundStatement(pParctx, pJlex, pSymtabProc);
					pParctx->m_pStnodScope = pStnodScopePrev;

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
							strName.PCoz());
						pStproc->m_iStnodBody = -1;
					}
				}
				else if (pStproc->m_iStnodBody == -1)
				{
					ParseError(pParctx, pJlex, "Procedure definition for '%s' has no body", strName.PCoz());
				}

				int cStnodParams;
				CSTNode ** ppStnodParams = PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

				//int cStnodReturns = CChildrenInList(pStnodReturns, ppStnodReturns, PARK_Uhhhh);
				CSTNode ** ppStnodReturns = &pStnodReturns;
				int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;

				size_t cBAlloc = CBAlign(sizeof(STypeInfoProcedure), EWC_ALIGN_OF(STypeInfo *));
				cBAlloc = cBAlloc +	(cStnodParams + cStnodReturns) * sizeof(STypeInfo *);

				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc,8);

				STypeInfoProcedure * pTinproc = new(pB) STypeInfoProcedure(strName.PCoz());
				STypeInfo ** ppTin = (STypeInfo**)PVAlign( pB + sizeof(STypeInfoProcedure), 
																		EWC_ALIGN_OF(STypeInfo *));
				pTinproc->m_arypTinParams.SetArray(ppTin, 0, cStnodParams);
				pTinproc->m_arypTinReturns.SetArray(&ppTin[cStnodParams], 0, cStnodReturns);
				pStnodProc->m_pTin = pTinproc;
				pTinproc->m_pStnodDefinition = pStnodProc;
				pTinproc->m_callconv = callconv;
				pTinproc->m_inlinek = inlinek;

				CSTNode ** ppStnodParamMax = &ppStnodParams[cStnodParams];
				for ( ; ppStnodParams != ppStnodParamMax; ++ppStnodParams)
				{
					CSTNode * pStnodParam = *ppStnodParams;
					if (pStnodParam->m_park == PARK_VariadicArg)
					{
						pTinproc->m_fHasVarArgs = true;
					}
					else if (pStnodParam->m_park == PARK_Decl, "Expected decl")
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
								ParseError(pParctx, pJlex, "Procedure '%s' is missing return statement", strName.PCoz());
							}
						}
					}
				}

				auto pErrman = pParctx->m_pWork->m_pErrman;
				SSymbol * pSymProc = pSymtabParent->PSymEnsure(pErrman, StrFromIdentifier(pStnodIdent), pStnodProc);
				pSymProc->m_pTin = pTinproc;
				pStnodProc->m_pSym = pSymProc;

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
				(void) pSymtabEnum->PSymEnsure(pErrman, "loose", pStnodEnum, FSYM_IsType);
				(void) pSymtabEnum->PSymEnsure(pErrman, "strict", pStnodEnum, FSYM_IsType);

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

				STypeInfoEnum * pTinenum = new(pB) STypeInfoEnum(strIdent.PCoz());
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
					auto pStnodMemberIt = *ppStnodMemberIt;
					switch (pStnodMemberIt->m_park)
					{
					case PARK_Decl:			
						{
							auto pStdecl = pStnodMemberIt->m_pStdecl;
							if (EWC_FVERIFY(pStdecl, "expected stdecl"))
							{
								if (pStdecl->m_iStnodChildMin == -1)	
								{
									++cStnodField; 
								}
								else
								{
									cStnodField += pStdecl->m_iStnodChildMax - pStdecl->m_iStnodChildMin;
								}
							}
						} break;
					case PARK_ConstantDecl:		break;
					case PARK_StructDefinition:	break;
					case PARK_EnumDefinition:	break;
					case PARK_Typedef:			break;
					default: EWC_ASSERT(false, "Unexpected member in structure %s", strIdent.PCoz());
					}
				}

				size_t cBAlloc = CBAlign(sizeof(STypeInfoStruct), EWC_ALIGN_OF(STypeStructMember)) + 
								cStnodField * sizeof(STypeStructMember);
				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoStruct * pTinstruct = new(pB) STypeInfoStruct(strIdent.PCoz());
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

					auto pStdecl = pStnodMember->m_pStdecl;
					if (EWC_FVERIFY(pStdecl, "expected stdecl"))
					{
						if (pStdecl->m_iStnodChildMin == -1)	
						{
							auto pTypememb = pTinstruct->m_aryTypemembField.AppendNew();
							pTypememb->m_pStnod = pStnodMember;
						}
						else
						{
							int iStnodChildMax = pStdecl->m_iStnodChildMax;
							for (int iStnod = pStdecl->m_iStnodChildMin; iStnod < iStnodChildMax; ++iStnod)
							{
								auto pTypememb = pTinstruct->m_aryTypemembField.AppendNew();
								pTypememb->m_pStnod = pStnodMember->PStnodChild(iStnod);
							}
						}
					}

					int cTypememb = pTinstruct->m_aryTypemembField.C();
					for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
					{
						auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

						auto pStnodChild = pTypememb->m_pStnod;
						auto pStnodMemberIdent = pStnodChild->PStnodChildSafe(pStnodChild->m_pStdecl->m_iStnodIdentifier);
						pTypememb->m_strName = StrFromIdentifier(pStnodMemberIdent);
						pTypememb->m_pTin = pStnodChild->m_pTin;
					}
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
					ParseError(pParctx, pJlex, "missing type value for typedef %s", strIdent.PCoz());

					pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
					return nullptr;
				}

				CSTNode * pStnodTypedef = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodTypedef->m_park = PARK_Typedef;

				(void) pStnodTypedef->IAppendChild(pStnodIdent);
				(void) pStnodTypedef->IAppendChild(pStnodType);

				CSymbolTable * pSymtab = pParctx->m_pSymtab;
				auto pErrman = pParctx->m_pWork->m_pErrman;

				auto pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodTypedef, FSYM_IsType);
				pStnodTypedef->m_pSym = pSym;

				return pStnodTypedef;
			}

			// constant decl

			auto pStnodInit = PStnodParseExpression(pParctx, pJlex);
			Expect(pParctx, pJlex, JTOK(';'));
			
			CString strIdent = StrFromIdentifier(pStnodIdent);
			if (!pStnodInit)
			{
				ParseError(pParctx, pJlex, "missing constant value for %s", strIdent.PCoz());

				pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
				return nullptr;
			}

			CSTNode * pStnodConst = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodConst->m_park = PARK_ConstantDecl;

			auto * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
			pStnodConst->m_pStdecl = pStdecl;

			pStdecl->m_iStnodIdentifier = pStnodConst->IAppendChild(pStnodIdent);
			pStdecl->m_iStnodInit = pStnodConst->IAppendChild(pStnodInit);

			CSymbolTable * pSymtab = pParctx->m_pSymtab;
			auto pErrman = pParctx->m_pWork->m_pErrman;
			pStnodIdent->m_pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodConst);

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

		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_jtok = JTOK('{');
		pStnodList->m_park = PARK_List;
		pStnodList->m_pSymtab = pSymtab;

		if (!pSymtab)
		{
			pSymtab = pParctx->m_pWork->PSymtabNew("anon");
			pStnodList->m_pSymtab = pSymtab;
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
		if (!pStnodStatement)
		{
			ParseError( pParctx, pJlex, "Error parsing if statement. unexpected token '%s'", PCozFromJtok((JTOK)pJlex->m_jtok));

			// move the lexer forward until it has some hope of generating decent errors
			static const JTOK s_aJtok[] = {JTOK(';'), JTOK('{') };
			SkipToToken(pJlex, s_aJtok, EWC_DIM(s_aJtok));
			return pStnodIf;
		}
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
		//for decl := iterMake {}
		//for decl : iterType = iterMake {}
		//for decl = iterMake {}
		//for iter {}
		//and maybe anonymous iterator...    for : iterMake {}			(I like the simplicity of this, but it's not really cohesive)
		//and maybe anonymous iterator...    for --- := iterMake {}		(This one matches the other syntaxes, but is ugly and verbose)

		CSTNode * pStnodFor = PStnodParseReservedWord(pParctx, pJlex, RWORD_For);
		
		auto * pStfor = EWC_NEW(pParctx->m_pAlloc, CSTFor) CSTFor();
		pStnodFor->m_pStfor = pStfor;

		SLexerLocation lexloc(pJlex);
		CSymbolTable * pSymtabLoop = pParctx->m_pWork->PSymtabNew("for");
		pStnodFor->m_pSymtab = pSymtabLoop;

		PushSymbolTable(pParctx, pSymtabLoop, lexloc);
		CSTNode * pStnodDecl = PStnodParseParameter(pParctx, pJlex, pSymtabLoop, FPDECL_None);
		CSTNode * pStnodIterator = nullptr;
		if (pStnodDecl)
		{
			pStfor->m_iStnodDecl = pStnodFor->IAppendChild(pStnodDecl);

			auto pStdecl = pStnodDecl->m_pStdecl;
			if (EWC_FVERIFY(pStdecl, "bad declaration in for loop"))
			{
				pStnodIterator = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodIdentifier);
			}
		}
		else
		{
			pStnodIterator = PStnodParseCastExpression(pParctx, pJlex);
			EWC_ASSERT(pStnodIterator, "null iterator");
			pStfor->m_iStnodIterator = pStnodFor->IAppendChild(pStnodIterator);

			if (FConsumeToken(pJlex, JTOK('=')))
			{
				auto pStnodInit = PStnodParseExpression(pParctx, pJlex);
				pStfor->m_iStnodInit = pStnodFor->IAppendChild(pStnodInit);
			}

			if (pStfor->m_iStnodIterator == -1 && pStfor->m_iStnodInit == -1)
			{
				ParseError(pParctx, pJlex, "Could not determine for loop iterator");
			}
		}

		if (!pStnodIterator)
		{
			ParseError(pParctx, pJlex, "Could not determine iterator used by for loop");
		}
		else
		{
			// add iterIsDone AST
			{
				CSTNode * pStnodPredIdent = PStnodAllocateIdentifier(pParctx, lexloc, "iterIsDone");

				CSTNode * pStnodArg = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArg->m_jtok = JTOK_Reference;
				pStnodArg->m_park = PARK_UnaryOp;
				pStnodArg->IAppendChild(PStnodCopy(pParctx->m_pAlloc, pStnodIterator));

				CSTNode * pStnodCall = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodCall->m_jtok = JTOK(pJlex->m_jtok);
				pStnodCall->m_park = PARK_ProcedureCall;
				pStnodCall->IAppendChild(pStnodPredIdent);
				pStnodCall->IAppendChild(pStnodArg);

				pStfor->m_iStnodPredicate = pStnodFor->IAppendChild(pStnodCall);
			}

			// add iterNext 
			{
				CSTNode * pStnodIncIdent = PStnodAllocateIdentifier(pParctx, lexloc, "iterNext");

				CSTNode * pStnodArg = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArg->m_jtok = JTOK_Reference;
				pStnodArg->m_park = PARK_UnaryOp;
				pStnodArg->IAppendChild(PStnodCopy(pParctx->m_pAlloc, pStnodIterator));

				CSTNode * pStnodCall = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodCall->m_jtok = JTOK(pJlex->m_jtok);
				pStnodCall->m_park = PARK_ProcedureCall;
				pStnodCall->IAppendChild(pStnodIncIdent);
				pStnodCall->IAppendChild(pStnodArg);

				pStfor->m_iStnodIncrement = pStnodFor->IAppendChild(pStnodCall);
			}

		}

		CSTNode * pStnodStatement = PStnodParseStatement(pParctx, pJlex);
		pStfor->m_iStnodBody = pStnodFor->IAppendChild(pStnodStatement);

		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pSymtabLoop == pSymtabPop, "CSymbol table push/pop mismatch (list)");

		return pStnodFor;
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
					(void) pWork->PFileEnsure(pJlex->m_str.PCoz(), CWorkspace::FILEK_Source);
				}
				else if (EWC_FVERIFY(rword == RWORD_ForeignLibraryDirective, "unknown directive"))
				{
					(void) pWork->PFileEnsure(pJlex->m_str.PCoz(), CWorkspace::FILEK_Library);
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
			ParseError( pParctx, pJlex, "Unexpected token at global scope '%s'", PCozCurrentToken(pJlex));
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


CSymbolTable::CSymbolIterator::CSymbolIterator(
	CSymbolTable * pSymtab,
	const CString & str,
	const SLexerLocation & lexloc,
	GRFSYMLOOK grfsymlook)
:m_pSymtab(pSymtab)
,m_pSym(nullptr)
,m_lexloc(lexloc)
,m_grfsymlook(grfsymlook)
{
	m_pSym = pSymtab->PSymLookup(str, lexloc, grfsymlook, &m_pSymtab);
}

SSymbol * CSymbolTable::CSymbolIterator::PSymNext()
{
	if (!m_pSym)
		return nullptr;

	SSymbol * apSym[3] = {nullptr, nullptr, nullptr}; // cur, next, buffer
	int ipSym = 0;

	bool fIsOrdered = m_pSymtab->m_grfsymtab.FIsSet(FSYMTAB_Ordered) && !m_grfsymlook.FIsSet(FSYMLOOK_IgnoreOrder);
	auto pSymIt = m_pSym;
	SSymbol * pSymReturn = nullptr;
	while (pSymIt)
	{
		SLexerLocation lexlocSym = (pSymIt->m_pStnodDefinition) ? pSymIt->m_pStnodDefinition->m_lexloc : SLexerLocation();
		if ((fIsOrdered == false) | (lexlocSym <= m_lexloc))
		{
			apSym[ipSym++] = pSymIt;
			if (pSymIt->m_pSymPrev)
				apSym[ipSym++] = pSymIt->m_pSymPrev;

			break;
		}
		pSymIt = pSymIt->m_pSymPrev;
	}
	if (ipSym >= 2)
	{
		m_pSym = apSym[1];
		return apSym[0];
	}

	CSymbolTable * pSymtabIt = (m_grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? m_pSymtab->m_pSymtabParent : nullptr;
	while (pSymtabIt)
	{
		SSymbol ** ppSym = pSymtabIt->m_hashHvPSym.Lookup(m_pSym->m_strName.Hv()); 

		if (ppSym)
		{
			bool fIsOrdered = pSymtabIt->m_grfsymtab.FIsSet(FSYMTAB_Ordered);
			pSymIt = *ppSym;
			while (pSymIt)
			{
				SLexerLocation lexlocSym = (pSymIt->m_pStnodDefinition) ? pSymIt->m_pStnodDefinition->m_lexloc : SLexerLocation();
				if ((fIsOrdered == false) | (lexlocSym <= m_lexloc))
				{
					apSym[ipSym++] = pSymIt;
					if (pSymIt->m_pSymPrev)
					{
						apSym[ipSym++] = pSymIt->m_pSymPrev;
					}
					m_pSymtab = pSymtabIt;

					if (ipSym >= 2)
					{
						m_pSym = apSym[1];
						return apSym[0];
					}
				}
			}
		}

		pSymtabIt = pSymtabIt->m_pSymtabParent;
	}

	m_pSym = apSym[1];
	return apSym[0];
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
	STypeInfo * pTin = EWC_NEW(pSymtab->m_pAlloc, STypeInfo) STypeInfo(strName.PCoz(), tink);

	pSymtab->AddBuiltInType(pErrman, nullptr, pTin);
}

void AddBuiltInInteger(SErrorManager * pErrman, CSymbolTable * pSymtab, const CString & strName, u32 cBit, bool fSigned)
{
	STypeInfoInteger * pTinint = EWC_NEW(pSymtab->m_pAlloc, STypeInfoInteger) STypeInfoInteger(strName.PCoz(), cBit, fSigned);
	pSymtab->AddBuiltInType(pErrman, nullptr, pTinint);
}

void AddBuiltInFloat(SErrorManager * pErrman, CSymbolTable * pSymtab, const CString & strName, u32 cBit)
{
	STypeInfoFloat * pTinfloat = EWC_NEW(pSymtab->m_pAlloc, STypeInfoFloat) STypeInfoFloat(strName.PCoz(), cBit);
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
	AddBuiltInInteger(pErrman, this, "char", 32, false);
	AddBuiltInInteger(pErrman, this, "u64", 64, false);

	AddBuiltInInteger(pErrman, this, "s8", 8, true);
	AddBuiltInInteger(pErrman, this, "s16", 16, true);
	AddBuiltInInteger(pErrman, this, "s32", 32, true);
	AddBuiltInInteger(pErrman, this, "s64", 64, true);

#if EWC_X64
	AddBuiltInInteger(pErrman, this, "int", 64, true);
	AddBuiltInInteger(pErrman, this, "uint", 64, false);
#else
	AddBuiltInInteger(pErrman, this, "int", 32, true);
	AddBuiltInInteger(pErrman, this, "uint", 32, false);
#endif

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
			//EmitError(pErrman, &lexloc, "Shadowing symbol '%s' in unordered symbol table", strName.PCoz());
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

SSymbol * CSymbolTable::PSymLookup(const CString & str, const SLexerLocation & lexloc, GRFSYMLOOK grfsymlook, CSymbolTable ** ppSymtabOut)
{
	if (ppSymtabOut)
		*ppSymtabOut = this;

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
			SSymbol * pSym = *ppSym;
			bool fIsOrdered = pSymtab->m_grfsymtab.FIsSet(FSYMTAB_Ordered);
			while (pSym)
			{
				SLexerLocation lexlocSym = (pSym->m_pStnodDefinition) ? pSym->m_pStnodDefinition->m_lexloc : SLexerLocation();
				if ((fIsOrdered == false) | (lexlocSym <= lexlocChild))
				{
					if (ppSymtabOut)
						*ppSymtabOut = pSymtab;
					return pSym;
				}
				pSym = pSym->m_pSymPrev;
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

STypeInfoPointer * CSymbolTable::PTinptrAllocReference(STypeInfo * pTinPointedTo)
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
		EmitError(pErrman, &lexloc, "Two types encountered with same name (%s)", strName.PCoz());
	}
}

void CSymbolTable::PrintDump()
{
	printf("Symbols:\n");
	EWC::CHash<HV, SSymbol *>::CIterator iter(&m_hashHvPSym);
	while (SSymbol ** ppSym = iter.Next())
	{
		SSymbol * pSym = *ppSym;
		printf("%p: %s : '%s'\n",pSym, pSym->m_strName.PCoz(), (pSym->m_pTin) ? pSym->m_pTin->m_strName.PCoz() : "Nill");
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
,m_pStfor(nullptr)
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
		m_arypStnodChild[ipStnod-1] = nullptr;
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

	if (m_pStfor)
	{
		pAlloc->EWC_DELETE(m_pStfor);
		m_pStfor = nullptr;
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

void PrintTypeInfo(EWC::SStringBuffer * pStrbuf, STypeInfo * pTin, PARK park, GRFDBGSTR grfdbgstr)
{
	if (pTin == nullptr)
	{
		switch (park)
		{
		case PARK_List:				AppendCoz(pStrbuf, "{}");		return;
		case PARK_Identifier:		AppendCoz(pStrbuf, "Ident");	return;
		case PARK_ParameterList:	AppendCoz(pStrbuf, "Params");	return; 
		case PARK_VariadicArg:		AppendCoz(pStrbuf, "..");		return;
		case PARK_Nop:				AppendCoz(pStrbuf, "Nop");		return;
		case PARK_Uninitializer:	AppendCoz(pStrbuf, "---");		return;
		case PARK_AssignmentOp:		AppendCoz(pStrbuf, "=");		return;
		default:					AppendCoz(pStrbuf, "???");		return;
		}
	}

	switch (pTin->m_tink)
	{
	case TINK_Pointer:		
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer*)pTin;
			AppendCoz(pStrbuf, PCozFromJtok(JTOK_Reference));
			PrintTypeInfo(pStrbuf, pTinptr->m_pTinPointedTo, park, grfdbgstr);
			return;
		}
	case TINK_Array:
		{
			STypeInfoArray * pTinary = (STypeInfoArray*)pTin;

			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:		FormatCoz(pStrbuf, "[%d]", pTinary->m_c);	break;
			case ARYK_Dynamic:		AppendCoz(pStrbuf, "[..]");				break;
			case ARYK_Reference:	AppendCoz(pStrbuf, "[]");					break;
			}

			PrintTypeInfo(pStrbuf, pTinary->m_pTin, park, grfdbgstr);
			return;
		}

	case TINK_Literal:		
		{
			AppendCoz(pStrbuf, "Literal");
			return;
		}
    case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;
			FormatCoz(pStrbuf, "%s(", pTin->m_strName.PCoz());

			size_t cpTin = pTinproc->m_arypTinParams.C();
			size_t cCommas = (pTinproc->m_fHasVarArgs) ? cpTin : cpTin - 1;
			for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
			{
				PrintTypeInfo(pStrbuf, pTinproc->m_arypTinParams[ipTin], PARK_Nil, grfdbgstr);

				if (ipTin < cCommas)
				{
					AppendCoz(pStrbuf, ", ");
				}
			}

			if (pTinproc->m_fHasVarArgs)
			{
				AppendCoz(pStrbuf, "..");
			}

			AppendCoz(pStrbuf, ")->");

			cpTin = pTinproc->m_arypTinReturns.C();
			for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
			{
				PrintTypeInfo(pStrbuf, pTinproc->m_arypTinReturns[ipTin], PARK_Nil, grfdbgstr);
			}
			return;
		}
    case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;
			FormatCoz(pStrbuf, "%s_struct", pTin->m_strName.PCoz());
			return;
		}
    case TINK_Enum:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;
			FormatCoz(pStrbuf, "%s_enum", pTin->m_strName.PCoz());
			return;
		}
	case TINK_Integer:
		{
			if (!grfdbgstr.FIsSet(FDBGSTR_UseSizedNumerics))
			{
				AppendCoz(pStrbuf, pTin->m_strName.PCoz()); 
				return;
			}

			// print out the size resolved type (rather than any type aliases - ie. int)
			auto pTinint = (STypeInfoInteger *)pTin;
			char chSigned = (pTinint->m_fIsSigned) ? 's' : 'u';
			switch(pTinint->m_cBit)
			{
			case 8:		FormatCoz(pStrbuf, "%c8", chSigned);	break;
			case 16:	FormatCoz(pStrbuf, "%c16", chSigned);	break;
			case 32:	FormatCoz(pStrbuf, "%c32", chSigned);	break;
			case 64:	FormatCoz(pStrbuf, "%c64", chSigned);	break;
			default: EWC_ASSERT(false, "unknown integer size");
			}

			return;
		}
    case TINK_Float:
		{
			if (!grfdbgstr.FIsSet(FDBGSTR_UseSizedNumerics))
			{
				AppendCoz(pStrbuf, pTin->m_strName.PCoz()); 
				return;
			}

			// print out the size resolved type (rather than any type aliases - ie. double)
			auto pTinfloat = (STypeInfoFloat *)pTin;
			AppendCoz(pStrbuf, (pTinfloat->m_cBit == 32) ? "f32" : "f64");
			return;
		}
    case TINK_Bool:			// fall through ...
    case TINK_String:		// fall through ...
    case TINK_Void:			// fall through ...
    case TINK_Null:			// fall through ...
    case TINK_Any:			// fall through ...
	default:
		AppendCoz(pStrbuf, pTin->m_strName.PCoz()); 
		return;
	}
}

void PrintStnodName(EWC::SStringBuffer * pStrbuf, CSTNode * pStnod)
{
	switch (pStnod->m_park)
	{
	case PARK_Identifier:			FormatCoz(pStrbuf, "$%s", StrFromIdentifier(pStnod).PCoz());	return;
	case PARK_ReservedWord:			AppendCoz(pStrbuf, PCozFromRword(pStnod->m_pStval->m_rword));	return;
	case PARK_Nop:					AppendCoz(pStrbuf, "nop");										return;
	case PARK_Literal:				
		{
			switch (pStnod->m_pStval->m_stvalk)
			{
			case STVALK_String:			FormatCoz(pStrbuf, "\"%s\"", pStnod->m_pStval->m_str.PCoz());	return;
			case STVALK_UnsignedInt:	FormatCoz(pStrbuf, "%llu", pStnod->m_pStval->m_nUnsigned);		return;
			case STVALK_SignedInt:		FormatCoz(pStrbuf, "%lld", pStnod->m_pStval->m_nSigned);		return;
			case STVALK_Float:			FormatCoz(pStrbuf, "%f", pStnod->m_pStval->m_g);				return;
			default:
				EWC_ASSERT(false, "unknown literal %s", PCozFromJtok(pStnod->m_jtok));
				return;
			}
		}
	case PARK_AdditiveOp:		    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_MultiplicativeOp:	    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_ShiftOp:			    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_EqualityOp:		    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_RelationalOp:		    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_BitwiseAndOrOp:	    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_LogicalAndOrOp:	    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_UnaryOp:			    FormatCoz(pStrbuf, "unary[%s]", PCozFromJtok(pStnod->m_jtok));		return;
	case PARK_PostfixUnaryOp:		FormatCoz(pStrbuf, "postUnary[%s]", PCozFromJtok(pStnod->m_jtok));	return;
	case PARK_AssignmentOp:		    FormatCoz(pStrbuf, "%s", PCozFromJtok(pStnod->m_jtok));				return;
	case PARK_ArrayElement:		    AppendCoz(pStrbuf, "elem");					return;
	case PARK_MemberLookup:		    AppendCoz(pStrbuf, "member");				return;
	case PARK_ProcedureCall:		AppendCoz(pStrbuf, "procCall");				return;
	case PARK_List:				    AppendCoz(pStrbuf, "{}");					return;
	case PARK_ParameterList:	    AppendCoz(pStrbuf, "params");				return;
	case PARK_If:				    AppendCoz(pStrbuf, "if");					return;
	case PARK_Else:				    AppendCoz(pStrbuf, "else");					return;
	case PARK_ArrayDecl:		    AppendCoz(pStrbuf, "[]");					return;
	case PARK_ProcedureReferenceDecl:
									AppendCoz(pStrbuf, "procref");				return;
	case PARK_Uninitializer:		AppendCoz(pStrbuf, "---");					return;
	case PARK_ReferenceDecl:		AppendCoz(pStrbuf, "ptr");					return;
	case PARK_Decl:					AppendCoz(pStrbuf, "decl");					return;
	case PARK_Typedef:				AppendCoz(pStrbuf, "typedef");				return;
	case PARK_ConstantDecl:			AppendCoz(pStrbuf, "const");				return;
	case PARK_ProcedureDefinition:	AppendCoz(pStrbuf, "func");					return;
	case PARK_EnumDefinition:		AppendCoz(pStrbuf, "enum");					return;
	case PARK_StructDefinition:		AppendCoz(pStrbuf, "struct");				return;
	case PARK_EnumConstant:			AppendCoz(pStrbuf, "enumConst");			return;
	case PARK_VariadicArg:			AppendCoz(pStrbuf, "..");					return;
	case PARK_ArrayLiteral:			AppendCoz(pStrbuf, "arrayLit");				return;
	case PARK_Cast:					AppendCoz(pStrbuf, "cast");					return;
	case PARK_Error:
	default:						AppendCoz(pStrbuf, "error");				return;
	}
}

void PrintStnod(SStringBuffer * pStrbuf, CSTNode * pStnod, GRFDBGSTR grfdbgstr)
{
	if (grfdbgstr.FIsSet(FDBGSTR_Name))
	{
		PrintStnodName(pStrbuf, pStnod);
		grfdbgstr.Clear(FDBGSTR_Name);

		if (CBFree(*pStrbuf) > 1 && grfdbgstr != FDBGSTR_None)
		{
			*pStrbuf->m_pCozAppend++ = '|';
			*pStrbuf->m_pCozAppend = '\0';
		}
	}

	if (grfdbgstr.FIsSet(FDBGSTR_Type))
	{
		if (pStnod->m_park == PARK_Identifier && pStnod->m_pTin == nullptr)
		{
			PrintStnodName(pStrbuf, pStnod);
		}
		else
		{
			PrintTypeInfo(pStrbuf, pStnod->m_pTin, pStnod->m_park, grfdbgstr);
		}
		grfdbgstr.Clear(FDBGSTR_Type);
	}

	if (grfdbgstr.FIsSet(FDBGSTR_LiteralSize))
	{
		if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		{
			const SLiteralType & litty = ((STypeInfoLiteral *)pStnod->m_pTin)->m_litty;

			FormatCoz(pStrbuf, ":%s", PChzFromLitk(litty.m_litk));

			if (litty.m_cBit >= 0)
			{
				FormatCoz(pStrbuf, "%d", litty.m_cBit);
			}
		}
		grfdbgstr.Clear(FDBGSTR_LiteralSize);
	}
}

void CSTNode::WriteDebugString(EWC::SStringBuffer * pStrbuf, GRFDBGSTR grfdbgstr)
{
	if (m_park == PARK_Literal)
	{
		EWC_ASSERT(m_pStval, "operand without value struct");
		PrintStnod(pStrbuf, this, grfdbgstr);
		return;
	}
	if (m_park == PARK_Identifier)
	{
		EWC_ASSERT(m_pStident, "identifier operand without string struct");
		PrintStnod(pStrbuf, this, grfdbgstr);
		return;
	}

	AppendCoz(pStrbuf, "(");
	PrintStnod(pStrbuf, this, grfdbgstr);

	for (size_t ipStnod = 0; ipStnod < m_arypStnodChild.C(); ++ipStnod)
	{
		if (EWC_FVERIFY(CBFree(*pStrbuf) > 0, "debug string overflow"))
		{
			*pStrbuf->m_pCozAppend++ = ' ';
		}
		if (EWC_FVERIFY(CBFree(*pStrbuf) > 0, "debug string storage overflow"))
		{
			CSTNode * pStnod = m_arypStnodChild[ipStnod];
			pStnod->WriteDebugString(pStrbuf, grfdbgstr);
		}
	}

	if (EWC_FVERIFY(CBFree(*pStrbuf) > 1, "debug string overflow"))
	{
		*pStrbuf->m_pCozAppend++ = ')';
	}

	EnsureTerminated(pStrbuf, '\0');
}

void WriteDebugStringForEntries(CWorkspace * pWork, char * pCo, char * pCoMax, GRFDBGSTR grfdbgstr)
{
	auto cB = pCoMax - pCo;

#define TEST_STNODE_COPY 1
#if TEST_STNODE_COPY
	auto pCoCopy = (char *)alloca(cB);
	EWC::SStringBuffer strbufCopy(pCoCopy, cB);
#endif

	EWC::SStringBuffer strbuf(pCo, cB);
	for (size_t ipStnod = 0; ipStnod < pWork->m_aryEntry.C(); ++ipStnod)
	{
		CSTNode * pStnod = pWork->m_aryEntry[ipStnod].m_pStnod;
		pStnod->WriteDebugString(&strbuf, grfdbgstr);

		if ((CBFree(strbuf) > 0) & (ipStnod+1 != pWork->m_aryEntry.C()))
		{
			*strbuf.m_pCozAppend++ = ' ';
		}

#if TEST_STNODE_COPY
		CSTNode * pStnodCopy = PStnodCopy(pWork->m_pAlloc, pStnod);
		pStnodCopy->WriteDebugString(&strbufCopy, grfdbgstr);
		pWork->m_pAlloc->EWC_DELETE(pStnodCopy);

		if ((CBFree(strbufCopy) > 0) & (ipStnod+1 != pWork->m_aryEntry.C()))
		{
			*strbufCopy.m_pCozAppend++ = ' ';
		}
#endif
	}

	EnsureTerminated(&strbuf, '\0');

#if TEST_STNODE_COPY
	EnsureTerminated(&strbufCopy, '\0');
	EWC_ASSERT(FAreCozEqual(strbuf.m_pCozBegin, strbufCopy.m_pCozBegin), "AST copy failed to produce an identical copy.");
#endif
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
	const char * pCozIn,
	const char * pCozExpected,
	const char * apCozExpectedImport[] = nullptr,
	const char * apCozExpectedLibrary[] = nullptr)
{

#ifdef EWC_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	pWork->m_pAlloc->SetAltrac(pAltrac);
#endif

	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pCozIn);

	EWC_ASSERT(pWork->m_pErrman->m_cError == 0, "parse errors detected");
	pWork->m_pErrman->Clear();

	ParseGlobalScope(pWork, &jlex, true);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	WriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Name);

	EWC_ASSERT(FAreCozEqual(aCh, pCozExpected), "parse debug string doesn't match expected value");

	if (apCozExpectedImport)
	{
		int ipCoz;
		for (ipCoz = 0; ; ++ipCoz)
		{
			const char * pCoz= apCozExpectedImport[ipCoz];
			if (!pCoz)
				break;

			HV hvImport = HvFromPChz(pCoz);
			EWC_ASSERT(pWork->PFileLookup(hvImport, CWorkspace::FILEK_Source), "expected import %s", pCoz);
		}
		EWC_ASSERT(pWork->CFile(CWorkspace::FILEK_Source), "missing import");
	}
	
	if (apCozExpectedLibrary)
	{
		int ipCoz;
		for (ipCoz = 0; ; ++ipCoz)
		{
			const char * pCoz= apCozExpectedLibrary[ipCoz];
			if (!pCoz)
				break;

			HV hvImport = HvFromPChz(pCoz);
			EWC_ASSERT(pWork->PFileLookup(hvImport, CWorkspace::FILEK_Library), "expected import %s", pCoz);
		}
		EWC_ASSERT(pWork->CFile(CWorkspace::FILEK_Library) == ipCoz, "missing import");
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

	const char * pCozIn;
	const char * pCozOut;
	{
		SErrorManager errman;
		CWorkspace work(&alloc, &errman);

		pCozIn = "for it := iterMake(foo) { }";
		pCozOut = "(for (decl $it (procCall $iterMake $foo)) (procCall $iterIsDone (unary[&] $it)) (procCall $iterNext (unary[&] $it)) ({}))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "for it = iterMake(foo) { }";
		pCozOut = "(for $it (procCall $iterMake $foo) (procCall $iterIsDone (unary[&] $it)) (procCall $iterNext (unary[&] $it)) ({}))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = u8"😁+✂;";
		pCozOut = u8"(+ $😁 $✂)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "n1: int, g1, g2: float = ---;";
		pCozOut = "(decl (decl $n1 $int) (decl $g1 $float) (decl $g2 $float) (---))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "n1, n2: int, g: float;";
		pCozOut = "(decl (decl $n1 $int) (decl $n2 $int) (decl $g $float))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "(@ppFunc)(2);";
		pCozOut = "(procCall (unary[@] $ppFunc) 2)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "func: (n: s32)->s64 = fooFunc;";		// procedure reference declaration
		pCozOut = "(decl $func (procref (params (decl $n $s32)) $s64) $fooFunc)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "apFunc[2](2);";
		pCozOut = "(procCall (elem $apFunc 2) 2)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "pN = cast(& int) pG;";
		pCozOut = "(= $pN (cast (ptr $int) $pG))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "aN := {:int: 2, 4, 5}; ";
		pCozOut = "(decl $aN (arrayLit $int ({} 2 4 5)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ ENUMK :: enum int { ENUMK_Nil : -1, ENUMK_Foo, ENUMK_Bah : 3 } enumk := ENUMK.Foo; }";
		pCozOut = "({} (enum $ENUMK $int ({} (enumConst $nil) (enumConst $min) (enumConst $last) (enumConst $max) (arrayLit $names) (arrayLit $values) (enumConst $ENUMK_Nil (unary[-] 1)) (enumConst $ENUMK_Foo) (enumConst $ENUMK_Bah 3)))"
			" (decl $enumk (member $ENUMK $Foo)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SOut :: struct { SIn :: struct { ConstTwo :: 2; }} n := SOut.SIn.ConstTwo; ";
		pCozOut = "(struct $SOut ({} (struct $SIn ({} (const $ConstTwo 2))))) (decl $n (member (member $SOut $SIn) $ConstTwo))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "PtrType :: typedef & s16; ArrayType :: typedef [2] s8; ";
		pCozOut = "(typedef $PtrType (ptr $s16)) (typedef $ArrayType ([] 2 $s8))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SomeConst :: 0xFF; ";
		pCozOut = "(const $SomeConst 255)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SomeConst : s16 : 0xFF; ";
		pCozOut = "(const $SomeConst $s16 255)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SFoo :: struct { m_n := 2; } foo : SFoo; foo.m_n = 1; ";
		pCozOut = "(struct $SFoo ({} (decl $m_n 2))) (decl $foo $SFoo) (= (member $foo $m_n) 1)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "paN : & [4] int;";
		pCozOut = "(decl $paN (ptr ([] 4 $int)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "aN : [4] int; n := aN[0];";
		pCozOut = "(decl $aN ([] 4 $int)) (decl $n (elem $aN 0))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "x + 3*5;";
		pCozOut = "(+ $x (* 3 5))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "(ugh + foo) / ((x + 3)*5);";
		pCozOut = "(/ (+ $ugh $foo) (* (+ $x 3) 5))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ugh/foo/guh/ack;";
		pCozOut = "(/ (/ (/ $ugh $foo) $guh) $ack)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "(5 + -x) * -(3 / foo);";
		pCozOut = "(* (+ 5 (unary[-] $x)) (unary[-] (/ 3 $foo)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick * ack * -(3 / foo);";
		pCozOut = "(* (* $ick $ack) (unary[-] (/ 3 $foo)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick & (ack&&foo | 123) || guh;";
		pCozOut = "(|| (& $ick (&& $ack (| $foo 123))) $guh)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick == ack < foo\n != 123 >= guh;";
		pCozOut = "(!= (== $ick (< $ack $foo)) (>= 123 $guh))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		// NOTE - weird ordering shouldn't matter as we will ensure lhs is l-value
		pCozIn = "ick = 5 += foo *= guh;";
		pCozOut = "(*= (+= (= $ick 5) $foo) $guh)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "++foo.bah[23];";
		pCozOut = "(unary[++] (elem (member $foo $bah) 23))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ i=5; foo.bah = ack; }";
		pCozOut = "({} (= $i 5) (= (member $foo $bah) $ack))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ if i==foo.bar.fug ick = 3; else ick = 7; }";
		pCozOut = "({} (if (== $i (member (member $foo $bar) $fug)) (= $ick 3) (else (= $ick 7))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ while x > 0 { --x; } }";
		pCozOut = "({} (while (> $x 0) ({} (unary[--] $x))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ break; continue; return foo=\"test\"; }";
		pCozOut = "({} (break) (continue) (return (= $foo \"test\")))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "VarArgs :: (a : int, ..) #foreign;";
		pCozOut = "(func $VarArgs (params (decl $a $int) (..)) $void)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ AddNums :: (a : int, b := 1) -> int { return a + b; } bah := 3; }";
		pCozOut = "(func $AddNums (params (decl $a $int) (decl $b 1)) $int ({} (return (+ $a $b))))"
			" ({} (decl $bah 3))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "AddLocal :: (nA : int) -> int { nLocal := 2; return nA + nLocal; }";
		pCozOut = "(func $AddLocal (params (decl $nA $int)) $int ({} (decl $nLocal 2) (return (+ $nA $nLocal))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ AddNums :: (a : int, b := 1) -> int { return a + b; } AddNums(2, 3); }";
		pCozOut = "(func $AddNums (params (decl $a $int) (decl $b 1)) $int ({} (return (+ $a $b))))"
			" ({} (procCall $AddNums 2 3))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ FooFunc(); n:=BarFunc(x+(ack)); }";
		pCozOut = "({} (procCall $FooFunc) (decl $n (procCall $BarFunc (+ $x $ack))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ NopFunc :: () { guh := 2; } wha : & int; }";
		pCozOut = "(func $NopFunc $void ({} (decl $guh 2) (return))) ({} (decl $wha (ptr $int)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "STest :: struct { m_a := 2; m_b : int; } boo : int = 3;";
		pCozOut = "(struct $STest ({} (decl $m_a 2) (decl $m_b $int))) (decl $boo $int 3)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "pChz := foo; guh : gur = 5; bah : s32 = woo;";
		pCozOut = "(decl $pChz $foo) (decl $guh $gur 5) (decl $bah $s32 $woo)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ForeignFunc :: () -> int #foreign;";
		pCozOut = "(func $ForeignFunc $int)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);
			
		pCozIn = "#import \"foo/blah/ack\" #foreign_library \"foo/blah/ack\" "
				"#import \"test\\wha\\huh\" #foreign_library \"test\\wha\\huh\" "
				"#import \"basic\" ";

		pCozOut = "";
		const char * apChzExpectedImport[] = { "foo/blah/ack", "test\\wha\\huh", "basic",nullptr };
		const char * apChzExpectedLibrary[] = { "foo/blah/ack", "test\\wha\\huh", nullptr };
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut, apChzExpectedImport, apChzExpectedLibrary);

		StaticShutdownStrings(&allocString);
	}
}