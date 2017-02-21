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

#include "Parser.h"
#include "TypeInfo.h"
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

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtab);
CSTNode * PStnodExpectCompoundStatement(CParseContext * pParctx, SLexer * pLex, const char * pCozPriorStatement);
CSTNode * PStnodParseDefinition(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseExpression(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseLogicalAndOrExpression(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseParameterList(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtabProc);
CSTNode * PStnodParseReturnArrow(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseStatement(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SLexer * pLex);

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
		"Expression List",
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

const char * PChzFromCallconv(CALLCONV callconv)
{
	static const char * s_mpCallconvPChz[] =
	{
		"#x86",			//CALLCONV_CX86
		"#stdcall",		//CALLCONV_StdcallX86
		"#x64",			//CALLCONV_X64
	};
	EWC_CASSERT(EWC_DIM(s_mpCallconvPChz) == CALLCONV_Max, "missing CALLCONV string");
	if (callconv == CALLCONV_Nil)
		return "Nil";

	if ((callconv < CALLCONV_Nil) | (callconv >= CALLCONV_Max))
		return "Unknown CALLCONV";

	return s_mpCallconvPChz[callconv];
}

const char * PChzFromInlinek(INLINEK inlinek)
{
	static const char * s_mpInlinekPChz[] =
	{
		"inline",		// INLINEK_AlwaysInline
		"noinline",		// INLINEK_NoInline
	};
	EWC_CASSERT(EWC_DIM(s_mpInlinekPChz) == INLINEK_Max, "missing INLINEK string");
	if (inlinek == INLINEK_Nil)
		return "Nil";

	if ((inlinek < INLINEK_Nil) | (inlinek >= INLINEK_Max))
		return "Unknown INLINEK";

	return s_mpInlinekPChz[inlinek];
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

	if (pStnodSrc->m_pOptype)
	{
		pStnodDst->m_pOptype = EWC_NEW(pAlloc, SOpTypes) SOpTypes();
		*pStnodDst->m_pOptype = *pStnodSrc->m_pOptype;
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

void ParseError(CParseContext * pParctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pParctx->m_pWork->m_pErrman, &lexloc, pChzFormat, ap);
}

void ParseWarning(CParseContext * pParctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitWarning(pParctx->m_pWork->m_pErrman, &lexloc, pChzFormat, ap);
}

EWC::CString StrUnexpectedToken(SLexer * pLex)
{
	if (pLex->m_tok == TOK_Identifier)
	{
		return pLex->m_str;
	}
	return CString(PCozCurrentToken(pLex));
}

void SkipToToken(SLexer * pLex, TOK const * const aTok, int cTok, GRFLEXER grflexer)
{
	while (1)
	{
		bool fFound = pLex->m_grflexer.FIsSet(grflexer);
		TOK tok = (TOK)pLex->m_tok;
		if (tok == TOK_Eof)
			break;

		for (int iTok = 0; !fFound && iTok < cTok; ++iTok)
		{
			fFound |= (tok == aTok[iTok]);
		}

		if (fFound)
			break;
		TokNext(pLex);
	}
}

void Expect(CParseContext * pParctx, SLexer * pLex, TOK tokExpected, const char * pCozInfo = nullptr, ...)
{
	if (pLex->m_tok != tokExpected)
	{
		char aB[1024] = {0};
		if (pCozInfo)
		{
			va_list ap;
			va_start(ap, pCozInfo);
			vsprintf_s(aB, EWC_DIM(aB), pCozInfo, ap);
		}

		auto strUnexpected = StrUnexpectedToken(pLex);
		ParseError(pParctx, pLex, "Expected '%s' before '%s' %s", PCozFromTok(tokExpected), strUnexpected.PCoz(), aB);
	}
	else
	{
		TokNext(pLex);
	}
}

bool FIsEndOfStatement(SLexer * pLex)
{
	return pLex->m_tok == TOK(';') || pLex->m_tok == TOK('}') || pLex->m_tok == TOK_Eof || pLex->m_grflexer.FIsSet(FLEXER_EndOfLine);
}

void ExpectEndOfStatement(CParseContext * pParctx, SLexer * pLex, const char * pCozInfo = nullptr, ...)
{
	if (FIsEndOfStatement(pLex))
	{
		if (pLex->m_tok == TOK(';'))
		{
			TokNext(pLex);

			if (pLex->m_tok != TOK(';') && FIsEndOfStatement(pLex))
			{
				auto strUnexpected = StrUnexpectedToken(pLex);
				ParseWarning(pParctx, pLex, "Unnecessary c-style ';' found before '%s'", strUnexpected.PCoz());
			}
		}
	}
	else
	{
		char aB[1024] = {0};
		if (pCozInfo)
		{
			va_list ap;
			va_start(ap, pCozInfo);
			vsprintf_s(aB, EWC_DIM(aB), pCozInfo, ap);
		}

		auto strUnexpected = StrUnexpectedToken(pLex);
		ParseError(pParctx, pLex, "Expected end-of-line or ';' before '%s' %s", strUnexpected.PCoz(), aB);
	}
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

CSTNode * PStnodParseIdentifier(CParseContext * pParctx, SLexer * pLex)
{
	if (pLex->m_tok != TOK_Identifier)
		return nullptr;

	SLexerLocation lexloc(pLex);

	CString strIdent = pLex->m_str;
	auto pStnod = PStnodAllocateIdentifier(pParctx, lexloc, strIdent);
	pStnod->m_tok = TOK(pLex->m_tok);

	if (strIdent.FIsEmpty())
	{
		ParseError(pParctx, pLex, "Identifier with no string");
	}
	else if (strIdent.PCoz()[0] == '#')
	{
		ParseError(pParctx, pLex, "Unknown directive encountered %s", strIdent.PCoz());
	}

	TokNext(pLex);
	return pStnod;
}

CSTNode * PStnodParseReservedWord(CParseContext * pParctx, SLexer * pLex, RWORD rwordExpected = RWORD_Nil)
{
	if (pLex->m_tok != TOK_ReservedWord)
		return nullptr;

	RWORD rwordLookup = RwordLookup(pLex);
	if ((rwordExpected != RWORD_Nil) & (rwordExpected != rwordLookup))
	{
		ParseError(pParctx, pLex, "Expected %s before %s", PCozFromRword(rwordExpected), PCozCurrentToken(pLex));
		return nullptr;
	}

	SLexerLocation lexloc(pLex);
	CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

	pStnod->m_tok = TOK(pLex->m_tok);
	pStnod->m_park = PARK_ReservedWord;

	CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
	pStval->m_str = pLex->m_str;
	pStval->m_rword = rwordLookup;
	pStnod->m_pStval = pStval;

	TokNext(pLex);
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
	SLexer * pLex)
{
	SLexerLocation lexloc(pLex);

	CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
	CSTNode * pStnodList = nullptr;

	if (pStnodExp)
	{
		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_park = PARK_ExpressionList;
		pStnodList->IAppendChild(pStnodExp);

		while (FConsumeToken(pLex, TOK(',')))
		{
			pStnodExp = PStnodParseExpression(pParctx, pLex);

			if (!pStnodExp)
			{
				ParseError(pParctx, pLex, "Expected expression before %s", PCozCurrentToken(pLex));
				break;
			}
			pStnodList->IAppendChild(pStnodExp);
		}
	}

	return pStnodList;
}


CSTNode * PStnodParsePrimaryExpression(CParseContext * pParctx, SLexer * pLex)
{
	switch(pLex->m_tok)
	{
		case TOK_Identifier:
			{
				CSTNode * pStnod = PStnodParseIdentifier(pParctx, pLex);
				return pStnod;
			}
		case TOK_ReservedWord:
			{
				RWORD rword = RwordLookup(pLex);
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

				SLexerLocation lexloc(pLex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_tok = TOK(pLex->m_tok);
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
					pStval->m_str = pLex->m_str;
				}


				TokNext(pLex);
				return pStnod;
			}
		case TOK_Literal:
			{
				// NOTE - Negative literals don't exist until the type checking phase folds in the unary '-' operator

				SLexerLocation lexloc(pLex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_tok = TOK(pLex->m_tok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_str = pLex->m_str;
				pStval->m_litkLex = pLex->m_litk;

				if (pLex->m_litk == LITK_Float)
				{
					SetFloatValue(pStval, pLex->m_g);
				}
				else if (pLex->m_litk == LITK_String)
				{
					pStval->m_stvalk = STVALK_String;
				}
				else if (pLex->m_litk == LITK_Char)
				{
					SetUnsignedIntValue(pStval, pLex->m_n);
				}
				else
				{
					SetUnsignedIntValue(pStval, pLex->m_n);
				}

				pStnod->m_pStval = pStval;

				TokNext(pLex);
				return pStnod;
			} 
		case TOK('{'): // array literals
			{
				SLexerLocation lexloc(pLex);
				TokNext(pLex); // consume '{'

				CSTNode * pStnodLit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodLit->m_tok = TOK(pLex->m_tok);
				pStnodLit->m_park = PARK_ArrayLiteral;

				// We're using a decl here... may need a custom value structure
				auto * pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
				pStnodLit->m_pStdecl = pStdecl;

				if (FConsumeToken(pLex, TOK(':')))
				{
					auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex);
					pStdecl->m_iStnodType = pStnodLit->IAppendChild(pStnodType);

					Expect(pParctx, pLex, TOK(':'));
				}

				CSTNode * pStnodValues = PStnodParseExpressionList(pParctx, pLex);
				pStdecl->m_iStnodInit = pStnodLit->IAppendChild(pStnodValues);

				Expect(pParctx, pLex, TOK('}'), "while parsing array literal");
				return pStnodLit;
			} break;
		case '(':	// ( Expression )
			{
				TokNext(pLex); // consume '('

				CSTNode * pStnodReturn = PStnodParseExpression(pParctx, pLex);
				Expect(pParctx, pLex, TOK(')'));
				return pStnodReturn;
			}

		default: return nullptr;
	}
}

CSTNode * PStnodParsePostfixExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParsePrimaryExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		if (FIsEndOfStatement(pLex))
			return pStnod;

		switch(pLex->m_tok)
		{
		case TOK('['):		// [ expression ]
			{
				SLexerLocation lexloc(pLex);
				TokNext(pLex); // consume '('

				CSTNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArray->m_tok = TOK(pLex->m_tok);
				pStnodArray->m_park = PARK_ArrayElement;
				pStnodArray->IAppendChild(pStnod);

				CSTNode * pStnodElement = PStnodParseExpression(pParctx, pLex);
				pStnodArray->IAppendChild(pStnodElement);

				pStnod = pStnodArray;
				Expect(pParctx, pLex, TOK(']'));
			} break;
		case TOK('('):		// ( )
			{				// ( ArgumentExpressionList )
				SLexerLocation lexloc(pLex);
				TokNext(pLex); // consume '('

				CSTNode * pStnodIdent = nullptr;
				if (pStnod->m_park == PARK_Identifier)
				{
					// clear out the identifier's type info
					pStnod->m_pTin = nullptr;
					pStnodIdent = pStnod;
				}

				CSTNode * pStnodArgList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArgList->m_tok = TOK(pLex->m_tok);
				pStnodArgList->m_park = PARK_ProcedureCall;
				pStnodArgList->IAppendChild(pStnod);
				pStnod = pStnodArgList;

				// parsing this with LogicalAndOrExpression even though ISO c uses assignmentExpression
				//  need to change this if we expect assignments to return the assigned value (x := a = b; )

				while (1)
				{
					CSTNode * pStnodArg = PStnodParseLogicalAndOrExpression(pParctx, pLex);
					pStnodArgList->IAppendChild(pStnodArg);

					if ((pStnodArg==nullptr) | (pLex->m_tok != TOK(',')))
						break;
					Expect(pParctx, pLex, TOK(','));
				}

				Expect(
					pParctx,
					pLex,
					TOK(')'),
					"while parsing procedure call '%s'", 
					pStnodIdent ? StrFromIdentifier(pStnodIdent).PCoz() : "unknown");
			} break;
		case TOK_Arrow:
			{ 
				SLexerLocation lexloc(pLex);
				EmitError(pParctx->m_pWork->m_pErrman, &lexloc, "c-style member dereference '->' is not required, use '.'");

			} // fallthrough
		case TOK('.'):		// . identifier
			{
				TokNext(pLex); // consume '.'
				SLexerLocation lexloc(pLex);

				TOK tokPrev = TOK(pLex->m_tok);	
				CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
				if (!pStnodIdent)
				{
					ParseError(pParctx, pLex, "Expected identifier after '.' before %s", PCozFromTok(tokPrev));
				}
				else
				{
					CSTNode * pStnodMember = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodMember->m_tok = tokPrev;
					pStnodMember->m_park = PARK_MemberLookup;
					pStnodMember->IAppendChild(pStnod);
					pStnodMember->IAppendChild(pStnodIdent);
					pStnod = pStnodMember;
				}
			} break;
		case TOK_PlusPlus:
		case TOK_MinusMinus:
			{
				SLexerLocation lexloc(pLex);

				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume '++' or '--'

				CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodUnary->m_tok = tokPrev;
				pStnodUnary->m_park = PARK_PostfixUnaryOp;
				pStnodUnary->IAppendChild(pStnod);

				pStnod = pStnodUnary;
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseUnaryExpression(CParseContext * pParctx, SLexer * pLex)
{
	if (pLex->m_tok == TOK_DoubleReference)
	{
		SplitToken(pLex, TOK_Reference);
	}

	switch(pLex->m_tok)
	{
	case TOK_ReservedWord:
		{
			auto rword = RwordLookup(pLex);
			switch (rword)
			{
			case RWORD_Sizeof:
			case RWORD_Alignof:
			case RWORD_Typeinfo:
				{
					TOK tokPrev = TOK(pLex->m_tok);	
					SLexerLocation lexloc(pLex);
					TokNext(pLex);

					Expect(pParctx, pLex, TOK('('));

					CSTNode * pStnodChild = PStnodParseUnaryExpression(pParctx, pLex);
					if (!pStnodChild)
					{
						ParseError(pParctx, pLex, "%s missing argument.", PCozFromRword(rword));
					}
					
					Expect(pParctx, pLex, TOK(')'));

					CSTNode * pStnodRword = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodRword->m_tok = tokPrev;
					pStnodRword->m_park = PARK_ReservedWord;
					pStnodRword->IAppendChild(pStnodChild);

					auto pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
					pStval->m_rword = rword;
					pStnodRword->m_pStval = pStval;

					return pStnodRword;
				} 
			case RWORD_Typeof:
				{
					ParseError(pParctx, pLex, "typeof not implemented yet.");
				} break;
			}
		} break;
	case TOK_Dereference:
	case TOK_Reference:
	case TOK('+'):
	case TOK('-'):
	case TOK('~'):
	case TOK('!'):
	case TOK_PlusPlus:
	case TOK_MinusMinus:
		{
			TOK tokPrev = TOK(pLex->m_tok);	
			SLexerLocation lexloc(pLex);

			TokNext(pLex); // consume unary operator

			CSTNode * pStnodExp = PStnodParseUnaryExpression(pParctx, pLex);
			if (!pStnodExp)
			{
				ParseError(
					pParctx,
					pLex,
					"Unary operator '%s' missing operand before %s",
					PCozFromTok(tokPrev),
					PCozCurrentToken(pLex));
				return nullptr;
			}

			CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodUnary->m_tok = tokPrev;
			pStnodUnary->m_park = PARK_UnaryOp;
			pStnodUnary->IAppendChild(pStnodExp);

			return pStnodUnary;
		}
	default: break;
	}

	return PStnodParsePostfixExpression(pParctx, pLex);
}

CSTNode * PStnodParseCastExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnodCast = nullptr;
	CSTDecl * pStdecl = nullptr;
	auto rword = RwordLookup(pLex);
	if (rword != RWORD_Cast && rword != RWORD_AutoCast)
	{
		return PStnodParseUnaryExpression(pParctx, pLex);
	}

	TokNext(pLex);

	SLexerLocation lexloc(pLex);
	pStnodCast = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodCast->m_park = PARK_Cast;

	pStdecl = EWC_NEW(pParctx->m_pAlloc, CSTDecl) CSTDecl();
	pStnodCast->m_pStdecl = pStdecl;

	if (rword == RWORD_Cast)
	{
		Expect(pParctx, pLex, TOK('('));

		auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex);
		pStdecl->m_iStnodType = pStnodCast->IAppendChild(pStnodType);

		Expect(pParctx, pLex, TOK(')'));
	}

	auto pStnodChild = PStnodParseCastExpression(pParctx, pLex);
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
	SLexer * pLex,
	const SLexerLocation & lexloc,
	TOK tokExpression,
	PARK parkExpression,
	CSTNode * pStnodLhs,
	CSTNode * pStnodRhs)
{
	if (!pStnodRhs)
	{
		ParseError(
			pParctx,
			pLex,
			"operator '%s' missing right hand side before %s",
			PCozFromTok(tokExpression),
			PCozCurrentToken(pLex));
		return pStnodLhs;
	}

	CSTNode * pStnodExp = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodExp->m_tok = tokExpression;
	pStnodExp->m_park = parkExpression;
	pStnodExp->IAppendChild(pStnodLhs);
	pStnodExp->IAppendChild(pStnodRhs);
	return pStnodExp;
}

CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseCastExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('*'):
		case TOK('/'):
		case TOK('%'):
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseCastExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_MultiplicativeOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseAdditiveExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseMultiplicativeExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('+'):
		case TOK('-'):
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseMultiplicativeExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_AdditiveOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseShiftExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseAdditiveExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK_ShiftLeft:
		case TOK_ShiftRight:
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseAdditiveExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_ShiftOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseRelationalExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseShiftExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('<'):
		case TOK('>'):
		case TOK_LessEqual:
		case TOK_GreaterEqual:
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseShiftExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_RelationalOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseBitwiseAndOrExpression(CParseContext * pParctx, SLexer * pLex)
{
	// BB - This is a little different than ISO C precedence rules, we're treating all bitwise operators the same
	//  rather than inclusiveOr < exclusiveOr < bitwiseAnd

	CSTNode * pStnod = PStnodParseRelationalExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('|'):
		case TOK('&'):
		case TOK('^'):
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseRelationalExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_BitwiseAndOrOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseEqualityExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseBitwiseAndOrExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK_EqualEqual:
		case TOK_NotEqual:
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseBitwiseAndOrExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_EqualityOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseLogicalAndOrExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseEqualityExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK_OrOr:
		case TOK_AndAnd:
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseEqualityExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseAssignmentExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseLogicalAndOrExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('='):
		case TOK_MulEqual:
		case TOK_DivEqual:
		case TOK_ModEqual:
		case TOK_PlusEqual:
		case TOK_MinusEqual:
		case TOK_AndEqual:
		case TOK_OrEqual:
		case TOK_XorEqual:
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseLogicalAndOrExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_AssignmentOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseAssignmentExpression(pParctx, pLex);

	// TODO: handle Expression > AssignmentExpression , AssignmentExpression

	return pStnod;
}

CSTNode * PStnodParseArrayDecl(CParseContext * pParctx, SLexer * pLex)
{
	if (FConsumeToken(pLex, TOK('[')))
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodArray->m_tok = (TOK)pLex->m_tok;
		pStnodArray->m_park = PARK_ArrayDecl;

		if (FConsumeToken(pLex, TOK_PeriodPeriod))
		{
			;
		}
		else if (pLex->m_tok != TOK(']'))
		{
			CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
			if (pStnodExp)
			{
				pStnodArray->IAppendChild(pStnodExp);
			}
		}

		Expect(pParctx, pLex, TOK(']'));
		return pStnodArray;
	}
	return nullptr;
}

CSTNode * PStnodParseProcedureReferenceDecl(CParseContext * pParctx, SLexer * pLex)
{
	if (FConsumeToken(pLex, TOK('(')))
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodProc->m_tok = (TOK)pLex->m_tok;
		pStnodProc->m_park = PARK_ProcedureReferenceDecl;

		CSTProcedure * pStproc = EWC_NEW(pParctx->m_pAlloc, CSTProcedure) CSTProcedure();
		pStnodProc->m_pStproc = pStproc;

		CSymbolTable * pSymtabProc = nullptr; //null symbol table as the we're a forward reference
		CSTNode * pStnodParams = PStnodParseParameterList(pParctx, pLex, pSymtabProc);
		pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
		Expect(pParctx, pLex, TOK(')'));

		auto pStnodReturns = PStnodParseReturnArrow(pParctx, pLex);
		pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);

		CSTNode ** ppStnodReturns = &pStnodReturns;
		int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;
		int cStnodParams;
		CSTNode ** ppStnodParams = PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

		auto pTinproc =  PTinprocAlloc(pParctx->m_pSymtab, cStnodParams, cStnodReturns, "");
		pTinproc->m_arypTinParams.AppendFill(cStnodParams, nullptr);
		pTinproc->m_arypTinReturns.AppendFill(cStnodReturns, nullptr);

		pTinproc->m_pStnodDefinition = pStnodProc;

		INLINEK inlinek = INLINEK_Nil;
		CALLCONV callconv = CALLCONV_Nil;
		if (pLex->m_tok == TOK_ReservedWord)
		{
			while (pLex->m_tok == TOK_ReservedWord)
			{
				RWORD rwordLookup = RwordLookup(pLex);
				TokNext(pLex);
				switch (rwordLookup)
				{
				case RWORD_ForeignDirective:
					{
						pStproc->m_fIsForeign = true;
						pStproc->m_fUseUnmangledName = true;

						if (!FIsEndOfStatement(pLex) && pLex->m_tok == TOK_Identifier)
						{
							auto pStnodAlias = PStnodParseIdentifier(pParctx, pLex);
							pStproc->m_iStnodForeignAlias = pStnodProc->IAppendChild(pStnodAlias);
						}
					} break;
				case RWORD_CDecl:	callconv = CALLCONV_CX86;			break;
				case RWORD_StdCall: callconv = CALLCONV_StdcallX86;		break;
				case RWORD_Inline:		inlinek = INLINEK_AlwaysInline;	break;
				case RWORD_NoInline:	inlinek = INLINEK_NoInline;		break;
				default:
					{
						ParseError(
							pParctx,
							pLex,
							"Unexpected token following procedure declaration %s\n",
							PCozFromRword(rwordLookup));
					} break;
				}
			}
		}
		pTinproc->m_callconv = callconv;
		pTinproc->m_inlinek = inlinek;

		pStnodProc->m_pTin = pTinproc;

		return pStnodProc;
	}
	return nullptr;
}

CSTNode * PStnodParsePointerDecl(CParseContext * pParctx, SLexer * pLex)
{
	// handle the mis-lexing of '&&' as one token here
	if (pLex->m_tok == TOK_DoubleReference)
	{
		SplitToken(pLex, TOK_Reference);
	}

	if (FConsumeToken(pLex, TOK_Reference))
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnod->m_tok = (TOK)pLex->m_tok;
		pStnod->m_park = PARK_ReferenceDecl;
		return pStnod;
	}

	return nullptr;
}

CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseIdentifier(pParctx, pLex);
	if (pStnod)
	{
		while (FConsumeToken(pLex, TOK('.')))
		{
			SLexerLocation lexloc(pLex);

			TOK tokPrev = TOK(pLex->m_tok);	
			CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
			if (!pStnodIdent)
			{
				ParseError(pParctx, pLex, "Expected identifier after '.' before %s", PCozFromTok(tokPrev));
			}
			else
			{
				CSTNode * pStnodMember = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodMember->m_tok = tokPrev;
				pStnodMember->m_park = PARK_MemberLookup;
				pStnodMember->IAppendChild(pStnod);
				pStnodMember->IAppendChild(pStnodIdent);
				pStnod = pStnodMember;
			}
		}
		return pStnod;
	}

	pStnod = PStnodParseProcedureReferenceDecl(pParctx, pLex);
	if (pStnod)
	{
		return pStnod;
	}

	pStnod = PStnodParsePointerDecl(pParctx, pLex);
	if (!pStnod)
	{
		pStnod = PStnodParseArrayDecl(pParctx, pLex);
	}
	if (!pStnod)
		return nullptr;

	CSTNode * pStnodChild = PStnodParseTypeSpecifier(pParctx, pLex);
	if (!pStnodChild)
	{
		ParseError(pParctx, pLex, "Expected type identifier before '%s'", PCozFromTok(TOK(pLex->m_tok)));
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

void ValidateDeclaration(CParseContext * pParctx, SLexer * pLex, CSTNode * pStnodDecl)
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
			ParseError(pParctx, pLex, "Internal error, compound decl should not specify a type");

		fMissingTypeSpecifier = false;
		for (int iStnodChild = pStdecl->m_iStnodChildMin; iStnodChild != pStdecl->m_iStnodChildMax; ++iStnodChild)
		{
			auto pStdeclChild = pStnodDecl->PStnodChild(iStnodChild)->m_pStdecl;
			EWC_ASSERT(pStdeclChild->m_iStnodIdentifier != -1, "declaration missing identifier");

			if (pStdeclChild->m_iStnodInit != -1)
				ParseError(pParctx, pLex, "Internal error, child decl should not specify an initializer");

			EWC_ASSERT(pStdeclChild->m_iStnodChildMin == -1 && pStdeclChild->m_iStnodChildMax == -1, "nested children not supported");

			fMissingTypeSpecifier |= (pStdeclChild->m_iStnodType == -1);
		}
	}
	
	auto pStnodInit = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
	if (fMissingTypeSpecifier & (pStnodInit == nullptr))
		ParseError(pParctx, pLex, "Expected type specifier or initialization");
	if (pStnodInit && pStnodInit->m_park == PARK_Uninitializer)
	{
		if (fMissingTypeSpecifier)
		{
			ParseError(pParctx, pLex, "Uninitializer not allowed without specified type");
		}
	}
}


CSTNode * PStnodParseParameter(
	CParseContext * pParctx,
	SLexer * pLex,
	CSymbolTable * pSymtab,
	GRFPDECL grfpdecl)
{

	SLexerLocation lexloc(pLex);
	if (pLex->m_tok == TOK_PeriodPeriod && grfpdecl.FIsSet(FPDECL_AllowVariadic))
	{
		TokNext(pLex);

		CSTNode * pStnodVarArgs = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodVarArgs->m_tok = (TOK)pLex->m_tok;
		pStnodVarArgs->m_park = PARK_VariadicArg;
		return pStnodVarArgs;
	}

	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodCompound = nullptr;
	CSTNode * pStnodInit = nullptr;
	bool fAllowCompoundDecl = grfpdecl.FIsSet(FPDECL_AllowCompoundDecl);

	SLexer lexPeek = *pLex;
	int cIdent = 0;
	while (1)
	{
		if (lexPeek.m_tok != TOK_Identifier)
			return nullptr;

		CString strIdent = lexPeek.m_str;

		++cIdent;
		TokNext(&lexPeek);

		if (fAllowCompoundDecl && FConsumeToken(&lexPeek, TOK(',')))
			continue;

		if ((lexPeek.m_tok != TOK(':')) & (lexPeek.m_tok != TOK_ColonEqual))
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
			ParseError(pParctx, pLex, "Initializer must come after all comma separated declarations");

		CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
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

		if (FConsumeToken(pLex, TOK_ColonEqual))
		{
			pStnodInit = PStnodParseExpression(pParctx, pLex);
		}
		else if (FConsumeToken(pLex, TOK(':')))
		{
			if (pStnodCompound)
			{
				EWC_ASSERT(cTypeNeeded, "No compound children?");

				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex);

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
				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex);
				pStdecl->m_iStnodType = pStnodDecl->IAppendChild(pStnodType);
				cTypeNeeded = 0;
			}

			if (FConsumeToken(pLex, TOK('=')))
			{
				if (FConsumeToken(pLex, TOK_TripleMinus))
				{
					if (!grfpdecl.FIsSet(FPDECL_AllowUninitializer))
					{
						ParseError(pParctx, pLex, "--- uninitializer not allowed in parameter lists");
					}
					else
					{
						SLexerLocation lexloc(pLex);
						pStnodInit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
						pStnodInit->m_tok = TOK_TripleMinus;
						pStnodInit->m_park = PARK_Uninitializer;
					}
				}
				else
				{
					pStnodInit = PStnodParseExpression(pParctx, pLex);
					if (!pStnodInit)
						ParseError(pParctx, pLex, "initial value expected before %s", PCozCurrentToken(pLex));
				}
			}
			else if (FConsumeToken(pLex, TOK(':')))
			{
				if (pStnodCompound)
					ParseError(pParctx, pLex, "Comma separated declarations not supported for constants");

				pStnodDecl->m_park = PARK_ConstantDecl;
				pStnodInit = PStnodParseExpression(pParctx, pLex);
				if (!pStnodInit)
					ParseError(pParctx, pLex, "initial value expected before %s", PCozCurrentToken(pLex));
			}
		}

	} while (fAllowCompoundDecl && FConsumeToken(pLex, TOK(',')));

	pStnodReturn->m_pStdecl->m_iStnodInit = pStnodReturn->IAppendChild(pStnodInit);

	ValidateDeclaration(pParctx, pLex, pStnodReturn);
	return pStnodReturn;
}

CSTNode * PStnodParseDecl(CParseContext * pParctx, SLexer * pLex)
{
	// stand alone declaration statement

	GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl;
	auto * pStnod =  PStnodParseParameter(pParctx, pLex, pParctx->m_pSymtab, grfpdecl);
	if (!pStnod)
		return nullptr;

	ExpectEndOfStatement(pParctx, pLex);
	return pStnod;
}

CSTNode * PStnodParseMemberDeclList(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnodList = nullptr;

	while (1)
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnod = PStnodParseDecl(pParctx, pLex);
		if (!pStnod)
		{
			pStnod = PStnodParseDefinition(pParctx, pLex);
		}
		if (!pStnod)
			break;

		if (pStnodList == nullptr)
		{
			pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodList->m_tok = TOK('{');
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

CSTNode * PStnodParseReturnArrow(CParseContext * pParctx, SLexer * pLex)
{
	if (FConsumeToken(pLex, TOK_Arrow))
	{
		// TODO : handle multiple return types

		return PStnodParseTypeSpecifier(pParctx, pLex);
	}
	else
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnodVoid = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnodVoid->m_tok = TOK_Identifier;
		pStnodVoid->m_park = PARK_Identifier;

		auto pStident = EWC_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
		pStident->m_str = CString("void");
		pStnodVoid->m_pStident = pStident;

		return pStnodVoid;
	}
}

CSTNode * PStnodParseParameterList(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtabProc)
{
	SLexerLocation lexloc(pLex);
	if (pSymtabProc)
	{
		pSymtabProc->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
		PushSymbolTable(pParctx, pSymtabProc, lexloc);
	}

	GRFPDECL grfpdecl = FPDECL_AllowVariadic;
	CSTNode * pStnodParam = PStnodParseParameter(pParctx, pLex, pSymtabProc, grfpdecl);
	CSTNode * pStnodList = nullptr;
	bool fHasVarArgs = pStnodParam && pStnodParam->m_park == PARK_VariadicArg;

	if (pStnodParam)
	{
		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_park = PARK_ParameterList;
		pStnodList->m_pSymtab = pSymtabProc;
		pStnodList->IAppendChild(pStnodParam);

		while (FConsumeToken(pLex, TOK(',')))
		{
			pStnodParam = PStnodParseParameter(pParctx, pLex, pSymtabProc, grfpdecl);

			if (!pStnodParam)
			{
				auto strUnexpected = StrUnexpectedToken(pLex);
				ParseError(pParctx, pLex, "expected parameter declaration before '%s'", strUnexpected.PCoz());
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
			ParseError(pParctx, pLex, "Variadic function argument found before the end of the argument list");
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

	pStnodConstant->m_pSym = pParctx->m_pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, strIdent, pStnodConstant, FSYM_VisibleWhenNested);
	return pStnodConstant;
}

CSTNode * PStnodParseEnumConstant(CParseContext * pParctx, SLexer * pLex)
{
	SLexerLocation lexloc(pLex);
	CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
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

	pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodConstant, FSYM_VisibleWhenNested);
	pStnodConstant->m_pSym = pSym;

	if (FConsumeToken(pLex, TOK(':')))
	{
		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
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

CSTNode * PStnodParseEnumConstantList(CParseContext * pParctx, SLexer * pLex)
{
	SLexerLocation lexloc(pLex);
	auto pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodList->m_tok = TOK('{');
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
		CSTNode * pStnod = PStnodParseEnumConstant(pParctx, pLex);
		if (!pStnod)
			break;

		pStnodList->IAppendChild(pStnod);

		if (!FConsumeToken(pLex, TOK(',')))
			break;
	}
	return pStnodList;
}

struct SOverloadInfo // tag = ovinf
{
	const char *	m_pCoz;
	TOK				m_tok;
	PARK			m_aPark[2];
};

static const SOverloadInfo s_aOvinf[] = {
	{ "operator+", TOK('+'),			{ PARK_AdditiveOp, PARK_UnaryOp} },
	{ "operator-", TOK('-'),			{ PARK_AdditiveOp, PARK_UnaryOp} },
	{ "operator*", TOK('*'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator/", TOK('/'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator%", TOK('%'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator|", TOK('|'),			{ PARK_BitwiseAndOrOp, PARK_Nil} },
	{ "operator|", TOK('&'),			{ PARK_BitwiseAndOrOp, PARK_Nil} },
	{ "operator|", TOK('^'),			{ PARK_BitwiseAndOrOp, PARK_Nil} },
	{ "operator<<", TOK_ShiftLeft,		{ PARK_ShiftOp, PARK_Nil} },
	{ "operator>>", TOK_ShiftRight,		{ PARK_ShiftOp, PARK_Nil} },
	{ "operator>", TOK('>'),			{ PARK_RelationalOp, PARK_Nil} },
	{ "operator>", TOK('<'),			{ PARK_RelationalOp, PARK_Nil} },
	{ "operator>", TOK_LessEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator>", TOK_GreaterEqual,	{ PARK_RelationalOp, PARK_Nil} },
	{ "operator==", TOK_EqualEqual,		{ PARK_EqualityOp, PARK_Nil} },
	{ "operator!=", TOK_NotEqual,		{ PARK_EqualityOp, PARK_Nil} },
	{ "operator+=", TOK_PlusEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator-=", TOK_MinusEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator*=", TOK_MulEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator/=", TOK_DivEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator=", TOK('='),			{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator:=", TOK_ColonEqual,		{ PARK_Decl, PARK_Nil} },
	{ "operator++", TOK_PlusPlus,		{ PARK_PostfixUnaryOp, PARK_Nil} },
	{ "operator--", TOK_MinusMinus,		{ PARK_PostfixUnaryOp, PARK_Nil} },
	{ "operator@", TOK_Dereference,		{ PARK_UnaryOp, PARK_Nil} },
	{ "operator&", TOK_Reference,		{ PARK_UnaryOp, PARK_Nil} },
	{ "operator~", TOK('~'),			{ PARK_UnaryOp, PARK_Nil} },
	{ "operator!", TOK('!'),			{ PARK_UnaryOp, PARK_Nil} },
};

enum FOVSIG
{
	FOVSIG_MustTakeReference	= 0x1,
	FOVSIG_ReturnBool			= 0x2,
	FOVSIG_AllowCommutative		= 0x4,


	FOVSIG_None			= 0x0,
	FOVSIG_All			= 0x7,
};
EWC_DEFINE_GRF(GRFOVSIG, FOVSIG, u32);

struct SOverloadSignature // tag=ovsig
{
	PARK			m_park;
	int				m_cParam;
	int				m_cReturn;
	GRFOVSIG		m_grfovsig;
	const char *	m_pChzDescription;
};

SOverloadSignature s_aOvsig[] =
{
	{ PARK_AdditiveOp,			2, 1,	FOVSIG_AllowCommutative,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_MultiplicativeOp,	2, 1,	FOVSIG_AllowCommutative,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_BitwiseAndOrOp,		2, 1,	FOVSIG_AllowCommutative,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_ShiftOp,				2, 1,	false,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_RelationalOp,		2, 1,	FOVSIG_ReturnBool,	"(Lhs: A, Rhs: B)->bool" },
	{ PARK_EqualityOp,			2, 1,	FOVSIG_ReturnBool,	"(Lhs: A, Rhs: B)->bool" },
	{ PARK_AssignmentOp,		2, 0,	FOVSIG_MustTakeReference,	"(Lhs: &B, Rhs: A)" },
	{ PARK_Decl,				2, 0,	FOVSIG_MustTakeReference,	"(Lhs: &B, Rhs: A)" },
	{ PARK_PostfixUnaryOp,		1, 1,	FOVSIG_MustTakeReference,	"(lHs: &A)->B" },
	{ PARK_UnaryOp,				1, 1,	false,	"(a: A)->B" },
};

const char * PChzOverloadSignature(PARK park)
{
	auto pOvsigMax = EWC_PMAC(s_aOvsig);
	for (SOverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
	{
		if (pOvsig->m_park == park)
			return pOvsig->m_pChzDescription;
	}
	return "Unknown";
}

bool FAllowsCommutative(PARK park)
{
	auto pOvsigMax = EWC_PMAC(s_aOvsig);
	for (SOverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
	{
		if (pOvsig->m_park == park)
			return pOvsig->m_grfovsig.FIsSet(FOVSIG_AllowCommutative);
	}
	return false;
}

const char * PCozOverloadNameFromTok(TOK tok)
{
	auto pOvinfMax = EWC_PMAC(s_aOvinf);
	for (auto pOvinf = s_aOvinf; pOvinf != pOvinfMax; ++pOvinf)
	{
		if (pOvinf->m_tok == tok)
		{
			return pOvinf->m_pCoz;
		}
	}
	return nullptr;
}

static bool FOperatorOverloadMustTakeReference(TOK tok)
{
	auto pOvinfMax = EWC_PMAC(s_aOvinf);
	for (auto pOvinf = s_aOvinf; pOvinf != pOvinfMax; ++pOvinf)
	{
		if (pOvinf->m_tok != tok)
			continue;

		int acBool[2] = {0,0};
		auto pOvsigMax = EWC_PMAC(s_aOvsig);
		for (int iPark = 0; iPark < EWC_DIM(pOvinf->m_aPark); ++iPark)
		{
			for (SOverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
			{
				if (pOvsig->m_park == pOvinf->m_aPark[iPark])
				{
					++acBool[pOvsig->m_grfovsig.FIsSet(FOVSIG_MustTakeReference)];
				}
			}
			EWC_ASSERT(acBool[0] == 0 || acBool[1] == 0, "conflicting results for diferrent operators");
		}
		return acBool[true] != 0;
	}
	return false;
}

bool FCheckOverloadSignature(PARK park, STypeInfoProcedure * pTinproc)
{
	size_t cReturn = pTinproc->m_arypTinReturns.C();
	if (cReturn == 1 && pTinproc->m_arypTinReturns[0]->m_tink == TINK_Void)
	{
		cReturn = 0;
	}

	auto pOvsigMax = EWC_PMAC(s_aOvsig);
	for (SOverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
	{
		if (pOvsig->m_park == park)
		{
			if (pTinproc->m_arypTinParams.C() != pOvsig->m_cParam || cReturn != pOvsig->m_cReturn)
				return false;
			if (pOvsig->m_grfovsig.FIsSet(FOVSIG_ReturnBool) && pTinproc->m_arypTinReturns[0]->m_tink != TINK_Bool)
				return false;

			return !pOvsig->m_grfovsig.FIsSet(FOVSIG_MustTakeReference) || pTinproc->m_arypTinParams[0]->m_tink == TINK_Pointer;
		}
	}
	return false;
}

bool FCheckOverloadSignature(TOK tok, STypeInfoProcedure * pTinproc, SErrorManager * pErrman, SLexerLocation * pLexloc)
{
	auto pOvinfMax = EWC_PMAC(s_aOvinf);
	for (auto pOvinf = s_aOvinf; pOvinf != pOvinfMax; ++pOvinf)
	{
		if (pOvinf->m_tok != tok)
			continue;

		for (int iPark = 0; iPark < EWC_DIM(pOvinf->m_aPark); ++iPark)
		{
			PARK park = pOvinf->m_aPark[iPark];
			if (park != PARK_Nil && FCheckOverloadSignature(park, pTinproc))
			{
				if (pTinproc->m_fIsCommutative && !FAllowsCommutative(park))
				{
					EmitError(pErrman, pLexloc, "'%s' is not allowed when overloading '%s'", PCozFromRword(RWORD_Commutative), PCozFromTok(tok));
					return false;
				}
				return true;
			}
		}

		SError error(pErrman);
		PrintErrorLine(&error, "Error:", pLexloc, "Incorrect signature for overloading operator '%s'. Options are:", PCozFromTok(tok));

		for (int iPark = 0; iPark < EWC_DIM(pOvinf->m_aPark); ++iPark)
		{
			PARK park = pOvinf->m_aPark[iPark];
			if (park != PARK_Nil)
			{
				PrintErrorLine(&error, "", pLexloc, "\toperator%s%s'", PCozFromTok(tok), PChzOverloadSignature(park));
			}
		}
		return false;
	}

	EWC_ASSERT(false, "unknown overload signature");
	return false;
}

CSTNode * PStnodParseDefinition(CParseContext * pParctx, SLexer * pLex)
{
	RWORD rword = RwordLookup(pLex);
	if (pLex->m_tok == TOK_Identifier || rword == RWORD_Operator)
	{
		SLexer lexPeek = *pLex;
		TokNext(&lexPeek);

		if (rword == RWORD_Nil)
		{
			rword = RwordLookup(&lexPeek);
		}

		bool fIsConstantDecl = lexPeek.m_tok == TOK_ColonColon;
		bool fIsDefinition = fIsConstantDecl;
		switch (rword)
		{
		case RWORD_Proc:
		case RWORD_Struct:
		case RWORD_Enum:
		case RWORD_Typedef:
		case RWORD_Operator:
			fIsDefinition = true;
			break;
		}

		if (fIsDefinition)
		{
			SLexerLocation lexloc(pLex);
			CSTNode * pStnodIdent;

			if (rword == RWORD_Operator)
			{
				TokNext(pLex);

				const char * pCozOverloadName = PCozOverloadNameFromTok((TOK)pLex->m_tok);
				if (!pCozOverloadName)
				{
					ParseError(pParctx, pLex, "Cannot overload operator '%s'", PCozFromTok((TOK)pLex->m_tok));
					pCozOverloadName = "OverloadError";
				}

				CString strIdent(pCozOverloadName);
				pStnodIdent = PStnodAllocateIdentifier(pParctx, lexloc, strIdent);
				pStnodIdent->m_tok = TOK(pLex->m_tok);
			}
			else
			{
				pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
				pStnodIdent->m_tok = TOK_Identifier;
			}

			*pLex = lexPeek;
			TokNext(pLex);
			
			// function definition
			if (rword == RWORD_Proc || rword == RWORD_Operator)
			{
				Expect(pParctx, pLex, TOK('('));

				CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodProc->m_park = PARK_ProcedureDefinition;
				pStnodProc->m_grfstnod.AddFlags(FSTNOD_EntryPoint);

				CSTProcedure * pStproc = EWC_NEW(pParctx->m_pAlloc, CSTProcedure) CSTProcedure();
				pStnodProc->m_pStproc = pStproc;
				pStproc->m_iStnodProcName = pStnodProc->IAppendChild(pStnodIdent);
				pStproc->m_pStnodParentScope = pParctx->m_pStnodScope;

				const CString & strName = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabProc = pParctx->m_pWork->PSymtabNew(strName, pSymtabParent->m_pUnsetTin);

				// BB - don't mangle the main function so the linker can find it. yuck.
				pStproc->m_fUseUnmangledName |= (strName == "main");

				CSTNode * pStnodParams = PStnodParseParameterList(pParctx, pLex, pSymtabProc);
				pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
				Expect(pParctx, pLex, TOK(')'));

				auto pStnodReturns = PStnodParseReturnArrow(pParctx, pLex);
				pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);

				INLINEK inlinek = INLINEK_Nil;
				CALLCONV callconv = CALLCONV_Nil;
				bool fIsCommutative = false;
				pStproc->m_iStnodBody = -1;
				if (pLex->m_tok == TOK_ReservedWord)
				{
					while (pLex->m_tok == TOK_ReservedWord)
					{
						RWORD rwordLookup = RwordLookup(pLex);
						TokNext(pLex);
						switch (rwordLookup)
						{
						case RWORD_ForeignDirective:
							{
								pStproc->m_fIsForeign = true;
								pStproc->m_fUseUnmangledName = true;

								if (!FIsEndOfStatement(pLex) && pLex->m_tok == TOK_Identifier)
								{
									auto pStnodAlias = PStnodParseIdentifier(pParctx, pLex);
									pStproc->m_iStnodForeignAlias = pStnodProc->IAppendChild(pStnodAlias);
								}
							} break;
						case RWORD_CDecl:	callconv = CALLCONV_CX86;			break;
						case RWORD_StdCall: callconv = CALLCONV_StdcallX86;		break;
						case RWORD_Inline:		inlinek = INLINEK_AlwaysInline;	break;
						case RWORD_NoInline:	inlinek = INLINEK_NoInline;		break;
						case RWORD_Commutative:
							{
								if (rword == RWORD_Operator)
								{
									fIsCommutative = true;	
								}
								else
								{
									ParseError( pParctx, pLex, "only operator overloads can be declared commutative, %s() is not an overload.\n", strName.PCoz());
								}
							} break;
						default:
							{
								ParseError(
									pParctx,
									pLex,
									"Unexpected token following procedure declaration %s\n",
									PCozFromRword(rwordLookup));
							} break;
						}
					}

					if (pLex->m_tok != TOK('{'))
					{
						ExpectEndOfStatement(pParctx, pLex, "While parsing procedure qualifiers");
					}
				}

				if (pLex->m_tok == TOK('{'))
				{
					auto pStnodScopePrev = pParctx->m_pStnodScope;
					pParctx->m_pStnodScope = pStnodProc;
					CSTNode * pStnodBody = PStnodParseCompoundStatement(pParctx, pLex, pSymtabProc);
					pParctx->m_pStnodScope = pStnodScopePrev;

					pStproc->m_iStnodBody = pStnodProc->IAppendChild(pStnodBody);
				}

				if (pStproc->m_fIsForeign)
				{
					if (pStproc->m_iStnodBody != -1)
					{
						ParseError(
							pParctx,
							pLex,
							"Procedure '%s' is marked foreign, but defines a procedure body.",
							strName.PCoz());
						pStproc->m_iStnodBody = -1;
					}
				}
				else if (pStproc->m_iStnodBody == -1)
				{
					ParseError(pParctx, pLex, "Procedure definition for '%s' has no body", strName.PCoz());
				}

				int cStnodParams;
				CSTNode ** ppStnodParams = PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

				CSTNode ** ppStnodReturn = &pStnodReturns;
				int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;

				auto pTinproc =  PTinprocAlloc(pSymtabParent, cStnodParams, cStnodReturns, strName.PCoz());

				if (rword == RWORD_Operator && FOperatorOverloadMustTakeReference(pStnodIdent->m_tok))
				{
					pTinproc->m_mpIptinGrfparmq[0].AddFlags(FPARMQ_ImplicitRef);
				}

				if (fIsCommutative)
				{
					if (cStnodParams != 2)
					{
						ParseError(pParctx, pLex, "Only operators with two arguments can be commutative ('%s' has %d)", strName.PCoz(), cStnodParams);
						fIsCommutative = false;
					}
					pTinproc->m_fIsCommutative = fIsCommutative;
				}

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

				CSTNode ** ppStnodReturnMax = &ppStnodReturn[cStnodReturns];
				for ( ; ppStnodReturn != ppStnodReturnMax; ++ppStnodReturn)
				{
					pTinproc->m_arypTinReturns.Append((*ppStnodReturn)->m_pTin);
				}

				pStnodProc->m_pTin = pTinproc;

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
								SLexerLocation lexloc(pLex);
								CSTNode * pStnodReturn = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

								pStnodReturn->m_tok = TOK_ReservedWord;
								pStnodReturn->m_park = PARK_ReservedWord;

								auto pStvalReturn = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
								pStvalReturn->m_str = CString("return");
								pStvalReturn->m_rword = RWORD_Return;
								pStnodReturn->m_pStval = pStvalReturn;

								(void) pStnodBody->IAppendChild(pStnodReturn);
							}
							else
							{
								ParseError(pParctx, pLex, "Procedure '%s' is missing return statement", strName.PCoz());
							}
						}
					}
				}

				auto pErrman = pParctx->m_pWork->m_pErrman;
				SSymbol * pSymProc = pSymtabParent->PSymEnsure( pErrman, StrFromIdentifier(pStnodIdent), pStnodProc, FSYM_VisibleWhenNested);
				pSymProc->m_pTin = pTinproc;
				pStnodProc->m_pSym = pSymProc;


				return pStnodProc;
			}
			else if (rword == RWORD_Enum)
			{
				SLexerLocation lexloc(pLex);
				CSTNode * pStnodEnum = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodEnum->m_park = PARK_EnumDefinition;

				CSTEnum * pStenum = EWC_NEW(pParctx->m_pAlloc, CSTEnum) CSTEnum();
				pStnodEnum->m_pStenum = pStenum;
				pStenum->m_iStnodIdentifier = pStnodEnum->IAppendChild(pStnodIdent);

				CSTNode * pStnodType = PStnodParseIdentifier(pParctx, pLex);
				pStenum->m_iStnodType = pStnodEnum->IAppendChild(pStnodType);
				
				const CString & strIdent = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabEnum = pParctx->m_pWork->PSymtabNew(strIdent, pSymtabParent->m_pUnsetTin);
				CSTNode * pStnodConstantList = nullptr;

				auto pErrman = pParctx->m_pWork->m_pErrman;
				(void) pSymtabEnum->PSymEnsure(pErrman, "loose", pStnodEnum, FSYM_IsType | FSYM_VisibleWhenNested);
				(void) pSymtabEnum->PSymEnsure(pErrman, "strict", pStnodEnum, FSYM_IsType | FSYM_VisibleWhenNested);

				Expect(pParctx, pLex, TOK('{'));

				pSymtabEnum->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
				PushSymbolTable(pParctx, pSymtabEnum, lexloc);
				pStnodEnum->m_pSymtab = pSymtabEnum;

				pStnodConstantList = PStnodParseEnumConstantList(pParctx, pLex);
				pStenum->m_iStnodConstantList = pStnodEnum->IAppendChild(pStnodConstantList);

				CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
				EWC_ASSERT(pSymtabEnum == pSymtabPop, "CSymbol table push/pop mismatch (enum)");

				Expect(pParctx, pLex, TOK('}'));

				// type info enum
				int cStnodChild;
				CSTNode ** ppStnodMember = PPStnodChildFromPark(pStnodConstantList, &cStnodChild, PARK_List);

				int cConstant = (ppStnodMember) ? 
									cStnodChild - (ENUMIMP_Max - ENUMIMP_Min) + ENUMIMP_CConstant : 
									0;
				size_t cBAlloc = CBAlign(sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeInfoEnumConstant)) + 
								cConstant * sizeof(STypeInfoEnumConstant);
				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoEnum * pTinenum = new(pB) STypeInfoEnum(strIdent, StrUniqueName(pSymtabParent->m_pUnsetTin, strIdent));

				auto aTinecon = (STypeInfoEnumConstant *)PVAlign( pB + sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeInfoEnumConstant));
				pTinenum->m_aryTinecon.SetArray(aTinecon, 0, cConstant);

				pTinenum->m_tinstructProduced.m_pStnodStruct = pStnodEnum;
				pSymtabParent->AddManagedTin(pTinenum);

				CSTNode ** ppStnodMemberMax = (ppStnodMember) ? &ppStnodMember[cStnodChild] : nullptr;
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

				GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
				SSymbol * pSymEnum = pSymtabParent->PSymEnsure(pErrman, strIdent, pStnodEnum, grfsym, FSHADOW_NoShadowing);
				pSymEnum->m_pTin = pTinenum;

				pStnodEnum->m_pSym = pSymEnum;
				pStnodEnum->m_pTin = pTinenum;

				return pStnodEnum;
			}
			else if (rword == RWORD_Struct)
			{
				Expect(pParctx, pLex, TOK('{'));

				CSTNode * pStnodStruct = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodStruct->m_park = PARK_StructDefinition;
				pStnodStruct->IAppendChild(pStnodIdent);

				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;

				const CString & strIdent = StrFromIdentifier(pStnodIdent);
				SLexerLocation lexlocChild(pLex);
				CSymbolTable * pSymtabStruct = pParctx->m_pWork->PSymtabNew(strIdent, pSymtabParent->m_pUnsetTin);

				// NOTE: struct symbol tables at the global scope should be unordered.
//				if (!pParctx->m_pSymtab->m_grfsymtab.FIsSet(FSYMTAB_Ordered))
//					pSymtabStruct->m_grfsymtab.Clear(FSYMTAB_Ordered);

				pSymtabStruct->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
				PushSymbolTable(pParctx, pSymtabStruct, lexlocChild);
				pStnodStruct->m_pSymtab = pSymtabStruct;

				CSTNode * pStnodDeclList = PStnodParseMemberDeclList(pParctx, pLex);

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

				auto pSymtab = pParctx->m_pSymtab;
				STypeInfoStruct * pTinstruct = new(pB) STypeInfoStruct(strIdent, StrUniqueName(pSymtab->m_pUnsetTin, strIdent));
				pSymtab->AddManagedTin(pTinstruct);

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

					size_t cTypememb = pTinstruct->m_aryTypemembField.C();
					for (size_t iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
					{
						auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

						auto pStnodChild = pTypememb->m_pStnod;
						auto pStnodMemberIdent = pStnodChild->PStnodChildSafe(pStnodChild->m_pStdecl->m_iStnodIdentifier);
						pTypememb->m_strName = StrFromIdentifier(pStnodMemberIdent);
						pTypememb->m_pTin = pStnodChild->m_pTin;
					}
				}

				auto pErrman = pParctx->m_pWork->m_pErrman;
				GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
				SSymbol * pSymStruct = pSymtabParent->PSymEnsure(pErrman, strIdent, pStnodStruct, grfsym, FSHADOW_NoShadowing);
				pStnodStruct->m_pSym = pSymStruct;
				pSymStruct->m_pTin = pTinstruct;
				pStnodStruct->m_pTin = pTinstruct;

				Expect(pParctx, pLex, TOK('}'));

				return pStnodStruct;
			}
			else if (rword == RWORD_Typedef)
			{
				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex);
				ExpectEndOfStatement(pParctx, pLex);
				
				CString strIdent = StrFromIdentifier(pStnodIdent);
				if (!pStnodType)
				{
					ParseError(pParctx, pLex, "missing type value for typedef %s", strIdent.PCoz());

					pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
					return nullptr;
				}

				CSTNode * pStnodTypedef = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodTypedef->m_park = PARK_Typedef;

				(void) pStnodTypedef->IAppendChild(pStnodIdent);
				(void) pStnodTypedef->IAppendChild(pStnodType);

				CSymbolTable * pSymtab = pParctx->m_pSymtab;
				auto pErrman = pParctx->m_pWork->m_pErrman;

				GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
				auto pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodTypedef, grfsym, FSHADOW_NoShadowing);
				pStnodTypedef->m_pSym = pSym;


				return pStnodTypedef;
			}
			else if (fIsConstantDecl)
			{
				// constant decl

				auto pStnodInit = PStnodParseExpression(pParctx, pLex);
				ExpectEndOfStatement(pParctx, pLex);
				
				CString strIdent = StrFromIdentifier(pStnodIdent);
				if (!pStnodInit)
				{
					ParseError(pParctx, pLex, "missing constant value for %s", strIdent.PCoz());

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
				pStnodIdent->m_pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodConst, FSYM_VisibleWhenNested);

				return pStnodConst;
			}
		} // rword definitions
	}
	return nullptr;
}

CSTNode * PStnodParseExpressionStatement(CParseContext * pParctx, SLexer * pLex)
{
	if (FConsumeToken(pLex, TOK(';')))
	{
		// return empty statement

		SLexerLocation lexloc(pLex);
		CSTNode * pStnodEmpty = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnodEmpty->m_tok = TOK(pLex->m_tok);
		pStnodEmpty->m_park = PARK_Nop;

		return pStnodEmpty;
	}

	CSTNode * pStnod = PStnodParseExpression(pParctx, pLex);
	if (pStnod)
	{
		ExpectEndOfStatement(pParctx, pLex);
	}
	return pStnod;
}

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtab)
{
	CSTNode * pStnodList = nullptr;

	if (FConsumeToken(pLex, TOK('{')))
	{
		SLexerLocation lexloc(pLex);

		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_tok = TOK('{');
		pStnodList->m_park = PARK_List;

		if (!pSymtab)
		{
			pSymtab = pParctx->m_pWork->PSymtabNew("anon", &pParctx->m_pWork->m_unsetTin);
		}
		pStnodList->m_pSymtab = pSymtab;
		PushSymbolTable(pParctx, pSymtab, lexloc);

		while (pLex->m_tok != TOK('}'))
		{
			CSTNode * pStnod = PStnodParseStatement(pParctx, pLex);
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
		Expect(pParctx, pLex, TOK('}'));
	}

	return pStnodList;
}

CSTNode * PStnodExpectCompoundStatement(CParseContext * pParctx, SLexer * pLex, const char * pCozPriorStatement)
{
	if (pLex->m_tok != TOK('{'))
	{
		ParseError(pParctx, pLex, "Expected '{' after %s'", pCozPriorStatement);

		// just take a swing at parsing a one line statement - not actually safe, just trying to generate the next error.
		return PStnodParseStatement(pParctx, pLex);
	}

	return PStnodParseCompoundStatement(pParctx, pLex, nullptr);
}

void CreateSwitchList(CParseContext * pParctx, SLexer * pLex, CSTNode ** ppStnodList)
{
	auto pStnodList = *ppStnodList;
	if (pStnodList)
	{
		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pStnodList->m_pSymtab == pSymtabPop, "CSymbol table push/pop mismatch (list)");
	}

	SLexerLocation lexloc(pLex);

	pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodList->m_tok = TOK('{');
	pStnodList->m_park = PARK_List;
	pStnodList->m_pSymtab = pParctx->m_pWork->PSymtabNew("case", &pParctx->m_pWork->m_unsetTin);
	PushSymbolTable(pParctx, pStnodList->m_pSymtab, lexloc);
	*ppStnodList = pStnodList;
}

CSTNode * PStnodParseSwitchStatement(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnodSwitch = PStnodParseReservedWord(pParctx, pLex, RWORD_Switch);
	CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);

	if (!pStnodExp)
	{
		ParseError(pParctx, pLex, "switch statement missing expression");

		pParctx->m_pAlloc->EWC_DELETE(pStnodSwitch);
		return nullptr;
	}

	pStnodSwitch->IAppendChild(pStnodExp);

	if (!FConsumeToken(pLex, TOK('{')))
	{
		ParseError(pParctx, pLex, "Expected '{' for case statements'");

		pParctx->m_pAlloc->EWC_DELETE(pStnodSwitch); // pStnodExp will be deleted in dtor
		return nullptr;
	}

	CSTNode * pStnodList = nullptr;
	bool fInvalidSwitch = false;
	while (pLex->m_tok != TOK('}') && !fInvalidSwitch)
	{
		RWORD rword = RwordLookup(pLex);
		switch(rword)
		{
			case RWORD_Case:
			{
				CSTNode * pStnodCase = PStnodParseReservedWord(pParctx, pLex, RWORD_Case);
				pStnodSwitch->IAppendChild(pStnodCase);

				while (1)
				{
					auto pStnodValue = PStnodParseExpression(pParctx, pLex);
					if (!pStnodValue)
					{
						ParseError(pParctx, pLex, "case statement missing it's label");
					}

					pStnodCase->IAppendChild(pStnodValue);

					if (pLex->m_tok != TOK(','))
						break;

					TokNext(pLex);
				}

				Expect(pParctx, pLex, TOK(':'));
				CreateSwitchList(pParctx, pLex, &pStnodList);

				pStnodCase->IAppendChild(pStnodList);

			} break;
			case RWORD_Default:
			{
				CSTNode * pStnodDefault = PStnodParseReservedWord(pParctx, pLex, RWORD_Default);
				pStnodSwitch->IAppendChild(pStnodDefault);

				Expect(pParctx, pLex, TOK(':'));
				CreateSwitchList(pParctx, pLex, &pStnodList);

				pStnodDefault->IAppendChild(pStnodList);

			} break;
			default:
			{
				CSTNode * pStnod = PStnodParseStatement(pParctx, pLex);
				if (!pStnod)
				{
					ParseError(pParctx, pLex, "missing 'case' or 'default' label");
					fInvalidSwitch = true;
				}
				else
				{
					pStnodList->IAppendChild(pStnod);
				}
			}
		}
	}

	if (pStnodList)
	{
		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pStnodList->m_pSymtab == pSymtabPop, "CSymbol table push/pop mismatch (list)");
	}

	Expect(pParctx, pLex, TOK('}'));

	if (pStnodSwitch->CStnodChild() < 2)
	{
		ParseError(pParctx, pLex, "switch statement contains no 'case' or 'default' labels");
		pParctx->m_pAlloc->EWC_DELETE(pStnodSwitch); // pStnodExp will be deleted in dtor
		return nullptr;
	}

	return pStnodSwitch;
}

CSTNode * PStnodParseJumpStatement(CParseContext * pParctx, SLexer * pLex)
{
	RWORD rword = RwordLookup(pLex);
	switch(rword)
	{
	case RWORD_Continue:
	case RWORD_Break:
	case RWORD_Fallthrough:
		{
			CSTNode * pStnod = PStnodParseReservedWord(pParctx, pLex, rword);
			if (pLex->m_tok == TOK_Identifier)
			{
				auto pStident = EWC_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
				pStident->m_str = pLex->m_str;
				pStnod->m_pStident = pStident;
				TokNext(pLex);
			}

			ExpectEndOfStatement(pParctx, pLex);

			return pStnod;
		} break;
	case RWORD_Return:
		{
			CSTNode * pStnodReturn = PStnodParseReservedWord(pParctx, pLex, rword);
			if (EWC_FVERIFY(pStnodReturn, "error parsing return"))
			{
				if (!FIsEndOfStatement(pLex))
				{
					CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
					pStnodReturn->IAppendChild(pStnodExp);
				}
			}

			ExpectEndOfStatement(pParctx, pLex);
			return pStnodReturn;
		} break;
	default:
		return nullptr;
	}
}

CSTNode * PStnodParseSelectionStatement(CParseContext * pParctx, SLexer * pLex, CSTIdentifier ** ppStidentLabel)
{
	RWORD rword = RwordLookup(pLex);
	if (rword == RWORD_If)
	{
		//if expression statement
		//if expression statement else statement

		CSTNode * pStnodIf = PStnodParseReservedWord(pParctx, pLex, RWORD_If);
		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
		pStnodIf->IAppendChild(pStnodExp);
		
		CSTNode * pStnodStatement = PStnodExpectCompoundStatement(pParctx, pLex, PCozFromRword(rword));
		if (!pStnodStatement)
		{
			ParseError( pParctx, pLex, "Error parsing if statement. unexpected token '%s'", PCozFromTok((TOK)pLex->m_tok));

			// move the lexer forward until it has some hope of generating decent errors
			static const TOK s_aTok[] = {TOK(';'), TOK('{') };
			SkipToToken(pLex, s_aTok, EWC_DIM(s_aTok), FLEXER_EndOfLine);
			return pStnodIf;
		}
		if (pStnodStatement->m_grfstnod.FIsSet(FSTNOD_EntryPoint))
		{
			EWC_ASSERT(pStnodStatement->m_park == PARK_ProcedureDefinition, "Unknown entry point park");
			ParseError( pParctx, pLex, "Local functions not directly allowed under conditional, add {}");
		}
		else
		{
			pStnodIf->IAppendChild(pStnodStatement);
		}

		RWORD rwordElse = RwordLookup(pLex);
		if (rwordElse == RWORD_Else)
		{
			CSTNode * pStnodElse = PStnodParseReservedWord(pParctx, pLex, RWORD_Else);

			RWORD rwordNext = RwordLookup(pLex);
			CSTNode * pStnodStatement = (rwordNext == RWORD_If) ? 
											PStnodParseSelectionStatement(pParctx, pLex, nullptr) :
											PStnodExpectCompoundStatement(pParctx, pLex, PCozFromRword(rword));

			if (pStnodStatement->m_grfstnod.FIsSet(FSTNOD_EntryPoint))
			{
				EWC_ASSERT(pStnodStatement->m_park == PARK_ProcedureDefinition, "Unknown entry point park");
				ParseError( pParctx, pLex, "Local functions not directly allowed under conditional, add {}");
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
		auto pStnodSw = PStnodParseSwitchStatement(pParctx, pLex);

		if (ppStidentLabel)
		{
			EWC_ASSERT(!pStnodSw->m_pStident, "expected null identifier");
			pStnodSw->m_pStident = *ppStidentLabel;
			*ppStidentLabel = nullptr;
		}

		return pStnodSw;

	}
	return nullptr;
}

CSTNode * PStnodParseIterationStatement(CParseContext * pParctx, SLexer * pLex, CSTIdentifier ** ppStidentLabel)
{
	RWORD rword = RwordLookup(pLex);
	if (rword == RWORD_For)
	{
		CSTNode * pStnodFor = PStnodParseReservedWord(pParctx, pLex, RWORD_For);

		auto * pStfor = EWC_NEW(pParctx->m_pAlloc, CSTFor) CSTFor();
		pStnodFor->m_pStfor = pStfor;

		if (ppStidentLabel)
		{
			pStnodFor->m_pStident = *ppStidentLabel;
			*ppStidentLabel = nullptr;
		}

		SLexerLocation lexloc(pLex);
		CSymbolTable * pSymtabLoop = pParctx->m_pWork->PSymtabNew("for", &pParctx->m_pWork->m_unsetTin);
		pStnodFor->m_pSymtab = pSymtabLoop;

		PushSymbolTable(pParctx, pSymtabLoop, lexloc);

		GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl;
		auto * pStnodDecl =  PStnodParseParameter(pParctx, pLex, pParctx->m_pSymtab, grfpdecl);
		if (pStnodDecl)
			ExpectEndOfStatement(pParctx, pLex);
		else
			Expect(pParctx, pLex, TOK(';'));
		pStfor->m_iStnodDecl = pStnodFor->IAppendChild(pStnodDecl);

		CSTNode * pStnodPred = PStnodParseExpression(pParctx, pLex);
		if (pStnodPred)
			ExpectEndOfStatement(pParctx, pLex);
		else
			Expect(pParctx, pLex, TOK(';'));
		pStfor->m_iStnodPredicate = pStnodFor->IAppendChild(pStnodPred);

		CSTNode * pStnodIncrement = PStnodParseExpression(pParctx, pLex);
		if (pStnodIncrement)
			ExpectEndOfStatement(pParctx, pLex);
		else
			Expect(pParctx, pLex, TOK(';'));
		pStfor->m_iStnodIncrement = pStnodFor->IAppendChild(pStnodIncrement);

		CSTNode * pStnodBody = PStnodExpectCompoundStatement(pParctx, pLex, PCozFromRword(rword));
		pStfor->m_iStnodBody = pStnodFor->IAppendChild(pStnodBody);

		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pSymtabLoop == pSymtabPop, "CSymbol table push/pop mismatch (list)");

		return pStnodFor;
	}

	else if (rword == RWORD_ForEach)
	{
		//for decl := iterMake {}
		//for decl : iterType = iterMake {}
		//for decl = iterMake {}
		//for iter {}
		//and maybe anonymous iterator...    for : iterMake {}			(I like the simplicity of this, but it's not really cohesive)
		//and maybe anonymous iterator...    for --- := iterMake {}		(This one matches the other syntaxes, but is ugly and verbose)

		CSTNode * pStnodFor = PStnodParseReservedWord(pParctx, pLex, RWORD_ForEach);
		
		auto * pStfor = EWC_NEW(pParctx->m_pAlloc, CSTFor) CSTFor();
		pStnodFor->m_pStfor = pStfor;

		if (ppStidentLabel)
		{
			pStnodFor->m_pStident = *ppStidentLabel;
			*ppStidentLabel = nullptr;
		}

		SLexerLocation lexloc(pLex);
		CSymbolTable * pSymtabLoop = pParctx->m_pWork->PSymtabNew("for", &pParctx->m_pWork->m_unsetTin);
		pStnodFor->m_pSymtab = pSymtabLoop;

		PushSymbolTable(pParctx, pSymtabLoop, lexloc);
		CSTNode * pStnodDecl = PStnodParseParameter(pParctx, pLex, pSymtabLoop, FPDECL_None);
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
			pStnodIterator = PStnodParseCastExpression(pParctx, pLex);
			EWC_ASSERT(pStnodIterator, "null iterator");
			pStfor->m_iStnodIterator = pStnodFor->IAppendChild(pStnodIterator);

			if (FConsumeToken(pLex, TOK('=')))
			{
				auto pStnodInit = PStnodParseExpression(pParctx, pLex);
				pStfor->m_iStnodInit = pStnodFor->IAppendChild(pStnodInit);
			}

			if (pStfor->m_iStnodIterator == -1 && pStfor->m_iStnodInit == -1)
			{
				ParseError(pParctx, pLex, "Could not determine for loop iterator");
			}
		}

		if (!pStnodIterator)
		{
			ParseError(pParctx, pLex, "Could not determine iterator used by for loop");
		}
		else
		{
			// add iterIsDone AST
			{
				CSTNode * pStnodPredIdent = PStnodAllocateIdentifier(pParctx, lexloc, "iterIsDone");

				CSTNode * pStnodArg = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArg->m_tok = TOK_Reference;
				pStnodArg->m_park = PARK_UnaryOp;
				pStnodArg->IAppendChild(PStnodCopy(pParctx->m_pAlloc, pStnodIterator));

				CSTNode * pStnodCall = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodCall->m_tok = TOK(pLex->m_tok);
				pStnodCall->m_park = PARK_ProcedureCall;
				pStnodCall->IAppendChild(pStnodPredIdent);
				pStnodCall->IAppendChild(pStnodArg);

				pStfor->m_iStnodPredicate = pStnodFor->IAppendChild(pStnodCall);
			}

			// add iterNext 
			{
				CSTNode * pStnodIncIdent = PStnodAllocateIdentifier(pParctx, lexloc, "iterNext");

				CSTNode * pStnodArg = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArg->m_tok = TOK_Reference;
				pStnodArg->m_park = PARK_UnaryOp;
				pStnodArg->IAppendChild(PStnodCopy(pParctx->m_pAlloc, pStnodIterator));

				CSTNode * pStnodCall = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodCall->m_tok = TOK(pLex->m_tok);
				pStnodCall->m_park = PARK_ProcedureCall;
				pStnodCall->IAppendChild(pStnodIncIdent);
				pStnodCall->IAppendChild(pStnodArg);

				pStfor->m_iStnodIncrement = pStnodFor->IAppendChild(pStnodCall);
			}

		}

		CSTNode * pStnodBody = PStnodExpectCompoundStatement(pParctx, pLex, PCozFromRword(rword));
		pStfor->m_iStnodBody = pStnodFor->IAppendChild(pStnodBody);

		CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
		EWC_ASSERT(pSymtabLoop == pSymtabPop, "CSymbol table push/pop mismatch (list)");

		return pStnodFor;
	}

	if (rword == RWORD_While)
	{
		CSTNode * pStnodWhile = PStnodParseReservedWord(pParctx, pLex, RWORD_While);
		CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
		pStnodWhile->IAppendChild(pStnodExp);
		
		if (ppStidentLabel)
		{
			pStnodWhile->m_pStident = *ppStidentLabel;
			*ppStidentLabel = nullptr;
		}
		
		CSTNode * pStnodStatement = PStnodExpectCompoundStatement(pParctx, pLex, PCozFromRword(rword));
		pStnodWhile->IAppendChild(pStnodStatement);
		return pStnodWhile;
	}

	return nullptr;
}

CSTNode * PStnodParseStatement(CParseContext * pParctx, SLexer * pLex)
{

	CSTNode * pStnod = PStnodParseCompoundStatement(pParctx, pLex, nullptr);
	if (pStnod)
		return pStnod;

	// Note - Declarations and definition checks need to come first because they peek ahead to see 
	//  if an identifier has ::, : or :=

	pStnod = PStnodParseDecl(pParctx, pLex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseDefinition(pParctx, pLex);
	if (pStnod)
		return pStnod;


	pStnod = PStnodParseExpressionStatement(pParctx, pLex);
	if (pStnod)
		return pStnod;

	// handle label for switches or loops

	CSTIdentifier * pStidentLabel = nullptr;
	RWORD rword = RwordLookup(pLex);
	if (rword == RWORD_LabelDirective)
	{
		TokNext(pLex);

		if (pLex->m_tok != TOK_Identifier)
		{
			ParseError(pParctx, pLex, "Encountered Label directive without label string");
		}
		else
		{
			pStidentLabel = EWC_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
			pStidentLabel->m_str = pLex->m_str;
			TokNext(pLex);
		}
	}

	pStnod = PStnodParseSelectionStatement(pParctx, pLex, &pStidentLabel);
	
	if (!pStnod)
	{
		pStnod = PStnodParseIterationStatement(pParctx, pLex, &pStidentLabel);
	}

	if (pStidentLabel)
	{
		ParseError(pParctx, pLex, "Label directive should precede loop or switch statement.");
		pParctx->m_pAlloc->EWC_FREE(pStidentLabel);
	}

	if (pStnod)
		return pStnod;

	return PStnodParseJumpStatement(pParctx, pLex);
}

bool FParseImportDirectives(CWorkspace * pWork, SLexer * pLex)
{
	if (pLex->m_tok == TOK_ReservedWord)
	{
		RWORD rword = RwordLookup(pLex);

		if ((rword == RWORD_ImportDirective) | (rword == RWORD_ForeignLibraryDirective))
		{
			TokNext(pLex);
			if (pLex->m_tok == TOK_Literal && pLex->m_litk == LITK_String)
			{
				if (rword == RWORD_ImportDirective)
				{
					char aChFilename[CWorkspace::s_cBFilenameMax];
					(void)CChConstructFilename(pLex->m_str.PCoz(), CWorkspace::s_pCozSourceExtension, aChFilename, EWC_DIM(aChFilename));
					(void)pWork->PFileEnsure(aChFilename, CWorkspace::FILEK_Source);
				}
				else if (EWC_FVERIFY(rword == RWORD_ForeignLibraryDirective, "unknown directive"))
				{
					(void) pWork->PFileEnsure(pLex->m_str.PCoz(), CWorkspace::FILEK_Library);
				}
				TokNext(pLex);
				return true;
			}
			else
			{
				ParseError(
					pWork->m_pParctx,
					pLex,
					"expected path following %s directive",
					(rword == RWORD_ImportDirective) ? "#import" : "#foreign_library");
			}
		}
	}
	return false;
}

void ParseGlobalScope(CWorkspace * pWork, SLexer * pLex, bool fAllowIllegalEntries)
{
	CParseContext * pParctx = pWork->m_pParctx;

	// load the first token
	TokNext(pLex);

	while (pLex->m_tok != TOK_Eof)
	{
		if (FParseImportDirectives(pWork, pLex))
			continue;

		CSTNode * pStnod = PStnodParseStatement(pWork->m_pParctx, pLex);

		if (!pStnod)
		{
			ParseError( pParctx, pLex, "Unexpected token at global scope '%s'", PCozCurrentToken(pLex));
			break;
		}

		pWork->AppendEntry(pStnod, pParctx->m_pSymtab);
		if (!fAllowIllegalEntries)
		{
			bool fIsDecl = pStnod->m_park == PARK_Decl;
			bool fIsDefinition = (pStnod->m_park ==PARK_ProcedureDefinition) | 
								(pStnod->m_park == PARK_EnumDefinition) | 
								(pStnod->m_park == PARK_StructDefinition);
			if (!fIsDecl | fIsDefinition)
			{
				ParseError(
					pParctx,
					pLex,
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

// Symbol lookup rules are as follows 
// Types symbols are unordered and visible anywhere within the symbol table parent hierarchy
// Instances are ordered and only visible within the current nesting level or the global one

enum TABVIS
{
	TABVIS_Unordered,		// all symbols are visible
	TABVIS_Ordered,			// instances are visible, if they come later lexically
	TABVIS_NoInstances,		// no instances, only types are visible
};

static inline TABVIS TabvisCompute(CSymbolTable * pSymtabCur, s32 iNestingDepthQuery, GRFSYMLOOK grfsymlook)
{
	if ((pSymtabCur->m_iNestingDepth == 0) | (grfsymlook.FIsSet(FSYMLOOK_IgnoreOrder)))
		return TABVIS_Unordered;

	if (pSymtabCur->m_iNestingDepth < iNestingDepthQuery)
		return TABVIS_NoInstances;
	return TABVIS_Ordered;
}

SSymbol * CSymbolTable::CSymbolIterator::PSymNext()
{
	if (!m_pSym)
		return nullptr;

	SSymbol * apSym[3] = {nullptr, nullptr, nullptr}; // cur, next, buffer
	int ipSym = 0;

	auto tabvis = TabvisCompute(m_pSymtab, m_pSymtab->m_iNestingDepth, m_grfsymlook);
	auto pSymIt = m_pSym;
	SSymbol * pSymReturn = nullptr;
	while (pSymIt)
	{
		SLexerLocation lexlocSym = (pSymIt->m_pStnodDefinition) ? pSymIt->m_pStnodDefinition->m_lexloc : SLexerLocation();
		bool fVisibleWhenNested = pSymIt->m_grfsym.FIsSet(FSYM_VisibleWhenNested);
		if (fVisibleWhenNested | (tabvis == TABVIS_Unordered) | ((tabvis < TABVIS_NoInstances) & (lexlocSym <= m_lexloc)))
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
			tabvis = TabvisCompute(pSymtabIt, m_pSymtab->m_iNestingDepth, m_grfsymlook);
			pSymIt = *ppSym;
			while (pSymIt)
			{
				SLexerLocation lexlocSym = (pSymIt->m_pStnodDefinition) ? pSymIt->m_pStnodDefinition->m_lexloc : SLexerLocation();
				bool fVisibleWhenNested = pSymIt->m_grfsym.FIsSet(FSYM_VisibleWhenNested);
				if (fVisibleWhenNested | (tabvis == TABVIS_Unordered) | ((tabvis < TABVIS_NoInstances) & (lexlocSym <= m_lexloc)))
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

void AddSimpleBuiltInType(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strName, TINK tink)
{
	STypeInfo * pTin = EWC_NEW(pSymtab->m_pAlloc, STypeInfo) STypeInfo(
																strName,
																StrUniqueName(pSymtab->m_pUnsetTin, strName),
																tink);

	pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTin);
}

void AddBuiltInInteger(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strName, u32 cBit, bool fSigned)
{
	STypeInfoInteger * pTinint = EWC_NEW(pSymtab->m_pAlloc, STypeInfoInteger) STypeInfoInteger(
																				strName,
																				StrUniqueName(pSymtab->m_pUnsetTin, strName),
																				cBit,
																				fSigned);
	pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTinint);
}


void AddBuiltInAlias(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strNameNew, const CString & strNameOld)
{
	STypeInfo *	pTinOld = pSymtab->PTinBuiltin(strNameOld);
	if (EWC_FVERIFY(pTinOld, "bad built in alias"))
	{
		switch (pTinOld->m_tink)
		{
		case TINK_Integer:
			{
				auto pTinintOld = PTinDerivedCast<STypeInfoInteger *>(pTinOld);
				auto pTinintNew = EWC_NEW(pSymtab->m_pAlloc, STypeInfoInteger) 
									STypeInfoInteger(
										strNameNew,
										StrUniqueName(pSymtab->m_pUnsetTin, strNameNew),
										pTinintOld->m_cBit,
										pTinintOld->m_fIsSigned);

				pTinintNew->m_pTinNative = (pTinOld->m_pTinNative) ? pTinOld->m_pTinNative : pTinOld;
				pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTinintNew);
			} break;
		case TINK_Float:
			{
				auto pTinfloatOld = PTinDerivedCast<STypeInfoFloat *>(pTinOld);
				auto pTinfloatNew = EWC_NEW(pSymtab->m_pAlloc, STypeInfoFloat) 
										STypeInfoFloat(
											strNameNew,
											StrUniqueName(pSymtab->m_pUnsetTin, strNameNew),
											pTinfloatOld->m_cBit);

				pTinfloatNew->m_pTinNative = (pTinOld->m_pTinNative) ? pTinOld->m_pTinNative : pTinOld;
				pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTinfloatNew);
			} break;
		default:
			EWC_ASSERT(false, "unsupported built in alias type");
		}
	}
}

void AddBuiltInFloat(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strName, u32 cBit)
{
	STypeInfoFloat * pTinfloat = EWC_NEW(pSymtab->m_pAlloc, STypeInfoFloat) STypeInfoFloat(
																				strName,
																				StrUniqueName(pSymtab->m_pUnsetTin, strName),
																				cBit);
	pSymtab->AddBuiltInType(nullptr, nullptr, pTinfloat);
}

void AddBuiltInLiteral(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strName, LITK litk, s8 cBit, bool fIsSigned)
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
		paryPTinlit->SetAlloc(pSymtab->m_pAlloc, EWC::BK_Parse, 8);
	}
	paryPTinlit->Append(pTinlit);
}

void CSymbolTable::AddBuiltInSymbols(CWorkspace * pWork)
{
	AddSimpleBuiltInType(pWork, this, "bool", TINK_Bool);
	AddSimpleBuiltInType(pWork, this, "void", TINK_Void);

	AddBuiltInInteger(pWork, this, "u8", 8, false);
	AddBuiltInInteger(pWork, this, "u16", 16, false);
	AddBuiltInInteger(pWork, this, "u32", 32, false);
	AddBuiltInInteger(pWork, this, "char", 32, false);
	AddBuiltInInteger(pWork, this, "u64", 64, false);

	AddBuiltInInteger(pWork, this, "s8", 8, true);
	AddBuiltInInteger(pWork, this, "s16", 16, true);
	AddBuiltInInteger(pWork, this, "s32", 32, true);
	AddBuiltInInteger(pWork, this, "s64", 64, true);

#if EWC_X64
	AddBuiltInAlias(pWork, this, "int", "s64");
	AddBuiltInAlias(pWork, this, "uint", "u64");
	AddBuiltInAlias(pWork, this, "sSize", "s64");
	AddBuiltInAlias(pWork, this, "uSize", "u64");
#else
	AddBuiltInAlias(pWork, this, "int", "s32");
	AddBuiltInAlias(pWork, this, "uint", "u32");
	AddBuiltInAlias(pWork, this, "sSize", "s32");
	AddBuiltInAlias(pWork, this, "uSize", "u32");
#endif

	AddBuiltInFloat(pWork, this, "f32", 32);
	AddBuiltInFloat(pWork, this, "f64", 64);
	AddBuiltInAlias(pWork, this, "float", "f32");
	AddBuiltInAlias(pWork, this, "double", "f64");

	AddBuiltInLiteral(pWork, this, "__bool_Literal", LITK_Bool, 8, false);
	AddBuiltInLiteral(pWork, this, "__u8_Literal", LITK_Integer, 8, false);
	AddBuiltInLiteral(pWork, this, "__u16_Literal", LITK_Integer, 16, false);
	AddBuiltInLiteral(pWork, this, "__u32_Literal", LITK_Integer, 32, false);
	AddBuiltInLiteral(pWork, this, "__u64_Literal", LITK_Integer, 64, false);
	AddBuiltInLiteral(pWork, this, "__s8_Literal", LITK_Integer, 8, true);
	AddBuiltInLiteral(pWork, this, "__s16_Literal", LITK_Integer, 16, true);
	AddBuiltInLiteral(pWork, this, "__s32_Literal", LITK_Integer, 32, true);
	AddBuiltInLiteral(pWork, this, "__s64_Literal", LITK_Integer, 64, true);
	AddBuiltInLiteral(pWork, this, "__f32_Literal", LITK_Float, 32, true);
	AddBuiltInLiteral(pWork, this, "__f64_Literal", LITK_Float, 64, true);
	AddBuiltInLiteral(pWork, this, "__string_Literal", LITK_String, -1, true);
	AddBuiltInLiteral(pWork, this, "__char_Literal", LITK_Char, 32, true);
	AddBuiltInLiteral(pWork, this, "__void_Literal", LITK_Null, -1, true);
}



SSymbol * CSymbolTable::PSymEnsure(
	SErrorManager * pErrman,
	const CString & strName,
	CSTNode * pStnodDefinition,
	GRFSYM grfsym,
	FSHADOW fshadow)
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

			 if (fshadow != FShadow_ShadowingAllowed)
			 {
				s32 iLine = 0;
				s32 iCol = 0;
				const char * pChzFilename = "unknown";
				if (pSymPrev->m_pStnodDefinition)
				{
					CalculateLinePosition(pErrman->m_pWork, &pSymPrev->m_pStnodDefinition->m_lexloc, &iLine, &iCol);
					pChzFilename = pSymPrev->m_pStnodDefinition->m_lexloc.m_strFilename.PCoz();

				}

				EmitError(pErrman, &lexloc, "%s symbol shadows previous type definition at %s(%d, %d)", 
					strName.PCoz(),
					pChzFilename,
					iLine,
					iCol);
			 }
		}
	}
	
	if (!pSym)
	{
		pSym = EWC_NEW(m_pAlloc, SSymbol) SSymbol;
		(void) m_hashHvPSym.FinsEnsureKeyAndValue(strName.Hv(), pSym);

		pSym->m_aryPSymReferencedBy.SetAlloc(m_pAlloc, BK_Dependency, 4);
		pSym->m_symdep = SYMDEP_Nil;
	}

	pSym->m_strName = strName;
	pSym->m_pStnodDefinition = pStnodDefinition;
	pSym->m_grfsym = grfsym;
	pSym->m_pTin = nullptr;
	pSym->m_pVal = nullptr;
	pSym->m_pSymPrev = pSymPrev;

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
			auto tabvis = TabvisCompute(this, m_iNestingDepth, grfsymlook);
			SSymbol * pSym = *ppSym;

			while (pSym)
			{
				SLexerLocation lexlocSym = (pSym->m_pStnodDefinition) ? pSym->m_pStnodDefinition->m_lexloc : SLexerLocation();
				bool fVisibleWhenNested = pSym->m_grfsym.FIsSet(FSYM_VisibleWhenNested);
				if (fVisibleWhenNested | (tabvis == TABVIS_Unordered) | ((tabvis < TABVIS_NoInstances) & (lexlocSym <= lexloc)))
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
			auto tabvis = TabvisCompute(pSymtab, m_iNestingDepth, grfsymlook);
			SSymbol * pSym = *ppSym;
			while (pSym)
			{
				SLexerLocation lexlocSym = (pSym->m_pStnodDefinition) ? pSym->m_pStnodDefinition->m_lexloc : SLexerLocation();
				bool fVisibleWhenNested = pSym->m_grfsym.FIsSet(FSYM_VisibleWhenNested);
				if (fVisibleWhenNested | (tabvis == TABVIS_Unordered) | ((tabvis < TABVIS_NoInstances) & (lexlocSym <= lexloc)))
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
	pTinptr->m_pTinPointedTo = pTinPointedTo;

	AddManagedTin(pTinptr);
	return pTinptr;
}

void CSymbolTable::AddManagedTin(STypeInfo * pTin)
{
	m_arypTinManaged.Append(pTin);
}
	
void AppendTypeDescriptor(STypeInfo * pTin, SStringEditBuffer * pSeb)
{
	switch (pTin->m_tink)
	{
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;

			switch(pTinary->m_aryk)
			{
			case ARYK_Fixed:
				{
					char aCh[48];
					SStringBuffer strbuf(aCh, EWC_DIM(aCh));
					FormatCoz(&strbuf, "[%d]", pTinary->m_c);
					pSeb->AppendCoz(aCh);
				} break;
			case ARYK_Reference:	pSeb->AppendCoz("[]");	break;
			case ARYK_Dynamic:		pSeb->AppendCoz("[..]");	break;
			}
			AppendTypeDescriptor(pTinary->m_pTin, pSeb);
		} break;
	case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;

			pSeb->AppendCoz("&");
			AppendTypeDescriptor(pTinptr->m_pTinPointedTo, pSeb);
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;

			pSeb->AppendCoz(pTinproc->m_strUnique.PCoz());
			pSeb->AppendCoz("(");

			auto ppTinParamMax = pTinproc->m_arypTinParams.PMac();
			for (auto ppTin = pTinproc->m_arypTinParams.A(); ppTin != ppTinParamMax; ++ppTin)
			{
				AppendTypeDescriptor(*ppTin, pSeb);
				if (ppTin+1 != ppTinParamMax)
				{
					pSeb->AppendCoz(",");
				}
			}

			if (pTinproc->m_fHasVarArgs)
			{
				pSeb->AppendCoz(",..");
			}
			pSeb->AppendCoz(")->(");

			auto ppTinReturnMax = pTinproc->m_arypTinReturns.PMac();
			for (auto ppTin = pTinproc->m_arypTinReturns.A(); ppTin != ppTinReturnMax; ++ppTin)
			{
				AppendTypeDescriptor(*ppTin, pSeb);
				if (ppTin+1 != ppTinReturnMax)
				{
					pSeb->AppendCoz(",");
				}
			}
			pSeb->AppendCoz(")");

			if (pTinproc->m_inlinek != INLINEK_Nil)
			{
				pSeb->AppendCoz(PChzFromInlinek(pTinproc->m_inlinek));
			}

			if (pTinproc->m_callconv != CALLCONV_Nil)
			{
				pSeb->AppendCoz(PChzFromCallconv(pTinproc->m_callconv));
			}

		} break;
	default:
		pSeb->AppendCoz(pTin->m_strUnique.PCoz());
	}
}

STypeInfo * CSymbolTable::PTinMakeUniqueBase(STypeInfo * pTin, SStringEditBuffer * pSeb)
{
	if (pTin->m_grftin.FIsSet(FTIN_IsUnique) || pTin->m_tink == TINK_Literal)
		return pTin;

	pSeb->Clear();
	AppendTypeDescriptor(pTin, pSeb);

	pTin->m_strDesc = CString(pSeb->PCoz(), pSeb->CB());

	STypeInfo ** ppTin;
	FINS fins = m_phashHvPTinUnique->FinsEnsureKey(pTin->m_strDesc.Hv(), &ppTin);
	if (fins == FINS_AlreadyExisted)
	{
		// NOTE: doesn't delete non-unique type!
		return *ppTin;
	}

	if (ppTin)
	{
		pTin->m_grftin.AddFlags(FTIN_IsUnique);

		if (pTin->m_pTinNative)
		{
			pTin->m_pTinNative = PTinMakeUniqueBase(pTin->m_pTinNative, pSeb);
		}

		*ppTin = pTin;
	}
	return pTin;
}

void CSymbolTable::AddManagedSymtab(CSymbolTable * pSymtab)
{
	EWC_ASSERT(pSymtab->m_pSymtabNextManaged == nullptr, "trying to add managed symtab");

	pSymtab->m_pSymtabNextManaged = m_pSymtabNextManaged;
	m_pSymtabNextManaged = pSymtab;
}

void CSymbolTable::AddBuiltInType(SErrorManager * pErrman, SLexer * pLex, STypeInfo * pTin)
{
	// NOTE: This function is for built-in types without a lexical order, so we shouldn't be calling it on an ordered table
	EWC_ASSERT(m_iNestingDepth == 0, "Cannot add built-in types to ordered symbol table.");

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

		auto pSym = PSymEnsure(pErrman, strName, nullptr, FSYM_IsBuiltIn | FSYM_IsType | FSYM_VisibleWhenNested);
		pSym->m_pTin = pTin;
	}
	else
	{
		SLexerLocation lexloc = (pLex) ? SLexerLocation(pLex) : SLexerLocation();
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
		if (pSym->m_pTin)
		{
			printf("%p: %s %x : '%s' %x\n",pSym, pSym->m_strName.PCoz(), pSym->m_strName.Hv(), pSym->m_pTin->m_strName.PCoz(), pSym->m_pTin->m_strName.Hv());
		}
		else
		{
			printf("%p: %s : 'nil'\n",pSym, pSym->m_strName.PCoz());
		}
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
:m_tok(TOK_Nil)
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
,m_pOptype(nullptr)
,m_pSymtab(nullptr)
,m_pSym(nullptr)
,m_arypStnodChild(pAlloc, EWC::BK_SyntaxTree)
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

	if (m_pOptype)
	{
		pAlloc->EWC_DELETE(m_pOptype);
		m_pOptype = nullptr;
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
		case PARK_ExpressionList:
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
			AppendCoz(pStrbuf, PCozFromTok(TOK_Reference));
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
			auto pTinlit = (STypeInfoLiteral *)pTin;
			if (pTinlit->m_litty.m_litk == LITK_Enum)
			{
				PrintTypeInfo(pStrbuf, pTinlit->m_pTinSource, PARK_Nil, grfdbgstr);
				AppendCoz(pStrbuf, " ");
			}
			
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
				EWC_ASSERT(false, "unknown literal %s", PCozFromTok(pStnod->m_tok));
				return;
			}
		}
	case PARK_AdditiveOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_MultiplicativeOp:	    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_ShiftOp:			    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_EqualityOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_RelationalOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_BitwiseAndOrOp:	    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_LogicalAndOrOp:	    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_UnaryOp:			    FormatCoz(pStrbuf, "unary[%s]", PCozFromTok(pStnod->m_tok));		return;
	case PARK_PostfixUnaryOp:		FormatCoz(pStrbuf, "postUnary[%s]", PCozFromTok(pStnod->m_tok));	return;
	case PARK_AssignmentOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_ArrayElement:		    AppendCoz(pStrbuf, "elem");					return;
	case PARK_MemberLookup:		    AppendCoz(pStrbuf, "member");				return;
	case PARK_ProcedureCall:		AppendCoz(pStrbuf, "procCall");				return;
	case PARK_ExpressionList:
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
		if (pStnod->m_pTin == nullptr && (pStnod->m_park == PARK_Identifier || pStnod->m_park == PARK_ReservedWord))
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

	SLexer lex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &lex, pCozIn);

	EWC_ASSERT(pWork->m_pErrman->m_cError == 0, "parse errors detected");
	pWork->m_pErrman->Clear();

	ParseGlobalScope(pWork, &lex, true);

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

			EWC_ASSERT(pWork->PFileLookup(pCoz, CWorkspace::FILEK_Source), "expected import %s", pCoz);
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

			EWC_ASSERT(pWork->PFileLookup(pCoz, CWorkspace::FILEK_Library), "expected import %s", pCoz);
		}
		EWC_ASSERT(pWork->CFile(CWorkspace::FILEK_Library) == ipCoz, "missing import");
	}

	EndParse(pWork, &lex);
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

		pCozIn = "operator * (lhs: SFoo, nRhs: int) -> int { return lhs.m_n + nRhs }";
		pCozOut = "(func $operator* (params (decl $lhs $SFoo) (decl $nRhs $int)) $int ({} (return (+ (member $lhs $m_n) $nRhs))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "switch (i) { case 1, 2: foo(); default: foo() }";
		pCozOut = "(switch $i (case 1 2 ({} (procCall $foo))) (default ({} (procCall $foo))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "switch (i) { case 1: { foo(); bar() } default: foo() }";
		pCozOut = "(switch $i (case 1 ({} ({} (procCall $foo) (procCall $bar)))) (default ({} (procCall $foo))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick[2] == ack";
		pCozOut = "(== (elem $ick 2) $ack)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "for_each it := iterMake(foo) { }";
		pCozOut = "(for_each (decl $it (procCall $iterMake $foo)) (procCall $iterIsDone (unary[&] $it)) (procCall $iterNext (unary[&] $it)) ({}))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "for_each it = iterMake(foo) { }";
		pCozOut = "(for_each $it (procCall $iterMake $foo) (procCall $iterIsDone (unary[&] $it)) (procCall $iterNext (unary[&] $it)) ({}))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = u8"😁+✂";
		pCozOut = u8"(+ $😁 $✂)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "n1: int, g1, g2: float = ---";
		pCozOut = "(decl (decl $n1 $int) (decl $g1 $float) (decl $g2 $float) (---))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "n1, n2: int, g: float";
		pCozOut = "(decl (decl $n1 $int) (decl $n2 $int) (decl $g $float))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "(@ppFunc)(2)";
		pCozOut = "(procCall (unary[@] $ppFunc) 2)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "func: (n: s32)->s64 = fooFunc";		// procedure reference declaration
		pCozOut = "(decl $func (procref (params (decl $n $s32)) $s64) $fooFunc)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "apFunc[2](2)";
		pCozOut = "(procCall (elem $apFunc 2) 2)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "pN = cast(& int) pG";
		pCozOut = "(= $pN (cast (ptr $int) $pG))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "aN := {:int: 2, 4, 5} ";
		pCozOut = "(decl $aN (arrayLit $int ({} 2 4 5)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ ENUMK enum int { ENUMK_Nil : -1, ENUMK_Foo, ENUMK_Bah : 3 } enumk := ENUMK.Foo }";
		pCozOut = "({} (enum $ENUMK $int ({} (enumConst $nil) (enumConst $min) (enumConst $last) (enumConst $max) (arrayLit $names) (arrayLit $values) (enumConst $ENUMK_Nil (unary[-] 1)) (enumConst $ENUMK_Foo) (enumConst $ENUMK_Bah 3)))"
			" (decl $enumk (member $ENUMK $Foo)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SOut struct { SIn struct { ConstTwo :: 2 }} n := SOut.SIn.ConstTwo ";
		pCozOut = "(struct $SOut ({} (struct $SIn ({} (const $ConstTwo 2))))) (decl $n (member (member $SOut $SIn) $ConstTwo))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "PtrType typedef & s16; ArrayType typedef [2] s8 ";
		pCozOut = "(typedef $PtrType (ptr $s16)) (typedef $ArrayType ([] 2 $s8))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SomeConst :: 0xFF ";
		pCozOut = "(const $SomeConst 255)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SomeConst : s16 : 0xFF ";
		pCozOut = "(const $SomeConst $s16 255)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "SFoo struct { m_n := 2 } foo : SFoo; foo.m_n = 1 ";
		pCozOut = "(struct $SFoo ({} (decl $m_n 2))) (decl $foo $SFoo) (= (member $foo $m_n) 1)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "paN : & [4] int";
		pCozOut = "(decl $paN (ptr ([] 4 $int)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "aN : [4] int; n := aN[0]";
		pCozOut = "(decl $aN ([] 4 $int)) (decl $n (elem $aN 0))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "x + 3*5";
		pCozOut = "(+ $x (* 3 5))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "(ugh + foo) / ((x + 3)*5)";
		pCozOut = "(/ (+ $ugh $foo) (* (+ $x 3) 5))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ugh/foo/guh/ack";
		pCozOut = "(/ (/ (/ $ugh $foo) $guh) $ack)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "(5 + -x) * -(3 / foo)";
		pCozOut = "(* (+ 5 (unary[-] $x)) (unary[-] (/ 3 $foo)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick * ack * -(3 / foo)";
		pCozOut = "(* (* $ick $ack) (unary[-] (/ 3 $foo)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick & (ack&&foo | 123) || guh";
		pCozOut = "(|| (& $ick (&& $ack (| $foo 123))) $guh)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ick == ack < foo\n != 123 >= guh";
		pCozOut = "(!= (== $ick (< $ack $foo)) (>= 123 $guh))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		// NOTE - weird ordering shouldn't matter as we will ensure lhs is l-value
		pCozIn = "ick = 5 += foo *= guh";
		pCozOut = "(*= (+= (= $ick 5) $foo) $guh)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "++foo.bah[23]";
		pCozOut = "(unary[++] (elem (member $foo $bah) 23))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ i=5; foo.bah = ack }";
		pCozOut = "({} (= $i 5) (= (member $foo $bah) $ack))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ if i==foo.bar.fug { ick = 3 } else { ick = 7 } }";
		pCozOut = "({} (if (== $i (member (member $foo $bar) $fug)) ({} (= $ick 3)) (else ({} (= $ick 7)))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ while x > 0 { --x } }";
		pCozOut = "({} (while (> $x 0) ({} (unary[--] $x))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ break; continue; return foo=\"test\" }";
		pCozOut = "({} (break) (continue) (return (= $foo \"test\")))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "VarArgs proc (a : int, ..) #foreign";
		pCozOut = "(func $VarArgs (params (decl $a $int) (..)) $void)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ AddNums proc (a : int, b := 1) -> int { return a + b } bah := 3 }";
		pCozOut = "(func $AddNums (params (decl $a $int) (decl $b 1)) $int ({} (return (+ $a $b))))"
			" ({} (decl $bah 3))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "AddLocal proc (nA : int) -> int { nLocal := 2; return nA + nLocal }";
		pCozOut = "(func $AddLocal (params (decl $nA $int)) $int ({} (decl $nLocal 2) (return (+ $nA $nLocal))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ AddNums proc (a : int, b := 1) -> int { return a + b }	AddNums(2, 3) }";
		pCozOut = "(func $AddNums (params (decl $a $int) (decl $b 1)) $int ({} (return (+ $a $b))))"
			" ({} (procCall $AddNums 2 3))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ FooFunc(); n:=BarFunc(x+(ack)) }";
		pCozOut = "({} (procCall $FooFunc) (decl $n (procCall $BarFunc (+ $x $ack))))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "{ NopFunc proc () { guh := 2 } wha : & int }";
		pCozOut = "(func $NopFunc $void ({} (decl $guh 2) (return))) ({} (decl $wha (ptr $int)))";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "STest struct { m_a := 2; m_b : int } boo : int = 3";
		pCozOut = "(struct $STest ({} (decl $m_a 2) (decl $m_b $int))) (decl $boo $int 3)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "pChz := foo; guh : gur = 5; bah : s32 = woo";
		pCozOut = "(decl $pChz $foo) (decl $guh $gur 5) (decl $bah $s32 $woo)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);

		pCozIn = "ForeignFunc proc () -> int #foreign";
		pCozOut = "(func $ForeignFunc $int)";
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut);
			
		pCozIn = "#import \"foo/blah/ack\" #foreign_library \"foo/blah/ack\" "
				"#import \"test\\wha\\huh\" #foreign_library \"test\\wha\\huh\" "
				"#import \"basic\" ";

		pCozOut = "";
		const char * apChzExpectedImport[] = { "foo/blah/ack.moe", "test\\wha\\huh.moe", "basic.moe",nullptr };
		const char * apChzExpectedLibrary[] = { "foo/blah/ack", "test\\wha\\huh", nullptr };
		AssertParseMatchTailRecurse(&work, pCozIn, pCozOut, apChzExpectedImport, apChzExpectedLibrary);

		StaticShutdownStrings(&allocString);
	}
}