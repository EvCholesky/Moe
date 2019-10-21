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
	FPDECL_AllowCompoundDecl	= 0x1,		// allow comma-separated declaration of multiple variables.
	FPDECL_AllowVariadic		= 0x2,		// allow the list to end with variadic arguments (..)
	FPDECL_AllowUninitializer	= 0x4,		// allow decls to specify explicit uninitializers (n:int=---;}
	FPDECL_AllowBakedTypes		= 0x8, 		// allow unspecified generic types (aka $T)
	FPDECL_AllowBakedValues		= 0x10,		// allow types to be marked as baked constant values
	FPDECL_AllowConstants		= 0x20, 	// allow constant declarations
	FPDECL_AllowUsing			= 0x40,		// allow 'using' declarations
	FPDECL_AllowUnnamed			= 0x80,		// allow unnamed declarations (ie foo proc (:$T) )
	FPDECL_AllowShadowing		= 0x100,	// allow shadowing of variable or generic arg names

	FPDECL_None			= 0x0,
	FPDECL_All			= 0xFF,
};
EWC_DEFINE_GRF(GRFPDECL, FPDECL, u32);

enum FEXP
{
	FEXP_AllowLiteralMemberLabel = 0x1,

	FEXP_None			= 0x0,
	FEXP_All			= 0x1,
};
EWC_DEFINE_GRF(GRFEXP, FEXP, u32);

CSTNode * PStnodParseCompoundStatement(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtab);
CSTNode * PStnodExpectCompoundStatement(CParseContext * pParctx, SLexer * pLex, const char * pCozPriorStatement);
CSTNode * PStnodParseDefinition(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseExpression(CParseContext * pParctx, SLexer * pLex, GRFEXP grfexp = FEXP_None);
CSTNode * PStnodParseLogicalOrExpression(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseProcParameterList(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtabProc, bool fIsOpOverload);
CSTNode * PStnodParseReturnArrow(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtabProc);
CSTNode * PStnodParseStatement(CParseContext * pParctx, SLexer * pLex);
CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SLexer * pLex, const char * pCozErrorContext, GRFPDECL grfpdecl);
CSTNode * PStnodParseGenericTypeDecl(CParseContext * pParctx, SLexer * pLex, GRFPDECL grfpdecl);

struct ProcSymtabStack // tag = procss
{
						ProcSymtabStack(CParseContext * pParctx)
						:m_pParctx(pParctx)
						,m_pSymtabPrev(nullptr)
							{ ; }

						~ProcSymtabStack()
						{
							if (m_pSymtabPrev)
								PSymtabPop();
						}

	void				Push(CSymbolTable * pSymtab, const SLexerLocation & lexloc)
							{
								m_pSymtabPrev = m_pParctx->m_pSymtabGeneric;
								m_pParctx->m_pSymtabGeneric = pSymtab;

								::PushSymbolTable(m_pParctx, pSymtab, lexloc);
							}

	CSymbolTable * 		PSymtabPop()
							{
								m_pParctx->m_pSymtabGeneric = m_pSymtabPrev;
								return ::PSymtabPop(m_pParctx);
							}

	CParseContext *		m_pParctx; 
	CSymbolTable *		m_pSymtabPrev;
};

static int g_nSymtabVisitId = 1; // visit index used by symbol table collision walks

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
		"Relational Operator",
		"LogicalAndOr Operator",
		"Assignment Operator",
		"Unary Operator",
		"Postfix Unary Operator",
		"Uninitializer",
		"Cast",
		"Array Element",		// [array, index]
		"Member Lookup",		// [struct, child]
		"Procedure Call",		// [procedure, arg0, arg1, ...]
		"Specialized Struct",
		"List",
		"Parameter List",
		"Expression List",
		"Generic Type Spec",
		"If",
		"Else",
		"Array Decl",
		"Reference Decl",
		"Qualifier Decl",
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
		"Argument Label",
		"Generic Decl",
		"Generic Struct Spec",
		"Type Argument",
		"Baked Value"
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
		"Compound",
		"Pointer",
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
		"Integer",
		"Float",
		"Bool",
		"Pointer",
		"Procedure",
		"Void",
		"Struct",
		"Array",
		"Null",
		"Any",
		"Enum",
		"Qualifier",
		"Interface",
		"Type",
		"ForwardDecl",
		"Literal",
		"Generic",
		"Flag"
	};
	EWC_CASSERT(EWC_DIM(s_mpTinkPChz) == TINK_Max, "missing TINK string");
	if (tink == TINK_Nil)
		return "Nil";

	if ((tink < TINK_Nil) | (tink >= TINK_Max))
		return "Unknown TINK";

	return s_mpTinkPChz[tink];
}

const char * PChzFromQualk(QUALK qualk)
{
	static const char * s_mpQualkPChz[] =
	{
		"const",
		"inarg",
	};
	EWC_CASSERT(EWC_DIM(s_mpQualkPChz) == QUALK_Max, "missing QUALK string");
	if (qualk == QUALK_Nil)
		return "Nil";

	if ((qualk < QUALK_Nil) | (qualk >= QUALK_Max))
		return "Unknown QUALK";

	return s_mpQualkPChz[qualk];
}

void AppendFlagNames(EWC::SStringBuffer * pStrbuf, GRFQUALK grfqualk, const char * pChzSpacer)
{
	const char * pChzSpacerCur = "";
	for (int qualk = QUALK_Min; qualk < QUALK_Max; ++qualk)
	{
		if (grfqualk.FIsSet(0x1 << qualk))
		{
			AppendCoz(pStrbuf, pChzSpacerCur);
			AppendCoz(pStrbuf, PChzFromQualk((QUALK)qualk));
			pChzSpacerCur = pChzSpacer;
		}
	}
}

bool FNeedsImplicitMember(ENUMIMP enumimp, ENUMK enumk)
{
	if (enumimp == ENUMIMP_Names || enumimp == ENUMIMP_Values)
		return true;
	bool fIsFlagEnumimp = enumimp > ENUMIMP_MaxConstant;
	return fIsFlagEnumimp == (enumk == ENUMK_FlagEnum);
}

const char * PChzFromEnumimp(ENUMIMP enumimp)
{
	static const char * s_mpEnumimpPChz[] =
	{
		"nil",		//ENUMIMP_NilConstant,
		"min",		//ENUMIMP_MinConstant,
		"last",		//ENUMIMP_LastConstant,
		"max",		//ENUMIMP_MaxConstant,
		"none",		//ENUMIMP_None,
		"all",		//ENUMIMP_All,
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

CSTValue * PStvalExpected(CSTNode * pStnod)
{
	if (EWC_FVERIFY(pStnod && pStnod->m_pStval, "Expected value"))
		return pStnod->m_pStval;
	static CSTValue stval;
	return &stval;
}

CSTValue * PStvalCopy(CAlloc * pAlloc, CSTValue * pStval)
{
	if (!pStval)
		return nullptr;

	auto pStvalRet = EWC_NEW(pAlloc, CSTValue) CSTValue();
	*pStvalRet = *pStval;
	return pStvalRet;
}

CSTNode * PStnodCopy(CAlloc * pAlloc, CSTNode * pStnodSrc, EWC::CHash<CSTNode *, CSTNode *> * pmpPStnodSrcPStnodDst)
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

	if (pStnodSrc->m_pStmap)
	{
		pStnodDst->m_pStmap = PStmapCopy(pAlloc, pStnodSrc->m_pStmap);
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
			pStnodChildCopy = PStnodCopy(pAlloc, pStnodChild, pmpPStnodSrcPStnodDst);
		}
		pStnodDst->m_arypStnodChild[ipStnod] = pStnodChildCopy;
	}

	if (pmpPStnodSrcPStnodDst)
	{
		pmpPStnodSrcPStnodDst->FinsEnsureKeyAndValue(pStnodSrc, pStnodDst);
	}

	return pStnodDst;
}

void ParseError(CParseContext * pParctx, SLexerLocation * pLexloc, ERRID errid, const char * pChzFormat, ...)
{
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pParctx->m_pWork->m_pErrman, pLexloc, errid, pChzFormat, ap);
}

void ParseError(CParseContext * pParctx, SLexer * pLex, ERRID errid, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pParctx->m_pWork->m_pErrman, &lexloc, errid, pChzFormat, ap);
}

void ParseError(CParseContext * pParctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pParctx->m_pWork->m_pErrman, &lexloc, ERRID_UnknownError, pChzFormat, ap);
}

void ParseWarning(CParseContext * pParctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitWarning(pParctx->m_pWork->m_pErrman, &lexloc, ERRID_UnknownWarning, pChzFormat, ap);
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
		bool fFound = (grflexer != FLEXER_None) && pLex->m_grflexer.FIsSet(grflexer);
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

bool FExpect(CParseContext * pParctx, SLexer * pLex, TOK tokExpected, const char * pCozInfo = nullptr, ...)
{
	if (pLex->m_tok != tokExpected)
	{
		char aB[1024] = {0};
		if (pCozInfo)
		{
			va_list ap;
			va_start(ap, pCozInfo);
#if WIN32
			vsprintf_s(aB, EWC_DIM(aB), pCozInfo, ap);
#else
			vsnprintf(aB, EWC_DIM(aB), pCozInfo, ap);
			aB[EWC_DIM(aB)-1] = 0;
#endif
		}

		auto strUnexpected = StrUnexpectedToken(pLex);
		ParseError(pParctx, pLex, "Expected '%s' before '%s' %s", PCozFromTok(tokExpected), strUnexpected.PCoz(), aB);
		return false;
	}
	else
	{
		TokNext(pLex);
		return true;
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
#if WIN32
			vsprintf_s(aB, EWC_DIM(aB), pCozInfo, ap);
#else
			vsnprintf(aB, EWC_DIM(aB), pCozInfo, ap);
			aB[EWC_DIM(aB)-1] = 0;
#endif
		}

		auto strUnexpected = StrUnexpectedToken(pLex);
		ParseError(pParctx, pLex, ERRID_ExpectedEndOfLine, "Expected end-of-line or ';' before '%s' %s", strUnexpected.PCoz(), aB);
	}
}

CSTNode * PStnodAllocateIdentifier(CAlloc * pAlloc, const SLexerLocation & lexloc, const CString & strIdent)
{
	CSTNode * pStnod = EWC_NEW(pAlloc, CSTNode) CSTNode(pAlloc, lexloc);

	pStnod->m_park = PARK_Identifier;

	auto pStident = EWC_NEW(pAlloc, CSTIdentifier) CSTIdentifier();
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
	auto pStnod = PStnodAllocateIdentifier(pParctx->m_pAlloc, lexloc, strIdent);
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

STypeInfo * PTinForFloat(CSymbolTable * pSymtab, f64 gMin, f64 gLast)
{
	return nullptr;
}

STypeInfo * PTinForInt(CSymbolTable * pSymtab, u64 uMin, u64 nLast)
{
	return nullptr;
}

CSTNode * PStnodParseExpressionList(
	CParseContext * pParctx,
	SLexer * pLex,
	GRFEXP grfexp)
{
	SLexerLocation lexloc(pLex);

	CSTNode * pStnodExp = PStnodParseExpression(pParctx, pLex, grfexp);
	CSTNode * pStnodList = nullptr;

	if (pStnodExp)
	{
		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_park = PARK_ExpressionList;
		pStnodList->IAppendChild(pStnodExp);

		while (FConsumeToken(pLex, TOK(',')))
		{
			CSTNode * pStnodLabel = nullptr;

			pStnodExp = PStnodParseExpression(pParctx, pLex, grfexp);

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
		case TOK_Generic:
			{
				ParseError(pParctx, pLex, "Token '%s' unexpected outside of declaration.", PCozFromTok((TOK)pLex->m_tok));
				TokNext(pLex);
			}
		case TOK_Identifier:
			{
				CSTNode * pStnod = PStnodParseIdentifier(pParctx, pLex);

				if (FConsumeToken(pLex, TOK_ColonColon))
				{
#if 1
					EWC_ASSERT(false, "Generic typespec shorthand is WIP");
#else
					SLexerLocation lexloc(pLex);
					CSTNode * pStnodSpec = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

					pStnodSpec->m_tok = TOK_ColonColon;
					pStnodSpec->m_park = PARK_GenericTypeSpec;

					pStnodSpec->IAppendChild(pStnod);
					do
					{
						auto pStnodTin = PStnodParseTypeSpecifier(pParctx, pLex, "generic specifier", FPDECL_None);
						pStnodSpec->IAppendChild(pStnodTin);
					} while (FConsumeToken(pLex, TOK(',')));

					return pStnodSpec;
#endif
				}
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
				default:
					break;
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
		case TOK(':'): // struct literal
		case TOK('{'): // array literals
			{
				SLexerLocation lexloc(pLex);

				CSTNode * pStnodType = nullptr;

				if (pLex->m_tok == TOK(':'))
				{
					// parse type specifier
					TokNext(pLex); // consume ':'
					
					pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "compound literal", FPDECL_None);

					if (!pStnodType)
					{
						ParseError(pParctx, &lexloc, ERRID_TypeSpecParseFail, "expected type specification following ':'");
						return nullptr;
					}
				}

				if (FConsumeToken(pLex, TOK('{')))
				{
					CSTNode * pStnodLit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodLit->m_tok = TOK(pLex->m_tok);
					pStnodLit->m_park = PARK_CompoundLiteral;

					// We're using a decl here... may need a custom structure
					auto pStdecl = pStnodLit->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

					pStdecl->m_iStnodType = pStnodLit->IAppendChild(pStnodType);

					CSTNode * pStnodValues = PStnodParseExpressionList(pParctx, pLex, FEXP_AllowLiteralMemberLabel);
					pStdecl->m_iStnodInit = pStnodLit->IAppendChild(pStnodValues);

					FExpect(pParctx, pLex, TOK('}'), "while parsing struct/array literal");
					return pStnodLit;
				}


				CSTNode * pStnodArg = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArg->m_tok = TOK(':');
				pStnodArg->m_park = PARK_TypeArgument;
				pStnodArg->IAppendChild(pStnodType);


				auto pTinType = pParctx->m_pSymtab->PTinBuiltin(CSymbolTable::s_strType);
				pStnodArg->m_pTin = pTinType;
				EWC_ASSERT(pStnodType, "expected type spec");

				return pStnodArg;

			} break;
		case '(':	// ( Expression )
			{
				TokNext(pLex); // consume '('

				CSTNode * pStnodReturn = PStnodParseExpression(pParctx, pLex);
				FExpect(pParctx, pLex, TOK(')'));
				return pStnodReturn;
			}

		default: return nullptr;
	}
}

enum FARGLIST
{
	FARGLIST_None				= 0x0,
	FARGLIST_AllowGenericValues = 0x1,

	FARGLIST_All				= 0x1,
};

EWC_DEFINE_GRF(GRFARGLIST, FARGLIST, u32);

CSTNode * PStnodParseBakedConstant(CParseContext * pParctx, SLexer * pLex, PARK parkDesired)
{
	if (!FConsumeToken(pLex, TOK_Generic))
		return nullptr;
	
	SLexerLocation lexloc(pLex);
	CSTNode * pStnodDecl = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodDecl->m_park = parkDesired;

	auto pStdecl = pStnodDecl->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);
	pStnodDecl->m_pStmap = pStdecl;
	pStdecl->m_fIsBakedConstant = true;

	GRFPDECL grfpdecl(FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues);
	auto pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
	if (pStnodIdent)
	{
		pStdecl->m_iStnodIdentifier = pStnodDecl->IAppendChild(pStnodIdent);
	}
	else
	{
		ParseError(pParctx, &lexloc, ERRID_MissingName, "Generic type is missing it's name");
	}

	auto pSymtabGeneric = pParctx->m_pSymtabGeneric;
	if (EWC_FVERIFY(pSymtabGeneric, "expected symbol table"))
	{
		FSHADOW fshadow = (grfpdecl.FIsSet(FPDECL_AllowShadowing)) ? FSHADOW_ShadowingAllowed : FSHADOW_NoShadowing;
		auto pSym = pSymtabGeneric->PSymEnsure(pParctx->m_pWork->m_pErrman, StrFromIdentifier(pStnodIdent), pStnodDecl, FSYM_IsType | FSYM_VisibleWhenNested, fshadow);
		pStnodDecl->m_pSymbase = pSym;

		CString strIdent = StrFromIdentifier(pStnodIdent).PCoz();
	}
	
	if (FConsumeToken(pLex, TOK(':')))
	{
		auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "type argument", grfpdecl);
		pStdecl->m_iStnodType = pStnodDecl->IAppendChild(pStnodType);
	}

	return pStnodDecl;
}

void ParseArgumentList(CParseContext * pParctx, SLexer * pLex, CSTNode * pStnodArgList, GRFARGLIST grfarglist = FARGLIST_None)
{
	while (1)
	{
		CSTNode * pStnodLabel = nullptr;
		const char * pCozLabel = "error";
		CSTNode * pStnodBaked = nullptr;
		if (grfarglist.FIsSet(FARGLIST_AllowGenericValues))
		{
			pStnodBaked = PStnodParseBakedConstant(pParctx, pLex, PARK_Decl);
		}

		if (pStnodBaked)
		{
			pStnodArgList->IAppendChild(pStnodBaked);
		}
		else
		{
			if (pLex->m_tok == TOK_Label)
			{
				TokNext(pLex);
				SLexerLocation lexloc(pLex);

				pStnodLabel = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodLabel->m_tok = TOK_Label;
				pStnodLabel->m_park = PARK_ArgumentLabel;

				CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
				if (!pStnodIdent)
				{
					ParseError(pParctx, pLex, "Argument label did not specify an argument name");
				}
				else
				{
					pCozLabel = StrFromIdentifier(pStnodIdent).PCoz();
					pStnodLabel->IAppendChild(pStnodIdent);
				}

				if (FConsumeToken(pLex, TOK('=')))
				{
					ParseError(pParctx, pLex, "Labeled arguments do not require an assignment operator");
				}
			}

			CSTNode * pStnodArg = PStnodParseLogicalOrExpression(pParctx, pLex);
			if (pStnodLabel)
			{
				if (!pStnodArg)
				{
					CSTNode * pStnodIdent = pStnodLabel->PStnodChildSafe(0);
					ParseError(pParctx, pLex, "Labeled argument '%s' does not specify a value", StrFromIdentifier(pStnodIdent).PCoz());
				}
				else
				{
					pStnodLabel->IAppendChild(pStnodArg);
					pStnodArg = pStnodLabel;
				}
			}

			pStnodArgList->IAppendChild(pStnodArg);

			if ((pStnodArg == nullptr))
				break;
		}

		if (!FConsumeToken(pLex, TOK(',')))
			break;
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
				FExpect(pParctx, pLex, TOK(']'));
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

				ParseArgumentList(pParctx, pLex, pStnod);

				FExpect(
					pParctx,
					pLex,
					TOK(')'),
					"while parsing procedure call '%s'", 
					pStnodIdent ? StrFromIdentifier(pStnodIdent).PCoz() : "unknown");
			} break;
		case TOK_Arrow:
			{ 
				SLexerLocation lexloc(pLex);
				EmitError(pParctx->m_pWork->m_pErrman, &lexloc, ERRID_UnknownError, 
					"c-style member dereference '->' is not required, use '.'");

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

					FExpect(pParctx, pLex, TOK('('));

					CSTNode * pStnodChild = PStnodParseUnaryExpression(pParctx, pLex);
					if (!pStnodChild)
					{
						ParseError(pParctx, pLex, "%s missing argument.", PCozFromRword(rword));
					}
					
					FExpect(pParctx, pLex, TOK(')'));

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
			default:
				break;
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
	auto rword = RwordLookup(pLex);
	if (rword != RWORD_Cast && rword != RWORD_AutoCast)
	{
		return PStnodParseUnaryExpression(pParctx, pLex);
	}

	TokNext(pLex);

	SLexerLocation lexloc(pLex);
	pStnodCast = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodCast->m_park = PARK_Cast;

	auto pStdecl = pStnodCast->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

	if (rword == RWORD_Cast)
	{
		FExpect(pParctx, pLex, TOK('('));

		auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "cast", FPDECL_None);
		pStdecl->m_iStnodType = pStnodCast->IAppendChild(pStnodType);

		FExpect(pParctx, pLex, TOK(')'));
	}

	auto pStnodChild = PStnodParseCastExpression(pParctx, pLex);
	if (!pStnodCast)
		return pStnodChild;

	pStdecl->m_iStnodInit = pStnodCast->IAppendChild(pStnodChild);

	if (pStdecl->m_iStnodInit < 0)
	{
		EmitError(pParctx->m_pWork->m_pErrman, &pStnodCast->m_lexloc, ERRID_UnknownError, "Cast statement missing right hand side");
	}

	if (pStdecl->m_iStnodType < 0 && rword != RWORD_AutoCast)
	{
		EmitError(pParctx->m_pWork->m_pErrman, &pStnodCast->m_lexloc, ERRID_UnknownError, "Cast statement missing type");
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

CSTNode * PStnodParseShiftExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseCastExpression(pParctx, pLex);
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

				CSTNode * pStnodExp = PStnodParseCastExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_ShiftOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseMultiplicativeExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseShiftExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('*'):
		case TOK('/'):
		case TOK('%'):
		case TOK('&'):
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseShiftExpression(pParctx, pLex);
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
		case TOK('|'):
		case TOK('^'):
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

CSTNode * PStnodParseRelationalExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseAdditiveExpression(pParctx, pLex);
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
		case TOK_EqualEqual:
		case TOK_NotEqual:
			{
				SLexerLocation lexloc(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				CSTNode * pStnodExp = PStnodParseAdditiveExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_RelationalOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseLogicalAndExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseRelationalExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		if (pLex->m_tok == TOK_AndAnd)
		{
			SLexerLocation lexloc(pLex);
			TOK tokPrev = TOK(pLex->m_tok);
			TokNext(pLex); // consume operator

			CSTNode * pStnodExp = PStnodParseRelationalExpression(pParctx, pLex);
			pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
		}
		else
		{
			return pStnod;
		}
	}
}

CSTNode * PStnodParseLogicalOrExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseLogicalAndExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		if (pLex->m_tok == TOK_OrOr)
		{
			SLexerLocation lexloc(pLex);
			TOK tokPrev = TOK(pLex->m_tok);	
			TokNext(pLex); // consume operator

			CSTNode * pStnodExp = PStnodParseLogicalAndExpression(pParctx, pLex);
			pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
		}
		else
		{
			return pStnod;
		}
	}
}

CSTNode * PStnodParseAssignmentExpression(CParseContext * pParctx, SLexer * pLex)
{
	CSTNode * pStnod = PStnodParseLogicalOrExpression(pParctx, pLex);
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

				CSTNode * pStnodExp = PStnodParseLogicalOrExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexloc, tokPrev, PARK_AssignmentOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

CSTNode * PStnodParseExpression(CParseContext * pParctx, SLexer * pLex, GRFEXP grfexp)
{
	SLexer lexStart = *pLex;

	CSTNode * pStnodLabel = nullptr;

	if (pLex->m_tok == TOK_Label && grfexp.FIsSet(FEXP_AllowLiteralMemberLabel))
	{
		TokNext(pLex);
		SLexerLocation lexloc(pLex);

		pStnodLabel = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodLabel->m_tok = TOK_Label;
		pStnodLabel->m_park = PARK_ArgumentLabel;

		CSTNode * pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
		if (!pStnodIdent)
		{
			ParseError(pParctx, pLex, "Argument label did not specify an argument name");
		}
		else
		{
			pStnodLabel->IAppendChild(pStnodIdent);
		}
	}

	CSTNode * pStnodExp = PStnodParseAssignmentExpression(pParctx, pLex);
	if (!pStnodExp)
	{
		*pLex = lexStart;
		if (pStnodLabel)
		{
			CSTNode * pStnodIdent = pStnodLabel->PStnodChildSafe(0);
			ParseError(pParctx, pLex, "Labeled expression '%s' does not specify a value", StrFromIdentifier(pStnodIdent).PCoz());

			pParctx->m_pAlloc->EWC_DELETE(pStnodLabel);
		}
		return nullptr;
	}

	if (pStnodLabel)
	{
		pStnodLabel->IAppendChild(pStnodExp);
		pStnodExp = pStnodLabel;
	}

	// TODO: handle Expression > AssignmentExpression , AssignmentExpression

	return pStnodExp;
}

CSTNode * PStnodParseArrayDecl(CParseContext * pParctx, SLexer * pLex, GRFPDECL grfpdecl)
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
			CSTNode * pStnodDim = PStnodParseBakedConstant(pParctx, pLex, PARK_Decl);
			if (pStnodDim)
			{
				auto pSymtab = pParctx->m_pSymtab;

				// BB - do we need to spoof a pStnodType here?
				pStnodDim->m_pTin = pSymtab->PTinBuiltin(CSymbolTable::s_strInt);
			}
			else
			{
				pStnodDim = PStnodParseExpression(pParctx, pLex);
			}

			if (pStnodDim)
			{
				pStnodArray->IAppendChild(pStnodDim);
			}
		}

		FExpect(pParctx, pLex, TOK(']'));
		return pStnodArray;
	}
	return nullptr;
}

CSTNode * PStnodFindChildPark(CParseContext * pParctx, CSTNode * pStnodRoot, PARK park)
{
	CDynAry<CSTNode *> arypStnodStack(pParctx->m_pAlloc, BK_Parse);

	arypStnodStack.Append(pStnodRoot);
	while (arypStnodStack.C())
	{
		auto pStnodIt = arypStnodStack.TPopLast();
		if (pStnodIt->m_park == park)
			return pStnodIt;

		int cpStnodChild = pStnodIt->CStnodChild();
		for (int ipStnodChild = 0; ipStnodChild < cpStnodChild; ++ipStnodChild)
		{
			arypStnodStack.Append(pStnodIt->PStnodChild(ipStnodChild));
		}
	}
	return nullptr;
}

void CheckGenericParams(CParseContext * pParctx, CSTNode * pStnodRoot, CHash<HV, CSTNode *> * pmpHvPStnod, GRFTINGEN * pGrftingen)
{
	CDynAry<CSTNode *> arypStnodStack(pParctx->m_pAlloc, BK_Parse);

	arypStnodStack.Append(pStnodRoot);
	while (arypStnodStack.C())
	{
		auto pStnodIt = arypStnodStack.TPopLast();
		if (pStnodIt->m_park == PARK_GenericDecl)
		{
			auto pTingen = PTinDerivedCast<STypeInfoGeneric *>(pStnodIt->m_pTin);
			if (!pTingen)
				continue;

			pGrftingen->AddFlags(FTINGEN_HasBakedTypeArgs);

			CSTNode ** ppStnodHash;
			FINS fins = pmpHvPStnod->FinsEnsureKey(pTingen->m_strName.Hv(), &ppStnodHash);
			if (fins == FINS_AlreadyExisted)
			{
				s32 iLine;
				s32 iCol;
				auto pLexlocDefinition = &(*ppStnodHash)->m_lexloc;
				CalculateLinePosition(pParctx->m_pWork, pLexlocDefinition, &iLine, &iCol);

				EmitError(pParctx->m_pWork->m_pErrman, &pStnodIt->m_lexloc, ERRID_MultipleAnchorDef, 
					"Generic type $%s is was already anchored here %s(%d,%d)",
					pTingen->m_strName.PCoz(),
					pLexlocDefinition->m_strFilename.PCoz(), iLine, iCol);
			}
			else
			{
				*ppStnodHash = pStnodIt;
			}

			continue;
		}
		else if (pStnodIt->m_park == PARK_Decl)
		{
			auto pStmap = PStmapDerivedCast<CSTDecl *>(pStnodIt->m_pStmap);

			// NOTE: if we have a baked type that is a parameter in another type, we're dealing with 
			//  a generic type - not a baked value
			if (pStmap->m_fIsBakedConstant)
			{
				pGrftingen->AddFlags(FTINGEN_HasBakedTypeArgs);
			}
		}

		int cpStnodChild = pStnodIt->CStnodChild();
		for (int ipStnodChild = 0; ipStnodChild < cpStnodChild; ++ipStnodChild)
		{
			arypStnodStack.Append(pStnodIt->PStnodChild(ipStnodChild));
		}
	}
}

void CheckTinprocGenerics(CParseContext * pParctx, CSTNode * pStnodProc, STypeInfoProcedure * pTinproc)
{
	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnodProc->m_pStmap);
	if (!pStproc)
		return;

	auto pStnodParameterList = pStnodProc->PStnodChildSafe(pStproc->m_iStnodParameterList);
	if (pStnodParameterList)
	{
		CHash<HV, CSTNode *> mpHvPStnod(pParctx->m_pAlloc, BK_Parse);

		int cpStnodParam = pStnodParameterList->CStnodChild();
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
		{
			auto pStnodParam = pStnodParameterList->PStnodChild(ipStnodParam);
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
			if (!pStdecl)
				continue;

			auto pStnodType = pStnodParam->PStnodChildSafe(pStdecl->m_iStnodType);
			if (pStdecl->m_fIsBakedConstant)
			{
				pTinproc->m_grftingen.AddFlags(FTINGEN_HasBakedValueArgs);
			}

			pTinproc->m_mpIptinGrfparmq[ipStnodParam].AssignFlags(FPARMQ_BakedValue, pStdecl->m_fIsBakedConstant);
			pTinproc->m_mpIptinGrfparmq[ipStnodParam].AssignFlags(FPARMQ_TypeArgument, pStdecl->m_iStnodIdentifier < 0);

			if (pStnodType)
			{
				CheckGenericParams(pParctx, pStnodType, &mpHvPStnod, &pTinproc->m_grftingen);
			}
		}
	}

	auto pStnodReturn = pStnodProc->PStnodChildSafe(pStproc->m_iStnodReturnType);
	if (pStnodReturn)
	{
		auto pStnodGeneric = PStnodFindChildPark(pParctx, pStnodReturn, PARK_GenericDecl);
		if (pStnodGeneric)
		{
			auto pTingen = PTinDerivedCast<STypeInfoGeneric *>(pStnodGeneric->m_pTin);
			EmitError(pParctx->m_pWork->m_pErrman, &pStnodReturn->m_lexloc, ERRID_NoGenericReturn,
				"Generic type anchor '$%s' is not allowed in return type",
				(pTingen) ? pTingen->m_strName.PCoz() : "unknown");
		}
	}
}

void CheckTinstructGenerics(CParseContext * pParctx, CSTNode * pStnodStruct, STypeInfoStruct * pTinstruct)
{
	auto pStstruct = PStmapDerivedCast<CSTStruct *>(pStnodStruct->m_pStmap);
	if (!pStstruct)
		return;

#if KEEP_TYPEINFO_DEBUG_STRING
	pTinstruct->m_strDebug = StrFromTypeInfo(pTinstruct);
#endif

	auto pStnodParameterList = pStnodStruct->PStnodChildSafe(pStstruct->m_iStnodParameterList);
	if (pStnodParameterList)
	{
		CHash<HV, CSTNode *> mpHvPStnod(pParctx->m_pAlloc, BK_Parse);

		int cpStnodParam = pStnodParameterList->CStnodChild();
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
		{
			auto pStnodParam = pStnodParameterList->PStnodChild(ipStnodParam);
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
			if (!pStdecl)
				continue;

			auto pStnodType = pStnodParam->PStnodChildSafe(pStdecl->m_iStnodType);
			if (pStdecl->m_fIsBakedConstant)
			{
				pTinstruct->m_grftingen.AddFlags(FTINGEN_HasBakedValueArgs);
			}

			if (pStnodType)
			{
				CheckGenericParams(pParctx, pStnodType, &mpHvPStnod, &pTinstruct->m_grftingen);
			}
		}
	}
}

CSTNode * PStnodParseProcedureReferenceDecl(CParseContext * pParctx, SLexer * pLex)
{
	if (FConsumeToken(pLex, TOK('(')))
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodProc->m_tok = (TOK)pLex->m_tok;
		pStnodProc->m_park = PARK_ProcedureReferenceDecl;

		auto pStproc = pStnodProc->PStmapEnsure<CSTProcedure>(pParctx->m_pAlloc);

		CSymbolTable * pSymtabProc = nullptr; //null symbol table as the we're a forward reference
		CSTNode * pStnodParams = PStnodParseProcParameterList(pParctx, pLex, pSymtabProc, false);
		pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
		FExpect(pParctx, pLex, TOK(')'));

		auto pStnodReturns = PStnodParseReturnArrow(pParctx, pLex, pSymtabProc);
		pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);

		int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;
		int cStnodParams;
		(void)PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

		auto pTinproc =  PTinprocAlloc(pParctx->m_pSymtab, cStnodParams, cStnodReturns, "");
		pTinproc->m_arypTinParams.AppendFill(cStnodParams, nullptr);
		pTinproc->m_arypTinReturns.AppendFill(cStnodReturns, nullptr);

		pTinproc->m_pStnodDefinition = pStnodProc;

		CheckTinprocGenerics(pParctx, pStnodProc, pTinproc);

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
						pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName);
						pTinproc->m_grftinproc.AddFlags(FTINPROC_IsForeign);

						if (!FIsEndOfStatement(pLex) && pLex->m_tok == TOK_Identifier)
						{
							auto pStnodAlias = PStnodParseIdentifier(pParctx, pLex);
							pStproc->m_iStnodForeignAlias = pStnodProc->IAppendChild(pStnodAlias);
						}
					} break;
				case RWORD_CDecl:		callconv = CALLCONV_CX86;			break;
				case RWORD_StdCall:		callconv = CALLCONV_StdcallX86;		break;
				case RWORD_Inline:		inlinek = INLINEK_AlwaysInline;		break;
				case RWORD_NoInline:	inlinek = INLINEK_NoInline;			break;
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

CSTNode * PStnodParseGenericTypeDecl(CParseContext * pParctx, SLexer * pLex, GRFPDECL grfpdecl)
{
	if (FConsumeToken(pLex, TOK_Generic))
	{
		SLexerLocation lexloc(pLex);
		CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnod->m_tok = TOK_Generic;
		pStnod->m_park = PARK_GenericDecl;

		auto pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
		if (!pStnodIdent)
		{
			ParseError(pParctx, &lexloc, ERRID_MissingName, "Generic type is missing it's name");
			return nullptr;
		}

		auto pSymtabGeneric = pParctx->m_pSymtabGeneric;
		if (EWC_FVERIFY(pSymtabGeneric, "expected symbol table"))
		{
			FSHADOW fshadow = (grfpdecl.FIsSet(FPDECL_AllowShadowing)) ? FSHADOW_ShadowingAllowed : FSHADOW_NoShadowing;
			auto pSym = pSymtabGeneric->PSymEnsure(pParctx->m_pWork->m_pErrman, StrFromIdentifier(pStnodIdent), pStnod, FSYM_IsType | FSYM_VisibleWhenNested, fshadow);
			pStnod->m_pSymbase = pSym;

			CString strIdent = StrFromIdentifier(pStnodIdent).PCoz();
			STypeInfoGeneric * pTingen = EWC_NEW(pSymtabGeneric->m_pAlloc, STypeInfoGeneric) 
											STypeInfoGeneric(strIdent, pSymtabGeneric->m_scopid);
			pSymtabGeneric->AddManagedTin(pTingen);
			pTingen->m_pStnodDefinition = pStnod;
			pStnod->m_pTin = pTingen;
			pSym->m_pTin = pTingen;
		}

		(void) pStnod->IAppendChild(pStnodIdent);
		return pStnod;
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

CSTNode * PStnodParseQualifierDecl(CParseContext * pParctx, SLexer * pLex)
{
	RWORD rword = RwordLookup(pLex);
	if (rword == RWORD_Const || rword == RWORD_InArg)
	{
		SLexerLocation lexloc(pLex);
		TokNext(pLex);	

		CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

		pStnod->m_tok = (TOK)pLex->m_tok;
		pStnod->m_park = PARK_QualifierDecl;

		CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
		pStval->m_str = pLex->m_str;
		pStval->m_rword = rword;
		pStnod->m_pStval = pStval;
		return pStnod;
	}

	return nullptr;
}

CSTNode * PStnodParseTypeSpecifier(CParseContext * pParctx, SLexer * pLex, const char * pCozErrorContext, GRFPDECL grfpdecl)
{
	CSTNode * pStnod = PStnodParseIdentifier(pParctx, pLex);
	if (pStnod)
	{
		if (FConsumeToken(pLex, TOK('(')))
		{
			CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
			SLexerLocation lexloc(pLex);

			CSTNode * pStnodStructInst = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			pStnodStructInst->m_park = PARK_GenericStructSpec;
			pStnodStructInst->IAppendChild(pStnod);
			
			ParseArgumentList(pParctx, pLex, pStnodStructInst, FARGLIST_AllowGenericValues);

			FExpect(pParctx, pLex, TOK(')'));
			return pStnodStructInst;

		}
		else if (pLex->m_tok == TOK('.'))
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
		}
		return pStnod;
	}

	pStnod = PStnodParseProcedureReferenceDecl(pParctx, pLex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseGenericTypeDecl(pParctx, pLex, grfpdecl);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParsePointerDecl(pParctx, pLex);
	if (!pStnod)
	{
		pStnod = PStnodParseQualifierDecl(pParctx, pLex);
	}

	if (!pStnod)
	{
		pStnod = PStnodParseArrayDecl(pParctx, pLex, grfpdecl);
	}
	if (!pStnod)
		return nullptr;

	CSTNode * pStnodChild = PStnodParseTypeSpecifier(pParctx, pLex, pCozErrorContext, grfpdecl);
	if (!pStnodChild)
	{
		ParseError(pParctx, pLex, "Expected type type name before '%s'", PCozFromTok(TOK(pLex->m_tok)));
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

void ValidateDeclaration(CParseContext * pParctx, SLexer * pLex, CSTNode * pStnodDecl, GRFPDECL grfpdecl)
{
	auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);
	if (!EWC_FVERIFY(pStdecl, "bad declaration in ValidateDeclaration"))
		return;

	bool fMissingTypeSpecifier;
	if (pStdecl->m_iStnodChildMin == -1)
	{
		fMissingTypeSpecifier = (pStdecl->m_iStnodType == -1);
		EWC_ASSERT(pStdecl->m_iStnodIdentifier != -1 || grfpdecl.FIsSet(FPDECL_AllowUnnamed), "declaration missing identifier");
	}
	else // compound decl
	{
		if (pStdecl->m_iStnodType != -1)
			ParseError(pParctx, pLex, "Internal error, compound decl should not specify a type");

		fMissingTypeSpecifier = false;
		for (int iStnodChild = pStdecl->m_iStnodChildMin; iStnodChild != pStdecl->m_iStnodChildMax; ++iStnodChild)
		{
			auto pStdeclChild = PStmapDerivedCast<CSTDecl *>(pStnodDecl->PStnodChild(iStnodChild)->m_pStmap);
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

CString StrFauxIdentifierFromTypeSpecifier(CSymbolTable * pSymtab, CSTNode * pStnodType)
{
	// we just need a unique name here - we'll walk the type spec looking for an identifier to use as a reference
	//  and then just fall back to the symbol table's 

	char aCh[128];
	SStringBuffer strbuf(aCh, EWC_DIM(aCh));
	AppendCoz(&strbuf, "0");	// appending an illegal prefix character to avoid collisions with legit symbol names

	auto pStnodIt = pStnodType;
	while (pStnodIt)
	{
		switch (pStnodIt->m_park)
		{
		case PARK_Identifier:
			{
				auto str = StrFromIdentifier(pStnodIt);
				AppendCoz(&strbuf, str.PCoz());
				pStnodIt = nullptr;
			} break;
		default:
			{
				AppendCoz(&strbuf, "unnamed");
				pStnodIt = nullptr;
			} break;
		}
	}

	EnsureTerminated(&strbuf, '\0');
	char aChOut[128];
	GenerateUniqueName(pSymtab->m_pUnset, strbuf.m_pCozBegin, aChOut, sizeof(aChOut));
	return CString(aChOut);
}

CSTNode * PStnodParseUsingStatement(
	CParseContext * pParctx,
	SLexer * pLex,
	CSymbolTable * pSymtab)
{
	RWORD rword = RwordLookup(pLex);
	if (rword != RWORD_Using)
	{
		return nullptr;
	}

	TokNext(pLex);

	SLexerLocation lexloc(pLex);

	SLexer lexPeek = *pLex;
	if (lexPeek.m_tok == TOK_Identifier)
	{
		TokNext(&lexPeek);
	}

	CSTNode * pStnodIdent = nullptr;
	bool fHasExplicitName = lexPeek.m_tok == TOK(':');
	if (fHasExplicitName)
	{
		pStnodIdent = PStnodParseIdentifier(pParctx, pLex);

		if (!pStnodIdent)
		{
			ParseError(pParctx, &lexloc, ERRID_BadUsingSyntax, "Anonymous using statement should not have ':' before type specification");
		}

		FExpect(pParctx, pLex, TOK(':'));
	}

	CSTNode * pStnodDecl = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodDecl->m_park = PARK_Decl;
	auto pStdecl = pStnodDecl->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);
	pStdecl->m_fHasUsingPrefix = true;

	GRFPDECL grfpdecl = FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues;
	auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "declaration", grfpdecl);

	if (!pStnodIdent)	
	{
		auto strFauxIdent = StrFauxIdentifierFromTypeSpecifier(pSymtab, pStnodType);
		pStnodIdent = PStnodAllocateIdentifier(pParctx->m_pAlloc, lexloc, strFauxIdent);
	}

	pStdecl->m_iStnodIdentifier = pStnodDecl->IAppendChild(pStnodIdent);
	pStdecl->m_iStnodType = pStnodDecl->IAppendChild(pStnodType);

	// NOTE: May not resolve symbols (symtab is null if this is a procedure reference)
	if (pSymtab)
	{
		pStnodIdent->m_pSymbase = pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, StrFromIdentifier(pStnodIdent), pStnodDecl);
	}

	ValidateDeclaration(pParctx, pLex, pStnodDecl, grfpdecl);
	return pStnodDecl;
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

	auto pStnodUsing = PStnodParseUsingStatement(pParctx, pLex, pSymtab);
	if (pStnodUsing)
	{
		if (grfpdecl.FIsSet(FPDECL_AllowUsing))
		{
			return pStnodUsing;
		}

		ParseError(pParctx, &lexloc, ERRID_UsingStatementNotAllowed, "Using statement not allowed in this context");
		pParctx->m_pAlloc->EWC_DELETE(pStnodUsing);
		return nullptr;
	}

	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodCompound = nullptr;
	CSTNode * pStnodInit = nullptr;
	bool fAllowCompoundDecl = grfpdecl.FIsSet(FPDECL_AllowCompoundDecl);
	bool fAllowBakedValues = grfpdecl.FIsSet(FPDECL_AllowBakedValues);
	bool fAllowConstants = grfpdecl.FIsSet(FPDECL_AllowConstants);
	bool fIsUnnamed = false;

	SLexer lexPeek = *pLex;
	int cIdent = 0;
	while (1)
	{
		if (fAllowBakedValues)
		{
			(void)FConsumeToken(&lexPeek, TOK_Generic); // ignore baked constant marks
		}

		if (lexPeek.m_tok != TOK_Identifier)
		{
			if (lexPeek.m_tok == TOK(':') && grfpdecl.FIsSet(FPDECL_AllowUnnamed))
			{
				fIsUnnamed = true;
				break;
			}
			return nullptr;
		}

		CString strIdent = lexPeek.m_str;

		++cIdent;
		TokNext(&lexPeek);

		if (fAllowCompoundDecl && FConsumeToken(&lexPeek, TOK(',')))
			continue;

		if (RwordLookup(&lexPeek) == RWORD_Immutable)
			break;

		if ((lexPeek.m_tok == TOK(':')) | (lexPeek.m_tok == TOK_ColonEqual))
			break;

		return nullptr;
	}

	int cTypeNeeded = 0;
	CSTNode * pStnodDecl = nullptr;
	do
	{
		bool fIsBakedConstant = false;
		if (fAllowBakedValues && FConsumeToken(pLex, TOK_Generic))
		{
			fIsBakedConstant = true;
		}

		if (pStnodInit)
			ParseError(pParctx, pLex, "Initializer must come after all comma separated declarations");

		CSTNode * pStnodIdent = nullptr; 
		if (!fIsUnnamed)
		{
			pStnodIdent = PStnodParseIdentifier(pParctx, pLex);
			if (!EWC_FVERIFY(pStnodIdent, "parse failed during decl peek"))
				return nullptr;
		}

		pStnodDecl = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodDecl->m_park = PARK_Decl;

		auto pStdecl = pStnodDecl->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);
		pStdecl->m_fIsBakedConstant = fIsBakedConstant;
		++cTypeNeeded;

		if (pStnodReturn)
		{
			if (!pStnodCompound)
			{
				pStnodCompound = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodCompound->m_park = PARK_Decl;

				auto pStdeclCompound = pStnodCompound->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

				pStdeclCompound->m_iStnodChildMin = pStnodCompound->IAppendChild(pStnodReturn);
				pStnodReturn = pStnodCompound;
			}

			auto pStdeclCompound = PStmapDerivedCast<CSTDecl *>(pStnodCompound->m_pStmap);
			pStdeclCompound->m_iStnodChildMax = pStnodCompound->IAppendChild(pStnodDecl) + 1;
		}
		else
		{
			pStnodReturn = pStnodDecl;
		}

		if (!fIsUnnamed)
		{
			// NOTE: May not resolve symbols (symtab is null if this is a procedure reference)
			if (pSymtab)
			{
				FSHADOW fshadow = (grfpdecl.FIsSet(FPDECL_AllowShadowing)) ? FSHADOW_ShadowingAllowed : FSHADOW_NoShadowing;
				pStnodIdent->m_pSymbase = pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, StrFromIdentifier(pStnodIdent), pStnodDecl, FSYM_None, fshadow);
			}

			pStdecl->m_iStnodIdentifier = pStnodDecl->IAppendChild(pStnodIdent);
		}


		if (RwordLookup(pLex) == RWORD_Immutable)
		{
			TokNext(pLex);

			if (pStnodCompound)
				ParseError(pParctx, pLex, "Comma separated declarations not supported for immutable values");

			if (!fAllowConstants)
			{
				ParseError(pParctx, pLex, "immutable declarations not supported in parameter list");
			}
			else
			{
				pStnodDecl->m_park = PARK_ConstantDecl;
			}
		}

		if (FConsumeToken(pLex, TOK_ColonEqual))
		{
			pStnodInit = PStnodParseExpression(pParctx, pLex);
		}
		else if (FConsumeToken(pLex, TOK(':')))
		{
			if (pStnodCompound)
			{
				EWC_ASSERT(cTypeNeeded, "No compound children?");

				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "declaration", grfpdecl);

				auto pStdeclCompound = PStmapDerivedCast<CSTDecl *>(pStnodCompound->m_pStmap);
				int iStnodChild = pStdeclCompound->m_iStnodChildMax - cTypeNeeded;
				int iChild = 0;
				for ( ; iStnodChild < pStdeclCompound->m_iStnodChildMax; ++iStnodChild)
				{
					auto pStnodChild = pStnodCompound->PStnodChild(iStnodChild);

					auto pStdeclChild = PStmapDerivedCast<CSTDecl *>(pStnodChild->m_pStmap);
					EWC_ASSERT(pStdeclChild->m_iStnodType == -1, "shouldn't set the type child twice");

					auto pStnodTypeCopy = (iChild == 0) ? pStnodType : PStnodCopy(pParctx->m_pAlloc, pStnodType);
					++iChild;

					pStdeclChild->m_iStnodType = pStnodChild->IAppendChild(pStnodTypeCopy);
				}

				cTypeNeeded = 0;
			}
			else
			{
				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "declaration", grfpdecl);
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
		}

	} while (fAllowCompoundDecl && FConsumeToken(pLex, TOK(',')));

	auto pStdeclReturn = PStmapDerivedCast<CSTDecl *>(pStnodReturn->m_pStmap);
	pStdeclReturn->m_iStnodInit = pStnodReturn->IAppendChild(pStnodInit);

	ValidateDeclaration(pParctx, pLex, pStnodReturn, grfpdecl);
	return pStnodReturn;
}

CSTNode * PStnodParseDecl(CParseContext * pParctx, SLexer * pLex)
{
	// stand alone declaration statement

	GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl | FPDECL_AllowConstants | FPDECL_AllowUsing | FPDECL_AllowShadowing;
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

CSTNode * PStnodParseReturnArrow(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtabProc)
{

	if (FConsumeToken(pLex, TOK_Arrow))
	{
		// TODO : handle multiple return types
		ProcSymtabStack procss(pParctx);
		if (pSymtabProc)
		{
			SLexerLocation lexloc(pLex);
			pSymtabProc->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
			procss.Push(pSymtabProc, lexloc);
			pParctx->m_pSymtabGeneric = pSymtabProc;
		}

		auto pStnodRet = PStnodParseTypeSpecifier(pParctx, pLex, "return value", FPDECL_AllowBakedTypes);
		if (pSymtabProc)
		{
			CSymbolTable * pSymtabPop = procss.PSymtabPop();
			EWC_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (list)");
		}

		if (pStnodRet)
		{
			return pStnodRet;
		}

		ParseError(pParctx, pLex, "expected type specification following return arrow.");
	}

	SLexerLocation lexloc(pLex);
	CSTNode * pStnodVoid = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

	pStnodVoid->m_tok = TOK_Identifier;
	pStnodVoid->m_park = PARK_Identifier;

	auto pStident = EWC_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
	pStident->m_str = CString("void");
	pStnodVoid->m_pStident = pStident;

	return pStnodVoid;
}

CSTNode * PStnodParseProcParameterList(CParseContext * pParctx, SLexer * pLex, CSymbolTable * pSymtabProc, bool fIsOpOverload)
{
	SLexerLocation lexloc(pLex);
	ProcSymtabStack procss(pParctx);
	if (pSymtabProc)
	{
		pSymtabProc->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
		procss.Push(pSymtabProc, lexloc);
		pParctx->m_pSymtabGeneric = pSymtabProc;
	}

	GRFPDECL grfpdecl = FPDECL_AllowVariadic | FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues | FPDECL_AllowUsing | FPDECL_AllowUnnamed;
	CSTNode * pStnodParam = PStnodParseParameter(pParctx, pLex, pSymtabProc, grfpdecl);
	CSTNode * pStnodList = nullptr;
	bool fHasVarArgs = pStnodParam && pStnodParam->m_park == PARK_VariadicArg;

	if (pStnodParam)
	{
		pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		pStnodList->m_park = PARK_ParameterList;
		pStnodList->m_pSymtab = pSymtabProc;
		pStnodList->IAppendChild(pStnodParam);

		auto pStdeclParam = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
		bool fNeedsDefaultArg = pStdeclParam && pStdeclParam->m_iStnodInit >= 0;
		while (FConsumeToken(pLex, TOK(',')))
		{
			pStnodParam = PStnodParseParameter(pParctx, pLex, pSymtabProc, grfpdecl);

			if (!pStnodParam)
			{
				auto strUnexpected = StrUnexpectedToken(pLex);
				ParseError(pParctx, pLex, "expected parameter declaration before '%s'", strUnexpected.PCoz());
				break;
			}

			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
			if (pStnodParam->m_park == PARK_Decl && EWC_FVERIFY(pStdecl, "expected parameter to be PARK_Decl"))
			{
				bool fHasDefaultArg = pStdecl->m_iStnodInit >= 0;
				if (fNeedsDefaultArg && !fHasDefaultArg)
				{
					CString strIdent(StrFromIdentifier(pStnodParam->PStnodChildSafe(pStdecl->m_iStnodIdentifier)));
					ParseError(pParctx, &lexloc, ERRID_MissingDefaultArgs, 
						"parameter '%s' must have a default argument because earlier arguments have defaults.", strIdent.PCoz());
				}
				if (fHasDefaultArg && fIsOpOverload)
				{
					CString strIdent(StrFromIdentifier(pStnodParam->PStnodChildSafe(pStdecl->m_iStnodIdentifier)));
					ParseError(pParctx, &lexloc, ERRID_DefaultParamOpOverload, 
						"default values for parameter '%s' is not allowed on an operator overload", strIdent.PCoz());
				}

				fNeedsDefaultArg |= fHasDefaultArg;
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
		CSymbolTable * pSymtabPop = procss.PSymtabPop();
		EWC_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (list)");
	}

	return pStnodList;
}

CSTNode * PStnodSpoofEnumConstant(CParseContext * pParctx, const SLexerLocation & lexloc, const CString & strIdent, PARK park)
{
	CSTNode * pStnodConstant = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodConstant->m_park = park;
	pStnodConstant->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);

	auto pStdecl = pStnodConstant->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

	auto pStnodIdent = PStnodAllocateIdentifier(pParctx->m_pAlloc, lexloc, strIdent);
	pStnodIdent->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);
	pStdecl->m_iStnodIdentifier = pStnodConstant->IAppendChild(pStnodIdent);

	pStnodConstant->m_pSymbase = pParctx->m_pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, strIdent, pStnodConstant, FSYM_VisibleWhenNested);
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

	auto pStdecl = pStnodConstant->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);
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

		EmitError(pErrman, &pStnodConstant->m_lexloc, 
			ERRID_EnumRepeat, "Enum constant name '%s' has already been defined at (%d, %d)", strIdent.PCoz(), iLine, iCol);
	}

	pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodConstant, FSYM_VisibleWhenNested);
	pStnodConstant->m_pSymbase = pSym;

	if (FConsumeToken(pLex, TOK_ColonEqual))
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

CSTNode * PStnodParseEnumConstantList(CParseContext * pParctx, SLexer * pLex, CSTEnum * pStenum)
{
	SLexerLocation lexloc(pLex);
	auto pStnodList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodList->m_tok = TOK('{');
	pStnodList->m_park = PARK_List;

	// add STNodes for implicit members: nil, min, max, etc.

	for (int enumimp = ENUMIMP_Min; enumimp < ENUMIMP_Max; ++enumimp)
	{
		if (!FNeedsImplicitMember((ENUMIMP)enumimp, pStenum->m_enumk))
			continue;

		PARK park = ((enumimp == ENUMIMP_Names) | (enumimp == ENUMIMP_Values)) ? PARK_CompoundLiteral : PARK_EnumConstant;
		auto pStnodImplicit = PStnodSpoofEnumConstant(pParctx, lexloc, PChzFromEnumimp((ENUMIMP)enumimp), park);
		pStenum->m_mpEnumimpIstnod[enumimp] = pStnodList->IAppendChild(pStnodImplicit);
	
		if (park == PARK_EnumConstant)
		{
			++pStenum->m_cConstantImplicit;
		}
	}

	while (1)
	{
		CSTNode * pStnod = PStnodParseEnumConstant(pParctx, pLex);
		if (!pStnod)
			break;

		pStnodList->IAppendChild(pStnod);
		++pStenum->m_cConstantExplicit;

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
	{ "operator|", TOK('|'),			{ PARK_AdditiveOp, PARK_Nil} },
	{ "operator^", TOK('^'),			{ PARK_AdditiveOp, PARK_Nil} },
	{ "operator*", TOK('*'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator/", TOK('/'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator%", TOK('%'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator&", TOK('&'),			{ PARK_MultiplicativeOp, PARK_UnaryOp} },
	{ "operator<<", TOK_ShiftLeft,		{ PARK_ShiftOp, PARK_Nil} },
	{ "operator>>", TOK_ShiftRight,		{ PARK_ShiftOp, PARK_Nil} },
	{ "operator>", TOK('>'),			{ PARK_RelationalOp, PARK_Nil} },
	{ "operator<", TOK('<'),			{ PARK_RelationalOp, PARK_Nil} },
	{ "operator<=", TOK_LessEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator>=", TOK_GreaterEqual,	{ PARK_RelationalOp, PARK_Nil} },
	{ "operator==", TOK_EqualEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator!=", TOK_NotEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator+=", TOK_PlusEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator-=", TOK_MinusEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator*=", TOK_MulEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator/=", TOK_DivEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator=", TOK('='),			{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator:=", TOK_ColonEqual,		{ PARK_Decl, PARK_Nil} },
	{ "operator++", TOK_PlusPlus,		{ PARK_PostfixUnaryOp, PARK_Nil} },
	{ "operator--", TOK_MinusMinus,		{ PARK_PostfixUnaryOp, PARK_Nil} },
	{ "operator@", TOK_Dereference,		{ PARK_UnaryOp, PARK_Nil} },
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
	{ PARK_ShiftOp,				2, 1,	false,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_RelationalOp,		2, 1,	FOVSIG_ReturnBool,	"(Lhs: A, Rhs: B)->bool" },
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

ERRID ErridCheckOverloadSignature(TOK tok, STypeInfoProcedure * pTinproc, SErrorManager * pErrman, SLexerLocation * pLexloc)
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
				if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsCommutative) && !FAllowsCommutative(park))
				{
					EmitError(pErrman, pLexloc, ERRID_UnknownError, 
						"'%s' is not allowed when overloading '%s'", PCozFromRword(RWORD_Commutative), PCozFromTok(tok));
					return ERRID_UnknownError;
				}

				return ERRID_Nil;
			}
		}

		SError error(pErrman, ERRID_BadOverloadSig);
		PrintErrorLine(&error, "Error:", pLexloc, "Incorrect signature for overloading operator '%s'. Options are:", PCozFromTok(tok));

		for (int iPark = 0; iPark < EWC_DIM(pOvinf->m_aPark); ++iPark)
		{
			PARK park = pOvinf->m_aPark[iPark];
			if (park != PARK_Nil)
			{
				PrintErrorLine(&error, "", pLexloc, "\toperator%s%s'", PCozFromTok(tok), PChzOverloadSignature(park));
			}
		}
		return ERRID_BadOverloadSig;
	}

	EmitError(pErrman, pLexloc, ERRID_UnknownError, "no supported overload signature for operator '%s'", PCozFromTok(tok));
	return ERRID_UnknownError;
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

		bool fIsDefinition;
		switch (rword)
		{
		case RWORD_Proc:
		case RWORD_Struct:
		case RWORD_Enum:
		case RWORD_FlagEnum:
		case RWORD_Typedef:
		case RWORD_Operator:
			fIsDefinition = true;
			break;
		default: 
			fIsDefinition = false;
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
					ParseError(pParctx, &lexloc, ERRID_InvalidOpOverload, "Cannot overload operator '%s'", PCozFromTok((TOK)pLex->m_tok));
					pCozOverloadName = "OverloadError";
				}

				CString strIdent(pCozOverloadName);
				pStnodIdent = PStnodAllocateIdentifier(pParctx->m_pAlloc, lexloc, strIdent);
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
				FExpect(pParctx, pLex, TOK('('));

				CSTNode * pStnodProc = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodProc->m_park = PARK_ProcedureDefinition;
				pStnodProc->m_grfstnod.AddFlags(FSTNOD_EntryPoint);

				auto pStproc = pStnodProc->PStmapEnsure<CSTProcedure>(pParctx->m_pAlloc);
				pStproc->m_iStnodProcName = pStnodProc->IAppendChild(pStnodIdent);
				pStproc->m_pStnodParentScope = pParctx->m_pStnodScope;

				const CString & strName = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabProc = PSymtabNew(pParctx->m_pAlloc, pSymtabParent, strName);

				// BB - don't mangle the main function so the linker can find it. yuck.
				if (strName == "main")
				{
					pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName);
				}

				CSTNode * pStnodParams = PStnodParseProcParameterList(pParctx, pLex, pSymtabProc, rword == RWORD_Operator);
				pStproc->m_iStnodParameterList = pStnodProc->IAppendChild(pStnodParams);
				FExpect(pParctx, pLex, TOK(')'));

				auto pStnodReturns = PStnodParseReturnArrow(pParctx, pLex, pSymtabProc);
				pStproc->m_iStnodReturnType = pStnodProc->IAppendChild(pStnodReturns);

				INLINEK inlinek = INLINEK_Nil;
				CALLCONV callconv = CALLCONV_Nil;
				GRFTINPROC grftinproc;
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
								pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName);
								grftinproc.AddFlags(FTINPROC_IsForeign);

								if (!FIsEndOfStatement(pLex) && pLex->m_tok == TOK_Identifier)
								{
									auto pStnodAlias = PStnodParseIdentifier(pParctx, pLex);
									pStproc->m_iStnodForeignAlias = pStnodProc->IAppendChild(pStnodAlias);
								}
							} break;
						case RWORD_CDecl:		
							{
								callconv = CALLCONV_CX86;
								pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName | FSTPROC_PublicLinkage);
							} break;
						case RWORD_StdCall:		callconv = CALLCONV_StdcallX86;		break;
						case RWORD_Inline:		inlinek = INLINEK_AlwaysInline;		break;
						case RWORD_NoInline:	inlinek = INLINEK_NoInline;			break;
						case RWORD_Commutative:
							{
								if (rword == RWORD_Operator)
								{
									grftinproc.AddFlags(FTINPROC_IsCommutative);
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

				if (grftinproc.FIsSet(FTINPROC_IsForeign))
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

				auto pTinproc = PTinprocAlloc(pSymtabParent, cStnodParams, cStnodReturns, strName.PCoz());

				if (rword == RWORD_Operator && FOperatorOverloadMustTakeReference(pStnodIdent->m_tok))
				{
					pTinproc->m_mpIptinGrfparmq[0].AddFlags(FPARMQ_ImplicitRef);
				}


				if (grftinproc.FIsSet(FTINPROC_IsCommutative))
				{
					if (cStnodParams != 2)
					{
						ParseError(pParctx, pLex, "Only operators with two arguments can be commutative ('%s' has %d)", strName.PCoz(), cStnodParams);
						grftinproc.Clear(FTINPROC_IsCommutative);
					}
				}

				pTinproc->m_grftinproc = grftinproc;
				pTinproc->m_pStnodDefinition = pStnodProc;
				pTinproc->m_callconv = callconv;
				pTinproc->m_inlinek = inlinek;

				CheckTinprocGenerics(pParctx, pStnodProc, pTinproc);

				CSTNode ** ppStnodParamMax = &ppStnodParams[cStnodParams];
				for ( ; ppStnodParams != ppStnodParamMax; ++ppStnodParams)
				{
					CSTNode * pStnodParam = *ppStnodParams;
					if (pStnodParam->m_park == PARK_VariadicArg)
					{
						pTinproc->m_grftinproc.AddFlags(FTINPROC_HasVarArgs);
					}
					else if (EWC_FVERIFY_LOC(pStnodParam->m_park == PARK_Decl, pParctx->m_pWork, &pStnodParam->m_lexloc, "Expected decl"))
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
					if (EWC_FVERIFY(pStproc->m_iStnodReturnType != -1, "return type expected. implicit void should be set by here"))
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
				pStnodProc->m_pSymbase = pSymProc;


				return pStnodProc;
			}
			else if (rword == RWORD_Enum || rword == RWORD_FlagEnum)
			{
				SLexerLocation lexloc(pLex);
				CSTNode * pStnodEnum = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodEnum->m_park = PARK_EnumDefinition;

				auto pStenum = pStnodEnum->PStmapEnsure<CSTEnum>(pParctx->m_pAlloc);
				pStenum->m_iStnodIdentifier = pStnodEnum->IAppendChild(pStnodIdent);
				pStenum->m_enumk = (rword == RWORD_FlagEnum) ? ENUMK_FlagEnum : ENUMK_Basic;

				CSTNode * pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "looseType", FPDECL_None);
				pStenum->m_iStnodType = pStnodEnum->IAppendChild(pStnodType);
				
				const CString & strIdent = StrFromIdentifier(pStnodIdent);
				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;
				CSymbolTable * pSymtabEnum = PSymtabNew(pParctx->m_pAlloc, pSymtabParent, strIdent);
				CSTNode * pStnodConstantList = nullptr;

				auto pErrman = pParctx->m_pWork->m_pErrman;
				(void) pSymtabEnum->PSymEnsure(pErrman, "loose", pStnodEnum, FSYM_IsType | FSYM_VisibleWhenNested);
				(void) pSymtabEnum->PSymEnsure(pErrman, "strict", pStnodEnum, FSYM_IsType | FSYM_VisibleWhenNested);

				FExpect(pParctx, pLex, TOK('{'));

				pSymtabEnum->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
				PushSymbolTable(pParctx, pSymtabEnum, lexloc);
				pStnodEnum->m_pSymtab = pSymtabEnum;

				pStnodConstantList = PStnodParseEnumConstantList(pParctx, pLex, pStenum);
				pStenum->m_iStnodConstantList = pStnodEnum->IAppendChild(pStnodConstantList);

				CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
				EWC_ASSERT(pSymtabEnum == pSymtabPop, "CSymbol table push/pop mismatch (enum)");

				if (!FExpect(pParctx, pLex, TOK('}')))
				{
					TOK closeBracket = TOK('}');
					SkipToToken(pLex, &closeBracket, 1, FLEXER_None);
				}

				// type info enum
				int cConstant = pStenum->m_cConstantImplicit + pStenum->m_cConstantExplicit;
				size_t cBAlloc = CBAlign(sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeInfoEnumConstant)) + 
								cConstant * sizeof(STypeInfoEnumConstant);
				u8 * pB = (u8 *)pParctx->m_pAlloc->EWC_ALLOC(cBAlloc, 8);

				STypeInfoEnum * pTinenum = new(pB) STypeInfoEnum(strIdent, pSymtabParent->m_scopid);
				pTinenum->m_enumk = pStenum->m_enumk;

				auto aTinecon = (STypeInfoEnumConstant *)PVAlign( pB + sizeof(STypeInfoEnum), EWC_ALIGN_OF(STypeInfoEnumConstant));
				pTinenum->m_aryTinecon.SetArray(aTinecon, 0, cConstant);

				pTinenum->m_tinstructProduced.m_pStnodStruct = pStnodEnum;
				pSymtabParent->AddManagedTin(pTinenum);

				for (int ipStnod = 0; ipStnod < pStnodConstantList->CStnodChild(); ++ipStnod) 
				{
					CSTNode * pStnodMember = pStnodConstantList->PStnodChild(ipStnod);

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
					case PARK_CompoundLiteral:
						{
						} break;
					default: EWC_ASSERT(false, "Expected enum child value");
					}
				}

				GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
				SSymbol * pSymEnum = pSymtabParent->PSymEnsure(pErrman, strIdent, pStnodEnum, grfsym, FSHADOW_NoShadowing);
				pSymEnum->m_pTin = pTinenum;

				pStnodEnum->m_pSymbase = pSymEnum;
				pStnodEnum->m_pTin = pTinenum;

				return pStnodEnum;
			}
			else if (rword == RWORD_Struct)
			{
				CSTNode * pStnodStruct = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodStruct->m_park = PARK_StructDefinition;
				auto pStstruct = pStnodStruct->PStmapEnsure<CSTStruct>(pParctx->m_pAlloc);
				pStstruct->m_iStnodIdentifier = pStnodStruct->IAppendChild(pStnodIdent);

				CSymbolTable * pSymtabParent = pParctx->m_pSymtab;

				const CString & strIdent = StrFromIdentifier(pStnodIdent);
				SLexerLocation lexlocChild(pLex);
				CSymbolTable * pSymtabStruct = PSymtabNew(pParctx->m_pAlloc, pSymtabParent, strIdent);

				if (FConsumeToken(pLex, TOK('(')))
				{
					CSTNode * pStnodParams = PStnodParseProcParameterList(pParctx, pLex, pSymtabStruct, false);

					pStstruct->m_iStnodParameterList = pStnodStruct->IAppendChild(pStnodParams);
					FExpect(pParctx, pLex, TOK(')'));

					if (!pStnodParams)
					{
						ParseError(pParctx, pLex, "Structure definition has parameter list, but no parameters.");
					}
					else if (EWC_FVERIFY(pStnodParams->m_park == PARK_ParameterList, "expected parameter list"))
					{
						for (int ipStnodParam = 0; ipStnodParam < pStnodParams->CStnodChild(); ++ipStnodParam)
						{
							auto pStnodParam = pStnodParams->PStnodChild(ipStnodParam);
							switch (pStnodParam->m_park)
							{
							case PARK_Decl:
								{
									auto pStdecl = PStmapDerivedCast<CSTDecl*>(pStnodParam->m_pStmap);
									auto strIdent = StrFromIdentifier(pStnodParam->PStnodChildSafe(pStdecl->m_iStnodIdentifier));
									if (!pStdecl->m_fIsBakedConstant && pStdecl->m_iStnodIdentifier >= 0)
									{
										// no need for named instances for values that won't be passed
										ParseError(pParctx, &pStnodParam->m_lexloc, ERRID_NonBakedStructParameter,
											"Structure argument '%s' is neither be baked value or unnamed type argument", 
											strIdent.PCoz());
									}
								} break;
							default: 
								ParseError(pParctx, pLex, "unexpected generic struct parameter kind (%s)", PChzFromPark(pStnodParam->m_park));
							}
						}

					}
				}

				FExpect(pParctx, pLex, TOK('{'));

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
					pStstruct->m_iStnodDeclList = pStnodStruct->IAppendChild(pStnodDeclList);
				}
				else
				{
					ParseError(pParctx, pLex, ERRID_EmptyStruct, "structure '%s' has no members - zero byte structures are not allowed", strIdent.PCoz());
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
							auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodMemberIt->m_pStmap);
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

				CSTNode * pStnodParameterList = pStnodStruct->PStnodChildSafe(pStstruct->m_iStnodParameterList);
				size_t cpStnodParam = (pStnodParameterList) ? pStnodParameterList->CStnodChild() : 0;
				auto pSymtab = pParctx->m_pSymtab;

				auto pTinstruct = PTinstructAlloc(pSymtab, strIdent, cStnodField, cpStnodParam);
				pTinstruct->m_pStnodStruct = pStnodStruct;

				for ( ; ppStnodMember != ppStnodMemberMax; ++ppStnodMember)
				{
					CSTNode * pStnodMember = *ppStnodMember;
					if (pStnodMember->m_park != PARK_Decl)
						continue;

					auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodMember->m_pStmap);
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
						auto pStdeclChild = PStmapDerivedCast<CSTDecl *>(pStnodChild->m_pStmap);
						auto pStnodMemberIdent = pStnodChild->PStnodChildSafe(pStdeclChild->m_iStnodIdentifier);
						pTypememb->m_strName = StrFromIdentifier(pStnodMemberIdent);
					}
				}

				auto pErrman = pParctx->m_pWork->m_pErrman;
				GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
				SSymbol * pSymStruct = pSymtabParent->PSymEnsure(pErrman, strIdent, pStnodStruct, grfsym, FSHADOW_NoShadowing);
				pStnodStruct->m_pSymbase = pSymStruct;
				pSymStruct->m_pTin = pTinstruct;
				pStnodStruct->m_pTin = pTinstruct;

				CheckTinstructGenerics(pParctx, pStnodStruct, pTinstruct);

				FExpect(pParctx, pLex, TOK('}'));

				return pStnodStruct;
			}
			else if (rword == RWORD_Typedef)
			{
				CString strIdent = StrFromIdentifier(pStnodIdent);

				// create a symbol table for any generic symbols that might be created by this typedef
				CSymbolTable * pSymtabTypedef = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, strIdent);
				pSymtabTypedef->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
				PushSymbolTable(pParctx, pSymtabTypedef, lexloc);

				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "typedef", FPDECL_None);
				ExpectEndOfStatement(pParctx, pLex);

				CSymbolTable * pSymtabPop = PSymtabPop(pParctx);
				EWC_ASSERT(pSymtabTypedef == pSymtabPop, "CSymbol table push/pop mismatch (struct)");
				
				if (!pStnodType)
				{
					ParseError(pParctx, pLex, "missing type value for typedef %s", strIdent.PCoz());

					pParctx->m_pAlloc->EWC_DELETE(pStnodIdent);
					return nullptr;
				}

				CSTNode * pStnodTypedef = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodTypedef->m_park = PARK_Typedef;
				pStnodTypedef->m_pSymtab = pSymtabTypedef;

				(void) pStnodTypedef->IAppendChild(pStnodIdent);
				(void) pStnodTypedef->IAppendChild(pStnodType);

				CSymbolTable * pSymtab = pParctx->m_pSymtab;
				auto pErrman = pParctx->m_pWork->m_pErrman;

				GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
				auto pSym = pSymtab->PSymEnsure(pErrman, strIdent, pStnodTypedef, grfsym, FSHADOW_NoShadowing);
				pStnodTypedef->m_pSymbase = pSym;


				return pStnodTypedef;
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
			pSymtab = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, "anon");
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
		FExpect(pParctx, pLex, TOK('}'));
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
	pStnodList->m_pSymtab = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, "case");
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

				FExpect(pParctx, pLex, TOK(':'), "Following 'case' statement");
				CreateSwitchList(pParctx, pLex, &pStnodList);

				pStnodCase->IAppendChild(pStnodList);

			} break;
			case RWORD_Else:
			{
				CSTNode * pStnodDefault = PStnodParseReservedWord(pParctx, pLex, RWORD_Else);
				pStnodSwitch->IAppendChild(pStnodDefault);

				FExpect(pParctx, pLex, TOK(':'), "Following switch 'else' statement");
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

	FExpect(pParctx, pLex, TOK('}'));

	for (int ipStnodCase = 1; ipStnodCase < pStnodSwitch->CStnodChild(); ++ipStnodCase)
	{
		auto pStnodCase = pStnodSwitch->PStnodChild(ipStnodCase);
		if (!EWC_FVERIFY(pStnodCase, "case should have value, list children"))
			continue;

		int ipStnodList = pStnodCase->CStnodChild() - 1; 
		auto _pStnodList = pStnodCase->PStnodChildSafe(ipStnodList);
		if (!EWC_FVERIFY(_pStnodList && _pStnodList->m_park == PARK_List, "Case without list child"))
			continue;

		if (_pStnodList->CStnodChild() == 0)
		{
			ParseError(pParctx, &pStnodCase->m_lexloc, ERRID_EmptyCase, 
				"empty switch case must contain at least one statement. Multiple case values are comma separated");
		}
	}

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

		SLexerLocation lexloc(pLex);
		if (pLex->m_tok == TOK('('))
		{
			EmitError(pParctx->m_pWork, &lexloc, ERRID_OldCStyle, "Parens are not needed for C-Style for loop");
			TokNext(pLex);
		}

		auto pStfor = pStnodFor->PStmapEnsure<CSTFor>(pParctx->m_pAlloc);

		if (ppStidentLabel)
		{
			pStnodFor->m_pStident = *ppStidentLabel;
			*ppStidentLabel = nullptr;
		}

		CSymbolTable * pSymtabLoop = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, "for");
		pStnodFor->m_pSymtab = pSymtabLoop;

		PushSymbolTable(pParctx, pSymtabLoop, lexloc);

		GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl;
		auto * pStnodDecl =  PStnodParseParameter(pParctx, pLex, pParctx->m_pSymtab, grfpdecl);
		if (pStnodDecl)
			ExpectEndOfStatement(pParctx, pLex);
		else
		{
			pStnodDecl = PStnodParseExpression(pParctx, pLex);
			FExpect(pParctx, pLex, TOK(';'));
		}
		pStfor->m_iStnodDecl = pStnodFor->IAppendChild(pStnodDecl);

		CSTNode * pStnodPred = PStnodParseExpression(pParctx, pLex);
		if (pStnodPred)
			ExpectEndOfStatement(pParctx, pLex);
		else
			FExpect(pParctx, pLex, TOK(';'));
		pStfor->m_iStnodPredicate = pStnodFor->IAppendChild(pStnodPred);

		CSTNode * pStnodIncrement = PStnodParseExpression(pParctx, pLex);
		if (pStnodIncrement)
			ExpectEndOfStatement(pParctx, pLex);
		else
			FExpect(pParctx, pLex, TOK(';'));
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
		
		auto pStfor = pStnodFor->PStmapEnsure<CSTFor>(pParctx->m_pAlloc);

		if (ppStidentLabel)
		{
			pStnodFor->m_pStident = *ppStidentLabel;
			*ppStidentLabel = nullptr;
		}

		SLexerLocation lexloc(pLex);
		CSymbolTable * pSymtabLoop = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, "for");
		pStnodFor->m_pSymtab = pSymtabLoop;

		PushSymbolTable(pParctx, pSymtabLoop, lexloc);
		CSTNode * pStnodDecl = PStnodParseParameter(pParctx, pLex, pSymtabLoop, FPDECL_None);
		CSTNode * pStnodIterator = nullptr;
		if (pStnodDecl)
		{
			pStfor->m_iStnodDecl = pStnodFor->IAppendChild(pStnodDecl);

			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);
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
				CSTNode * pStnodPredIdent = PStnodAllocateIdentifier(pParctx->m_pAlloc, lexloc, "iterIsDone");

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
				CSTNode * pStnodIncIdent = PStnodAllocateIdentifier(pParctx->m_pAlloc, lexloc, "iterNext");

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

	if (FConsumeToken(pLex, TOK_Label))
	{
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

		if ((rword == RWORD_ImportDirective) | (rword == RWORD_ForeignLibraryDirective) |
			(rword == RWORD_StaticLibraryDirective) | (rword == RWORD_DynamicLibraryDirective))
		{
			TokNext(pLex);
			if (pLex->m_tok == TOK_Literal && pLex->m_litk == LITK_String)
			{
				switch(rword)
				{
				case RWORD_ImportDirective:
					{
						char aChFilename[CWorkspace::s_cBFilenameMax];
						(void)CChConstructFilename(pLex->m_str.PCoz(), CWorkspace::s_pCozSourceExtension, aChFilename, EWC_DIM(aChFilename));
						(void)pWork->PFileEnsure(aChFilename, CWorkspace::FILEK_Source);
					} break;
				case RWORD_ForeignLibraryDirective:
					(void) pWork->PFileEnsure(pLex->m_str.PCoz(), CWorkspace::FILEK_Library);
					break;
				case RWORD_StaticLibraryDirective:
					(void) pWork->PFileEnsure(pLex->m_str.PCoz(), CWorkspace::FILEK_StaticLibrary);
					break;
				case RWORD_DynamicLibraryDirective:
					(void) pWork->PFileEnsure(pLex->m_str.PCoz(), CWorkspace::FILEK_DynamicLibrary);
					break;
				default:
					EWC_ASSERT(false, "unknown import directive");		
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

void ParseGlobalScope(CWorkspace * pWork, SLexer * pLex, GRFUNT grfunt)
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
			ParseError( pParctx, pLex, ERRID_UnexpectedToken, "Unexpected token at global scope '%s'", PCozCurrentToken(pLex));
			break;
		}

		pWork->AppendEntry(pStnod, pParctx->m_pSymtab);
		if (!grfunt.FIsSet(FUNT_ImplicitProc))
		{
			bool fIsLegalGlobal =	(pStnod->m_park == PARK_Decl) | 
									(pStnod->m_park == PARK_ConstantDecl) |
									(pStnod->m_park == PARK_Typedef) |
									(pStnod->m_park == PARK_ProcedureDefinition) | 
									(pStnod->m_park == PARK_EnumDefinition) | 
									(pStnod->m_park == PARK_StructDefinition) | 
									(pStnod->m_park == PARK_Nop);
			if (!fIsLegalGlobal)
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

SLexerLocation LexlocFromSym(SSymbol * pSym)
{
	if (!pSym || !pSym->m_pStnodDefinition)
		return SLexerLocation();
	return pSym->m_pStnodDefinition->m_lexloc;
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
				else
				{
					break;
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

CString CSymbolTable::s_strVoid;
CString CSymbolTable::s_strChar;
CString CSymbolTable::s_strBool;
CString CSymbolTable::s_strInt;
CString CSymbolTable::s_strUint;
CString CSymbolTable::s_strSsize;
CString CSymbolTable::s_strUsize;
CString CSymbolTable::s_strFloat;
CString CSymbolTable::s_strDouble;
CString CSymbolTable::s_strType;
CString CSymbolTable::s_strS8;
CString CSymbolTable::s_strS16;
CString CSymbolTable::s_strS32;
CString CSymbolTable::s_strS64;
CString CSymbolTable::s_strU8;
CString CSymbolTable::s_strU16;
CString CSymbolTable::s_strU32;
CString CSymbolTable::s_strU64;
CString CSymbolTable::s_strF32;
CString CSymbolTable::s_strF64;

CSymbolTable::SUsing::~SUsing()
{
	auto pAlloc = m_hashHvPSymp.PAlloc();
	CHash<HV, SSymbolPath *>::CIterator iterPSymp(&m_hashHvPSymp);
	while (SSymbolPath ** ppSymp = iterPSymp.Next())
	{
		pAlloc->EWC_DELETE(*ppSymp);
	}
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

	for (SSymbol ** ppSym = m_arypSymGenerics.A(); ppSym != m_arypSymGenerics.PMac(); ++ppSym)
	{
		m_pAlloc->EWC_DELETE(*ppSym);
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

void AddSimpleBuiltInType(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strName, TINK tink, GRFSYM grfsym = FSYM_None)
{
	STypeInfo * pTin = EWC_NEW(pSymtab->m_pAlloc, STypeInfo) STypeInfo(
																strName,
																pSymtab->m_scopid,
																tink);

	pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTin, grfsym);
}

void AddBuiltInInteger(CWorkspace * pWork, CSymbolTable * pSymtab, const CString & strName, u32 cBit, bool fSigned)
{
	STypeInfoInteger * pTinint = EWC_NEW(pSymtab->m_pAlloc, STypeInfoInteger) STypeInfoInteger(
																				strName,
																				pSymtab->m_scopid,
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
										pSymtab->m_scopid,
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
											pSymtab->m_scopid,
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
																				pSymtab->m_scopid,
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

void CSymbolTable::StaticStringInit()
{
	s_strVoid = CString("void");
	s_strChar = CString("char");
	s_strBool = CString("bool");
	s_strInt = CString("int");
	s_strUint = CString("uint");
	s_strSsize = CString("sSize");
	s_strUsize = CString("uSize");
	s_strFloat = CString("float");
	s_strDouble = CString("double");
	s_strType = CString("type");
	s_strS8 = CString("s8");
	s_strS16 = CString("s16");
	s_strS32 = CString("s32");
	s_strS64 = CString("s64");
	s_strU8 = CString("u8");
	s_strU16 = CString("u16");
	s_strU32 = CString("u32");
	s_strU64 = CString("u64");
	s_strF32 = CString("f32");
	s_strF64 = CString("f64");
}
void CSymbolTable::StaticStringShutdown()
{
	s_strVoid = CString();
	s_strChar = CString();
	s_strBool = CString();
	s_strInt = CString();
	s_strUint = CString();
	s_strSsize = CString();
	s_strUsize = CString();
	s_strFloat = CString();
	s_strDouble = CString();
	s_strType = CString();
	s_strS8 = CString();
	s_strS16 = CString();
	s_strS32 = CString();
	s_strS64 = CString();
	s_strU8 = CString();
	s_strU16 = CString();
	s_strU32 = CString();
	s_strU64 = CString();
	s_strF32 = CString();
	s_strF64 = CString();
}

void CSymbolTable::AddBuiltInSymbols(CWorkspace * pWork)
{

	AddSimpleBuiltInType(pWork, this, s_strBool.PCoz(), TINK_Bool);
	AddSimpleBuiltInType(pWork, this, "_flag", TINK_Flag, FSYM_InternalUseOnly);
	AddSimpleBuiltInType(pWork, this, "void", TINK_Void);
	AddSimpleBuiltInType(pWork, this, s_strType, TINK_Type);

	AddBuiltInInteger(pWork, this, s_strU8.PCoz(), 8, false);
	AddBuiltInInteger(pWork, this, s_strU16.PCoz(), 16, false);
	AddBuiltInInteger(pWork, this, s_strU32.PCoz(), 32, false);
	AddBuiltInInteger(pWork, this, "char", 32, false);
	AddBuiltInInteger(pWork, this, s_strU64, 64, false);

	AddBuiltInInteger(pWork, this, s_strS8.PCoz(), 8, true);
	AddBuiltInInteger(pWork, this, s_strS16.PCoz(), 16, true);
	AddBuiltInInteger(pWork, this, s_strS32.PCoz(), 32, true);
	AddBuiltInInteger(pWork, this, s_strS64.PCoz(), 64, true);

#if EWC_X64
	AddBuiltInAlias(pWork, this, s_strInt.PCoz(), "s64");
	AddBuiltInAlias(pWork, this, s_strUint.PCoz(), "u64");
	AddBuiltInAlias(pWork, this, s_strSsize.PCoz(), "s64");
	AddBuiltInAlias(pWork, this, s_strUsize.PCoz(), "u64");
#else
	AddBuiltInAlias(pWork, this, s_strInt.PCoz(), "s32");
	AddBuiltInAlias(pWork, this, s_strUint.PCoz(), "u32");
	AddBuiltInAlias(pWork, this, s_strSsize.PCoz(), "s32");
	AddBuiltInAlias(pWork, this, s_strUsize.PCoz(), "u32");
#endif

	AddBuiltInFloat(pWork, this, s_strF32.PCoz(), 32);
	AddBuiltInFloat(pWork, this, s_strF64.PCoz(), 64);
	AddBuiltInAlias(pWork, this, s_strFloat.PCoz(), "f32");
	AddBuiltInAlias(pWork, this, s_strDouble.PCoz(), "f64");

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

SSymbol * CSymbolTable::PSymGenericInstantiate(SSymbol * pSymGeneric, STypeInfo * pTinInstance)
{
	auto pSymNew = PSymNewUnmanaged(pSymGeneric->m_strName, pSymGeneric->m_pStnodDefinition, pSymGeneric->m_grfsym);
	m_arypSymGenerics.Append(pSymNew);

	pSymNew->m_pTin = pTinInstance;

	pSymNew->m_aryPSymReferencedBy.SetAlloc(m_pAlloc, BK_Dependency, 4);
	pSymNew->m_aryPSymHasRefTo.SetAlloc(m_pAlloc, BK_Dependency, 4);
	return pSymNew;
}

enum SYMCOLLIS 
{
	SYMCOLLIS_SymbolName,
	SYMCOLLIS_CyclicUsing,

	EWC_MAX_MIN_NIL(SYMCOLLIS)
};

enum SYMCCK		// SYMbol Collision Check Kind
{
	SYMCCK_Entry,
	SYMCCK_UsedBy,	// walking symbol tables that use the entry table
	SYMCCK_Uses,	// walking symtabs that the entry table uses
};

SYMCOLLIS SymcollisCheck(
	CSymbolTable * pSymtab,
	const HV * pHvMin,
	const HV * pHvMax,
	u64 nVisitId,
	FSHADOW fshadow,
	CSTNode ** ppStnodCollision, 
	SYMCCK symcck = SYMCCK_Entry,
	CSymbolTable * pSymtabUser = nullptr)
{
	if (pSymtab->m_nVisitId == nVisitId)
	{
		*ppStnodCollision = nullptr;
		return SYMCOLLIS_CyclicUsing;
	}
	pSymtab->m_nVisitId = nVisitId;

	SYMCOLLIS symcollis = SYMCOLLIS_Nil;
	if (fshadow == FSHADOW_NoShadowing)
	{
		for (auto pHvIt = pHvMin; pHvIt != pHvMax; ++pHvIt)
		{
			SSymbol ** ppSym = pSymtab->m_hashHvPSym.Lookup(*pHvIt);
			if (ppSym)
			{
				*ppStnodCollision = (*ppSym)->m_pStnodDefinition;
				symcollis = SYMCOLLIS_SymbolName;
				break;
			}
		}
	}

	// We need to check all of the symbol tables 'derived' from the one that started our recursive check, but not
	//   tables derived from the symtabs we are using. Also we need to make sure we don't recurse back up the symtab path 
	//   that brought us here.
	if (symcck != SYMCCK_Uses )
	{
		auto ppSymtabMax = pSymtab->m_arypSymtabUsedBy.PMac();
		for (auto ppSymtabIt = pSymtab->m_arypSymtabUsedBy.A(); ppSymtabIt != ppSymtabMax; ++ppSymtabIt)
		{
			auto pSymtabUsedBy = *ppSymtabIt;
			symcollis = ewcMax(symcollis, SymcollisCheck(pSymtabUsedBy, pHvMin, pHvMax, nVisitId, FSHADOW_NoShadowing, ppStnodCollision, SYMCCK_UsedBy, pSymtab));
			if (symcollis == SYMCOLLIS_CyclicUsing)
				return symcollis;
		}
	}

	auto pUsingMax = pSymtab->m_aryUsing.PMac();
	for (auto pUsingIt = pSymtab->m_aryUsing.A(); pUsingIt != pUsingMax; ++pUsingIt)
	{
		if (pUsingIt->m_pSymtab == pSymtabUser)
			continue;

		symcollis = ewcMax(symcollis, SymcollisCheck(pUsingIt->m_pSymtab, pHvMin, pHvMax, nVisitId, FSHADOW_NoShadowing, ppStnodCollision, SYMCCK_Uses));
		if (symcollis == SYMCOLLIS_CyclicUsing)
			return symcollis;
	}

	return symcollis;
}

ERRID ErridCheckSymbolCollision(
	SErrorManager * pErrman,
	SLexerLocation * pLexloc,
	const char * pChzContext,
	CSymbolTable * pSymtabContext,
	const HV * pHvMin,
	const HV * pHvMax,
	FSHADOW fshadow,
	u64 nVisitId)
{
	CSTNode * pStnodCollision = nullptr;
	auto symcollis = SymcollisCheck(pSymtabContext, pHvMin, pHvMax, nVisitId, fshadow, &pStnodCollision);
	if (symcollis != SYMCOLLIS_Nil)
	{
		s32 iLine = 0;
		s32 iCol = 0;
		const char * pChzFilename = "unknown";
		const char * pChzSymName = "";
		if (pStnodCollision)
		{
			CalculateLinePosition(pErrman->m_pWork, &pStnodCollision->m_lexloc, &iLine, &iCol);
			pChzFilename = pStnodCollision->m_lexloc.m_strFilename.PCoz();

			if (pStnodCollision->m_pSymbase && pStnodCollision->m_pSymbase->m_symk == SYMK_Symbol)
			{
				pChzSymName = ((SSymbol *)pStnodCollision->m_pSymbase)->m_strName.PCoz();
			}
		}

		switch (symcollis)
		{
		case SYMCOLLIS_SymbolName: EmitError(pErrman, pLexloc, ERRID_UsingStatementCollision, 
										"%s shadows symbol name '%s' at %s(%d, %d)", 
										pChzContext,
										pChzSymName,
										pChzFilename, iLine, iCol);
									return ERRID_UsingStatementCollision;
		case SYMCOLLIS_CyclicUsing: EmitError(pErrman, pLexloc, ERRID_UsingStatementCycle, 
										"%s causes using statement cycle at %s(%d, %d)", 
										pChzContext,
										pChzFilename, iLine, iCol);
									return ERRID_UsingStatementCycle;
		}
	}
	return ERRID_Nil;
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

	HV hv = strName.Hv();
	(void) ErridCheckSymbolCollision(
		pErrman, 
		&lexloc,
		strName.PCoz(),
		this,
		&hv, &hv + 1,
		fshadow,
		++g_nSymtabVisitId);

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

			 if (fshadow != FSHADOW_ShadowingAllowed)
			 {
				s32 iLine = 0;
				s32 iCol = 0;
				const char * pChzFilename = "unknown";
				if (pSymPrev->m_pStnodDefinition)
				{
					CalculateLinePosition(pErrman->m_pWork, &pSymPrev->m_pStnodDefinition->m_lexloc, &iLine, &iCol);
					pChzFilename = pSymPrev->m_pStnodDefinition->m_lexloc.m_strFilename.PCoz();

				}

				EmitError(pErrman, &lexloc, ERRID_ShadowedDefine, "%s symbol shadows previous type definition at %s(%d, %d)", 
					strName.PCoz(),
					pChzFilename,
					iLine,
					iCol);
			 }
		}
	}
	
	if (!pSym)
	{
		pSym = PSymNewUnmanaged(strName, pStnodDefinition, grfsym);
		(void) m_hashHvPSym.FinsEnsureKeyAndValue(strName.Hv(), pSym);
	}

	pSym->m_pSymPrev = pSymPrev;
	return pSym;
}

bool FAddSymbolInUsing(SSymbol * pSym)
{
	// don't add the symbol for generics as they will already be added to the chlld namespace and cause a collision

	if (pSym->m_pStnodDefinition && pSym->m_pStnodDefinition->m_park == PARK_GenericDecl)
		return false;
	return true;
}

void FlattenUsingTree(CSymbolTable * pSymtab, CDynAry<HV> * paryHv, u64 nVisitId)
{
	if (pSymtab->m_nVisitId == nVisitId)
		return;
	pSymtab->m_nVisitId = nVisitId;

	paryHv->EnsureSize(paryHv->C() + pSymtab->m_hashHvPSym.C());
	EWC::CHash<HV, SSymbol *>::CIterator iter(&pSymtab->m_hashHvPSym);
	HV * pHv;
	while (SSymbol ** ppSym = iter.Next(&pHv))
	{
		if (!FAddSymbolInUsing(*ppSym))
			continue;

		paryHv->Append(*pHv);
	}

	auto pUsingMax = pSymtab->m_aryUsing.PMac();
	for (auto pUsingIt = pSymtab->m_aryUsing.A(); pUsingIt != pUsingMax; ++pUsingIt)
	{
		FlattenUsingTree(pUsingIt->m_pSymtab, paryHv, nVisitId);
	}
}

void CSymbolTable::AddUsingScope(SErrorManager * pErrman, CSymbolTable * pSymtabNew, CSTNode * pStnodUsingDecl)
{
	CDynAry<HV> aryHvFlat(m_pAlloc, BK_Symbol);
	CDynAry<SSymbol *> aryPSymFlat(m_pAlloc, BK_Symbol);
	FlattenUsingTree(pSymtabNew, &aryHvFlat, ++g_nSymtabVisitId);

	pSymtabNew->m_nVisitId = g_nSymtabVisitId;

	auto errid = ErridCheckSymbolCollision(
					pErrman, 
					&pStnodUsingDecl->m_lexloc,
					"using declaration",
					this,
					aryHvFlat.A(), aryHvFlat.PMac(),
					FSHADOW_NoShadowing,
					g_nSymtabVisitId);
	if (errid != ERRID_Nil)
		return;

	auto pUsingNew = m_aryUsing.AppendNew();
	pUsingNew->m_pSymtab = pSymtabNew;
	pUsingNew->m_pStnod = pStnodUsingDecl;
	pUsingNew->m_hashHvPSymp.SetAlloc(m_pAlloc, BK_Symbol);
	EWC_ASSERT(pStnodUsingDecl->PSym(), "expected symbol for using decl");

	pSymtabNew->m_arypSymtabUsedBy.Append(this);
}

SSymbol * CSymbolTable::PSymNewUnmanaged(const CString & strName, CSTNode * pStnodDefinition, GRFSYM grfsym)
{
	auto pSym = EWC_NEW(m_pAlloc, SSymbol) SSymbol;
	pSym->m_symk = SYMK_Symbol;
	pSym->m_symdep = SYMDEP_Nil;

	pSym->m_aryPSymReferencedBy.SetAlloc(m_pAlloc, BK_Dependency, 4);
	pSym->m_aryPSymHasRefTo.SetAlloc(m_pAlloc, BK_Dependency, 4);

	pSym->m_strName = strName;
	pSym->m_pStnodDefinition = pStnodDefinition;
	pSym->m_grfsym = grfsym;
	pSym->m_pTin = nullptr;
	pSym->m_pSymPrev = nullptr;
	return pSym;
}

static SSymbolPath * PSympLookup(
	CSymbolTable::SUsing * pUsingSource,
	const CString & str,
	const SLexerLocation & lexloc,
	GRFSYMLOOK grfsymlook,
	int cpSymPath = 0)
{
	auto pSymtab = pUsingSource->m_pSymtab;
	auto pSym = pSymtab->PSymLookup(str, lexloc, grfsymlook);
	if (pSym)
	{
		auto pSymp = EWC_NEW(pSymtab->m_pAlloc, SSymbolPath) SSymbolPath;
		pSymp->m_symk = SYMK_Path;
		pSymp->m_arypSym.SetAlloc(pSymtab->m_pAlloc, BK_Symbol);
		pSymp->m_arypSym.AppendFill(cpSymPath+1, nullptr);
		pSymp->m_arypSym[cpSymPath] = pSym;
		pSymp->m_arypSym[cpSymPath-1] = pUsingSource->m_pStnod->PSym();

		return pSymp;
	}

	for (auto pUsingIt = pSymtab->m_aryUsing.A(); pUsingIt != pSymtab->m_aryUsing.PMac(); ++pUsingIt)
	{
		auto pSymp = PSympLookup(pUsingIt, str, lexloc, grfsymlook, cpSymPath + 1);
		if (pSymp)
		{
			pSymp->m_arypSym[cpSymPath-1] = pUsingSource->m_pStnod->PSym();
			return pSymp;
		}
	}
	return nullptr;
}

SSymbolBase * PSymbaseLookup(
	CSymbolTable * pSymtab,
	const CString & str,
	const SLexerLocation & lexloc,
	GRFSYMLOOK grfsymlook)
{
	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		auto pSym = pSymtab->PSymLookup(str, lexloc, grfsymlook);
		if (pSym)
			return pSym;

		for (auto pUsing = pSymtab->m_aryUsing.A(); pUsing != pSymtab->m_aryUsing.PMac(); ++pUsing)
		{
			auto ppSymp = pUsing->m_hashHvPSymp.Lookup(str.Hv());
			if (ppSymp)
			{
				return *ppSymp;
			}
		}

		for (auto pUsing = pSymtab->m_aryUsing.A(); pUsing != pSymtab->m_aryUsing.PMac(); ++pUsing)
		{
			auto pSymp = PSympLookup(pUsing, str, lexloc, grfsymlook | FSYMLOOK_IgnoreOrder, 1);
			if (pSymp)
			{
				pUsing->m_hashHvPSymp.FinsEnsureKeyAndValue(str.Hv(), pSymp);
				return pSymp;
			}
		}
	}

	CSymbolTable * pSymtabIt = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? pSymtab->m_pSymtabParent : nullptr;
	SLexerLocation lexlocChild = lexloc;
	while (pSymtabIt)
	{
		auto pSym = pSymtabIt->PSymLookup(str, lexloc, grfsymlook);
		if (pSym)
			return pSym;

		for (auto pUsing = pSymtabIt->m_aryUsing.A(); pUsing != pSymtabIt->m_aryUsing.PMac(); ++pUsing)
		{
			auto ppSymp = pUsing->m_hashHvPSymp.Lookup(str.Hv());
			if (ppSymp)
			{
				return *ppSymp;
			}
		}

		for (auto pUsing = pSymtabIt->m_aryUsing.A(); pUsing != pSymtabIt->m_aryUsing.PMac(); ++pUsing)
		{
			auto pSymp = PSympLookup(pUsing, str, lexloc, grfsymlook | FSYMLOOK_IgnoreOrder, 1);
			if (pSymp)
			{
				pUsing->m_hashHvPSymp.FinsEnsureKeyAndValue(str.Hv(), pSymp);
				return pSymp;
			}
		}
		
		pSymtabIt = pSymtabIt->m_pSymtabParent;
	}

	return nullptr;
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

STypeInfoQualifier * CSymbolTable::PTinqualEnsure(STypeInfo * pTinTarget, GRFQUALK grfqualk)
{
	// Note: I should unique'ify these
	if (pTinTarget && pTinTarget->m_tink == TINK_Qualifier)
	{
		auto pTinqualPrev = (STypeInfoQualifier *)pTinTarget;

		if (pTinqualPrev->m_grfqualk.FIsSet(grfqualk))
			return pTinqualPrev;
		grfqualk |= pTinqualPrev->m_grfqualk;
		pTinTarget = pTinqualPrev->m_pTin;
	}

	STypeInfoQualifier * pTinqual = EWC_NEW(m_pAlloc, STypeInfoQualifier) STypeInfoQualifier(grfqualk);
	pTinqual->m_pTin = pTinTarget;

	AddManagedTin(pTinqual);
	return pTinqual;
}

STypeInfoQualifier * CSymbolTable::PTinqualWrap(STypeInfo * pTinTarget, GRFQUALK grfqualk)
{
	if (!pTinTarget)	
		return nullptr;

	return PTinqualEnsure(pTinTarget, grfqualk);
}

STypeInfoPointer * CSymbolTable::PTinptrAllocate(STypeInfo * pTinPointedTo)
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
	if (!pTin->m_strDesc.FIsEmpty())
	{
		pSeb->AppendCoz(pTin->m_strDesc.PCoz());
		return;
	}

	auto cBPrefix = pSeb->CB();
	if (cBPrefix > 0)
		--cBPrefix;

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
			case ARYK_Reference:	pSeb->AppendCoz("[]");		break;
			case ARYK_Dynamic:		pSeb->AppendCoz("[..]");	break;
			default: 
				EWC_ASSERT(false, "Unhandled ARYK");
				break;
			}
			AppendTypeDescriptor(pTinary->m_pTin, pSeb);
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;

			char aCh[64];
			SStringBuffer strbuf(aCh, EWC_DIM(aCh));
			AppendFlagNames(&strbuf, pTinqual->m_grfqualk, " ");
			pSeb->AppendCoz(aCh);
			pSeb->AppendCoz(" ");
			AppendTypeDescriptor(pTinqual->m_pTin, pSeb);
		}break;
	case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;

			pSeb->AppendCoz("&");
			AppendTypeDescriptor(pTinptr->m_pTinPointedTo, pSeb);
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;
			char aChScopid[32];
			SStringBuffer strbuf(aChScopid, EWC_DIM(aChScopid));
			FormatCoz(&strbuf, "%d)", pTin->m_scopid);
			pSeb->AppendCoz(aChScopid);

			pSeb->AppendCoz(pTinproc->m_strName.PCoz());
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

			if (pTinproc->FHasVarArgs())
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

			if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				pSeb->AppendCoz("#foreign");
			}

			if (pTinproc->m_inlinek != INLINEK_Nil)
			{
				pSeb->AppendCoz(PChzFromInlinek(pTinproc->m_inlinek));
			}

			if (pTinproc->m_callconv != CALLCONV_Nil)
			{
				pSeb->AppendCoz(PChzFromCallconv(pTinproc->m_callconv));
			}

		} break;
	case TINK_Struct:
		{
			char aChScopid[32];
			SStringBuffer strbuf(aChScopid, EWC_DIM(aChScopid));
			FormatCoz(&strbuf, "%d)", pTin->m_scopid);

			pSeb->AppendCoz(aChScopid);
			pSeb->AppendCoz(pTin->m_strName.PCoz());

			auto pTinstruct = (STypeInfoStruct *)pTin;
			if (pTinstruct->m_pStnodStruct)
			{
				auto pStnodStruct = pTinstruct->m_pStnodStruct;
				auto pStstruct = PStmapRtiCast<CSTStruct *>(pStnodStruct->m_pStmap);
				const char * pChzPrefix = "";
				if (EWC_FVERIFY(pStstruct, "expected struct") && pStstruct->m_iStnodParameterList >= 0)
				{
					auto pStnodParamList = pStnodStruct->PStnodChild(pStstruct->m_iStnodParameterList);
					for (int iStnodParam = 0; iStnodParam < pStnodParamList->CStnodChild(); ++iStnodParam)
					{
						auto pStnodParam = pStnodParamList->PStnodChild(iStnodParam);
						if (EWC_FVERIFY(pStnodParam->m_pTin, "expected type"))
						{
							pSeb->AppendCoz(pChzPrefix);
							pChzPrefix = ",";
							AppendTypeDescriptor(pStnodParam->m_pTin, pSeb);
						}
					}
				}
			}
			else
			{
				EWC_ASSERT(false, "expected an AST");
				// may need to handle generic structs that haven't instantiated their AST yet.
			}
		} break;
	default:
		char aChScopid[32];
		SStringBuffer strbuf(aChScopid, EWC_DIM(aChScopid));
		FormatCoz(&strbuf, "%d)", pTin->m_scopid);

		pSeb->AppendCoz(aChScopid);
		pSeb->AppendCoz(pTin->m_strName.PCoz());
	}

	pTin->m_strDesc = CString(&pSeb->PCoz()[cBPrefix], pSeb->CB());
}

CUniqueTypeRegistry::CUniqueTypeRegistry(CAlloc * pAlloc)
:m_pAlloc(pAlloc)
,m_hashHvPTinUnique(pAlloc, BK_UniqueTypeRegistry, 0)
,m_scopidNext(SCOPID_Min)
{
}

void CUniqueTypeRegistry::Clear()
{
	m_scopidNext = SCOPID_Min;
	m_hashHvPTinUnique.Clear(0);
}

static inline u64 HvForPTin(STypeInfo * pTin, TINK tink, u8 other = 0)
{
	EWC_ASSERT(!pTin->m_strDesc.FIsEmpty(), "expected descriptor string");
	return u64(u64(pTin->m_strDesc.Hv()) | (u64(tink) << 32) | (u64(other) << 40));
}

static inline u64 HvForPTin(const CString & str, TINK tink)
{
	// really not needed, should switch back to just a string HV
	return u64(str.Hv() ^ (u64(tink) << 56));
}

STypeInfo * CUniqueTypeRegistry::PTinMakeUnique(STypeInfo * pTin)
{
	// NOTES ON UNIQUE TYPES (to avoid me getting it wrong again)
	// named types are unique'd based on name + definition namespace + parameters (if needed)
	//    not: based on type footprint ala LLVM type

	// unnamed types (pointer, array, qualifier) are named relative to contained types

	if (pTin->m_grftin.FIsSet(FTIN_IsUnique))
		return pTin;

	u64 hv;
	switch (pTin->m_tink)
	{
	case TINK_Literal:
	case TINK_Generic:
	case TINK_Type:
		// these types are not 'unique'd
		return pTin;
#if 0 // probably not worth the hassle here
	case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;
			auto pTinPointedTo = PTinMakeUnique(pTinptr->m_pTinPointedTo);
			pTinptr->m_pTinPointedTo = pTinPointedTo;

			hv = HvForPTin(pTinPointedTo, TINK_Pointer);
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			auto pTinUnqual = PTinMakeUnique(pTinqual->m_pTin);
			pTinqual->m_pTin = pTinUnqual;

			hv = HvForPTin(pTinUnqual, TINK_Qualifier, pTinqual->m_grfqualk.m_raw);
		} break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
			auto pTinElement = PTinMakeUnique(pTinary->m_pTin);
			pTinary->m_pTin = pTinElement;

			if (pTinary->m_aryk != ARYK_Fixed)
			{
				hv = HvForPTin(pTinElement, TINK_Array, u8(pTinary->m_aryk));
			}
		} // fallthrough
#else
	case TINK_Pointer:
	case TINK_Qualifier:
	case TINK_Array:
#endif
	case TINK_Bool:
	case TINK_Void:
	case TINK_Null:
	case TINK_Any:
	case TINK_Integer:
	case TINK_Float:
	case TINK_Enum:
		{
			EWC::SStringEditBuffer seb(m_pAlloc);

			seb.Clear();
			AppendTypeDescriptor(pTin, &seb);
			hv = HvForPTin(pTin->m_strDesc, pTin->m_tink);

		} break;

	case TINK_Procedure:
	case TINK_Struct:
		{
			EWC::SStringEditBuffer seb(m_pAlloc);

			seb.Clear();
			AppendTypeDescriptor(pTin, &seb);
			hv = HvForPTin(pTin->m_strDesc, pTin->m_tink);
		} break;
	default:
		EWC_ASSERT(false, "unhandled type kind");
		break;
	}

	STypeInfo ** ppTin;
	FINS fins = m_hashHvPTinUnique.FinsEnsureKey(hv, &ppTin);
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
			pTin->m_pTinNative = PTinMakeUnique(pTin->m_pTinNative);
		}

		*ppTin = pTin;

		// make sure the types referred to by this type are unique
		// NOTE: cyclical references shouldn't be a problem now that we've added this type
		switch (pTin->m_tink)
		{
		case TINK_Pointer:
			{
				auto pTinptr = (STypeInfoPointer *)pTin;
				pTinptr->m_pTinPointedTo = PTinMakeUnique(pTinptr->m_pTinPointedTo);
			} break;
		case TINK_Qualifier:
			{
				auto pTinqual = (STypeInfoQualifier *)pTin;
				pTinqual->m_pTin = PTinMakeUnique(pTinqual->m_pTin);
			} break;
		case TINK_Array:
			{
				auto pTinary = (STypeInfoArray *)pTin;
				pTinary->m_pTin = PTinMakeUnique(pTinary->m_pTin);
			} break;
		case TINK_Procedure:
			{
				auto pTinproc = (STypeInfoProcedure *)pTin;
				for (size_t ipTin = 0; ipTin < pTinproc->m_arypTinParams.C(); ++ipTin)
				{
					pTinproc->m_arypTinParams[ipTin] =  PTinMakeUnique(pTinproc->m_arypTinParams[ipTin]);
				}

				for (size_t ipTin = 0; ipTin < pTinproc->m_arypTinReturns.C(); ++ipTin)
				{
					pTinproc->m_arypTinReturns[ipTin] =  PTinMakeUnique(pTinproc->m_arypTinReturns[ipTin]);
				}
			} break;
		case TINK_Struct:
			{
				auto pTinstruct = (STypeInfoStruct *)pTin;
			} break;

		default:
			break;
		}
	}
	return pTin;
}

void CSymbolTable::AddManagedSymtab(CSymbolTable * pSymtab)
{
	EWC_ASSERT(pSymtab->m_pSymtabNextManaged == nullptr, "trying to add managed symtab");

	pSymtab->m_pSymtabNextManaged = m_pSymtabNextManaged;
	m_pSymtabNextManaged = pSymtab;
}

void CSymbolTable::AddBuiltInType(SErrorManager * pErrman, SLexer * pLex, STypeInfo * pTin, GRFSYM grfsym)
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

		auto pSym = PSymEnsure(pErrman, strName, nullptr, FSYM_IsBuiltIn | FSYM_IsType | FSYM_VisibleWhenNested | grfsym.m_raw);
		pSym->m_pTin = pTin;
	}
	else
	{
		SLexerLocation lexloc = (pLex) ? SLexerLocation(pLex) : SLexerLocation();
		EmitError(pErrman, &lexloc, ERRID_UnknownError, "Two types encountered with same name (%s)", strName.PCoz());
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
,m_pStmap(nullptr)
,m_lexloc(lexLoc)
,m_pTin(nullptr)
,m_pOptype(nullptr)
,m_pSymtab(nullptr)
,m_pSymbase(nullptr)
,m_arypStnodChild(pAlloc, EWC::BK_SyntaxTree)
#if TRACK_IINSREQ
,m_iInsreq(-1)
#endif
{
}

CSTNode::~CSTNode()
{
	EWC_ASSERT(!m_grfstnod.FIsSet(FSTNOD_AssertOnDelete), "assert on delete flag set");

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

	if (m_pStmap)
	{
		pAlloc->EWC_DELETE(m_pStmap);
		m_pStmap = nullptr;
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
		case PARK_ConstantDecl:		AppendCoz(pStrbuf, "constdecl");return;
		case PARK_Decl:				AppendCoz(pStrbuf, "decl");		return;
		case PARK_ArrayDecl:		AppendCoz(pStrbuf, "arydecl");	return;
		default:					AppendCoz(pStrbuf, "unk");		return;
		}
	}

	switch (pTin->m_tink)
	{
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			const char * pChzSpacer = (grfdbgstr.FIsSet(FDBGSTR_NoWhitespace)) ? "." : " ";
			AppendFlagNames(pStrbuf, pTinqual->m_grfqualk, pChzSpacer);
			AppendCoz(pStrbuf, pChzSpacer);
			PrintTypeInfo(pStrbuf, pTinqual->m_pTin, park, grfdbgstr);
			return;
		} break;
	case TINK_Pointer:		
		{
			auto pTinptr = (STypeInfoPointer*)pTin;
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
			case ARYK_Dynamic:		AppendCoz(pStrbuf, "[..]");					break;
			case ARYK_Reference:	AppendCoz(pStrbuf, "[]");					break;
			default: 
				EWC_ASSERT(false, "Unhandled ARYK");
				break;
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
			else if (!grfdbgstr.FIsSet(FDBGSTR_LiteralSize))
			{
				AppendCoz(pStrbuf, PChzFromLitk(pTinlit->m_litty.m_litk));
				AppendCoz(pStrbuf, " ");

				if (pTinlit->m_litty.m_litk == LITK_Compound && pTinlit->m_pTinSource)
				{
					AppendCoz(pStrbuf, "(");
					PrintTypeInfo(pStrbuf, pTinlit->m_pTinSource, PARK_Nil, grfdbgstr);
					AppendCoz(pStrbuf, ")");
				}
			}
			
			AppendCoz(pStrbuf, "Literal");
			return;
		}
	case TINK_Generic:
		{
			auto pTingen = (STypeInfoGeneric *)pTin;
			FormatCoz(pStrbuf, "$%s", pTingen->m_strName.PCoz());
			return;	
		}
    case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;
			FormatCoz(pStrbuf, "%s(", pTin->m_strName.PCoz());

			size_t cpTin = pTinproc->m_arypTinParams.C();
			size_t cCommas = (pTinproc->FHasVarArgs()) ? cpTin : cpTin - 1;
			for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
			{
				PrintTypeInfo(pStrbuf, pTinproc->m_arypTinParams[ipTin], PARK_Nil, grfdbgstr);

				if (ipTin < cCommas)
				{
					AppendCoz(pStrbuf, ", ");
				}
			}

			if (pTinproc->FHasVarArgs())
			{
				AppendCoz(pStrbuf, "..");
			}

			AppendCoz(pStrbuf, ")->");

			cpTin = pTinproc->m_arypTinReturns.C();
			for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
			{
				PrintTypeInfo(pStrbuf, pTinproc->m_arypTinReturns[ipTin], PARK_Nil, grfdbgstr);
			}

			if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				AppendCoz(pStrbuf, " #foreign");
			}

			return;
		}
    case TINK_Struct:
		{
			FormatCoz(pStrbuf, "%s", pTin->m_strName.PCoz());

			auto pTinstruct = (STypeInfoStruct *)pTin;
			if (pTinstruct->m_pGenmap)
			{
				PrintGenmapAnchors(pStrbuf, pTinstruct->m_pGenmap);
			}
			else if (pTinstruct->FHasGenericParams() && grfdbgstr.FIsSet(FDBGSTR_ShowStructArgs))
			{
				auto pStnodStruct = pTinstruct->m_pStnodStruct;
				CSTStruct * pStstruct = nullptr;

				if (pStnodStruct)
				{
					pStstruct = PStmapRtiCast<CSTStruct *>(pStnodStruct->m_pStmap);
				}

				if (EWC_FVERIFY(pStstruct && pStstruct->m_iStnodParameterList >= 0, "expected parameter list"))
				{
					auto pStnodParameterList = pStnodStruct->PStnodChild(pStstruct->m_iStnodParameterList);
					int cpStnodParam = pStnodParameterList->CStnodChild();
					const char * pChzSeparate = "(";
					const char * pChzClose = ")";
					for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
					{
						auto pStnodParam = pStnodParameterList->PStnodChild(ipStnodParam);
						auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
						if (!pStdecl)
							continue;
	
						FormatCoz(pStrbuf, "%s", pChzSeparate);
						pChzSeparate = ", ";
						pChzClose = ")";
						if (pStdecl->m_iStnodIdentifier >= 0)
						{
							auto strIdent = StrFromIdentifier(pStnodParam->PStnodChild(pStdecl->m_iStnodIdentifier));
							FormatCoz(pStrbuf, "$%s ", strIdent.PCoz());
						}

						if (pStdecl->m_iStnodType >= 0)
						{
							auto strType = StrFromTypeInfo(pStnodParam->PStnodChild(pStdecl->m_iStnodType)->m_pTin);
							FormatCoz(pStrbuf, ":%s", strType.PCoz());
						}
					}
					AppendCoz(pStrbuf, pChzClose);
				}
			}

			return;
		}
    case TINK_Enum:
		{
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
    case TINK_Flag:			// fall through ...
    case TINK_Void:			// fall through ...
    case TINK_Null:			// fall through ...
    case TINK_Any:			// fall through ...
    case TINK_Type:			// fall through ...
		AppendCoz(pStrbuf, pTin->m_strName.PCoz()); 
		break;
	default:
		EWC_ASSERT(false, "unhandled TINK in PrintTypeInfo");
		return;
	}
}

void PrintLiteral(EWC::SStringBuffer * pStrbuf, CSTNode * pStnodLit)
{
	if (!EWC_FVERIFY(pStnodLit->m_park == PARK_Literal && pStnodLit->m_pStval, "bad literal in PrintLiteral"))
		return;

	switch (pStnodLit->m_pStval->m_stvalk)
	{
	case STVALK_String:			FormatCoz(pStrbuf, "\"%s\"", pStnodLit->m_pStval->m_str.PCoz());		return;
	case STVALK_UnsignedInt:	FormatCoz(pStrbuf, "%llu", pStnodLit->m_pStval->m_nUnsigned);			return;
	case STVALK_SignedInt:		FormatCoz(pStrbuf, "%lld", pStnodLit->m_pStval->m_nSigned);				return;
	case STVALK_Float:			FormatCoz(pStrbuf, "%f", pStnodLit->m_pStval->m_g);						return;
	case STVALK_ReservedWord:	FormatCoz(pStrbuf, "%s", PCozFromRword(pStnodLit->m_pStval->m_rword));	return;
	default:
		EWC_ASSERT(false, "unknown literal %s", PCozFromTok(pStnodLit->m_tok));
		return;
	}
}

void PrintStval(EWC::SStringBuffer * pStrbuf, CSTNode * pStnod)
{
	if (!pStnod->m_pStval)
	{
		AppendCoz(pStrbuf, "novalue");
		return;
	}

	auto pStval = pStnod->m_pStval;
	auto  pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnod->m_pTin);
	auto stvalk = pStval->m_stvalk;
	if (pTinlit)
	{
		switch (pTinlit->m_litty.m_litk)
		{
			case LITK_Char:		// fallthrough
			case LITK_Enum:		// fallthrough
			case LITK_Integer:	stvalk = (pTinlit->m_litty.m_fIsSigned) ? STVALK_SignedInt : STVALK_UnsignedInt;	break;
			case LITK_Float:	stvalk = STVALK_Float;																break;
			case LITK_String:	stvalk = STVALK_String;																break;
			case LITK_Bool:		FormatCoz(pStrbuf, "%s", (pStval->m_nUnsigned == 0) ? "false" : "true");			return;
			case LITK_Null:		AppendCoz(pStrbuf, "null");															return;
			case LITK_Compound:
				{
					AppendCoz(pStrbuf, "(");

					auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
					if (!EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "array literal with no values"))
						break;

					auto pStnodList = pStnod->PStnodChild(pStdecl->m_iStnodInit);
					for (int ipStnod = 0; ipStnod < pStnodList->CStnodChild(); ++ipStnod)
					{
						PrintStval(pStrbuf, pStnodList->PStnodChild(ipStnod));
						if (ipStnod + 1 < pStnodList->CStnodChild())
						{
							AppendCoz(pStrbuf, ", ");
						}
					}

					AppendCoz(pStrbuf, ")");
				} break;
		}
	}

	switch (stvalk)
	{
	case STVALK_Float:			FormatCoz(pStrbuf, "%f", pStval->m_g);												return;
	case STVALK_SignedInt:		FormatCoz(pStrbuf, "%lld", pStval->m_nSigned);										return;
	case STVALK_UnsignedInt:	FormatCoz(pStrbuf, "%llu", pStval->m_nUnsigned);									return;
	case STVALK_String:			FormatCoz(pStrbuf, "'%s'", pStval->m_str.PCoz());									return;
	case STVALK_ReservedWord:	FormatCoz(pStrbuf, "%s", PCozFromRword(pStval->m_rword));							return;
	}
}

void PrintStnodName(EWC::SStringBuffer * pStrbuf, CSTNode * pStnod)
{
	if (!pStnod)
	{
		AppendCoz(pStrbuf, "null");
		return;
	}

	switch (pStnod->m_park)
	{
	case PARK_Identifier:			FormatCoz(pStrbuf, "%s", StrFromIdentifier(pStnod).PCoz());	return;
	case PARK_ReservedWord:			AppendCoz(pStrbuf, PCozFromRword(pStnod->m_pStval->m_rword));	return;
	case PARK_Nop:					AppendCoz(pStrbuf, "nop");										return;
	case PARK_Literal:				
		{
			PrintLiteral(pStrbuf, pStnod);
			return;
		}
	case PARK_AdditiveOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_MultiplicativeOp:	    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_ShiftOp:			    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_RelationalOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_LogicalAndOrOp:	    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_UnaryOp:			    FormatCoz(pStrbuf, "unary[%s]", PCozFromTok(pStnod->m_tok));		return;
	case PARK_PostfixUnaryOp:		FormatCoz(pStrbuf, "postUnary[%s]", PCozFromTok(pStnod->m_tok));	return;
	case PARK_AssignmentOp:		    FormatCoz(pStrbuf, "%s", PCozFromTok(pStnod->m_tok));				return;
	case PARK_ArrayElement:		    AppendCoz(pStrbuf, "elem");					return;
	case PARK_MemberLookup:		    AppendCoz(pStrbuf, "member");				return;
	case PARK_ProcedureCall:		AppendCoz(pStrbuf, "procCall");				return;
	case PARK_SpecializedStruct:	AppendCoz(pStrbuf, "specStruct");			return;
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
	case PARK_QualifierDecl:		AppendCoz(pStrbuf, PCozFromRword(pStnod->m_pStval->m_rword));	return;
	case PARK_Decl:					AppendCoz(pStrbuf, "decl");					return;
	case PARK_Typedef:				AppendCoz(pStrbuf, "typedef");				return;
	case PARK_ConstantDecl:			AppendCoz(pStrbuf, "const");				return;
	case PARK_ProcedureDefinition:	AppendCoz(pStrbuf, "func");					return;
	case PARK_EnumDefinition:		AppendCoz(pStrbuf, "enum");					return;
	case PARK_StructDefinition:		AppendCoz(pStrbuf, "struct");				return;
	case PARK_EnumConstant:			AppendCoz(pStrbuf, "enumConst");			return;
	case PARK_VariadicArg:			AppendCoz(pStrbuf, "..");					return;
	case PARK_CompoundLiteral:		AppendCoz(pStrbuf, "compLit");				return;
	case PARK_Cast:					AppendCoz(pStrbuf, "cast");					return;
	case PARK_ArgumentLabel:		AppendCoz(pStrbuf, "`"); 					return;
	case PARK_GenericDecl:			AppendCoz(pStrbuf, "gendecl"); 				return;
	case PARK_GenericStructSpec:	AppendCoz(pStrbuf, "genstruct");			return;
	case PARK_TypeArgument:			AppendCoz(pStrbuf, "typearg");				return;
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

	if (grfdbgstr.FIsSet(FDBGSTR_Values))
	{
		if (pStnod->m_pStval)
		{
			PrintStval(pStrbuf, pStnod);
		}
		else
		{
			if (pStnod->m_park == PARK_Identifier || pStnod->m_park == PARK_ReservedWord)
			{
				PrintStnodName(pStrbuf, pStnod);
			}
			else
			{
				AppendCoz(pStrbuf, "_");
			}
		}
		grfdbgstr.Clear(FDBGSTR_Values);
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

CString StrFromTypeInfo(STypeInfo * pTin)
{
	if (!pTin)
	{
		return CString("null");
	}

	char aCh[1024];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	PrintTypeInfo(&strbuf, pTin, PARK_Nil);
	return CString(aCh);
}

CString StrFromSTNode(CSTNode * pStnod)
{
	if (!pStnod)
	{
		return CString("null");
	}

	char aCh[1024];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	PrintStnodName(&strbuf, pStnod);
	return CString(aCh);
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

void HideDebugStringForEntries(CWorkspace * pWork, size_t cBHiddenMax)
{
	// hide entrys before a given point (for omitting the prereq from a unit test)

	BlockListEntry::CIterator iter(&pWork->m_blistEntry);
	while (SWorkspaceEntry * pEntry = iter.Next())
	{
		auto pStnod = pEntry->m_pStnod;
		if (pStnod && pStnod->m_lexloc.m_dB < cBHiddenMax)
		{
			pEntry->m_fHideDebugString = true;
		}
	}
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

	int ipStnod = 0;
	int cEntry = (int)pWork->m_blistEntry.C();

	BlockListEntry::CIterator iter(&pWork->m_blistEntry);
	while (SWorkspaceEntry * pEntry = iter.Next())
	{
		++ipStnod;
		if (pEntry->m_fHideDebugString)
			continue;

		CSTNode * pStnod = pEntry->m_pStnod;
		pStnod->WriteDebugString(&strbuf, grfdbgstr);

		if ((CBFree(strbuf) > 0) & (ipStnod != cEntry))
		{
			*strbuf.m_pCozAppend++ = ' ';
		}

#if TEST_STNODE_COPY
		CSTNode * pStnodCopy = PStnodCopy(pWork->m_pAlloc, pStnod);
		pStnodCopy->WriteDebugString(&strbufCopy, grfdbgstr);
		pWork->m_pAlloc->EWC_DELETE(pStnodCopy);

		if ((CBFree(strbufCopy) > 0) & (ipStnod != cEntry))
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

CString StrIdentifierFromDecl(CSTNode * pStnodDecl)
{
	if (pStnodDecl &&
		EWC_FVERIFY(pStnodDecl->m_park == PARK_Decl, "expected decl") &&
		EWC_FVERIFY(pStnodDecl->m_pStmap && pStnodDecl->m_pStmap->m_stmapk == STMAPK_Decl, "decl has no stmap"))
	{
		auto pStdecl = (CSTDecl*)pStnodDecl->m_pStmap;
		if (pStdecl->m_iStnodIdentifier >= 0)
		{
			return StrFromIdentifier(pStnodDecl->PStnodChild(pStdecl->m_iStnodIdentifier));
		}
	}

	return CString();
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
