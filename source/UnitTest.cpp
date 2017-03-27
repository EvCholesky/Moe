/* Copyright (C) 2017 Evan Christensen
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


#include "UnitTest.h"

#include "CodeGen.h"
#include "Error.h"
#include "EwcArray.h"
#include "EwcString.h"
#include "EwcTypes.h"
#include "Lexer.h"
#include "Parser.h"
#include "Workspace.h"

#include <cstdarg>
#include <stdio.h>

using namespace EWC;


extern bool FTestLexing();
extern bool FTestSigned65();
extern bool FTestUnicode();
extern bool FTestUniqueNames(CAlloc * pAlloc);


struct SPermutation;
struct SOption // tag = opt
{
							SOption(CAlloc * pAlloc)
							:m_pCozOption(nullptr)
							,m_erridExpected(ERRID_Nil)
							,m_fAllowSubstitution(false)
								{ ; }

							void FreeAll(CAlloc * pAlloc)
								{ ; }

	const char *			m_pCozOption;
	ERRID					m_erridExpected;
	bool					m_fAllowSubstitution;
};

struct SPermutation // tag = perm
{
							SPermutation(CAlloc * pAlloc)
							:m_strVar()
							,m_arypOpt(pAlloc, BK_UnitTest)
							,m_arypPermChild(pAlloc, BK_UnitTest)
							,m_lexloc()
								{ ; }

							void FreeAll(CAlloc * pAlloc)
							{
								auto ppOptEnd = m_arypOpt.PMac();
								for (auto ppOpt = m_arypOpt.A(); ppOpt != ppOptEnd; ++ppOpt)
								{
									(*ppOpt)->FreeAll(pAlloc);
								}
								m_arypOpt.Clear();

								auto ppPermEnd = m_arypPermChild.PMac();
								for (auto ppPerm = m_arypPermChild.A(); ppPerm != ppPermEnd; ++ppPerm)
								{
									(*ppPerm)->FreeAll(pAlloc);
								}
								m_arypPermChild.Clear();
							}

	CString					m_strVar;			// substitution name '$type' (excluding the '$')
	CDynAry<SOption *>		m_arypOpt;
	CDynAry<SPermutation *>	m_arypPermChild;	// other variable to permute for this option
	SLexerLocation			m_lexloc;
};

enum UTESTK
{
	UTESTK_Permute,
	UTESTK_Builtin,

	EWC_MAX_MIN_NIL(UTESTK)
};

struct SUnitTest // tag = utest
{
							SUnitTest(CAlloc * pAlloc)
							:m_strName()
							,m_pCozPrereq(nullptr)
							,m_pCozInput(nullptr)
							,m_pCozParse(nullptr)
							,m_pCozTypeCheck(nullptr)
							,m_utestk(UTESTK_Permute)
							,m_arypPerm(pAlloc, BK_UnitTest)
								{ ; }

							void FreeAll(CAlloc * pAlloc)
							{
								pAlloc->EWC_FREE(m_pCozPrereq);
								pAlloc->EWC_FREE(m_pCozInput);
								pAlloc->EWC_FREE(m_pCozParse);
								pAlloc->EWC_FREE(m_pCozTypeCheck);
								m_pCozPrereq = nullptr;
								m_pCozPrereq = nullptr;
								m_pCozPrereq = nullptr;
								m_pCozPrereq = nullptr;

								auto ppPermEnd = m_arypPerm.PMac();
								for (auto ppPerm = m_arypPerm.A(); ppPerm != ppPermEnd; ++ppPerm)
								{
									(*ppPerm)->FreeAll(pAlloc);
								}
								m_arypPerm.Clear();
							}

	CString					m_strName;
	char *					m_pCozPrereq;
	char *					m_pCozInput;
	char *					m_pCozParse;
	char *					m_pCozTypeCheck;
	UTESTK					m_utestk;

	CDynAry<SPermutation *>	m_arypPerm;
};



enum FUNT	// Flag UNit Tests
{
	FUNT_First		= 0x1,

	FUNT_None		= 0x0,
	FUNT_All		= 0x1,
};

EWC_DEFINE_GRF(GRFUNT, FUNT, u32)


struct SSubstitution // tag = sub
{
	CString			m_strVar;
	SOption	*		m_pOpt;
	const char *	m_pCozOption;	// option string with substitutions
};

enum TESTRES	// TEST RESults
{
	TESTRES_Success,
	TESTRES_UnitTestFailure,	// failed parsing the test
	TESTRES_SourceError,		// the unit test has an unexpected error in it's source
	TESTRES_MissingExpectedErr,
	TESTRES_ParseMismatch,		
	TESTRES_TypeCheckMismatch,
	TESTRES_CodeGenFailure,
	TESTRES_BuiltinFailure,
	
	EWC_MAX_MIN_NIL(TESTRES)
};

typedef CFixAry<SSubstitution, 64> CSubStack;

struct STestContext // tag= tesctx
{
						STestContext(EWC::CAlloc * pAlloc, SErrorManager * pErrman, CWorkspace * pWork)
						:m_pAlloc(pAlloc)
						,m_pErrman(pErrman)
						,m_pWork(pWork)
						,m_arySubStack()
							{ ZeroAB(m_mpTestresCResults, sizeof(m_mpTestresCResults)); }

	EWC::CAlloc *			m_pAlloc;
	SErrorManager *			m_pErrman;
	CWorkspace *			m_pWork;

	int						m_mpTestresCResults[TESTRES_Max];
	CSubStack				m_arySubStack;
};

static bool FConsumeIdentifier(STestContext * pTesctx, SLexer  * pLex, const CString & strIdent)
{
	if (pLex->m_tok != TOK_Identifier)
		return false;

	if (pLex->m_str != strIdent)
		return false;

	TokNext(pLex);
	return true;
}

void ParseError(STestContext * pTesctx, SLexerLocation * pLexloc, const char * pChzFormat, ...)
{
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pTesctx->m_pErrman, pLexloc, ERRID_UnknownError, pChzFormat, ap);
}

void ParseError(STestContext * pTesctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pTesctx->m_pErrman, &lexloc, ERRID_UnknownError, pChzFormat, ap);
}

void ParseWarning(STestContext * pTesctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitWarning(pTesctx->m_pErrman, &lexloc, ERRID_UnknownWarning, pChzFormat, ap);
}

char * PCozExpectString(STestContext * pTesctx, SLexer * pLex)
{
	if (pLex->m_tok != TOK_Literal || pLex->m_litk != LITK_String)
	{
		ParseError(pTesctx, pLex, "Expected string literal but encountered '%s'", PCozCurrentToken(pLex));
		return nullptr;
	}

	size_t cB = pLex->m_str.CB();
	char * pCozReturn = (char*)pTesctx->m_pAlloc->EWC_ALLOC(cB, EWC_ALIGN_OF(char));
	EWC::CBCopyCoz(pLex->m_str.PCoz(), pCozReturn, cB);

	TokNext(pLex);
	return pCozReturn;
}

char * PCozParseInside(STestContext * pTesctx, SLexer * pLex, TOK tokBegin, TOK tokEnd)
{
	if (pLex->m_tok != tokBegin)
	{
		ParseError(pTesctx, pLex, "Expected '%s' but encountered '%s'", PCozFromTok(tokBegin), PCozCurrentToken(pLex));
		return nullptr;
	}

	TokNext(pLex);

	SLexerLocation lexlocBegin(pLex);
	auto pCozInput = pLex->m_pChBegin;

	int cMatches = 1;
	while (pLex->m_tok != TOK_Eof)
	{
		if (pLex->m_tok == tokBegin)
			++cMatches;
		if (pLex->m_tok == tokEnd)
		{
			--cMatches;
			if (cMatches <= 0)
				break;
		}
		TokNext(pLex);
	}
	SLexerLocation lexlocEnd(pLex);
	TokNext(pLex);

	size_t cB = lexlocEnd.m_dB - lexlocBegin.m_dB + 1;
	char * pCozReturn = (char*)pTesctx->m_pAlloc->EWC_ALLOC(cB, EWC_ALIGN_OF(char));
	EWC::CBCopyCoz(pCozInput, pCozReturn, cB);
	return pCozReturn;
}

bool FExpectToken(STestContext * pTesctx, SLexer * pLex, TOK tok)
{
	if (!FConsumeToken(pLex, tok))
	{
		ParseError(pTesctx, pLex, "Expected '%s', but encountered '%s'", PCozFromTok(tok), PCozCurrentToken(pLex));
		return false;
	}

	return true;
}

void PromoteStringEscapes(STestContext * pTesctx, SLexerLocation * pLexloc, const char * pCozInput, SStringEditBuffer * pSeb)
{
	const char * pCozIn = pCozInput;
	if (*pCozIn != '"')
	{
		ParseError(pTesctx, pLexloc, "missing opening string quote for string literal (%s) ", pCozInput);
	}
	else
	{
		++pCozIn;
	}

	while (*pCozIn != '"')
	{
		if (*pCozIn == '\0')
		{
			ParseError(pTesctx, pLexloc, "missing closing quote for string literal (%s) ", pCozInput);
			break;
		}

		if (*pCozIn == '\\')
		{
			++pCozIn;
			switch (*pCozIn)
			{
			case '\\':	pSeb->AppendCoz("\\");	break;
			case 'n':	pSeb->AppendCoz("\n");	break;
			case 'r':	pSeb->AppendCoz("\r");	break;
			case 't':	pSeb->AppendCoz("\t");	break;
			case '"':	pSeb->AppendCoz("\"");	break;
			case '\'':	pSeb->AppendCoz("\\");	break;

			default:	pSeb->AppendCoz("?");		break;
			}
			++pCozIn;
		}
		else
		{
			pCozIn += pSeb->CBAppendCodepoint(pCozIn);
		}
	}
}

static inline bool FIsIdentifierChar(const char * pCoz)
{
	return ((*pCoz >= 'a') & (*pCoz <= 'z')) | ((*pCoz >= 'A') & (*pCoz <= 'Z'))
		 || (*pCoz == '_') 
		 || (u8(*pCoz) >= 128);   // >= 128 is UTF8 char
}

char * PCozAllocateSubstitution(
	STestContext * pTesctx,
	SLexerLocation * pLexloc,
	const char * pCozInput,
	const CSubStack & arySub,
	SSubstitution * pSubSelf = nullptr)
{
	if (!pCozInput)
		pCozInput = "";
	SStringEditBuffer seb(pTesctx->m_pAlloc);

	const char * pCozIt = pCozInput;
	while (*pCozIt != '\0')
	{
		if (*pCozIt == '$')
		{
			++pCozIt;

			int cCodepoint = 0;
			auto pCozEnd = pCozIt;
			while (FIsIdentifierChar(pCozEnd))
			{
				++cCodepoint;
				pCozEnd += CBCodepoint(pCozEnd);
			}

			auto pSubMax = arySub.PMac();
			const SSubstitution * pSub = nullptr;
			for (auto pSubIt = arySub.A(); pSubIt != pSubMax; ++pSubIt)
			{
				if (FAreCozEqual(pSubIt->m_strVar.PCoz(), pCozIt, cCodepoint) && CCodepoint(pSubIt->m_strVar.PCoz()) == cCodepoint)
				{
					pSub = pSubIt;
					break;
				}
			}

			if (!pSub)
			{
				CString strName(pCozIt, pCozEnd - pCozIt +1);
				ParseError(pTesctx, pLexloc, "Unable to find substitution for $%s in string %s", strName.PCoz(), pCozInput);
				return nullptr;
			}
			else if (pSub == pSubSelf)
			{
				ParseError(pTesctx, pLexloc, "Cannot substitute $%s recursively in option %s", pSubSelf->m_strVar.PCoz(), pCozInput);
				return nullptr;
			}

			seb.AppendCoz(pSub->m_pCozOption);
			pCozIt = pCozEnd;
		}
		else
		{
			pCozIt += seb.CBAppendCodepoint(pCozIt);
		}
	}

	size_t cBSeb = seb.CB();
	return seb.PCozAllocateCopy(pTesctx->m_pAlloc);
}

SOption * POptParse(STestContext * pTesctx, SLexer * pLex)
{
	SLexerLocation lexloc(pLex);
	const char * pCozMin = pLex->m_pChBegin;
	const char * pCozMax = pCozMin;
	const char * pCozErridMin = nullptr;
	const char * pCozErridMax = nullptr;
	ERRID errid = ERRID_Nil;
	while (pLex->m_tok != TOK('|') && pLex->m_tok != TOK(')'))
	{
		if (pLex->m_tok == TOK('$'))
		{
			pCozErridMin = pLex->m_pChBegin;

			TokNext(pLex);

			if (!FConsumeIdentifier(pTesctx, pLex, "errid"))
			{
				ParseError(pTesctx, pLex, "expected $errid, encountered '%s'", PCozCurrentToken(pLex));
				break;
			}
			
			if (FExpectToken(pTesctx, pLex, TOK('(')))
			{
				if (pLex->m_tok != TOK_Literal)
				{
					ParseError(pTesctx, pLex, "expected $errid number, encountered '%s'", PCozCurrentToken(pLex));
				}
				else
				{
					errid = (ERRID)pLex->m_n;
					TokNext(pLex);
				}
				FExpectToken(pTesctx, pLex, TOK(')'));
			}
			pCozErridMax = pLex->m_pChBegin;
			continue;
		}
		TokNext(pLex);
	}

	SOption * pOpt = EWC_NEW(pTesctx->m_pAlloc, SOption) SOption(pTesctx->m_pAlloc);

	// copy the option string but omit the errid section

	pCozMax = pLex->m_pChBegin;
	size_t cBPrefix = pCozMax - pCozMin;
	if (pCozErridMin)
	{
		cBPrefix = pCozErridMin - pCozMin;
	}

	size_t cBPostfix = 0;
	if (pCozErridMin)
	{
		EWC_ASSERT(pCozErridMax, "expected min and max together");
		cBPostfix = (pCozMax - pCozErridMax);
	}
	EWC_ASSERT(cBPrefix + cBPostfix > 0, "no option string");
	char * pCozOption = (char*)pTesctx->m_pAlloc->EWC_ALLOC(cBPrefix + cBPostfix+1, EWC_ALIGN_OF(char));

	if (cBPrefix)
	{
		EWC::CBCopyCoz(pCozMin, pCozOption, cBPrefix+1);
	}
	if (cBPostfix)
	{
		EWC::CBCopyCoz(pCozErridMin, pCozOption + cBPrefix, cBPostfix+1);
	}

	if (pCozOption[0] == '"')
	{
		SStringEditBuffer seb(pTesctx->m_pAlloc);
		PromoteStringEscapes(pTesctx, &lexloc, pCozOption, &seb);
		pTesctx->m_pAlloc->EWC_DELETE(pCozOption);
		pCozOption = seb.PCozAllocateCopy(pTesctx->m_pAlloc);
		pOpt->m_fAllowSubstitution = true;
	}

	pOpt->m_pCozOption = pCozOption;

	pOpt->m_erridExpected = errid;
	return pOpt;
}

static SPermutation * PPermParse(STestContext * pTesctx, SLexer * pLex)
{
	SLexerLocation lexloc(pLex);
	if (pLex->m_tok != TOK('$'))
	{
		return nullptr;
	}

	TokNext(pLex);

	if (pLex->m_tok != TOK_Identifier)
	{
		ParseError(pTesctx, pLex, "Expected variable name (following '$'), but encountered '%s'", PCozCurrentToken(pLex));
		return nullptr;
	}

	SPermutation * pPerm = EWC_NEW(pTesctx->m_pAlloc, SPermutation) SPermutation(pTesctx->m_pAlloc);
	pPerm->m_strVar = pLex->m_str;
	pPerm->m_lexloc = lexloc;
	TokNext(pLex);

	if (FConsumeToken(pLex, TOK('(')))
	{
		while (1)
		{
			auto pOpt = POptParse(pTesctx, pLex);
			if (pOpt)
			{
				pPerm->m_arypOpt.Append(pOpt);
			}

			if (pLex->m_tok != TOK('|'))
				break;
			TokNext(pLex); // consume the '|'
		}
	}
	else
	{
		ParseError(pTesctx, pLex, "Expected permutation options, but encountered '%s'", PCozCurrentToken(pLex));
	}

	(void)FExpectToken(pTesctx, pLex, TOK(')'));

	// parse child permutations
	if (FConsumeToken(pLex, TOK('+')))
	{
		auto pPermChild = PPermParse(pTesctx, pLex);
		if (!pPermChild)
		{
			ParseError(pTesctx, pLex, "Expected permutation, but encountered '%s'", PCozCurrentToken(pLex));
		}
		else
		{
			pPerm->m_arypPermChild.Append(pPermChild);
		}
	}
	else if (FConsumeToken(pLex, TOK('{')))
	{
		do
		{
			// allow a trailing comma on a list of permutations 
			if (pLex->m_tok == TOK('}'))
				break;

			auto pPermChild = PPermParse(pTesctx, pLex);
			if (!pPermChild)
			{
				ParseError(pTesctx, pLex, "Expected permutation, but encountered '%s'", PCozCurrentToken(pLex));
				break;
			}

			pPerm->m_arypPermChild.Append(pPermChild);
		}
		while (FConsumeToken(pLex, TOK(',')));

		(void) FExpectToken(pTesctx, pLex, TOK('}'));
	}

	return pPerm;
}
static void ParsePermuteString(STestContext * pTesctx, SLexer * pLex, SUnitTest * pUtest)
{
	FExpectToken(pTesctx, pLex, TOK('{'));
	do
	{
		auto pPerm = PPermParse(pTesctx, pLex);
		if (!pPerm)
			break;

		pUtest->m_arypPerm.Append(pPerm);
	}
	while (FConsumeToken(pLex, TOK(',')));

	FExpectToken(pTesctx, pLex, TOK('}'));
}

static SUnitTest * PUtestParse(STestContext * pTesctx, SLexer * pLex)
{
	if (!FConsumeIdentifier(pTesctx, pLex, "test"))
	{
		ParseError(pTesctx, pLex, "Expected 'test' directive, but encountered '%s'", PCozCurrentToken(pLex));
		return nullptr;
	}

	SLexerLocation lexloc(pLex);
	SUnitTest * pUtest = EWC_NEW(pTesctx->m_pAlloc, SUnitTest) SUnitTest(pTesctx->m_pAlloc);

	auto pChzDirective = "test";
	if (pLex->m_str == "builtin")
	{
		TokNext(pLex);
		pUtest->m_utestk = UTESTK_Builtin;
		pChzDirective = "builtin";
	}
	if (pLex->m_tok != TOK_Identifier)
	{
		ParseError(pTesctx, pLex, "Expected test name to follow %s directive, but encountered '%s'", pChzDirective, PCozCurrentToken(pLex));
		return nullptr;
	}


	pUtest->m_strName = pLex->m_str;
	TokNext(pLex);

	while (1)
	{
		if (FConsumeIdentifier(pTesctx, pLex, "prereq"))
		{
			pUtest->m_pCozPrereq = PCozExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, "input"))
		{
			pUtest->m_pCozInput = PCozExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, "parse"))
		{
			pUtest->m_pCozParse = PCozExpectString(pTesctx, pLex);
		}
		else if (FConsumeIdentifier(pTesctx, pLex, "typecheck"))
		{
			pUtest->m_pCozTypeCheck = PCozExpectString(pTesctx, pLex);
		}
		else if (pLex->m_tok == TOK('{'))
		{
			ParsePermuteString(pTesctx, pLex, pUtest);
		}
		else
		{
			if ((pLex->m_tok == TOK_Identifier && pLex->m_str == CString("test")) ||
				pLex->m_tok == TOK_Eof)
			{
				break;
			}

			ParseError(pTesctx, pLex, "unknown token encountered '%s' during test %s", PCozCurrentToken(pLex), pUtest->m_strName.PCoz());
			TokNext(pLex);
			SkipRestOfLine(pLex);
		}
	}

	if (pUtest->m_utestk != UTESTK_Permute)
	{
		if (pUtest->m_pCozPrereq || pUtest->m_pCozInput || pUtest->m_pCozParse || pUtest->m_pCozTypeCheck || pUtest->m_arypPerm.C())
		{
			ParseError(pTesctx, pLex, "permute string test is not allowed for test %s", pUtest->m_strName.PCoz());
		}
	}
	
	return pUtest;
}

void ParseMoetestFile(STestContext * pTesctx, SLexer * pLex, CDynAry<SUnitTest *> * parypUtest)
{
	// load the first token
	TokNext(pLex);

	while (pLex->m_tok != TOK_Eof)
	{
		SUnitTest * pUtest = PUtestParse(pTesctx, pLex);

		if (!pUtest)
		{
			ParseError(pTesctx, pLex, "Unexpected token '%s' in test definition", PCozCurrentToken(pLex));
			break;
		}

		parypUtest->Append(pUtest);
	}
}

void PrintTestError(const char * pCozIn, const char * pCozOut, const char * pCozExpected)
{
	printf("in : %s\n", pCozIn);
	printf("out: %s\n", pCozOut);
	printf("exp: %s\n", pCozExpected);

	printf("   : ");
	auto pChOut = pCozOut;
	auto pChExp = pCozExpected;
	while (*pChOut != '\0' && *pChExp != '\0')
	{
		auto cBOut = CBCodepoint(pChOut);
		auto cBExp = CBCodepoint(pChExp);
		bool fAreSame = cBOut == cBExp;
		if (fAreSame)
		{
			for (int iB = 0; iB < cBOut; ++iB)
			{
				fAreSame &= pChOut[iB] == pChExp[iB];
			}
		}

		printf("%c", (fAreSame) ? ' ' : '^');

		pChOut += cBOut;
		pChExp += cBExp;
	}

	printf("\n\n");
}

bool FCheckForExpectedErrors(SErrorManager * pErrman, ERRID erridMin, ERRID erridMax, TESTRES * pTestres)
{
	auto paryErrcExpected = pErrman->m_paryErrcExpected;
	if (!paryErrcExpected)
		return false;

	int cErrInRange = 0;
	auto pErrcMax = paryErrcExpected->PMac();
	for (auto pErrc = paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
	{
		if (pErrc->m_errid < erridMin || pErrc->m_errid >= erridMax)
			continue;

		++cErrInRange;
		if (pErrc->m_c == 0)
		{
			printf("FAILURE: Missing expected Error(%d)\n", pErrc->m_errid);
			*pTestres = TESTRES_MissingExpectedErr;
		}
	}

	if (cErrInRange)
		return true;
	return false;
}

TESTRES TestresRunUnitTest(
	CWorkspace * pWorkParent,
	SUnitTest * pUtest,
	const char * pCozPrereq,
	const char * pCozIn,
	const char * pCozParseExpected,
	const char * pCozTypeCheckExpected,
	CDynAry<SErrorCount> * paryErrcExpected)
{
	//if (pCozPrereq && pCozPrereq[0] != '\0')
	//	printf("(%s): %s\n%s ", pUtest->m_strName.PCoz(), pCozPrereq, pCozIn);
	//else
	printf("(%s): %s ", pUtest->m_strName.PCoz(), pCozIn);

	if (paryErrcExpected)
	{
		auto pErrcMax = paryErrcExpected->PMac();

		const char * pCozSpacer = "";
		for (auto pErrc = paryErrcExpected->A(); pErrc != pErrcMax; ++pErrc)
		{
			printf("%sErrid(%d)", pCozSpacer, pErrc->m_errid);
			pCozSpacer = ", ";
		}
	}
	printf("\n");

	SErrorManager errmanTest;
	CWorkspace work(pWorkParent->m_pAlloc, &errmanTest);
	errmanTest.m_paryErrcExpected = paryErrcExpected;
	work.CopyUnitTestFiles(pWorkParent);

#ifdef EWC_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	work.m_pAlloc->SetAltrac(pAltrac);
#endif

	BeginWorkspace(&work);

	SStringEditBuffer sebFilename(work.m_pAlloc);
	SStringEditBuffer sebInput(work.m_pAlloc);
	CWorkspace::SFile * pFile = nullptr;

	work.m_globmod = GLOBMOD_UnitTest;

	sebFilename.AppendCoz(pUtest->m_strName.PCoz());
	pFile = work.PFileEnsure(sebFilename.PCoz(), CWorkspace::FILEK_Source);

	size_t cbPrereq = 0;
	if (pCozPrereq)
	{
		sebInput.AppendCoz(pCozPrereq);
		sebInput.AppendCoz("\n");
		cbPrereq = sebInput.CB() - 1; // don't count the null terminator
	}

	if (pCozIn)
	{
		sebInput.AppendCoz(pCozIn);
	}

	pFile->m_pChzFileBody = sebInput.PCoz();

	SLexer lex;
	BeginParse(&work, &lex, sebInput.PCoz(), sebFilename.PCoz());
	work.m_pErrman->Clear();

	// Parse
	ParseGlobalScope(&work, &lex, true);
	EndParse(&work, &lex);

	HideDebugStringForEntries(&work, cbPrereq);

	TESTRES testres = TESTRES_Success;
	if (work.m_pErrman->FHasErrors())
	{
		printf("Unexpected error parsing error during test %s\n", pUtest->m_strName.PCoz());
		printf("input = \"%s\"\n", sebInput.PCoz());
		testres = TESTRES_SourceError;
	}

	
	bool fHasExpectedErr = FCheckForExpectedErrors(&errmanTest, ERRID_Min, ERRID_ParserMax, &testres);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];
	if (!fHasExpectedErr && testres == TESTRES_Success)
	{
		if (!FIsEmptyString(pCozParseExpected))
		{
			WriteDebugStringForEntries(&work, pCh, pChMax, FDBGSTR_Name);

			if (!FAreCozEqual(aCh, pCozParseExpected))
			{
				// print error location
				printf("PARSE ERROR during test for '%s'\n", pUtest->m_strName.PCoz());
				PrintTestError(pCozIn, aCh, pCozParseExpected);
				testres = TESTRES_ParseMismatch;
			}
		}
	}

	if (testres == TESTRES_Success)
	{
		// Type Check
		PerformTypeCheck(work.m_pAlloc, work.m_pErrman, work.m_pSymtab, &work.m_aryEntry, &work.m_aryiEntryChecked, work.m_globmod);
		if (work.m_pErrman->FHasErrors())
		{
			printf("Unexpected error during type check test %s\n", pUtest->m_strName.PCoz());
			printf("input = \"%s\"\n", pCozIn);
		}

		fHasExpectedErr = FCheckForExpectedErrors(&errmanTest, ERRID_TypeCheckMin, ERRID_TypeCheckMax, &testres);

		if (!fHasExpectedErr && testres == TESTRES_Success && !FIsEmptyString(pCozTypeCheckExpected))
		{
			WriteDebugStringForEntries(&work, pCh, pChMax, FDBGSTR_Type | FDBGSTR_LiteralSize | FDBGSTR_NoWhitespace);

			size_t cB = CBCoz(pCozTypeCheckExpected);
			char * aChExpected = (char *)work.m_pAlloc->EWC_ALLOC(cB, 1);
			SwapDoubleHashForPlatformBits(pCozTypeCheckExpected, aChExpected, cB);

			if (!FAreCozEqual(aCh, aChExpected))
			{
				// print error location
				printf("TYPE CHECK ERROR during test for '%s'\n", pUtest->m_strName.PCoz());
				PrintTestError(pCozIn, aCh, aChExpected);
				testres = TESTRES_TypeCheckMismatch;
			}
			work.m_pAlloc->EWC_DELETE(aChExpected);
		}
	}

	if (testres == TESTRES_Success)
	{
		CIRBuilder build(&work, &work.m_arypValManaged, sebFilename.PCoz(), FCOMPILE_None);
		CodeGenEntryPoint(&work, &build, work.m_pSymtab, &work.m_aryEntry, &work.m_aryiEntryChecked);

		if (work.m_pErrman->FHasErrors())
		{
			printf("Unexpected error during codegen for test %s\n", pUtest->m_strName.PCoz());
			testres = TESTRES_CodeGenFailure;
		}
	}

	(void) FCheckForExpectedErrors(&errmanTest, ERRID_CodeGenMin, ERRID_Max, &testres);

	sebFilename.Resize(0, 0, 0);
	sebInput.Resize(0, 0, 0);
	

	if (pFile && pFile->m_pDif)
	{
		work.m_pAlloc->EWC_DELETE(pFile->m_pDif);
		pFile->m_pDif = nullptr;
	}


	pWorkParent->m_pErrman->AddChildErrors(&errmanTest);
	errmanTest.m_aryErrid.Clear();
	EWC_ASSERT(pWorkParent->m_pErrman->m_pWork == pWorkParent, "whaa?");

	EndWorkspace(&work);

#ifdef EWC_TRACK_ALLOCATION
	DeleteAltrac(&allocAltrac, pAltrac);
	work.m_pAlloc->SetAltrac(nullptr);
#endif
	
	return testres;
}


void TestPermutation(STestContext * pTesctx, SPermutation * pPerm, SUnitTest * pUtest)
{
	auto pSub = pTesctx->m_arySubStack.AppendNew();
	pSub->m_strVar = pPerm->m_strVar;
	pSub->m_pCozOption = nullptr;

	char * pCozSub = nullptr;
	auto ppOptMax = pPerm->m_arypOpt.PMac();
	for (auto ppOpt = pPerm->m_arypOpt.A(); ppOpt != ppOptMax; ++ppOpt)
	{
		pSub->m_pOpt = *ppOpt;
		if (pSub->m_pOpt->m_fAllowSubstitution)
		{
			pCozSub = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pSub->m_pOpt->m_pCozOption, pTesctx->m_arySubStack, pSub);
			pSub->m_pCozOption = pCozSub;
		}
		else
		{
			pSub->m_pCozOption = pSub->m_pOpt->m_pCozOption;
		}


		if (pPerm->m_arypPermChild.C())
		{
			auto ppPermChildMax = pPerm->m_arypPermChild.PMac();
			for (auto ppPermChild = pPerm->m_arypPermChild.A(); ppPermChild != ppPermChildMax; ++ppPermChild)
			{
				TestPermutation(pTesctx, *ppPermChild, pUtest);
			}
		}
		else
		{
			CDynAry<SErrorCount> aryErrcExpected(pTesctx->m_pAlloc, BK_UnitTest);
			auto pSubMax = pTesctx->m_arySubStack.PMac();
			for (auto pSubIt = pTesctx->m_arySubStack.A(); pSubIt != pSubMax; ++pSubIt)
			{
				//printf("$%s:%s, ", pSubIt->m_strVar.PCoz(), (pSubIt->m_pCozOption) ? pSubIt->m_pCozOption : "null");

				if (pSubIt->m_pOpt->m_erridExpected != ERRID_Nil)
				{
					aryErrcExpected.Append(pSubIt->m_pOpt->m_erridExpected);
				}
			}

			//printf("\n");


			// error testing is not in yet.
			{
				auto pCozPrereq = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozPrereq, pTesctx->m_arySubStack);
				auto pCozInput = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozInput, pTesctx->m_arySubStack);
				auto pCozParse = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozParse, pTesctx->m_arySubStack);
				auto pCozTypeCheck = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozTypeCheck, pTesctx->m_arySubStack);

				if (!pCozInput || !pCozParse || !pCozTypeCheck)
				{
					printf("... skipping test due to errors\n");
				}
				else
				{
					TESTRES testres = TESTRES_UnitTestFailure;
					testres = TestresRunUnitTest(pTesctx->m_pWork, pUtest, pCozPrereq, pCozInput, pCozParse, pCozTypeCheck, &aryErrcExpected);
					++pTesctx->m_mpTestresCResults[testres];
				}

				if (pCozPrereq) pTesctx->m_pAlloc->EWC_DELETE((char*)pCozPrereq);
				if (pCozInput) pTesctx->m_pAlloc->EWC_DELETE((char*)pCozInput);
				if (pCozParse) pTesctx->m_pAlloc->EWC_DELETE((char*)pCozParse);
				if (pCozTypeCheck) pTesctx->m_pAlloc->EWC_DELETE((char*)pCozTypeCheck);
			}
		}

		if (pCozSub)
		{
			pTesctx->m_pAlloc->EWC_DELETE(pCozSub);
			pCozSub = nullptr;
		}
		pSub->m_pCozOption = nullptr;
	}

	EWC_ASSERT(pTesctx->m_arySubStack.PLast() == pSub, "bad push/pop");
	pTesctx->m_arySubStack.PopLast();
}

bool FRunBuiltinTest(const CString & strName, CAlloc * pAlloc)
{
	bool fReturn;
	if (strName == "Lexer")
	{
		fReturn = FTestLexing();
	}
	else if (strName == "Signed65")
	{
		fReturn = FTestSigned65();
	}
	else if (strName == "Unicode")
	{
		fReturn = FTestUnicode();
	}
	else if (strName == "UniqueNames")
	{
		fReturn = FTestUniqueNames(pAlloc);
	}
	else
	{
		printf("ERROR: Unknown built in test %s\n", strName.PCoz());
		fReturn = false;
	}
	
	return fReturn;
}

void ParseAndTestMoetestFile(EWC::CAlloc * pAlloc, SErrorManager * pErrman, SLexer * pLex)
{
	STestContext tesctx(pAlloc, pErrman, pErrman->m_pWork);
	CDynAry<SUnitTest *> arypUtest(pAlloc, BK_UnitTest);

	ParseMoetestFile(&tesctx, pLex, &arypUtest);
	if (pErrman->FHasErrors())
	{
		int cError, cWarning;
		pErrman->ComputeErrorCounts(&cError, &cWarning);
		printf("Failed parsing unit test file: %d errors, %d warnings\n", cError, cWarning);
		return;
	}
	
	SUnitTest ** ppUtestMax = arypUtest.PMac();
	for (auto ppUtest = arypUtest.A(); ppUtest != ppUtestMax; ++ppUtest)
	{
		SUnitTest * pUtest = *ppUtest;

		if (pUtest->m_utestk == UTESTK_Builtin)
		{
			printf("Built-In %s\n", pUtest->m_strName.PCoz());
			if (!FRunBuiltinTest(pUtest->m_strName, pAlloc))
			{
				printf("Built in test %s Failed\n", pUtest->m_strName.PCoz());
				++tesctx.m_mpTestresCResults[TESTRES_BuiltinFailure];
			}
			else
			{
				++tesctx.m_mpTestresCResults[TESTRES_Success];
			}
			continue;
		}

		if (pUtest->m_arypPerm.FIsEmpty())
		{
			// no permutations, just test it as is.
			TESTRES testres = TESTRES_UnitTestFailure;
			testres = TestresRunUnitTest(tesctx.m_pWork, pUtest, pUtest->m_pCozPrereq, pUtest->m_pCozInput, pUtest->m_pCozParse, pUtest->m_pCozTypeCheck, nullptr);
			++tesctx.m_mpTestresCResults[testres];
			continue;
		}

		auto ppPermMax = pUtest->m_arypPerm.PMac();
		for (auto ppPerm = pUtest->m_arypPerm.A(); ppPerm != ppPermMax; ++ppPerm)
		{
			TestPermutation(&tesctx, *ppPerm, pUtest);
		}
	}

	int cTests = 0;
	for (int testres = TESTRES_Min; testres < TESTRES_Max; ++testres)
	{
		cTests += tesctx.m_mpTestresCResults[testres];
	}

	if (tesctx.m_mpTestresCResults[TESTRES_Success] == cTests)
		printf("\nSUCCESS: ");
	else
		printf("\nFailure: ");

	printf("%d / %d tests succeeded\n", tesctx.m_mpTestresCResults[TESTRES_Success], cTests);
}

bool FUnitTestFile(CWorkspace * pWork, const char * pChzFilenameIn)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	if (!pChzFilenameIn)
		pChzFilenameIn = "";
	
	auto pFile = pWork->PFileEnsure(pChzFilenameIn, CWorkspace::FILEK_UnitTest);
	pFile->m_pChzFileBody = nullptr;

	char aChFilenameOut[CWorkspace::s_cBFilenameMax];
	(void)CChConstructFilename(pFile->m_strFilename.PCoz(), CWorkspace::s_pCozUnitTestExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));

	pFile->m_pChzFileBody = pWork->PChzLoadFile(aChFilenameOut, pWork->m_pAlloc);
	if (!pFile->m_pChzFileBody)
	{
		return false;
	}

	const char * pCozFileBody = PCozSkipUnicodeBOM(pFile->m_pChzFileBody);

	printf("Testing %s\n", pFile->m_strFilename.PCoz());

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->EWC_ALLOC(cChStorage, 4);
	SLexer lex;
	InitLexer(&lex, pCozFileBody, &pCozFileBody[CBCoz(pCozFileBody)-1], aChStorage, cChStorage);
	lex.m_pCozFilename = pFile->m_strFilename.PCoz();

	ParseAndTestMoetestFile(pAlloc, pWork->m_pErrman, &lex);
	return !pWork->m_pErrman->FHasErrors();
}
		