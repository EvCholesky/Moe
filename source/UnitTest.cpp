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

struct SUnitTest // tag = utest
{
							SUnitTest(CAlloc * pAlloc)
							:m_strName()
							,m_pCozPrereq(nullptr)
							,m_pCozInput(nullptr)
							,m_pCozParse(nullptr)
							,m_pCozTypeCheck(nullptr)
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
	TESTRES_ParseMismatch,		
	TESTRES_TypeCheckMismatch,
	TESTRES_CodeGenFailure,
	
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
	EmitError(pTesctx->m_pErrman, pLexloc, pChzFormat, ap);
}

void ParseError(STestContext * pTesctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitError(pTesctx->m_pErrman, &lexloc, pChzFormat, ap);
}

void ParseWarning(STestContext * pTesctx, SLexer * pLex, const char * pChzFormat, ...)
{
	SLexerLocation lexloc(pLex);
	va_list ap;
	va_start(ap, pChzFormat);
	EmitWarning(pTesctx->m_pErrman, &lexloc, pChzFormat, ap);
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
			case '"':	pSeb->AppendCoz("\r");	break;
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
	const CSubStack & arySub)
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

			seb.AppendCoz(pSub->m_pCozOption);
			pCozIt = pCozEnd;
		}
		else
		{
			pCozIt += seb.CBAppendCodepoint(pCozIt);
		}
	}

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

	if (pLex->m_tok != TOK_Identifier)
	{
		ParseError(pTesctx, pLex, "Expected test name to follow test directive, but encountered '%s'", PCozCurrentToken(pLex));
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
		else if (FConsumeIdentifier(pTesctx, pLex, "permute"))
		{
			ParsePermuteString(pTesctx, pLex, pUtest);
		}
		else
		{
			break;
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

TESTRES TestresRunUnitTest(
	CWorkspace * pWork,
	SUnitTest * pUtest,
	const char * pCozPrereq,
	const char * pCozIn,
	const char * pCozParseExpected,
	const char * pCozTypeCheckExpected)
{

#ifdef EWC_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	pWork->m_pAlloc->SetAltrac(pAltrac);
#endif

	BeginWorkspace(pWork);

	SStringEditBuffer sebFilename(pWork->m_pAlloc);
	SStringEditBuffer sebInput(pWork->m_pAlloc);
	CWorkspace::SFile * pFile = nullptr;

	pWork->m_globmod = GLOBMOD_UnitTest;

	sebFilename.AppendCoz(pUtest->m_strName.PCoz());
	pFile = pWork->PFileEnsure(sebFilename.PCoz(), CWorkspace::FILEK_Source);

	if (pCozPrereq)
	{
		sebInput.AppendCoz(pCozPrereq);
		sebInput.AppendCoz("\n");
	}

	size_t cbPrereq = sebInput.CB() - 1; // don't count the null terminator
	if (pCozIn)
	{
		sebInput.AppendCoz(pCozIn);
	}

	pFile->m_pChzFileBody = sebInput.PCoz();

	SLexer lex;
	BeginParse(pWork, &lex, sebInput.PCoz(), sebFilename.PCoz());
	pWork->m_pErrman->Clear();

	// Parse
	ParseGlobalScope(pWork, &lex, true);
	EndParse(pWork, &lex);

	HideDebugStringForEntries(pWork, cbPrereq);

	TESTRES testres = TESTRES_Success;
	if (pWork->m_pErrman->m_cError)
	{
		printf("Unexpected error parsing error during test %s\n", pUtest->m_strName.PCoz());
		printf("input = \"%s\"\n", sebInput.PCoz());
		testres = TESTRES_SourceError;
	}

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];
	if (testres == TESTRES_Success)
	{
		if (!FIsEmptyString(pCozParseExpected))
		{
			WriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Name);

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
		PerformTypeCheck(pWork->m_pAlloc, pWork->m_pErrman, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked, pWork->m_globmod);
		if (pWork->m_pErrman->m_cError)
		{
			printf("Unexpected error during type check test %s\n", pUtest->m_strName.PCoz());
			printf("input = \"%s\"\n", pCozIn);
		}

		if (!FIsEmptyString(pCozTypeCheckExpected))
		{
			WriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type | FDBGSTR_LiteralSize | FDBGSTR_NoWhitespace);

			size_t cB = CBCoz(pCozTypeCheckExpected);
			char * aChExpected = (char *)pWork->m_pAlloc->EWC_ALLOC(cB, 1);
			SwapDoubleHashForPlatformBits(pCozTypeCheckExpected, aChExpected, cB);

			if (!FAreCozEqual(aCh, aChExpected))
			{
				// print error location
				printf("TYPE CHECK ERROR during test for '%s'\n", pUtest->m_strName.PCoz());
				PrintTestError(pCozIn, aCh, aChExpected);
				testres = TESTRES_TypeCheckMismatch;
			}
			pWork->m_pAlloc->EWC_DELETE(aChExpected);
		}
	}

	if (testres == TESTRES_Success)
	{
		CIRBuilder build(pWork, &pWork->m_arypValManaged, sebFilename.PCoz(), FCOMPILE_None);
		CodeGenEntryPoint(pWork, &build, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

		if (pWork->m_pErrman->m_cError)
		{
			printf("Unexpected error during codegen for test %s\n", pUtest->m_strName.PCoz());
			testres = TESTRES_CodeGenFailure;
		}
	}
	sebFilename.Resize(0, 0, 0);
	sebInput.Resize(0, 0, 0);
	

	if (pFile && pFile->m_pDif)
	{
		pWork->m_pAlloc->EWC_DELETE(pFile->m_pDif);
		pFile->m_pDif = nullptr;
	}

	EndWorkspace(pWork);

#ifdef EWC_TRACK_ALLOCATION
	DeleteAltrac(&allocAltrac, pAltrac);
	pWork->m_pAlloc->SetAltrac(nullptr);
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
			pCozSub = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pSub->m_pOpt->m_pCozOption, pTesctx->m_arySubStack);
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
			bool fHasError = false;

			auto pSubMax = pTesctx->m_arySubStack.PMac();
			for (auto pSubIt = pTesctx->m_arySubStack.A(); pSubIt != pSubMax; ++pSubIt)
			{
				//printf("$%s:%s, ", pSubIt->m_strVar.PCoz(), (pSubIt->m_pCozOption) ? pSubIt->m_pCozOption : "null");

				if (pSubIt->m_pOpt->m_erridExpected != ERRID_Nil)
				{
				//	printf("ERRID(%d) ", pSubIt->m_pOpt->m_erridExpected);
					fHasError = true;
				}
			}

			//printf("\n");

			// error testing is not in yet.
			if (!fHasError)
			{
				auto pCozPrereq = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozPrereq, pTesctx->m_arySubStack);
				auto pCozInput = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozInput, pTesctx->m_arySubStack);
				auto pCozParse = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozParse, pTesctx->m_arySubStack);
				auto pCozTypeCheck = PCozAllocateSubstitution(pTesctx, &pPerm->m_lexloc, pUtest->m_pCozTypeCheck, pTesctx->m_arySubStack);

				printf("(%s): %s\n", pUtest->m_strName.PCoz(), pCozInput);
				//printf("    par: %s\n", pCozParse);
				//printf("    typ: %s\n", pCozTypeCheck);

				TESTRES testres = TESTRES_UnitTestFailure;
				if (!pCozInput || !pCozParse || !pCozTypeCheck)
				{
					printf("... skipping test due to errors\n");
				}
				else
				{
					SErrorManager errmanTest(pTesctx->m_pWork);
					CWorkspace workChild(pTesctx->m_pWork->m_pAlloc, &errmanTest);
					workChild.CopyUnitTestFiles(pTesctx->m_pWork);
					testres = TestresRunUnitTest(&workChild, pUtest, pCozPrereq, pCozInput, pCozParse, pCozTypeCheck);
					pTesctx->m_pErrman->AddChildErrors(&errmanTest);

					pTesctx->m_pErrman->m_pWork = pTesctx->m_pWork;
				}
				++pTesctx->m_mpTestresCResults[testres];

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

void ParseAndTestMoetestFile(EWC::CAlloc * pAlloc, SErrorManager * pErrman, SLexer * pLex)
{
	STestContext tesctx(pAlloc, pErrman, pErrman->m_pWork);
	CDynAry<SUnitTest *> arypUtest(pAlloc, BK_UnitTest);

	ParseMoetestFile(&tesctx, pLex, &arypUtest);
	
	SUnitTest ** ppUtestMax = arypUtest.PMac();
	for (auto ppUtest = arypUtest.A(); ppUtest != ppUtestMax; ++ppUtest)
	{
		SUnitTest * pUtest = *ppUtest;

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
	printf("%d / %d tests succeeded\n", tesctx.m_mpTestresCResults[TESTRES_Success], cTests);
}

bool FUnitTestFile(CWorkspace * pWork, const char * pChzFilenameIn)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	auto pFile = pWork->PFileEnsure(pChzFilenameIn, CWorkspace::FILEK_UnitTest);

	char aChFilenameOut[CWorkspace::s_cBFilenameMax];
	(void)CChConstructFilename(pFile->m_strFilename.PCoz(), CWorkspace::s_pCozUnitTestExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));

	pFile->m_pChzFileBody = pWork->PChzLoadFile(aChFilenameOut, pWork->m_pAlloc);
	if (!pFile->m_pChzFileBody)
		return false;

	const char * pCozFileBody = PCozSkipUnicodeBOM(pFile->m_pChzFileBody);

	printf("Testing %s\n", pFile->m_strFilename.PCoz());

	static const size_t cChStorage = 1024 * 8;
	char * aChStorage = (char *)pAlloc->EWC_ALLOC(cChStorage, 4);
	SLexer lex;
	InitLexer(&lex, pCozFileBody, &pCozFileBody[CBCoz(pCozFileBody)-1], aChStorage, cChStorage);
	lex.m_pCozFilename = pFile->m_strFilename.PCoz();

	ParseAndTestMoetestFile(pAlloc, pWork->m_pErrman, &lex);
	return pWork->m_pErrman->m_cError == 0;
}
		