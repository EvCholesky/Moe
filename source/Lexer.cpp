﻿/* Copyright (C) 2015 Evan Christensen
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

#include "Lexer.h"
#include "EwcString.h"
#include "EwcTypes.h"



static int JtokSetTokinf(SLexer * pJlex, JTOK jtok, const char * pChStart, const char * pChEnd)
{
	pJlex->m_jtok = jtok;
	pJlex->m_pChBegin = pChStart;
	pJlex->m_pChEnd = pChEnd;
	pJlex->m_pChParse = pChEnd + 1;

	pJlex->m_litk = LITK_Nil;
	return 1;
}

RWORD RwordFromHv(u32 hv)
{
	// BB - Should switch to hash?

	#define RW(x)
	#define STR(x) EWC::HvFromPCoz(#x)
	u32 s_aHv[] =
	{
		RESERVED_WORD_LIST
	};
	#undef STR
	#undef RW

	for (int iHv = 0; iHv < EWC_DIM(s_aHv); ++iHv)
	{
		if (s_aHv[iHv] == hv)
		{
			return (RWORD)iHv;
		}
	}
	return RWORD_Nil;
}

const char * PCozFromRword(RWORD rword)
{
	if (!EWC_FVERIFY((rword >= RWORD_Nil) & (rword < RWORD_Max), "unknown reserved word"))
		return "(unknown)";

	if (rword <= RWORD_Nil)
		return "nil";

	#define RW(x) 
	#define STR(x) #x
	const char * s_mpRwordPCoz[] =
	{
		RESERVED_WORD_LIST
	};
	#undef STR
	#undef RW

	EWC_CASSERT(EWC_DIM(s_mpRwordPCoz) == RWORD_Max, "missing token string");
	return s_mpRwordPCoz[rword];
}

static int FIsWhitespace(int ch)
{
   return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\f';
}

static const char * PChzStrStr(const char * pChz, int ch)
{
	for (; *pChz; ++pChz) 
	{
		if (*pChz == ch)
			return pChz;
	}
	return nullptr;
}

static inline char NToLower(char c)
{
	if ((c >= 'A') & (c <= 'Z'))
		return c + ('a' - 'A');
	return c;
}

// copy suffixes at the end of a number into the working string
static int JtokParseSuffixes(SLexer * pJlex, JTOK jtok, LITK litk, const char * pChzStart, const char * pChzCur)
{
	pJlex->m_str = EWC::CString();

	int jtokReturn = JtokSetTokinf(pJlex, jtok, pChzStart, pChzCur-1);
	pJlex->m_litk = litk;
	return jtokReturn;
}

static F64 GParse(const char * pChzIn, char const ** pChzOut)
{
	F64 gReturn = 0.0f;
	while ((*pChzIn >= '0') & (*pChzIn <= '9'))
	{
		gReturn = gReturn*10.0f + (*pChzIn++ - '0');
	}

	if (*pChzIn == '.') 
	{
		F64 gPow10 = 1.0f;
		F64 gAddend = 0;
		++pChzIn;
		while ((*pChzIn >= '0') & (*pChzIn <= '9'))
		{
			gAddend = gAddend * 10 + (*pChzIn++ - '0');
			gPow10 *= 10;
		}
		gReturn += gAddend / gPow10;
	}
	if (*pChzIn == 'e' || *pChzIn == 'E') 
	{
		int nSign = pChzIn[1] == '-';
		int fHasSign = (pChzIn[1] == '-') | (pChzIn[1] == '+');
		int nExponent = 0;
		double gPow10 = 10;
		pChzIn += 1+fHasSign;
		while ((*pChzIn >= '0') & (*pChzIn <= '9'))
		{
			nExponent = nExponent*10 + (*pChzIn++ - '0');
		}

		// can't use pow() from stdlib, so do it slow way
		while (--nExponent > 0)
			gPow10 *= 10;

		if (nSign)
			gReturn /= gPow10;
		else
			gReturn *= gPow10;
	}

	*pChzOut = pChzIn;
	return gReturn;
}

static int JtokParseChar(const char * pChzIn, char const ** pChzOut)
{
	if (*pChzIn == '\\') 
	{
		*pChzOut = pChzIn+2; // tentatively parse two characters
		switch(pChzIn[1]) 
		{
			case '\\': return '\\';
			case '\'': return '\'';
			case '"': return '"';
			case 't': return '\t';
			case 'f': return '\f';
			case 'n': return '\n';
			case 'r': return '\r';
			case '0': return '\0';			// @TODO ocatal constants
			case 'x':  // fall through...
			case 'X': return -1;			// @TODO hex constants
			case 'u': return -1;			// @TODO unicode constants
		}
	}
	*pChzOut = pChzIn+1;
	return (unsigned char) *pChzIn;
}

static int JtokParseString(SLexer * pJlex, const char * pChz)
{
	const char * pChzStart = pChz;
	char chDelim = *pChz++; // grab the " or ' for later matching
	char * pChOut = pJlex->m_aChScratch;
	char * pChOutEnd = pJlex->m_aChScratch + pJlex->m_cChScratch;
	while (*pChz != chDelim) 
	{
		int jtok;
		if (*pChz == '\\') 
		{
			const char * pChzNext;
			jtok = JtokParseChar(pChz, &pChzNext);
			if (jtok < 0)
			{
				return JtokSetTokinf(pJlex, JTOK_ParseError, pChzStart, pChzNext);
			}
			pChz = pChzNext;
		} 
		else 
		{
			// @OPTIMIZE: could speed this up by looping-while-not-backslash
			jtok = (unsigned char) *pChz++;
		}
		if (pChOut+1 > pChOutEnd)
		{
			return JtokSetTokinf(pJlex, JTOK_ParseError, pChzStart, pChz);
		}

	      // @TODO expand unicode escapes to UTF8
		*pChOut++ = (char) jtok;
	}

	*pChOut = 0;
	pJlex->m_str = EWC::CString(pJlex->m_aChScratch, pChOut - pJlex->m_aChScratch);

	int jtok = JtokSetTokinf(pJlex, JTOK_Literal, pChzStart, pChz);
	pJlex->m_litk = LITK_String;
	return jtok;
}

static int JtokLexHereString(SLexer * pJlex, const char * pChz)
{
	while (pChz != pJlex->m_pChEof && FIsWhitespace(*pChz))
	{
		++pChz;
	}

	// make sure this is a valid delimiter
	const char * pChzDelim = pChz;
	const char * pChzDelimEnd = pChz;

	while (!FIsWhitespace(*pChzDelimEnd))
	{
		if (pChzDelimEnd == pJlex->m_pChEof)
			return JTOK_ParseError;

		++pChzDelimEnd;
	}

	// make sure it's all whitespace from here to the EOL
	pChz = pChzDelimEnd;
	while (*pChz != '\n')
	{
		if (pChz == pJlex->m_pChEof || !FIsWhitespace(*pChz))
			return JTOK_ParseError;
		++pChz;
	}

	++pChz; // skip the newline
	const char * pChzStart = pChz;

	int cChDelim = pChzDelimEnd - pChzDelim;
	int cChMatch = 0;
	const char * pChzLine = pChz;
	while (1)
	{
		if (pChz == pJlex->m_pChEof)
			return JTOK_ParseError;

		if (pChzLine)
		{
			if (pChzLine[cChMatch] == pChzDelim[cChMatch])
			{
				++cChMatch;
				if (cChMatch >= cChDelim)
					break;
			}
			else
			{
				cChMatch = 0;
				pChzLine = nullptr;
			}
		}

		if (*pChz == '\n')
		{
			pChzLine = pChz + 1;
		}
		++pChz;
	}

	// we've found a match!

	pJlex->m_str = EWC::CString(pChzStart, pChzLine - pChzStart);

	int jtok = JtokSetTokinf(pJlex, JTOK_Literal, pChzStart, pChz);
	pJlex->m_litk = LITK_String;
	return jtok;
}

// method used to split compound tokens (ie '&&' split into two '&' '&' tokens)
void SplitToken(SLexer * pJlex, JTOK jtokSplit)
{
	pJlex->m_jtok = jtokSplit;

	const char * pChzJtok = PCozFromJtok(jtokSplit);
	const char * pChIt = pJlex->m_pChBegin;
	const char * pChEnd = pJlex->m_pChEnd;
	while (pChIt != pChEnd)
	{
		EWC_ASSERT(*pChIt == *pChzJtok, "Split token mismatch");
		if (*pChIt == '\0')
			break;

		++pChIt;
		++pChzJtok;
	}

	pJlex->m_pChEnd = pChIt - 1;
	pJlex->m_pChParse = pChIt;
}

bool FConsumeToken(SLexer * pJlex, JTOK jtok)
{
	if (pJlex->m_jtok == jtok)
	{
		JtokNextToken(pJlex);
		return true;
	}
	return false;
}

int JtokNextToken(SLexer * pJlex)
{
	const char * pChz = pJlex->m_pChParse;

	// skip whitespace and comments
	for (;;) 
	{
		while (pChz != pJlex->m_pChEof && FIsWhitespace(*pChz))
		{
			++pChz;
		}

		// C++ comments, aka. double slash comments
		if (pChz != pJlex->m_pChEof && ((pChz[0] == '/') & (pChz[1] == '/')))
		{
			while (pChz != pJlex->m_pChEof && ((*pChz != '\r') & (*pChz != '\n')))
				++pChz;
		    continue;
		}

		// C comments /* like this */
		if (pChz != pJlex->m_pChEof && ((pChz[0] == '/') & (pChz[1] == '*')))
		{
			const char * pChStart = pChz;
		    pChz += 2;
		    while (pChz != pJlex->m_pChEof && ((pChz[0] != '*') | (pChz[1] != '/')))
		       ++pChz;
		    if (pChz == pJlex->m_pChEof)
		       return JtokSetTokinf(pJlex, JTOK_ParseError, pChStart, pChz-1);
		    pChz += 2;
		    continue;
		}
		break;
	}

	if (pChz == pJlex->m_pChEof)
	{
	   pJlex->m_jtok = JTOK_Eof;
	   return 0;
	}

	switch (*pChz) 
	{
		default:
			if (  ((*pChz >= 'a') & (*pChz <= 'z'))
				| ((*pChz >= 'A') & (*pChz <= 'Z'))
				| (*pChz == '_') 
				| (*pChz == '#')		// allowing identifiers containing '#' to catch directive rwords, parse will error.
				| (u8(*pChz) >= 128))   // >= 128 is UTF8 char
			{
				size_t iCh = 0;
				char * pChzScratch = pJlex->m_aChScratch;
				do 
				{
					if (iCh+1 >= pJlex->m_cChScratch)
						return JtokSetTokinf(pJlex, JTOK_ParseError, pChz, pChz+iCh);

					pChzScratch[iCh] = pChz[iCh];
					++iCh;
				} while ( ((pChz[iCh] >= 'a') & (pChz[iCh] <= 'z'))
						| ((pChz[iCh] >= 'A') & (pChz[iCh] <= 'Z'))
						| ((pChz[iCh] >= '0') & (pChz[iCh] <= '9')) // allow digits in middle of identifier
						| (pChz[iCh] == '_')
						| (u8(pChz[iCh]) >= 128));
				pChzScratch[iCh] = '\0';
				pJlex->m_str = EWC::CString(pChzScratch);

				u32 Hv = EWC::HvFromPCoz(pChz, iCh);
				RWORD rword = RwordFromHv(Hv);
				pJlex->m_rword = rword;

				if (rword != RWORD_Nil)
				{
					if (rword == RWORD_StringDirective)
					{
						return JtokLexHereString(pJlex, pChz+iCh);
					}
					return JtokSetTokinf(pJlex, JTOK_ReservedWord, pChz, pChz+iCh-1);
				}

				return JtokSetTokinf(pJlex, JTOK_Identifier, pChz, pChz+iCh-1);
			}

		single_char:         
			// not an identifier, return the character as itself
			return JtokSetTokinf(pJlex, JTOK(*pChz), pChz, pChz);

		case '+':
		if (pChz+1 != pJlex->m_pChEof) 
		{
			if (pChz[1] == '+') 
				return JtokSetTokinf(pJlex, JTOK_PlusPlus, pChz, pChz+1);
		    if (pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_PlusEqual, pChz, pChz+1);
		} goto single_char;

		case '-':
		if (pChz+1 != pJlex->m_pChEof) 
		{
			if (pChz[1] == '-') 
			{
				if ((pChz+2 != pJlex->m_pChEof) && pChz[2] == '-')
					return JtokSetTokinf(pJlex, JTOK_TripleMinus, pChz, pChz+2);
				return JtokSetTokinf(pJlex, JTOK_MinusMinus, pChz, pChz+1);
			}
			if (pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_MinusEqual, pChz, pChz+1);
			if (pChz[1] == '>') 
				return JtokSetTokinf(pJlex, JTOK_Arrow, pChz, pChz+1);
		} goto single_char;

		case '&':
		 if (pChz+1 != pJlex->m_pChEof) 
		 {
		    if (pChz[1] == '&') 
				return JtokSetTokinf(pJlex, JTOK_AndAnd, pChz, pChz+1);
		    if (pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_AndEqual, pChz, pChz+1);
		 } goto single_char;

		case '|':
		if (pChz+1 != pJlex->m_pChEof) 
		{
		    if (pChz[1] == '|') 
				return JtokSetTokinf(pJlex, JTOK_OrOr, pChz, pChz+1);
		    if (pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_OrEqual, pChz, pChz+1);
		} goto single_char;

		case '=':
		    if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_EqualEqual, pChz, pChz+1);
			goto single_char;

		case '!':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_NotEqual, pChz, pChz+1);
			goto single_char;

		case '^':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_XorEqual, pChz,pChz+1);
			goto single_char;

		case ':':
			if (pChz+1 != pJlex->m_pChEof)
				{
					if (pChz[1] == ':') 
						return JtokSetTokinf(pJlex, JTOK_ColonColon, pChz,pChz+1);
					if (pChz[1] == '=') 
						return JtokSetTokinf(pJlex, JTOK_ColonEqual, pChz,pChz+1);
				}
			goto single_char;

		case '.':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '.') 
				return JtokSetTokinf(pJlex, JTOK_PeriodPeriod, pChz,pChz+1);
			goto single_char;

		case '~':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=') 
				return JtokSetTokinf(pJlex, JTOK_TildeEqual, pChz,pChz+1);
			goto single_char;
			
		case '%':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=')
				return JtokSetTokinf(pJlex, JTOK_ModEqual, pChz, pChz+1);
			goto single_char;

		case '*':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=')
				return JtokSetTokinf(pJlex, JTOK_MulEqual, pChz, pChz+1);
			goto single_char;

		case '/':
			if (pChz+1 != pJlex->m_pChEof && pChz[1] == '=')
				return JtokSetTokinf(pJlex, JTOK_DivEqual, pChz, pChz+1);
			goto single_char;
			
		case '<':
			if (pChz+1 != pJlex->m_pChEof) 
			{
			    if (pChz[1] == '=') 
					return JtokSetTokinf(pJlex, JTOK_LessEqual, pChz, pChz+1);
			    if (pChz[1] == '<') 
                   return JtokSetTokinf(pJlex, JTOK_ShiftLeft, pChz, pChz+1);
			} goto single_char;

		case '>':
			if (pChz+1 != pJlex->m_pChEof) 
			{
			    if (pChz[1] == '=') 
					return JtokSetTokinf(pJlex, JTOK_GreaterEqual, pChz, pChz+1);
			    if (pChz[1] == '>') 
					return JtokSetTokinf(pJlex, JTOK_ShiftRight, pChz, pChz+1);
			 }
			 goto single_char;

		case '"':
			return JtokParseString(pJlex, pChz);

		case '\'':
		//STB_C_LEX_C_SQ_STRINGS(return stb__clex_parse_string(pJlex, pChz, CLEX_sqstring);)
		// single quote chars
		{
		    const char * pChzStart = pChz;
		    pJlex->m_n = JtokParseChar(pChz+1, &pChz);

		    if (pJlex->m_n < 0)
		       return JtokSetTokinf(pJlex, JTOK_ParseError, pChzStart,pChzStart);
		    if (pChz == pJlex->m_pChEof || *pChz != '\'')
		       return JtokSetTokinf(pJlex, JTOK_ParseError, pChzStart, pChz);
			int jtok = JtokSetTokinf(pJlex, JTOK_Literal, pChzStart, pChz);
			pJlex->m_litk = LITK_Char;
			return jtok;
		}
		goto single_char;
		
		case '0':
			// hex ints
		    if (pChz+1 != pJlex->m_pChEof) 
			{
				if (pChz[1] == 'x' || pChz[1] == 'X') 
				{
					const char * pChzNext = pChz+2;
					#ifdef STB__CLEX_use_stdlib
					pJlex->m_n = strtol((char *) pChz, (char **) pChzNext, 16);
					#else
					u64 n = 0;
					while (pChzNext != pJlex->m_pChEof) 
					{
						if		((*pChzNext >= '0') & (*pChzNext <= '9'))	n = n*16 + (*pChzNext - '0');
						else if ((*pChzNext >= 'a') & (*pChzNext <= 'f'))	n = n*16 + (*pChzNext - 'a') + 10;
						else if ((*pChzNext >= 'A') & (*pChzNext <= 'F'))	n = n*16 + (*pChzNext - 'A') + 10;
						else
						    break;
						++pChzNext;
					}
					pJlex->m_n = n;
					#endif
					if (pChzNext == pChz+2)
						return JtokSetTokinf(pJlex, JTOK_ParseError, pChz-2, pChz-1);
					return JtokParseSuffixes(pJlex, JTOK_Literal, LITK_Integer, pChz, pChzNext);
				}
			}

		// fall through
		case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':

		{	// floats
		    const char * pChzNext = pChz;
			while (pChzNext != pJlex->m_pChEof && ((*pChzNext >= '0') & (*pChzNext <= '9')))
		       ++pChzNext;
		    if (pChzNext != pJlex->m_pChEof) 
			{
				if ((*pChzNext == '.') | (*pChzNext == 'e') | (*pChzNext == 'E'))
				{
					#ifdef STB__CLEX_use_stdlib
					pJlex->m_g = strtod((char *) pChz, (char**) &pChzNext);
					#else
					pJlex->m_g = GParse(pChz, &pChzNext);
					#endif

					int jtok = JtokParseSuffixes(pJlex, JTOK_Literal, LITK_Float, pChz, pChzNext);
					return jtok;
				}
			}
		}

		{	// decimal ints
		    const char * pChzNext = pChz;
		    #ifdef STB__CLEX_use_stdlib
		    pJlex->m_n = strtol((char *) pChz, (char **) &pChzNext, 10);
		    #else
		    u64 n = 0;
			while (pChzNext != pJlex->m_pChEof) 
			{
				if ((*pChzNext >= '0') & (*pChzNext <= '9'))
					n = n*10 + (*pChzNext - '0');
				else
					break;
				++pChzNext;
		    }
		    pJlex->m_n = n;
		    #endif

		    return JtokParseSuffixes(pJlex, JTOK_Literal, LITK_Integer, pChz, pChzNext);
		 }
		 goto single_char;
	}
}

void InitLexer(SLexer * pJlex, const char * pCoInput, const char * pCoInputEnd, char * aChStorage, u32 cChStorage)
{
	pJlex->m_pChInput = pCoInput;
	pJlex->m_pChParse = pCoInput;
	pJlex->m_pChEof =  pCoInputEnd;
	pJlex->m_aChScratch = aChStorage;
	pJlex->m_cChScratch = cChStorage;

	pJlex->m_pCozFilename = "unknown filename";
	pJlex->m_pChBegin = nullptr;
	pJlex->m_pChEnd = nullptr;
	pJlex->m_n = 0;
	pJlex->m_g = 0;
	pJlex->m_litk = LITK_Nil;
	pJlex->m_rword = RWORD_Nil;
}

RWORD RwordLookup(SLexer * pJlex)
{
	if (pJlex->m_jtok != JTOK_ReservedWord)
		return RWORD_Nil;
	return pJlex->m_rword;
}

const char * PCozFromJtok(JTOK jtok)
{
	if (!EWC_FVERIFY((jtok >= JTOK_Nil) & (jtok < JTOK_Max), "bad token value"))
		return "(err)";

	if (jtok <= JTOK_Nil)
		return "(nil)";
	if (jtok < JTOK_SimpleMax)
	{
		static char s_aB[JTOK_SimpleMax * 2];
		s_aB[jtok] = (char)jtok;
		s_aB[jtok + 1] = '\0';
		return &s_aB[jtok];
	}

	static const char * s_mpJtokPCoz[] = 
	{
		"(Eof)",
		"(ParseError)",
		"(Literal)",
		"(Identifier)",
		"(ReservedWord)",
		"==",
		"!=",
		"<=",
		">=",
		"&&",
		"||",
		"&=",
		"|=",
		"^=",
		"~=",
		"<<",
		">>",
		"++",
		"--",
		"---",
		"+=",
		"-=",
		"*=",
		"/=",
		"%=",
		"->",
		"::",
		":=",
		"..",
	};
	EWC_CASSERT(EWC_DIM(s_mpJtokPCoz) == JTOK_Max - JTOK_SimpleMax, "missing token string");
	return s_mpJtokPCoz[jtok - JTOK_SimpleMax];
}

const char * PCozCurrentToken(SLexer * pJlex)
{
	JTOK jtok = (JTOK)pJlex->m_jtok;
	if (jtok == JTOK_ReservedWord)
		return PCozFromRword(pJlex->m_rword);

	return PCozFromJtok(jtok);
}

#define JLEX_TEST
#ifdef JLEX_TEST
void AssertMatches(
	const char * pCozInput, 
	const JTOK * aJtok, 
	const int * aN = nullptr, 
	const F64 * aG = nullptr,
	const char * apCoz[] = nullptr,
	const RWORD * aRword = nullptr,
	const LITK * aLitk = nullptr)
{
	SLexer jlex;
	char aChStorage[1024 * 8];
	InitLexer(&jlex, pCozInput, &pCozInput[EWC::CBCoz(pCozInput)-1], aChStorage, EWC_DIM(aChStorage));
	
	int iJtok = 0;
	const JTOK * pJtok = aJtok;
	while (JtokNextToken(&jlex)) 
	{
		if (pJtok && *pJtok == JTOK_Nil)
			pJtok = nullptr;
		EWC_ASSERT(!pJtok || jlex.m_jtok == *pJtok, "lexed token doesn't match expected");
		++pJtok;

		if (apCoz && apCoz[iJtok] == nullptr)
			apCoz = nullptr;

		bool fIsStringLiteral = jlex.m_jtok == JTOK_Literal && jlex.m_litk == LITK_String;
		if (apCoz && (jlex.m_jtok == JTOK_Identifier || fIsStringLiteral))
		{
			EWC_ASSERT(
				EWC::CBCoz(apCoz[iJtok]) == jlex.m_str.CB(), 
				"lexed string length doesn't match expected value");
			EWC_ASSERT(
				EWC::FAreCozEqual(apCoz[iJtok], jlex.m_str.PCoz(), jlex.m_str.CCodepoint()), 
				"lexed string doesn't match expected value");
		}

		bool fIsIntLiteral = jlex.m_jtok == JTOK_Literal && jlex.m_litk == LITK_Integer;
		if (aN && fIsIntLiteral)
		{
			EWC_ASSERT(jlex.m_n == aN[iJtok], "integer literal value doesn't match expected"); 
		}

		bool fIsFloatLiteral = jlex.m_jtok == JTOK_Literal && jlex.m_litk == LITK_Float;
		if (aG && fIsFloatLiteral)
		{
			EWC_ASSERT(jlex.m_g == aG[iJtok], "float literal value doesn't match expected"); 
		}

		if (aLitk)
		{
			EWC_ASSERT(aLitk[iJtok] == jlex.m_litk, "Literal type mismatch");
		}

		if (aRword && jlex.m_jtok == JTOK_ReservedWord)
		{
			EWC_ASSERT(jlex.m_rword == aRword[iJtok], "reserved word doesn't match expected"); 
		}

		++iJtok;
	}
}

void TestLexing()
{
	u8 aBString[1024 * 100];
	EWC::CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	const char * s_pChz = u8"😁+✂";
	const JTOK s_aJtokEmoji[] = {	
										JTOK_Identifier, JTOK('+'),
										JTOK_Identifier, JTOK(';'),
										JTOK_Nil};

	AssertMatches(s_pChz, s_aJtokEmoji);

	const char * s_pChzLitString = " 'f'; \"foo\"; ";
	const JTOK s_aJtokLitString[] = {	
										JTOK_Literal, JTOK(';'),
										JTOK_Literal, JTOK(';'),
										JTOK_Nil};
	AssertMatches(s_pChzLitString, s_aJtokLitString);

	const char * s_pChzComparisons = "< > <= >= != ==";
	const JTOK s_aJtokComparisons[] = {	JTOK('<'), JTOK('>'), 
										JTOK_LessEqual, JTOK_GreaterEqual, 
										JTOK_NotEqual, JTOK_EqualEqual, 
										JTOK_Nil};
	AssertMatches(s_pChzComparisons, s_aJtokComparisons);

	const char * s_pChzBitwise = "& && &= | || |= ^ ^= ~ ~=";
	const JTOK s_aJtokBitwise[] = {	JTOK('&'), JTOK_AndAnd, JTOK_AndEqual, 
									JTOK('|'), JTOK_OrOr, JTOK_OrEqual, 
									JTOK('^'), JTOK_XorEqual, 
									JTOK('~'), JTOK_TildeEqual, 
									JTOK_Nil};
	AssertMatches(s_pChzBitwise, s_aJtokBitwise);

	const char * s_pChzOperator = "+ += - -= * *= / /= % %= << >>";
	const JTOK s_aJtokOperator[] = {	JTOK('+'), JTOK_PlusEqual, JTOK('-'), JTOK_MinusEqual, 
										JTOK('*'), JTOK_MulEqual, JTOK('/'), JTOK_DivEqual,
										JTOK('%'), JTOK_ModEqual,
										JTOK_ShiftLeft, JTOK_ShiftRight,
										JTOK_Nil };
	AssertMatches(s_pChzOperator, s_aJtokOperator);

	const char * s_pChzMisc = "foo:=bar->guh.wut;";
	const JTOK s_aJtokMisc[] = {JTOK_Identifier, JTOK_ColonEqual, 
								JTOK_Identifier, JTOK_Arrow,
								JTOK_Identifier, JTOK('.'),
								JTOK_Identifier, JTOK(';'),
								JTOK_Nil };
	const char * s_apChzMiscStrings[] = {"foo", "", "bar", "", "guh", "", "wut", "", nullptr};
	AssertMatches(s_pChzMisc, s_aJtokMisc, nullptr, nullptr, s_apChzMiscStrings);

	const char * s_pChzNum = "2.456,5,7.3,-1,0xFFFF, 12.34e12 12.34e-12";
	const JTOK s_aJtokNum[] = {JTOK_Literal, JTOK(','), 
								JTOK_Literal, JTOK(','), 
								JTOK_Literal, JTOK(','), 
								JTOK('-'), JTOK_Literal, JTOK(','),
								JTOK_Literal, JTOK(','),
								JTOK_Literal, JTOK_Literal,
								JTOK_Nil };
	const LITK s_aLitk[] = {	LITK_Float, LITK_Nil,
								LITK_Integer, LITK_Nil,
										LITK_Float, LITK_Nil,
										LITK_Nil, LITK_Integer, LITK_Nil,
										LITK_Integer, LITK_Nil,
										LITK_Float,
										LITK_Float
									};
										
	const int s_aNNum[] = {0,	  0, 5, 0, 0,   0, 0, 1, 0, 65535, 0, 0,        0};	// negative integer literal comes through as two tokens
	const F64 s_aGNum[] = {2.456, 0, 0, 0, 7.3, 0, 0, 1, 0, 0,     0, 12.34e12, 12.34e-12};
	static_assert(EWC_DIM(s_aJtokNum)-1 == EWC_DIM(s_aNNum), "s_aNNum size mismatch");
	static_assert(EWC_DIM(s_aJtokNum)-1 == EWC_DIM(s_aGNum), "s_aGNum size mismatch");

	AssertMatches(s_pChzNum, s_aJtokNum, s_aNNum, s_aGNum, nullptr, nullptr, s_aLitk);

	const char * s_pChzRword = "for:=new.if\nelse while SOA";
	const JTOK s_aJtokRword[] = {JTOK_ReservedWord, JTOK_ColonEqual, 
								JTOK_ReservedWord, JTOK('.'),
								JTOK_ReservedWord, JTOK_ReservedWord,
								JTOK_ReservedWord, JTOK_ReservedWord,
								JTOK_Nil };
	const RWORD s_apRword[] = {	RWORD_For, RWORD_Nil,
								RWORD_New, RWORD_Nil,	
								RWORD_If, RWORD_Else,
								RWORD_While, RWORD_Soa};

	AssertMatches(s_pChzRword, s_aJtokRword, nullptr, nullptr, nullptr, s_apRword);

	const char * s_pChzTriple = ". .. ... - -- --- : ::";
	const JTOK s_aJtokTriple[] = {JTOK('.'), JTOK_PeriodPeriod, 
								JTOK_PeriodPeriod, JTOK('.'),
								JTOK('-'), JTOK_MinusMinus,
								JTOK_TripleMinus, 
								JTOK(':'), JTOK_ColonColon,
								JTOK_Nil };

	AssertMatches(s_pChzTriple, s_aJtokTriple);

	StaticShutdownStrings(&allocString);
}
#else // JLEX_TEST
void TestLexing()
{
}
#endif // JLEX_TEST