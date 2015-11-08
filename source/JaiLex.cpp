#include "JaiLex.h"
#include "EwcString.h"
#include "EwcTypes.h"

namespace EWC
{


static int JtokSetTokinf(SJaiLexer * pJlex, JTOK jtok, const char * pChStart, const char * pChEnd)
{
	if (jtok == JTOK_ParseError)
		DoNothing();

	pJlex->m_jtok = jtok;
	pJlex->m_pChBegin = pChStart;
	pJlex->m_pChEnd = pChEnd;
	pJlex->m_pChParse = pChEnd + 1;

	pJlex->m_litty.m_litk = LITK_Nil;
	pJlex->m_litty.m_litsign = LITSIGN_Nil;
	pJlex->m_litty.m_litsize = LITSIZE_Nil;
	return 1;
}

RWORD RwordFromHv(U32 hv)
{
	// BB - Should switch to hash?

	#define RW(x)
	#define STR(x) HvFromPchz(#x)
	U32 s_aHv[] =
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

const char * PChzFromRword(RWORD rword)
{
	if (!EWC_FVERIFY((rword >= RWORD_Nil) & (rword < RWORD_Max), "unknown reserved word"))
		return "(unknown)";

	if (rword <= RWORD_Nil)
		return "nil";

	#define RW(x) 
	#define STR(x) #x
	const char * s_mpRwordPchz[] =
	{
		RESERVED_WORD_LIST
	};
	#undef STR
	#undef RW

	EWC_CASSERT(EWC_DIM(s_mpRwordPchz) == RWORD_Max, "missing token string");
	return s_mpRwordPchz[rword];
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
static int JtokParseSuffixes(SJaiLexer * pJlex, JTOK jtok, LITK litk, const char * pChzStart, const char * pChzCur)
{
	pJlex->m_pChString = pJlex->m_aChStorage;
	pJlex->m_cChString = 0;

	// don't know the suffix format so now I'm going with 
	//	s,u			signed/unsigned 
	//	b,h,w,d		byte,half(2bytes),word(4bytes),double(8bytes)

	// would it be better to use types?
	// 5s8 (5 : s8) 200s16 (200 : s16) etc... very understandable, less readable...

	// Note: JBlow says literal suffixes shouldn't be needed for Jai, I'll leave them here until I'm sure.


	SLiteralType litty;
	litty.m_litk = litk;
	litty.m_litsign = LITSIGN_Nil;
	litty.m_litsize = LITSIZE_Nil;

	while (((*pChzCur >= 'a') & (*pChzCur <= 'z')) | ((*pChzCur >= 'A') & (*pChzCur <= 'Z'))) 
	{
		switch (NToLower(*pChzCur))
		{
			case 's': litty.m_litsign = LITSIGN_Signed;		break;
			case 'u': litty.m_litsign = LITSIGN_Unsigned;	break;
			case 'b': litty.m_litsize = LITSIZE_8;		    break;
			case 'h': litty.m_litsize = LITSIZE_16;		    break;
			case 'w': litty.m_litsize = LITSIZE_32;		    break;
			case 'd': litty.m_litsize = LITSIZE_64;	        break;
			default: 
				return JtokSetTokinf(pJlex, JTOK_ParseError, pChzStart, pChzCur);
		}

		if (pJlex->m_cChString+1 >= pJlex->m_cChStorage)
			return JtokSetTokinf(pJlex, JTOK_ParseError, pChzStart, pChzCur);
		pJlex->m_pChString[pJlex->m_cChString++] = *pChzCur++;
	}

	int jtokReturn = JtokSetTokinf(pJlex, jtok, pChzStart, pChzCur-1);
	pJlex->m_litty = litty;
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
		int nExponent = 0;
		double gPow10 = 10;
		pChzIn += 1+nSign;
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

static int JtokParseString(SJaiLexer * pJlex, const char * pChz)
{
	const char * pChzStart = pChz;
	char chDelim = *pChz++; // grab the " or ' for later matching
	char * pChOut = pJlex->m_aChStorage;
	char * pChOutEnd = pJlex->m_aChStorage + pJlex->m_cChStorage;
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
	pJlex->m_pChString = pJlex->m_aChStorage;
	pJlex->m_cChString = pChOut - pJlex->m_aChStorage;

	int jtok = JtokSetTokinf(pJlex, JTOK_Literal, pChzStart, pChz);
	pJlex->m_litty.m_litk = LITK_String;
	return jtok;
}

int JtokNextToken(SJaiLexer * pJlex)
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
				| (U8(*pChz) >= 128))    // >= 128 is UTF8 char
			{
				int iCh = 0;
				pJlex->m_pChString = pJlex->m_aChStorage;
				do 
				{
					if (iCh+1 >= pJlex->m_cChStorage)
						return JtokSetTokinf(pJlex, JTOK_ParseError, pChz, pChz+iCh);

					pJlex->m_pChString[iCh] = pChz[iCh];
					++iCh;
				} while ( ((pChz[iCh] >= 'a') & (pChz[iCh] <= 'z'))
						| ((pChz[iCh] >= 'A') & (pChz[iCh] <= 'Z'))
						| ((pChz[iCh] >= '0') & (pChz[iCh] <= '9')) // allow digits in middle of identifier
						| (pChz[iCh] == '_')
						| (U8(pChz[iCh]) >= 128));
				pJlex->m_pChString[iCh] = 0;
				pJlex->m_cChString = iCh;

				U32 Hv = HvFromPchz(pChz, iCh);
				RWORD rword = RwordFromHv(Hv);
				pJlex->m_rword = rword;

				return JtokSetTokinf(pJlex, (rword == RWORD_Nil) ? JTOK_Identifier : JTOK_ReservedWord, pChz, pChz+iCh-1);
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
			int jtok = JtokSetTokinf(pJlex, JTOK_Literal, pChzStart, pChz+1);
			pJlex->m_litty.m_litk = LITK_Char;
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
					int n = 0;
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
					return JtokParseSuffixes(pJlex, JTOK_Literal, LITK_Int, pChz, pChzNext);
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

					if (pJlex->m_litty.m_litsign != LITSIGN_Nil)
					{
						// floats are always signed
						return JtokSetTokinf(pJlex, JTOK_ParseError, pChz, pChzNext);
					}
	
					if (pJlex->m_litty.m_litsize != LITSIZE_Nil && pJlex->m_litty.m_litsize < LITSIZE_32)
					{
						return JtokSetTokinf(pJlex, JTOK_ParseError, pChz, pChzNext);
					}
					return jtok;
				}
			}
		}

		{	// decimal ints
		    const char * pChzNext = pChz;
		    #ifdef STB__CLEX_use_stdlib
		    pJlex->m_n = strtol((char *) pChz, (char **) &pChzNext, 10);
		    #else
		    int n = 0;
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

		    return JtokParseSuffixes(pJlex, JTOK_Literal, LITK_Int, pChz, pChzNext);
		 }
		 goto single_char;
	}
}

void InitJaiLexer(SJaiLexer * pJlex, const char * pChInput, const char * pChInputEnd, char * aChStorage, int cChStorage)
{
	pJlex->m_pChInput = pChInput;
	pJlex->m_pChParse = pChInput;
	pJlex->m_pChEof =  pChInputEnd;
	pJlex->m_aChStorage = aChStorage;
	pJlex->m_cChStorage = cChStorage;

	pJlex->m_pChzFilename = "unknown filename";
	pJlex->m_pChBegin = nullptr;
	pJlex->m_pChEnd = nullptr;
	pJlex->m_n = 0;
	pJlex->m_g = 0;
	pJlex->m_litty.m_litk = LITK_Nil;
	pJlex->m_litty.m_litsign = LITSIGN_Nil;
	pJlex->m_litty.m_litsize = LITSIZE_Nil;
	pJlex->m_rword = RWORD_Nil;
}

RWORD RwordLookup(SJaiLexer * pJlex)
{
	if (pJlex->m_jtok != JTOK_ReservedWord)
		return RWORD_Nil;
	return pJlex->m_rword;
}

const char * PChzFromJtok(JTOK jtok)
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

	static const char * s_mpJtokPchz[] = 
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
	EWC_CASSERT(EWC_DIM(s_mpJtokPchz) == JTOK_Max - JTOK_SimpleMax, "missing token string");
	return s_mpJtokPchz[jtok - JTOK_SimpleMax];
}

#define JLEX_TEST
#ifdef JLEX_TEST
void AssertMatches(
	const char * pChzInput, 
	const JTOK * aJtok, 
	const int * aN = nullptr, 
	const F64 * aG = nullptr,
	const char * apChz[] = nullptr,
	const RWORD * aRword = nullptr,
	const SLiteralType * aLitty = nullptr)
{
	SJaiLexer jlex;
	char aChStorage[1024 * 8];
	InitJaiLexer(&jlex, pChzInput, &pChzInput[CCh(pChzInput)], aChStorage, EWC_DIM(aChStorage));
	
	int iJtok = 0;
	const JTOK * pJtok = aJtok;
	while (JtokNextToken(&jlex)) 
	{
		if (pJtok && *pJtok == JTOK_Nil)
			pJtok = nullptr;
		EWC_ASSERT(!pJtok || jlex.m_jtok == *pJtok, "lexed token doesn't match expected");
		++pJtok;

		if (apChz && apChz[iJtok] == nullptr)
			apChz = nullptr;

		bool fIsStringLiteral = jlex.m_jtok == JTOK_Literal && jlex.m_litty.m_litk == LITK_String;
		if (apChz && jlex.m_jtok == JTOK_Identifier || fIsStringLiteral)
		{
			EWC_ASSERT(
				CCh(apChz[iJtok]) == jlex.m_cChString, 
				"lexed string length doesn't match expected value");
			EWC_ASSERT(
				FAreSame(apChz[iJtok], jlex.m_pChString, jlex.m_cChString), 
				"lexed string doesn't match expected value");
			EWC_ASSERT(jlex.m_litty.m_litsize == LITSIZE_Nil && jlex.m_litty.m_litsign == LITSIGN_Nil, "errant literal type");
		}

		bool fIsIntLiteral = jlex.m_jtok == JTOK_Literal && jlex.m_litty.m_litk == LITK_Int;
		if (aN && fIsIntLiteral)
		{
			EWC_ASSERT(jlex.m_n == aN[iJtok], "integer literal value doesn't match expected"); 
		}

		bool fIsFloatLiteral = jlex.m_jtok == JTOK_Literal && jlex.m_litty.m_litk == LITK_Float;
		if (aG && fIsFloatLiteral)
		{
			EWC_ASSERT(jlex.m_g == aG[iJtok], "float literal value doesn't match expected"); 
			EWC_ASSERT(
				(jlex.m_litty.m_litsize == LITSIZE_Nil || jlex.m_litty.m_litsize >= LITSIZE_32) &&
				jlex.m_litty.m_litsign == LITSIGN_Nil, "errant literal type");
		}

		if (aLitty && (fIsIntLiteral || fIsFloatLiteral))
		{
			EWC_ASSERT(aLitty[iJtok].m_litk == jlex.m_litty.m_litk, "Literal type mismatch");
			EWC_ASSERT(aLitty[iJtok].m_litsize == jlex.m_litty.m_litsize, "Literal type mismatch");
			EWC_ASSERT(aLitty[iJtok].m_litsign == jlex.m_litty.m_litsign, "Literal type mismatch");
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

	const char * s_pChzNum = "2.456,5ub,7.3D,-1,0xFFFF, 12.34e12 12.34e-12";
	const JTOK s_aJtokNum[] = {JTOK_Literal, JTOK(','), 
								JTOK_Literal, JTOK(','), 
								JTOK_Literal, JTOK(','), 
								JTOK('-'), JTOK_Literal, JTOK(','),
								JTOK_Literal, JTOK(','),
								JTOK_Literal, JTOK_Literal,
								JTOK_Nil };
	const SLiteralType s_aLitty[] = {	SLiteralType(LITK_Float, LITSIGN_Nil, LITSIZE_Nil), SLiteralType(),
										SLiteralType(LITK_Int, LITSIGN_Unsigned, LITSIZE_8), SLiteralType(),
										SLiteralType(LITK_Float, LITSIGN_Nil, LITSIZE_64), SLiteralType(),
										SLiteralType(), SLiteralType(LITK_Int, LITSIGN_Nil, LITSIZE_Nil), SLiteralType(),
										SLiteralType(LITK_Int, LITSIGN_Nil, LITSIZE_Nil), SLiteralType(),
										SLiteralType(LITK_Float, LITSIGN_Nil, LITSIZE_Nil), 
										SLiteralType(LITK_Float, LITSIGN_Nil, LITSIZE_Nil),
									};
										
	const int s_aNNum[] = {0,	  0, 5, 0, 0,   0, 0, 1, 0, 65535, 0, 0,        0};	// negative integer literal comes through as two tokens
	const F64 s_aGNum[] = {2.456, 0, 0, 0, 7.3, 0, 0, 1, 0, 0,     0, 12.34e12, 12.34e-12};
	static_assert(EWC_DIM(s_aJtokNum)-1 == EWC_DIM(s_aNNum), "s_aNNum size mismatch");
	static_assert(EWC_DIM(s_aJtokNum)-1 == EWC_DIM(s_aGNum), "s_aGNum size mismatch");

	AssertMatches(s_pChzNum, s_aJtokNum, s_aNNum, s_aGNum, nullptr, nullptr, s_aLitty);

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
}
#else // JLEX_TEST
void TestLexing()
{
}
#endif // JLEX_TEST

} // namespace EWC