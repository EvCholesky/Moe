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

#pragma once

#include "EwcTypes.h"
#include "EwcString.h"

enum JTOK
{
	// don't define single char tokens (they are just ascii codepoints)
	JTOK_Eof = 256,
	JTOK_ParseError,
	JTOK_Literal,
	JTOK_Identifier,
	JTOK_ReservedWord,
	JTOK_EqualEqual,
	JTOK_NotEqual,
	JTOK_LessEqual,
	JTOK_GreaterEqual,
	JTOK_AndAnd,
	JTOK_OrOr,
	JTOK_AndEqual,
	JTOK_OrEqual,
	JTOK_XorEqual,
	JTOK_TildeEqual,
	JTOK_ShiftLeft,
	JTOK_ShiftRight,
	JTOK_PlusPlus,
	JTOK_MinusMinus,
	JTOK_TripleMinus,
	JTOK_PlusEqual,
	JTOK_MinusEqual,
	JTOK_MulEqual,
	JTOK_DivEqual,
	JTOK_ModEqual,
	JTOK_Arrow,
	JTOK_ColonColon,
	JTOK_ColonEqual,
	JTOK_PeriodPeriod,

	JTOK_Max,
	JTOK_Min = 0,
	JTOK_Nil = -1,

	// token alias (for easy rebinding)
	JTOK_Reference = '&',
	JTOK_DoubleReference = JTOK_AndAnd,
	JTOK_Dereference = '@',

	JTOK_SimpleMax = JTOK_Eof,
};

#define RESERVED_WORD_LIST \
		RW(If) STR(if),	\
		RW(Else) STR(else), \
		RW(Switch) STR(switch), \
		RW(Break) STR(break), \
		RW(Continue) STR(continue), \
		RW(Return) STR(return), \
		RW(For) STR(for), \
		RW(While) STR(while), \
		RW(Enum) STR(enum), \
		RW(Struct) STR(struct), \
		RW(Typedef) STR(typedef), \
		RW(Soa) STR(SOA), \
		RW(Inline) STR(inline), \
		RW(NoInline) STR(noinline), \
		RW(Defer) STR(defer), \
		RW(Null) STR(null), \
		RW(True) STR(true), \
		RW(False) STR(false), \
		RW(New) STR(new), \
		RW(Delete) STR(delete), \
		RW(Using) STR(using), \
		RW(ImportDirective) STR(#import), \
		RW(ForeignDirective) STR(#foreign), \
		RW(ForeignLibraryDirective) STR(#foreign_library), \
		RW(Cast) STR(cast)

#define RW(x) RWORD_##x
#define STR(x)
	enum RWORD
	{
		RESERVED_WORD_LIST,

		RWORD_Max,
		RWORD_Min = 0,
		RWORD_Nil = -1,
	};
#undef STR
#undef RW



enum LITK
{
	LITK_Integer,
	LITK_Float,
	LITK_Char,
	LITK_String,
	LITK_Bool,
	LITK_Null,
	LITK_Enum,
	LITK_Array,

	EWC_MAX_MIN_NIL(LITK)
};

enum LITSIGN
{
	LITSIGN_Unsigned,
	LITSIGN_Signed
};

struct SLiteralType	// litty
{
			SLiteralType()
			:m_litk(LITK_Nil)
			,m_cBit(-1)
			,m_fIsSigned(true)
				{ ; }

			SLiteralType(LITK litk, LITSIGN litsign = LITSIGN_Signed, s8 cBit = -1)
			:m_litk(litk)
			,m_cBit(cBit)
			,m_fIsSigned(litsign == LITSIGN_Signed)
				{ ; }

	LITK	m_litk;
	s8		m_cBit;
	bool	m_fIsSigned;
};

struct SJaiLexer // tag = jlex
{
   // lexer variables
   const char *		m_pChInput;
   const char *		m_pChEof;
   const char *		m_pChParse;
   char *			m_aChStorage;
   u32				m_cChStorage;

   // lexer parse location for error messages
   const char *		m_pChzFilename;
   const char *		m_pChBegin;
   const char *		m_pChEnd;

   // current token info
   u32				m_jtok;		// lexer->token is the token ID, which is unicode code point for a single-char token, 
								// < 0 for an error, > 256 for multichar or eof
   RWORD			m_rword;
   F64				m_g;
   u64				m_n;
   LITK				m_litk;
   char *			m_pChString;
   size_t			m_cChString;
};


struct SLexerLocation // tag = lexloc
{
					SLexerLocation()
					:m_strFilename()
					,m_dB(-1)
						{ ; }

					SLexerLocation(const EWC::CString & strFilename, s32 dB = -1)
					:m_strFilename(strFilename)
					,m_dB(dB)
					{ ; }

	explicit		SLexerLocation(SJaiLexer * pJlex)
					:m_strFilename(pJlex->m_pChzFilename)
					,m_dB((s32)(pJlex->m_pChBegin - pJlex->m_pChInput))
						{ ; }

	bool			operator<=(const SLexerLocation & lexlocRhs) const
						{
							if (m_strFilename.Hv() == lexlocRhs.m_strFilename.Hv())
								return m_dB <= lexlocRhs.m_dB;

							return m_strFilename.Hv() <= lexlocRhs.m_strFilename.Hv();
						}
		
	bool			FIsValid() const
						{ return m_dB >= 0;}

	EWC::CString	m_strFilename;
	s32				m_dB;
};

void InitJaiLexer(SJaiLexer * pJlex, const char * pChInput, const char * pChInputEnd, char * aChStorage, u32 cChStorage);
bool FConsumeToken(SJaiLexer * pJlex, JTOK jtok);
int JtokNextToken(SJaiLexer * pJlex);
void SplitToken(SJaiLexer * pJlex, JTOK jtokSplit);
RWORD RwordLookup(SJaiLexer * pJlex);

const char * PChzFromJtok(JTOK jtok);
const char * PChzFromRword(RWORD rword);
const char * PChzCurrentToken(SJaiLexer * pJlex);
