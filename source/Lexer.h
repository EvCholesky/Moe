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

enum TOK
{
	// don't define single char tokens (they are just ascii codepoints)
	TOK_Eof = 256,
	TOK_ParseError,
	TOK_Literal,
	TOK_Identifier,
	TOK_ReservedWord,
	TOK_EqualEqual,
	TOK_NotEqual,
	TOK_LessEqual,
	TOK_GreaterEqual,
	TOK_AndAnd,
	TOK_OrOr,
	TOK_AndEqual,
	TOK_OrEqual,
	TOK_XorEqual,
	TOK_TildeEqual,
	TOK_ShiftLeft,
	TOK_ShiftRight,
	TOK_PlusPlus,
	TOK_MinusMinus,
	TOK_TripleMinus,
	TOK_PlusEqual,
	TOK_MinusEqual,
	TOK_MulEqual,
	TOK_DivEqual,
	TOK_ModEqual,
	TOK_Arrow,
	TOK_ColonColon,
	TOK_ColonEqual,
	TOK_PeriodPeriod,

	TOK_Max,
	TOK_Min = 0,
	TOK_Nil = -1,

	// token alias (for easy rebinding)
	TOK_Reference = '&',
	TOK_DoubleReference = TOK_AndAnd,
	TOK_Dereference = '@',
	TOK_Label = '`',
	TOK_Generic = '$',

	TOK_SimpleMax = TOK_Eof,
};

#define RESERVED_WORD_LIST \
		RW(If) STR(if),	\
		RW(Else) STR(else), \
		RW(Switch) STR(switch), \
		RW(Case) STR(case), \
		RW(Fallthrough) STR(fallthrough), \
		RW(Break) STR(break), \
		RW(Continue) STR(continue), \
		RW(Return) STR(return), \
		RW(For) STR(for), \
		RW(ForEach) STR(for_each), \
		RW(While) STR(while), \
		RW(Enum) STR(enum), \
		RW(FlagEnum) STR(flag_enum), \
		RW(Proc) STR(proc), \
		RW(Struct) STR(struct), \
		RW(Const) STR(const), \
		RW(InArg) STR(inarg), \
		RW(Typedef) STR(typedef), \
		RW(Soa) STR(SOA), \
		RW(Inline) STR(inline), \
		RW(NoInline) STR(no_inline), \
		RW(Defer) STR(defer), \
		RW(Null) STR(null), \
		RW(True) STR(true), \
		RW(False) STR(false), \
		RW(New) STR(new), \
		RW(Delete) STR(delete), \
		RW(Using) STR(using), \
		RW(Operator) STR(operator), \
		RW(Commutative) STR(#commutative), \
		RW(ImportDirective) STR(#import), \
		RW(ForeignDirective) STR(#foreign), \
		RW(FileDirective) STR(#file), \
		RW(LineDirective) STR(#line), \
		RW(StringDirective) STR(#string), \
		RW(ForeignLibraryDirective) STR(#foreign_library), \
		RW(StaticLibraryDirective) STR(#static_library), \
		RW(DynamicLibraryDirective) STR(#dynamic_library), \
		RW(Cast) STR(cast), \
		RW(AutoCast) STR(acast), \
		RW(Sizeof) STR(sizeof), \
		RW(Alignof) STR(alignof), \
		RW(Typeof) STR(typeof), \
		RW(Typeinfo) STR(typeinfo), \
		RW(CDecl) STR(#cdecl), \
		RW(StdCall) STR(#stdcall)

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
	LITK_Pointer,	// pointer literal in bytecode

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

enum FLEXER
{
	FLEXER_EndOfLine	= 0x1,	// lexing to this token passed a newline - really StartOfLine

	FLEXER_None			= 0x0,
	FLEXER_All			= 0x1,
};
EWC_DEFINE_GRF(GRFLEXER, FLEXER, u8);

struct SLexer // tag = lex
{
   // lexer variables
   const char *		m_pChInput;
   const char *		m_pChEof;
   const char *		m_pChParse;		// current parse location - points just after the current token
   char *			m_aChScratch;	// working character space - NOTE:
   u32				m_cChScratch;

   // lexer parse location for error messages
   const char *		m_pCozFilename;
   const char *		m_pChBegin;
   const char *		m_pChEnd;

   // current token info
   u32				m_tok;		// lexer->token is the token ID, which is unicode code point for a single-char token, 
								// < 0 for an error, > 256 for multichar or eof
   GRFLEXER			m_grflexer;
   RWORD			m_rword;
   f64				m_g;
   u64				m_n;
   LITK				m_litk;
   EWC::CString		m_str;
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

	explicit		SLexerLocation(SLexer * pLex)
					:m_strFilename(pLex->m_pCozFilename)
					,m_dB((s32)(pLex->m_pChBegin - pLex->m_pChInput))
						{ ; }

	bool			operator<(const SLexerLocation & lexlocRhs) const
						{
							if (m_strFilename.Hv() == lexlocRhs.m_strFilename.Hv())
								return m_dB < lexlocRhs.m_dB;

							return m_strFilename.Hv() < lexlocRhs.m_strFilename.Hv();
						}

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

void InitLexer(SLexer * pLex, const char * pCoInput, const char * pCoInputEnd, char * aChStorage, u32 cChStorage);
bool FConsumeToken(SLexer * pLex, TOK tok);
int TokNext(SLexer * pLex);

void SkipToToken(SLexer * pLex, TOK const * const aTok, int cTok, GRFLEXER grflexer);
inline void SkipRestOfLine(SLexer * pLex)
	{ SkipToToken(pLex, nullptr, 0, FLEXER_EndOfLine); }

void SplitToken(SLexer * pLex, TOK tokSplit);
RWORD RwordLookup(SLexer * pLex);

const char * PCozFromTok(TOK tok);
const char * PCozFromRword(RWORD rword);
const char * PCozCurrentToken(SLexer * pLex);
