#pragma once
// copyright (C) 2015 Evan Christensen

#include "EwcTypes.h"
#include "EwcString.h"

namespace EWC
{

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
		RW(Soa) STR(SOA), \
		RW(Inline) STR(inline), \
		RW(NoInline) STR(noinline), \
		RW(Defer) STR(defer), \
		RW(Null) STR(null), \
		RW(True) STR(true), \
		RW(False) STR(false), \
		RW(New) STR(new), \
		RW(Delete) STR(delete), \
		RW(Using) STR(using)


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


enum FLIT // LITeral Flags
{
	FLIT_None	= 0,	
	FLIT_Signed	= 0x1,
	FLIT_Byte	= 0x2,
	FLIT_Short	= 0x4,
	FLIT_Long	= 0x8,
	FLIT_All	= 0xF
};

enum LITK
{
	LITK_Int,			// int with unassigned type
	LITK_Float,
	LITK_Char,
	LITK_String,
	LITK_Bool,
	LITK_Null,

	EWC_MAX_MIN_NIL(LITK)
};

enum LITSIGN
{
	LITSIGN_Unsigned,
	LITSIGN_Signed,

	EWC_MAX_MIN_NIL(LITSIGN)
};

enum LITSIZE
{
	LITSIZE_8,
	LITSIZE_16,
	LITSIZE_32,
	LITSIZE_64,

	EWC_MAX_MIN_NIL(LITSIZE)
};


struct SLiteralType	// litty
{
			SLiteralType()
			:m_litk(LITK_Nil)
			,m_litsign(LITSIGN_Nil)
			,m_litsize(LITSIZE_Nil)
				{ ; }

			SLiteralType(LITK litk, LITSIGN litsign, LITSIZE litsize)
			:m_litk(litk)
			,m_litsign(litsign)
			,m_litsize(litsize)
				{ ; }

	LITK	m_litk;
	LITSIGN	m_litsign;
	LITSIZE	m_litsize;
};

EWC_DEFINE_GRF(GRFLIT, FLIT, U16);

struct SJaiLexer // tag = jlex
{
   // lexer variables
   const char *		m_pChInput;
   const char *		m_pChEof;
   const char *		m_pChParse;
   char *			m_aChStorage;
   I32				m_cChStorage;

   // lexer parse location for error messages
   const char *		m_pChzFilename;
   const char *		m_pChBegin;
   const char *		m_pChEnd;

   // current token info
   U32				m_jtok;		// lexer->token is the token ID, which is unicode code point for a single-char token, 
								// < 0 for an error, > 256 for multichar or eof
   RWORD			m_rword;
   F64				m_g;
   U64				m_n;
   SLiteralType		m_litty;
   char *			m_pChString;
   size_t			m_cChString;
};


struct SLexerLocation // tag = lexloc
{
	CString	m_strFilename;
	I32		m_iLine;
	I32		m_iCh;
};

void InitJaiLexer(SJaiLexer * pJlex, const char * pChInput, const char * pChInputEnd, char * aChStorage, int cChStorage);
int JtokNextToken(SJaiLexer * pJlex);
RWORD RwordLookup(SJaiLexer * pJlex);
const char * PChzFromJtok(JTOK jtok);
const char * PChzFromRword(RWORD rword);

inline int NLine(const SJaiLexer * pJlex)
	{ return -1; }

void TestLexing();

} // namespace EWC
