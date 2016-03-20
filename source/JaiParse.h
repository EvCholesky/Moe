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

#include "EwcArray.h"
#include "EwcHash.h"
#include "EwcTypes.h"
#include "EwcString.h"
#include "JaiLex.h"



class CIRValue;
class CParseContext;
class CSTNode;
class CSymbolTable;
class CWorkspace;
struct SErrorManager;
struct SSymbol;
struct STypeInfo;
struct STypeInfoEnum;
struct STypeInfoForwardDecl;
struct STypeInfoLiteral;
struct STypeInfoPointer;
struct STypeInfoProcedure;

// type should only indicate storage type - actual type info should come from STypeInfoLiteral
enum STVALK
{
	STVALK_Nil = -1,
	STVALK_Float,
	STVALK_SignedInt,
	STVALK_UnsignedInt,
	STVALK_String,
	STVALK_ReservedWord,
};


// syntax tree value storage
class CSTValue	// tag = stval
{
public:
						CSTValue()
						:m_stvalk(STVALK_Nil)
						,m_str()
						,m_nUnsigned(0)
						,m_rword(RWORD_Nil)
						,m_litkLex(LITK_Nil)
							{ ; }

	STVALK				m_stvalk;
	EWC::CString		m_str;
	union
	{
		F64					m_g;
		u64					m_nUnsigned;
		s64					m_nSigned;
	};

	RWORD				m_rword;		// only valid iff park == PARK_ReservedWord
	LITK				m_litkLex;		// literal type determined by lexing, 
										// BB - should STVALK should be expanded to make this unnecessary?
};

inline void SetFloatValue(CSTValue * pStval, F64 g)
{
	pStval->m_stvalk = STVALK_Float;
	pStval->m_g = g;
}

inline void SetSignedIntValue(CSTValue * pStval, s64 n)
{
	pStval->m_stvalk = STVALK_SignedInt;
	pStval->m_nSigned = n;
}

inline void SetUnsignedIntValue(CSTValue * pStval, u64 n)
{
	pStval->m_stvalk = STVALK_UnsignedInt;
	pStval->m_nUnsigned = n;
}

inline void SetBoolValue(CSTValue * pStval, bool f)
{
	pStval->m_stvalk = STVALK_UnsignedInt;
	pStval->m_nUnsigned = f;
}



enum PARK // PARse Kind
{
	PARK_Error,
	PARK_Identifier,
	PARK_ReservedWord,
	PARK_Nop,
	PARK_Literal,
	PARK_AdditiveOp,
	PARK_MultiplicativeOp,
	PARK_ShiftOp,
	PARK_EqualityOp,
	PARK_RelationalOp,
	PARK_BitwiseAndOrOp,
	PARK_LogicalAndOrOp,
	PARK_AssignmentOp,
	PARK_UnaryOp,
	PARK_PostfixUnaryOp,	// postfix increment, decrement
	PARK_Uninitializer,

	// non-terminals
	PARK_ArrayElement,		// [array, index]
	PARK_MemberLookup,		// [struct, child]
	PARK_ProcedureCall,		// [procedure, arg0, arg1, ...]
	PARK_List,
	PARK_ParameterList,
	PARK_ArrayDecl,
	PARK_If,
	PARK_Else,

	PARK_ReferenceDecl,		// used in type specification, not used for the unary address-of operator
	PARK_Decl,
	PARK_Typedef,
	PARK_ConstantDecl,
	PARK_ProcedureDefinition,
	PARK_EnumDefinition,
	PARK_StructDefinition,
	PARK_EnumConstant,
	PARK_VariadicArg,
	
	EWC_MAX_MIN_NIL(PARK)
};

const char * PChzFromPark(PARK park);
const char * PChzFromLitk(LITK litk);
EWC::CString StrFromIdentifier(CSTNode * pStnod);
bool FDoesOperatorReturnBool(PARK park);



class CSTDecl // tag = stdecl
{
public:
					CSTDecl()
					:m_iStnodIdentifier(-1)
					,m_iStnodType(-1)
					,m_iStnodInit(-1)
					,m_pTin(nullptr)
						{ ; }

	int				m_iStnodIdentifier;
	int				m_iStnodType;
	int				m_iStnodInit;
	STypeInfo * 	m_pTin;
};

class CSTProcedure // tag = stproc
{
public:
					CSTProcedure()
					:m_iStnodProcName(-1)
					,m_iStnodParameterList(-1)
					,m_iStnodReturnType(-1)
					,m_iStnodBody(-1)
					,m_pTinproc(nullptr)
					,m_fIsForeign(false)
						{ ; }

	int						m_iStnodProcName;
	int						m_iStnodParameterList;
	int						m_iStnodReturnType;
	int						m_iStnodBody;
	STypeInfoProcedure *	m_pTinproc;
	bool					m_fIsForeign;
};

class CSTEnum // tag = stenum
{
public:
					CSTEnum()
					:m_iStnodIdentifier(-1)
					,m_iStnodType(-1)
					,m_iStnodConstantList(-1)
					,m_pTinenum(nullptr)
						{ ; }

	int				m_iStnodIdentifier;
	int				m_iStnodType;
	int				m_iStnodConstantList;
	STypeInfoEnum * m_pTinenum;
};

enum ENUMIMP	// implicit enum members
{
	ENUMIMP_NilChild,	// -1 or max unsigned (not included in min)
	ENUMIMP_MinChild,	// lowest user value
	ENUMIMP_LastChild,	// highest user value 
	ENUMIMP_MaxChild,	// one past the highest value
	//ENUMIMP_Names,
	//ENUMIMP_Values,

	EWC_MAX_MIN_NIL(ENUMIMP)
};

const char * PChzFromEnumimp(ENUMIMP enumimp);

// Syntax tree string values - used for identifiers and reserved words
class CSTIdentifier // tag = stident
{
public:
						CSTIdentifier()
						:m_str()
							{ ; }

	EWC::CString		m_str;
};

enum STREES
{
	STREES_Parsed,
	STREES_SignatureTypeChecked,	// function's signature has been type checked, but not it's body. it's enough to type check
						//  proc calls, BB - Is this really necessary? should we just mark TypeChecked before we're done?
	STREES_TypeChecked,

	EWC_MAX_MIN_NIL(STREES)
};

enum FSTNOD
{
	FSTNOD_EntryPoint	= 0x1,		// this should be inserted as a top level entry point, not in place (local function)
	FSTNOD_ImplicitMember = 0x2,	// this node was created as an implicit member, did not come directly from the source

	FSTNOD_None			= 0x0,
	FSTNOD_All			= 0x3,
};
EWC_DEFINE_GRF(GRFSTNOD, FSTNOD, u8);

enum FDBGSTR // DeBuG STRing Flags
{
	FDBGSTR_Name        = 0x1,
	FDBGSTR_Type        = 0x2,
	FDBGSTR_LiteralSize = 0x4,

	FDBGSTR_None        = 0x0,
	FDBGSTR_All         = 0x7,
};
EWC_DEFINE_GRF(GRFDBGSTR, FDBGSTR, u32);

class CSTNode // tag = stnod
{
public:
						CSTNode(EWC::CAlloc * pAlloc, const SLexerLocation & lexloc);
						~CSTNode();

	int					IAppendChild(CSTNode * pStnodChild)
							{ 
								if (pStnodChild)
								{
									EWC_ASSERT(
										!pStnodChild->m_grfstnod.FIsAnySet(FSTNOD_EntryPoint), 
										"Node marked as EntryPoint being added as child");

									m_arypStnodChild.Append(pStnodChild); 
									return (int)m_arypStnodChild.C() - 1;
								}
								return -1;
							}

	size_t				CChWriteDebugString(char * pCh, char * pChEnd, GRFDBGSTR = FDBGSTR_Name);

	int					CStnodChild() const
							{ return (int)m_arypStnodChild.C(); }
	CSTNode *			PStnodChild(int iStnod)
							{ return m_arypStnodChild[iStnod]; }
	CSTNode *			PStnodChildSafe(int iStnod)
							{ return ((iStnod >= 0) & (iStnod < (int)m_arypStnodChild.C())) ? m_arypStnodChild[iStnod] : nullptr; }

	JTOK					m_jtok;
	PARK					m_park;
	STREES					m_strees;
	GRFSTNOD				m_grfstnod;

	CSTValue *				m_pStval;
	CSTIdentifier *			m_pStident;
	CSTDecl *				m_pStdecl;
	CSTProcedure *			m_pStproc;
	CSTEnum *				m_pStenum;
	SLexerLocation			m_lexloc;
	STypeInfo *				m_pTin;	
	STypeInfo *				m_pTinOperand;		// used for implicit casts, could be replaced with an explicit PARK_Cast
	CSymbolTable *			m_pSymtab;
	SSymbol *				m_pSym;

	EWC::CDynAry<CSTNode *>	m_arypStnodChild;
};

CSTNode ** PPStnodChildFromPark(CSTNode * pStnod, int * pCStnodChild, PARK park);
size_t CChPrintTypeInfo(STypeInfo * pTin, PARK park, char * pCh, char * pChEnd);
void CChWriteDebugStringForEntries(CWorkspace * pWork, char * pCh, char * pChEnd, GRFDBGSTR grfdbgstr);

inline bool FIsReservedWord(CSTNode * pStnod, RWORD rword)
{
	if ((pStnod->m_park != PARK_ReservedWord) | (pStnod->m_pStval == nullptr))
		return false;

	return pStnod->m_pStval->m_rword == rword;
}

inline bool FIsIdentifier(CSTNode * pStnod, const char * pChzIdent)
{
	if ((pStnod->m_park != PARK_Identifier) | (pStnod->m_pStident == nullptr))
		return false;

	return pStnod->m_pStident->m_str == pChzIdent;
}

EWC::CString StrFromIdentifier(CSTNode * pStnod);



enum FSYM		// SYMbol flags
{
	FSYM_None			= 0x0,
	FSYM_IsBuiltIn		= 0x1,
	FSYM_IsType			= 0x2,	// this is a type declaration (if not set this is a named instance)

	FSYM_All			= 0x3,
};
EWC_DEFINE_GRF(GRFSYM, FSYM, u32);

enum FSYMLOOK	// SYMbol LOOKup flags
{
	FSYMLOOK_None		= 0x0,
	FSYMLOOK_Local		= 0x1,
	FSYMLOOK_Ancestors	= 0x2,

	FSYMLOOK_All		= 0x3,
	FSYMLOOK_Default	= FSYMLOOK_Local | FSYMLOOK_Ancestors,
};

EWC_DEFINE_GRF(GRFSYMLOOK, FSYMLOOK, u32);

struct SSymbol	// tag = sym
{
	EWC::CString	m_strName;

	CSTNode *		m_pStnodDefinition;
	GRFSYM			m_grfsym;

	STypeInfo *		m_pTin;
	CIRValue *		m_pVal;

	SSymbol *		m_pSymPrev;		// list of shadowed symbols in reverse lexical order. 
};

enum FSYMTAB	// SYMbol LOOKup flags
{
	FSYMTAB_Ordered		= 0x1, // symbols cannot be referenced before their lexical position

	FSYMTAB_None		= 0x0,
	FSYMTAB_All			= 0x1,
	FSYMTAB_Default		= FSYMTAB_Ordered,
};

EWC_DEFINE_GRF(GRFSYMTAB, FSYMTAB, u8);

class CSymbolTable		// tag = symtab
{
protected:
	friend class CWorkspace;

							// protected constructor to force use of CWorkspace::PSymtabNew()
							CSymbolTable(const EWC::CString & strName, EWC::CAlloc * pAlloc)
							:m_strName(strName)
							,m_pAlloc(pAlloc)
							,m_hashHvPSym(pAlloc)
							,m_hashHvPTin(pAlloc)
							,m_hashHvPTinfwd(pAlloc)
							,m_arypTinManaged(pAlloc)
							,m_pSymtabParent(nullptr)
							,m_pSymtabNextManaged(nullptr)
							,m_grfsymtab(FSYMTAB_Default)
								{ ; }

public:
							~CSymbolTable();

	void					AddBuiltInSymbols(SErrorManager * pErrman);
	SSymbol *				PSymEnsure(
								SErrorManager * pErrman,
								const EWC::CString & strName,
								CSTNode * pStnodDefinition,
								GRFSYM grfsym = FSYM_None);

	SSymbol *				PSymLookup(
								const EWC::CString & str,
								const SLexerLocation & lexloc, 
								GRFSYMLOOK grfsymlook = FSYMLOOK_Default);
	STypeInfo *				PTinBuiltin( const EWC::CString & str);
	STypeInfoLiteral *		PTinlitFromLitk(LITK litk);
	STypeInfoLiteral *		PTinlitFromLitk(LITK litk, int cBit, bool fIsSigned);
	STypeInfoPointer *		PTinptrGetReference(STypeInfo * pTinPointedTo);

	void					AddBuiltInType(SErrorManager * pErrman, SJaiLexer * pJlex, STypeInfo * pTin);
	void					AddManagedTin(STypeInfo * pTin);
	void					AddManagedSymtab(CSymbolTable * pSymtab);

	void					PrintDump();

	EWC::CString				m_strName;
	EWC::CAlloc *				m_pAlloc;
	EWC::CHash<HV, SSymbol *>	m_hashHvPSym;		// All the symbols defined within this scope, a full lookup requires
													//  walking up the parent list
	EWC::CHash<HV, STypeInfo *>
								m_hashHvPTin;		// Declared types in this scope
	EWC::CHash<HV, STypeInfoForwardDecl *>
								m_hashHvPTinfwd;	// all pending forward declarations
	EWC::CDynAry<STypeInfo *>	m_arypTinManaged;	// all type info structs that need to be deleted.
	EWC::CDynAry<STypeInfoLiteral *>	
								m_mpLitkArypTinlit[LITK_Max];

	CSymbolTable *				m_pSymtabParent;
	SLexerLocation				m_lexlocParent;		// position that this table was defined relative to it's parent 

	CSymbolTable *				m_pSymtabNextManaged;	// next table in the global list
	GRFSYMTAB					m_grfsymtab;
};



class CParseContext // tag = parctx
{
public:
						CParseContext(EWC::CAlloc * pAlloc, CWorkspace * pWork)
						:m_pAlloc(pAlloc)
						,m_pWork(pWork)
						,m_pSymtab(nullptr)
						,m_grfsymlook(FSYMLOOK_Default)
							{ ; }

	EWC::CAlloc * 		m_pAlloc;
	CWorkspace *		m_pWork;
	CSymbolTable *		m_pSymtab;
	GRFSYMLOOK			m_grfsymlook;
};

void			PushSymbolTable(CParseContext * pParctx, CSymbolTable * pSymtab, const SLexerLocation & lexloc);
CSymbolTable *	PSymtabPop(CParseContext * pParctx);
CSymbolTable *	PSymtabFromPTin(STypeInfo * pTin);


void ParseGlobalScope(CWorkspace * pWork, SJaiLexer * pJlex, bool fAllowIllegalEntries = false);

