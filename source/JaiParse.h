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



class CParseContext;
class CSTNode;
class CSymbolTable;
class CWorkspace;
struct STypeInfo;
struct STypeInfoEnum;
struct STypeInfoForwardDecl;
struct STypeInfoProcedure;

// syntax tree nodes
class CSTValue	// tag = stval
{
public:
						CSTValue()
						:m_litty()
						,m_str()
						,m_nUnsigned(0)
						,m_rword(RWORD_Nil)
						,m_tfnSigned(TFN_Nil)
							{ ; }

	SLiteralType		m_litty;

	EWC::CString		m_str;
	union
	{
		F64					m_g;
		u64					m_nUnsigned;
		s64					m_nSigned;
	};
	RWORD				m_rword;
	TFN					m_tfnSigned;
};

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

	PARK_Reference,
	PARK_Decl,
	PARK_ProcedureDefinition,
	PARK_EnumDefinition,
	PARK_StructDefinition,
	PARK_EnumConstant,
	
	PARK_Max,
	PARK_Min = 0,
	PARK_Nil = -1,
};

const char * PChzFromPark(PARK park);



class CSTDecl // tag = stdecl
{
public:
					CSTDecl()
					:m_iStnodIdentifier(-1)
					,m_iStnodType(-1)
					,m_iStnodInit(-1)
					,m_pTin(nullptr)
						{ ; }

	EWC::CString	StrIdentifier(CSTNode * pStnod);

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
						{ ; }

	int				m_iStnodProcName;
	int				m_iStnodParameterList;
	int				m_iStnodReturnType;
	int				m_iStnodBody;
	STypeInfoProcedure *
					m_pTinproc;
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

enum STREES
{
	STREES_Parsed,
	STREES_SignatureTypeChecked,	// function's signature has been type checked, but not it's body. it's enough to type check
						//  proc calls, BB - Is this really necessary? should we just mark TypeChecked before we're done?
	STREES_TypeChecked,

	EWC_MAX_MIN_NIL(STREES)
};

enum FDBGSTR // DeBuG STRing Flags
{
	FDBGSTR_Name = 0x1,
	FDBGSTR_Type = 0x2,

	FDBGSTR_None = 0x0,
	FDBGSTR_All = 0x3,
};
EWC_DEFINE_GRF(GRFDBGSTR, FDBGSTR, u32);

class CSTNode // tag = stnod
{
public:
						CSTNode(EWC::CAlloc * pAlloc);
						~CSTNode();

	int					IAppendChild(CSTNode * pStnodChild)
							{ 
								if (pStnodChild)
								{
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
							{ return iStnod >= 0 ? m_arypStnodChild[iStnod] : nullptr; }

	JTOK					m_jtok;
	PARK					m_park;
	STREES					m_strees;

	CSTValue *				m_pStval;
	CSTDecl *				m_pStdecl;
	CSTProcedure *			m_pStproc;
	CSTEnum *				m_pStenum;
	SLexerLocation			m_lexloc;
	STypeInfo *				m_pTin;		// Type info structs are owned by the symbol table

	EWC::CDynAry<CSTNode *>	m_arypStnodChild;
};

size_t CChPrintTypeInfo(STypeInfo * pTin, PARK park, char * pCh, char * pChEnd);
void CChWriteDebugStringForEntries(CWorkspace * pWork, char * pCh, char * pChEnd, GRFDBGSTR grfdbgstr);



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

	int				m_cB;					// inline size of this symbol in bytes ie. CB(* s16) == 8;
	CSTNode *		m_pStnodDefinition;
	GRFSYM			m_grfsym;

	STypeInfo *		m_pTin;
	CSymbolTable *	m_pSymtab;	// should this be here? or in pTin?
};

struct SUnknownSymbol // tag = unksym
{
							SUnknownSymbol(EWC::CAlloc * pAlloc)
							:m_arypStnodRef(pAlloc)
								{ ; }

	EWC::CString			m_strName;
	EWC::CDynAry<CSTNode *>	m_arypStnodRef;
};

enum SYMTABK
{
	SYMTABK_Global,
	SYMTABK_Struct,
	SYMTABK_Procedure,	// procedure or nested procedure

	EWC_MAX_MIN_NIL(SYMTABK)
};

class CSymbolTable	// tag = symtab
{
public:
							CSymbolTable(EWC::CAlloc * pAlloc)
							:m_pAlloc(pAlloc)
							,m_hashHvPSym(pAlloc)
							,m_hashHvPUnksym(pAlloc)
							,m_hashHvPTin(pAlloc)
							,m_hashHvPTinfwd(pAlloc)
							,m_arypTinManaged(pAlloc)
							,m_pSymtabParent(nullptr)
								{ ; }

							~CSymbolTable();

	void					AddBuiltInSymbols();
	SSymbol *				PSymEnsure(
								const EWC::CString & strName,
								CSTNode * pStnodDefinition,
								int cB = -1,
								GRFSYM grfsym = FSYM_None);

	SSymbol *				PSymLookup(const EWC::CString & str, GRFSYMLOOK grfsymlook = FSYMLOOK_Default);
	STypeInfo *				PTinLookup(const EWC::CString & str, GRFSYMLOOK grfsymlook = FSYMLOOK_Default, SSymbol ** ppSym = nullptr);
	STypeInfoForwardDecl *	PTinfwdLookup(const EWC::CString & str, GRFSYMLOOK grfsymlook = FSYMLOOK_Default);

	STypeInfoForwardDecl *	PTinfwdBegin(const EWC::CString & str);
	void					EndForwardDecl(
								const EWC::CString & str,
								STypeInfoForwardDecl * pTinfwd,
								STypeInfo * pTinResolved);

	void					AddUnknownSymbolReference(const EWC::CString & str, CSTNode * pStnodRef);
	void					AddNamedType(CParseContext * pParctx, SJaiLexer * pJlex, STypeInfo * pTin);
	void					AddManagedTin(STypeInfo * pTin);

	EWC::CAlloc *				m_pAlloc;
	EWC::CHash<HV, SSymbol *>	m_hashHvPSym;		// All the symbols defined within this scope, a full lookup requires
													//  walking up the parent list

	EWC::CHash<HV, SUnknownSymbol *> 
								m_hashHvPUnksym;	// All the symbols we encountered during parse that hadn't been defined
	EWC::CHash<HV, STypeInfo *>
								m_hashHvPTin;		// Declared types in this scope
	EWC::CHash<HV, STypeInfoForwardDecl *>
								m_hashHvPTinfwd;	// all pending forward declarations
	EWC::CDynAry<STypeInfo *>	m_arypTinManaged;	// all type info structs that need to be deleted.

	CSymbolTable *				m_pSymtabParent;
};

#ifdef UNUSED_DEPENDENCY_CRAP
enum DPHASE
{
	DPHASE_Parse,			// first pass, do lazy type inference and track unknown symbols and partial types
	DPHASE_TypeInference,
	DPHASE_TypeCheck,		// check types and complete sizing

	EWC_MAX_MIN_NIL(DPHASE)
};

enum FDEP
{
	FDEP_ChildNames		= 0x1,	// have names for the children, but has untyped
	FDEP_ChildTypes		= 0x2,	// names for the children (including inferred types) 
	FDEP_Sized			= 0x4,	// know the size and layout

	FDEP_None			= 0x0,
	FDEP_All			= 0x7,
};

EWC_DEFINE_GRF(GRFDEP, FDEP, u16);

struct SDependencyFrame;
struct SPartialType	// party
{
	EWC::CStringHash			m_shash;
	GRFDEP						m_grfdep;
	CDynAry<SDependencyFrame *>	m_arypDepf;
};

struct SDependencyFrame // tag = depf
{
	DPHASE						m_dphase;
	SDependencyFrame *			m_pDepfNextDirty;
	CSTNode *					m_pStnodEntry;	// entry point
	GRFDEP						m_grfdepDirty;	// which dependency phases have changed (should be counts?)
	CDynAry<SPartialType *>		m_arypParty;
};
#endif // UNUSED_DEPENDENCY_CRAP


class CParseContext // tag = parctx
{
public:
						CParseContext(EWC::CAlloc * pAlloc)
						:m_pAlloc(pAlloc)
						,m_pSymtab(nullptr)
						,m_cError(0)
						,m_grfsymlook(FSYMLOOK_Default)
							{ ; }

	EWC::CAlloc * 		m_pAlloc;
	CSymbolTable *		m_pSymtab;
	int					m_cError;
	GRFSYMLOOK			m_grfsymlook;

	//SDependencyFrame *	m_pDepfDirtyList;	 // singly linked list of dirty frames
	// map from dphase to arypDepf?
};

void			PushSymbolTable(CParseContext * pParctx, CSymbolTable * pSymtab);
CSymbolTable *	PSymtabPop(CParseContext * pParctx);
CSymbolTable *	PSymtabFromPTin(STypeInfo * pTin);


void ParseGlobalScope(CWorkspace * pWork, SJaiLexer * pJlex, bool fAllowIllegalEntries = false);

void TestParse();
