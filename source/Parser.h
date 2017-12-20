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

#include "Error.h"
#include "EwcArray.h"
#include "EwcHash.h"
#include "EwcTypes.h"
#include "EwcString.h"
#include "Lexer.h"
#include "TypeInfo.h"



class CIRValue;
class CParseContext;
class CSTNode;
class CSymbolTable;
class CWorkspace;
struct SErrorManager;
struct SOpTypes;
struct SSymbol;
struct SUniqueNameSet;

namespace EWC
{
	struct SStringEditBuffer;
}

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



enum PARK : s16 // PARse Kind
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

	PARK_Cast,
	PARK_ArrayElement,		// [array, index]
	PARK_MemberLookup,		// [struct, child]
	PARK_ProcedureCall,		// [procedure, arg0, arg1, ...]

	PARK_List,				// declarations used by structs
	PARK_ParameterList,		// comma separated declarations used by argument lists
	PARK_ExpressionList,	// list of expressions, used by array literals - doesn't error on rhs only values.
	PARK_If,
	PARK_Else,

	PARK_ArrayDecl,
	PARK_ReferenceDecl,		// used in type specification, not used for the unary address-of operator
	PARK_QualifierDecl,

	PARK_ProcedureReferenceDecl,
	PARK_Decl,
//	PARK_CompoundDecl,	// comma separated declarations - specialized AST node for future tuple return value support.
	PARK_Typedef,
	PARK_ConstantDecl,
	PARK_ProcedureDefinition,
	PARK_EnumDefinition,
	PARK_StructDefinition,
	PARK_EnumConstant,
	PARK_VariadicArg,
	PARK_ArrayLiteral,
	PARK_ArgumentLabel,
	PARK_GenericDecl,
	
	EWC_MAX_MIN_NIL(PARK)
};

const char * PChzFromPark(PARK park);
const char * PChzFromLitk(LITK litk);
EWC::CString StrFromIdentifier(CSTNode * pStnod);
EWC::CString StrFromTypeInfo(STypeInfo * pTin);



class CSTDecl // tag = stdecl
{
public:
					CSTDecl()
					:m_iStnodIdentifier(-1)
					,m_iStnodType(-1)
					,m_iStnodInit(-1)
					,m_iStnodChildMin(-1)
					,m_iStnodChildMax(-1)
					,m_pTin(nullptr)
						{ ; }

	int				m_iStnodIdentifier;
	int				m_iStnodType;
	int				m_iStnodInit;
	int				m_iStnodChildMin;
	int				m_iStnodChildMax;
	STypeInfo * 	m_pTin;
};

class CSTFor // tag = stfor
{
public:
					CSTFor()
					:m_iStnodDecl(-1)
					,m_iStnodIterator(-1)
					,m_iStnodInit(-1)
					,m_iStnodBody(-1)
					,m_iStnodPredicate(-1)
					,m_iStnodIncrement(-1)
						{ ; }

	int				m_iStnodDecl;
	int				m_iStnodIterator;	// lhs iterator if not a decl
	int				m_iStnodInit;		// rhs init if not a decl
	int				m_iStnodBody;
	int				m_iStnodPredicate;
	int				m_iStnodIncrement;
};

class CSTProcedure // tag = stproc
{
public:
					CSTProcedure()
					:m_iStnodProcName(-1)
					,m_iStnodParameterList(-1)
					,m_iStnodReturnType(-1)
					,m_iStnodBody(-1)
					,m_iStnodForeignAlias(-1)
					,m_pStnodParentScope(nullptr)
					,m_pTinproc(nullptr)
					,m_fIsForeign(false)
					,m_fUseUnmangledName(false)
						{ ; }

	int						m_iStnodProcName;
	int						m_iStnodParameterList;
	int						m_iStnodReturnType;
	int						m_iStnodBody;
	int						m_iStnodForeignAlias;
	CSTNode *				m_pStnodParentScope;	// procedure this proc is nested inside (if there is one)
	STypeInfoProcedure *	m_pTinproc;
	bool					m_fIsForeign;
	bool					m_fUseUnmangledName;
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

enum ENUMIMP	// implicit enum members (added as STNodes during parse)
{
	ENUMIMP_NilConstant,	// -1 or max unsigned (not included in min)
	ENUMIMP_MinConstant,	// lowest user value
	ENUMIMP_LastConstant,	// highest user value 
	ENUMIMP_MaxConstant,	// one past the highest value
	ENUMIMP_Names,
	ENUMIMP_Values,

	ENUMIMP_Max,
	ENUMIMP_Min = 0,
	ENUMIMP_Nil = -1,
	ENUMIMP_CConstant = (ENUMIMP_MaxConstant + 1) - ENUMIMP_NilConstant
};

const char * PChzFromEnumimp(ENUMIMP enumimp);

// Syntax tree string values - used for identifiers, labels and reserved words
class CSTIdentifier // tag = stident
{
public:
						CSTIdentifier()
						:m_str()
							{ ; }

	EWC::CString		m_str;
};

enum STREES : s8
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
	FSTNOD_Fallthrough = 0x4,		// this node (should be a case/default statement) falls through - BB, need a better place to store this
	FSTNOD_CommutativeCall = 0x8,	// this function is an overloaded operator with arguments reversed.
	FSTNOD_NoCodeGeneration = 0x10, // skip this node for codegen - used by generic definitions

	FSTNOD_None			= 0x0,
	FSTNOD_All			= 0x1F,
};
EWC_DEFINE_GRF(GRFSTNOD, FSTNOD, u8);

enum FDBGSTR // DeBuG STRing Flags
{
	FDBGSTR_Name				= 0x1,
	FDBGSTR_Type				= 0x2,
	FDBGSTR_LiteralSize			= 0x4,
	FDBGSTR_UseSizedNumerics	= 0x8, // resolve type aliasing for simple integers - should this be all type aliasing?
	FDBGSTR_NoWhitespace		= 0x10,

	FDBGSTR_None				= 0x0,
	FDBGSTR_All					= 0x1F,
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

	void				ReplaceChild(CSTNode * pStnodOld, CSTNode * pStnodNew);

	void				WriteDebugString(EWC::SStringBuffer * pStrbuf, GRFDBGSTR = FDBGSTR_Name);

	int					CStnodChild() const
							{ return (int)m_arypStnodChild.C(); }
	CSTNode *			PStnodChild(int iStnod)
							{ return m_arypStnodChild[iStnod]; }
	CSTNode *			PStnodChildSafe(int iStnod)
							{ return ((iStnod >= 0) & (iStnod < (int)m_arypStnodChild.C())) ? m_arypStnodChild[iStnod] : nullptr; }

	TOK						m_tok;
	PARK					m_park;
	STREES					m_strees;
	GRFSTNOD				m_grfstnod;

	CSTValue *				m_pStval;
	CSTIdentifier *			m_pStident;
	CSTDecl *				m_pStdecl;
	CSTProcedure *			m_pStproc;
	CSTFor *				m_pStfor;
	CSTEnum *				m_pStenum;
	SLexerLocation			m_lexloc;
	STypeInfo *				m_pTin;	
	SOpTypes *				m_pOptype;
	CSymbolTable *			m_pSymtab;
	SSymbol *				m_pSym;

	EWC::CDynAry<CSTNode *>	m_arypStnodChild;
};

CSTValue * PStvalCopy(EWC::CAlloc * pAlloc, CSTValue * pStval);
CSTNode * PStnodCopy(EWC::CAlloc * pAlloc, CSTNode * pStnodSrc);
CSTValue * PStvalExpected(CSTNode * pStnod);

CSTNode ** PPStnodChildFromPark(CSTNode * pStnod, int * pCStnodChild, PARK park);
void PrintTypeInfo(EWC::SStringBuffer * pStrbuf, STypeInfo * pTin, PARK park, GRFDBGSTR grfdbgstr = FDBGSTR_None);
void WriteDebugStringForEntries(CWorkspace * pWork, char * pCh, char * pChEnd, GRFDBGSTR grfdbgstr);
void HideDebugStringForEntries(CWorkspace * pWork, size_t cBHiddenMax);

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
	FSYM_None				= 0x0,
	FSYM_IsBuiltIn			= 0x1,
	FSYM_IsType				= 0x2,	// this is a type declaration (if not set this is a named instance)
	FSYM_VisibleWhenNested	= 0x4,	// types, constants and procedures that are visible in a more deeply nested symbol table
									// - ie. not an instance. Nested proceedure should be able to call peer procedure, but not
									//   access variable from parent proc.
	FSYM_NeedsGenericRemap	= 0x8,	// Temp Flag used during instantiation of generics

	FSYM_All				= 0xF,
};
EWC_DEFINE_GRF(GRFSYM, FSYM, u32);

enum FSYMLOOK	// SYMbol LOOKup flags
{
	FSYMLOOK_None			= 0x0,
	FSYMLOOK_Local			= 0x1,
	FSYMLOOK_Ancestors		= 0x2,
	FSYMLOOK_IgnoreOrder	= 0x4,

	FSYMLOOK_All			= 0x7,
	FSYMLOOK_Default		= FSYMLOOK_Local | FSYMLOOK_Ancestors,
};

EWC_DEFINE_GRF(GRFSYMLOOK, FSYMLOOK, u32);


enum SYMDEP		// SYMbol DEPendency 
{
						// NIL = Haven't determined if used
	SYMDEP_Unused,		// Symbol is not referenced by any live code - no need to codeGen
	SYMDEP_Used,		// Referenced by live code 

	EWC_MAX_MIN_NIL(SYMDEP)
};

struct SSymbol	// tag = sym
{
	EWC::CString			m_strName;

	CSTNode *				m_pStnodDefinition;
	GRFSYM					m_grfsym;
	SYMDEP					m_symdep;

	STypeInfo *				m_pTin;
	CIRValue *				m_pVal;

	SSymbol *				m_pSymPrev;				// list of shadowed symbols in reverse lexical order. 

	EWC::CDynAry<SSymbol *>	m_aryPSymReferencedBy;
};

SLexerLocation LexlocFromSym(SSymbol * pSym);

enum FSHADOW
{
	FSHADOW_NoShadowing,
	FShadow_ShadowingAllowed,
};

struct SGenericMap // tag = genmap
{
							SGenericMap(EWC::CAlloc * pAlloc, SSymbol * pSymDefinition)
							:m_pSymDefinition(pSymDefinition)	
							,m_mpPTingenPTinRemapped(pAlloc, EWC::BK_TypeCheckGenerics)
								{ ; }

	void 					Swap(SGenericMap * pGenmapOther)
								{ 
									auto pSymDefinitionTemp = m_pSymDefinition;
									m_pSymDefinition = pGenmapOther->m_pSymDefinition;
									pGenmapOther->m_pSymDefinition = pSymDefinitionTemp;

									m_mpPTingenPTinRemapped.Swap(&pGenmapOther->m_mpPTingenPTinRemapped);
								}

	bool					FIsEmpty() const
								{
									return m_mpPTingenPTinRemapped.FIsEmpty();// && m_mpPSymGenericPSymRemapped.FIsEmpty();
								}


	SSymbol *									m_pSymDefinition; 
	EWC::CHash<STypeInfoGeneric *, STypeInfo *> m_mpPTingenPTinRemapped;
};

class CSymbolTable		// tag = symtab
{
protected:
	friend CSymbolTable * PSymtabNew(EWC::CAlloc *, CSymbolTable *, const EWC::CString &, SUniqueNameSet *, EWC::CHash<HV, STypeInfo *> *);

							// protected constructor to force use of CWorkspace::PSymtabNew()
							CSymbolTable(
								const EWC::CString & strNamespace,
								EWC::CAlloc * pAlloc,
								EWC::CHash<HV, STypeInfo *> * phashHvPTinUnique,
								SUniqueNameSet * pUnsetTin)
							:m_strNamespace(strNamespace)
							,m_pAlloc(pAlloc)
							,m_hashHvPSym(pAlloc, EWC::BK_Symbol)
							,m_hashHvPTinBuiltIn(pAlloc, EWC::BK_Symbol)
							,m_phashHvPTinUnique(phashHvPTinUnique)
							,m_hashHvPTinfwd(pAlloc, EWC::BK_Symbol)
							,m_arypTinManaged(pAlloc, EWC::BK_Symbol)
							,m_arypSymGenerics(pAlloc, EWC::BK_Symbol)	
							,m_pUnsetTin(pUnsetTin)
							,m_pSymtabParent(nullptr)
							,m_pSymtabNextManaged(nullptr)
							,m_iNestingDepth(0)
								{ ; }

public:
	class CSymbolIterator // tag = symiter
	{
	public:
							CSymbolIterator()
							:m_pSymtab(nullptr)
							,m_pSym(nullptr)
							,m_grfsymlook(FSYMLOOK_Default)
								{ ; }


							CSymbolIterator(
								CSymbolTable * pSymtab,
								const EWC::CString & str,
								const SLexerLocation & lexloc,
								GRFSYMLOOK grfsymlook);

		SSymbol *			PSymNext();
		bool				FIsDone() const
								{ return (m_pSymtab == nullptr) | (m_pSym == nullptr); }

		CSymbolTable *		m_pSymtab;
		SSymbol *			m_pSym;
		SLexerLocation		m_lexloc;
		GRFSYMLOOK			m_grfsymlook;
	};

							~CSymbolTable();

	void					AddBuiltInSymbols(CWorkspace * pWork);
	SSymbol *				PSymEnsure(
								SErrorManager * pErrman,
								const EWC::CString & strName,
								CSTNode * pStnodDefinition,
								GRFSYM grfsym = FSYM_None, 
	 							FSHADOW fshadow = FShadow_ShadowingAllowed);

	SSymbol *				PSymNewUnmanaged(const EWC::CString & strName, CSTNode * pStnodDefinition, GRFSYM grfsym);
	SSymbol * 				PSymGenericInstantiate(SSymbol * pSym, STypeInfo * pTinInstance);

	SSymbol *				PSymLookup(
								const EWC::CString & str,
								const SLexerLocation & lexloc, 
								GRFSYMLOOK grfsymlook = FSYMLOOK_Default,
								CSymbolTable ** ppSymtabOut = nullptr);

	STypeInfo *				PTinBuiltin( const EWC::CString & str);
	STypeInfoLiteral *		PTinlitFromLitk(LITK litk);
	STypeInfoLiteral *		PTinlitFromLitk(LITK litk, int cBit, bool fIsSigned);
	STypeInfoPointer *		PTinptrAllocate(STypeInfo * pTinPointedTo);
	STypeInfoQualifier *	PTinqualEnsure(STypeInfo * pTinTarget, GRFQUALK grfqualk);

	STypeInfo *				PTinMakeUniqueBase(STypeInfo * pTin, EWC::SStringEditBuffer * pSeb);

	template <typename T>
	T *						PTinMakeUnique(T * pTin)
								{ 
									EWC::SStringEditBuffer seb(m_pAlloc);
									return (T *)PTinMakeUniqueBase(pTin, &seb); 
								}

	void					AddBuiltInType(SErrorManager * pErrman, SLexer * pLex, STypeInfo * pTin);
	void					AddManagedTin(STypeInfo * pTin);
	void					AddManagedSymtab(CSymbolTable * pSymtab);

	void					PrintDump();

	EWC::CString				m_strNamespace;	// unique name for this symbol table's scope
	EWC::CAlloc *				m_pAlloc;
	EWC::CHash<HV, SSymbol *>	m_hashHvPSym;		// All the symbols defined within this scope, a full lookup requires
													//  walking up the parent list
	EWC::CHash<HV, STypeInfo *>
								m_hashHvPTinBuiltIn;	// Builtin Types declared in this scope

	EWC::CHash<HV, STypeInfo *> *
								m_phashHvPTinUnique;	// pointer to global unique type table

	EWC::CHash<HV, STypeInfoForwardDecl *>
								m_hashHvPTinfwd;	// all pending forward declarations
	EWC::CDynAry<STypeInfo *>	m_arypTinManaged;	// all type info structs that need to be deleted.
	EWC::CDynAry<SSymbol *>		m_arypSymGenerics;	// symbol copies for generics, not mapped to an identifier
	SUniqueNameSet *			m_pUnsetTin;		// set of unique names for types (created during parse)
	EWC::CDynAry<STypeInfoLiteral *>	
								m_mpLitkArypTinlit[LITK_Max];

	CSymbolTable *				m_pSymtabParent;

	CSymbolTable *				m_pSymtabNextManaged;	// next table in the global list
	s32							m_iNestingDepth;					
};



class CParseContext // tag = parctx
{
public:
						CParseContext(EWC::CAlloc * pAlloc, CWorkspace * pWork)
						:m_pAlloc(pAlloc)
						,m_pWork(pWork)
						,m_pSymtab(nullptr)
						,m_pStnodScope(nullptr)
						,m_grfsymlook(FSYMLOOK_Default)
							{ ; }

	EWC::CAlloc * 		m_pAlloc;
	CWorkspace *		m_pWork;
	CSymbolTable *		m_pSymtab;
	CSTNode *			m_pStnodScope;	// current containg scope
	GRFSYMLOOK			m_grfsymlook;
};

void			PushSymbolTable(CParseContext * pParctx, CSymbolTable * pSymtab, const SLexerLocation & lexloc);
CSymbolTable *	PSymtabPop(CParseContext * pParctx);
CSymbolTable *	PSymtabFromPTin(STypeInfo * pTin);

STypeInfoProcedure * PTinprocAlloc(CSymbolTable * pSymtab, size_t cParam, size_t cReturn, const char * pCozName);

STypeInfo * PTinQualifyAfterAssignment(STypeInfo * pTin, CSymbolTable * pSymtab);
STypeInfo * PTinStripQualifiers(STypeInfo * pTin);

const char * PCozOverloadNameFromTok(TOK tok);
ERRID ErridCheckOverloadSignature(TOK tok, STypeInfoProcedure * pTinproc, SErrorManager * pErrman, SLexerLocation * pLexloc);
bool FAllowsCommutative(PARK park);

void ParseGlobalScope(CWorkspace * pWork, SLexer * pLex, bool fAllowIllegalEntries = false);



enum IVALK // Instance VALue Kind
{
	IVALK_Error,	
	IVALK_Type,		// not an expression value: either a type or Type.m_nonConstantMember
	IVALK_RValue,	// has a value, but does not correspond to a memory location
	IVALK_LValue,	// has an assignable value

	EWC_MAX_MIN_NIL(IVALK)
};



IVALK IvalkCompute(CSTNode * pStnod);