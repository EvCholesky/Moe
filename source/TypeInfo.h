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




// notes on type uniqueness:

// It is difficult/impossible eradicate non-unique types in the compiler without creating a relocatable type pointer-pointer. 
// Non-unique types must be created prior to type checking (as we cannot resolve names) and by the time types are 
// resolved types pointers are buried throughout the code. (syntax-tree, symbol tables, type definitions, etc.) 

// The Plan:
//  Leave non-unique types throughout the compiler, build unique types and find the unique values whenever they are needed for 
//  code generation.

// Named types are
// - basic types	(unique globally from the start)
// - typedefs		(unique name in definition scope, target type can be uniqued after type checking)
// - structures		(unique name in definition scope)
// - enums			(unique name in definition scope)
// - named procedures (unique name in definition scope)

// Unnamed types
// - pointer type
// - array type
// - anonymous procedure type

// not unique types
// - unfinalized literals




#pragma once

#include "BigMath.h"
#include "EwcArray.h"
#include "EwcString.h"
#include "lexer.h"

class CIRGlobal;
class CSTNode;

// opaque CodeGen types and values used for both LLVM IR and bytecode
typedef void * CodeGenValueRef;		// tag = cgval

// unique scope id used for uniquify-ing types
enum SCOPID : s32
{
	SCOPID_Min = 0,
	SCOPID_Nil = -1,
};

enum TINK : s8
{
    TINK_Integer	= 0,
    TINK_Float		= 1,
    TINK_Bool		= 2,			// no specialized type info
    TINK_Pointer	= 3,
    TINK_Procedure	= 4,
    TINK_Void		= 5,			// no specialized type info
    TINK_Struct		= 6,
    TINK_Array		= 7,
    TINK_Null		= 8,			// no specialized type info
    TINK_Any		= 9,			// no specialized type info
    TINK_Enum		= 10,
	TINK_Qualifier	= 11,
	TINK_ReflectedMax,

	TINK_ForwardDecl= TINK_ReflectedMax,	// Type info added for resolving pointers to self during the type-check phase.
	TINK_Literal,							// literal that hasn't been resolved to a specific type yet
	TINK_Generic,
	TINK_Flag,						// Type for enum flag assignments; no specialized type info

	EWC_MAX_MIN_NIL(TINK)
};

const char * PChzFromTink(TINK tink);

enum FTIN
{
	FTIN_IsUnique	= 0x1,

	FTIN_None,
	FTIN_All		= 0x1
};

EWC_DEFINE_GRF(GRFTIN, FTIN, u8);

struct SDataLayout		// tag = dlay
{
	s32		m_cBBool;			// byte size of "bool"
	s32		m_cBInt;			// byte size of "int"
	s32		m_cBFloat;			// byte size of "float"
	s32		m_cBPointer;		// byte size of pointer
	s32		m_cBStackAlign;		// code gen stack alignment
};

struct STypeInfo	// tag = tin
{
						STypeInfo(const EWC::CString & strName, SCOPID scopid, TINK tink)
						:m_tink(tink)
						,m_grftin(FTIN_None)
						,m_scopid(scopid)
						,m_strName(strName)
						,m_strDesc()
						,m_pCgvalDIType(nullptr)
						,m_pCgvalReflectGlobal(nullptr)
						,m_pTinNative(nullptr)
							{ ; }

    TINK				m_tink;
	GRFTIN				m_grftin;
	SCOPID				m_scopid;				// definition scope (for unique-ing)

	EWC::CString		m_strName;				// user facing name 
	EWC::CString		m_strDesc;				// unique descriptor used to unique this type 


	// BB - This shouldn't be embedded in the typeinfo - it won't work for multiple codegen passes
	CodeGenValueRef		m_pCgvalDIType;
	CodeGenValueRef		m_pCgvalReflectGlobal;	// global variable pointing to the type info struct
												// const TypeInfo entry in the reflection type table

	STypeInfo *			m_pTinNative;			// native non-aliased source type (ie sSize->s64)

	static const char * s_pChzGlobalTinTable;
};

template <typename T>
T PTinRtiCast(STypeInfo * pTin)
{
	if (pTin && pTin->m_tink == EWC::SStripPointer<T>::Type::s_tink)
		return (T)pTin;
	return nullptr;
}

template <typename T>
T PTinDerivedCast(STypeInfo * pTin)
{
	EWC_ASSERT(pTin && pTin->m_tink == EWC::SStripPointer<T>::Type::s_tink, "illegal type info derived cast");
	return (T)pTin;
}

struct STypeInfoInteger : public STypeInfo // tag = tinint
{
	static const TINK s_tink = TINK_Integer;

			STypeInfoInteger(const EWC::CString & strName, SCOPID scopid, u32 cBit, bool fSigned)
			:STypeInfo(strName, scopid, s_tink)
			,m_cBit(cBit)
			,m_fIsSigned(fSigned)
				{ ; }

	u32		m_cBit;
	bool	m_fIsSigned;
};

struct STypeInfoFloat : public STypeInfo	// tag = tinfloat
{
	static const TINK s_tink = TINK_Float;

			STypeInfoFloat(const EWC::CString & strName, SCOPID scopid, u32 cBit)
			:STypeInfo(strName, scopid, s_tink)
			,m_cBit(cBit)
				{ ; }

	u32		m_cBit;
};

struct STypeInfoPointer : public STypeInfo	// tag = tinptr
{
	static const TINK s_tink = TINK_Pointer;

						STypeInfoPointer()
						:STypeInfo("", SCOPID_Nil, s_tink)
						,m_pTinPointedTo(nullptr)
							{ ; }

	STypeInfo *			m_pTinPointedTo;
};

enum QUALK : s8
{
	QUALK_Const,		// Read only value, transitive ie. members of an const struct or target of a const ref are const
						// - implies that the values will not change during program execution,
						// - unlike c it is NOT safe to upcast to const
						// - types infered from a const type are not const ( n := 5; '5' is const, n is not)

	QUALK_InArg,		// procedure arguments, variable can't be changed, but not transitive
						// - non arguments can currently be declared as inarg for testing, but I'm not sure I'll keep that.

	EWC_MAX_MIN_NIL(QUALK)
};

const char * PChzFromQualk(QUALK qualk);

enum FQUALK
{
	FQUALK_Const	= 0x1 << QUALK_Const,
	FQUALK_InArg	= 0x1 << QUALK_InArg,

	FQUALK_None		= 0x0,
	FQUALK_All		= FQUALK_Const | FQUALK_InArg
};


EWC_DEFINE_GRF(GRFQUALK, FQUALK, u8);

void AppendFlagNames(EWC::SStringBuffer * pStrbuf, GRFQUALK grfqualk, const char * pChzSpacer);

struct STypeInfoQualifier : public STypeInfo // tag == tinqual
{
	static const TINK s_tink = TINK_Qualifier;

						STypeInfoQualifier(GRFQUALK grfqualk)
						:STypeInfo("", SCOPID_Nil, s_tink)
						,m_grfqualk(grfqualk)
							{ ; }

	STypeInfo *			m_pTin;
	GRFQUALK			m_grfqualk;
};



enum CALLCONV
{
	CALLCONV_CX86		= 0,
	CALLCONV_StdcallX86	= 1,
	CALLCONV_X64		= 2,

	EWC_MAX_MIN_NIL(CALLCONV)
};
const char * PChzFromCallconv(CALLCONV callconv);

enum INLINEK
{
	INLINEK_AlwaysInline = 0,
	INLINEK_NoInline	 = 1,

	EWC_MAX_MIN_NIL(INLINEK)
};
const char * PChzFromInlinek(INLINEK inlinek);

enum FPARMQ	// Flags for ARGument Qualifiers
{
	FPARMQ_ImplicitRef		= 0x1,		// convert LValue argument and pass into procedure's pointer argument
	FPARMQ_BakedValue		= 0x2,		// baked value parameter, will be dropped during generic specialization.

	FPARMQ_None				= 0x0,
	FPARMQ_All				= 0x3,
};

EWC_DEFINE_GRF(GRFPARMQ, FPARMQ, u8);

enum FTINPROC
{
	FTINPROC_HasVarArgs			= 0x1,
	FTINPROC_HasBakedTypeArgs	= 0x2,
	FTINPROC_HasBakedValueArgs	= 0x4,
	FTINPROC_IsCommutative		= 0x8,
	FTINPROC_Initializer		= 0x10,

	FTINPROC_None				= 0x0,
	FTINPROC_All				= 0x1F,
	FTINPROC_HasGenericArgs		= FTINPROC_HasBakedTypeArgs | FTINPROC_HasBakedValueArgs,
};
EWC_DEFINE_GRF(GRFTINPROC, FTINPROC, u8);

struct STypeInfoProcedure : public STypeInfo	// tag = 	tinproc
{
	static const TINK s_tink = TINK_Procedure;

						STypeInfoProcedure(const EWC::CString & strName, SCOPID scopid)
						:STypeInfo(strName, scopid, s_tink)
						,m_strMangled()
						,m_pStnodDefinition(nullptr)
						,m_arypTinParams()
						,m_arypTinReturns()
						,m_mpIptinGrfparmq()
						,m_grftinproc(FTINPROC_None)
						,m_inlinek(INLINEK_Nil)
						,m_callconv(CALLCONV_Nil)
							{ ; }

	bool				FHasVarArgs() const
							{ return m_grftinproc.FIsSet(FTINPROC_HasVarArgs); }
	bool				FHasGenericArgs() const
							{ return m_grftinproc.FIsAnySet(FTINPROC_HasGenericArgs); }

	EWC::CString				m_strMangled;
	CSTNode *					m_pStnodDefinition;
	EWC::CAllocAry<STypeInfo *>	m_arypTinParams;
	EWC::CAllocAry<STypeInfo *>	m_arypTinReturns;
	EWC::CAllocAry<GRFPARMQ>	m_mpIptinGrfparmq;

	GRFTINPROC					m_grftinproc;
	INLINEK						m_inlinek;
	CALLCONV					m_callconv;

	// BB - need names for named argument matching?
};

struct STypeInfoForwardDecl : public STypeInfo	// tag = tinfwd
{
	static const TINK s_tink = TINK_ForwardDecl;
						STypeInfoForwardDecl(EWC::CAlloc * pAlloc, const EWC::CString & strName, SCOPID scopid)
						:STypeInfo(strName, scopid, s_tink)
						,m_arypTinReferences(pAlloc, EWC::BK_TypeCheck)
							{ ; }

	EWC::CDynAry<STypeInfo *>	m_arypTinReferences;
};

struct STypeInfoGeneric : public STypeInfo // tag = tingen
{
	static const TINK s_tink = TINK_Generic;
						STypeInfoGeneric(const EWC::CString & strName, SCOPID scopid)
						:STypeInfo(strName, scopid, s_tink)
						,m_pStnodDefinition(nullptr)
							{ ; }

	CSTNode *			m_pStnodDefinition;
};

// NOTE: just documenting a subtle relationship: CSTVal stores a literal value and is enough to determine 
// what the STypeInfoLiteral *could* be, (ie PTinlitDefault, PTinlitTightest) but the actual type is determined
// by the nodes type passed down after type checking.

struct STypeInfoLiteral : public STypeInfo // tag = tinlit
{
	static const TINK s_tink = TINK_Literal;
						STypeInfoLiteral()
						:STypeInfo("", SCOPID_Nil, s_tink)
						,m_c(-1)
						,m_pTinSource(nullptr)
						,m_fIsFinalized(false)
						,m_litty()
						,m_pStnodDefinition(nullptr)
						,m_pGlob(nullptr)
							{ ; }
	
	s64					m_c;
	STypeInfo *			m_pTinSource;		// source type (for finalized null pointers or enum literals)
	bool				m_fIsFinalized;		// literals are finalized once they are assigned to a concrete (or default) type
	SLiteralType		m_litty;
	CSTNode *			m_pStnodDefinition;	// (needed for array literal values)
	CIRGlobal *			m_pGlob;			// value for this literal's global instance, created iff needed to array index literals
};

struct STypeStructMember	// tag = typememb
{
					STypeStructMember()
					:m_strName()
					,m_pTin(nullptr)
					,m_pStnod(nullptr)
					,m_dBOffset(-1)
						{ ;}

	EWC::CString	m_strName;
	STypeInfo *		m_pTin;
	CSTNode *		m_pStnod;		// syntax tree node for this member
	s32				m_dBOffset;		// for bytecode GEP
};

struct STypeInfoStruct : public STypeInfo	// tag = tinstruct
{
	static const TINK s_tink = TINK_Struct;

										STypeInfoStruct(const EWC::CString & strName, SCOPID scopid)
										:STypeInfo(strName, scopid, s_tink)
										,m_pStnodStruct(nullptr)
										,m_aryTypemembField()
										,m_arypTinGenericParam()
										,m_pTinprocInit(nullptr)
										,m_cB(-1)
										,m_cBAlign(-1)
											{ ; }
	
	bool								FHasGenericParams() const
											{ return m_arypTinGenericParam.C() > 0; }

	CSTNode *							m_pStnodStruct;
	EWC::CAllocAry<STypeStructMember>	m_aryTypemembField;
	EWC::CAllocAry<STypeInfo *>			m_arypTinGenericParam;
	STypeInfoProcedure *				m_pTinprocInit;			// procedure used when cginitk == CGINITK_InitializerProc

	s64									m_cB;
	s64									m_cBAlign;
};

STypeStructMember * PTypemembLookup(STypeInfoStruct * pTinstruct, const EWC::CString & strMemberName);

struct STypeInfoEnumConstant // tag = tinecon
{
	EWC::CString		m_strName;
	SBigInt				m_bintValue;
};

enum ENUMK
{
	ENUMK_Basic,
	ENUMK_FlagEnum,
};

struct STypeInfoEnum : public STypeInfo	// tag = tinenum
{
	static const TINK s_tink = TINK_Enum;

						STypeInfoEnum(const EWC::CString & strName, SCOPID scopid)
						:STypeInfo(strName, scopid, s_tink)
						,m_pTinLoose(nullptr)
						,m_enumk(ENUMK_Basic)
						,m_bintMin()
						,m_bintMax()
						,m_bintLatest()
						,m_tinstructProduced(strName, scopid)
							{ ; }

	STypeInfo *			m_pTinLoose;
	ENUMK				m_enumk;
	SBigInt				m_bintMin;
	SBigInt				m_bintMax;
	SBigInt				m_bintLatest;
	STypeInfoStruct 	m_tinstructProduced;

	EWC::CAllocAry<STypeInfoEnumConstant>	
						m_aryTinecon;
};

enum ARYK
{
    ARYK_Fixed		= 0,	// c-style fixed size array.			aN : [3] int;
    ARYK_Dynamic	= 1,	// dynamically resizing array.			aN : [..] int;
    ARYK_Reference	= 2,	// reference to array of either type.	aN : [] int;

	EWC_MAX_MIN_NIL(ARYK)
};
const char * PChzFromAryk(ARYK aryk);

enum ARYMEMB
{
	ARYMEMB_Count,
	ARYMEMB_Data,

	EWC_MAX_MIN_NIL(ARYMEMB)
};

const char * PChzFromArymemb(ARYMEMB arymemb);
ARYMEMB ArymembLookup(const char * pChzMember);

struct STypeInfoArray : public STypeInfo	// tag = tinary
{
	static const TINK s_tink = TINK_Array;

					STypeInfoArray()
					:STypeInfo("", SCOPID_Nil, s_tink)
					,m_pTin(nullptr)
					,m_pTinstructImplicit(nullptr)
					,m_c(0)
					,m_aryk(ARYK_Fixed)
					{ ; }

	STypeInfo *			m_pTin;
	STypeInfoStruct *	m_pTinstructImplicit;
	s64					m_c;
	ARYK				m_aryk;
};

void DeleteTypeInfo(EWC::CAlloc * pAlloc, STypeInfo * pTin);
bool FTypesAreSame(STypeInfo * pTinLhs, STypeInfo * pTinRhs);



struct SOpTypes // tag = optype
{
				SOpTypes()
				:m_pTinLhs(nullptr)
				,m_pTinRhs(nullptr)
				,m_pTinResult(nullptr)
				,m_pTinprocOverload(nullptr)
					{ ; }

				SOpTypes(STypeInfo * pTinLhs, STypeInfo * pTinRhs, STypeInfo * pTinResult)
				:m_pTinLhs(pTinLhs)
				,m_pTinRhs(pTinRhs)
				,m_pTinResult(pTinResult)
				,m_pTinprocOverload(nullptr)
					{ ; }

				bool FIsValid() const
					{ return m_pTinResult != nullptr; }

	STypeInfo *				m_pTinLhs;
	STypeInfo *				m_pTinRhs;
	STypeInfo *				m_pTinResult;
	STypeInfoProcedure *	m_pTinprocOverload;
};
