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

#include "BigMath.h"
#include "EwcArray.h"
#include "EwcString.h"

class CIRGlobal;
class CSTNode;
struct LLVMOpaqueType;
struct LLVMOpaqueValue;

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
	TINK_ReflectedMax,

	TINK_ForwardDecl= TINK_ReflectedMax,	// Type info added for resolving pointers to self during the type-check phase.
	TINK_Literal,							// literal that hasn't been resolved to a specific type yet

	EWC_MAX_MIN_NIL(TINK)
};

const char * PChzFromTink(TINK tink);

struct STypeInfo	// tag = tin
{
						STypeInfo(const char * pCozName, TINK tink)
						:m_tink(tink)
						,m_strName(pCozName)
						,m_pLvalDIType(nullptr)
						,m_pLvalReflectGlobal(nullptr)
						,m_pTinSource(nullptr)
							{ ; }

    TINK				m_tink;
	EWC::CString		m_strName;
	LLVMOpaqueValue *	m_pLvalDIType;
	LLVMOpaqueValue *	m_pLvalReflectGlobal;	// global variable pointing to the type info struct
												// const TypeInfo entry in the reflection type table

	STypeInfo *			m_pTinSource;			// actual source type, ignoring aliases (ie sSize->s64)
};

template <typename T>
T PTinRtiCast(STypeInfo * pTin)
{
	if (pTin && pTin->m_tink == SStripPointer<T>::Type::s_tink)
		return (T)pTin;
	return nullptr;
}

template <typename T>
T PTinDerivedCast(STypeInfo * pTin)
{
	EWC_ASSERT(pTin && pTin->m_tink == SStripPointer<T>::Type::s_tink, "illegal type info derived cast");
	return (T)pTin;
}

struct STypeInfoInteger : public STypeInfo // tag = tinint
{
	static const TINK s_tink = TINK_Integer;

			STypeInfoInteger(const char * pCozName, u32 cBit, bool fSigned)
			:STypeInfo(pCozName, s_tink)
			,m_cBit(cBit)
			,m_fIsSigned(fSigned)
				{ ; }

	u32		m_cBit;
	bool	m_fIsSigned;
};

struct STypeInfoFloat : public STypeInfo	// tag = tinfloat
{
	static const TINK s_tink = TINK_Float;

			STypeInfoFloat(const char * pCozName, u32 cBit)
			:STypeInfo(pCozName, s_tink)
			,m_cBit(cBit)
				{ ; }

	u32		m_cBit;
};

struct STypeInfoPointer : public STypeInfo	// tag = tinptr
{
	static const TINK s_tink = TINK_Pointer;

						STypeInfoPointer()
						:STypeInfo("", s_tink)
						,m_pTinPointedTo(nullptr)
						,m_soaPacking(-1)
							{ ; }

	STypeInfo *			m_pTinPointedTo;
	s32					m_soaPacking;	// -1 means no SOA. 0 means no size limit. >0 is AOSOA of that chunk size.
};

enum CALLCONV
{
	CALLCONV_CX86		= 0,
	CALLCONV_StdcallX86	= 1,
	CALLCONV_X64		= 2,

	EWC_MAX_MIN_NIL(CALLCONV)
};

enum INLINEK
{
	INLINEK_AlwaysInline = 0,
	INLINEK_NoInline	 = 1,

	EWC_MAX_MIN_NIL(INLINEK)
};

struct STypeInfoProcedure : public STypeInfo	// tag = tinproc
{
	static const TINK s_tink = TINK_Procedure;

						STypeInfoProcedure(const char * pCozName)
						:STypeInfo(pCozName, s_tink)
						,m_strMangled()
						,m_pStnodDefinition(nullptr)
						,m_arypTinParams()
						,m_arypTinReturns()
						,m_fHasVarArgs(false)
						,m_inlinek(INLINEK_Nil)
						,m_callconv(CALLCONV_Nil)
							{ ; }

	EWC::CString			m_strMangled;
	CSTNode *				m_pStnodDefinition;
	EWC::CAry<STypeInfo *>	m_arypTinParams;
	EWC::CAry<STypeInfo *>	m_arypTinReturns;
	bool					m_fHasVarArgs;
	INLINEK					m_inlinek;
	CALLCONV				m_callconv;

	// BB - need names for named argument matching?
};

struct STypeInfoForwardDecl : public STypeInfo	// tag = tinfwd
{
	static const TINK s_tink = TINK_ForwardDecl;
						STypeInfoForwardDecl(EWC::CAlloc * pAlloc, const char * pCozName)
						:STypeInfo(pCozName, s_tink)
						,m_arypTinReferences(pAlloc, EWC::BK_TypeCheck)
							{ ; }

	EWC::CDynAry<STypeInfo *>	m_arypTinReferences;
};


// NOTE: just documenting a subtle relationship: CSTVal stores a literal value and is enough to determine 
// what the STypeInfoLiteral *could* be, (ie PTinlitDefault, PTinlitTightest) but the actual type is determined
// by the nodes type passed down after type checking.

struct STypeInfoLiteral : public STypeInfo // tag = tinlit
{
	static const TINK s_tink = TINK_Literal;
						STypeInfoLiteral()
						:STypeInfo("", s_tink)
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
						{ ;}

	EWC::CString	m_strName;
	STypeInfo *		m_pTin;
	CSTNode *		m_pStnod;	// syntax tree node for this member
};

struct STypeInfoStruct : public STypeInfo	// tag = tinstruct
{
	static const TINK s_tink = TINK_Struct;

									STypeInfoStruct(const char * pCozName)
									:STypeInfo(pCozName, s_tink)
									,m_pLvalInitMethod(nullptr)
									,m_pLtype(nullptr)
									,m_pStnodStruct(nullptr)
									,m_aryTypemembField()
										{ ; }
	
	LLVMOpaqueValue *				m_pLvalInitMethod;
	LLVMOpaqueType *				m_pLtype;			// llvm type reference, here to avoid infinite recursion in
														//  self referential member pointers

	CSTNode *						m_pStnodStruct;
	EWC::CAry<STypeStructMember>	m_aryTypemembField;
};

STypeStructMember * PTypemembLookup(STypeInfoStruct * pTinstruct, const EWC::CString & strMemberName);

struct STypeInfoEnumConstant // tag = tinecon
{
	EWC::CString		m_strName;
	SBigInt				m_bintValue;
};

struct STypeInfoEnum : public STypeInfo	// tag = tinenum
{
	static const TINK s_tink = TINK_Enum;

						STypeInfoEnum(const char * pCozName)
						:STypeInfo(pCozName, s_tink)
						,m_pTinLoose(nullptr)
						,m_bintMin()
						,m_bintMax()
						,m_bintLatest()
						,m_tinstructProduced(pCozName)
							{ ; }

	STypeInfo *			m_pTinLoose;
	SBigInt				m_bintMin;
	SBigInt				m_bintMax;
	SBigInt				m_bintLatest;
	STypeInfoStruct 	m_tinstructProduced;

	EWC::CAry<STypeInfoEnumConstant>	
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
					:STypeInfo("", s_tink)
					,m_pTin(nullptr)
					,m_c(0)
					,m_aryk(ARYK_Fixed)
					{ ; }

	STypeInfo *		m_pTin;
	s64				m_c;
	ARYK			m_aryk;
};

void DeleteTypeInfo(EWC::CAlloc * pAlloc, STypeInfo * pTin);
bool FTypesAreSame(STypeInfo * pTinLhs, STypeInfo * pTinRhs);



struct SOpTypes // tag = optype
{
				SOpTypes()
				:m_pTinLhs(nullptr)
				,m_pTinRhs(nullptr)
				,m_pTinResult(nullptr)
					{ ; }

				SOpTypes(STypeInfo * pTinLhs, STypeInfo * pTinRhs, STypeInfo * pTinResult)
				:m_pTinLhs(pTinLhs)
				,m_pTinRhs(pTinRhs)
				,m_pTinResult(pTinResult)
					{ ; }

				bool FIsValid() const
					{ return m_pTinResult != nullptr; }

	STypeInfo * m_pTinLhs;
	STypeInfo * m_pTinRhs;
	STypeInfo * m_pTinResult;
};
