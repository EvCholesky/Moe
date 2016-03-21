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

class CSTNode;
struct LLVMOpaqueType;
struct LLVMOpaqueValue;

enum TINK 
{
    TINK_Integer,
    TINK_Float,
    TINK_Bool,			// no specialized type info
    TINK_String,		// no specialized type info
    TINK_Pointer,
    TINK_Procedure,
    TINK_Void,			// no specialized type info
    TINK_Struct,
    TINK_Array,
    TINK_Null,			// no specialized type info
    TINK_Any,			// no specialized type info
    TINK_Enum,
	TINK_ForwardDecl,	// Type info added for resolving pointers to self during the type-check phase.
	TINK_Literal,		// literal that hasn't been resolved to a specific type yet

	EWC_MAX_MIN_NIL(TINK)
};

const char * PChzFromTink(TINK tink);

struct STypeInfo	// tag = tin
{
			STypeInfo(const char * pChzName, TINK tink)
			:m_tink(tink)
			,m_strName(pChzName)
				{ ; }

    TINK			m_tink;
	EWC::CString	m_strName;
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

			STypeInfoInteger(const char * pChzName, u32 cBit, bool fSigned)
			:STypeInfo(pChzName, s_tink)
			,m_cBit(cBit)
			,m_fIsSigned(fSigned)
				{ ; }

	u32		m_cBit;
	bool	m_fIsSigned;
};

struct STypeInfoFloat : public STypeInfo	// tag = tinfloat
{
	static const TINK s_tink = TINK_Float;

			STypeInfoFloat(const char * pChzName, u32 cBit)
			:STypeInfo(pChzName, s_tink)
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

struct STypeInfoProcedure : public STypeInfo	// tag = tinproc
{
	static const TINK s_tink = TINK_Procedure;

						STypeInfoProcedure(const char * pChzName)
						:STypeInfo(pChzName, s_tink)
						,m_arypTinParams()
						,m_arypTinReturns()
						,m_fHasVarArgs(false)
							{ ; }

	EWC::CAry<STypeInfo *>	m_arypTinParams;
	EWC::CAry<STypeInfo *>	m_arypTinReturns;
	bool					m_fHasVarArgs;

	// BB - need names for named argument matching?
};

struct STypeInfoForwardDecl : public STypeInfo	// tag = tinfwd
{
	static const TINK s_tink = TINK_ForwardDecl;
						STypeInfoForwardDecl(EWC::CAlloc * pAlloc, const char * pChzName)
						:STypeInfo(pChzName, s_tink)
						,m_arypTinReferences(pAlloc)
							{ ; }

	EWC::CDynAry<STypeInfo *>	m_arypTinReferences;
};


// NOTE: just documenting a subtle relationship: CSTVal stores a literal value and is enough to determine 
// what the STypeInfoLiteral *could* be, (ie PTinlitDefault, PTinlitTightest) but the actual type is determined
// by the nodes type passed down after type checkingbigmath

struct STypeInfoLiteral : public STypeInfo // tag = tinlit
{
	static const TINK s_tink = TINK_Literal;
						STypeInfoLiteral()
						:STypeInfo("", s_tink)
						,m_fIsFinalized(false)
						,m_pTinSource(nullptr)
						,m_litty()
							{ ; }
	
	bool			m_fIsFinalized;		// literals are finalized once they are assigned to a concrete (or default) type
	STypeInfo *		m_pTinSource;		// source type (for finalized null pointers or enum literals)
	SLiteralType	m_litty;
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

									STypeInfoStruct(const char * pChzName)
									:STypeInfo(pChzName, s_tink)
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

struct STypeInfoEnum : public STypeInfo	// tag = tinenum
{
	static const TINK s_tink = TINK_Enum;

						STypeInfoEnum(const char * pChzName)
						:STypeInfo(pChzName, s_tink)
						,m_pTinLoose(nullptr)
						,m_bintMin()
						,m_bintMax()
						,m_bintLatest()
						,m_tinstructProduced(pChzName)
							{ ; }

	STypeInfo *			m_pTinLoose;
	SBigInt				m_bintMin;
	SBigInt				m_bintMax;
	SBigInt				m_bintLatest;
	STypeInfoStruct 	m_tinstructProduced;
};

enum ARYK
{
    ARYK_Fixed,
    ARYK_Static,
    ARYK_Dynamic,
};

struct STypeInfoArray : public STypeInfo	// tag = tinary
{
	static const TINK s_tink = TINK_Array;

					STypeInfoArray()
					:STypeInfo("", s_tink)
					,m_pTin(nullptr)
					,m_soaPacking(-1)
					,m_c(0)
					,m_aryk(ARYK_Fixed)
					{ ; }

	STypeInfo *		m_pTin;
	u64				m_c;
	s32				m_soaPacking;	// -1 means no SOA. 0 means no size limit. >0 is AOSOA of that chunk size.
	ARYK			m_aryk;
};

void DeleteTypeInfo(EWC::CAlloc * pAlloc, STypeInfo * pTin);
