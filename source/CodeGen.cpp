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

#include "CodeGen.h"
#include "EwcTypes.h"
#include "JaiParse.h"
#include "JaiTypes.h"
#include "Workspace.h"

using namespace EWC;

#include "llvm-c/Analysis.h"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include <stdio.h>

CIRProcedure * PProcCodegenPrototype(CIRBuilder * pBuild, CSTNode * pStnod);



static inline size_t CChGetTypeString(LLVMOpaqueType * pLtype, char * pCh, const char * pChEnd)
{
	switch (LLVMGetTypeKind(pLtype))
	{
	case LLVMIntegerTypeKind:
		{
			switch (LLVMGetIntTypeWidth(pLtype))
			{
			case 1: return CChCopy("bool", pCh, pChEnd-pCh);
			case 8: return CChCopy("s8", pCh, pChEnd-pCh);
			case 16: return CChCopy("s16", pCh, pChEnd-pCh);
			case 32: return CChCopy("s32", pCh, pChEnd-pCh);
			case 64: return CChCopy("s64", pCh, pChEnd-pCh);
			}
		}
	case LLVMFloatTypeKind:		return CChCopy("f32", pCh, pChEnd-pCh);
	case LLVMDoubleTypeKind:	return CChCopy("f64", pCh, pChEnd-pCh);
	case LLVMVoidTypeKind:		return CChCopy("void", pCh, pChEnd-pCh);
	case LLVMPointerTypeKind:
		{
			size_t cChPrefix = CChCopy("* ", pCh, pChEnd-pCh);
			LLVMOpaqueType * pLtypeElement = LLVMGetElementType(pLtype);
			return cChPrefix + CChGetTypeString(pLtypeElement, pCh+cChPrefix, pChEnd);
		}
	}

	return CChCopy("unknown", pCh, pChEnd-pCh);
}

static inline void DumpLtype(const char * pChzLabel, CIRValue * pVal)
{
	printf("%s: ", pChzLabel);

	auto pLtype = LLVMTypeOf(pVal->m_pLval);
	LLVMDumpType(pLtype);
}



CIRValue::CIRValue(VALK valk)
:m_pLval(nullptr)
,m_pStnod(nullptr)
,m_valk(valk)
{
}



CIRBasicBlock::CIRBasicBlock(EWC::CAlloc * pAlloc)
:m_pLblock(nullptr)
,m_arypInst(pAlloc)
{
}

CIRBasicBlock::~CIRBasicBlock()
{
	m_arypInst.Clear();
}

void CIRBasicBlock::Append(CIRInstruction * pInst)
{
	m_arypInst.Append(pInst);
}

static inline s8 COperand(IROP irop)
{
	if (irop == IROP_Call)
		return 0;
	if (((irop >= IROP_BinaryOpMin) & (irop < IROP_BinaryOpMax)) | 
		((irop >= IROP_CmpOpMin) & (irop < IROP_CmpOpMax)) | 
		((irop >= IROP_LogicOpMin) & (irop < IROP_LogicOpMax)) | 
		(irop == IROP_Store))
		return 2;
	return 1;
}

s8 CIRInstruction::CpValOperandMax()
{
	return COperand(m_irop);
}



CIRProcedure::~CIRProcedure()
{
	size_t cpBlock = m_arypBlockManaged.C();
	for (size_t ipBlock = 0; ipBlock < cpBlock; ++ipBlock)
	{
		m_pAlloc->EWC_DELETE(m_arypBlockManaged[ipBlock]);
	}
	m_arypBlockManaged.Clear();

	size_t cpVal = m_arypValManaged.C();
	for (size_t ipVal = 0; ipVal < cpVal; ++ipVal)
	{
		m_pAlloc->EWC_DELETE(m_arypValManaged[ipVal]);
	}
	m_arypValManaged.Clear();
}



// Builder class Methods
CIRBuilder::CIRBuilder(EWC::CAlloc * pAlloc)
:m_pLbuild(nullptr)
,m_pAlloc(pAlloc)
,m_inspt()
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
,m_hashHvNUnique(pAlloc)
{ 
	m_pLbuild = LLVMCreateBuilder();
	m_pLmoduleCur = LLVMModuleCreateWithName("JaiModule");
}
	
CIRBuilder::~CIRBuilder()
{
	if (m_pLbuild)
	{
		LLVMDisposeBuilder(m_pLbuild);
		m_pLbuild = nullptr;
	}

	if (m_pLmoduleCur)
	{
		LLVMDisposeModule(m_pLmoduleCur);
		m_pLmoduleCur = nullptr;
	}
}

size_t CIRBuilder::CChGenerateUniqueName(const char * pChzIn, char * pChzOut, size_t cChMax)
{
	size_t cChIn = CCh(pChzIn);
	size_t iCh = cChIn - 1;

	// not handling whitespace...
	u32 nIn = 0;
	u32 nMultiple = 1;
	while (iCh >= 0 && ((pChzIn[iCh] >= '0') & (pChzIn[iCh] <= '9')))
	{
		nIn = (pChzIn[iCh] - '0') * nMultiple + nIn;
		nMultiple *= 10;
		--iCh;
	}

	HV hv = 0;
	if (iCh >= 0)
	{
		hv = HvFromPChz(pChzIn, iCh+1);
	}

	u32 * pN = nullptr;
	FINS fins = m_hashHvNUnique.FinsEnsureKey(hv, &pN);
	if (fins == FINS_Inserted)
	{
		*pN = nIn;
		return CChCopy(pChzIn, pChzOut, cChMax);
	}
	else
	{
		*pN = ewcMax(nIn, *pN + 1);
		CChCopy(pChzIn, pChzOut, cChMax);

		size_t iChNumber = iCh+1;
		size_t cChNumber = CChFormat(&pChzOut[iChNumber], cChMax - (iChNumber), "%d", *pN); 	
		return iChNumber + cChNumber;
	}
}

void CIRBuilder::PrintDump()
{
	if (!m_pLmoduleCur)
	{
		printf("No code generated.");
		return;
	}

	LLVMDumpModule(m_pLmoduleCur);
}

CIRInstruction * CIRBuilder::PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	int cpValOperand = COperand(irop);
	size_t cB = sizeof(CIRInstruction) + ewcMax((cpValOperand - 1), 0) * sizeof(CIRValue *);

	CIRInstruction * pInst = (CIRInstruction *)m_pAlloc->EWC_ALLOC(cB, EWC_ALIGN_OF(CIRInstruction));
	new (pInst) CIRInstruction(irop);
	AddManagedVal(pInst);

	pInst->m_apValOperand[0] = pValLhs;
	if (cpValOperand > 1)
	{
		pInst->m_apValOperand[1] = pValRhs;
	}
	else
	{
		EWC_ASSERT(pValRhs == nullptr, "unexpected second operand");
	}
	pInst->m_cpValOperand = cpValOperand;

	return pInst;
}

void CIRBuilder::AddManagedVal(CIRValue * pVal)
{
	if (EWC_FVERIFY(m_pProcCur, "adding managed value with no active procedure"))
	{
		m_pProcCur->m_arypValManaged.Append(pVal);
	}
}

CIRInstruction * CIRBuilder::PInstCreateNAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SAdd : IROP_UAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildAdd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildFAdd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SSub : IROP_USub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildSub(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GSub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildFSub(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SMul : IROP_UMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildMul(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildFMul(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNShr(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate(IROP_Shr, pValLhs, pValRhs, pChzName);

	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
	if (fSigned)
	{
		pInst->m_pLval = LLVMBuildAShr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	else
	{
		pInst->m_pLval = LLVMBuildLShr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNShl(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate(IROP_Shl, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildShl(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNAnd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_And, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildAnd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNOr(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_Or, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildOr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNNeg(CIRValue * pValOperand, const char * pChzName, bool fIsSigned)
{
	CIRInstruction * pInst = PInstCreate(IROP_NNeg, pValOperand, nullptr, pChzName);
	pInst->m_pLval = LLVMBuildNeg(m_pLbuild, pValOperand->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGNeg(CIRValue * pValOperand, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GNeg, pValOperand, nullptr, pChzName);
	pInst->m_pLval = LLVMBuildFNeg(m_pLbuild, pValOperand->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateFNot(CIRValue * pValOperand, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_Not, pValOperand, nullptr, pChzName);
	pInst->m_pLval = LLVMBuildNot(m_pLbuild, pValOperand->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst;
	if (fSigned)
	{
		pInst = PInstCreate(IROP_SDiv, pValLhs, pValRhs, pChzName);
		pInst->m_pLval = LLVMBuildSDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	else
	{
		pInst = PInstCreate(IROP_UDiv, pValLhs, pValRhs, pChzName);
		pInst->m_pLval = LLVMBuildUDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GDiv, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildFDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}
					
CIRInstruction * CIRBuilder::PInstCreateCondBranch(
	CIRValue * pValPred,
	CIRBasicBlock * pBlockTrue,
	CIRBasicBlock * pBlockFalse)
{
	CIRInstruction * pInst = PInstCreate(IROP_CondBranch, pValPred, nullptr, "branch");
	auto pLblockFalse = (pBlockFalse == nullptr) ? nullptr : pBlockFalse->m_pLblock;
	pInst->m_pLval = LLVMBuildCondBr(m_pLbuild, pValPred->m_pLval, pBlockTrue->m_pLblock, pLblockFalse);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNCmp(
	NCMPPRED ncmppred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
#define JAI_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMIntPredicate s_mpNcmpredLpredicate[] =
	{
		NCMPPRED_LIST
	};
#undef JAI_PRED
#undef LLVM_PRED
	EWC_CASSERT(EWC_DIM(s_mpNcmpredLpredicate) == NCMPPRED_Max, "missing elements in int predicate map");
	auto lpredicate = s_mpNcmpredLpredicate[ncmppred];

	CIRInstruction * pInst = PInstCreate(IROP_NCmp, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildICmp(m_pLbuild, lpredicate, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGCmp(
	GCMPPRED gcmppred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
#define JAI_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMRealPredicate s_mpGcmpredLpredicate[] =
	{
		GCMPPRED_LIST
	};
#undef JAI_PRED
#undef LLVM_PRED
	EWC_CASSERT(EWC_DIM(s_mpGcmpredLpredicate) == GCMPPRED_Max, "missing elements in int predicate map");
	auto lpredicate = s_mpGcmpredLpredicate[gcmppred];

	CIRInstruction * pInst = PInstCreate(IROP_GCmp, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = LLVMBuildFCmp(m_pLbuild, lpredicate, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateBranch(CIRBasicBlock * pBlock)
{
	CIRInstruction * pInst = PInstCreate(IROP_Branch, nullptr, nullptr, "branch");
	pInst->m_pLval = LLVMBuildBr(m_pLbuild, pBlock->m_pLblock);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateRet(CIRValue * pValRhs)
{
	CIRInstruction * pInst = PInstCreate(IROP_Ret, pValRhs, nullptr, "RetTmp");
	if (pValRhs)
	{
		pInst->m_pLval = LLVMBuildRet(m_pLbuild, pValRhs->m_pLval);
	}
	else
	{
		pInst->m_pLval = LLVMBuildRetVoid(m_pLbuild);
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateAlloca(LLVMOpaqueType * pLtype, u64 cElement, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_Alloca, nullptr, nullptr, pChzName);
	if (cElement > 1)
	{
		auto pLvalCElement = LLVMConstInt(LLVMInt64Type(), cElement, false);
		pInst->m_pLval = LLVMBuildArrayAlloca(m_pLbuild, pLtype, pLvalCElement, pChzName);
	}
	else
	{
		pInst->m_pLval = LLVMBuildAlloca(m_pLbuild, pLtype, pChzName);
	}

	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGEP(
	CIRValue * pValLhs,
	LLVMOpaqueValue ** apLvalIndices,
	u32 cpIndices,
	const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GEP, nullptr, nullptr, pChzName);
	pInst->m_pLval = LLVMBuildGEP(m_pLbuild, pValLhs->m_pLval, apLvalIndices, cpIndices, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstFromSymbol(SSymbol * pSym)
{
	if (!EWC_FVERIFY(pSym->m_pVal && pSym->m_pVal->m_valk == VALK_Instruction, "expected alloca value for symbol"))
		return nullptr;

	CIRInstruction * pInstSym = (CIRInstruction *)pSym->m_pVal;
	if (!EWC_FVERIFY(pInstSym->m_irop = IROP_Alloca, "expected alloca for symbol"))
		return nullptr;

	return pInstSym;
}

CIRInstruction * CIRBuilder::PInstCreateLoad(CIRValue * pValPT, const char * pChzName)
{
	CIRInstruction * pInstLoad = PInstCreate(IROP_Load, pValPT, nullptr, pChzName);
	pInstLoad->m_pLval = LLVMBuildLoad(m_pLbuild, pValPT->m_pLval, pChzName);
	return pInstLoad;
}

CIRInstruction * CIRBuilder::PInstCreateStore(CIRValue * pValPT, CIRValue * pValT)
{
	//store t into address pointed at by pT

	if (!EWC_FVERIFY(pValPT && pValPT->m_valk == VALK_Instruction, "expected alloca value for symbol"))
		return nullptr;

	CIRInstruction * pInstPT = (CIRInstruction *)pValPT;
	if (!EWC_FVERIFY(pInstPT->m_irop = IROP_Alloca, "expected alloca for symbol"))
		return nullptr;
	
	auto pLtypeT = LLVMTypeOf(pValT->m_pLval);
	auto pLtypePT = LLVMTypeOf(pInstPT->m_pLval);
	bool fIsPointerKind = LLVMGetTypeKind(pLtypePT) == LLVMPointerTypeKind;
	bool fTypesMatch = false;
	if (fIsPointerKind)
	{
		auto pLtypeElem = LLVMGetElementType(pLtypePT);
		fTypesMatch = pLtypeElem == pLtypeT;
	}
	if (!fIsPointerKind || !fTypesMatch)
	{
		printf("bad store information");
		printf("pLtypeT:"); LLVMDumpType(pLtypeT);
		printf("pLtypePT: (dest)"); LLVMDumpType(pLtypePT);
	}

	CIRInstruction * pInstStore = PInstCreate(IROP_Store, pInstPT, pValT, "store");
    pInstStore->m_pLval = LLVMBuildStore(m_pLbuild, pValT->m_pLval, pInstPT->m_pLval);
	return pInstStore;
}

static inline LLVMOpaqueType * PLtypeFromPTin(STypeInfo * pTin, u64 * pCElement = nullptr)
{
	if (!pTin)
		return nullptr;

	if (pCElement)
	{
		if (pTin->m_tink == TINK_Array)
		{
			STypeInfoArray * pTinary = (STypeInfoArray *)pTin;
			*pCElement = pTinary->m_c;
		}
		else
		{
			*pCElement = 1;
		}
	}

	switch (pTin->m_tink)
	{
		case TINK_Void:
		{
			return LLVMVoidType();
		}
		case TINK_Pointer:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTin;
			auto pLtypePointedTo = PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			return LLVMPointerType(pLtypePointedTo, 0);
		}
		case TINK_Array:
		{
			STypeInfoArray * pTinary = (STypeInfoArray *)pTin;
			auto pLtypeElement = PLtypeFromPTin(pTinary->m_pTin);
			return LLVMArrayType(pLtypeElement, u32(pTinary->m_c));
		}
		case TINK_Bool:		return LLVMInt1Type();
	    case TINK_Integer:	
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			switch (pTinint->m_cBit)
			{
			case 8:		return LLVMInt8Type();
			case 16:	return LLVMInt16Type();
			case 32:	return LLVMInt32Type();
			case 64:	return LLVMInt64Type();
			default:	return nullptr;
			}
		}
	    case TINK_Float:
		{
			auto pTinfloat = (STypeInfoFloat *)pTin;
			switch (pTinfloat->m_cBit)
			{
			case 32:	return LLVMFloatType();
			case 64:	return LLVMDoubleType();
			default:	return nullptr;
			}
		}
		case TINK_Literal:
		{
			auto pTinlit = (STypeInfoLiteral *)pTin;
			switch (pTinlit->m_litty.m_litk)
			{
			case LITK_Integer:	return LLVMInt64Type();
			case LITK_Float:	return LLVMDoubleType();
			case LITK_Bool:		return LLVMInt1Type();
			case LITK_Char:		return nullptr;
			case LITK_String:	return LLVMPointerType(LLVMInt8Type(), 0);
			case LITK_Null:		return PLtypeFromPTin(pTinlit->m_pTinptrNull);
			default:			return nullptr;
			}
		}
		case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;

			if (pTinstruct->m_pLtype)
			{
				return pTinstruct->m_pLtype;
			}

			LLVMOpaqueType * pLtype = LLVMStructCreateNamed(LLVMGetGlobalContext(), pTinstruct->m_strName.PChz());
			pTinstruct->m_pLtype = pLtype;

			int cTypememb = (int)pTinstruct->m_aryTypememb.C();
			auto apLtypeMember = (LLVMTypeRef *)(alloca(sizeof(LLVMTypeRef) * cTypememb));
			LLVMTypeRef * ppLtypeMember = apLtypeMember;

			auto pTypemembMac = pTinstruct->m_aryTypememb.PMac();
			for ( auto pTypememb = pTinstruct->m_aryTypememb.A(); pTypememb != pTypemembMac; ++pTypememb)
			{
				auto pLtypeMember = PLtypeFromPTin(pTypememb->m_pTin);
				EWC_ASSERT(pLtypeMember, "failed to compute type for structure member %s", pTypememb->m_strName.PChz());
				*ppLtypeMember++ = pLtypeMember;
			}

			LLVMStructSetBody(pLtype, apLtypeMember, cTypememb, false);
			return pLtype;
		}
		default: return nullptr;
	}
}

void ExtractNumericInfo(STypeInfo * pTin, u32 * pCBit, bool * pFSigned)
{
	// BB - Is there really any good reason not to make one type info for ints, bools and floats?
	switch (pTin->m_tink)
	{
	case TINK_Bool:
		{
			*pCBit = 1;	
			*pFSigned = false;
		} break;
	case TINK_Integer:
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			*pCBit = pTinint->m_cBit;	
			*pFSigned = pTinint->m_fIsSigned;
		} break;
	case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTin;
			*pCBit = pTinfloat->m_cBit;	
			*pFSigned = true;
		} break;
	default: EWC_ASSERT(false, "non-numeric type info");
	}
}

inline LLVMOpaqueValue * PLvalConstantInt(int cBit, bool fIsSigned, u64 nUnsigned)
{
	switch (cBit)
	{
	case 8:	 return LLVMConstInt(LLVMInt8Type(), U8Coerce(nUnsigned & 0xFF), fIsSigned);
	case 16: return LLVMConstInt(LLVMInt16Type(), U16Coerce(nUnsigned & 0xFFFF), fIsSigned);
	case 32: return LLVMConstInt(LLVMInt32Type(), U32Coerce(nUnsigned & 0xFFFFFFFF), fIsSigned);
	case -1: // fall through
	case 64: return LLVMConstInt(LLVMInt64Type(), nUnsigned, fIsSigned);
	default: EWC_ASSERT(false, "unhandled integer size");
		return nullptr;
	}
}

inline LLVMOpaqueValue * PLvalConstantFloat(int cBit, F64 g)
{
	switch (cBit)
	{
	case 32: return LLVMConstReal(LLVMFloatType(), g);
	case 64: return LLVMConstReal(LLVMDoubleType(), g);
	default:	EWC_ASSERT(false, "unhandled float size");
		return nullptr;
	}
}

LLVMOpaqueValue * PLvalZeroInType(CIRBuilder * pBuild, STypeInfo * pTin)
{
	auto pLbuild = pBuild->m_pLbuild;
	switch (pTin->m_tink)
	{
	case TINK_Bool:		return LLVMConstInt(LLVMInt1Type(), 0, false);
	case TINK_Integer:	return PLvalConstantInt(((STypeInfoInteger *)pTin)->m_cBit, false, 0);
	case TINK_Float:	return PLvalConstantFloat(((STypeInfoFloat *)pTin)->m_cBit, 0.0);
	case TINK_Pointer:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTin;
			auto * pLtype = PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			if (pLtype)
				return LLVMConstNull(LLVMPointerType(pLtype, 0));
		} break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
			auto pLvalElement = PLvalZeroInType(pBuild, pTinary->m_pTin);

			auto apLval = (LLVMValueRef *)pBuild->m_pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, pTinary->m_c);

			auto pLvalZero = PLvalZeroInType(pBuild, pTinary->m_pTin);
			EWC_ASSERT(pLvalZero != nullptr, "expected zero value");

			for (u64 iElement = 0; iElement < pTinary->m_c; ++iElement)
			{
				apLval[iElement] = pLvalZero;
			}

			LLVMOpaqueType * pLtypeElement = PLtypeFromPTin(pTinary->m_pTin);
			auto pLvalReturn = LLVMConstArray(pLtypeElement, apLval, u32(pTinary->m_c));
			pBuild->m_pAlloc->EWC_DELETE(apLval);
			return pLvalReturn;
		}
	case TINK_Struct:
	{
		auto pTinstruct = (STypeInfoStruct *)pTin;

		int cpLvalMember = (int)pTinstruct->m_aryTypememb.C();
		size_t cB = sizeof(LLVMOpaqueValue *) * cpLvalMember;
		auto apLvalMember = (LLVMOpaqueValue **)(alloca(cB));

		for (int ipLval = 0; ipLval < cpLvalMember; ++ipLval)
		{
			apLvalMember[ipLval] = PLvalZeroInType(pBuild, pTinstruct->m_aryTypememb[ipLval].m_pTin);
		}

		return LLVMConstNamedStruct(pTinstruct->m_pLtype, apLvalMember, cpLvalMember);
	}

	default: break;
	}

	return nullptr;
}

LLVMOpaqueValue * PLvalFromLiteral(CIRBuilder * pBuild, STypeInfoLiteral * pTinlit, CSTValue * pStval)
{
	LLVMOpaqueValue * pLval = nullptr;
	switch (pTinlit->m_litty.m_litk)
	{
	case LITK_Integer:
		{
			EWC_ASSERT(
				(pStval->m_stvalk == STVALK_UnsignedInt) | (pStval->m_stvalk == STVALK_SignedInt),
				"bad literal value");

			pLval = PLvalConstantInt(pTinlit->m_litty.m_cBit, pTinlit->m_litty.m_fIsSigned, pStval->m_nUnsigned);
		}break;
	case LITK_Float:
		{
			pLval = PLvalConstantFloat(pTinlit->m_litty.m_cBit, pStval->m_g);
		}break;
	case LITK_Bool:
	{
			if (pStval->m_stvalk == STVALK_ReservedWord)
			{
				EWC_ASSERT(
					((pStval->m_nUnsigned == 1) & (pStval->m_rword == RWORD_True)) | 
					((pStval->m_nUnsigned == 0) & (pStval->m_rword == RWORD_False)), "bad boolean reserved word");
			}
			else
			{
				EWC_ASSERT(pStval->m_stvalk == STVALK_UnsignedInt, "bad literal value");
			}

			pLval = LLVMConstInt(LLVMInt1Type(), pStval->m_nUnsigned, false);
		} break;
	case LITK_Char:		EWC_ASSERT(false, "TBD"); return nullptr;
	case LITK_String:
		{
			if (!EWC_FVERIFY(pStval->m_stvalk == STVALK_String, "bad value in string literal"))
				return nullptr;

			// string literals aren't really constants in the eyes of llvm, but it'll work for now
			pLval = LLVMBuildGlobalStringPtr(pBuild->m_pLbuild, pStval->m_str.PChz(), "strlit");
		} break;
	case LITK_Null:
		{
			auto pLtype = PLtypeFromPTin(pTinlit->m_pTinptrNull);
			if (!EWC_FVERIFY(pLtype, "could not find llvm type for null pointer"))
				return nullptr;

			pLval = LLVMConstNull(pLtype);
		}
	}

	EWC_ASSERT(pLval, "unknown LITK in PLValueFromLiteral");
	return pLval;
}

inline CIRInstruction * PInstCreateCast(
							CIRBuilder * pBuild,
							IROP irop,
							CIRValue * pValSrc,
							const char * pChz,
							LLVMOpaqueValue * pLval)
{
	CIRInstruction * pInst = pBuild->PInstCreate(irop, pValSrc, nullptr, pChz); \
	pInst->m_pLval = pLval;
	return pInst;
}

CIRValue * PValCreateCast(CIRBuilder * pBuild, CIRValue * pValSrc, STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
#define PINST_CREATE_CAST(IROP, CREATE_FUNC, PCHZ) \
	PInstCreateCast(pBuild, IROP, pValSrc, PCHZ, CREATE_FUNC(pBuild->m_pLbuild, pValSrc->m_pLval, pLtypeDst, PCHZ));

	u32 cBitSrc = 0;
	u32 cBitDst;
	bool fSignedSrc = false;
	bool fSignedDst;
	if (pTinSrc->m_tink == TINK_Literal)
		return pValSrc;
	if (pTinSrc->m_tink == TINK_Pointer)
	{
		if (pTinDst->m_tink != TINK_Bool)
		{
			if (EWC_FVERIFY(pTinDst->m_tink == TINK_Pointer, "trying to cast pointer to non-pointer. (not supported yet)"))
				return pValSrc;
		}
	}
	else
	{
		ExtractNumericInfo(pTinSrc, &cBitSrc, &fSignedSrc);
	}
	ExtractNumericInfo(pTinDst, &cBitDst, &fSignedDst);
	auto pLtypeDst = PLtypeFromPTin(pTinDst);

	CIRInstruction * pInst = nullptr;
	switch (pTinDst->m_tink)
	{
	case TINK_Integer:
		{
			switch (pTinSrc->m_tink)
			{
			case TINK_Integer: // fall through
			case TINK_Bool:
				{
					if (cBitDst < cBitSrc)				{ return PINST_CREATE_CAST(IROP_NTrunc, LLVMBuildTrunc, "NTrunc"); }
					else if (fSignedSrc & fSignedDst)	{ return PINST_CREATE_CAST(IROP_SignExt, LLVMBuildSExt, "SignExt"); }
					else								{ return PINST_CREATE_CAST(IROP_ZeroExt, LLVMBuildZExt, "ZeroExt"); }
				} break;
			case TINK_Float:
				{
					if (fSignedSrc) { return PINST_CREATE_CAST(IROP_GToS, LLVMBuildFPToSI, "GToS"); }
					else			{ return PINST_CREATE_CAST(IROP_GToU, LLVMBuildFPToUI, "GToU"); }
				} break;
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			}
		} break;
	case TINK_Float:
		{
			switch (pTinSrc->m_tink)
			{
			case TINK_Integer: // fall through
			case TINK_Bool:
				{
					if (fSignedSrc) { return PINST_CREATE_CAST(IROP_SToG, LLVMBuildSIToFP, "SToG"); }
					else			{ return PINST_CREATE_CAST(IROP_UToG, LLVMBuildUIToFP, "UToG"); }
				}
			case TINK_Float:
					if (cBitDst > cBitSrc)	{ return PINST_CREATE_CAST(IROP_GExtend, LLVMBuildFPExt, "GExtend"); }
					else					{ return PINST_CREATE_CAST(IROP_GTrunc, LLVMBuildFPTrunc, "GTrunc"); }
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			}

		}break;
	case TINK_Bool:
			switch (pTinSrc->m_tink)
			{
			case TINK_Bool:	
				return pValSrc;
			case TINK_Integer:
				{
					auto pLvalZero = PLvalZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreate(IROP_NCmp, pValSrc, nullptr, "NCmp"); 
					pInst->m_pLval = LLVMBuildICmp(pBuild->m_pLbuild, LLVMIntNE, pValSrc->m_pLval, pLvalZero, "NToBool"); 
					return pInst;
				} 
			case TINK_Float:
				{
					auto pLvalZero = PLvalZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreate(IROP_GCmp, pValSrc, nullptr, "GCmp");
					pInst->m_pLval = LLVMBuildFCmp(pBuild->m_pLbuild, LLVMRealONE, pValSrc->m_pLval, pLvalZero, "GToBool");
					return pInst;
				}
			case TINK_Pointer:
				{
					auto pLvalZero = PLvalZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreate(IROP_GCmp, pValSrc, nullptr, "NCmp");
					pInst->m_pLval = LLVMBuildICmp(pBuild->m_pLbuild, LLVMIntNE, pValSrc->m_pLval, pLvalZero, "PToBool");
					return pInst;
				}
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			} break;
	case TINK_Literal:
		EWC_ASSERT(false, "can't cast to literal");
		return nullptr;
	default:
		EWC_ASSERT(false, "unsupported cast destination type in PValCreateCast");
	}
#undef PINST_CREATE_CAST

	return pValSrc;
}

static inline LLVMOpaqueValue * PLvalDefaultInitializer( CIRBuilder * pBuild, STypeInfo * pTin)
{
	LLVMOpaqueValue * pLval;
	if (pTin->m_tink == TINK_Struct)
	{
		auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pTin);

		int cpLvalMember = (int)pTinstruct->m_aryTypememb.C();
		size_t cB = sizeof(LLVMOpaqueValue *) * cpLvalMember;
		auto apLvalMember = (LLVMOpaqueValue **)(alloca(cB));
		EWC::ZeroAB(apLvalMember, cB);

		bool fHasConstInitializer = true;
		CSTNode * pStnodStruct = pTinstruct->m_pStnodStruct;
		EWC_ASSERT(pStnodStruct, "missing definition in struct type info");
		CSTNode * pStnodList = pStnodStruct->PStnodChildSafe(1);

		if (pStnodList && EWC_FVERIFY(pStnodList->m_park == PARK_List, "expected member list"))
		{
			int cpStnod = pStnodList->CStnodChild();
			EWC_ASSERT(cpStnod == cpLvalMember, "AST decl mismatch for members");
			for (int ipStnod = 0; ipStnod < cpStnod; ++ipStnod)
			{
				CSTNode * pStnodDecl = pStnodList->PStnodChild(ipStnod);
				if (!EWC_FVERIFY(pStnodDecl->m_park == PARK_Decl && pStnodDecl->m_pStdecl, "expected decl"))
					continue;

				auto pStdecl = pStnodDecl->m_pStdecl; 
				auto pStnodInit = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
				if (pStnodInit)
				{
					if (pStnodInit->m_pTin && pStnodInit->m_pTin->m_tink == TINK_Literal)
					{
						apLvalMember[ipStnod] = PLvalFromLiteral(
													pBuild,
													(STypeInfoLiteral*)pStnodInit->m_pTin,
													pStnodInit->m_pStval);
					}
					else
					{
						fHasConstInitializer = false;
					}
				}
			}
		}

		if (fHasConstInitializer)
		{
			// This method doesn't allow for uninitializing part of our struct

			for (int ipLval = 0; ipLval < cpLvalMember; ++ipLval)
			{
				if (!apLvalMember[ipLval])
				{
					apLvalMember[ipLval] = PLvalZeroInType(pBuild, pTinstruct->m_aryTypememb[ipLval].m_pTin);
				}
			}

			pLval = LLVMConstNamedStruct(pTinstruct->m_pLtype, apLvalMember, cpLvalMember);
		}
		else
		{
			// create an init function and call it
			EWC_ASSERT(false, "tbd");
		}
	}
	else if (pTin->m_tink == TINK_Array)
	{
		auto pTinary = (STypeInfoArray *)pTin;
		auto pLvalElement = PLvalZeroInType(pBuild, pTinary->m_pTin);

		size_t cB = sizeof(LLVMOpaqueValue *) * pTinary->m_c;
		auto apLval = (LLVMOpaqueValue **)(alloca(cB));

		auto pLvalZero = PLvalDefaultInitializer(pBuild, pTinary->m_pTin);
		EWC_ASSERT(pLvalZero != nullptr, "expected zero value");

		for (u64 iElement = 0; iElement < pTinary->m_c; ++iElement)
		{
			apLval[iElement] = pLvalZero;
		}

		LLVMOpaqueType * pLtypeElement = PLtypeFromPTin(pTinary->m_pTin);
		pLval = LLVMConstArray(pLtypeElement, apLval, u32(pTinary->m_c));
	}
	else
	{
		pLval = PLvalZeroInType(pBuild, pTin);
	}

	if (!EWC_FVERIFY(pLval, "unexpected type in PValGenerateDefaultInitializer"))
		return nullptr;

	return pLval;
}

static inline CIRValue * PValGenerateRefCast(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodRhs, STypeInfo * pTinOut)
{
	STypeInfo * pTinRhs = pStnodRhs->m_pTin;
	if ( pTinRhs && pTinRhs->m_tink == TINK_Array)
	{
		CIRValue * pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Reference);

		// special case for assigning arrays to pointers, need reference to the array type.
		if (pTinRhs->m_tink == TINK_Array && pTinOut->m_tink == TINK_Pointer)
		{
			LLVMOpaqueValue * apLvalIndex[2] = {};
			apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);
			apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 0, false);

			return pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, EWC_DIM(apLvalIndex), "aryGep");
		}

		CIRValue * pValSrc = pBuild->PInstCreateLoad(pValRhsRef, "castLoad");
		return PValCreateCast(pBuild, pValSrc, pTinRhs, pTinOut);
	}

	CIRValue * pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);
	return PValCreateCast(pBuild, pValRhs, pTinRhs, pTinOut);
}

CIRValue * PValGenerate(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod, VALGENK valgenk)
{
	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
	{
		CSTValue * pStval = pStnod->m_pStval;
		STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnod->m_pTin;
		if (!pStval || !pTinlit || pTinlit->m_tink != TINK_Literal)
			return nullptr;

		if (!EWC_FVERIFY(pTinlit->m_fIsFinalized, "non-finalized literal type encountered during code gen"))
			return nullptr;
	
		auto pLval = PLvalFromLiteral(pBuild, pTinlit, pStval);
		if (!pLval)
			return nullptr;

		CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
		pBuild->AddManagedVal(pConst);
		pConst->m_pLval = pLval;
		return pConst;
	}

	switch (pStnod->m_park)
	{
	case PARK_List:
		{
			int cStnodChild = pStnod->CStnodChild();
			for (int iStnodChild = 0; iStnodChild < cStnodChild; ++iStnodChild)
			{
				CIRValue * pVal = PValGenerate(pWork, pBuild, pStnod->PStnodChild(iStnodChild), VALGENK_Instance);
			}

		}break;
	case PARK_Decl:
		{
			CSTDecl * pStdecl = pStnod->m_pStdecl;
			if (!pStdecl || !EWC_FVERIFY(pStnod->m_pSym, "declaration without symbol"))
				return nullptr;

			u64 cElement;
			auto pLtype = PLtypeFromPTin(pStnod->m_pTin, &cElement);
			if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
				return nullptr;

			auto * pInstAlloca = pBuild->PInstCreateAlloca(pLtype, cElement, pStnod->m_pSym->m_strName.PChz());
			pStnod->m_pSym->m_pVal = pInstAlloca;
			
			// need to generate code for local var, just running init, leg for now.
			if (pStdecl->m_iStnodInit >= 0)
			{
				CSTNode * pStnodRhs = pStnod->PStnodChild(pStdecl->m_iStnodInit);
				if (pStnodRhs->m_park != PARK_Uninitializer)
				{
					CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pStnod->m_pTin);
					pBuild->PInstCreateStore(pInstAlloca, pValRhsCast);
				}
			}
			else
			{
				auto pLvalDefault = PLvalDefaultInitializer(pBuild, pStnod->m_pTin);	
				if (pLvalDefault)
				{
					CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
					pConst->m_pLval = pLvalDefault;
					pBuild->AddManagedVal(pConst);
					return pConst;

					pBuild->PInstCreateStore(pInstAlloca, pConst);
				}
			}

		} break;
	case PARK_StructDefinition:
		{
			// nothing to do here yet...
		} break;
	case PARK_Literal:
		{
			EWC_ASSERT(false, "encountered literal AST node during codegen, should have encountered literal type first");
		} break;
	case PARK_ReservedWord:
		{
			if (!EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
				return nullptr;

			RWORD rword = pStnod->m_pStval->m_rword;
			switch (rword)
			{
			case RWORD_If:
				{

					//		NCmp
					//		CondBr then, elifA
					//	}
					//	{ then:
					//		...do stuff
					//		Br post
					//	}
					//	{ :elifA
					//		NCmp	
					//		CondBr thenA, else
					//	}
					//	{ :thenA
					//		...do stuff A	
					//		Br post
					//	}
					//	{ :else
					//		...do stuff C
					//		Br post
					//	}
					//	{ :post

					if (pStnod->CStnodChild() < 2)
						return nullptr;

					CIRProcedure * pProc = pBuild->m_pProcCur;
					CIRBasicBlock * pBlockPost = nullptr;	
					CSTNode * pStnodCur = pStnod;

					while (pStnodCur)
					{
						// BB - should handle declarations inside conditional statement? ie, does it need a new block

						CSTNode * pStnodIf = pStnodCur;
						CSTNode * pStnodPred = pStnodIf->PStnodChild(0);
						CIRValue * pValPred = PValGenerate(pWork, pBuild, pStnodPred, VALGENK_Instance);

						STypeInfo * pTinBool = pStnodIf->m_pTin;
						EWC_ASSERT(pTinBool->m_tink == TINK_Bool, "expected bool type for if");
						CIRValue * pValPredCast = PValCreateCast(pBuild, pValPred, pStnodPred->m_pTin, pTinBool);

						CIRBasicBlock *	pBlockTrue = pBuild->PBlockCreate(pProc, "ifThen");
						CIRBasicBlock * pBlockFalse = nullptr;
						pStnodCur = nullptr;
						CSTNode * pStnodElseChild = nullptr;
						if (pStnodIf->CStnodChild() == 3)
						{
							CSTNode * pStnodElse = pStnodIf->PStnodChild(2);
							if (FIsReservedWord(pStnodElse, RWORD_Else))
							{
								CSTNode * pStnodChild = nullptr;
								if (EWC_FVERIFY(pStnodElse->CStnodChild() == 1, "expected one child for else statement")) //statement or list
								{
									pStnodChild = pStnodElse->PStnodChild(0);
								}

								if (pStnodChild && FIsReservedWord(pStnodChild, RWORD_If))
								{
									pBlockFalse = pBuild->PBlockCreate(pProc, "ifElif");
									pStnodCur = pStnodChild;
								}
								else
								{
									pBlockFalse = pBuild->PBlockCreate(pProc, "ifElse");
									pStnodElseChild = pStnodChild;
								}
							}
						}

						if (!pBlockPost)
						{
							pBlockPost = pBuild->PBlockCreate(pProc, "postIf");
							if (!pBlockFalse)
								pBlockFalse = pBlockPost;
						}

						(void) pBuild->PInstCreateCondBranch(pValPredCast, pBlockTrue, pBlockFalse);

						pBuild->ActivateBlock(pBlockTrue);
						auto pValThen = PValGenerate(pWork, pBuild, pStnodIf->PStnodChild(1), VALGENK_Instance);
						(void) pBuild->PInstCreateBranch(pBlockPost);	
						pBlockTrue = pBuild->m_pBlockCur;	// could have changed during this' codegen

						if (pBlockFalse != pBlockPost)
						{
							pBuild->ActivateBlock(pBlockFalse);
						}

						if (pStnodElseChild)
						{
							auto pValThen = PValGenerate(pWork, pBuild, pStnodElseChild, VALGENK_Instance);
							pBlockFalse = pBuild->m_pBlockCur;	// could have changed during else's codegen

							(void) pBuild->PInstCreateBranch(pBlockPost);	
						}
					}

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_Else:
				EWC_ASSERT(false, "Else reserved word should be handled during codegen for if");
				return nullptr;
			case RWORD_While:
				{
					if (pStnod->CStnodChild() < 2)
						return nullptr;

					CIRProcedure * pProc = pBuild->m_pProcCur;

					CIRBasicBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "wpred");
					CIRBasicBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "wbody");
					CIRBasicBlock * pBlockPost = pBuild->PBlockCreate(pProc, "wpost");
					(void) pBuild->PInstCreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPred);

					// BB - should handle declarations inside conditional statement? ie, does it need a new block

					CSTNode * pStnodWhile = pStnod;
					CSTNode * pStnodPred = pStnodWhile->PStnodChild(0);
					CIRValue * pValPred = PValGenerate(pWork, pBuild, pStnodPred, VALGENK_Instance);

					STypeInfo * pTinBool = pStnodWhile->m_pTin;
					EWC_ASSERT(pTinBool->m_tink == TINK_Bool, "expected bool type for while predicate");
					CIRValue * pValPredCast = PValCreateCast(pBuild, pValPred, pStnodPred->m_pTin, pTinBool);

					(void) pBuild->PInstCreateCondBranch(pValPredCast, pBlockBody, pBlockPost);

					pBuild->ActivateBlock(pBlockBody);
					(void) PValGenerate(pWork, pBuild, pStnodWhile->PStnodChild(1), VALGENK_Instance);
					(void) pBuild->PInstCreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_Return:
				{
					CIRValue * pValRhs = nullptr;
					if (pStnod->CStnodChild() == 1)
					{
						pValRhs = PValGenerate(pWork, pBuild, pStnod->PStnodChild(0), VALGENK_Instance);
					}

					CIRInstruction * pInstRet = pBuild->PInstCreateRet(pValRhs);
					if (EWC_FVERIFY(pBuild->m_pBlockCur, "no current block"))
					{
						pBuild->m_pBlockCur->Append(pInstRet);
					}
				}break;
			default:
				EWC_ASSERT(false, "Unhandled reserved word in code gen");
				break;
			}
		} break;
	case PARK_ProcedureCall:
		{
			if (!EWC_FVERIFY(pStnod->m_pSym, "calling function without generated code"))
				return nullptr;

			if (!pStnod->m_pSym->m_pVal)
			{
				(void) PProcCodegenPrototype(pBuild, pStnod->m_pSym->m_pStnodDefinition);

				if (!pStnod->m_pSym->m_pVal)
					return nullptr;
			}

			CIRProcedure * pProc = (CIRProcedure *)pStnod->m_pSym->m_pVal;
			if (!EWC_FVERIFY(pProc->m_valk == VALK_ProcedureDefinition, "expected procedure value type"))
				return nullptr;

			auto pLvalFunction = pProc->m_pLvalFunction;
			int cStnodArgs = pStnod->CStnodChild();
			if (!EWC_FVERIFY(LLVMCountParams(pLvalFunction) != cStnodArgs, "unexpected number of arguments"))
				return nullptr;

			CDynAry<LLVMValueRef> arypLvalArgs(pBuild->m_pAlloc);
			for (int iStnodChild = 1; iStnodChild < cStnodArgs; ++iStnodChild)
			{
				CIRValue * pVal = PValGenerate(pWork, pBuild, pStnod->PStnodChild(iStnodChild), VALGENK_Instance);

				CSTNode * pStnodCall = pStnod->PStnodChild(iStnodChild);
				CIRValue * pValRhsCast = PValCreateCast(pBuild, pVal, pStnodCall->m_pTinOperand, pStnodCall->m_pTin);

				arypLvalArgs.Append(pValRhsCast->m_pLval);
				if (!EWC_FVERIFY(*arypLvalArgs.PLast(), "missing argument value"))
					return 0;
			}

			CIRInstruction * pInst = pBuild->PInstCreate(IROP_Call, nullptr, nullptr, "RetTmp");
			pInst->m_pLval = LLVMBuildCall(
								pBuild->m_pLbuild,
								pProc->m_pLvalFunction,
								arypLvalArgs.A(),
								(u32)arypLvalArgs.C(),
								"");
			return pInst;
		}
	case PARK_Identifier:
		{
			CIRInstruction * pInst = pBuild->PInstFromSymbol(pStnod->m_pSym);
			if (EWC_FVERIFY(pInst, "unknown identifier in codegen") && valgenk != VALGENK_Reference)
			{
				return pBuild->PInstCreateLoad(pInst, pStnod->m_pSym->m_strName.PChz());
			}
			return pInst;
		}
	case PARK_MemberLookup:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			STypeInfoStruct * pTinstruct = nullptr;
			auto pTinLhs = pStnodLhs->m_pTin;

			VALGENK valgenkLhs = VALGENK_Reference;
			if (pTinLhs)
			{
				if (pTinLhs->m_tink == TINK_Pointer)
				{
					pTinLhs = ((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo;
					valgenkLhs = VALGENK_Instance;
				}

				pTinstruct = PTinRtiCast<STypeInfoStruct *>(pTinLhs);
			}
			CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, valgenkLhs);
			
			CString strMemberName = StrFromIdentifier(pStnod->PStnodChild(1));
			int iTypememb = ITypemembLookup(pTinstruct, strMemberName);
			if (!EWC_FVERIFY(iTypememb >= 0, "cannot find structure member %s", strMemberName.PChz()))
				return nullptr;

			LLVMOpaqueValue * apLvalIndex[3] = {};
			int cpLvalIndex = 0;
			apLvalIndex[cpLvalIndex++] = LLVMConstInt(LLVMInt32Type(), 0, false);
			apLvalIndex[cpLvalIndex++] = LLVMConstInt(LLVMInt32Type(), iTypememb, false);
			auto pInst = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, cpLvalIndex, "aryGep");

			if (EWC_FVERIFY(pInst, "member dereference failure in codegen") && valgenk != VALGENK_Reference)
			{
				pInst = pBuild->PInstCreateLoad(pInst, "membLoad");
			}
			return pInst;
		}
	case PARK_ArrayElement:
		{ 
			if (!EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected (array, index) for array element node"))
				return nullptr;

			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodIndex = pStnod->PStnodChild(1);

			CIRValue * pValLhs;
			CIRValue * pValIndex = PValGenerate(pWork, pBuild, pStnodIndex, VALGENK_Instance);
			if (!EWC_FVERIFY(pValIndex->m_pLval, "null index llvm value"))
				return nullptr;

			LLVMOpaqueValue * apLvalIndex[2] = {};
			TINK tinkLhs = pStnodLhs->m_pTin->m_tink;
			int cpLvalIndex;
			if (tinkLhs == TINK_Array)
			{
				pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);
				apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);
				apLvalIndex[1] = pValIndex->m_pLval;
				cpLvalIndex = 2;
			}
			else if (tinkLhs == TINK_Pointer)
			{
				pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Instance);
				apLvalIndex[0] = pValIndex->m_pLval;
				cpLvalIndex = 1;
			}
			else
			{
				EWC_ASSERT(false, "unexpected type on left hand side of array element");
				return nullptr;
			}

			CIRInstruction * pInst = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, cpLvalIndex, "aryGep");

			if (EWC_FVERIFY(pInst, "unknown identifier in codegen") && valgenk != VALGENK_Reference)
			{
				return pBuild->PInstCreateLoad(pInst, "aryLoad");
			}
			return pInst;
		} break;
	case PARK_AssignmentOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);
			CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pStnod->m_pTin);

			if (!EWC_FVERIFY((pStnodLhs != nullptr) & (pValRhsCast != nullptr), "null operand"))
				return nullptr;

			if (!EWC_FVERIFY(pValLhs->m_pLval != nullptr, "null llvm operand") || 
				!EWC_FVERIFY(pValRhsCast->m_pLval != nullptr, "null llvm operand"))
				return nullptr;

			auto pInstOp = pBuild->PInstCreateStore(pValLhs, pValRhsCast);
			if ( EWC_FVERIFY(pBuild->m_pBlockCur, "missing current block"))
			{
				pBuild->m_pBlockCur->Append(pInstOp);
			}

			return pInstOp;
		}
	case PARK_AdditiveOp:
	case PARK_MultiplicativeOp:
	case PARK_ShiftOp:
	case PARK_BitwiseAndOrOp:
	case PARK_RelationalOp:
	case PARK_EqualityOp:
	case PARK_LogicalAndOrOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);
			CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Instance);

			STypeInfo * pTinOutput = pStnod->m_pTinOperand;
			CIRValue * pValLhsCast = PValGenerateRefCast(pWork, pBuild, pStnodLhs, pTinOutput);
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pTinOutput);
			if (!EWC_FVERIFY((pValLhsCast != nullptr) & (pValRhsCast != nullptr), "null operand"))
				return nullptr;

			if (!EWC_FVERIFY((pValLhsCast->m_pLval != nullptr) & (pValRhsCast->m_pLval != nullptr), "null llvm operand"))
				return nullptr;

			STypeInfo * pTinLhs = pStnodLhs->m_pTin;
			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), "bad cast"))
				return nullptr;
	
			CIRInstruction * pInstOp = nullptr;

			bool fIsSigned = true;
			TINK tink = pTinOutput->m_tink;
			if (pTinOutput->m_tink == TINK_Literal)
			{
				STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTinOutput;
				fIsSigned = pTinlit->m_litty.m_fIsSigned;
				switch (pTinlit->m_litty.m_litk)
				{
				case LITK_Integer:	tink = TINK_Integer;	break;
				case LITK_Float:	tink = TINK_Float;		break;
				default:			tink = TINK_Nil;
				}
			}
			else if (pTinOutput->m_tink == TINK_Integer)
			{
				fIsSigned = ((STypeInfoInteger *)pTinOutput)->m_fIsSigned;
			}

			switch (tink)
			{
			case TINK_Bool:
				switch (pStnod->m_jtok)
				{
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhsCast, pValRhsCast, "NCmpEq"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhsCast, pValRhsCast, "NCmpNq"); break;
				} break;
			case TINK_Integer:
				switch (pStnod->m_jtok)
				{
					case '+': 				pInstOp = pBuild->PInstCreateNAdd(pValLhsCast, pValRhsCast, "nAddTmp", fIsSigned); break;
					case '-': 				pInstOp = pBuild->PInstCreateNSub(pValLhsCast, pValRhsCast, "nSubTmp", fIsSigned); break;
					case '*': 				pInstOp = pBuild->PInstCreateNMul(pValLhsCast, pValRhsCast, "nMulTmp", fIsSigned); break;
					case '/': 				pInstOp = pBuild->PInstCreateNDiv(pValLhsCast, pValRhsCast, "nDivTmp", fIsSigned); break;
					case '&':				pInstOp = pBuild->PInstCreateNAnd(pValLhsCast, pValRhsCast, "nAndTmp"); break;
					case '|':				pInstOp = pBuild->PInstCreateNOr(pValLhsCast, pValRhsCast, "nOrTmp"); break;
					case JTOK_ShiftRight:	pInstOp = pBuild->PInstCreateNShr(pValLhsCast, pValRhsCast, "nShrTmp", fIsSigned); break;
					case JTOK_ShiftLeft:	pInstOp = pBuild->PInstCreateNShl(pValLhsCast, pValRhsCast, "nShlTmp", fIsSigned); break;
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhsCast, pValRhsCast, "NCmpEq"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhsCast, pValRhsCast, "NCmpNq"); break;
					case JTOK_LessEqual:
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSLE, pValLhsCast, pValRhsCast, "NCmpSLE");
						else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpULE, pValLhsCast, pValRhsCast, "NCmpULE");
						break;
					case JTOK_GreaterEqual:
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSGE, pValLhsCast, pValRhsCast, "NCmpSGE");
						else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpUGE, pValLhsCast, pValRhsCast, "NCmpUGE");
						break;
					case '<':
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSLT, pValLhsCast, pValRhsCast, "NCmpSLT");
						else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpULT, pValLhsCast, pValRhsCast, "NCmpULT");
						break;
					case '>':
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSGT, pValLhsCast, pValRhsCast, "NCmpSGT");
						else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpUGT, pValLhsCast, pValRhsCast, "NCmpUGT");
						break;
				} break;
			case TINK_Float:
				switch (pStnod->m_jtok)
				{
					case '+': 				pInstOp = pBuild->PInstCreateGAdd(pValLhsCast, pValRhsCast, "gAddTmp"); break;
					case '-': 				pInstOp = pBuild->PInstCreateGSub(pValLhsCast, pValRhsCast, "gSubTmp"); break;
					case '*': 				pInstOp = pBuild->PInstCreateGMul(pValLhsCast, pValRhsCast, "gMulTmp"); break;
					case '/': 				pInstOp = pBuild->PInstCreateGDiv(pValLhsCast, pValRhsCast, "gDivTmp"); break;
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOEQ, pValLhsCast, pValRhsCast, "NCmpOEQ"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpONE, pValLhsCast, pValRhsCast, "NCmpONE"); break;
					case JTOK_LessEqual:	pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOLE, pValLhsCast, pValRhsCast, "NCmpOLE"); break;
					case JTOK_GreaterEqual:	pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOGE, pValLhsCast, pValRhsCast, "NCmpOGE"); break;
					case '<': 				pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOLT, pValLhsCast, pValRhsCast, "NCmpOLT"); break;
					case '>': 				pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOGT, pValLhsCast, pValRhsCast, "NCmpOGT"); break;
				} break;
			case TINK_Pointer:
				switch (pStnod->m_jtok)
				{
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhsCast, pValRhsCast, "NCmpEq"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhsCast, pValRhsCast, "NCmpNq"); break;
					case '-': 				
						pValRhsCast = pBuild->PInstCreateNNeg(pValRhsCast, "NNeg", fIsSigned);
						// fallthrough
					case '+': 				
					{
						LLVMOpaqueValue * pLvalIndex = pValRhsCast->m_pLval;
						pInstOp = pBuild->PInstCreateGEP(pValLhsCast, &pLvalIndex, 1, "ptrAdd"); break;
					} break;
				}
			default: 
				break;
			}

			if (EWC_FVERIFY(pInstOp, "%s operator unsupported in codegen", PChzFromJtok(pStnod->m_jtok)) &
				EWC_FVERIFY(pBuild->m_pBlockCur, "missing current block"))
			{
				pBuild->m_pBlockCur->Append(pInstOp);
			}

			return pInstOp;
		}

	case PARK_PostfixUnaryOp:
	case PARK_UnaryOp:
		{
			CSTNode * pStnodOperand = pStnod->PStnodChild(0);

			if (pStnod->m_jtok == JTOK_Reference)
			{
				return PValGenerate(pWork, pBuild, pStnodOperand, VALGENK_Reference);
			}

			JTOK jtok = pStnod->m_jtok;
			VALGENK valgenkUnary = ((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) ? VALGENK_Reference : VALGENK_Instance;
			CIRValue * pValOperand = PValGenerate(pWork, pBuild, pStnodOperand, valgenkUnary);
			if (!EWC_FVERIFY(pValOperand != nullptr, "null operand"))
				return nullptr;

			if (!EWC_FVERIFY(pValOperand->m_pLval != nullptr, "null llvm operand"))
				return nullptr;

			STypeInfo * pTinOutput = pStnod->m_pTinOperand;
			STypeInfo * pTinOperand = pStnodOperand->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinOperand != nullptr), "bad cast"))
				return nullptr;
	
			CIRValue * pValOp = nullptr;

			bool fIsSigned = true;
			TINK tink = pTinOutput->m_tink;
			if (pTinOutput->m_tink == TINK_Literal)
			{
				STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTinOutput;
				fIsSigned = pTinlit->m_litty.m_fIsSigned;
				switch (pTinlit->m_litty.m_litk)
				{
				case LITK_Integer:	tink = TINK_Integer;	break;
				case LITK_Float:	tink = TINK_Float;		break;
				default:			tink = TINK_Nil;
				}
			}
			else if (pTinOutput->m_tink == TINK_Integer)
			{
				fIsSigned = ((STypeInfoInteger *)pTinOutput)->m_fIsSigned;
			}

			switch (pStnod->m_jtok)
			{
			case '!':				
				{
					EWC_ASSERT(tink == TINK_Bool, "expected value cannot be cast to bool for '!' operand");

					// OPTIMIZE: could probably save an instruction here by not comparing (for the cast to bool)
					//  then inverting with a FNot

					CIRValue * pValOperandCast = PValCreateCast(pBuild, pValOperand, pTinOperand, pTinOutput);
					pValOp = pBuild->PInstCreateFNot(pValOperandCast, "NCmpEq");
				} break;
			case '-':
				{
					switch (tink)
					{
					case TINK_Float:	pValOp = pBuild->PInstCreateGNeg(pValOperand, "GNeg"); break;
					case TINK_Integer:	pValOp = pBuild->PInstCreateNNeg(pValOperand, "NNeg", fIsSigned); break;
					default: EWC_ASSERT(false, "unexpected type '%s' for negate operator", PChzFromTink(tink));
					}
				} break;
			case JTOK_Dereference:
				{
					if (valgenk != VALGENK_Reference)
					{
						pValOp = pBuild->PInstCreateLoad(pValOperand, "Deref");
					}
					else
					{
						pValOp = pValOperand;
					}
				} break;
			case JTOK_PlusPlus:
			case JTOK_MinusMinus:
				{
					if (!EWC_FVERIFY((pTinOutput == pTinOperand), "increment type mismatch (?)"))
						return nullptr;
			
					CIRInstruction * pInstLoad = pBuild->PInstCreateLoad(pValOperand, "IncLoad");
					CIRInstruction * pInstAdd = nullptr;
					switch (tink)
					{
					case TINK_Float:	
						{
							CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
							pBuild->AddManagedVal(pConst);
							pConst->m_pLval = PLvalConstantFloat(((STypeInfoFloat *)pTinOperand)->m_cBit, 1.0);

							if (pStnod->m_jtok == JTOK_PlusPlus)
								pInstAdd = pBuild->PInstCreateGAdd(pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreateGSub(pInstLoad, pConst, "gDec");

						} break;
					case TINK_Integer:
						{
							auto pTinint = (STypeInfoInteger *)pTinOperand;
							CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
							pBuild->AddManagedVal(pConst);
							pConst->m_pLval = PLvalConstantInt(pTinint->m_cBit, fIsSigned, 1);

							if (pStnod->m_jtok == JTOK_PlusPlus)
								pInstAdd = pBuild->PInstCreateNAdd(pInstLoad, pConst, "gInc", fIsSigned);
							else
								pInstAdd = pBuild->PInstCreateNSub(pInstLoad, pConst, "gDec", fIsSigned);
						} break;
					case TINK_Pointer:
						{
							int nDelta = (pStnod->m_jtok == JTOK_PlusPlus) ? 1 : -1;
							LLVMOpaqueValue * pLvalIndex = PLvalConstantInt(64, fIsSigned, nDelta);

							auto pValLoad = pBuild->PInstCreateLoad(pValOperand, "incLoad");
							pInstAdd = pBuild->PInstCreateGEP(pValLoad, &pLvalIndex, 1, "incGep");
						} break;
					default: EWC_ASSERT(false, "unexpected type '%s' for increment/decrement operator", PChzFromTink(tink));
					}

					auto pInstStore = pBuild->PInstCreateStore(pValOperand, pInstAdd);
					pValOp = (pStnod->m_park == PARK_PostfixUnaryOp) ? pInstLoad : pInstAdd;

					if ( EWC_FVERIFY(pBuild->m_pBlockCur, "missing current block"))
					{
						pBuild->m_pBlockCur->Append(pInstLoad);
						pBuild->m_pBlockCur->Append(pInstAdd);
						pBuild->m_pBlockCur->Append(pInstStore);
					}
				}
			default: break;
			}

			EWC_ASSERT(
				pValOp != nullptr,
				"bad operand '%s' for type '%s'",
				PChzFromJtok(pStnod->m_jtok),
				PChzFromTink(tink));

			return pValOp;
		}
	case PARK_Nop: 
		break;
	default:
		EWC_ASSERT(false, "unhandled PARK (%s) in code generation.", PChzFromPark(pStnod->m_park));
	}
	return nullptr;
}

CIRBasicBlock * CIRBuilder::PBlockCreate(CIRProcedure * pProc, const char * pChzName)
{
	CIRBasicBlock * pBlock = EWC_NEW(m_pAlloc, CIRBasicBlock) CIRBasicBlock(m_pAlloc);

	EWC_ASSERT(pProc->m_pLvalFunction, "expected function value");
	auto pLblock = LLVMAppendBasicBlock(pProc->m_pLvalFunction, pChzName);
	pBlock->m_pLblock = pLblock;

	if (EWC_FVERIFY(pProc, "missing procedure in PBlockCreate"))
	{
		pProc->m_arypBlockManaged.Append(pBlock);
	}
	return pBlock;
}

void CIRBuilder::ActivateProcedure(CIRProcedure * pProc, CIRBasicBlock * pBlock)
{
	m_pProcCur = pProc;
	ActivateBlock(pBlock);
}

void CIRBuilder::ActivateBlock(CIRBasicBlock * pBlock)
{
	if (pBlock == m_pBlockCur)
		return;

	if (pBlock)
	{
		EWC_ASSERT(pBlock->m_pLblock);
		auto pLvalBlockParent = LLVMGetBasicBlockParent(pBlock->m_pLblock);

		EWC_ASSERT(pLvalBlockParent == m_pProcCur->m_pLvalFunction, "block with multiple parents?");
		LLVMPositionBuilder(m_pLbuild, pBlock->m_pLblock, nullptr);
	}

	m_pBlockCur = pBlock;
}

CIRProcedure * PProcCodegenPrototype(CIRBuilder * pBuild, CSTNode * pStnod)
{
	CSTProcedure * pStproc = pStnod->m_pStproc;
	CSTNode * pStnodParamList = nullptr;
	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodName = nullptr;
	if (EWC_FVERIFY(pStproc, "Encountered procedure without CSTProcedure"))
	{
		pStnodParamList = pStnod->PStnodChildSafe(pStproc->m_iStnodParameterList);
		pStnodReturn = pStnod->PStnodChildSafe(pStproc->m_iStnodReturnType);
		pStnodName = pStnod->PStnodChildSafe(pStproc->m_iStnodProcName);
	}

	bool fHasVarArgs = false;
	CDynAry<LLVMTypeRef> arypLtype(pBuild->m_pAlloc);
	if (pStnodParamList && EWC_FVERIFY(pStnodParamList->m_park == PARK_ParameterList, "expected parameter list"))
	{
		int cpStnodParams = pStnodParamList->CStnodChild();
		for (int ipStnod = 0; ipStnod < cpStnodParams; ++ipStnod)
		{
			CSTNode * pStnodDecl = pStnodParamList->PStnodChild(ipStnod);
			if (pStnodDecl->m_park == PARK_VariadicArg)
			{
				fHasVarArgs = true;
				continue;
			}

			if (!EWC_FVERIFY(pStnodDecl->m_park == PARK_Decl, "bad parameter"))
				continue;

			auto pLtype = PLtypeFromPTin(pStnodDecl->m_pTin);
			if (EWC_FVERIFY(pLtype, "Could not compute LLVM type for parameter"))
			{
				arypLtype.Append(pLtype);
			}
		}
	}

	LLVMOpaqueType * pLtypeReturn = nullptr;
	if (pStnodReturn)
	{
		pLtypeReturn = PLtypeFromPTin(pStnodReturn->m_pTin);
		EWC_FVERIFY(pLtypeReturn, "Could not compute LLVM type for return type");
	}
	if (!pLtypeReturn)
	{
		pLtypeReturn = LLVMVoidType();
	}

	const char * pChzName;
	if (pStnodName)
	{
		CString strProcName = StrFromIdentifier(pStnodName);
		pChzName = strProcName.PChz();
	}

	char aCh[128];
	if (!pStnodName)
	{
		(void) pBuild->CChGenerateUniqueName("__AnnonFunc__", aCh, EWC_DIM(aCh));
		pChzName = aCh;
	}

	CAlloc * pAlloc = pBuild->m_pAlloc;
	CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);

	auto pLtypeFunction = LLVMFunctionType(pLtypeReturn, arypLtype.A(), (u32)arypLtype.C(), fHasVarArgs);
	pProc->m_pLvalFunction = LLVMAddFunction(pBuild->m_pLmoduleCur, pChzName, pLtypeFunction);

	if (!pStproc->m_fIsForeign)
	{
		pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, pChzName);
	}

	if (EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check"))
	{
		pStnod->m_pSym->m_pVal = pProc;
	}

	auto pBlockPrev = pBuild->m_pBlockCur;
	auto pProcPrev = pBuild->m_pProcCur;

	pBuild->ActivateProcedure(pProc, (pStproc->m_fIsForeign) ? pBlockPrev : pProc->m_pBlockEntry);
	if (pStnodParamList)
	{
		// set up llvm argument values for our formal parameters
		
		int cpStnodParam = pStnodParamList->CStnodChild();
		int cpLvalParams = LLVMCountParams(pProc->m_pLvalFunction);

		CAlloc * pAlloc = pBuild->m_pAlloc;
		LLVMValueRef * appLvalParams = (LLVMValueRef*)pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, cpLvalParams);
		LLVMGetParams(pProc->m_pLvalFunction, appLvalParams);
		
		LLVMValueRef * ppLvalParam = appLvalParams;
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
		{
			CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
			if (pStnodParam->m_park == PARK_VariadicArg)
				continue;

			EWC_ASSERT((ppLvalParam - appLvalParams) < cpLvalParams, "parameter count mismatch");

			if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
			{
				const char * pChzArgName = pStnodParam->m_pSym->m_strName.PChz();
				LLVMSetValueName(*ppLvalParam, pChzArgName);

				CIRArgument * pArg = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
				pArg->m_pLval = *ppLvalParam;
				pBuild->AddManagedVal(pArg);

				if (!pStproc->m_fIsForeign)
				{
					auto pInstAlloca = pBuild->PInstCreateAlloca(arypLtype[ipStnodParam], 1, pChzArgName);
					pStnodParam->m_pSym->m_pVal = pInstAlloca;

					(void)pBuild->PInstCreateStore(pStnodParam->m_pSym->m_pVal, pArg);
				}
			}
			++ppLvalParam;
		}

		pAlloc->EWC_FREE(appLvalParams);
	}
	pBuild->ActivateProcedure(pProcPrev, pBlockPrev);

	return pProc;
}


void CodeGenEntryPoint(
	CWorkspace * pWork,
	CIRBuilder * pBuild, 
	CSymbolTable * pSymtabTop,
	CAry<CWorkspace::SEntry> * paryEntry,
	CAry<int> * paryiEntryOrder)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	int * piEntryMax = paryiEntryOrder->PMac();
	for (int * piEntry = paryiEntryOrder->A(); piEntry != piEntryMax; ++piEntry)
	{
		CWorkspace::SEntry *pEntry = &(*paryEntry)[*piEntry];
		CSTNode * pStnod = pEntry->m_pStnod;

		EWC_ASSERT(pBuild->m_pProcCur == nullptr, "expected null procedure for entry point.");
		CIRProcedure * pProc = nullptr;
		CSTNode * pStnodBody = nullptr;
		bool fImplicitFunction = pStnod->m_park != PARK_ProcedureDefinition;
		if (fImplicitFunction)
		{
			pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);
			pEntry->m_pProc = pProc;

			char aCh[128];
			(void) pBuild->CChGenerateUniqueName("__AnonFunc__", aCh, EWC_DIM(aCh));

			auto pLtypeFunction = LLVMFunctionType(LLVMVoidType(), nullptr, 0, false);
			pProc->m_pLvalFunction = LLVMAddFunction(pBuild->m_pLmoduleCur, aCh, pLtypeFunction);

			pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, aCh);
			pStnodBody = pStnod;
		}
		else
		{
			// BB - this should move into PValGenerate, under a PARK_ProcedureDefinition case.
			if (!EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check"))
				return;

			if (!pStnod->m_pSym->m_pVal)
			{
				pProc = PProcCodegenPrototype(pBuild, pStnod);
			}
			else
			{
				pProc = (CIRProcedure *)pStnod->m_pSym->m_pVal;
				if (!EWC_FVERIFY(pProc->m_valk == VALK_ProcedureDefinition, "expected IR procedure"))
					return;
			}
			pEntry->m_pProc = pProc;

			CSTProcedure * pStproc = pStnod->m_pStproc;
			if (!pStproc->m_fIsForeign && EWC_FVERIFY(pStproc && pProc->m_pBlockEntry, "Encountered procedure without CSTProcedure"))
			{
				pStnodBody = pStnod->PStnodChildSafe(pStproc->m_iStnodBody);
			}
		}

		if (pProc && pStnodBody)
		{
			pBuild->ActivateProcedure(pProc, pProc->m_pBlockEntry);
			(void) PValGenerate(pWork, pBuild, pStnodBody, VALGENK_Instance);

			if (fImplicitFunction)
			{
				CIRInstruction * pInstRet = pBuild->PInstCreateRet(nullptr);
				if (EWC_FVERIFY(pBuild->m_pBlockCur, "no current block"))
				{
					pBuild->m_pBlockCur->Append(pInstRet);
				}
			}

			LLVMBool fFailed = LLVMVerifyFunction(pProc->m_pLvalFunction, LLVMPrintMessageAction);
			if (fFailed)
			{
				pBuild->PrintDump();
				EmitError(pWork, nullptr, "Code generation for entry point is invalid");
			}
			pBuild->ActivateProcedure(nullptr, nullptr);
		}
	}
}

void TestUniqueNames(CAlloc * pAlloc)
{
	size_t cbFreePrev = pAlloc->CB();
	{
		CIRBuilder build(pAlloc);

		const char * pChzIn;
		char aCh[128];
		size_t cCh;

		pChzIn = "funcName";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame(pChzIn, aCh), "bad unique name");

		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("funcName1", aCh), "bad unique name");

		pChzIn = "funcName20";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("funcName20", aCh), "bad unique name");

		pChzIn = "234";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("234", aCh), "bad unique name");

		pChzIn = "test6000";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("test6000", aCh), "bad unique name");

		pChzIn = "test6000";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("test6001", aCh), "bad unique name");
	}

	size_t cbFreePost = pAlloc->CB();
	EWC_ASSERT(cbFreePrev == cbFreePost, "memory leak testing unique names");
}

size_t CChConstructFilename(const char * pChzFilenameIn, const char * pChzExtension, char * pChzFilenameOut, size_t cChOutMax)
{
	// remove the last extension (if one exists) and replace it with the supplied one.

	char * pChzPeriod = nullptr;
	char * pChzOut = pChzFilenameOut;
	char * pChzOutMax = &pChzFilenameOut[cChOutMax];
	const char * pChIt = pChzFilenameIn;
	for ( ; *pChIt != '\0' && pChzOut != pChzOutMax; ++pChIt)
	{
		if (*pChIt == '.')
		{
			pChzPeriod = pChzOut;
		}

		*pChzOut++ = *pChIt;
	}

	if (pChzPeriod)
		pChzOut = pChzPeriod;

	pChIt = pChzExtension; 
	for ( ; *pChIt != '\0' && pChzOut != pChzOutMax; ++pChIt)
	{
		*pChzOut++ = *pChIt;
	}

	*pChzOut++ = '\0';
	return pChzOut - pChzFilenameOut;
}

void TokenizeTripleString(char * pChzTripleCopy, char ** ppChzArch, char ** ppChzVendor, char ** ppChzOs, char **ppChzEnv)
{
	char * apCh[3];
	for (char ** ppCh = apCh; ppCh != EWC_PMAC(apCh); ++ppCh)
		*ppCh = nullptr;

	int ipCh = 0;
	for (char * pCh = pChzTripleCopy; *pCh != '\0'; ++pCh)
	{
		if (*pCh == '-')
		{
			*pCh++ = '\0';

			if (*pCh != '\0')
			{
				apCh[ipCh++] = pCh;
				if (ipCh >= EWC_DIM(apCh))
					break;
			}
		}
	}

	if (ppChzArch)	*ppChzArch = pChzTripleCopy;
	if (ppChzVendor)*ppChzVendor = apCh[0];
	if (ppChzOs)	*ppChzOs = apCh[1];
	if (ppChzEnv)	*ppChzEnv = apCh[2];
}

void CompileToObjectFile(CWorkspace * pWork, LLVMModuleRef pLmodule, const char * pChzFilenameIn)
{
	LLVMTarget * pLtarget = nullptr;
	//const char * pChzTriple = "x86_64-pc-windows-msvc"; //LLVMGetTarget(pLmodule);
	char * pChzTriple = LLVMGetDefaultTargetTriple();

	bool fUsingWindows;
	{
		size_t cBTriple = CCh(pChzTriple)+1;
		char * pChzTripleCopy = (char*)pWork->m_pAlloc->EWC_ALLOC_TYPE_ARRAY(char, cBTriple);
		CChCopy(pChzTriple, pChzTripleCopy, cBTriple);

		char * pChzOs;
		TokenizeTripleString(pChzTripleCopy, nullptr, nullptr, &pChzOs, nullptr);
		fUsingWindows = FAreSame(pChzOs, "windows");
		
		pWork->m_pAlloc->EWC_DELETE(pChzTripleCopy);
	}

	char * pChzError = nullptr;
	LLVMBool fFailed = LLVMGetTargetFromTriple(pChzTriple, &pLtarget, &pChzError);
	if (fFailed)
	{
		EmitError(pWork, nullptr, "Error generating llvm target. (triple = %s)\n%s", pChzTriple, pChzError);
		LLVMDisposeMessage(pChzError);
		return;
	}

	LLVMCodeGenOptLevel loptlevel = LLVMCodeGenLevelDefault;
	switch (pWork->m_optlevel)
	{
	default:				EWC_ASSERT(false, "unknown optimization level"); // fall through
	case OPTLEVEL_Debug:	loptlevel = LLVMCodeGenLevelNone;			break; // -O0
	case OPTLEVEL_Release:	loptlevel = LLVMCodeGenLevelAggressive;		break; // -O2
	}

	LLVMRelocMode lrelocmode = LLVMRelocDefault;
	LLVMCodeModel lcodemodel = LLVMCodeModelJITDefault;

	// NOTE: llvm-c doesn't expose functions to query these, but it seems to just return the empty string.
	//  This may not work for all target backends.
	const char * pChzCPU = "";
	const char * pChzFeatures = "";

	auto pLtmachine = LLVMCreateTargetMachine(pLtarget, pChzTriple, pChzCPU, pChzFeatures, loptlevel, lrelocmode, lcodemodel);

	const char * pChzExtension;
	if (fUsingWindows)
      pChzExtension = ".obj";
    else
      pChzExtension = ".o";

	char aChFilenameOut[256];
	size_t cCh = CChConstructFilename(pChzFilenameIn, pChzExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));
	pWork->SetObjectFilename(aChFilenameOut, cCh);

	fFailed = LLVMTargetMachineEmitToFile(pLtmachine, pLmodule, aChFilenameOut, LLVMObjectFile, &pChzError);

	LLVMDisposeTargetMachine(pLtmachine);
	if (fFailed)
	{
		EmitError(pWork, nullptr, "Error generating object file\n%s", pChzError);
		LLVMDisposeMessage(pChzError);
	}

	LLVMDisposeMessage(pChzTriple);
	pChzTriple = nullptr;
}

void InitLLVM()
{
	// Initialize targets first, so that --version shows registered targets.
	LLVMInitializeNativeTarget();
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmPrinter();
	LLVMInitializeX86AsmParser();

	// Initialize codegen and IR passes used by llc so that the -print-after,
	// -print-before, and -stop-after options work.

	LLVMPassRegistryRef lpassregistry = LLVMGetGlobalPassRegistry();
	LLVMInitializeCore(lpassregistry);
	//LLVMInitializeCodeGen(*Registry);
	//LLVMInitializeLoopStrengthReducePass(*Registry);
	//LLVMInitializeLowerIntrinsicsPass(*Registry);
	//LLVMInitializeUnreachableBlockElimPass(*Registry);
}

void ShutdownLLVM()
{
	LLVMShutdown();
}

bool FCompileModule(CWorkspace * pWork, GRFCOMPILE grfcompile, const char * pChzFilenameIn)
{
	SJaiLexer jlex;

	pWork->EnsureFile(pChzFilenameIn, CWorkspace::FILEK_Source);

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		CWorkspace::SFile * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != CWorkspace::FILEK_Source)
			continue;

		char aChFilenameOut[256];
		(void)CChConstructFilename(pFile->m_strFilename.PChz(), ".jai", aChFilenameOut, EWC_DIM(aChFilenameOut));

		pFile->m_pChzFile = pWork->PChzLoadFile(aChFilenameOut, pWork->m_pAlloc);
		if (!pFile->m_pChzFile)
			continue;

		BeginParse(pWork, &jlex, pFile->m_pChzFile);
		jlex.m_pChzFilename = pFile->m_strFilename.PChz();

		EWC_ASSERT(pWork->m_pParctx->m_cError == 0, "parse errors detected");
		pWork->m_pParctx->m_cError = 0;

		ParseGlobalScope(pWork, &jlex, true);
		EWC_ASSERT(pWork->m_aryEntry.C() > 0);

		EndParse(pWork, &jlex);
	}

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

	if (pWork->m_pErrman->m_cError == 0)
	{
		CIRBuilder build(pWork->m_pAlloc);
		CodeGenEntryPoint(pWork, &build, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

		CompileToObjectFile(pWork, build.m_pLmoduleCur, pChzFilenameIn);

		if (grfcompile.FIsSet(FCOMPILE_PrintIR))
		{
			build.PrintDump();
		}

	}
	else
	{
		printf("Compilation failed: %d errors\n", pWork->m_pErrman->m_cError);
		return false;
	}

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		CWorkspace::SFile * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != CWorkspace::FILEK_Source)
			continue;

		if (pFile->m_pChzFile)
		{
			pWork->m_pAlloc->EWC_DELETE((void *)pFile->m_pChzFile);
			pFile->m_pChzFile = nullptr;
		}
	}

	return true;
}

void AssertTestCodeGen(
	CWorkspace * pWork,
	const char * pChzIn)
	//const char * pChzOut)
{
	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pChzIn);

	EWC_ASSERT(pWork->m_pParctx->m_cError == 0, "parse errors detected");
	pWork->m_pParctx->m_cError = 0;

	ParseGlobalScope(pWork, &jlex, true);
	EWC_ASSERT(pWork->m_aryEntry.C() > 0);

	EndParse(pWork, &jlex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);
	{
		CIRBuilder build(pWork->m_pAlloc);
		CodeGenEntryPoint(pWork, &build, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);
	}

	EndWorkspace(pWork);
}

void TestCodeGen()
{
	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 200];
	CAlloc alloc(aB, sizeof(aB));

	TestUniqueNames(&alloc);

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);
	const char * pChzIn;

	pChzIn = "SFoo :: struct { m_n : s32; } { foo : SFoo; pFoo := *foo; pFoo.m_n = 2; } ";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "SFoo :: struct { m_n : s32; m_g := 1.2; } foo : SFoo;";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ aN : [4] s32;  pN : * s32; fTest := aN == pN; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : * s32; ++pN; --pN; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : * s32; pN = pN + 2; pN = pN - 2; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ aN : [4] s32; n := aN[2]; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : * s32; n := pN[0]; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ i:=0; while i < 5 { i = i + 1; } }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "printf :: (pCh : * u8, ..) -> s32 #foreign;";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pChz := \"testString\"; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : * int; n := 2;   if (!pN) pN = *n;   if (pN) @pN = 2; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ n : int = 32; pN : * int; pN = *n; n2 := @pN; @pN = 6;}";
	AssertTestCodeGen(&work, pChzIn);
	//pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n) nRet = 4; else nRet =1; return nRet; }";
	//pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n) nRet = 4; return nRet; }";
	pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n == 4) nRet = 4; else if (n == 3) nRet =3; else nRet = 2; return nRet; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ i:=5 + 2 * 3; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"AddNums :: (nA : int, nB : int) -> int { return nA + nB; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"AddLocal :: (nA : int) -> int { nFoo : int; nFoo = 2; return nA + nFoo; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "Foo :: (nA : s8, nB : s8) -> bool { return (nA < nB) == (nB >= nA); }";
	AssertTestCodeGen(&work, pChzIn);

	//pChzIn =	"GetTwo :: ()-> int { return 2; } AddTwo :: (nA : int) -> int { return nA + GetTwo(); }";
	//pChzIn =	"AddTwo :: (nA : int) -> int { return nA + GetTwo(); } GetTwo :: ()-> int { return 2; }";
	pChzIn =	"Foo :: () -> int { return Bah(); } Bah :: ()-> int { return Foo(); }";
	AssertTestCodeGen(&work, pChzIn);

	StaticShutdownStrings(&allocString);
}
