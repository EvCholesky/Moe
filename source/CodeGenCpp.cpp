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

#include "CodeGenCpp.h"
#include "EwcTypes.h"
#include "JaiParse.h"
#include "JaiTypes.h"
#include "Workspace.h"

using namespace EWC;

#pragma warning ( push )
#pragma warning(disable : 4100)
#pragma warning(disable : 4127)
#pragma warning(disable : 4146)
#pragma warning(disable : 4180)
#pragma warning(disable : 4204)
#pragma warning(disable : 4244)
#pragma warning(disable : 4245)
#pragma warning(disable : 4258)
#pragma warning(disable : 4267)
#pragma warning(disable : 4291)
#pragma warning(disable : 4310)
#pragma warning(disable : 4324)
#pragma warning(disable : 4345)
#pragma warning(disable : 4351)
#pragma warning(disable : 4355)
#pragma warning(disable : 4389)
#pragma warning(disable : 4456)
#pragma warning(disable : 4457)
#pragma warning(disable : 4458)
#pragma warning(disable : 4459)
#pragma warning(disable : 4503)
#pragma warning(disable : 4505)
#pragma warning(disable : 4510)
#pragma warning(disable : 4512)
#pragma warning(disable : 4610)
#pragma warning(disable : 4611)
#pragma warning(disable : 4624)
#pragma warning(disable : 4701)
#pragma warning(disable : 4702)
#pragma warning(disable : 4703)
#pragma warning(disable : 4706)
#pragma warning(disable : 4722)
#pragma warning(disable : 4800)
#pragma warning(disable : 4805)
#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_WARNINGS
#define _SCL_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_WARNINGS
#define __STDC_CONSTANT_MACROS
#define __STDC_FORMAT_MACROS
#define __STDC_LIMIT_MACROS

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/ManagedStatic.h"

namespace llvm
{
	class MachineFunctionInitializer;
}

#undef _CRT_SECURE_NO_DEPRECATE
#undef _CRT_SECURE_NO_WARNINGS
#undef _CRT_NONSTDC_NO_DEPRECATE
#undef _CRT_NONSTDC_NO_WARNINGS
#undef _SCL_SECURE_NO_DEPRECATE
#undef _SCL_SECURE_NO_WARNINGS
#undef __STDC_CONSTANT_MACROS
#undef __STDC_FORMAT_MACROS
#undef __STDC_LIMIT_MACROS
#pragma warning ( pop )

CIRProcedure * PProcCodegenPrototype(CIRBuilder * pBuild, CSTNode * pStnod);

// wrapper class to avoid the forward decl template mess.
class LlvmIRBuilder : public llvm::IRBuilder<>
{
public:
					LlvmIRBuilder(llvm::LLVMContext * pLctx)
					:llvm::IRBuilder<>(*pLctx)
						{ ; }
};



static inline size_t CChGetTypeString(llvm::Type * pType, char * pCh, const char * pChEnd)
{
	if (pType->isIntegerTy())
	{
		switch (pType->getIntegerBitWidth())
		{
		case 1: return CChCopy("bool", pCh, pChEnd-pCh);
		case 8: return CChCopy("s8", pCh, pChEnd-pCh);
		case 16: return CChCopy("s16", pCh, pChEnd-pCh);
		case 32: return CChCopy("s32", pCh, pChEnd-pCh);
		case 64: return CChCopy("s64", pCh, pChEnd-pCh);
		}
	}
	else if (pType->isFloatTy())
	{
		return CChCopy("f32", pCh, pChEnd-pCh);
	}
	else if (pType->isDoubleTy())
	{
		return CChCopy("f64", pCh, pChEnd-pCh);
	}
	else if (pType->isVoidTy())
	{
		return CChCopy("void", pCh, pChEnd-pCh);
	}
	else if (pType->isPointerTy())
	{
		size_t cChPrefix = CChCopy("* ", pCh, pChEnd-pCh);
		llvm::Type * pLtypeElement = pType->getPointerElementType();
		return cChPrefix + CChGetTypeString(pLtypeElement, pCh+cChPrefix, pChEnd);
	}

	return CChCopy("unknown", pCh, pChEnd-pCh);
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
	llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
	m_pLbuild = EWC_NEW(m_pAlloc, LlvmIRBuilder) LlvmIRBuilder(pLctx);
	m_pLmoduleCur = EWC_NEW(m_pAlloc, llvm::Module) llvm::Module("Jai Jit", *pLctx);
}
	
CIRBuilder::~CIRBuilder()
{
	if (m_pLbuild)
	{
		m_pAlloc->EWC_DELETE(m_pLbuild);
		m_pLbuild = nullptr;
	}

	if (m_pLmoduleCur)
	{
		m_pAlloc->EWC_DELETE(m_pLmoduleCur);
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

	m_pLmoduleCur->dump();
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
	pInst->m_pLval = m_pLbuild->CreateAdd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFAdd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SSub : IROP_USub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateSub(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GSub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFSub(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SMul : IROP_UMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateMul(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFMul(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNShr(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate(IROP_Shr, pValLhs, pValRhs, pChzName);

	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
	if (fSigned)
	{
		pInst->m_pLval = m_pLbuild->CreateAShr(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	else
	{
		pInst->m_pLval = m_pLbuild->CreateLShr(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNShl(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate(IROP_Shl, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateShl(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNAnd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_And, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateAnd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNOr(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_Or, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateOr(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNNeg(CIRValue * pValOperand, const char * pChzName, bool fIsSigned)
{
	CIRInstruction * pInst = PInstCreate(IROP_NNeg, pValOperand, nullptr, pChzName);
	pInst->m_pLval = m_pLbuild->CreateNeg(pValOperand->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGNeg(CIRValue * pValOperand, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GNeg, pValOperand, nullptr, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFNeg(pValOperand->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateFNot(CIRValue * pValOperand, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_Not, pValOperand, nullptr, pChzName);
	pInst->m_pLval = m_pLbuild->CreateNot(pValOperand->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst;
	if (fSigned)
	{
		pInst = PInstCreate(IROP_UDiv, pValLhs, pValRhs, pChzName);
		pInst->m_pLval = m_pLbuild->CreateSDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	else
	{
		pInst = PInstCreate(IROP_SDiv, pValLhs, pValRhs, pChzName);
		pInst->m_pLval = m_pLbuild->CreateUDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GDiv, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}
					
CIRInstruction * CIRBuilder::PInstCreateCondBranch(
	CIRValue * pValPred,
	CIRBasicBlock * pBlockTrue,
	CIRBasicBlock * pBlockFalse)
{
	CIRInstruction * pInst = PInstCreate(IROP_CondBranch, pValPred, nullptr, "branch");
	auto pLblockFalse = (pBlockFalse == nullptr) ? nullptr : pBlockFalse->m_pLblock;
	pInst->m_pLval = m_pLbuild->CreateCondBr(pValPred->m_pLval, pBlockTrue->m_pLblock, pLblockFalse);
	return pInst;
}

llvm::CmpInst::Predicate LpredicateFromCmppred(CMPPRED cmppred)
{
#define JAI_PRED(X) 
#define LLVM_PRED(X) llvm::CmpInst::##X ,
	static const llvm::CmpInst::Predicate s_mpCmpredLpredicate[] =
	{
		CMPPRED_LIST
	};
#undef JAI_PRED
#undef LLVM_PRED
	EWC_CASSERT(CMPPRED_Max == 16, "bad count");
	EWC_CASSERT(EWC_DIM(s_mpCmpredLpredicate) == CMPPRED_Max, "missing elements in predicate map");

	return s_mpCmpredLpredicate[cmppred];
}

CIRInstruction * CIRBuilder::PInstCreateNCmp(
	CMPPRED cmppred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_NCmp, pValLhs, pValRhs, pChzName);
	auto lPredicate = LpredicateFromCmppred(cmppred);
	pInst->m_pLval = m_pLbuild->CreateICmp(lPredicate, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGCmp(
	CMPPRED cmppred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GCmp, pValLhs, pValRhs, pChzName);
	auto lPredicate = LpredicateFromCmppred(cmppred);
	pInst->m_pLval = m_pLbuild->CreateFCmp(lPredicate, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateBranch(CIRBasicBlock * pBlock)
{
	CIRInstruction * pInst = PInstCreate(IROP_Branch, nullptr, nullptr, "branch");
	pInst->m_pLval = m_pLbuild->CreateBr(pBlock->m_pLblock);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateRet(CIRValue * pValRhs)
{
	CIRInstruction * pInst = PInstCreate(IROP_Ret, pValRhs, nullptr, "RetTmp");
	if (pValRhs)
	{
		pInst->m_pLval = m_pLbuild->CreateRet(pValRhs->m_pLval);
	}
	else
	{
		pInst->m_pLval = m_pLbuild->CreateRetVoid();
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateAlloca(llvm::Type * pLtype, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_Alloca, nullptr, nullptr, pChzName);
	pInst->m_pLval = m_pLbuild->CreateAlloca(pLtype, 0, pChzName);
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
	pInstLoad->m_pLval = m_pLbuild->CreateLoad(pValPT->m_pLval, pChzName);
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

	CIRInstruction * pInstStore = PInstCreate(IROP_Store, pInstPT, pValT, "store");
    pInstStore->m_pLval = m_pLbuild->CreateStore(pValT->m_pLval, pInstPT->m_pLval);
	return pInstStore;
}

static inline llvm::Type * PLtypeFromPTin(STypeInfo * pTin)
{
	if (!pTin)
		return nullptr;

	llvm::LLVMContext & lctx = llvm::getGlobalContext();

	switch (pTin->m_tink)
	{
		case TINK_Void:
		{
			return llvm::Type::getVoidTy(lctx);
		}
		case TINK_Pointer:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer*)pTin;
			auto pLtypePointedTo = PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			return pLtypePointedTo->getPointerTo();
		}
	    case TINK_Integer:	
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			switch (pTinint->m_cBit)
			{
			case 8:		return llvm::Type::getInt8Ty(lctx);
			case 16:	return llvm::Type::getInt16Ty(lctx);
			case 32:	return llvm::Type::getInt32Ty(lctx);
			case 64:	return llvm::Type::getInt64Ty(lctx);
			default:	return nullptr;
			}
		}
		case TINK_Bool:		return llvm::Type::getInt1Ty(lctx);
	    case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTin;
			switch (pTinfloat->m_cBit)
			{
			case 32:	return llvm::Type::getFloatTy(lctx);
			case 64:	return llvm::Type::getDoubleTy(lctx);
			default:	return nullptr;
			}
		}
		case TINK_Literal:
		{
			STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTin;
			switch (pTinlit->m_litty.m_litk)
			{
			case LITK_Integer:	return llvm::Type::getInt64Ty(lctx);
			case LITK_Float:	return llvm::Type::getDoubleTy(lctx);
			case LITK_Bool:		return llvm::Type::getInt1Ty(lctx);
			case LITK_Char:		return nullptr;
			case LITK_String:	return llvm::Type::getInt8Ty(lctx)->getPointerTo();
			case LITK_Null:		return PLtypeFromPTin(pTinlit->m_pTinptrNull);
			default:			return nullptr;
			}
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

llvm::Value * PLvalZeroInType(CIRBuilder * pBuild, STypeInfo * pTin)
{
	LlvmIRBuilder * pLbuild = pBuild->m_pLbuild;
	switch (pTin->m_tink)
	{
	case TINK_Integer:	
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			switch (pTinint->m_cBit)
			{
			case 8: return pLbuild->getInt8(0);
			case 16: return pLbuild->getInt16(0);
			case 32: return pLbuild->getInt32(0);
			case 64: return pLbuild->getInt64(0);
			}
		} break;
	case TINK_Float:	
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTin;
			switch (pTinfloat->m_cBit)
			{
			case 32: return llvm::ConstantFP::get(Type::getFloatTy(llvm::getGlobalContext()), 0.0);
			case 64: return llvm::ConstantFP::get(Type::getDoubleTy(llvm::getGlobalContext()), 0.0);
			}
		} break;
	case TINK_Bool:		return llvm::ConstantInt::getFalse(llvm::getGlobalContext());
	case TINK_Pointer:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTin;
			auto * pLtype = PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			if (pLtype)
				return llvm::Constant::getNullValue(pLtype->getPointerTo());
		} break;
	default: break;
	}

	return nullptr;
}

inline CIRInstruction * PInstCreateCast(CIRBuilder * pBuild, IROP irop, CIRValue * pValSrc, const char * pChz, llvm::Value * pLval)
{
	CIRInstruction * pInst = pBuild->PInstCreate(irop, pValSrc, nullptr, pChz); \
	pInst->m_pLval = pLval;
	return pInst;
}

CIRValue * PValCreateCast(CIRBuilder * pBuild, CIRValue * pValSrc, STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
#define PINST_CREATE_CAST(Irop, CreateFunc, pChz) PInstCreateCast(pBuild, Irop, pValSrc, pChz, pBuild->m_pLbuild->CreateFunc(pValSrc->m_pLval, pLtypeDst, pChz));

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
					if (cBitDst < cBitSrc)				{ return PINST_CREATE_CAST(IROP_NTrunc, CreateTrunc, "NTrunc"); }
					else if (fSignedSrc & fSignedDst)	{ return PINST_CREATE_CAST(IROP_SignExt, CreateSExt, "SignExt"); }
					else								{ return PINST_CREATE_CAST(IROP_ZeroExt, CreateZExt, "ZeroExt"); }
				} break;
			case TINK_Float:
				{
					if (fSignedSrc) { return PINST_CREATE_CAST(IROP_GToS, CreateFPToSI, "GToS"); }
					else			{ return PINST_CREATE_CAST(IROP_GToU, CreateFPToUI, "GToU"); }
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
					if (fSignedSrc) { return PINST_CREATE_CAST(IROP_SToG, CreateSIToFP, "SToG"); }
					else			{ return PINST_CREATE_CAST(IROP_UToG, CreateUIToFP, "UToG"); }
				}
			case TINK_Float:
					if (cBitDst > cBitSrc)	{ return PINST_CREATE_CAST(IROP_GExtend, CreateFPExt, "GExtend"); }
					else					{ return PINST_CREATE_CAST(IROP_GTrunc, CreateFPTrunc, "GTrunc"); }
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
					pInst->m_pLval = pBuild->m_pLbuild->CreateICmpNE(pValSrc->m_pLval, pLvalZero, "NToBool"); 
					return pInst;
				} 
			case TINK_Float:
				{
					auto pLvalZero = PLvalZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreate(IROP_GCmp, pValSrc, nullptr, "GCmp");
					pInst->m_pLval = pBuild->m_pLbuild->CreateFCmpONE(pValSrc->m_pLval, pLvalZero, "GToBool");
					return pInst;
				}
			case TINK_Pointer:
				{
					auto pLvalZero = PLvalZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreate(IROP_GCmp, pValSrc, nullptr, "NCmp");
					pInst->m_pLval = pBuild->m_pLbuild->CreateICmpNE(pValSrc->m_pLval, pLvalZero, "PToBool");
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

static inline CIRValue * PValCreateDefaultInitializer(CIRBuilder * pBuild, STypeInfo * pTin, llvm::Type * pLtype)
{
	llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
	LlvmIRBuilder * pLbuild = pBuild->m_pLbuild;
	llvm::Constant * pLconst = nullptr;

	switch (pTin->m_tink)
	{
	case TINK_Integer:	pLconst = pLbuild->getInt64(0);									break;
	case TINK_Float:	pLconst = llvm::ConstantFP::get(*pLctx, llvm::APFloat(0.0f));	break;
	case TINK_Bool:		pLconst = llvm::ConstantInt::getFalse(*pLctx);					break;
	case TINK_Pointer:	pLconst = Constant::getNullValue(pLtype);						break;
	default:			break;
	}

	if (!EWC_FVERIFY(pLconst, "unexpected type in PValGenerateDefaultInitializer"))
		return nullptr;

	// BB - don't need a unique constant here...
	CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
	pConst->m_pLval = pLconst;
	pBuild->AddManagedVal(pConst);

	return pConst;
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
	
		llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
		llvm::Constant * pLconst = nullptr;
		LlvmIRBuilder * pLbuild = pBuild->m_pLbuild;

		switch (pTinlit->m_litty.m_litk)
		{
		case LITK_Integer:
			{
				EWC_ASSERT(
					(pStval->m_stvalk == STVALK_UnsignedInt) | (pStval->m_stvalk == STVALK_SignedInt),
					"bad literal value");

				switch (pTinlit->m_litty.m_cBit)
				{
				case 8:		pLconst = pLbuild->getInt8(U8Coerce(pStval->m_nUnsigned & 0xFF));			break;
				case 16:	pLconst = pLbuild->getInt16(U16Coerce(pStval->m_nUnsigned & 0xFFFF));		break;
				case 32:	pLconst = pLbuild->getInt32(U32Coerce(pStval->m_nUnsigned & 0xFFFFFFFF));	break;
				case -1: // fall through
				case 64:	pLconst = pLbuild->getInt64(pStval->m_nUnsigned);							break;
				default:	EWC_ASSERT(false, "unhandled integer size");
				}
			}break;
		case LITK_Float:
			{
				if (pTinlit->m_litty.m_cBit == 64)
				{
					pLconst = llvm::ConstantFP::get(Type::getDoubleTy(llvm::getGlobalContext()), pStval->m_g);
				}
				else
				{
					EWC_ASSERT(pTinlit->m_litty.m_cBit == 32, "unhandled float size");
					pLconst = llvm::ConstantFP::get(Type::getFloatTy(llvm::getGlobalContext()), pStval->m_g);
				}
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
				pLconst = (pStval->m_nUnsigned) ? 
							llvm::ConstantInt::getTrue(*pLctx) :
							llvm::ConstantInt::getFalse(*pLctx);
			} break;
		case LITK_Char:		EWC_ASSERT(false, "TBD"); return nullptr;
		case LITK_String:
			{
				if (!EWC_FVERIFY(pStval->m_stvalk == STVALK_String, "bad value in string literal"))
					return nullptr;

				// string literals aren't really constants in the eyes of llvm, but it'll work for now
				CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
				pBuild->AddManagedVal(pConst);
				pConst->m_pLval = pLbuild->CreateGlobalStringPtr(pStval->m_str.PChz());
				return pConst;
			} break;
		case LITK_Null:
			{
				llvm::Type * pLtype = PLtypeFromPTin(pTinlit->m_pTinptrNull);
				if (!EWC_FVERIFY(pLtype, "could not find llvm type for null pointer"))
					return nullptr;

			    pLconst = llvm::ConstantPointerNull::get(llvm::cast<PointerType>(pLtype));
			}
		}

		if (!pLconst)
		{
			EWC_ASSERT(false, "unknown LITK in PValGenerate");
			return nullptr;
		}

		CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
		pBuild->AddManagedVal(pConst);
		pConst->m_pLval = pLconst;
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
				// not adding 
			}

		}break;
	case PARK_Decl:
		{
			CSTDecl * pStdecl = pStnod->m_pStdecl;
			if (!pStdecl || !EWC_FVERIFY(pStnod->m_pSym, "declaration without symbol"))
				return nullptr;

			llvm::Type * pLtype = PLtypeFromPTin(pStnod->m_pTin);
			if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
				return nullptr;

			auto * pInstAlloca = pBuild->PInstCreateAlloca(pLtype, pStnod->m_pSym->m_strName.PChz());
			pStnod->m_pSym->m_pVal = pInstAlloca;
			
			// need to generate code for local var, just running init, leg for now.
			CIRValue * pValInit;
			if (pStdecl->m_iStnodInit >= 0)
			{
				pValInit = PValGenerate(pWork, pBuild, pStnod->PStnodChild(pStdecl->m_iStnodInit), VALGENK_Instance);
			}
			else
			{
				pValInit = PValCreateDefaultInitializer(pBuild, pStnod->m_pTin, pLtype);	
			}
			pBuild->PInstCreateStore(pInstAlloca, pValInit);

		}break;
	case PARK_Literal:
		{
			EWC_ASSERT(false, "encountered literal AST node during codegen, should have encountered literal type first");
		}break;
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

						if (!pBlockPost)
						{
							pBlockPost = pBuild->PBlockCreate(pProc, "postIf");
						}

						CIRBasicBlock *	pBlockTrue = pBuild->PBlockCreate(pProc, "ifThen");
						CIRBasicBlock * pBlockFalse = pBlockPost;
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
				{
					EWC_ASSERT(false, "Unhandled reserved word in code gen");
				}
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

			llvm::Function * pLfunc = pProc->m_pLfunc;
			int cStnodArgs = pStnod->CStnodChild();

			if (!EWC_FVERIFY(pLfunc->arg_size() != cStnodArgs, "unexpected number of arguments"))
				return nullptr;

			std::vector<llvm::Value *> aryPLvalArgs;
			for (int iStnodChild = 1; iStnodChild < cStnodArgs; ++iStnodChild)
			{
				CIRValue * pVal = PValGenerate(pWork, pBuild, pStnod->PStnodChild(iStnodChild), VALGENK_Instance);

				CSTNode * pStnodCall = pStnod->PStnodChild(iStnodChild);
				CIRValue * pValRhsCast = PValCreateCast(pBuild, pVal, pStnodCall->m_pTinOperand, pStnodCall->m_pTin);

				aryPLvalArgs.push_back(pValRhsCast->m_pLval);
				if (!EWC_FVERIFY(aryPLvalArgs.back(), "missing argument value"))
					return 0;
			}

			CIRInstruction * pInst = pBuild->PInstCreate(IROP_Call, nullptr, nullptr, "RetTmp");
			pInst->m_pLval = pBuild->m_pLbuild->CreateCall(pProc->m_pLfunc, aryPLvalArgs);


			return pInst;
		}
	case PARK_Identifier:
		{
			CIRInstruction * pInst = pBuild->PInstFromSymbol(pStnod->m_pSym);
			if (EWC_FVERIFY(pInst, "unknown identifier in codegen") && 
				valgenk != VALGENK_Reference)
			{
				return pBuild->PInstCreateLoad(pInst, pStnod->m_pSym->m_strName.PChz());
			}
			return pInst;
		}
	case PARK_AssignmentOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);
			CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);
			CIRValue * pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);

			if (!EWC_FVERIFY((pStnodLhs != nullptr) & (pValRhs != nullptr), "null operand"))
				return nullptr;

			if (!EWC_FVERIFY(pValLhs->m_pLval != nullptr, "null llvm operand") || 
				!EWC_FVERIFY(pValRhs->m_pLval != nullptr, "null llvm operand"))
				return nullptr;

			STypeInfo * pTinOutput = pStnod->m_pTin;
			STypeInfo * pTinLhs = pStnodLhs->m_pTin;
			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), "bad cast"))
				return nullptr;
	
			CIRValue * pValRhsCast = PValCreateCast(pBuild, pValRhs, pTinRhs, pTinOutput);

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
			CIRValue * pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);
			if (!EWC_FVERIFY((pValLhs != nullptr) & (pValRhs != nullptr), "null operand"))
				return nullptr;

			if (!EWC_FVERIFY((pValLhs->m_pLval != nullptr) & (pValRhs->m_pLval != nullptr), "null llvm operand"))
				return nullptr;

			STypeInfo * pTinOutput = pStnod->m_pTinOperand;
			STypeInfo * pTinLhs = pStnodLhs->m_pTin;

			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), "bad cast"))
				return nullptr;
	
			CIRValue * pValLhsCast = PValCreateCast(pBuild, pValLhs, pTinLhs, pTinOutput);
			CIRValue * pValRhsCast = PValCreateCast(pBuild, pValRhs, pTinRhs, pTinOutput);
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
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
				} break;
			case TINK_Integer:
				switch (pStnod->m_jtok)
				{
					case '+': 				pInstOp = pBuild->PInstCreateNAdd(pValLhs, pValRhs, "nAddTmp", fIsSigned); break;
					case '-': 				pInstOp = pBuild->PInstCreateNSub(pValLhs, pValRhs, "nSubTmp", fIsSigned); break;
					case '*': 				pInstOp = pBuild->PInstCreateNMul(pValLhs, pValRhs, "nMulTmp", fIsSigned); break;
					case '/': 				pInstOp = pBuild->PInstCreateNDiv(pValLhs, pValRhs, "nDivTmp", fIsSigned); break;
					case '&':				pInstOp = pBuild->PInstCreateNAnd(pValLhs, pValRhs, "nAndTmp"); break;
					case '|':				pInstOp = pBuild->PInstCreateNOr(pValLhs, pValRhs, "nOrTmp"); break;
					case JTOK_ShiftRight:	pInstOp = pBuild->PInstCreateNShr(pValLhs, pValRhs, "nShrTmp", fIsSigned); break;
					case JTOK_ShiftLeft:	pInstOp = pBuild->PInstCreateNShl(pValLhs, pValRhs, "nShlTmp", fIsSigned); break;
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
					case JTOK_LessEqual:
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpSLE, pValLhs, pValRhs, "NCmpSLE");
						else			pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpULE, pValLhs, pValRhs, "NCmpULE");
						break;
					case JTOK_GreaterEqual:
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpSGE, pValLhs, pValRhs, "NCmpSGE");
						else			pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpUGE, pValLhs, pValRhs, "NCmpUGE");
						break;
					case '<':
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpSLT, pValLhs, pValRhs, "NCmpSLT");
						else			pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpULT, pValLhs, pValRhs, "NCmpULT");
						break;
					case '>':
						if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpSGT, pValLhs, pValRhs, "NCmpSGT");
						else			pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpUGT, pValLhs, pValRhs, "NCmpUGT");
						break;
				} break;
			case TINK_Float:
				switch (pStnod->m_jtok)
				{
					case '+': 				pInstOp = pBuild->PInstCreateGAdd(pValLhs, pValRhs, "gAddTmp"); break;
					case '-': 				pInstOp = pBuild->PInstCreateGSub(pValLhs, pValRhs, "gSubTmp"); break;
					case '*': 				pInstOp = pBuild->PInstCreateGMul(pValLhs, pValRhs, "gMulTmp"); break;
					case '/': 				pInstOp = pBuild->PInstCreateGDiv(pValLhs, pValRhs, "gDivTmp"); break;
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateGCmp(CMPPRED_GCmpOEQ, pValLhs, pValRhs, "NCmpOEQ"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateGCmp(CMPPRED_GCmpONE, pValLhs, pValRhs, "NCmpONE"); break;
					case JTOK_LessEqual:	pInstOp = pBuild->PInstCreateGCmp(CMPPRED_GCmpOLE, pValLhs, pValRhs, "NCmpOLE"); break;
					case JTOK_GreaterEqual:	pInstOp = pBuild->PInstCreateGCmp(CMPPRED_GCmpOGE, pValLhs, pValRhs, "NCmpOGE"); break;
					case '<': 				pInstOp = pBuild->PInstCreateGCmp(CMPPRED_GCmpOLT, pValLhs, pValRhs, "NCmpOLT"); break;
					case '>': 				pInstOp = pBuild->PInstCreateGCmp(CMPPRED_GCmpOGT, pValLhs, pValRhs, "NCmpOGT"); break;
				} break;
			case TINK_Pointer:
				switch (pStnod->m_jtok)
				{
					case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
					case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(CMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
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

	case PARK_UnaryOp:
		{
			CSTNode * pStnodOperand = pStnod->PStnodChild(0);

			if (pStnod->m_jtok == JTOK_Reference)
			{
				return PValGenerate(pWork, pBuild, pStnodOperand, VALGENK_Reference);
			}

			CIRValue * pValOperand = PValGenerate(pWork, pBuild, pStnodOperand, VALGENK_Instance);
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
			default: break;
			}

			EWC_ASSERT(
				pValOp != nullptr,
				"bad operand '%s' for type '%s'",
				PChzFromJtok(pStnod->m_jtok),
				PChzFromTink(tink));

			return pValOp;
		}
	default:
		EWC_ASSERT(false, "unhandled PARK (%s) in code generation.", PChzFromPark(pStnod->m_park));
	}
	return nullptr;
}

/* deprecated - delete after it's added to source control
CIRInstruction * PInstFindPointer(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod)
{
	if (pStnod->m_park == PARK_Identifier)
	{
		SSymbol * pSym = pStnod->m_pSym;
		if (!EWC_FVERIFY(pSym->m_pVal && pSym->m_pVal->m_valk == VALK_Instruction, "expected alloca value for symbol"))
			return nullptr;

		CIRInstruction * pInstSym = (CIRInstruction *)pSym->m_pVal;
		if (!EWC_FVERIFY(pInstSym->m_irop = IROP_Alloca, "expected alloca for symbol"))
			return nullptr;

		return pInstSym;
	}

	EmitError(pWork, &pStnod->m_lexloc, "Cannot take the address of %s", PChzFromPark(pStnod->m_park));
	return nullptr;
}
*/

CIRBasicBlock * CIRBuilder::PBlockCreate(CIRProcedure * pProc, const char * pChzName)
{
	CIRBasicBlock * pBlock = EWC_NEW(m_pAlloc, CIRBasicBlock) CIRBasicBlock(m_pAlloc);

	// NOTE: we're not passing the lFunc into BasicBlock::Create, you must call ActivateBlock

	llvm::BasicBlock * pLblock = llvm::BasicBlock::Create(llvm::getGlobalContext(), pChzName, nullptr);
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
		if (!pBlock->m_pLblock->getParent())
		{
			pBlock->m_pLblock->insertInto(m_pProcCur->m_pLfunc, nullptr);
		}

		EWC_ASSERT(pBlock->m_pLblock->getParent() == m_pProcCur->m_pLfunc, "block with multiple parents?");
		m_pLbuild->SetInsertPoint(pBlock->m_pLblock);
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
	std::vector<llvm::Type*> aryPLtype;
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

			llvm::Type * pLtype = PLtypeFromPTin(pStnodDecl->m_pTin);
			if (EWC_FVERIFY(pLtype, "Could not compute LLVM type for parameter"))
			{
				aryPLtype.push_back(pLtype);
			}
		}
	}

	llvm::Type * pLtypeReturn = nullptr;
	if (pStnodReturn)
	{
		pLtypeReturn = PLtypeFromPTin(pStnodReturn->m_pTin);
		EWC_FVERIFY(pLtypeReturn, "Could not compute LLVM type for return type");
	}
	if (!pLtypeReturn)
	{
		pLtypeReturn = pBuild->m_pLbuild->getVoidTy();
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

	llvm::FunctionType * pLfunctype = llvm::FunctionType::get(pLtypeReturn, aryPLtype, fHasVarArgs);
	pProc->m_pLfunc = llvm::Function::Create(
										pLfunctype,
										llvm::Function::ExternalLinkage,
										pChzName,
										pBuild->m_pLmoduleCur);

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
		// set up llvm::Argument values for our formal parameters
		
		int cpStnodParam = pStnodParamList->CStnodChild();
		llvm::Function::arg_iterator argIt = pProc->m_pLfunc->arg_begin();
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam, ++argIt)
		{
			CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
			if (pStnodParam->m_park == PARK_VariadicArg)
				continue;

			if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
			{
				const char * pChzArgName = pStnodParam->m_pSym->m_strName.PChz();
				argIt->setName(pChzArgName);

				CIRArgument * pArg = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
				pArg->m_pLval = argIt;
				pBuild->AddManagedVal(pArg);

				if (!pStproc->m_fIsForeign)
				{
					auto pInstAlloca = pBuild->PInstCreateAlloca(aryPLtype[ipStnodParam], pChzArgName);
					pStnodParam->m_pSym->m_pVal = pInstAlloca;

					(void)pBuild->PInstCreateStore(pStnodParam->m_pSym->m_pVal, pArg);
				}
			}
		}
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

			std::vector<llvm::Type *> argsEmpty;
			llvm::FunctionType * pLfunctype = llvm::FunctionType::get(pBuild->m_pLbuild->getVoidTy(), argsEmpty, false);

			pProc->m_pLfunc = llvm::Function::Create(pLfunctype, llvm::Function::ExternalLinkage, aCh, pBuild->m_pLmoduleCur);
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

			bool fFailed = llvm::verifyFunction(*pProc->m_pLfunc);
			EWC_ASSERT(!fFailed);

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

static std::unique_ptr<tool_output_file> PLoutfileOpen(CWorkspace * pWork, const char * pChzFilenameOut)
{
	// Open the file.
	std::error_code errcode;
	llvm::sys::fs::OpenFlags OpenFlags = llvm::sys::fs::F_None; // fBinary file
	auto pLoutfile = llvm::make_unique<tool_output_file>(pChzFilenameOut, errcode, OpenFlags);
	if (errcode) 
	{
		EmitError(pWork, nullptr, "error making output file");
		errs() << errcode.message() << '\n';
		return nullptr;
	}

	return pLoutfile;
}


void CompileToObjectFile(CWorkspace * pWork, llvm::Module * pLmodule, const char * pChzFilenameIn)
{
	llvm::Triple ltriple;

	if (ltriple.getTriple().empty())
		ltriple.setTriple(llvm::sys::getDefaultTargetTriple());

	std::string strMarch; // lookup valid option for -march
	std::string strError;
	const llvm::Target * pLtarget = llvm::TargetRegistry::lookupTarget(strMarch, ltriple, strError);
	if (!EWC_FVERIFY(pLtarget, "Compiler Error: %s", strError.c_str()))
		return;

	llvm::CodeGenOpt::Level loptlevel = llvm::CodeGenOpt::Default;
	switch (pWork->m_optlevel)
	{
	default:				EWC_ASSERT(false, "uknown optimization level"); // fall through
	case OPTLEVEL_Debug:	loptlevel = llvm::CodeGenOpt::None;				break; // -O0
	case OPTLEVEL_Release:	loptlevel = llvm::CodeGenOpt::Aggressive;		break; // -O2
	}

	llvm::TargetOptions loptions;

	llvm::Reloc::Model lrelocmodel = llvm::Reloc::Default;
	llvm::CodeModel::Model lcodemodel = llvm::CodeModel::JITDefault;

	std::string strCPU = getCPUStr();
	std::string strFeatures = getFeaturesStr();
	std::unique_ptr<llvm::TargetMachine> pLtmachine( pLtarget->createTargetMachine(
																ltriple.getTriple(),
																strCPU,
																strFeatures,
																loptions,
																lrelocmodel,
																lcodemodel,
																loptlevel));

	if (!EWC_FVERIFY(pLtmachine, "Could not allocate target machine!"))
		return;

	const char * pChzExtension;
    if (ltriple.getOS() == Triple::Win32)
      pChzExtension = ".obj";
    else
      pChzExtension = ".o";

	char aChFilenameOut[256];
	size_t cCh = CChConstructFilename(pChzFilenameIn, pChzExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));
	pWork->SetObjectFilename(aChFilenameOut, cCh);

	// Figure out where we are going to send the output.
	std::unique_ptr<llvm::tool_output_file> pLoutfile = PLoutfileOpen(pWork, aChFilenameOut);
	if (!pLoutfile) 
		return;

	// Build up all of the passes that we want to do to the module.
	llvm::legacy::PassManager lpassman;

	// Add an appropriate TargetLibraryInfo pass for the module's triple.
	llvm::TargetLibraryInfoImpl TLII(Triple(pLmodule->getTargetTriple()));

	lpassman.add(new llvm::TargetLibraryInfoWrapperPass(TLII));

	// Add the target data from the target machine, if it exists, or the module.
	if (const llvm::DataLayout * pLdatalay = pLtmachine->getDataLayout())
		pLmodule->setDataLayout(*pLdatalay);

    llvm::raw_pwrite_stream * pLos = &pLoutfile->os();
    std::unique_ptr<buffer_ostream> lbos;
	llvm::TargetMachine::CodeGenFileType lfiletype = llvm::TargetMachine::CGFT_ObjectFile;
	if (lfiletype != TargetMachine::CGFT_AssemblyFile && !pLoutfile->os().supportsSeeking())
	{
		lbos = llvm::make_unique<llvm::buffer_ostream>(*pLos);
		pLos = lbos.get();
	}

    llvm::AnalysisID pVStartBeforeID = nullptr;
    llvm::AnalysisID pVStartAfterID = nullptr;
    llvm::AnalysisID pVStopAfterID = nullptr;
    const llvm::PassRegistry * pLpassreg = llvm::PassRegistry::getPassRegistry();

    // Ask the target to add backend passes as necessary.
	bool fDisableVerify = false;
	llvm::MachineFunctionInitializer * pLmfi = nullptr; // used when reading .mir files?
    if (pLtmachine->addPassesToEmitFile(
					lpassman,
					*pLos,
					lfiletype,
					fDisableVerify,
					pVStartBeforeID,
			        pVStartAfterID,
					pVStopAfterID,
					pLmfi))
	{
		EmitError(pWork, nullptr, "Error generating object file");
		return;
    }

	// Before executing passes, print the final values of the LLVM options.
	cl::PrintOptionValues();

	lpassman.run(*pLmodule);

	// Declare success.
	pLoutfile->keep();
}

void InitLLVM()
{
	(void)getGlobalContext();

	// Initialize targets first, so that --version shows registered targets.
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmPrinter();
	LLVMInitializeX86AsmParser();

	// Initialize codegen and IR passes used by llc so that the -print-after,
	// -print-before, and -stop-after options work.
	PassRegistry *Registry = PassRegistry::getPassRegistry();
	initializeCore(*Registry);
	initializeCodeGen(*Registry);
	initializeLoopStrengthReducePass(*Registry);
	initializeLowerIntrinsicsPass(*Registry);
	initializeUnreachableBlockElimPass(*Registry);
}

void ShutdownLLVM()
{
	llvm_shutdown();
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

	/*
	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type);

	EWC_ASSERT(EWC::FAreSame(aCh, pChzOut), "type check debug string doesn't match expected value");
	*/

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

	pChzIn = "printf :: (pCh : * u8, ..) -> s32 #foreign;";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : * int; n := 2;   if (!pN) pN = *n;   if (pN) @pN = 2; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pChz := \"testString\"; }";
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