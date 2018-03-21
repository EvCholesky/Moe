/* Copyright (C) 2018 Evan Christensen
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

#include "ByteCode.h"
#include "CodeGen.h"
#include "parser.h"
#include "stdio.h"
#include "Util.h"
#include "workspace.h"


// Stack layout (grows down)
//	4 |___null____________________| +
//	3 |___childRet________________| | 
//	2 |___working_________________| |
//	1 |___Local var_______________| | 
//	0 |___Local var_______________|_v_	iBStack(0) for main                                    
// -1 |___pInst return____________|		address of call instruction from main              |___
//	  |.................. pad arg.|						                                     ^
// a2 |___return__________________| |	iBStack for return storage in main stack frame       |   pProc->m_cBArg
// a1 |___arg 1___________________| |                                                        |
// a0 |___arg 0___________________|_v_	iBStack(0) arg frame for childProc                  _v_
//	  |.................. pad ....|                                                          ^
//	3 |___________________________| +                                                        |   pProc->m_cBStack
//	2 |___________________________| |                                                        |
//	1 |___working_________________| |  childProc (a,b) -> childRet                           |
//	0 |___local var_______________|_v_														_v_

using namespace EWC;

void CalculateByteSizeAndAlign(SDataLayout * pDlay, STypeInfo * pTin, u64 * pcB, u64 * pcBAlign)
{
	// return the embeded size of a type (ie. how many bytes would be needed to include it in a struct)

	s64 cB = 1;
	switch (pTin->m_tink)
	{
	case TINK_Integer:	cB = ((STypeInfoInteger*)pTin)->m_cBit >> 0x3;		break;
	case TINK_Float:	cB = ((STypeInfoFloat*)pTin)->m_cBit >> 0x3;		break;
	case TINK_Bool:		cB = pDlay->m_cBBool;								break;
    case TINK_Enum:		CalculateByteSizeAndAlign(pDlay, ((STypeInfoEnum *)pTin)->m_pTinLoose, pcB, pcBAlign);	return;
	case TINK_Qualifier: CalculateByteSizeAndAlign(pDlay, ((STypeInfoQualifier *)pTin)->m_pTin, pcB, pcBAlign);	return;

    case TINK_Null:		cB = pDlay->m_cBPointer;	break;
	case TINK_Pointer:	cB = pDlay->m_cBPointer;	break;
	case TINK_Procedure:cB = pDlay->m_cBPointer;	break;

    case TINK_Struct: 
	{
		auto pTinstruct = (STypeInfoStruct *)pTin;
		if (EWC_FVERIFY(pTinstruct->m_cB > 0, "checking size of struct before it is calculated"))
		{
			*pcB = pTinstruct->m_cB;
			*pcBAlign = pTinstruct->m_cBAlign;
			return;	
		}
	}

    case TINK_Array:
	{
		auto pTinary = (STypeInfoArray *)pTin;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed: 
		{
			CalculateByteSizeAndAlign(pDlay, pTinary->m_pTin, pcB, pcBAlign);
			*pcB = *pcB * pTinary->m_c;
			return;
		}
		case ARYK_Reference:
		{
			*pcB = 2 * pDlay->m_cBPointer;	//(count, pointer) 
			*pcBAlign = pDlay->m_cBPointer;
			return;
		}
		case ARYK_Dynamic:
			EWC_ASSERT(false, "dyn array is TBD");
		} break;
	}

	case TINK_Literal:
	{
		auto pTinlit = (STypeInfoLiteral *)pTin;
		EWC_ASSERT(pTinlit->m_fIsFinalized, "cannot calculate size of unfinalized literal");

		if (pTinlit->m_litty.m_litk == LITK_Array)
		{
			CalculateByteSizeAndAlign(pDlay, pTinlit->m_pTinSource, pcB, pcBAlign);
			*pcB = pTinlit->m_c * *pcB;
			return;
		}

		cB = pTinlit->m_litty.m_cBit >> 0x3;
	} break;

	case TINK_Generic:
		EWC_ASSERT(false, "generic types should be resolved prior to codegen");
		break;

	case TINK_ForwardDecl:
    case TINK_Any:
    case TINK_Void:
	default:
		EWC_ASSERT(false, "unhandled type kind in CBFromTin");
		break;
	}

	*pcB = cB;
	*pcBAlign = cB;
}

namespace BCode 
{

template <typename T>
T * PValRtiCast(SValue * pVal)
{
	if (pVal && pVal->m_valk == EWC::SStripPointer<T>::Type::s_valk)
		return (T)pVal;
	return nullptr;
}

template <typename T>
T PValDerivedCast(SValue * pVal)
{
	EWC_ASSERT(pVal && pVal->m_valk == EWC::SStripPointer<T>::Type::s_valk, "illegal derived cast");
	return (T)pVal;
}


SProcedure::SProcedure(EWC::CAlloc * pAlloc, STypeInfoProcedure * pTinproc)
:SValue(VALK_Procedure)
,m_pTinproc(pTinproc)
,m_cBStack(0)
,m_cBArg(0)
,m_pBlockLocals(nullptr)
,m_pBlockFirst(nullptr)
,m_arypBlock(pAlloc, BK_ByteCodeCreator, 16)
,m_aryInst(pAlloc, BK_ByteCode, 0)
,m_aParamArg(nullptr)
,m_aParamRet(nullptr)
{
}



CBuilder::CBuilder(CWorkspace * pWork, SDataLayout * pDlay)
:CBuilderBase(pWork)
,m_pAlloc(pWork->m_pAlloc)
,m_pBerrctx(nullptr)
,m_pDlay(pDlay)
,m_hashHvMangledPProc(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_arypBlockManaged(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_aryJumptStack(pWork->m_pAlloc, EWC::BK_ByteCodeCreator)
,m_blistGep(pWork->m_pAlloc, BK_ByteCodeCreator)
,m_arypValManaged(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_blistConst(pWork->m_pAlloc, BK_ByteCodeCreator)
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
{
}

CBuilder::~CBuilder()
{
	Clear();
}

void CBuilder::Clear()
{
	auto ppValMac = m_arypValManaged.PMac();
	for (auto ppVal = m_arypValManaged.A(); ppVal != ppValMac; ++ppVal)
	{
		m_pAlloc->EWC_DELETE(*ppVal);
	}
	m_arypValManaged.Clear();
}

static inline s64 IBArgAlloc(s64 * pcBArg, s64 cB, s64 cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	s64 cBStack = U32Coerce((*pcBArg + cBMasked) & ~cBMasked);
	*pcBArg = cBStack + cB;
	return cBStack;
}

SProcedure * CBuilder::PProcCreate(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod)
{
	size_t cArg = pTinproc->m_arypTinParams.C();
	size_t cRet = pTinproc->m_arypTinReturns.C();
	size_t cBAlloc = sizeof(SProcedure) + sizeof(SParameter) * (cArg + cRet);
	cBAlloc = EWC::CBAlign(cBAlloc, EWC_ALIGN_OF(SParameter));

	u8 * pBAlloc = (u8 *)m_pAlloc->EWC_ALLOC(cBAlloc, EWC_ALIGN_OF(SProcedure));

	auto pProc = new(pBAlloc) SProcedure(m_pAlloc, pTinproc);
	auto fins = m_hashHvMangledPProc.FinsEnsureKeyAndValue(pTinproc->m_strMangled.Hv(), pProc);
	EWC_ASSERT(fins == FINS_Inserted, "adding procedure that already exists");

	const char * pCozName = pTinproc->m_strName.PCoz();
	pProc->m_pBlockLocals = PBlockCreate(pProc, pCozName);
	pProc->m_pBlockFirst = PBlockCreate(pProc, pCozName);
	
	SParameter * aParamArg = (SParameter *)PVAlign(pBAlloc + sizeof(SProcedure), EWC_ALIGN_OF(SParameter));
	if (cArg)
	{
		pProc->m_aParamArg = aParamArg;
	}

	if (cRet)
	{
		pProc->m_aParamRet = aParamArg + cArg;
	}

	u64 cB; 
	u64 cBAlign;
	for (size_t iArg = 0; iArg < cArg; ++iArg)
	{
		auto pTinParam = pTinproc->m_arypTinParams[iArg];
		CalculateByteSizeAndAlign(m_pDlay, pTinParam, &cB, &cBAlign);

		auto pParam = &pProc->m_aParamArg[iArg];
		pParam->m_cB = S32Coerce(cB);
		pParam->m_iBStack = S32Coerce(IBArgAlloc(&pProc->m_cBArg, cB, cBAlign));
	}

	for (size_t iRet = 0; iRet < cRet; ++iRet)
	{
		auto pTinParam = pTinproc->m_arypTinReturns[iRet];
		CalculateByteSizeAndAlign(m_pDlay, pTinParam, &cB, &cBAlign);

		auto pParam = &pProc->m_aParamRet[iRet];
		pParam->m_cB = S32Coerce(cB);
		pParam->m_iBStack = S32Coerce(IBArgAlloc(&pProc->m_cBArg, cB, cBAlign));
	}

	pProc->m_cBArg += sizeof(SInstruction *);
	pProc->m_cBArg = CBAlign(pProc->m_cBArg, m_pDlay->m_cBStackAlign);

	return pProc;
}

SProcedure * CBuilder::PProcCreate(
	CWorkspace * pWork,
	STypeInfoProcedure * pTinproc,
	const char * pChzMangled,
	CSTNode * pStnod,
	CSTNode * pStnodBody,
	EWC::CDynAry<LType *> * parypLtype,
	LType * pLtypeReturn)
{
	EWC_ASSERT(false, "bytecode tbd");
	return nullptr;
}

void CBuilder::SetupParamBlock(
	CWorkspace * pWork,
	SProcedure * pProc,
	CSTNode * pStnod,
	CSTNode * pStnodParamList, 
	EWC::CDynAry<LType *> * parypLtype)
{
	// BB - Could we merge this with the CodeGen version?
	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
	int cpStnodParam = pStnodParamList->CStnodChild();

	auto pTinproc = pProc->m_pTinproc;
	int iParam = 0;
	for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
	{
		CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
		if (pStnodParam->m_park == PARK_VariadicArg)
			continue;

		CSTDecl * pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
		if (pStdecl && pStdecl->m_fIsBakedConstant)
			continue;

		EWC_ASSERT(iParam < pTinproc->m_arypTinParams.C(), "parameter count mismatch");
		auto strArgName = StrPunyEncode(pStnodParam->m_pSym->m_strName.PCoz());

		if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
		{
			if (!pStproc->m_fIsForeign)
			{
				auto pInstAlloca = PValCreateAlloca(pTinproc->m_arypTinParams[iParam], 1, strArgName.PCoz());
				pStnodParam->m_pSym->m_pVValue = pInstAlloca;

				(void) PInstCreateStore(pInstAlloca, PRegArg(pProc->m_aParamArg[iParam].m_iBStack));

			}
		}
		++iParam;
	}
}

void CBuilder::ActivateProc(SProcedure * pProc, SBlock * pBlock)
{
	if (m_pBlockCur)
	{
		EWC_ASSERT(!pBlock || m_pBlockCur == pBlock, "mismatch ending block");
		m_pBlockCur = nullptr;
	}

	if (m_pProcCur)
	{
		EWC_ASSERT(!pProc || m_pProcCur == pProc, "mismatch ending procedure");
		m_pProcCur = nullptr;
	}

	m_pProcCur = pProc;
	m_pBlockCur = pBlock;
}

void CBuilder::FinalizeProc(SProcedure * pProc)
{
	s32 cInst = 0;
	for (auto ppBlock = pProc->m_arypBlock.A(); ppBlock != pProc->m_arypBlock.PMac(); ++ppBlock)
	{
		auto pBlock = *ppBlock;
		pBlock->m_iInstFinal = cInst;
		cInst += (s32)pBlock->m_aryInst.C();
	}

	pProc->m_cBStack = CBAlign(pProc->m_cBStack, m_pDlay->m_cBStackAlign);

	auto iBArgFFrame = (s32)pProc->m_cBStack;
	pProc->m_aryInst.EnsureSize(cInst);
	for (auto ppBlock = pProc->m_arypBlock.A(); ppBlock != pProc->m_arypBlock.PMac(); ++ppBlock)
	{
		auto pBlock = *ppBlock;

		// change all branch instructions from pBlock to proc relative iInst
		for (auto pBranch = pBlock->m_aryBranch.A(); pBranch != pBlock->m_aryBranch.PMac(); ++pBranch)
		{
			auto iInstFinal = pBranch->m_pBlockDest->m_iInstFinal;
			EWC_ASSERT(iInstFinal >= 0, "block was not finalized");
			*pBranch->m_pIInstDst = iInstFinal;
		}

		auto pInstMac = pBlock->m_aryInst.PMac(); 
		for (auto pInst = pBlock->m_aryInst.A(); pInst != pInstMac; ++pInst)
		{
			pProc->m_aryInst.Append(*pInst);
			auto pInstNew = pProc->m_aryInst.PLast();
			
			if ((pInstNew->m_opkLhs & FOPK_Arg) == FOPK_Arg)
			{
				pInstNew->m_wordLhs.m_s32 += iBArgFFrame;
			}

			if ((pInstNew->m_opkRhs & FOPK_Arg) == FOPK_Arg)
			{
				pInstNew->m_wordRhs.m_s32 += iBArgFFrame;
			}
		}
	}
}

void CBuilder::PrintDump()
{
}

void CBuilder::FinalizeBuild(CWorkspace * pWork)
{
}

CBuilder::LType * CBuilder::PLtypeVoid()
{
	// BB - this should be relocated... somewhere...
	static STypeInfo s_tinVoid("void", "void", TINK_Void);
	return &s_tinVoid;
}

SBlock * CBuilder::PBlockCreate(SProcedure * pProc, const char * pChzName)
{
	auto pBlock = EWC_NEW(m_pAlloc, SBlock) SBlock();
	pBlock->m_aryInst.SetAlloc(m_pAlloc, BK_ByteCode, 128);
	pBlock->m_aryBranch.SetAlloc(m_pAlloc, BK_ByteCode, 4);
	pBlock->m_pProc = pProc;
	pProc->m_arypBlock.Append(pBlock);
	m_arypBlockManaged.Append(pBlock);

	return pBlock;
}

void CBuilder::ActivateBlock(SBlock * pBlock)
{
	if (m_pBlockCur)
	{
		m_pBlockCur = nullptr;
	}

	EWC_ASSERT(m_pProcCur, "cannot begin basic block without active procedure");
	EWC_ASSERT(m_pProcCur == pBlock->m_pProc, "block activated inside wrong procedure");
	EWC_ASSERT(!m_pBlockCur, "cannot begin basic block; previous block was not ended");
	m_pBlockCur = pBlock;

	if (!m_pProcCur->m_pBlockFirst)
	{
		m_pProcCur->m_pBlockFirst = pBlock;
	}
}

CBuilder::Instruction * CBuilder::PInstCreateNCmp(NPRED npred, SValue * pValLhs, SValue * pValRhs, const char * pChz)
{
	auto pInst = PInstCreateRaw(IROP_NCmp, pValLhs, pValRhs);
	pInst->m_pred = (u8)npred;
	return pInst;
}

CBuilder::Instruction * CBuilder::PInstCreateGCmp(GPRED gpred, SValue * pValLhs, SValue * pValRhs, const char * pChz)
{
	auto pInst = PInstCreateRaw(IROP_GCmp, pValLhs, pValRhs);
	pInst->m_pred = (u8)gpred;
	return pInst;
}

CBuilder::Instruction * CBuilder::PInstCreateGEP(SValue * pValLhs, GepIndex ** apLvalIndices, u32 cpIndices, const char * pChzName)
{
	EWC_ASSERT(false, "bytecode tbd");
	return nullptr;
}

SValue * CBuilder::PValCreateAlloca(STypeInfo * pTin, u64 cElement, const char * pChzName)
{
	// Alloca returns a pointer  to stack mem (but the actual addres isn't known until runtime)
	//  allocate both the requested space and room for the pointer

	EWC_ASSERT(m_pProcCur, "no active procedure");

	u64 cB; 
	u64 cBAlign;
	CalculateByteSizeAndAlign(m_pDlay, pTin, &cB, &cBAlign);

	auto pInst = PInstCreateRaw(IROP_Alloca, cB, nullptr, nullptr, pChzName);
	if (pInst->m_irop == IROP_Error)
		return pInst;

	if (cElement > 1)
	{
		EWC_ASSERT(false, "bytecode tbd");
		return nullptr;
	}
	else
	{
		pInst->m_wordLhs.m_s32 = S32Coerce(IBStackAlloc(cB, cBAlign));

		pInst->m_cBOperand = cB;

		s32 iBStackPointer = S32Coerce(IBStackAlloc(sizeof(u8*), EWC_ALIGN_OF(u8*)));
		pInst->m_iBStackOut = iBStackPointer;
	}

	return pInst;
}

CBuilder::Instruction * CBuilder::PInstCreateMemset(CWorkspace * pWork, SValue * pValLhs, s64 cBSize, s32 cBAlign, u8 bFill)
{
	EWC_ASSERT(false, "bytecode tbd");
	return nullptr;
}

CBuilder::Instruction * CBuilder::PInstCreateMemcpy(CWorkspace * pWork, STypeInfo * pTin, SValue * pValLhs, SValue * pValRhsRef)
{
	EWC_ASSERT(false, "bytecode tbd");
	return nullptr;
}

CBuilder::Instruction * CBuilder::PInstCreateLoopingInit(CWorkspace * pWork, STypeInfo * pTin, SValue * pValLhs, CSTNode * pStnodInit)
{
	EWC_ASSERT(false, "bytecode tbd");
	return nullptr;
}

CBuilder::GepIndex * CBuilder::PGepIndex(u64 idx)
{
	GepIndex * pGep = m_blistGep.AppendNew();
	*pGep = (GepIndex)idx;
	return pGep;
}

CBuilder::GepIndex * CBuilder::PGepIndexFromValue(SValue * pVal)
{
	EWC_ASSERT(false, "GEP index should be SValue, not raw int");
	return nullptr;
}

CBuilder::Instruction * CBuilder::PInstCreatePhi(LType * pLtype, const char * pChzName)
{
	EWC_ASSERT(false, "GEP index should be SValue, not raw int");
	return nullptr;
}

void CBuilder::AddPhiIncoming(SValue * pInstPhi, SValue * pVal, SBlock * pBlock)
{
	EWC_ASSERT(false, "GEP index should be SValue, not raw int");
}

CBuilder::Global * CBuilder::PGlobCreate(STypeInfo * pTin, const char * pChzName)
{
	EWC_ASSERT(false,"bytecode tbd");
	return nullptr;
}

SValue * CBuilder::PValGenerateCall(
	CWorkspace * pWork,
	CSTNode * pStnod,
	EWC::CDynAry<ProcArg *> * parypArgs,
	bool fIsDirectCall,
	STypeInfoProcedure * pTinproc,
	VALGENK valgenk)
{
	EWC_ASSERT(false, "bytecode TBD");
	return false;
}

CBuilder::ProcArg *	CBuilder::PProcArg(SValue * pVal)
{
	return pVal;
}

SInstruction * CBuilder::PInstCreateCall(SValue * pValProc, ProcArg ** apLvalArgs, int cpLvalArg)
{
	if (!EWC_FVERIFY(m_pProcCur, "Cannot add a procedure call outside of a procedure"))
		return PInstCreateError();

	auto pProc = PValDerivedCast<SProcedure *>(pValProc); 
	if (!EWC_FVERIFY(pProc->m_pTinproc->m_arypTinParams.C() == cpLvalArg, "variadic args not yet supported"))
		return PInstCreateError();

	s32 iBStackReturnStore = 0;
	u64 cB;
	u64 cBAlign;
	auto pTinproc = pProc->m_pTinproc;
	auto cBArg = pProc->m_cBArg;
	int cpTinReturn = (int)pTinproc->m_arypTinReturns.C();
	for (int ipTinReturn = 0; ipTinReturn < cpTinReturn; ++ipTinReturn)
	{
		auto pTinRet = pTinproc->m_arypTinReturns[ipTinReturn];

		CalculateByteSizeAndAlign(m_pDlay, pTinRet, &cB, &cBAlign);
		iBStackReturnStore = IBStackAlloc(cB, cBAlign);

		auto pParam = &pProc->m_aParamRet[ipTinReturn];

		// subtract cBArg here because we're still in the caller's stack frame
		(void) PInstCreate(IROP_StoreToReg, PReg(pParam->m_iBStack - cBArg), PConstInt(iBStackReturnStore, 32));
	}

	for (int iRec = 0; iRec < cpLvalArg; ++iRec)
	{
		// NOTE: Lhs is relative to called function stack frame, Rhs is relative to calling stack frame

		auto pParam = &pProc->m_aParamArg[iRec];
		(void) PInstCreate(IROP_StoreToReg, PReg(pParam->m_iBStack - cBArg), apLvalArgs[iRec]);
	}

	u8 cBReturnOp = 0;
	if (cpTinReturn)
	{
		cBReturnOp = pProc->m_aParamRet[0].m_cB;
	}

	auto pInstCall = PInstCreateRaw(IROP_Call, cBReturnOp, PConstPointer(pProc), PConstPointer(m_pProcCur));
	pInstCall->m_iBStackOut = iBStackReturnStore;


	return pInstCall;
}

void CBuilder::CreateReturn(SValue ** apVal, int cpVal)
{
	if (!EWC_FVERIFY(m_pProcCur, "Cannot add a return opcode outside of a procedure"))
		return;

	for (int iReturn = 0; iReturn < cpVal; ++iReturn)
	{
		// add the returnIdx(callee relative) stored as an argument to (cBArg + cBStack)

		// add the calling stack relative return index to (cBArg + cBStack)
		auto pInstOffset = PInstCreateRaw(IROP_NAdd, PRegArg(m_pProcCur->m_aParamRet[0].m_iBStack, 32), PConstArg(m_pProcCur->m_cBArg, 32));

		(void) PInstCreate(IROP_StoreToIdx, pInstOffset, apVal[iReturn]);
	}

	auto pTinproc = m_pProcCur->m_pTinproc;
	s32 cBReturnOp = 0;
	if (!pTinproc->m_arypTinReturns.FIsEmpty())
	{
		cBReturnOp = m_pProcCur->m_aParamRet[0].m_cB;
	}

	PInstCreateRaw(IROP_Ret, cBReturnOp, PConstArg(m_pProcCur->m_cBArg, 32), nullptr);
}

SInstruction * CBuilder::PInstCreateTraceStore(SValue * pVal, STypeInfo * pTin)
{
	return PInstCreateRaw(IROP_TraceStore, pVal, PConstPointer(pTin));
}

/*
void CBuilder::AddCondBranch(SRecord & recPred, SBlock * pBlockTrue, SBlock * pBlockFalse)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, IROP_CondBranch, 1, recPred, RecUnsigned(0));

	EWC_ASSERT(m_pBlockCur && !m_pBlockCur->FIsFinalized(), "cannot allocate instructions without a unfinalized basic block");

	s32 * pIInstDst = (s32*)&pInst->m_wordRhs;
	auto pBranchTrue = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchTrue->m_pBlockDest = pBlockTrue;
	pBranchTrue->m_pIInstDst = &pIInstDst[true];

	auto pBranchFalse = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchFalse->m_pBlockDest = pBlockFalse;
	pBranchFalse->m_pIInstDst = &pIInstDst[false];

	pInst->m_iBStackOut = 0;
}
*/
void CBuilder::CreateBranch(SBlock * pBlock)
{
	EWC_ASSERT(m_pBlockCur && !m_pBlockCur->FIsFinalized(), "cannot allocate instructions without a unfinalized basic block");

	auto pInst = PInstCreateRaw(IROP_Branch, nullptr, nullptr);

	s32 * pIInstDst = (s32*)&pInst->m_wordRhs;
	auto pBranch = m_pBlockCur->m_aryBranch.AppendNew();
	pBranch->m_pBlockDest = pBlock;
	pBranch->m_pIInstDst = pIInstDst;

	pInst->m_iBStackOut = 0;
}

SInstruction * CBuilder::PInstCreateCondBranch(SValue * pValPred, SBlock * pBlockTrue, SBlock * pBlockFalse)
{
	auto pInst = PInstCreateRaw(IROP_CondBranch, pValPred, nullptr);

	s32 * pIInstDst = (s32*)&pInst->m_wordRhs;
	auto pBranchTrue = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchTrue->m_pBlockDest = pBlockTrue;
	pBranchTrue->m_pIInstDst = &pIInstDst[true];

	auto pBranchFalse = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchFalse->m_pBlockDest = pBlockFalse;
	pBranchFalse->m_pIInstDst = &pIInstDst[false];

	pInst->m_iBStackOut = 0;
	return pInst;
}

/*
SRecord	CBuilder::AllocLocalVar(u32 cB, u32 cBAlign)
{
	u32 iBStackOut = IBStackAlloc(cB, cBAlign);
	return RecUnsigned(iBStackOut);
}*/

s32	CBuilder::IBStackAlloc(s64 cB, s64 cBAlign)
{
	if (!EWC_FVERIFY(m_pProcCur, "Allocating from the stack without an active procedure"))
		return 0;
	if (cB == 0)
		return 0;
	size_t cBMasked = cBAlign - 1;
	u32 cBStack = U32Coerce((m_pProcCur->m_cBStack + cBMasked) & ~cBMasked);
	m_pProcCur->m_cBStack = cBStack + cB;
	return cBStack;
}

SInstruction * CBuilder::PInstAlloc()
{
	EWC_ASSERT(m_pBlockCur && !m_pBlockCur->FIsFinalized(), "cannot allocate instructions without a unfinalized basic block");
	return m_pBlockCur->m_aryInst.AppendNew();
}

SValue * CBuilder::PValFromSymbol(SSymbol * pSym)
{
	EWC_ASSERT(false, "codegen TBD");
	return nullptr;
}

SInstruction * CBuilder::PInstCreate(IROP irop, SValue * pValLhs, const char * pChzName)
{
	return PInstCreateRaw(irop, pValLhs, nullptr, pChzName);
}

SInstruction * CBuilder::PInstCreate(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName)
{
	return PInstCreateRaw(irop, pValLhs, pValRhs, pChzName);
}

const OpSignature * POpsig(IROP irop)
{
	#define OP(x) 
	#define OPMN(RANGE, x) 
	#define OPMX(RANGE, x) 
	#define OPSIZE(LHS, RHS, RET) {OPSZ_##LHS, OPSZ_##RHS, OPSZ_##RET, (OPSZ_##LHS == OPSZ_CB || OPSZ_##LHS == OPSZ_PCB) ? CBSRC_Lhs : ((OPSZ_##RHS == OPSZ_CB || OPSZ_##RHS == OPSZ_PCB) ? CBSRC_Rhs : CBSRC_Nil) },
	static const OpSignature s_mpIropOpsig [] =
	{
		OPCODE_LIST
	};
	#undef OPSIZE
	#undef OPMX
	#undef OPMN
	#undef OP

	EWC_CASSERT(EWC_DIM(s_mpIropOpsig) == IROP_Max, "missing OpSignature string");
	return &s_mpIropOpsig[irop];
}

static inline bool FIsValidCBOperand(int cBOperand)
{
	return (cBOperand == 1) | (cBOperand == 2) | (cBOperand == 4) | (cBOperand == 8);
}

static inline bool FDefinesCB(OPSZ opsz)
{
	return (opsz == OPSZ_CB) || (opsz == OPSZ_PCB);
}

inline void SetOperandFromValue(SValue * pValSrc, OPK * pOpkOut, SWord * pWordOut, int * pCBOperand)
{
	if (!pValSrc)
		return;

	switch (pValSrc->m_valk)
	{
	case VALK_Constant:
	case VALK_BCodeRegister:
	{
		auto pConst = PValDerivedCast<SConstant *>(pValSrc);

		*pOpkOut = pConst->m_opk;
		pWordOut->m_u64 = pConst->m_word.m_u64;
		*pCBOperand = pConst->m_litty.m_cBit / 8;
		EWC_ASSERT(FIsValidCBOperand(*pCBOperand), "unexpected operand size.");
	} break;
	case VALK_Instruction:
	{
		auto pInst = PValDerivedCast<SInstruction *>(pValSrc);
		//*pOpkOut = OPK_Literal; // BB - is this ever an arg register?
		*pOpkOut = OPK_Register; // BB - is this ever an arg register?
		pWordOut->m_s32 = pInst->m_iBStackOut;
		*pCBOperand = pInst->m_cBOperand;
		EWC_ASSERT(FIsValidCBOperand(*pCBOperand), "unexpected operand size from instruction.");
	} break;
	default: 
		EWC_ASSERT(false, "unhandled VALK");
		break;
	}
}
CBuilder::Instruction *	CBuilder::PInstCreateRaw(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName)
{
	return PInstCreateRaw(irop, -1, pValLhs, pValRhs, pChzName);
}

CBuilder::Instruction *	CBuilder::PInstCreateRaw(IROP irop, s64 cBOperandArg, SValue * pValLhs, SValue * pValRhs, const char * pChzName)
{
	if (!EWC_FVERIFY(m_pBlockCur, "creating instruction with no active block"))
		return nullptr;

	if (m_pBlockCur->FIsFinalized())
	{
		if (irop != IROP_Branch && EWC_FVERIFY(m_pBerrctx, "trying to throw warning with no error context"))
		{
			EmitWarning(m_pBerrctx->m_pErrman, m_pBerrctx->m_pLexloc, ERRID_UnreachableInst, "Unreachable instruction detected");
		}
		irop = IROP_Error;
		pValLhs = nullptr;
		pValRhs = nullptr;
	}

	auto pOpsig = POpsig(irop);
	auto pInst = PInstAlloc();
	pInst->m_irop = irop;

	int cBLhs = 0;
	if (pValLhs)
	{
		EWC_ASSERT(pOpsig->m_opszLhs != OPSZ_0, "unexpected LHS operand");
		SetOperandFromValue(pValLhs, &pInst->m_opkLhs, &pInst->m_wordLhs, &cBLhs);

		// BB - ugly exception for void returns 
//		EWC_ASSERT(cBLhs != 0 || (irop == IROP_Ret), "expected LHS operand, but has zero size (irop = %s)", PChzFromIrop(irop));
	}

	int cBRhs = 0;
	if (pValRhs)
	{
		EWC_ASSERT(pOpsig->m_opszRhs != OPSZ_0, "unexpected RHS operand");
		SetOperandFromValue(pValRhs, &pInst->m_opkRhs, &pInst->m_wordRhs, &cBRhs);

		EWC_ASSERT(cBRhs != 0, "expected RHS operand, but has zero size");
	}
	EWC_ASSERT(!FDefinesCB(pOpsig->m_opszRhs) || !FDefinesCB(pOpsig->m_opszLhs) || cBLhs == cBRhs, "operand size mismatch");

	u32 cBOperand = 0;
	switch (pOpsig->m_cbsrc)
	{
	case CBSRC_Lhs:	
		EWC_ASSERT(cBOperandArg < 0, "passing cBOperand into irop with CBSRC");
		cBOperand = cBLhs;
		break;
	case CBSRC_Rhs:		
		EWC_ASSERT(cBOperandArg < 0, "passing cBOperand into irop with CBSRC");
		cBOperand = cBRhs;	
		break;
	default:			
		if (cBOperandArg < 0)
		{
			EWC_ASSERT(pOpsig->m_opszRet != OPSZ_CB && pOpsig->m_opszRet != OPSZ_PCB, "unable to determine OPSZ_CB");
		}
		else
		{
			cBOperand = S8Coerce(cBOperandArg);	
		}
		break;
	}

	u32 iBStackOut = 0;
	switch (pOpsig->m_opszRet)
	{
		case OPSZ_1:	iBStackOut = IBStackAlloc(1, 1);						break;
		case OPSZ_2:	iBStackOut = IBStackAlloc(2, 2);						break;
		case OPSZ_4:	iBStackOut = IBStackAlloc(4, 4);						break;
		case OPSZ_8:	iBStackOut = IBStackAlloc(8, 8);						break;
		case OPSZ_CB:	iBStackOut = IBStackAlloc(cBOperand, cBOperand);		break;
		case OPSZ_PCB:	iBStackOut = IBStackAlloc(m_pDlay->m_cBPointer, m_pDlay->m_cBPointer);		break;
		case OPSZ_Ptr:	iBStackOut = IBStackAlloc(m_pDlay->m_cBPointer, m_pDlay->m_cBPointer);		break;
		case OPSZ_RegIdx:iBStackOut = IBStackAlloc(4, 4);						break;

		case OPSZ_0:	// fallthrough
		default:		iBStackOut = 0;
	}
	
	EWC_ASSERT(FIsValidCBOperand(cBOperand) || (cBOperand == 0 && pOpsig->m_cbsrc == CBSRC_Nil), "unexpected operand size.");
	pInst->m_cBOperand = cBOperand;
	pInst->m_iBStackOut = iBStackOut;

	return pInst;
}

SInstruction * CBuilder::PInstCreateError()
{
	auto pInst = PInstAlloc();

	pInst->m_irop = IROP_Error;
	pInst->m_cBOperand = 0;
	pInst->m_iBStackOut = 0;
	return pInst;
}

SInstruction * CBuilder::PInstCreateCast(IROP irop, SValue * pValLhs, STypeInfo * pTinDst, const char * pChzName)
{
	EWC_ASSERT(false, "codegen TBD");
	return nullptr;
}

SInstruction * CBuilder::PInstCreateStore(SValue * pValPDst, SValue * pValSrc)
{
	// store pValSrc into the address pointed to by pValPDst

	return PInstCreate(IROP_Store, pValPDst, pValSrc);
}

void CBuilder::AddManagedVal(SValue * pVal)
{
	m_arypValManaged.Append(pVal);
}


static inline void LoadWord(CVirtualMachine * pVm, SWord * pWord, u32 iB, int cB)
{
	u8 * pB = &pVm->m_pBStack[iB];
	switch (cB)
	{
		case 1:	pWord->m_u8 = *pB;			break;
		case 2:	pWord->m_u16 = *(u16*)pB;	break;
		case 4:	pWord->m_u32 = *(u32*)pB;	break;
		case 8:	pWord->m_u64 = *(u64*)pB;	break;
		default: 
			EWC_ASSERT(false, "unexpected operand byte count (%d)", cB);
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, int cB, SWord * pWordLhs)
{
	if ((pInst->m_opkLhs & FOPK_Dereference) == FOPK_Dereference)
	{
		LoadWord(pVm, pWordLhs, pInst->m_wordLhs.m_s32, cB);
	}
	else
	{
		*pWordLhs = pInst->m_wordLhs;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, int cB, SWord * pWordLhs, SWord * pWordRhs)
{
	if ((pInst->m_opkLhs & FOPK_Dereference) == FOPK_Dereference)
	{
		LoadWord(pVm, pWordLhs, pInst->m_wordLhs.m_s32, cB);
	}
	else
	{
		*pWordLhs = pInst->m_wordLhs;
	}

	//if (pInst->m_opkRhs == OPK_Stack || pInst->m_opkRhs == OPK_Argument)
	if ((pInst->m_opkRhs & FOPK_Dereference) == FOPK_Dereference)
	{
		LoadWord(pVm, pWordRhs, pInst->m_wordRhs.m_s32, cB);
	}
	else
	{
		*pWordRhs = pInst->m_wordRhs;
	}
}

static inline void ReadOpcodesStoreToReg(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordLhs, SWord * pWordRhs)
{
	u8 cB = pInst->m_cBOperand;
	EWC_ASSERT ((pInst->m_opkLhs & FOPK_Dereference) == FOPK_Dereference, "expected register lhs");
	*pWordLhs = pInst->m_wordLhs;

	if ((pInst->m_opkRhs & FOPK_Dereference) == FOPK_Dereference)
	{
		LoadWord(pVm, pWordRhs, pInst->m_wordRhs.m_s32, cB);
	}
	else
	{
		*pWordRhs = pInst->m_wordRhs;
	}
}



/*
template <typename T> struct SWordElement		{ T Lookup(SWord & word) { return word.m_u64; } };
template <> struct SWordElement<s8>				{ T Lookup(SWord & word) { return word.m_s8; } };
template <> struct SWordElement<s16>			{ T Lookup(SWord & word) { return word.m_s16; } };
template <> struct SWordElement<s32>			{ T Lookup(SWord & word) { return word.m_s32; } };
template <> struct SWordElement<s64>			{ T Lookup(SWord & word) { return word.m_s64; } };
template <> struct SWordElement<u8>				{ T Lookup(SWord & word) { return word.m_u8; } };
template <> struct SWordElement<u16>			{ T Lookup(SWord & word) { return word.m_u16; } };
template <> struct SWordElement<u32>			{ T Lookup(SWord & word) { return word.m_u32; } };
template <> struct SWordElement<u64>			{ T Lookup(SWord & word) { return word.m_u64; } };
template <> struct SWordElement<f32>			{ T Lookup(SWord & word) { return word.m_f32; } };
template <> struct SWordElement<f64>			{ T Lookup(SWord & word) { return word.m_f64; } };
*/

// partial specialization to help write op handlers
template <s32 CB> struct SWordOpsize			{ };
template <> struct SWordOpsize<1>			
	{ 
		static s8 Signed(SWord & word)		{ return word.m_s8; }
		static u8 Unsigned(SWord & word)	{ return word.m_u8; }
	};
template <> struct SWordOpsize<2>			
	{ 
		static s16 Signed(SWord & word)		{ return word.m_s16; }
		static u16 Unsigned(SWord & word)	{ return word.m_u16; }
	};
template <> struct SWordOpsize<4>
	{ 
		static s32 Signed(SWord & word)		{ return word.m_s32; }
		static u32 Unsigned(SWord & word)	{ return word.m_u32; }
		static f32 Float(SWord & word)		{ return word.m_f32; }
	};
template <> struct SWordOpsize<8>
	{ 
		static s64 Signed(SWord & word)		{ return word.m_s64; }
		static u64 Unsigned(SWord & word)	{ return word.m_u64; }
		static f64 Float(SWord & word)		{ return word.m_f64; }
	};

template <s32 CB>
bool FEvaluateNCmp(NPRED npred, SWord & wordLhs, SWord & wordRhs)
{
	switch (npred)
	{
	case NPRED_EQ:	return SWordOpsize<CB>::Unsigned(wordLhs) == SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_NE:	return SWordOpsize<CB>::Unsigned(wordLhs) != SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SGT:	return SWordOpsize<CB>::Signed(wordLhs) > SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_UGT:	return SWordOpsize<CB>::Unsigned(wordLhs) > SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SGE:	return SWordOpsize<CB>::Signed(wordLhs) >= SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_UGE:	return SWordOpsize<CB>::Unsigned(wordLhs) >= SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SLT:	return SWordOpsize<CB>::Signed(wordLhs) < SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_ULT:	return SWordOpsize<CB>::Unsigned(wordLhs) < SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SLE:	return SWordOpsize<CB>::Signed(wordLhs) <= SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_ULE:	return SWordOpsize<CB>::Unsigned(wordLhs) <= SWordOpsize<CB>::Unsigned(wordRhs);
	}

	EWC_ASSERT(false, "unhandled predicate type");
	return false;
}

template <s32 CB>
bool FEvaluateGCmp(GPRED gpred, SWord & wordLhs, SWord & wordRhs)
{
	switch (gpred)
	{
	case GPRED_EQ:	return SWordOpsize<CB>::Float(wordLhs) == SWordOpsize<CB>::Float(wordRhs);
	case GPRED_NE:	return SWordOpsize<CB>::Float(wordLhs) != SWordOpsize<CB>::Float(wordRhs);
	case GPRED_GT:	return SWordOpsize<CB>::Float(wordLhs) > SWordOpsize<CB>::Float(wordRhs);
	case GPRED_GE:	return SWordOpsize<CB>::Float(wordLhs) >= SWordOpsize<CB>::Float(wordRhs);
	case GPRED_LT:	return SWordOpsize<CB>::Float(wordLhs) < SWordOpsize<CB>::Float(wordRhs);
	case GPRED_LE:	return SWordOpsize<CB>::Float(wordLhs) <= SWordOpsize<CB>::Float(wordRhs);
	}

	EWC_ASSERT(false, "unhandled predicate type");
	return false;
}

void PrintInstance(CVirtualMachine * pVm, STypeInfo * pTin, u8 * pData)
{
	switch (pTin->m_tink)
	{
    case TINK_Integer:
	{
		auto pTinint = (STypeInfoInteger *)pTin;
		if (pTinint->m_fIsSigned)
		{
			switch (pTinint->m_cBit)
			{
			case 8: FormatCoz(pVm->m_pStrbuf, "%d", *(s8*)pData);	break;
			case 16: FormatCoz(pVm->m_pStrbuf, "%d", *(s16*)pData); break;
			case 32: FormatCoz(pVm->m_pStrbuf, "%d", *(s32*)pData); break;
			case 64: FormatCoz(pVm->m_pStrbuf, "%lld", *(s64*)pData); break;
			default: EWC_ASSERT(false, "unexpected float size");
			}
		}
		else
		{
			switch (pTinint->m_cBit)
			{
			case 8: FormatCoz(pVm->m_pStrbuf, "%u", *(u8*)pData);	break;
			case 16: FormatCoz(pVm->m_pStrbuf, "%u", *(u16*)pData); break;
			case 32: FormatCoz(pVm->m_pStrbuf, "%u", *(u32*)pData); break;
			case 64: FormatCoz(pVm->m_pStrbuf, "%llu", *(u64*)pData); break;
			default: EWC_ASSERT(false, "unexpected float size");
			}
		}
	} break;
    case TINK_Float:
	{
		auto pTinfloat = (STypeInfoFloat *)pTin;
		switch (pTinfloat->m_cBit)
		{
		case 32: FormatCoz(pVm->m_pStrbuf, "%f", *(f32*)pData);	break;
		case 64: FormatCoz(pVm->m_pStrbuf, "%f", *(f64*)pData); break;
		default: EWC_ASSERT(false, "unexpected float size");
		}
	} break;
    case TINK_Bool:
	{
		EWC_ASSERT(pVm->m_pDlay->m_cBBool == sizeof(bool), "unexpected bool size");
		FormatCoz(pVm->m_pStrbuf, "%s", (*(bool*)pData) ? "true" : "false");
	} break;
    case TINK_Pointer:
	{
		FormatCoz(pVm->m_pStrbuf, "%p", pData);	break;
	} break;
    case TINK_Procedure:
	{
		FormatCoz(pVm->m_pStrbuf, "%p", pData);	break;
	} break;
    case TINK_Struct:
	{
		EWC_ASSERT(false, "TBD"); // not writing this until I can test it
	} break;
	case TINK_Enum:
	{
		EWC_ASSERT(false, "TBD"); // not writing this until I can test it
	} break;
    case TINK_Array:
	{
		auto pTinary = (STypeInfoArray *)pTin;
		s64 c;
		u8 * pDataAdj = pData;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed:
			{
				FormatCoz(pVm->m_pStrbuf, "[%d]{", pTinary->m_c);
				c = pTinary->m_c;
			} break;
		case ARYK_Dynamic:
			{
				AppendCoz(pVm->m_pStrbuf, "[..]{");
					
			} break;
		case ARYK_Reference:
			{
				AppendCoz(pVm->m_pStrbuf, "[]{");
				c = *(s64 *)pData;
				pDataAdj = pData + sizeof(s64);
			} break;
		default: 
			EWC_ASSERT(false, "unhandled ARYK");
			break;
		}

		u64 cBElement;
		u64 cBAlignElement;
		CalculateByteSizeAndAlign(pVm->m_pDlay, pTinary->m_pTin, &cBElement, &cBAlignElement);
		size_t cBStride = EWC::CBAlign(cBElement, cBAlignElement);
		for (int i = 0; i < c; ++i)
		{
			PrintInstance(pVm, pTinary->m_pTin, pDataAdj);
			pDataAdj += cBStride;
		}
		AppendCoz(pVm->m_pStrbuf, "}");
	} break;
	case TINK_Qualifier:
	{	
		auto pTinqual = (STypeInfoQualifier *)pTin;
		PrintInstance(pVm, pTinqual->m_pTin, pData);
	} break;
    case TINK_Null:		AppendCoz(pVm->m_pStrbuf, "null"); break;
    case TINK_Void:		AppendCoz(pVm->m_pStrbuf, "void"); break;
    case TINK_Any:		AppendCoz(pVm->m_pStrbuf, "any(tbd)"); break;
	default:
		EWC_ASSERT(false, "unhandled type info kind");
		break;
	}
}

void PrintParameter(CVirtualMachine * pVm, STypeInfo * pTin, SParameter * pParam)
{
	PrintInstance(pVm, pTin, &pVm->m_pBStack[pParam->m_iBStack]);
}

void ExecuteBytecode(CVirtualMachine * pVm, SProcedure * pProcEntry)
{
#if DEBUG_PROC_CALL
	auto pDebcall = pVm->m_aryDebCall.AppendNew();
	pDebcall->m_pInstCall = nullptr;
	pDebcall->m_pBStackSrc = pVm->m_pBStack;
	pDebcall->m_pBStackArg = pVm->m_pBStack - pProcEntry->m_cBArg;
	pDebcall->m_pBStackDst = pVm->m_pBStack - (pProcEntry->m_cBArg + pProcEntry->m_cBStack);
	pDebcall->m_pBReturnStorage = nullptr;
#endif

	pVm->m_pProcCurDebug = pProcEntry;
	pVm->m_pInst = pProcEntry->m_aryInst.A();

	// build the stack frame for our top level procedure
	u8 * pBStack = pVm->m_pBStack;

	pBStack -= sizeof(SInstruction*);
	*(SInstruction **)(pBStack) = nullptr;
	pBStack -= pProcEntry->m_cBStack;

	if (pVm->m_pStrbuf)
	{
		AppendCoz(pVm->m_pStrbuf, "{");
	}

	pVm->m_pBStack = pBStack;

	#define MASHOP(OP, CB)						(u32)(OP | (CB << 16))
	#define FETCH(IB, TYPE)						*(TYPE *)&pVm->m_pBStack[IB]
	#define STORE(IBOUT, TYPE, VALUE)			*(TYPE *)&pVm->m_pBStack[IBOUT] = VALUE

	SWord wordLhs, wordRhs;

	SInstruction * pInstMin = pVm->m_pInst;
	SInstruction * pInst = pInstMin;
	while (1)
	{
		switch (MASHOP(pInst->m_irop, pInst->m_cBOperand))
		{
		case MASHOP(IROP_Alloca, 1):	
		case MASHOP(IROP_Alloca, 2):	
		case MASHOP(IROP_Alloca, 4):	
		case MASHOP(IROP_Alloca, 8):	
			{
				u8 * ptr = &pVm->m_pBStack[pInst->m_wordLhs.m_s32];
				*(u8 **)&pVm->m_pBStack[pInst->m_iBStackOut] = ptr;
			} break;

		case MASHOP(IROP_Store, 1):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs, &wordRhs); 
			*((u8*)wordLhs.m_pV) = wordRhs.m_u8;	
			break;
		case MASHOP(IROP_Store, 2):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs, &wordRhs); *((u16*)wordLhs.m_pV) = wordRhs.m_u16;	break;
		case MASHOP(IROP_Store, 4):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs, &wordRhs); *((u32*)wordLhs.m_pV) = wordRhs.m_u32;	break;
		case MASHOP(IROP_Store, 8):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs, &wordRhs); *((u64*)wordLhs.m_pV) = wordRhs.m_u64;	break;

		case MASHOP(IROP_Load, 1):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs); 
			STORE(pInst->m_iBStackOut, u8, *(u8*)wordLhs.m_pV); 
			break;
		case MASHOP(IROP_Load, 2):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs); STORE(pInst->m_iBStackOut, u16, *(u16*)wordLhs.m_pV); break;
		case MASHOP(IROP_Load, 4):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs); STORE(pInst->m_iBStackOut, u32, *(u32*)wordLhs.m_pV); break;
		case MASHOP(IROP_Load, 8):	
			ReadOpcodes(pVm, pInst, sizeof(u8*), &wordLhs); STORE(pInst->m_iBStackOut, u64, *(u64*)wordLhs.m_pV); break;

		case MASHOP(IROP_NAdd, 1):	
			{ 
				ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 + wordRhs.m_u8);
			}	break;
		case MASHOP(IROP_NAdd, 2):	
			{ 
				ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 + wordRhs.m_u16);
			}	break;
		case MASHOP(IROP_NAdd, 4):	
			{ 
				ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 + wordRhs.m_u32);
			}	break;
		case MASHOP(IROP_NAdd, 8):	
			{ 
				ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 + wordRhs.m_u64);
			}	break;

		case MASHOP(IROP_NTrace, 1):	
			ReadOpcodes(pVm, pInst, 1, &wordLhs); printf("1byte %d\n", wordLhs.m_u8); break;
		case MASHOP(IROP_NTrace, 2):	
			ReadOpcodes(pVm, pInst, 2, &wordLhs); printf("2byte %d\n", wordLhs.m_u16); break;
		case MASHOP(IROP_NTrace, 4):	
			ReadOpcodes(pVm, pInst, 4, &wordLhs); printf("4byte %d\n", wordLhs.m_u32); break;
		case MASHOP(IROP_NTrace, 8):	
			ReadOpcodes(pVm, pInst, 8, &wordLhs); printf("8byte %lld\n", wordLhs.m_u64); break;

		case MASHOP(IROP_TraceStore, 0):	
		case MASHOP(IROP_TraceStore, 1):	
		case MASHOP(IROP_TraceStore, 2):	
		case MASHOP(IROP_TraceStore, 4):	
		case MASHOP(IROP_TraceStore, 8):	
		{
			if (pVm->m_pStrbuf)
			{
				auto pTin = (STypeInfo *)pInst->m_wordRhs.m_pV;
				PrintInstance(pVm, pTin, &pVm->m_pBStack[pInst->m_wordLhs.m_s32]);
				AppendCoz(pVm->m_pStrbuf, ";");
			}
		} break;

			// Store value to virtual register (pV, value)
		case MASHOP(IROP_StoreToReg, 1):	
			ReadOpcodesStoreToReg(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_wordLhs.m_s32, u8, wordRhs.m_u8); 
			break;
		case MASHOP(IROP_StoreToReg, 2):	
			ReadOpcodesStoreToReg(pVm, pInst, &wordLhs, &wordRhs);
			STORE(pInst->m_wordLhs.m_s32, u16, wordRhs.m_u16);
			break;
		case MASHOP(IROP_StoreToReg, 4):	
			ReadOpcodesStoreToReg(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_wordLhs.m_s32, u32, wordRhs.m_u32); 
			break;
		case MASHOP(IROP_StoreToReg, 8):	
			EWC_ASSERT((pInst->m_opkLhs & FOPK_Dereference) != 0, "expected register for destination");
			ReadOpcodesStoreToReg(pVm, pInst, &wordLhs, &wordRhs);
			STORE(pInst->m_wordLhs.m_s32, u64, wordRhs.m_u64); 
			break;

			// Store value to virtual register (pV, value)
		case MASHOP(IROP_StoreToIdx, 1):	
			ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); 
			STORE(wordLhs.m_s32, u8, wordRhs.m_u8); 
			break;
		case MASHOP(IROP_StoreToIdx, 2):	
			ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs);
			STORE(wordLhs.m_s32, u16, wordRhs.m_u16);
			break;
		case MASHOP(IROP_StoreToIdx, 4):	
			ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); 
			STORE(wordLhs.m_s32, u32, wordRhs.m_u32); 
			break;
		case MASHOP(IROP_StoreToIdx, 8):	
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs);
			STORE(wordLhs.m_s32, u64, wordRhs.m_u64); 
			break;

		case MASHOP(IROP_NCmp, 1):
			{
				ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs);
				bool fEval = FEvaluateNCmp<1>((NPRED)pInst->m_pred, wordLhs, wordRhs);
				STORE(pInst->m_iBStackOut, u8, fEval);
			} break;
		case MASHOP(IROP_NCmp, 2):
			ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u16, FEvaluateNCmp<2>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(IROP_NCmp, 4):
			ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u32, FEvaluateNCmp<4>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(IROP_NCmp, 8):
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u64, FEvaluateNCmp<8>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(IROP_GCmp, 4):
			ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u32, FEvaluateGCmp<4>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(IROP_GCmp, 8):
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u64, FEvaluateGCmp<8>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(IROP_Call, 0):
		case MASHOP(IROP_Call, 1):
		case MASHOP(IROP_Call, 2):
		case MASHOP(IROP_Call, 4):
		case MASHOP(IROP_Call, 8):
		{
			ReadOpcodes(pVm, pInst, 1, &wordLhs);

			auto pProc = (SProcedure *)wordLhs.m_pV;

			SInstruction ** ppInstRet = (SInstruction **)(pVm->m_pBStack - sizeof(SInstruction *));
			
#if DEBUG_PROC_CALL
			auto pDebcall = pVm->m_aryDebCall.AppendNew();
			pDebcall->m_pInstCall = pInst;
			pDebcall->m_pBStackSrc = pVm->m_pBStack;
			pDebcall->m_pBStackArg = pVm->m_pBStack - pProc->m_cBArg;
			pDebcall->m_pBStackDst = pVm->m_pBStack - (pProc->m_cBArg + pProc->m_cBStack);
			pDebcall->m_pBReturnStorage = &pVm->m_pBStack[pInst->m_iBStackOut];
#endif 
			pVm->m_pBStack -= pProc->m_cBArg;
			if (pVm->m_pStrbuf)
			{
				FormatCoz(pVm->m_pStrbuf, "%s(", pProc->m_pTinproc->m_strName.PCoz());
				STypeInfoProcedure * pTinproc = pProc->m_pTinproc;
				for (int iParam = 0; iParam < pTinproc->m_arypTinParams.C(); ++iParam)
				{
					if (iParam > 0)
						AppendCoz(pVm->m_pStrbuf, ", ");

					SParameter * pParam = &pProc->m_aParamArg[iParam];
					PrintParameter(pVm, pTinproc->m_arypTinParams[iParam], pParam);
				}
				AppendCoz(pVm->m_pStrbuf, "){");
			}

			pVm->m_pBStack -= pProc->m_cBStack;
			EWC_ASSERT((uintptr_t(pVm->m_pBStack) & (pVm->m_pDlay->m_cBStackAlign - 1)) == 0,
				"stack frame should be %d byte aligned.", pVm->m_pDlay->m_cBStackAlign);
			EWC_ASSERT(uintptr_t(pVm->m_pBStack) >= uintptr_t(pVm->m_pBStackMin), "stack overflow");

			//printf("IROP_Call) pBStack = %p, ppInst = %p, cBStack = %lld, cBArg = %lld\n", pVm->m_pBStack, ppInstRet, pProc->m_cBStack, pProc->m_cBArg);
			
			*ppInstRet = pInst;

			pInst = pProc->m_aryInst.A() - 1; // this will be incremented below
			pVm->m_pProcCurDebug = pProc;

		} break;

		case MASHOP(IROP_Ret, 0):
		case MASHOP(IROP_Ret, 1):
		case MASHOP(IROP_Ret, 2):
		case MASHOP(IROP_Ret, 4):
		case MASHOP(IROP_Ret, 8):
		{
			if (pInst->m_cBOperand == 0)
			{
				wordLhs.m_u64 = 0;
			}
			else
			{
				ReadOpcodes(pVm, pInst, pInst->m_cBOperand, &wordLhs);
			}

			u8 * pBStackCalled = pVm->m_pBStack;
			pVm->m_pBStack += pInst->m_wordLhs.m_s32; // cBArg + cBStack
			SInstruction ** ppInstRet = ((SInstruction **)pVm->m_pBStack) - 1;
			auto pProcCalled = pVm->m_pProcCurDebug;
#if DEBUG_PROC_CALL
			auto debcall = pVm->m_aryDebCall.TPopLast();
			EWC_ASSERT(debcall.m_pInstCall == *ppInstRet, "bad return instruction");
			EWC_ASSERT(debcall.m_pBStackDst == pBStackCalled, "called proc stack frame mismatch");
			EWC_ASSERT(debcall.m_pBStackArg == pBStackCalled + pProcCalled->m_cBStack, "called proc stack frame mismatch");
			EWC_ASSERT(debcall.m_pBStackSrc == pVm->m_pBStack, "source proc stack frame mismatch");
#endif

			if (pVm->m_pStrbuf && EWC_FVERIFY(pProcCalled, "missing called proc"))
			{
				u8 * pBStackArg = pBStackCalled + pProcCalled->m_cBStack;
				auto pTinproc = pProcCalled->m_pTinproc;

				if (pTinproc->m_arypTinReturns.C())
				{
					AppendCoz(pVm->m_pStrbuf, "}->");
					for (int ipTin = 0; ipTin < pTinproc->m_arypTinReturns.C(); ++ipTin)
					{
						SParameter * pParam = &pProcCalled->m_aParamRet[ipTin];
						auto iBStackRet = *(s32*)&pBStackArg[pParam->m_iBStack];

#if DEBUG_PROC_CALL
						EWC_ASSERT(ipTin > 0 || debcall.m_pBReturnStorage == &pVm->m_pBStack[iBStackRet], "bad return storage calculation");
#endif
						PrintInstance(pVm, pTinproc->m_arypTinReturns[ipTin], &pVm->m_pBStack[iBStackRet]);
					}
				}
				else
				{
					AppendCoz(pVm->m_pStrbuf, "}");
				}
				AppendCoz(pVm->m_pStrbuf, "; ");
			}

			if (*ppInstRet == nullptr)
			{
				return; // halt
			}

			auto pInstCall = *ppInstRet;
			EWC_ASSERT(pInstCall->m_irop == IROP_Call, "procedure return did not return to call instruction");

			auto pProcPrev = (SProcedure *)pInstCall->m_wordRhs.m_pV;
			pVm->m_pProcCurDebug = pProcPrev;
			pInst = pInstCall;
			
		} break;

		case MASHOP(IROP_CondBranch, 0):
		{
			ReadOpcodes(pVm, pInst, 1, &wordLhs);
			EWC_ASSERT(wordLhs.m_u8 >= 0 && wordLhs.m_u8 <= 1, "expected 0 or 1");

			u8 iOp = (wordLhs.m_u8 != 0);
			s32 * pIInst = (s32*)&pInst->m_wordRhs;
			s32 iInst = pIInst[iOp];

			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;
		case MASHOP(IROP_Branch, 0):
		{	
			//ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			s32 iInst = pInst->m_wordRhs.m_s32;

			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;

		default:

			EWC_ASSERT(false, "unhandled opcode IROP_%s\n", PChzFromIrop(pInst->m_irop));
		}
		++pInst;
	}

	#undef MASHOP
	#undef FETCH
	#undef MASHOP
}

SProcedure * PProcLookup(CVirtualMachine * pVm, HV hv)
{
	SProcedure ** ppProc = pVm->m_hashHvMangledPProc.Lookup(hv);
	if (!ppProc)
		return nullptr;

	return *ppProc;
}

CVirtualMachine::CVirtualMachine(u8 * pBStackMin, u8 * pBStackMax, SDataLayout * pDlay)
:m_pDlay(pDlay)
,m_pInst(nullptr)
,m_pInstArgMin(nullptr)
,m_pBStackMin(pBStackMin)
,m_pBStackMax(pBStackMax)
,m_pBStack(pBStackMax)
,m_pProcCurDebug(nullptr)
,m_pStrbuf(nullptr)
#if DEBUG_PROC_CALL
,m_aryDebCall()
#endif
{
}

SConstant * CBuilder::PConstPointer(void * pV)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_opk = OPK_Literal;
	pConst->m_word.m_pV = pV;

	pConst->m_litty.m_litk = LITK_Pointer;
	pConst->m_litty.m_cBit = sizeof(pV) * 8;
	pConst->m_litty.m_fIsSigned = false;
	return pConst;
}

SConstant * CBuilder::PConstInt(u64 nUnsigned, int cBit, bool fIsSigned)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_opk = OPK_Literal;
	pConst->m_word.m_u64 = nUnsigned;

	pConst->m_litty.m_litk = LITK_Integer;
	pConst->m_litty.m_cBit = cBit;
	pConst->m_litty.m_fIsSigned = fIsSigned;
	return pConst;
}

SConstant * CBuilder::PConstFloat(f64 g, int cBit)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_opk = OPK_Literal;
	pConst->m_word.m_f64 = g;

	pConst->m_litty.m_litk = LITK_Float;
	pConst->m_litty.m_cBit = cBit;
	pConst->m_litty.m_fIsSigned = true;
	return pConst;
}

CBuilder::LValue * CBuilder::PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName)
{
	EWC_ASSERT(false, "codegen TBD");
	return nullptr;
}

CBuilder::LValue * CBuilder::PLvalConstantNull(LType * pLtype)
{
	EWC_ASSERT(false, "codegen TBD");
	return nullptr;
}

CBuilder::LValue * CBuilder::PLvalConstantArray(LType * pLtype, LValue ** apLval, u32 cpLval)
{
	EWC_ASSERT(false, "codegen TBD");
	return nullptr;
}

CBuilder::Constant * CBuilder::PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval)
{
	EWC_ASSERT(false, "codegen TBD");
	return nullptr;
}

SConstant * CBuilder::PConstArg(s64 n, int cBit, bool fIsSigned)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_opk = OPK_LiteralArg;
	pConst->m_word.m_s64 = n;

	pConst->m_litty.m_litk = LITK_Integer;
	pConst->m_litty.m_cBit = cBit;
	pConst->m_litty.m_fIsSigned = fIsSigned;
	return pConst;
}

SRegister * CBuilder::PReg(s64 n, int cBit, bool fIsSigned)
{
	EWC_CASSERT(sizeof(SRegister) == sizeof(SConstant), "size mismatch between SRegister and SConstant");
	auto pReg = (SRegister *)m_blistConst.AppendNew();
	pReg->m_opk = OPK_Register;
	pReg->m_word.m_s64 = n;

	pReg->m_litty.m_litk = LITK_Integer;
	pReg->m_litty.m_cBit = cBit;
	pReg->m_litty.m_fIsSigned = fIsSigned;
	return pReg;
}

SRegister * CBuilder::PRegArg(s64 n, int cBit, bool fIsSigned)
{
	EWC_CASSERT(sizeof(SRegister) == sizeof(SConstant), "size mismatch between SRegister and SConstant");
	auto pReg = (SRegister *)m_blistConst.AppendNew();
	pReg->m_opk = OPK_RegisterArg;
	pReg->m_word.m_s64 = n;

	pReg->m_litty.m_litk = LITK_Integer;
	pReg->m_litty.m_cBit = cBit;
	pReg->m_litty.m_fIsSigned = fIsSigned;
	return pReg;
}



void BuildStubDataLayout(SDataLayout * pDlay)
{
	pDlay->m_cBBool = 1;
	pDlay->m_cBInt = sizeof(int);
	pDlay->m_cBFloat = sizeof(float);
	pDlay->m_cBPointer = sizeof(void *);
	pDlay->m_cBStackAlign = sizeof(void *);
}

void BuildTestByteCode(CWorkspace * pWork, EWC::CAlloc * pAlloc)
{
	SDataLayout dlay;
	BuildStubDataLayout(&dlay);

	CBuilder buildBc(pWork, &dlay);

	EWC::CHash<HV, STypeInfo *>	hashHvPTin(pAlloc, BK_ByteCodeTest);
	SUniqueNameSet unsetTin(pAlloc, BK_ByteCodeTest);
	auto pSymtab = PSymtabNew(pAlloc, nullptr, "bytecode", &unsetTin, &hashHvPTin);
	pSymtab->AddBuiltInSymbols(pWork);

	auto pTinprocMain = PTinprocAlloc(pSymtab, 0, 0, "main");
	pTinprocMain->m_strMangled = pTinprocMain->m_strName;

	auto pTinprocPrint = PTinprocAlloc(pSymtab, 2, 0, "print");
	pTinprocPrint->m_strMangled = pTinprocPrint->m_strName;
	auto pTinS8 = pSymtab->PTinBuiltin("s8");
	auto pTinS16 = pSymtab->PTinBuiltin("s16");
	auto pTinS32 = pSymtab->PTinBuiltin("s32");
	pTinprocPrint->m_arypTinParams.Append(pTinS16);
	pTinprocPrint->m_arypTinParams.Append(pTinS32);

	auto pTinprocSum = PTinprocAlloc(pSymtab, 2, 1, "sum");
	pTinprocSum->m_strMangled = pTinprocSum->m_strName;
	pTinprocSum->m_arypTinParams.Append(pTinS32);
	pTinprocSum->m_arypTinParams.Append(pTinS32);
	pTinprocSum->m_arypTinReturns.Append(pTinS32);

	auto pProcPrint = buildBc.PProcCreate(pWork, pTinprocPrint, nullptr);
	auto pBlockPrint = buildBc.PBlockCreate(pProcPrint);
	buildBc.ActivateProc(pProcPrint, pBlockPrint);
	{
		//auto pRegVal = buildBc.PValCreateAlloca(pTinS8, 1);

		(void)buildBc.PInstCreate(IROP_NTrace, buildBc.PRegArg(pProcPrint->m_aParamArg[0].m_iBStack, 16));
		(void)buildBc.PInstCreate(IROP_NTrace, buildBc.PRegArg(pProcPrint->m_aParamArg[1].m_iBStack, 32));

		buildBc.PInstCreateTraceStore(buildBc.PReg(pProcPrint->m_aParamArg[0].m_iBStack, 16), pTinS16);
		buildBc.PInstCreateTraceStore(buildBc.PReg(pProcPrint->m_aParamArg[1].m_iBStack, 32), pTinS32);
		//(void) buildBc.PInstCreate(IROP_Ret, nullptr);
		buildBc.CreateReturn(nullptr, 0);
	}

	buildBc.ActivateProc(nullptr, nullptr);
	buildBc.FinalizeProc(pProcPrint);


	auto pProcSum = buildBc.PProcCreate(pWork, pTinprocSum, nullptr);
	auto pBlockSum = buildBc.PBlockCreate(pProcSum);
	buildBc.ActivateProc(pProcSum, pBlockSum);
	
	{
		auto pInstRhs = buildBc.PInstCreate(
									IROP_NAdd,
									buildBc.PRegArg(pProcSum->m_aParamArg[0].m_iBStack, pProcSum->m_aParamArg[0].m_cB * 8),
									buildBc.PRegArg(pProcSum->m_aParamArg[1].m_iBStack, pProcSum->m_aParamArg[1].m_cB * 8));

		buildBc.CreateReturn((SValue**)&pInstRhs, 1);
		//void) buildBc.RecAddInst(IROP_Store, 4, RecSigned(pProcSum->m_aParamRet[0].m_iBStack), recRhs);
	}

	buildBc.ActivateProc(nullptr, nullptr);
	buildBc.FinalizeProc(pProcSum);


	auto pProc = buildBc.PProcCreate(pWork, pTinprocMain, nullptr);
	auto pBlockPre = buildBc.PBlockCreate(pProc);
	buildBc.ActivateProc(pProc, pBlockPre);
	
	auto pRegPtr = buildBc.PValCreateAlloca(pTinS8, 1);
	(void) buildBc.PInstCreate(IROP_Store, pRegPtr, buildBc.PConstInt(55, 8));
	auto pRegVal = buildBc.PInstCreate(IROP_Load, pRegPtr);
	(void) buildBc.PInstCreateTraceStore(pRegVal, pTinS8);
	//auto pConstVarVal = buildBc.PReg(pInstVarAddr->m_iBStackOut, 8);

	//auto recVarAddr = buildBc.AllocLocalVar(1, 1);
	//auto recVarVal = buildBc.RecAddInst(IROP_Store, 1, recVarAddr, RecSigned(55));

	auto pInstRhs = buildBc.PInstCreate(IROP_NAdd, buildBc.PConstInt(5, 8), buildBc.PConstInt(3, 8));
	auto pInstOut = buildBc.PInstCreate(IROP_NAdd, pInstRhs, pRegVal);

	auto pInstCmp = buildBc.PInstCreateNCmp(NPRED_SGT, pInstOut, buildBc.PConstInt(100, 8));
	auto pBlockTrue = buildBc.PBlockCreate(pProc);
	auto pBlockFalse = buildBc.PBlockCreate(pProc);
	auto pBlockPost = buildBc.PBlockCreate(pProc);
	(void) buildBc.PInstCreateCondBranch(pInstCmp, pBlockTrue, pBlockFalse);

	buildBc.ActivateBlock(pBlockTrue);
	(void) buildBc.PInstCreate(IROP_StoreToReg, pRegVal, buildBc.PConstInt(11, 8));
	buildBc.CreateBranch(pBlockPost);

	buildBc.ActivateBlock(pBlockFalse);
	(void) buildBc.PInstCreate(IROP_StoreToReg, pRegVal, buildBc.PConstInt(22, 8));
	buildBc.CreateBranch(pBlockPost);

	buildBc.ActivateBlock(pBlockPost);
	//pInstVarVal = buildBc.PInstCreate(IROP_Load, pRegVal);
	(void) buildBc.PInstCreate(IROP_NTrace, pRegVal);

	SValue * apVal[2];
	apVal[0] = buildBc.PConstInt(111, 16);
	apVal[1] = buildBc.PConstInt(222, 32);
	buildBc.PInstCreateCall(pProcPrint, apVal, 2);

	auto pRegPInt = buildBc.PValCreateAlloca(pTinS32, 1);
	(void) buildBc.PInstCreate(IROP_Store, pRegPInt, buildBc.PConstInt(444, 32));
	auto pRegIntVal = buildBc.PInstCreate(IROP_Load, pRegPInt);

	apVal[0] = buildBc.PConstInt(333, 32);
	apVal[1] = pRegIntVal;
	buildBc.PInstCreateCall(pProcPrint, apVal, 2);

	apVal[0] = buildBc.PConstInt(123, 32);
	apVal[1] = buildBc.PConstInt(111, 32);
	auto pInstRet = buildBc.PInstCreateCall(pProcSum, apVal, 2);
	(void) buildBc.PInstCreate(IROP_NTrace, pInstRet);

	buildBc.CreateReturn(nullptr, 0);

	buildBc.ActivateProc(nullptr, nullptr);
	buildBc.FinalizeProc(pProc);

	static const u32 s_cBStackMax = 2048;
	u8 * pBStack = (u8 *)pAlloc->EWC_ALLOC(s_cBStackMax, 16);

	char aCh[2048];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	CVirtualMachine vm(pBStack, &pBStack[s_cBStackMax], &dlay);
	vm.m_pStrbuf = &strbuf;

#if DEBUG_PROC_CALL
	vm.m_aryDebCall.SetAlloc(pAlloc, BK_ByteCode, 32);
#endif

	ExecuteBytecode(&vm, pProc);

	EWC::EnsureTerminated(&strbuf, '\0');
	printf("%s\n", aCh);
}
} // namespace BCode 

// [x] generate instructions into basic blocks
// [ ] plumb #bctrace through other phases for unit testing
// [ ] add bcode string to unit test system