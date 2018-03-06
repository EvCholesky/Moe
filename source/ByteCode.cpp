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
#include "parser.h"
#include "stdio.h"
#include "workspace.h"


// Stack layout (grows down)
//	4 |___null____________________| +
//	3 |___childRet________________| | 
//	2 |___working_________________| |
//	1 |___Local var_______________| | 
//	0 |___Local var_______________|_v_	iBStack(0) for main
// -1 |___pInst return____________|		address of call instruction from main
//	  |.................. pad arg.|						
// a2 |___return__________________| |	iBStack for return storage in main stack frame
// a1 |___arg 1___________________| |  
// a0 |___arg 0___________________|_v_	iBStack(0) arg frame for clide proc
//	  |.................. pad ....|
//	3 |___________________________| +  
//	2 |___________________________| |
//	1 |___working_________________| |  childProc (a,b) -> childRet
//	0 |___local var_______________|_v_  



using namespace EWC;

void CalculateByteSizeAndAlign(SDataLayout * pDlay, STypeInfo * pTin, s64 * pcB, s64 * pcBAlign)
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

OPARG OpargFromOp(OP op)
{
	if (op > OP_Max)
		return OPARG_Nil;

#define OP(x)
#define OPARG(ARG) OPARG_##ARG
#define OP_RANGE(range, PREV_VAL)
	static const OPARG s_mpOpOparg [] =
	{
		BC_OPCODE_LIST
	};
#undef OP_RANGE
#undef OP
#undef OPARG

	return s_mpOpOparg[op];
}

const char * PChzFromOp(OP op)
{
	if (op > OP_Max)
		return "Error";

#define OP(x) #x
#define OPARG(ARG)
#define OP_RANGE(range, PREV_VAL)
	static const char * s_mpOpPChz [] =
	{
		BC_OPCODE_LIST
	};
#undef OP_RANGE
#undef OP
#undef OPARG

	return s_mpOpPChz[op];
}

SProcedure::SProcedure(EWC::CAlloc * pAlloc, STypeInfoProcedure * pTinproc)
:m_pTinproc(pTinproc)
,m_cBStack(0)
,m_cBArg(0)
,m_pBlockEntry(nullptr)
,m_arypBlock(pAlloc, BK_ByteCodeCreator, 16)
,m_aryInst(pAlloc, BK_ByteCode, 0)
,m_aParamArg(nullptr)
,m_aParamRet(nullptr)
{
}



CBuilder::CBuilder(EWC::CAlloc * pAlloc, SDataLayout * pDlay)
:m_pAlloc(pAlloc)
,m_pDlay(pDlay)
,m_hashHvMangledPProc(pAlloc, BK_ByteCodeCreator, 256)
,m_arypBlockManaged(pAlloc, BK_ByteCodeCreator, 256)
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
{
}

static inline s64 IBArgAlloc(s64 * pcBArg, s64 cB, s64 cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	s64 cBStack = U32Coerce((*pcBArg + cBMasked) & ~cBMasked);
	*pcBArg = cBStack + cB;
	return cBStack;
}

SProcedure * CBuilder::PProcCreate(STypeInfoProcedure * pTinproc)
{
	size_t cArg = pTinproc->m_arypTinParams.C();
	size_t cRet = pTinproc->m_arypTinReturns.C();
	size_t cBAlloc = sizeof(SProcedure) + sizeof(SParameter) * (cArg + cRet);
	cBAlloc = EWC::CBAlign(cBAlloc, EWC_ALIGN_OF(SParameter));

	u8 * pBAlloc = (u8 *)m_pAlloc->EWC_ALLOC(cBAlloc, EWC_ALIGN_OF(SProcedure));

	auto pProc = new(pBAlloc) SProcedure(m_pAlloc, pTinproc);
	auto fins = m_hashHvMangledPProc.FinsEnsureKeyAndValue(pTinproc->m_strMangled.Hv(), pProc);
	EWC_ASSERT(fins == FINS_Inserted, "adding procedure that already exists");
	
	SParameter * aParamArg = (SParameter *)PVAlign(pBAlloc + sizeof(SProcedure), EWC_ALIGN_OF(SParameter));
	if (cArg)
	{
		pProc->m_aParamArg = aParamArg;
	}

	if (cRet)
	{
		pProc->m_aParamRet = aParamArg + cArg;
	}

	s64 cB; 
	s64 cBAlign;
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

void CBuilder::BeginProc(SProcedure * pProc)
{
	EWC_ASSERT(!m_pProcCur, "cannot begin procedure %s; previous procedure %s was not ended", 
		m_pProcCur->m_pTinproc->m_strName.PCoz(), pProc->m_pTinproc->m_strName.PCoz());
	m_pProcCur = pProc;
}

void CBuilder::EndProc(SProcedure * pProc)
{
	EWC_ASSERT(m_pProcCur == pProc, "mismatch ending procedure %s", pProc->m_pTinproc->m_strName.PCoz());
	m_pProcCur = nullptr;
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

SBlock * CBuilder::PBlockCreate()
{
	auto pBlock = EWC_NEW(m_pAlloc, SBlock) SBlock();
	pBlock->m_aryInst.SetAlloc(m_pAlloc, BK_ByteCode, 128);
	pBlock->m_aryBranch.SetAlloc(m_pAlloc, BK_ByteCode, 4);
	m_arypBlockManaged.Append(pBlock);

	return pBlock;
}

void CBuilder::BeginBlock(SBlock * pBlock)
{
	EWC_ASSERT(m_pProcCur, "cannot begin basic block without active procedure");
	EWC_ASSERT(!m_pBlockCur, "cannot begin basic block; previous block was not ended");
	m_pBlockCur = pBlock;

	if (!m_pProcCur->m_pBlockEntry)
	{
		m_pProcCur->m_pBlockEntry = pBlock;
	}

	EWC_ASSERT(!pBlock->m_pProc || pBlock->m_pProc == m_pProcCur, "block activated in multiple procedures");
	if (!pBlock->m_pProc)
	{
		pBlock->m_pProc = m_pProcCur;
		m_pProcCur->m_arypBlock.Append(pBlock);
	}
}

void CBuilder::EndBlock(SBlock * pBlock)
{
	EWC_ASSERT(m_pBlockCur == pBlock, "mismatch ending block");
	m_pBlockCur = nullptr;
}

void CBuilder::AddInst(OP op)
{
	auto oparg = OpargFromOp(op);
	EWC_ASSERT(oparg == OPARG_OnlyOpcode, "unexpected operator '%s' in only opcode instruction", PChzFromOp(op));

	auto pInst = PInstAlloc();
	pInst->m_op = op;
	pInst->m_cB = 0;
	pInst->m_opkLhs = OPK_Nil;
	pInst->m_opkRhs = OPK_Nil;
	
	pInst->m_iBStackOut = 0;
	pInst->m_wordLhs.m_u64 = 0;
	pInst->m_wordRhs.m_u64 = 0;
}

SRecord CBuilder::RecAddInst(OP op, u8 cB, const SRecord & recLhs)
{
	auto oparg = OpargFromOp(op);
	EWC_ASSERT(oparg == OPARG_Unary, "unexpected operator '%s' in unary add inst", PChzFromOp(op));

	auto pInst = PInstAlloc();
	pInst->m_op = op;
	pInst->m_cB = cB;
	pInst->m_opkLhs = recLhs.m_opk;
	pInst->m_opkRhs = OPK_Nil;

	u32 iBStackOut = IBStackAlloc(cB, cB);
	pInst->m_iBStackOut = iBStackOut;

	pInst->m_wordLhs.m_u64 = recLhs.m_word.m_u64;
	pInst->m_wordRhs.m_u64 = 0;
	
	return RecStack(iBStackOut);
}

static inline void RecSetupInst(
	SInstruction * pInst,
	OP op,
	u8 cB,
	const SRecord & recLhs,
	const SRecord & recRhs)
{
	pInst->m_op = op;
	pInst->m_cB = cB;
	pInst->m_opkLhs = recLhs.m_opk;
	pInst->m_opkRhs = recRhs.m_opk;

	pInst->m_wordLhs.m_u64 = recLhs.m_word.m_u64;
	pInst->m_wordRhs.m_u64 = recRhs.m_word.m_u64;
}

SRecord CBuilder::RecAddInst(OP op, u8 cB, const SRecord & recLhs, const SRecord & recRhs)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, op, cB, recLhs, recRhs);

	auto oparg = OpargFromOp(op);
	EWC_ASSERT(oparg == OPARG_Binary || oparg == OPARG_Store, "unexpected operator '%s' in binary add inst", PChzFromOp(op));

	u32 iBStackOut;
	if (oparg == OPARG_Store)
	{
		iBStackOut = recLhs.m_word.m_s32;
	}
	else
	{
		iBStackOut = IBStackAlloc(cB, cB);
	}

	pInst->m_iBStackOut = iBStackOut;
	return RecStack(iBStackOut);
}

SRecord	CBuilder::RecAddNCmp(u8 cB, NPRED npred, const SRecord & recLhs, const SRecord & recRhs)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_NCmp, cB, recLhs, recRhs);

	pInst->m_pred = (u8)npred;

	u32 iBStackOut = IBStackAlloc(1, 1);
	pInst->m_iBStackOut = iBStackOut;
	return RecStack(iBStackOut);
}

SRecord	CBuilder::RecAddGCmp(u8 cB, GPRED gpred, const SRecord & recLhs, const SRecord & recRhs)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_GCmp, cB, recLhs, recRhs);

	pInst->m_pred = (u8)gpred;

	u32 iBStackOut = IBStackAlloc(1, 1);
	pInst->m_iBStackOut = iBStackOut;
	return RecStack(iBStackOut);
}

void CBuilder::AddCall(SProcedure * pProc, SRecord * aRecArg, int cRecArg)
{
	auto rec = RecAddCall(pProc, aRecArg, cRecArg);
	EWC_ASSERT(rec.m_word.m_s32 = -1, "ignoring return argument");
}

SRecord CBuilder::RecAddCall(SProcedure * pProc, SRecord * aRecArg, int cRecArg)
{
	if (!EWC_FVERIFY(m_pProcCur, "Cannot add a procedure call outside of a procedure"))
		return RecSigned(-1);

	if (!EWC_FVERIFY(pProc->m_pTinproc->m_arypTinParams.C() == cRecArg, "variadic args not yet supported"))
		return RecSigned(-1);

	s32 iBStackRet = -1;
	s64 cB;
	s64 cBAlign;
	auto pTinproc = pProc->m_pTinproc;
	auto cBArg = pProc->m_cBArg;
	int cpTinReturn = (int)pTinproc->m_arypTinReturns.C();
	for (int ipTinReturn = 0; ipTinReturn < cpTinReturn; ++ipTinReturn)
	{
		auto pTinRet = pTinproc->m_arypTinReturns[ipTinReturn];

		CalculateByteSizeAndAlign(m_pDlay, pTinRet, &cB, &cBAlign);
		auto iBStack = IBStackAlloc(cB, cBAlign);
		if (!ipTinReturn)
		{
			iBStackRet = iBStack;
		}

		auto pParam = &pProc->m_aParamRet[ipTinReturn];
		(void) RecAddInst(OP_Store, pParam->m_cB, RecSigned(pParam->m_iBStack - cBArg), RecSigned(iBStack));
	}

	for (int iRec = 0; iRec < cRecArg; ++iRec)
	{
		// NOTE: Lhs is relative to called function stack frame, Rhs is relative to calling stack frame
		auto pParam = &pProc->m_aParamArg[iRec];
		(void) RecAddInst(OP_Store, pParam->m_cB, RecSigned(pParam->m_iBStack - cBArg), aRecArg[iRec]);
	}

	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_Call, 0, RecPointer(pProc), RecPointer(m_pProcCur));
	pInst->m_iBStackOut = 0;

	return RecStack(iBStackRet);
}

void CBuilder::AddReturn(SRecord * aRecRet, int cReturn)
{
	if (!EWC_FVERIFY(m_pProcCur, "Cannot add a return opcode outside of a procedure"))
		return;

	for (int iReturn = 0; iReturn < cReturn; ++iReturn)
	{
		auto recOffset = RecAddInst(OP_NAdd, 4, RecArg(m_pProcCur->m_aParamRet[0].m_iBStack), RecArgLiteral(m_pProcCur->m_cBArg));
		RecAddInst(OP_Store, 4, recOffset, aRecRet[iReturn]);
	}

	auto pInst = PInstAlloc(); 

	RecSetupInst(pInst, OP_Return, 0, RecArgLiteral(m_pProcCur->m_cBArg), RecSigned(cReturn));
	pInst->m_iBStackOut = 0;
}

void CBuilder::AddCondBranch(SRecord & recPred, SBlock * pBlockTrue, SBlock * pBlockFalse)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_CondBranch, 1, recPred, RecUnsigned(0));

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

void CBuilder::AddBranch(SBlock * pBlock)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_Branch, 0, RecUnsigned(0), RecUnsigned(0));

	EWC_ASSERT(m_pBlockCur && !m_pBlockCur->FIsFinalized(), "cannot allocate instructions without a unfinalized basic block");

	s32 * pIInstDst = (s32*)&pInst->m_wordRhs;
	auto pBranch = m_pBlockCur->m_aryBranch.AppendNew();
	pBranch->m_pBlockDest = pBlock;
	pBranch->m_pIInstDst = pIInstDst;

	pInst->m_iBStackOut = 0;
}

SRecord	CBuilder::AllocLocalVar(u32 cB, u32 cBAlign)
{
	u32 iBStackOut = IBStackAlloc(cB, cBAlign);
	return RecUnsigned(iBStackOut);
}

s32	CBuilder::IBStackAlloc(s64 cB, s64 cBAlign)
{
	if (!EWC_FVERIFY(m_pProcCur, "Allocating from the stack without an active procedure"))
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



static inline void LoadWord(CVirtualMachine * pVm, SWord * pWord, u32 iB, u8 cB)
{
	u8 * pB = &pVm->m_pBStack[iB];
	switch (cB)
	{
		case 1:	pWord->m_u8 = *pB;			break;
		case 2:	pWord->m_u16 = *(u16*)pB;	break;
		case 4:	pWord->m_u32 = *(u32*)pB;	break;
		case 8:	pWord->m_u64 = *(u64*)pB;	break;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordLhs)
{
	if ((pInst->m_opkLhs & FOPK_DerefStack) == FOPK_DerefStack)
	{
		u8 cB = pInst->m_cB;
		LoadWord(pVm, pWordLhs, pInst->m_wordLhs.m_s32, cB);
	}
	else
	{
		*pWordLhs = pInst->m_wordLhs;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordLhs, SWord * pWordRhs)
{
	u8 cB = pInst->m_cB;

	if ((pInst->m_opkLhs & FOPK_DerefStack) == FOPK_DerefStack)
	{
		LoadWord(pVm, pWordLhs, pInst->m_wordLhs.m_s32, cB);
	}
	else
	{
		*pWordLhs = pInst->m_wordLhs;
	}

	//if (pInst->m_opkRhs == OPK_Stack || pInst->m_opkRhs == OPK_Argument)
	if ((pInst->m_opkRhs & FOPK_DerefStack) == FOPK_DerefStack)
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

		s64 cBElement;
		s64 cBAlignElement;
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

void ExecuteBytecode(CVirtualMachine * pVm, SProcedure * pProc)
{
	pVm->m_pProcCurDebug = pProc;
	pVm->m_pInst = pProc->m_aryInst.A();

	// build the stack frame for our top level procedure
	u8 * pBStack = pVm->m_pBStackMax;

	pBStack -= sizeof(SInstruction*);
	*(SInstruction **)(pBStack) = nullptr;
	pBStack -= pProc->m_cBStack;

	pVm->m_pBStack = pBStack;

	#define MASHOP(OP, CB)						(u32)(OP | (CB << 16))
	#define FETCH(IB, TYPE)						*(TYPE *)&pVm->m_pBStack[IB]
	#define STORE(IBOUT, TYPE, VALUE)			*(TYPE *)&pVm->m_pBStack[IBOUT] = VALUE

	SWord wordLhs, wordRhs;

	SInstruction * pInstMin = pVm->m_pInst;
	SInstruction * pInst = pInstMin;
	while (1)
	{
		switch (MASHOP(pInst->m_op, pInst->m_cB))
		{
		case MASHOP(OP_NAdd, 1):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 + wordRhs.m_u8);
			}	break;
		case MASHOP(OP_NAdd, 2):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 + wordRhs.m_u16);
			}	break;
		case MASHOP(OP_NAdd, 4):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 + wordRhs.m_u32);
			}	break;
		case MASHOP(OP_NAdd, 8):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 + wordRhs.m_u64);
			}	break;

		case MASHOP(OP_NTrace, 1):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("1byte %d\n", wordLhs.m_u8); break;
		case MASHOP(OP_NTrace, 2):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("2byte %d\n", wordLhs.m_u16); break;
		case MASHOP(OP_NTrace, 4):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("4byte %d\n", wordLhs.m_u32); break;
		case MASHOP(OP_NTrace, 8):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("8byte %lld\n", wordLhs.m_u64); break;

			// Store(pV, value)
		case MASHOP(OP_Store, 1):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_s32, u8, wordRhs.m_u8); break;
		case MASHOP(OP_Store, 2):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_s32, u16, wordRhs.m_u16); break;
		case MASHOP(OP_Store, 4):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(wordLhs.m_s32, u32, wordRhs.m_u32); 
			break;
		case MASHOP(OP_Store, 8):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_s32, u64, wordRhs.m_u64); break;

			// Load(pV) -> iBStackOut
		case MASHOP(OP_Load, 1):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_s32, pInst->m_cB);
			STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8);
		} break;
		case MASHOP(OP_Load, 2):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_s32, pInst->m_cB);
			STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16);
		} break;
		case MASHOP(OP_Load, 4):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_s32, pInst->m_cB);
			STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32);
		} break;
		case MASHOP(OP_Load, 8):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_s32, pInst->m_cB);
			STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64);
		} break;

		case MASHOP(OP_NCmp, 1):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u8, FEvaluateNCmp<1>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_NCmp, 2):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u16, FEvaluateNCmp<2>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_NCmp, 4):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u32, FEvaluateNCmp<4>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_NCmp, 8):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u64, FEvaluateNCmp<8>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(OP_GCmp, 4):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u32, FEvaluateGCmp<4>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_GCmp, 8):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u64, FEvaluateGCmp<8>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(OP_Call, 0):
		{
			ReadOpcodes(pVm, pInst, &wordLhs);

			auto pProc = (SProcedure *)wordLhs.m_pV;

			SInstruction ** ppInstRet = (SInstruction **)(pVm->m_pBStack - sizeof(SInstruction *));
			
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

			//printf("OP_Call) pBStack = %p, ppInst = %p, cBStack = %lld, cBArg = %lld\n", pVm->m_pBStack, ppInstRet, pProc->m_cBStack, pProc->m_cBArg);
			
			*ppInstRet = pInst;

			pInst = pProc->m_aryInst.A() - 1; // this will be incremented below
			pVm->m_pProcCurDebug = pProc;

		} break;

		case MASHOP(OP_Return, 0):
		{
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);


			u8 * pBStackCalled = pVm->m_pBStack;
			pVm->m_pBStack += wordLhs.m_s32;
			SInstruction ** ppInstRet = ((SInstruction **)pVm->m_pBStack) - 1;


			//printf("OP_Ret ) pBStack = %p, ppInst = %p , dB = %d\n", pVm->m_pBStack, ppInstRet, wordLhs.m_s32);
			auto pProcCalled = pVm->m_pProcCurDebug;
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
			EWC_ASSERT(pInstCall->m_op == OP_Call, "procedure return did not return to call instruction");

			auto pProcPrev = (SProcedure *)pInstCall->m_wordRhs.m_pV;
			pVm->m_pProcCurDebug = pProcPrev;
			pInst = pInstCall;
			
		} break;

		case MASHOP(OP_CondBranch, 1):
		{
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			u8 iOp = (wordLhs.m_u8 != 0);
			s32 * pIInst = (s32*)&pInst->m_wordRhs;
			s32 iInst = pIInst[iOp];

			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;
		case MASHOP(OP_Branch, 0):
		{	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			s32 iInst = *(s32*)&pInst->m_wordRhs;

			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;

		default:

			EWC_ASSERT(false, "unhandled opcode OP_%s\n", PChzFromOp(pInst->m_op));
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
{
	m_pBStack -= 4;
	auto piInstTerm = (u32*)m_pBStack;
	*piInstTerm = 0;
}

SRecord RecFloat(f64 g)
{
	SWord word;
	word.m_f64 = g;
	return SRecord(OPK_Literal, word);
}

SRecord RecSigned(s64 nSigned)
{
	SWord word;
	word.m_s64 = nSigned;
	return SRecord(OPK_Literal, word);
}

SRecord RecUnsigned(u64 nUnsigned)
{
	SWord word;
	word.m_u64 = nUnsigned;
	return SRecord(OPK_Literal, word);
}

SRecord RecStack(s64 iBStack)
{
	SWord word;
	word.m_s64 = iBStack;
	return SRecord(OPK_Stack, word);
}

SRecord RecArg(s64 iBStack)
{
	SWord word;
	word.m_s64 = iBStack;
	return SRecord(OPK_ArgStack, word);
}

SRecord RecArgLiteral(s64 iBStack)
{
	SWord word;
	word.m_s64 = iBStack;
	return SRecord(OPK_ArgLiteral, word);
}

SRecord RecPointer(void * pV)
{
	SWord word;
	word.m_pV = pV;
	return SRecord(OPK_Literal, word);
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

	CBuilder bcbuild(pAlloc, &dlay);

	EWC::CHash<HV, STypeInfo *>	hashHvPTin(pAlloc, BK_ByteCodeTest);
	SUniqueNameSet unsetTin(pAlloc, BK_ByteCodeTest);
	auto pSymtab = PSymtabNew(pAlloc, nullptr, "bytecode", &unsetTin, &hashHvPTin);
	pSymtab->AddBuiltInSymbols(pWork);

	auto pTinprocMain = PTinprocAlloc(pSymtab, 0, 0, "main");
	pTinprocMain->m_strMangled = pTinprocMain->m_strName;

	auto pTinprocPrint = PTinprocAlloc(pSymtab, 2, 0, "print");
	pTinprocPrint->m_strMangled = pTinprocPrint->m_strName;
	auto pTinS16 = pSymtab->PTinBuiltin("s16");
	auto pTinS32 = pSymtab->PTinBuiltin("s32");
	pTinprocPrint->m_arypTinParams.Append(pTinS16);
	pTinprocPrint->m_arypTinParams.Append(pTinS32);

	auto pTinprocSum = PTinprocAlloc(pSymtab, 2, 1, "sum");
	pTinprocSum->m_strMangled = pTinprocSum->m_strName;
	pTinprocSum->m_arypTinParams.Append(pTinS32);
	pTinprocSum->m_arypTinParams.Append(pTinS32);
	pTinprocSum->m_arypTinReturns.Append(pTinS32);


	auto pProcPrint = bcbuild.PProcCreate(pTinprocPrint);
	bcbuild.BeginProc(pProcPrint);
	
	auto pBlockPrint = bcbuild.PBlockCreate();
	bcbuild.BeginBlock(pBlockPrint);
	{
		(void)bcbuild.RecAddInst(OP_NTrace, 2, RecArg(pProcPrint->m_aParamArg[0].m_iBStack));
		(void)bcbuild.RecAddInst(OP_NTrace, 4, RecArg(pProcPrint->m_aParamArg[1].m_iBStack));
		bcbuild.AddReturn(nullptr, 0);
	}

	bcbuild.EndBlock(pBlockPrint);
	bcbuild.EndProc(pProcPrint);
	bcbuild.FinalizeProc(pProcPrint);


	auto pProcSum = bcbuild.PProcCreate(pTinprocSum);
	bcbuild.BeginProc(pProcSum);
	
	auto pBlockSum = bcbuild.PBlockCreate();
	bcbuild.BeginBlock(pBlockSum);
	{
		auto recRhs = bcbuild.RecAddInst(
									OP_NAdd,
									4, 
									RecArg(pProcSum->m_aParamArg[0].m_iBStack),
									RecArg(pProcSum->m_aParamArg[1].m_iBStack));

		bcbuild.AddReturn(&recRhs, 1);
		//void) bcbuild.RecAddInst(OP_Store, 4, RecSigned(pProcSum->m_aParamRet[0].m_iBStack), recRhs);
	}

	bcbuild.EndBlock(pBlockSum);
	bcbuild.EndProc(pProcSum);
	bcbuild.FinalizeProc(pProcSum);


	auto pProc = bcbuild.PProcCreate(pTinprocMain);
	bcbuild.BeginProc(pProc);
	
	auto pBlockPre = bcbuild.PBlockCreate();
	bcbuild.BeginBlock(pBlockPre);
	auto recVarAddr = bcbuild.AllocLocalVar(1, 1);
	auto recVarVal = bcbuild.RecAddInst(OP_Store, 1, recVarAddr, RecSigned(55));

	auto recRhs = bcbuild.RecAddInst(OP_NAdd, 1, RecSigned(5), RecSigned(3));
	auto recOut = bcbuild.RecAddInst(OP_NAdd, 1, recRhs, recVarVal);

	auto recCmp = bcbuild.RecAddNCmp(1, NPRED_SGT, recOut, RecSigned(100));
	auto pBlockTrue = bcbuild.PBlockCreate();
	auto pBlockFalse = bcbuild.PBlockCreate();
	auto pBlockPost = bcbuild.PBlockCreate();
	bcbuild.AddCondBranch(recCmp, pBlockTrue, pBlockFalse);
	bcbuild.EndBlock(pBlockPre);

	bcbuild.BeginBlock(pBlockTrue);
	(void) bcbuild.RecAddInst(OP_Store, 1, recVarAddr, RecSigned(11));
	bcbuild.AddBranch(pBlockPost);
	bcbuild.EndBlock(pBlockTrue);

	bcbuild.BeginBlock(pBlockFalse);
	(void) bcbuild.RecAddInst(OP_Store, 1, recVarAddr, RecSigned(22));
	bcbuild.AddBranch(pBlockPost);
	bcbuild.EndBlock(pBlockFalse);

	bcbuild.BeginBlock(pBlockPost);
	recVarVal = bcbuild.RecAddInst(OP_Load, 1, recVarAddr);
	(void) bcbuild.RecAddInst(OP_NTrace, 1, recVarVal);

	SRecord aRecArg[2];
	aRecArg[0] = RecSigned(111);
	aRecArg[1] = RecSigned(222);
	bcbuild.AddCall(pProcPrint, aRecArg, 2);

	(void) bcbuild.RecAddInst(OP_NTrace, 8, RecSigned(-1));

	auto recIntAddr = bcbuild.AllocLocalVar(4, 4);
	auto recIntVal = bcbuild.RecAddInst(OP_Store, 4, recIntAddr, RecSigned(444));
	aRecArg[0] = RecSigned(333);
	aRecArg[1] = recIntVal;
	bcbuild.AddCall(pProcPrint, aRecArg, 2);

	aRecArg[0] = RecSigned(123);
	aRecArg[1] = RecSigned(111);
	auto recSumVal = bcbuild.RecAddCall(pProcSum, aRecArg, 2);
	(void) bcbuild.RecAddInst(OP_NTrace, 4, recSumVal);

	bcbuild.AddReturn(nullptr, 0);
	bcbuild.EndBlock(pBlockPost);
	bcbuild.EndProc(pProc);
	bcbuild.FinalizeProc(pProc);

	static const u32 s_cBStackMax = 2048;
	u8 * pBStack = (u8 *)pAlloc->EWC_ALLOC(s_cBStackMax, 16);

	char aCh[2048];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));

	CVirtualMachine vm(pBStack, &pBStack[s_cBStackMax], &dlay);
	vm.m_pStrbuf = &strbuf;

	ExecuteBytecode(&vm, pProc);

	EWC::EnsureTerminated(&strbuf, '\0');
	printf("%s\n", aCh);
}

} // namespace BCode 

// [x] generate instructions into basic blocks
// [ ] plumb #bctrace through other phases for unit testing
// [ ] add bcode string to unit test system