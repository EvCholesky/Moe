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
#include "stdio.h"

using namespace EWC;

namespace BCode 
{



u32 CBFromOpsz(OPSZ opsz)
{
	static const u32 s_mpOpszCB [] =
	{
		1,	// OPSZ_8,
		2,	// OPSZ_16,
		4,	// OPSZ_32,
		8,	// OPSZ_64,
	};
	EWC_CASSERT(EWC_DIM(s_mpOpszCB) == OPSZ_Max, "missing size");

	if (!EWC_FVERIFY(opsz < OPSZ_Max))
		return 1;
	return s_mpOpszCB[opsz];
}

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

SProcedure::SProcedure(EWC::CAlloc * pAlloc, EWC::CString strName, EWC::CString strMangled)
:m_strName(strName)
,m_strMangled(strMangled)
,m_cBStack(0)
,m_pBlockEntry(nullptr)
,m_arypBlock(pAlloc, BK_ByteCodeCreator, 16)
,m_aryInst(pAlloc, BK_ByteCode, 0)
{
}



CBuilder::CBuilder(EWC::CAlloc * pAlloc)
:m_pAlloc(pAlloc)
,m_arypProcManaged(pAlloc, BK_ByteCodeCreator, 256)
,m_arypBlockManaged(pAlloc, BK_ByteCodeCreator, 256)
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
{
}

SProcedure * CBuilder::PProcCreate(const char * pChzName, const char * pChzMangled)
{
	auto pProc = EWC_NEW(m_pAlloc, SProcedure) SProcedure(m_pAlloc, pChzName, pChzMangled);
	m_arypProcManaged.Append(pProc);

	return pProc;
}

void CBuilder::BeginProc(SProcedure * pProc)
{
	EWC_ASSERT(!m_pProcCur, "cannot begin procedure %s; previous procedure %s was not ended", 
		m_pProcCur->m_strName.PCoz(), pProc->m_strName.PCoz());
	m_pProcCur = pProc;
}

void CBuilder::EndProc(SProcedure * pProc)
{
	EWC_ASSERT(m_pProcCur == pProc, "mismatch ending procedure %s", pProc->m_strName.PCoz());
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

		pProc->m_aryInst.Append(pBlock->m_aryInst.A(), pBlock->m_aryInst.C());
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

SRecord CBuilder::RecAddInst(OP op, OPSZ opsz, const SRecord & recLhs)
{
	auto oparg = OpargFromOp(op);
	EWC_ASSERT(oparg == OPARG_Unary, "unexpected operator '%s' in unary add inst", PChzFromOp(op));

	u32 cB = CBFromOpsz(opsz);

	auto pInst = PInstAlloc();
	pInst->m_op = op;
	pInst->m_opsz = opsz;
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
	OPSZ opsz,
	const SRecord & recLhs,
	const SRecord & recRhs)
{
	pInst->m_op = op;
	pInst->m_opsz = opsz;
	pInst->m_opkLhs = recLhs.m_opk;
	pInst->m_opkRhs = recRhs.m_opk;

	pInst->m_wordLhs.m_u64 = recLhs.m_word.m_u64;
	pInst->m_wordRhs.m_u64 = recRhs.m_word.m_u64;
}

SRecord CBuilder::RecAddInst(OP op, OPSZ opsz, const SRecord & recLhs, const SRecord & recRhs)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, op, opsz, recLhs, recRhs);

	auto oparg = OpargFromOp(op);
	EWC_ASSERT(oparg == OPARG_Binary || oparg == OPARG_Store, "unexpected operator '%s' in binary add inst", PChzFromOp(op));

	u32 iBStackOut;
	if (oparg == OPARG_Store)
	{
		iBStackOut = recLhs.m_word.m_u32;
	}
	else
	{
		u32 cB = CBFromOpsz(opsz);
		iBStackOut = IBStackAlloc(cB, cB);
	}

	pInst->m_iBStackOut = iBStackOut;
	return RecStack(iBStackOut);
}

SRecord	CBuilder::RecAddNCmp(OPSZ opsz, NPRED npred, const SRecord & recLhs, const SRecord & recRhs)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_NCmp, opsz, recLhs, recRhs);

	pInst->m_pred = (u8)npred;

	u32 iBStackOut = IBStackAlloc(1, 1);
	pInst->m_iBStackOut = iBStackOut;
	return RecStack(iBStackOut);
}

SRecord	CBuilder::RecAddGCmp(OPSZ opsz, GPRED gpred, const SRecord & recLhs, const SRecord & recRhs)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_GCmp, opsz, recLhs, recRhs);

	pInst->m_pred = (u8)gpred;

	u32 iBStackOut = IBStackAlloc(1, 1);
	pInst->m_iBStackOut = iBStackOut;
	return RecStack(iBStackOut);
}

void CBuilder::RecAddCondBranch(SRecord & recPred, SBlock * pBlockTrue, SBlock * pBlockFalse)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_CondBranch, OPSZ_8, recPred, RecUnsigned(0));

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

void CBuilder::RecAddBranch(SBlock * pBlock)
{
	auto pInst = PInstAlloc(); 
	RecSetupInst(pInst, OP_Branch, OPSZ_8, RecUnsigned(0), RecUnsigned(0));

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

u32	CBuilder::IBStackAlloc(u32 cB, u32 cBAlign)
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



template <typename T>
static inline T StackFetch(CVirtualMachine * pVm)
{
	auto ibStack = *(u32 *)pVm->m_pBInst;
	pVm->m_pBInst += 4;
	return *(T *)&pVm->m_pBStack[ibStack];
}

static inline void LoadWord(CVirtualMachine * pVm, SWord * pWord, u32 iB, OPSZ opsz)
{
	u8 * pB = &pVm->m_pBStack[iB];
	switch (opsz)
	{
		case OPSZ_8:	pWord->m_u8 = *pB;			break;
		case OPSZ_16:	pWord->m_u16 = *(u16*)pB;	break;
		case OPSZ_32:	pWord->m_u32 = *(u32*)pB;	break;
		case OPSZ_64:	pWord->m_u64 = *(u64*)pB;	break;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordLhs)
{
	if (pInst->m_opkLhs == OPK_Stack)
	{
		OPSZ opsz = (OPSZ)pInst->m_opsz;
		u32 cB = CBFromOpsz(opsz);
		LoadWord(pVm, pWordLhs, pInst->m_wordLhs.m_u32, opsz);
	}
	else
	{
		*pWordLhs = pInst->m_wordLhs;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordLhs, SWord * pWordRhs)
{
	OPSZ opsz = (OPSZ)pInst->m_opsz;
	u32 cB = CBFromOpsz(opsz);

	if (pInst->m_opkLhs == OPK_Stack)
	{
		LoadWord(pVm, pWordLhs, pInst->m_wordLhs.m_u32, opsz);
	}
	else
	{
		*pWordLhs = pInst->m_wordLhs;
	}

	if (pInst->m_opkRhs == OPK_Stack)
	{
		LoadWord(pVm, pWordRhs, pInst->m_wordRhs.m_u32, opsz);
	}
	else
	{
		*pWordRhs = pInst->m_wordRhs;
	}
}


template <typename T>
static inline void StackStore(CVirtualMachine * pVm, T val)
{
	auto ibStack = *(u32 *)pVm->m_pBInst;
	pVm->m_pBInst += 4;
	*(T *)&pVm->m_pBStack[ibStack] = val;
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
template <OPSZ SZ> struct SWordOpsize			{ };
template <> struct SWordOpsize<OPSZ_8>			
	{ 
		static s8 Signed(SWord & word)		{ return word.m_s8; }
		static u8 Unsigned(SWord & word)	{ return word.m_u8; }
	};
template <> struct SWordOpsize<OPSZ_16>			
	{ 
		static s16 Signed(SWord & word)	{ return word.m_s16; }
		static u16 Unsigned(SWord & word)	{ return word.m_u16; }
	};
template <> struct SWordOpsize<OPSZ_32>
	{ 
		static s32 Signed(SWord & word)	{ return word.m_s32; }
		static u32 Unsigned(SWord & word)	{ return word.m_u32; }
		static f32 Float(SWord & word)		{ return word.m_f32; }
	};
template <> struct SWordOpsize<OPSZ_64>
	{ 
		static s64 Signed(SWord & word)	{ return word.m_s64; }
		static u64 Unsigned(SWord & word)	{ return word.m_u64; }
		static f64 Float(SWord & word)		{ return word.m_f64; }
	};

template <OPSZ SZ>
bool FEvaluateNCmp(NPRED npred, SWord & wordLhs, SWord & wordRhs)
{
	switch (npred)
	{
	case NPRED_EQ:	return SWordOpsize<SZ>::Unsigned(wordLhs) == SWordOpsize<SZ>::Unsigned(wordRhs);
	case NPRED_NE:	return SWordOpsize<SZ>::Unsigned(wordLhs) != SWordOpsize<SZ>::Unsigned(wordRhs);
	case NPRED_SGT:	return SWordOpsize<SZ>::Signed(wordLhs) > SWordOpsize<SZ>::Signed(wordRhs);
	case NPRED_UGT:	return SWordOpsize<SZ>::Unsigned(wordLhs) > SWordOpsize<SZ>::Unsigned(wordRhs);
	case NPRED_SGE:	return SWordOpsize<SZ>::Signed(wordLhs) >= SWordOpsize<SZ>::Signed(wordRhs);
	case NPRED_UGE:	return SWordOpsize<SZ>::Unsigned(wordLhs) >= SWordOpsize<SZ>::Unsigned(wordRhs);
	case NPRED_SLT:	return SWordOpsize<SZ>::Signed(wordLhs) < SWordOpsize<SZ>::Signed(wordRhs);
	case NPRED_ULT:	return SWordOpsize<SZ>::Unsigned(wordLhs) < SWordOpsize<SZ>::Unsigned(wordRhs);
	case NPRED_SLE:	return SWordOpsize<SZ>::Signed(wordLhs) <= SWordOpsize<SZ>::Signed(wordRhs);
	case NPRED_ULE:	return SWordOpsize<SZ>::Unsigned(wordLhs) <= SWordOpsize<SZ>::Unsigned(wordRhs);
	}

	EWC_ASSERT(false, "unhandled predicate type");
	return false;
}

template <OPSZ SZ>
bool FEvaluateGCmp(GPRED gpred, SWord & wordLhs, SWord & wordRhs)
{
	switch (gpred)
	{
	case GPRED_EQ:	return SWordOpsize<SZ>::Float(wordLhs) == SWordOpsize<SZ>::Float(wordRhs);
	case GPRED_NE:	return SWordOpsize<SZ>::Float(wordLhs) != SWordOpsize<SZ>::Float(wordRhs);
	case GPRED_GT:	return SWordOpsize<SZ>::Float(wordLhs) > SWordOpsize<SZ>::Float(wordRhs);
	case GPRED_GE:	return SWordOpsize<SZ>::Float(wordLhs) >= SWordOpsize<SZ>::Float(wordRhs);
	case GPRED_LT:	return SWordOpsize<SZ>::Float(wordLhs) < SWordOpsize<SZ>::Float(wordRhs);
	case GPRED_LE:	return SWordOpsize<SZ>::Float(wordLhs) <= SWordOpsize<SZ>::Float(wordRhs);
	}

	EWC_ASSERT(false, "unhandled predicate type");
	return false;
}

void ExecuteBytecode(CVirtualMachine * pVm)
{
	#define MASHOP(OP, OPSZ)					(u32)(OP | (OPSZ << 16))
	#define FETCH(IB, TYPE)						*(TYPE *)&pVm->m_pBStack[IB]
	#define STORE(IBOUT, TYPE, VALUE)			*(TYPE *)&pVm->m_pBStack[IBOUT] = VALUE

	SWord wordLhs, wordRhs;

	SInstruction * pInstMin = (SInstruction *)pVm->m_pBInst;
	SInstruction * pInst = pInstMin;
	while (1)
	{
		if (pInst->m_op == OP_Halt)
			return;

		switch (MASHOP(pInst->m_op, pInst->m_opsz))
		{
		case MASHOP(OP_NAdd, OPSZ_8):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 + wordRhs.m_u8);
			}	break;
		case MASHOP(OP_NAdd, OPSZ_16):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 + wordRhs.m_u16);
			}	break;
		case MASHOP(OP_NAdd, OPSZ_32):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 + wordRhs.m_u32);
			}	break;
		case MASHOP(OP_NAdd, OPSZ_64):	
			{ 
				ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
				STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 + wordRhs.m_u64);
			}	break;

		case MASHOP(OP_NTrace, OPSZ_8):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("1byte %d\n", wordLhs.m_u8); break;
		case MASHOP(OP_NTrace, OPSZ_16):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("2byte %d\n", wordLhs.m_u16); break;
		case MASHOP(OP_NTrace, OPSZ_32):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("4byte %d\n", wordLhs.m_u32); break;
		case MASHOP(OP_NTrace, OPSZ_64):	
			ReadOpcodes(pVm, pInst, &wordLhs); printf("8byte %lld\n", wordLhs.m_u64); break;

			// Store(pV, value)
		case MASHOP(OP_Store, OPSZ_8):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_u32, u8, wordRhs.m_u8); break;
		case MASHOP(OP_Store, OPSZ_16):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_u32, u16, wordRhs.m_u16); break;
		case MASHOP(OP_Store, OPSZ_32):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_u32, u32, wordRhs.m_u32); break;
		case MASHOP(OP_Store, OPSZ_64):	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); STORE(wordLhs.m_u32, u64, wordRhs.m_u64); break;

			// Load(pV) -> iBStackOut
		case MASHOP(OP_Load, OPSZ_8):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_u32, (OPSZ)pInst->m_opsz);
			STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8);
		} break;
		case MASHOP(OP_Load, OPSZ_16):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_u32, (OPSZ)pInst->m_opsz);
			STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16);
		} break;
		case MASHOP(OP_Load, OPSZ_32):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_u32, (OPSZ)pInst->m_opsz);
			STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32);
		} break;
		case MASHOP(OP_Load, OPSZ_64):	
		{
			ReadOpcodes(pVm, pInst, &wordLhs); 
			LoadWord(pVm, &wordLhs, wordLhs.m_u32, (OPSZ)pInst->m_opsz);
			STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64);
		} break;

		case MASHOP(OP_NCmp, OPSZ_8):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u8, FEvaluateNCmp<OPSZ_8>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_NCmp, OPSZ_16):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u16, FEvaluateNCmp<OPSZ_16>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_NCmp, OPSZ_32):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u32, FEvaluateNCmp<OPSZ_32>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_NCmp, OPSZ_64):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u64, FEvaluateNCmp<OPSZ_64>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(OP_GCmp, OPSZ_32):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u32, FEvaluateGCmp<OPSZ_32>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(OP_GCmp, OPSZ_64):
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs); 
			STORE(pInst->m_iBStackOut, u64, FEvaluateGCmp<OPSZ_64>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(OP_CondBranch, OPSZ_8):
		{
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			u8 iOp = (wordLhs.m_u8 != 0);
			s32 * pIInst = (s32*)&pInst->m_wordRhs;
			s32 iInst = pIInst[iOp];
			if (!EWC_FVERIFY(iInst >= 0 && iInst < pVm->m_cInst, "bad branch index"))
				return;

			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;
		case MASHOP(OP_Branch, OPSZ_8):
		{	
			ReadOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			s32 iInst = *(s32*)&pInst->m_wordRhs;
			if (!EWC_FVERIFY(iInst >= 0 && iInst < pVm->m_cInst, "bad branch index"))
				return;

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

CVirtualMachine::CVirtualMachine(u8 * pBInst, s32 cInst, u8 * pBStack)
:m_pBInst(pBInst)
,m_cInst(cInst)
,m_pBStack(pBStack)
{
}

SRecord RecFloat(f64 g)
{
	SRecord rec;
	rec.m_opk	= OPK_Literal;
	rec.m_word.m_f64 = g;
	return rec;
}

SRecord RecSigned(s64 nSigned)
{
	SRecord rec;
	rec.m_opk	= OPK_Literal;
	rec.m_word.m_s64 = nSigned;
	return rec;
}

SRecord RecUnsigned(u64 nUnsigned)
{
	SRecord rec;
	rec.m_opk	= OPK_Literal;
	rec.m_word.m_u64 = nUnsigned;
	return rec;
}

SRecord RecStack(u32 iBStack)
{
	SRecord rec;
	rec.m_opk	= OPK_Stack;
	rec.m_word.m_u32 = iBStack;
	return rec;
}

void BuildTestByteCode(EWC::CAlloc * pAlloc)
{
	CBuilder bcbuild(pAlloc);

	auto pProc = bcbuild.PProcCreate("main", "main");
	bcbuild.BeginProc(pProc);
	
	auto pBlockPre = bcbuild.PBlockCreate();
	bcbuild.BeginBlock(pBlockPre);
	auto recVarAddr = bcbuild.AllocLocalVar(1, 1);
	auto recVarVal = bcbuild.RecAddInst(OP_Store, OPSZ_8, recVarAddr, RecSigned(55));

//	(void) bcbuild.RecAddInst(OP_NTrace, OPSZ_8, recVarVal);

	auto recRhs = bcbuild.RecAddInst(OP_NAdd, OPSZ_8, RecSigned(5), RecSigned(3));
	auto recOut = bcbuild.RecAddInst(OP_NAdd, OPSZ_8, recRhs, recVarVal);
//	(void) bcbuild.RecAddInst(OP_NTrace, OPSZ_8, recOut);

	auto recCmp = bcbuild.RecAddNCmp(OPSZ_8, NPRED_SGT, recOut, RecSigned(100));
	auto pBlockTrue = bcbuild.PBlockCreate();
	auto pBlockFalse = bcbuild.PBlockCreate();
	auto pBlockPost = bcbuild.PBlockCreate();
	bcbuild.RecAddCondBranch(recCmp, pBlockTrue, pBlockFalse);
	bcbuild.EndBlock(pBlockPre);

	bcbuild.BeginBlock(pBlockTrue);
	(void) bcbuild.RecAddInst(OP_Store, OPSZ_8, recVarAddr, RecSigned(11));
	bcbuild.RecAddBranch(pBlockPost);
	bcbuild.EndBlock(pBlockTrue);

	bcbuild.BeginBlock(pBlockFalse);
	(void) bcbuild.RecAddInst(OP_Store, OPSZ_8, recVarAddr, RecSigned(22));
	bcbuild.RecAddBranch(pBlockPost);
	bcbuild.EndBlock(pBlockFalse);

	bcbuild.BeginBlock(pBlockPost);
	recVarVal = bcbuild.RecAddInst(OP_Load, OPSZ_8, recVarAddr);
	(void) bcbuild.RecAddInst(OP_NTrace, OPSZ_8, recVarVal);

	(void) bcbuild.RecAddInst(OP_Halt, OPSZ_8, RecUnsigned(0));
	bcbuild.EndBlock(pBlockPost);
	bcbuild.EndProc(pProc);
	bcbuild.FinalizeProc(pProc);

	u32 cBStackMax = 0;
	for (auto ppProc = bcbuild.m_arypProcManaged.A(); ppProc != bcbuild.m_arypProcManaged.PMac(); ++ppProc)
	{
		cBStackMax = ewcMax(cBStackMax, (*ppProc)->m_cBStack);
	}

	u8 * pBStack = (u8 *)pAlloc->EWC_ALLOC(cBStackMax, 16);
	CVirtualMachine vm((u8*)pProc->m_aryInst.A(), S32Coerce(pProc->m_aryInst.C()), pBStack);

	ExecuteBytecode(&vm);
}

} // namespace BCode 

// [x] generate instructions into basic blocks
// [ ] plumb #bctrace through other phases for unit testing
// [ ] add bcode string to unit test system