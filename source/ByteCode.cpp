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

using namespace EWC;

u32 CBFromBcopsz(BCOPSZ bcopsz)
{
	static const u32 s_mpBcopszCB [] =
	{
		1,	// BCOPSZ_8,
		2,	// BCOPSZ_16,
		4,	// BCOPSZ_32,
		8,	// BCOPSZ_64,
	};
	EWC_CASSERT(EWC_DIM(s_mpBcopszCB) == BCOPSZ_Max, "missing size");

	if (!EWC_FVERIFY(bcopsz < BCOPSZ_Max))
		return 1;
	return s_mpBcopszCB[bcopsz];
}

SBCProcedure::SBCProcedure(EWC::CString strName, EWC::CString strMangled)
:m_strName(strName)
,m_strMangled(strMangled)
,m_cBStack(0)
,m_pBlock(nullptr)
{
}



CByteCodeBuilder::CByteCodeBuilder(EWC::CAlloc * pAlloc)
:m_pAlloc(pAlloc)
,m_aryBcval(pAlloc, BK_ByteCodeCreator, 512)
,m_arypProcManaged(pAlloc, BK_ByteCodeCreator, 256)
,m_pProcCur(nullptr)
,m_pBInstMin(nullptr)
,m_pBInstCur(nullptr)
,m_pBInstMax(nullptr)
{
	static const int s_cBInst = 4 * 1024;
	m_pBInstMin = (u8 *)pAlloc->EWC_ALLOC(s_cBInst, 4);
	m_pBInstCur = m_pBInstMin;
	m_pBInstMax = m_pBInstMin + s_cBInst;
}

SBCProcedure * CByteCodeBuilder::PProcCreate(const char * pChzName, const char * pChzMangled)
{
	auto pProc = EWC_NEW(m_pAlloc, SBCProcedure) SBCProcedure(pChzName, pChzMangled);
	m_arypProcManaged.Append(pProc);

	return pProc;
}

void CByteCodeBuilder::BeginProc(SBCProcedure * pProc)
{
	EWC_ASSERT(!m_pProcCur, "cannot begin procedure %s; previous procedure %s was not ended", 
		m_pProcCur->m_strName.PCoz(), pProc->m_strName.PCoz());
	m_pProcCur = pProc;
}

void CByteCodeBuilder::EndProc(SBCProcedure * pProc)
{
	EWC_ASSERT(m_pProcCur == pProc, "mismatch ending procedure %s", pProc->m_strName.PCoz());
	m_pProcCur = nullptr;
}

SBCBlock * CByteCodeBuilder::PBlockBegin()
{
	return nullptr;
}

void CByteCodeBuilder::EndBlock(SBCBlock * pBlock)
{
}

/*
SBCValue CByteCodeBuilder::BcvalAddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs)
{
	SBCValue bcvalRet;
	bcvalRet.m_bcopsz = bcopsz;

	return bcvalRet;
}

SBCValue CByteCodeBuilder::BcvalAddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs, SBCOperand & opRhs)
{
	SBCValue bcvalRet;
	bcvalRet.m_bcopsz = bcopsz;

	return bcvalRet;
}*/


SRecord CByteCodeBuilder::RecAddInst(BCOP bcop, BCOPSZ bcopsz, const SRecord & recLhs)
{
	// setup output value
	/*
	SRecord recOutAddr;
	VID vidOut = VidCoerce(m_aryBcval.C());
	SBCValue * pBcvalOut = m_aryBcval.AppendNew();
	pBcvalOut->m_bcopsz = bcopsz;
	*/

	u32 cB = CBFromBcopsz(bcopsz);
	u32 iBStackOut = IBStackAlloc(cB, cB);

	//auto pInst = (SInstruction *)PBInstAlloc(sizeof(SInstruction), EWC_ALIGN_OF(SInstruction));
	SInstruction inst;
	inst.m_bcop = bcop;
	inst.m_bcopsz = bcopsz;
	inst.m_bcopkLhs = recLhs.m_bcopk;
	inst.m_bcopkRhs = BCOPK_Nil;
	inst.m_bcopkOut = BCOPK_Stack;
	PackInst(&inst, sizeof(SInstruction));

	#define MASHTYPE(BCOPK, BCOPSZ)				(u32)(BCOPK | (BCOPSZ << 16))
	switch (MASHTYPE(recLhs.m_bcopk, bcopsz))
	{
		case MASHTYPE(BCOPK_Literal, BCOPSZ_8):		PackInst(&recLhs.m_u8, 1);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_16):	PackInst(&recLhs.m_u16, 2);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_32):	PackInst(&recLhs.m_u32, 4);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_64):	PackInst(&recLhs.m_u64, 8);	break;

		case MASHTYPE(BCOPK_Stack, BCOPSZ_32):		PackInst(&recLhs.m_u32, 4);	break;
		default:
			EWC_ASSERT(false, "unhandled opcode kind");
	}
	#undef MASHTYPE
	
	PackInst(&iBStackOut, 4);
	return RecStack(iBStackOut);
}

SRecord CByteCodeBuilder::RecAddInst(BCOP bcop, BCOPSZ bcopsz, const SRecord & recLhs, const SRecord & recRhs)
{
	u32 cB = CBFromBcopsz(bcopsz);
	u32 iBStackOut = IBStackAlloc(cB, cB);

	SInstruction inst;
	inst.m_bcop = bcop;
	inst.m_bcopsz = bcopsz;
	inst.m_bcopkLhs = recLhs.m_bcopk;
	inst.m_bcopkRhs = recRhs.m_bcopk;
	inst.m_bcopkOut = BCOPK_Stack;
	PackInst(&inst, sizeof(SInstruction));


	#define MASHTYPE(BCOPK, BCOPSZ)				(u32)(BCOPK | (BCOPSZ << 16))
	switch (MASHTYPE(recLhs.m_bcopk, bcopsz))
	{
		case MASHTYPE(BCOPK_Literal, BCOPSZ_8):		PackInst(&recLhs.m_u8, 1);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_16):	PackInst(&recLhs.m_u16, 2);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_32):	PackInst(&recLhs.m_u32, 4);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_64):	PackInst(&recLhs.m_u64, 8);	break;

		case MASHTYPE(BCOPK_Stack, BCOPSZ_8):
		case MASHTYPE(BCOPK_Stack, BCOPSZ_16):
		case MASHTYPE(BCOPK_Stack, BCOPSZ_32):
		case MASHTYPE(BCOPK_Stack, BCOPSZ_64):		PackInst(&recLhs.m_u32, 4);	break;
		default:
			EWC_ASSERT(false, "unhandled opcode kind");
	}

	switch (MASHTYPE(recRhs.m_bcopk, bcopsz))
	{
		case MASHTYPE(BCOPK_Literal, BCOPSZ_8):		PackInst(&recRhs.m_u8, 1);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_16):	PackInst(&recRhs.m_u16, 2);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_32):	PackInst(&recRhs.m_u32, 4);	break;
		case MASHTYPE(BCOPK_Literal, BCOPSZ_64):	PackInst(&recRhs.m_u64, 8);	break;

		case MASHTYPE(BCOPK_Stack, BCOPSZ_8):
		case MASHTYPE(BCOPK_Stack, BCOPSZ_16):
		case MASHTYPE(BCOPK_Stack, BCOPSZ_32):
		case MASHTYPE(BCOPK_Stack, BCOPSZ_64):		PackInst(&recRhs.m_u32, 4);	break;
		default:
			EWC_ASSERT(false, "unhandled opcode kind");
	}
	#undef MASHTYPE
	
	PackInst(&iBStackOut, 4);
	return RecStack(iBStackOut);
}

u32	CByteCodeBuilder::IBStackAlloc(u32 cB, u32 cBAlign)
{
	if (!EWC_FVERIFY(m_pProcCur, "Allocating from the stack without an active procedure"))
		return 0;
	size_t cBMasked = cBAlign - 1;
	u32 cBStack = U32Coerce((m_pProcCur->m_cBStack + cBMasked) & ~cBMasked);
	m_pProcCur->m_cBStack = cBStack + cB;
	return cBStack;
}

/*
u8 * CByteCodeBuilder::PBStackAlloc(u32 cB, u32 cBAlign)
{
	if (!EWC_FVERIFY(m_pProcCur, "Allocating from the stack without an active procedure"))
		return 0;
	size_t cBMasked = cBAlign - 1;
	u32 cBStack = U32Coerce((m_pProcCur->m_cBStack + cBMasked) & ~cBMasked);
	m_pProcCur->m_cBStack = cBStack + cB;
	return cBStack;
}*/

/*
u8 * CByteCodeBuilder::PBInstAlloc(u32 cBAlloc, u32 cBAlign)
{
	//auto pBAlign = (u8 *)EWC::PVAlign(m_pBInstCur, EWC_ALIGN_OF(SInstruction));
	auto pBAlign = (u8 *)EWC::PVAlign(m_pBInstCur, cBAlign);
	ptrdiff_t cBAlign = pBAlign - m_pBInstCur;

	u32 cB = U32Coerce(cB + cBAlign);
	ptrdiff_t cBRemain = m_pBInstMax - m_pBInstCur;
	if (cBRemain < cB)
		return nullptr;

	auto pB = (u8 *)(m_pBInstCur + cBAlign);
	m_pBInstCur += cB;
	return pB;
}*/

void CByteCodeBuilder::PackInst(const void * pV, u32 cB)
{
	ptrdiff_t cBRemain = m_pBInstMax - m_pBInstCur;
	if (cBRemain < cB)
		return;

	EWC::CopyAB(pV, m_pBInstCur, cB);
	m_pBInstCur += cB;
}
void UnpackInst(u8 ** ppBInst, void * pVOut, u32 cB)
{
	EWC::CopyAB(*ppBInst, pVOut, cB);
	*ppBInst += cB;
}


template <typename T>
static inline T StackFetch(CVirtualMachine * pVm)
{
	auto ibStack = *(u32 *)pVm->m_pBInst;
	pVm->m_pBInst += 4;
	return *(T *)&pVm->m_pBStack[ibStack];
}

static inline void ReadOpcodes(CVirtualMachine * pVm, u32 * piLhs, u32 * piOut)
{
	u32 * pI = (u32*)(pVm->m_pBInst + sizeof(SInstruction));
	*piLhs = *pI++;
	*piOut = *pI++;
	pVm->m_pBInst = (u8 *)pI;
}

static inline void ReadOpcodes(CVirtualMachine * pVm, u32 * piLhs, u32 * piRhs, u32 * piOut)
{
	u32 * pI = (u32*)(pVm->m_pBInst + sizeof(SInstruction));
	*piLhs = *pI++;
	*piRhs = *pI++;
	*piOut = *pI++;
	pVm->m_pBInst = (u8 *)pI;
}

static inline void LoadRecord(CVirtualMachine * pVm, SRecord * pRec, u32 iB, BCOPSZ bcopsz)
{
	u8 * pB = &pVm->m_pBStack[iB];
	switch (bcopsz)
	{
		case BCOPSZ_8:	pRec->m_u8 = *pB;
		case BCOPSZ_16:	pRec->m_u16 = *(u16*)pB;
		case BCOPSZ_32: pRec->m_u32 = *(u32*)pB;
		case BCOPSZ_64: pRec->m_u64 = *(u64*)pB;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SRecord * pRecLhs, u32 * piOut)
{
	BCOPSZ bcopsz = (BCOPSZ)pInst->m_bcopsz;
	u32 cB = CBFromBcopsz(bcopsz);
	UnpackInst(&pVm->m_pBInst, &pRecLhs->m_u64, cB);
	if (pInst->m_bcopkLhs == BCOPK_Stack)
	{
		u32 iBStack;
		UnpackInst(&pVm->m_pBInst, &iBStack, sizeof(iBStack));
		LoadRecord(pVm, pRecLhs, iBStack, bcopsz);
	}
	else
	{
		UnpackInst(&pVm->m_pBInst, &pRecLhs->m_u64, cB);
	}

	UnpackInst(&pVm->m_pBInst, piOut, 4);
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SRecord * pRecLhs, SRecord * pRecRhs, u32 * piOut)
{
	BCOPSZ bcopsz = (BCOPSZ)pInst->m_bcopsz;
	u32 cB = CBFromBcopsz(bcopsz);
	if (pInst->m_bcopkLhs == BCOPK_Stack)
	{
		u32 iBStack;
		UnpackInst(&pVm->m_pBInst, &iBStack, sizeof(iBStack));
		LoadRecord(pVm, pRecLhs, iBStack, bcopsz);
	}
	else
	{
		UnpackInst(&pVm->m_pBInst, &pRecLhs->m_u64, cB);
	}


	if (pInst->m_bcopkRhs == BCOPK_Stack)
	{
		u32 iBStack;
		UnpackInst(&pVm->m_pBInst, &iBStack, sizeof(iBStack));
		LoadRecord(pVm, pRecRhs, iBStack, bcopsz);
	}
	else
	{
		UnpackInst(&pVm->m_pBInst, &pRecRhs->m_u64, cB);
	}

	UnpackInst(&pVm->m_pBInst, piOut, 4);
}

template <typename T>
static inline void StackStore(CVirtualMachine * pVm, T val)
{
	auto ibStack = *(u32 *)pVm->m_pBInst;
	pVm->m_pBInst += 4;
	*(T *)&pVm->m_pBStack[ibStack] = val;
}

void ExecuteBytecode(CVirtualMachine * pVm)
{
	#define MASHOP(BCOP, BCOPSZ)				(u32)(BCOP | (BCOPSZ << 16))


	// BB - Ignoring BCOPK - everthing is on the stack
#if 0
	#define FETCH(NAME, TYPE)					auto ibStack##NAME = *(u32 *)pVm->m_pBInst; \
												pVm->m_pBInst += 4; \
												auto NAME = *(TYPE *)&pVm->m_pBStack[ibStack##NAME]

	#define STORE(VALUE, DEST, TYPE)			auto ibStack##NAME = *(u32 *)pVm->m_pBInst; \
												pVm->m_pBInst += 4; \
												*(TYPE *)&pVm->m_pBStack[ibStack##DEST] = VALUE

	while (1)
	{
		SInstruction * pInst = (SInstruction *)pVm->m_pBInst;
		//pVm->m_pBInst += pBInst->m_dB;

		switch (MASHOP(pInst->m_bcop, pInst->m_bcopsz))
		{
		case MASHOP(BCOP_NAdd, BCOPSZ_8):	{ FETCH(Lhs, u8); FETCH(Rhs, u8);	STORE(Rhs + Lhs, Out, u8); } break;
		case MASHOP(BCOP_NAdd, BCOPSZ_16):	{ FETCH(Lhs, u16); FETCH(Rhs, u16); STORE(Rhs + Lhs, Out, u16); } break;
		case MASHOP(BCOP_NAdd, BCOPSZ_32):	{ FETCH(Lhs, u32); FETCH(Rhs, u32); STORE(Rhs + Lhs, Out, u32); } break;
		case MASHOP(BCOP_NAdd, BCOPSZ_64):	{ FETCH(Lhs, u64); FETCH(Rhs, u64); STORE(Rhs + Lhs, Out, u64); } break;

		case MASHOP(BCOP_GAdd, BCOPSZ_32):	{ FETCH(Lhs, f32); FETCH(Rhs, f32); STORE(Rhs + Lhs, Out, f32); } break;
		case MASHOP(BCOP_GAdd, BCOPSZ_64):	{ FETCH(Lhs, f64); FETCH(Rhs, f64); STORE(Rhs + Lhs, Out, f64); } break;

		case MASHOP(BCOP_NSub, BCOPSZ_8):	{ FETCH(Lhs, u8); FETCH(Rhs, u8);	STORE(Rhs - Lhs, Out, u8); } break;
		case MASHOP(BCOP_NSub, BCOPSZ_16):	{ FETCH(Lhs, u16); FETCH(Rhs, u16); STORE(Rhs - Lhs, Out, u16); } break;
		case MASHOP(BCOP_NSub, BCOPSZ_32):	{ FETCH(Lhs, u32); FETCH(Rhs, u32); STORE(Rhs - Lhs, Out, u32); } break;
		case MASHOP(BCOP_NSub, BCOPSZ_64):	{ FETCH(Lhs, u64); FETCH(Rhs, u64); STORE(Rhs - Lhs, Out, u64); } break;

		case MASHOP(BCOP_GSub, BCOPSZ_32):	{ FETCH(Lhs, f32); FETCH(Rhs, f32); STORE(Rhs - Lhs, Out, f32); } break;
		case MASHOP(BCOP_GSub, BCOPSZ_64):	{ FETCH(Lhs, f64); FETCH(Rhs, f64); STORE(Rhs - Lhs, Out, f64); } break;
			break;
		}
	}
#endif
	#define FETCH(IB, TYPE)						*(TYPE *)&pVm->m_pBStack[IB]

	#define STORE(IBOUT, TYPE, VALUE)			*(TYPE *)&pVm->m_pBStack[IBOUT] = VALUE



	u32 iBStackOut;	
	SRecord recLhs, recRhs;

	while (1)
	{
		//SInstruction * pInst = (SInstruction *)pVm->m_pBInst;
		SInstruction inst;
		UnpackInst(&pVm->m_pBInst, &inst, sizeof(SInstruction));

		//pVm->m_pBInst += pBInst->m_dB;

		switch (MASHOP(inst.m_bcop, inst.m_bcopsz))
		{
		case MASHOP(BCOP_NAdd, BCOPSZ_8):	
			{ 
				//ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u8, FETCH(iBLhs, u8) + FETCH(iBRhs, u8));
				ReadOpcodes(pVm, &inst, &recLhs, &recRhs, &iBStackOut);
				STORE(iBStackOut, u8, recLhs.m_u8 + recRhs.m_u8);
			}	break;
		case MASHOP(BCOP_NAdd, BCOPSZ_16):	
			//{ ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u16, FETCH(iBLhs, u16) + FETCH(iBRhs, u16)); }	break;
			{ 
				//ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u8, FETCH(iBLhs, u8) + FETCH(iBRhs, u8));
				ReadOpcodes(pVm, &inst, &recLhs, &recRhs, &iBStackOut);
				STORE(iBStackOut, u16, recLhs.m_u16 + recRhs.m_u16);
			}	break;
		case MASHOP(BCOP_NAdd, BCOPSZ_32):	
			//{ ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u32, FETCH(iBLhs, u32) + FETCH(iBRhs, u32)); }	break;
			{ 
				//ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u8, FETCH(iBLhs, u8) + FETCH(iBRhs, u8));
				ReadOpcodes(pVm, &inst, &recLhs, &recRhs, &iBStackOut);
				STORE(iBStackOut, u32, recLhs.m_u32 + recRhs.m_u32);
			}	break;
		case MASHOP(BCOP_NAdd, BCOPSZ_64):	
			//{ ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u64, FETCH(iBLhs, u64) + FETCH(iBRhs, u64)); }	break;
			{ 
				//ReadOpcodes(pVm, &iBLhs, &iBRhs, &iBOut); STORE(iBOut, u8, FETCH(iBLhs, u8) + FETCH(iBRhs, u8));
				ReadOpcodes(pVm, &inst, &recLhs, &recRhs, &iBStackOut);
				STORE(iBStackOut, u64, recLhs.m_u64 + recRhs.m_u64);
			}	break;

			break;
		}

#if 0
		switch (pInst->m_bcop)
		{
		case BCOP_NAdd:
		case BCOP_GSub:
		case BCOP_NSub:
		case BCOP_GSub:
		case BCOP_NMul:
		case BCOP_GMul:
		case BCOP_SDiv:
		case BCOP_UDiv:
		case BCOP_GDiv:
		case BCOP_SRem:
		case BCOP_URem:
		case BCOP_GRem:
		}
#endif
	}

	#undef MASHOP
	#undef FETCH
	#undef MASHOP
}

CVirtualMachine::CVirtualMachine(u8 * pBInst, u8 * pBStack)
:m_pBInstStart(pBInst)
,m_pBInst(pBInst)
,m_pBStack(pBStack)
{
}

SRecord RecFloat(f64 g)
{
	SRecord rec;
	rec.m_bcopk	= BCOPK_Literal;
	rec.m_bcoptype = BCOPTYPE_Float;
	rec.m_f64 = g;
	return rec;
}

SRecord RecSigned(s64 nSigned)
{
	SRecord rec;
	rec.m_bcopk	= BCOPK_Literal;
	rec.m_bcoptype = BCOPTYPE_Signed;
	rec.m_s64 = nSigned;
	return rec;
}

SRecord RecUnsigned(u64 nUnsigned)
{
	SRecord rec;
	rec.m_bcopk	= BCOPK_Literal;
	rec.m_bcoptype = BCOPTYPE_Unsigned;
	rec.m_u64 = nUnsigned;
	return rec;
}

SRecord RecStack(u32 iBStack)
{
	SRecord rec;
	rec.m_bcopk	= BCOPK_Stack;
	rec.m_bcoptype = BCOPTYPE_Unsigned;
	rec.m_u32 = iBStack;
	return rec;
}

/*
SRecord RecVid(VID vid)
{
	SRecord rec;
	rec.m_bcopk	= BCOPK_Stack;
	rec.m_bcoptype = BCOPTYPE_Vid;
	rec.m_nSigned = vid;
	return rec;
}*/

void BuildTestByteCode(EWC::CAlloc * pAlloc)
{
	CByteCodeBuilder bcbuild(pAlloc);

	auto pProc = bcbuild.PProcCreate("main", "main");
	
	bcbuild.BeginProc(pProc);
	auto pBlock = bcbuild.PBlockBegin();
	auto recRhs = bcbuild.RecAddInst(BCOP_NAdd, BCOPSZ_8, RecSigned(5), RecSigned(3));
	(void) bcbuild.RecAddInst(BCOP_NAdd, BCOPSZ_8, recRhs, RecSigned(12));
	bcbuild.EndBlock(pBlock);
	bcbuild.EndProc(pProc);

	u32 cBStackMax = 0;
	for (auto ppProc = bcbuild.m_arypProcManaged.A(); ppProc != bcbuild.m_arypProcManaged.PMac(); ++ppProc)
	{
		cBStackMax = ewcMax(cBStackMax, (*ppProc)->m_cBStack);
	}

	u8 * pBStack = (u8 *)pAlloc->EWC_ALLOC(cBStackMax, 16);
	CVirtualMachine vm(bcbuild.m_pBInstMin, pBStack);

	ExecuteBytecode(&vm);
}
