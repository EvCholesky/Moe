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



#define OPLIT(A)	SBCOperand(BCOPK_Literal, A)


SBCProcedure * CByteCodeBuilder::PProcCreate(const char * pChzName, const char * pChzMangled)
{
	return nullptr;
}

SBCBlock * CByteCodeBuilder::PBlockBegin()
{
	return nullptr;
}

void CByteCodeBuilder::EndBlock(SBCBlock * pBlock)
{
}

void CByteCodeBuilder::AddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs)
{
}

void CByteCodeBuilder::AddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs, SBCOperand & opRhs)
{
}

void ExecuteBytecode(CVirtualMachine * pVm)
{
	#define MASHOP(BCOP, BCOPSZ)				(u32)(BCOP | (BCOPSZ << 16))


	#define FETCH(NAME, TYPE)					auto ibStack##NAME = *(u32 *)pVm->m_pBInst; \
												pVm->m_pBInst += 4; \
												auto NAME = *(TYPE *)&pVm->m_pBStack[ibStack##NAME]

	#define STORE(VALUE, DEST, TYPE)			*(TYPE *)&pVm->m_pBStack[ibStack##DEST] = VALUE


	static int kDBStack = 4;

	while (1)
	{
		SInstruction * pInst = (SInstruction *)pVm->m_pBInst;
		//pVm->m_pBInst += pBInst->m_dB;

		switch (MASHOP(pInst->m_bcop, pInst->m_bcopsz))
		{
		case MASHOP(BCOP_NAdd, BCOPSZ_8):
			{
				FETCH(Src, u8);
				FETCH(Dst, u8);
				STORE(Dst + Src, Dst, u8);
			} break;
		case MASHOP(BCOP_NAdd, BCOPSZ_16):
			{
				FETCH(Src, u16);
				FETCH(Dst, u16);
				STORE(Dst + Src, Dst, u16);
			} break;
		case MASHOP(BCOP_NAdd, BCOPSZ_32):
			{
				FETCH(Src, u32);
				FETCH(Dst, u32);
				STORE(Dst + Src, Dst, u32);
			} break;
		case MASHOP(BCOP_NAdd, BCOPSZ_64):
			{
				FETCH(Src, u64);
				FETCH(Dst, u64);
				STORE(Dst + Src, Dst, u64);
			} break;

		case MASHOP(BCOP_GAdd, BCOPSZ_32):
		case MASHOP(BCOP_GAdd, BCOPSZ_64):
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

void BuildTestByteCode(EWC::CAlloc * pAlloc)
{
	CByteCodeBuilder bcbuild(pAlloc);

	auto pProc = bcbuild.PProcCreate("main", "main");
	
	auto pBlock = bcbuild.PBlockBegin();
	bcbuild.AddInst(BCOP_NAdd, BCOPSZ_8, OPLIT(5), OPLIT(3));
	bcbuild.AddInst(BCOP_NAdd, BCOPSZ_8, OPLIT(5), OPLIT(3));
	bcbuild.AddInst(BCOP_NAdd, BCOPSZ_8, OPLIT(5), OPLIT(3));
	bcbuild.EndBlock(pBlock);

}