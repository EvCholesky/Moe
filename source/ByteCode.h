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

#pragma once

#include "EwcString.h"
#include "EwcTypes.h"

#define BC_OPCODE_LIST \
		OP(Error), \
		OP(Ret), \
		OP_RANGE(TerminalOp, Ret) \
		\
		OP(Call), \
		OP(CondBranch), \
		OP(Branch), \
		OP(Phi), \
		OP_RANGE(JumpOp, TerminalOpMax) \
		\
		OP(NAdd), \
		OP(GAdd), \
		OP(NSub), \
		OP(GSub), \
		OP(NMul), \
		OP(GMul), \
		OP(SDiv), \
		OP(UDiv), \
		OP(GDiv), \
		OP(SRem), \
		OP(URem), \
		OP(GRem), \
		OP_RANGE(BinaryOp, JumpOpMax) \
		\
		OP(NNeg), \
		OP(GNeg), \
		OP(Not), \
		OP_RANGE(UnaryOp, BinaryOpMax) \
		\
		OP(NCmp), \
		OP(GCmp), \
		OP_RANGE(CmpOp, UnaryOpMax) \
		\
		OP(Shl), \
		OP(AShr), \
		OP(LShr), \
		OP(And), \
		OP(Or), \
		OP(Xor), \
		OP_RANGE(LogicOp, CmpOpMax) \
		\
		OP(Alloca), \
		OP(Load), \
		OP(Store), \
		OP(GEP), \
		OP(PtrDiff), \
		OP(Memcpy), \
		OP_RANGE(MemoryOp, LogicOpMax) \
		\
		OP(NTrunc), \
		OP(SignExt), \
		OP(ZeroExt), \
		OP(GToS), \
		OP(GToU), \
		OP(SToG), \
		OP(UToG), \
		OP(GTrunc), \
		OP(GExtend), \
		OP(PtrToInt), \
		OP(IntToPtr), \
		OP(Bitcast), \
		OP_RANGE(CastOp, MemoryOpMax) \

#define OP(x) BCOP_##x
#define OP_RANGE(range, PREV_VAL) BCOP_##range##Max, BCOP_##range##Min = BCOP_##PREV_VAL, BCOP_##range##Last = BCOP_##range##Max - 1,
	enum BCOP : u8
	{
		BC_OPCODE_LIST

		BCOP_Max,
		BCOP_Min = 0,
	};
#undef OP_RANGE
#undef OP



enum BCOPSZ	// tag = Byte Code OPerand Size
{
	BCOPSZ_8,
	BCOPSZ_16,
	BCOPSZ_32,
	BCOPSZ_64,
};

enum BCOPK : u8	// tag = Byte Code OPERand Kind
{
	/*
	BCOPK_Lit8,
	BCOPK_Lit16,
	BCOPK_Lit32,
	BCOPK_Lit64,
	BCOPK_Stack8,
	BCOPK_Stack16,
	BCOPK_Stack32,
	BCOPK_Stack64,
	BCOPK_Register,
	*/
	BCOPK_Literal,
	BCOPK_Stack,
	BCOPK_Register,
};



struct SInstruction		// tag = inst
{
	BCOP	m_bcop;
	u8		m_bcopkDst	: 4;		
	u8		m_bcopkSrc	: 4;		
	u8		m_bcopkMod	: 4;		
	u8		m_bcopsz	: 4;		
};



class CVirtualMachine	// tag = vm
{
public:
	u8 *	m_pBInstStart;
	u8 *	m_pBInst;
	u8 *	m_pBStack;

	u8 *	m_pBHeap;
	u8 *	m_pBCode;
};



struct SBCOperand		// tag = op 
{
			SBCOperand(BCOPK bcopk, u64 nValue)
			:m_bcopk(bcopk)
			,m_nUnsigned(nValue)
				{ ; }


	BCOPK	m_bcopk;
	union
	{
		f64		m_g;
		s64		m_nSigned;
		u64		m_nUnsigned;
	};
};



struct SBCBlock			// tag = block
{
	u32			m_iBInstMin;
};



struct SBCProcedure		// tag = proc
{
	EWC::CString		m_strName;
	EWC::CString		m_strMangled;

	SBCBlock *			m_pBlock;
};



class CByteCodeBuilder // tag = bcbuild
{
public:
					CByteCodeBuilder(EWC::CAlloc * pAlloc);

	//u32			IBStackPush(u8 cB);
	//void			StackPop(u8 cB);

	SBCProcedure *	PProcCreate(const char * pChzName, const char * pChzMangled);

	SBCBlock *		PBlockBegin();
	void			EndBlock(SBCBlock * pBlock);

	SBCOperand 		PRegisterAlloc();
	void			PRegisterFree(SBCOperand * pOp);
	//SBCOperand *	POpLoad("memory address?");

	void			AddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs);
	void			AddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs, SBCOperand & opRhs);

	EWC::CAlloc *	m_pAlloc;
	u8 *			m_pBInstMin;	// start of the instruction buffer
	u8 *			m_pBInstMax;
	u8 *			m_pBDataMin;	// start of the data buffer
	u8 *			m_pBDataMax;
};



void ExecuteBytecode(CVirtualMachine * pVm);
void BuildTestByteCode();


