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

#include "EwcArray.h"
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



enum BCOPSZ : u8 // tag = Byte Code OPerand Size
{
	BCOPSZ_8,
	BCOPSZ_16,
	BCOPSZ_32,
	BCOPSZ_64,
	BCOPSZ_Max
};

enum BCOPK : u8	// tag = Byte Code OPERand Kind
{
	BCOPK_Literal,
	BCOPK_Stack,
	BCOPK_Register,
	BCOPK_Nil = 255
};

enum BCOPTYPE : u8
{
	BCOPTYPE_Float,
	BCOPTYPE_Signed,
	BCOPTYPE_Unsigned,
	BCOPTYPE_Vid,
};



struct SInstruction		// tag = inst
{
	BCOP	m_bcop;
	u8		m_bcopkLhs	: 4;		
	u8		m_bcopkRhs	: 4;		
	u8		m_bcopkOut	: 4;		
	u8		m_bcopsz	: 4;		
};



class CVirtualMachine	// tag = vm
{
public:
			CVirtualMachine(u8 * pBInst, u8 * pBStack);

	u8 *	m_pBInstStart;
	u8 *	m_pBInst;
	u8 *	m_pBStack;
};



/*
enum VID : u16		//  tag = Value ID
{
	VID_Max = 0xFFFF,
	VID_Nil = 0xFFFF
};*/



struct SRecord		// tag = rec 
{
	BCOPK		m_bcopk;
	BCOPTYPE	m_bcoptype;
	BCOPSZ		m_bcopsz;

	union
	{
		s8		m_s8;
		s16		m_s16;
		s32		m_s32;
		s64		m_s64;
		u8		m_u8;
		u16		m_u16;
		u32		m_u32;
		u64		m_u64;
		f32		m_f32;
		f64		m_f64;
	};
};

SRecord RecFloat(f64 g);
SRecord RecSigned(s64 nSigned);
SRecord RecUnsigned(u64 nUnsigned);
SRecord RecStack(u32 iBStack);
//SRecord RecVid(VID vid);



struct SBCBlock			// tag = block
{
	u32			m_iBInstMin;
	u32			m_cBStack;	// room needed on the stack for temporaries and local vars
};



struct SBCProcedure		// tag = proc
{
						SBCProcedure(EWC::CString strName, EWC::CString strMangled);
	EWC::CString		m_strName;
	EWC::CString		m_strMangled;

	u32					m_cBStack;	// allocated bytes on stack

	SBCBlock *			m_pBlock;
};



/*
struct SBCValue		//	bcval
{
	u32			m_iBStack;	// stack space allocated for 
	BCOPSZ		m_bcopsz;
};*/



class CByteCodeBuilder // tag = bcbuild
{
public:
					CByteCodeBuilder(EWC::CAlloc * pAlloc);

	SBCProcedure *	PProcCreate(const char * pChzName, const char * pChzMangled);
	void			BeginProc(SBCProcedure * pProc);
	void			EndProc(SBCProcedure * pProc);

	SBCBlock *		PBlockBegin();
	void			EndBlock(SBCBlock * pBlock);

//	SBCOperand 		PRegisterAlloc();
//	void			PRegisterFree(SBCOperand * pOp);

	//SBCValue		BcvalAddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs);
	//SBCValue		BcvalAddInst(BCOP bcop, BCOPSZ bcopsz, SBCOperand & opLhs, SBCOperand & opRhs);

	SRecord			RecAddInst(BCOP bcop, BCOPSZ bcopsz, const SRecord & recLhs);
	SRecord			RecAddInst(BCOP bcop, BCOPSZ bcopsz, const SRecord & recLhs, const SRecord & recRhs);

	u32				IBStackAlloc(u32 cB, u32 cBAlign);
	u8 *			PBStackAlloc(u32 cB, u32 cBAlign);
	//u8 *			PBInstAlloc(u32 cB, u32 cBAlign);
	void			PackInst(const void * pV, u32 cB);
	//void			UnpackInst(void * pV, u32 cB);

	EWC::CAlloc *					m_pAlloc;
	EWC::CDynAry<SRecord>			m_aryBcval;		
	EWC::CDynAry<SBCProcedure *>	m_arypProcManaged;

	SBCProcedure *			m_pProcCur;
	u8 *					m_pBInstMin;	// start of the instruction buffer
	u8 *					m_pBInstCur;
	u8 *					m_pBInstMax;
//	u8 *					m_pBDataMin;	// start of the data buffer
//	u8 *					m_pBDataMax;
};



void ExecuteBytecode(CVirtualMachine * pVm);
void BuildTestByteCode(EWC::CAlloc * pAlloc);


