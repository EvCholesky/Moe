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

#include "CodeGen.h"
#include "EwcArray.h"
#include "EwcString.h"
#include "EwcTypes.h"


namespace BCode
{
	struct SBlock;
	struct SProcedure;

	// Legend
	// Stack		- OPK_Stack - will be looked up
	// StackAddr	- OPK_Literal stores address of stack val
	// Literal		- OPK_Literal stored in instruction stream

	enum OPARG // OPerand ARGuments - define the expected in->out arguments for a given opcode
	{
		OPARG_Error,
		OPARG_Unary,		// (Stack|Literal) -> Stack
		OPARG_Binary,		// (Stack|Literal, Stack|Literal) -> Stack
		OPARG_Compare,		// (Stack|Literal, Stack|Literal) -> Stack(bool)
		OPARG_Store,		// (StackAddr, Stack|Literal) -> Stack(lhs)
		OPARG_Branch,		// (Stack|Literal predicate, pack(iInstTrue, iInstFalse) -> 0

		OPARG_Nil = -1,
	};



	#define MOE_PRED(X) GPRED_##X,
	#define LLVM_PRED(X)
	enum GPRED
	{
		GCMPPRED_LIST

		GPRED_Max,
		GPRED_Min = 0,
		GPRED_Nil = 1,
	};
	#undef MOE_PRED
	#undef LLVM_PRED

	#define MOE_PRED(X) NPRED_##X,
	#define LLVM_PRED(X)
	enum NPRED
	{
		NCMPPRED_LIST

		NPRED_Max,
		NPRED_Min = 0,
		NPRED_Nil = 1,
	};
	#undef MOE_PRED
	#undef LLVM_PRED



#define BC_OPCODE_LIST \
		OP(Error) OPARG(Error), \
		OP(Ret) OPARG(Error), \
		OP(Halt) OPARG(Unary), \
		OP_RANGE(TerminalOp, Ret) \
		\
		OP(Call) OPARG(Error), \
		OP(CondBranch) OPARG(Branch), \
		OP(Branch) OPARG(Branch), \
		OP(Phi) OPARG(Error), \
		OP_RANGE(JumpOp, TerminalOpMax) \
		\
		OP(NAdd) OPARG(Binary), \
		OP(GAdd) OPARG(Error), \
		OP(NSub) OPARG(Error), \
		OP(GSub) OPARG(Error), \
		OP(NMul) OPARG(Error), \
		OP(GMul) OPARG(Error), \
		OP(SDiv) OPARG(Error), \
		OP(UDiv) OPARG(Error), \
		OP(GDiv) OPARG(Error), \
		OP(SRem) OPARG(Error), \
		OP(URem) OPARG(Error), \
		OP(GRem) OPARG(Error), \
		OP_RANGE(BinaryOp, JumpOpMax) \
		\
		OP(NNeg) OPARG(Error), \
		OP(GNeg) OPARG(Error), \
		OP(Not) OPARG(Error), \
		OP_RANGE(UnaryOp, BinaryOpMax) \
		\
		OP(NCmp) OPARG(Compare), \
		OP(GCmp) OPARG(Compare), \
		OP_RANGE(CmpOp, UnaryOpMax) \
		\
		OP(Shl) OPARG(Error), \
		OP(AShr) OPARG(Error), \
		OP(LShr) OPARG(Error), \
		OP(And) OPARG(Error), \
		OP(Or) OPARG(Error), \
		OP(Xor) OPARG(Error), \
		OP_RANGE(LogicOp, CmpOpMax) \
		\
		OP(Alloca) OPARG(Error), \
		OP(Load) OPARG(Unary), \
		OP(Store) OPARG(Store), \
		OP(GEP) OPARG(Error), \
		OP(PtrDiff) OPARG(Error), \
		OP(Memcpy) OPARG(Error), \
		OP(NTrace) OPARG(Unary), \
		OP_RANGE(MemoryOp, LogicOpMax) \
		\
		OP(NTrunc) OPARG(Error), \
		OP(SignExt) OPARG(Error), \
		OP(ZeroExt) OPARG(Error), \
		OP(GToS) OPARG(Error), \
		OP(GToU) OPARG(Error), \
		OP(SToG) OPARG(Error), \
		OP(UToG) OPARG(Error), \
		OP(GTrunc) OPARG(Error), \
		OP(GExtend) OPARG(Error), \
		OP(PtrToInt) OPARG(Error), \
		OP(IntToPtr) OPARG(Error), \
		OP(Bitcast) OPARG(Error), \
		OP_RANGE(CastOp, MemoryOpMax) \


#define OP(x) OP_##x
#define OPARG(x)
#define OP_RANGE(range, PREV_VAL) OP_##range##Max, OP_##range##Min = OP_##PREV_VAL, OP_##range##Last = OP_##range##Max - 1,
	enum OP : u8
	{
		BC_OPCODE_LIST

		OP_Max,
		OP_Min = 0,
	};
#undef OP_RANGE
#undef OP
#undef OPARG



	enum OPSZ : u8 // tag = Byte Code OPerand Size
	{
		OPSZ_8,
		OPSZ_16,
		OPSZ_32,
		OPSZ_64,
		OPSZ_Max
	};

	enum OPK : u8	// tag = Byte Code OPERand Kind
	{
		OPK_Literal,
		OPK_Stack,
		OPK_Register,

		OPK_Nil			= 255
	};



	struct SWord	// tag = word
	{
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

	struct SInstruction		// tag = inst
	{
		OP		m_op;
		u8		m_opsz:4;
		u8		m_pred:4;
		OPK		m_opkLhs;
		OPK		m_opkRhs;

		u32		m_iBStackOut;
		SWord	m_wordLhs;
		SWord	m_wordRhs;
	};



	class CVirtualMachine	// tag = vm
	{
	public:
		CVirtualMachine(u8 * pBInst, s32 cInst, u8 * pBStack);

		u8 *	m_pBInst;
		s32		m_cInst;

		u8 *	m_pBStack;
	};

	struct SRecord		// tag = rec 
	{
		OPK			m_opk;
		SWord		m_word;
	};

	SRecord RecFloat(f64 g);
	SRecord RecSigned(s64 nSigned);
	SRecord RecUnsigned(u64 nUnsigned);
	SRecord RecStack(u32 iBStack);


	struct SBranch // tag = branch
	{
		s32 *		m_pIInstDst;			// opcode referring to branch - finalized to iInstDst
		SBlock *	m_pBlockDest;
	};

	struct SBlock			// tag = block
	{
						SBlock()
						:m_iInstFinal(-1)
						,m_pProc(nullptr)
						,m_aryInst()
						,m_aryBranch()
							{ ; }

						bool FIsFinalized() const
							{ return m_iInstFinal >= 0; }

		s32							m_iInstFinal;
		SProcedure *				m_pProc;
		EWC::CDynAry<SInstruction>	m_aryInst;
		EWC::CDynAry<SBranch>		m_aryBranch;	// outgoing links in control flow graph.
	};



	struct SProcedure		// tag = proc
	{
									SProcedure(EWC::CAlloc * pAlloc, EWC::CString strName, EWC::CString strMangled);

		EWC::CString				m_strName;
		EWC::CString				m_strMangled;

		u32							m_cBStack;		// allocated bytes on stack

		SBlock *					m_pBlockEntry;
		EWC::CDynAry<SBlock *>		m_arypBlock;	// blocks that have written to this procedure 
		EWC::CDynAry<SInstruction>	m_aryInst;
	};



	class CBuilder // tag = bcbuild
	{
	public:
		CBuilder(EWC::CAlloc * pAlloc);

		SProcedure *	PProcCreate(const char * pChzName, const char * pChzMangled);
		void			BeginProc(SProcedure * pProc);
		void			EndProc(SProcedure * pProc);
		void			FinalizeProc(SProcedure * pProc);

		SBlock *		PBlockCreate();
		void			BeginBlock(SBlock * pBlock);
		void			EndBlock(SBlock * pBlock);

		SRecord			RecAddInst(OP bcop, OPSZ copsz, const SRecord & recLhs);
		SRecord			RecAddInst(OP bcop, OPSZ copsz, const SRecord & recLhs, const SRecord & recRhs);
		SRecord			RecAddNCmp(OPSZ copsz, NPRED npred, const SRecord & recLhs, const SRecord & recRhs);
		SRecord			RecAddGCmp(OPSZ copsz, GPRED gpred, const SRecord & recLhs, const SRecord & recRhs);

		void			RecAddCondBranch(SRecord & recPred, SBlock * pBlockTrue, SBlock * pBlockFalse);
		void			RecAddBranch(SBlock * pBlock);

		SRecord			RecAddInstInternal(OP bcop, OPSZ copsz, u8 pred, const SRecord & recLhs, const SRecord & recRhs);
		SRecord			AllocLocalVar(u32 cB, u32 cBAlign);
		
		u32				IBStackAlloc(u32 cB, u32 cBAlign);
		SInstruction *	PInstAlloc();

		EWC::CAlloc *					m_pAlloc;
		EWC::CDynAry<SProcedure *>		m_arypProcManaged;
		EWC::CDynAry<SBlock *>			m_arypBlockManaged;

		SProcedure *					m_pProcCur;
		SBlock *						m_pBlockCur;
	};



	void ExecuteBytecodeOld(CVirtualMachine * pVm);
	void BuildTestByteCode(EWC::CAlloc * pAlloc);

}

