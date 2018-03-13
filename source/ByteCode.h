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
#include "EwcHash.h"
#include "EwcString.h"
#include "typeinfo.h"


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
		OPARG_OnlyOpcode,
		OPARG_Unary,		// (Stack|Literal) -> Stack
		OPARG_Binary,		// (Stack|Literal, Stack|Literal) -> Stack
		OPARG_Compare,		// (Stack|Literal, Stack|Literal) -> Stack(bool)
		OPARG_Store,		// (StackAddr, Stack|Literal) -> Stack(lhs)
		OPARG_Branch,		// (Stack|Literal predicate, pack(iInstTrue, iInstFalse) -> 0

		OPARG_Nil = -1,
	};



#define BC_OPCODE_LIST \
		OP(Error) OPARG(Error), \
		OP(Ret) OPARG(Error), \
		OP_RANGE(TerminalOp, Ret) \
		\
		OP(Call) OPARG(Error), \
		OP(Return) OPARG(Error), \
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
		OP(TraceStore) OPARG(Unary), \
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


	enum FOPK : u8
	{
		FOPK_DerefStack	= 0x1,
		FOPK_Arg		= 0x2, // index onto the stack, adjusted to before the pushed current stack frame
	};

	enum OPK : u8	// tag = Byte Code OPERand Kind
	{
		OPK_Literal		= 0,
		OPK_Stack		= FOPK_DerefStack,
		OPK_ArgLiteral  = FOPK_Arg, 
		OPK_ArgStack	= FOPK_Arg | FOPK_DerefStack,

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

			void *	m_pV;
		};
	};

	struct SInstruction		// tag = inst
	{
		OP		m_op;
		u8		m_cB:4;		// operand byte count
		u8		m_pred:4;
		OPK		m_opkLhs;
		OPK		m_opkRhs;

		u32		m_iBStackOut;
		SWord	m_wordLhs;
		SWord	m_wordRhs;
	};



	struct SRecord		// tag = rec 
	{
					SRecord()
						{ ; }

					SRecord(OPK opk, SWord word)
					:m_opk(opk)
					,m_word(word)
						{ ; }

		OPK			m_opk;
		SWord		m_word;
	};

	SRecord RecFloat(f64 g);
	SRecord RecSigned(s64 nSigned);
	SRecord RecUnsigned(u64 nUnsigned);
	SRecord RecStack(s64 iBStack);
	SRecord RecArg(s64 iBStack);
	SRecord RecArgLiteral(s64 iBStack);
	SRecord RecPointer(void * pV);


	struct SBranch	// tag = branch
	{
		s32 *		m_pIInstDst;			// opcode referring to branch - finalized to iInstDst
		SBlock *	m_pBlockDest;
	};

	struct SValue	// tag = val
	{
						SValue(VALK valk)
						:m_valk(valk)
							{ ; }

		VALK			m_valk;
		OPK				m_opk;
		s32				m_cB;
		SWord			m_word;
	};

	struct SBlock // tag = block
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

	struct SParameter // tag = param
	{
		s32		m_cB;
		s32 	m_iBStack;
	};

	struct SProcedure : public SValue // tag = proc
	{
									SProcedure(EWC::CAlloc * pAlloc, STypeInfoProcedure *pTinproc);

		STypeInfoProcedure *		m_pTinproc;

		s64							m_cBStack;		// allocated bytes on stack
		s64							m_cBArg;		// stack space reserved for arguments (includes pInst return)

		SBlock *					m_pBlockLocals;
		SBlock *					m_pBlockFirst;
		EWC::CDynAry<SBlock *>		m_arypBlock;	// blocks that have written to this procedure 
		EWC::CDynAry<SInstruction>	m_aryInst;
		SParameter *				m_aParamArg;
		SParameter *				m_aParamRet;
	};

	struct SJumpTargets // tag = jumpt
	{
						SJumpTargets()
						:m_pBlockBreak(nullptr)
						,m_pBlockContinue(nullptr)
							{ ; }

		SBlock * m_pBlockBreak;
		SBlock * m_pBlockContinue;
		EWC::CString	m_strLabel;
	};

	class CBuilder : public CBuilderBase // tag = bcbuild
	{
	public:
		typedef BCode::SBlock Block;
		typedef BCode::SValue Constant;
		typedef BCode::SValue Global;
		typedef BCode::SValue Instruction;
		typedef BCode::SProcedure Proc;
		typedef BCode::SValue Value;
		typedef BCode::SValue LValue;
		typedef STypeInfo LType;
		typedef int GepIndex;
		typedef BCode::SValue ProcArg;

						CBuilder(CWorkspace * pWork, SDataLayout * pDlay);
						~CBuilder();
		void			Clear();

		void			PrintDump();
		void			FinalizeBuild(CWorkspace * pWork);

		SProcedure *	PProcCreate(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod);
		SProcedure *	PProcCreate(
							CWorkspace * pWork,
							STypeInfoProcedure * pTinproc,
							const char * pChzMangled,
							CSTNode * pStnod,
							CSTNode * pStnodBody,
							EWC::CDynAry<LType *> * parypLtype,
							LType * pLtypeReturn);
		void			SetupParamBlock(
							CWorkspace * pWork,
							Proc * pProc,
							CSTNode * pStnod,
							CSTNode * pStnodParamList, 
							EWC::CDynAry<LType *> * parypLtype);

		void			ActivateProc(SProcedure * pProc, SBlock * pBlock);
		void			FinalizeProc(SProcedure * pProc);

		SBlock *		PBlockCreate(SProcedure * pProc, const char * pChzName = nullptr);
		void			ActivateBlock(SBlock * pBlock);
		void			DeactivateBlock(SBlock * pBlock);

		static LType *	PLtypeFromPTin(STypeInfo * pTin)
							{ return pTin; } 
		static LType *	PLtypeVoid();

		void			AddInst(OP op);
		SRecord			RecAddInst(OP op, u8 cB, const SRecord & recLhs);
		SRecord			RecAddInst(OP cop, u8 cB, const SRecord & recLhs, const SRecord & recRhs);
		SRecord			RecAddNCmp(u8 cB, NCMPPRED npred, const SRecord & recLhs, const SRecord & recRhs);
		SRecord			RecAddGCmp(u8 cB, GCMPPRED gpred, const SRecord & recLhs, const SRecord & recRhs);
		Instruction *	PInstCreateNCmp(NCMPPRED npred, const SValue * pValLhs, const SValue * pValRhs, const char * pChzName);
		Instruction *	PInstCreateGCmp(GCMPPRED gpred, const SValue * pValLhs, const SValue * pValRhs, const char * pChzName);

		void			AddCall(SProcedure * pProc, SRecord * aRecArg, int cRecArg);
		SRecord			RecAddCall(SProcedure * pProc, SRecord * aRecArg, int cRecArg);
		void			CreateProcCall(LValue * pLvalProc, ProcArg ** ppProcArg, unsigned cArg, Instruction * pInstOut);

		void			CreateReturn(SValue ** ppVal, int cpVal);
		void			AddReturn(SRecord * aRecArg, int cRecArg);
		void			AddCondBranch(SRecord & recPred, SBlock * pBlockTrue, SBlock * pBlockFalse);
		void			CreateBranch(SBlock * pBlock);
		void			AddTraceStore(SRecord & rec, STypeInfo * pTin);

		SRecord			RecAddInstInternal(OP op, u8 cB, u8 pred, const SRecord & recLhs, const SRecord & recRhs);
		SRecord			AllocLocalVar(u32 cB, u32 cBAlign);
		
		s32				IBStackAlloc(s64 cB, s64 cBAlign);
		SInstruction *	PInstAlloc();

		Instruction *	PInstCreate(IROP irop, SValue * pValLhs, const char * pChzName);
		Instruction *	PInstCreate(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName);
		Instruction *	PInstCreateRaw(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName);

		Instruction *	PInstCreateCast(IROP irop, SValue * pValLhs, STypeInfo * pTinDst, const char * pChzName);
		Instruction *	PInstCreateStore(SValue * pValPT, SValue * pValT);

		Instruction *	PInstCreateAlloca(LType * pLtype, u64 cElement, const char * pChzName);
		Instruction *	PInstCreateMemset(CWorkspace * pWork, SValue * pValLhs, s64 cBSize, s32 cBAlign, u8 bFill);
		Instruction *	PInstCreateMemcpy(CWorkspace * pWork, STypeInfo * pTin, SValue * pValLhs, SValue * pValRhsRef);
		Instruction *	PInstCreateLoopingInit(CWorkspace * pWork, STypeInfo * pTin, SValue * pValLhs, CSTNode * pStnodInit);

		Instruction *	PInstCreateGEP(SValue * pValLhs, GepIndex ** apLvalIndices, u32 cpIndices, const char * pChzName);
		GepIndex *		PGepIndex(u64 idx);
		GepIndex *		PGepIndexFromValue(SValue * pVal);

		Global *		PGlobCreate(STypeInfo * pTin, const char * pChzName);

		SValue *		PValGenerateCall(
							CWorkspace * pWork,
							CSTNode * pStnod,
							EWC::CDynAry<ProcArg *> * parypArgs,
							bool fIsDirectCall,
							STypeInfoProcedure * pTinproc, 
							VALGENK valgenk);

		static ProcArg *	PProcArg(SValue * pVal);

		SValue *		PInstCreatePhi(LType * pLtype, const char * pChzName);
		void			AddPhiIncoming(SValue * pInstPhi, SValue * pVal, SBlock * pBlock);

		Constant *			PConstInt(int cBit, bool fIsSigned, u64 nUnsigned);
		Constant *			PConstFloat(int cBit, f64 g);
		static LValue *		PLvalConstantInt(int cBit, bool fIsSigned, u64 nUnsigned);
		static LValue *		PLvalConstantFloat(int cBit, f64 g);
		LValue *			PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName);
		static LValue *		PLvalConstantNull(LType * pLtype);
		static LValue *		PLvalConstantArray(LType * pLtype, LValue ** apLval, u32 cpLval);


		Constant *			PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval);
		SValue *			PValFromSymbol(SSymbol * pSym);

		void				AddManagedVal(SValue * pVal);

		EWC::CAlloc *					m_pAlloc;
		CIRBuilderErrorContext *		m_pBerrctx;
		SDataLayout *					m_pDlay;
		EWC::CHash<HV, SProcedure *>	m_hashHvMangledPProc;
		EWC::CDynAry<SBlock *>			m_arypBlockManaged;
		EWC::CDynAry<SJumpTargets>		m_aryJumptStack;
		EWC::CBlockList<GepIndex, 128>	m_blistGep;
		EWC::CDynAry<SValue *>			m_arypValManaged;


		SProcedure *					m_pProcCur;
		SBlock *						m_pBlockCur;
	};



	// Do nothing struct used as a proxy for LLVM debug info values
	struct SStub
	{
		int m_pad;
	};


	class CVirtualMachine	// tag = vm
	{
	public:
		CVirtualMachine(u8 * pBStack, u8 * pBStackMax, SDataLayout * pDlay);

		SDataLayout *	m_pDlay;
		SInstruction *	m_pInst;
		SInstruction *  m_pInstArgMin;		// first argument to push when executing a call instruction

		u8 *			m_pBStackMin;
		u8 *			m_pBStackMax;
		u8 *			m_pBStack;			// current stack bottom (grows down)
		SProcedure *	m_pProcCurDebug;	// current procedure being executed (not available in release)
		EWC::SStringBuffer *
						m_pStrbuf;			

		EWC::CHash<HV, SProcedure *>	m_hashHvMangledPProc;
	};

	SProcedure * PProcLookup(CVirtualMachine * pVm, HV hv);

	void ExecuteBytecode(CVirtualMachine * pVm, SProcedure * pProc);
	void BuildTestByteCode(CWorkspace * pWork, EWC::CAlloc * pAlloc);

} // namespace BCode

void CalculateByteSizeAndAlign(SDataLayout * pDlay, STypeInfo * pTin, u64 * pcB, u64 * pcBAlign);
