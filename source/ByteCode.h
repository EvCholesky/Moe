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



	enum FOPK : u8
	{
		FOPK_Dereference	= 0x1,	// endless register values, stored on the stack - "dereferenced" upon use.
		FOPK_Arg			= 0x2,	// index onto the stack, adjusted to before the pushed current stack frame
	};


	// notes for keeping this straight in my head:
	// "SValues" only exist during build time, they are completely baked into the stack and the instruction stream by
	// invocation time.

	//  stack opk values are essentially endless register values, they need both Arg and non-arg variants as they
	//  can exist in the argument stack block. using stack indices rather than pointers as the stack positions change
	//  from invocation to invocation
	// 
	//	this is different from an actual pointer value which will be stored in a location on the stack, but have a stack 
	//  "register" denoting its location.


	enum OPK : u8	// tag = Byte Code OPERand Kind
	{
		OPK_Literal			= 0,
		OPK_LiteralArg		= FOPK_Arg, 

		OPK_Register		= FOPK_Dereference,
		OPK_RegisterArg		= FOPK_Dereference | FOPK_Arg,
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

	struct SValue	// tag = val
	{
						SValue(VALK valk)
						:m_valk(valk)
							{ ; }

		VALK			m_valk;
	};

	struct SConstant : public SValue // tag = const
	{
		static const VALK s_valk = VALK_Constant;

						SConstant(VALK valk = VALK_Constant)
						:SValue(VALK_Constant)
							{ ; }

		OPK				m_opk;
		SLiteralType	m_litty;

		SWord			m_word;
	};

	// BB - should this really be a distict struct?
	struct SRegister : public SConstant // tag = reg
	{
		static const VALK s_valk = VALK_BCodeRegister;

						SRegister()
						:SConstant(VALK_BCodeRegister)
							{ ; }
	};

	struct SInstruction : public SValue // tag = inst
	{
		static const VALK s_valk = VALK_Instruction;

						SInstruction()
						:SValue(VALK_Instruction)
						,m_irop(IROP_Error)
						,m_cBOperand(0)
						,m_pred(0)
						,m_opkLhs(OPK_Literal)
						,m_opkRhs(OPK_Literal)
						,m_iBStackOut(0)
							{ ; }

		//BB - doesn't fit 20 bytes because SValue::Valk
		IROP			m_irop;
		//OPK				m_opk;
		u8				m_cBOperand:4;		// operand byte count
		u8				m_pred:4;

		OPK				m_opkLhs;
		OPK				m_opkRhs;

		u32				m_iBStackOut;
		SWord			m_wordLhs;
		SWord			m_wordRhs;
	};



	struct SBranch	// tag = branch
	{
		s32 *		m_pIInstDst;			// opcode referring to branch - finalized to iInstDst
		SBlock *	m_pBlockDest;
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
		static const VALK s_valk = VALK_Procedure;

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

	struct CallStack
	{

	};


	class CBuilder : public CBuilderBase // tag = bcbuild
	{
	public:
		typedef SBlock Block;
		typedef SConstant Constant;
		typedef SValue Global;
		typedef SInstruction Instruction;
		typedef SProcedure Proc;
		typedef SValue Value;
		typedef SValue LValue;
		typedef STypeInfo LType;
		typedef int GepIndex;
		typedef SValue ProcArg;

						CBuilder(CWorkspace * pWork, SDataLayout * pDlay);
						~CBuilder();
		void			Clear();

		void			PrintDump();
		void			FinalizeBuild(CWorkspace * pWork);

		SProcedure *	PProcCreateImplicit(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod);
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

		SInstruction *	PInstCreateNCmp(NPRED npred, SValue * pValLhs, SValue * pValRhs, const char * pChzName = "");
		SInstruction *	PInstCreateGCmp(GPRED gpred, SValue * pValLhs, SValue * pValRhs, const char * pChzName = "");

		SInstruction *	PInstCreateCall(SValue * pValProc, ProcArg ** apLvalArgs, int cpLvalArg);

		void			CreateReturn(SValue ** ppVal, int cpVal, const char * pChzName = "");
		void			CreateBranch(SBlock * pBlock);
		SInstruction *	PInstCreateCondBranch(SValue * pValPred, SBlock * pBlockTrue, SBlock * pBlockFalse);
		SInstruction *	PInstCreateTraceStore(SValue * pVal, STypeInfo * pTin);

		s32				IBStackAlloc(s64 cB, s64 cBAlign);
		SInstruction *	PInstAlloc();

		Instruction *	PInstCreateRaw(IROP irop, s64 cBOperand, SValue * pValLhs, SValue * pValRhs, const char * pChzName = "");
		Instruction *	PInstCreateRaw(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName = "");
		Instruction *	PInstCreate(IROP irop, SValue * pValLhs, const char * pChzName = nullptr);
		Instruction *	PInstCreate(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName = nullptr);
		SInstruction *	PInstCreateError();

		Instruction *	PInstCreateCast(IROP irop, SValue * pValLhs, STypeInfo * pTinDst, const char * pChzName);
		SInstruction *	PInstCreatePtrToInt(SValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName);
		Instruction *	PInstCreateStore(SValue * pValPT, SValue * pValT);

		SValue *		PValCreateAlloca(LType * pLtype, u64 cElement, const char * pChzName = "");
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

		SInstruction *	PInstCreatePhi(LType * pLtype, const char * pChzName);
		void			AddPhiIncoming(SValue * pInstPhi, SValue * pVal, SBlock * pBlock);


		//Constant *			PConst();
		SConstant *			PConstArg(s64 n, int cBit = 64, bool fIsSigned = true);
		SRegister *			PReg(s64 n, int cBit = 64, bool fIsSigned = true);
		SRegister *			PRegArg(s64 n, int cBit = 64, bool fIsSigned = true);

		SConstant *			PConstPointer(void * pV);
		SConstant *			PConstRegAddr(s32 iBStack, int cBitRegister);
		SConstant *			PConstInt(u64 nUnsigned, int cBit = 64, bool fIsSigned = true);
		SConstant *			PConstFloat(f64 g, int cBit = 64);

		LValue *			PLvalConstantInt(u64 nUnsigned, int cBit, bool fIsSigned)
								{ return PConstInt(nUnsigned, cBit, fIsSigned); }
		LValue *			PLvalConstantFloat(f64 g, int cBit)
								{ return PConstFloat(g, cBit); }

		LValue *			PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName);
		static LValue *		PLvalConstantNull(LType * pLtype);
		static LValue *		PLvalConstantArray(LType * pLtype, LValue ** apLval, u32 cpLval);


		Constant *			PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval);
		SValue *			PValFromSymbol(SSymbol * pSym);
		void				SetSymbolValue(SSymbol * pSym, SValue * pVal);

		void				AddManagedVal(SValue * pVal);

		EWC::CAlloc *					m_pAlloc;
		CIRBuilderErrorContext *		m_pBerrctx;
		SDataLayout *					m_pDlay;
		EWC::CHash<HV, SProcedure *>	m_hashHvMangledPProc;
		EWC::CDynAry<SBlock *>			m_arypBlockManaged;
		EWC::CDynAry<SJumpTargets>		m_aryJumptStack;
		EWC::CBlockList<GepIndex, 128>	m_blistGep;
		EWC::CDynAry<SValue *>			m_arypValManaged;

		EWC::CBlockList<SConstant, 255>	m_blistConst; // constants / registers used during code generation


		SProcedure *					m_pProcCur;
		SBlock *						m_pBlockCur;
	};


#define DEBUG_PROC_CALL 1
#if DEBUG_PROC_CALL
	struct SDebugCall // tag = debcall
	{
		SInstruction **	m_ppInstCall;
		u8 *			m_pBReturnStorage;
		u8 *			m_pBStackSrc; // calling stack frame
		u8 *			m_pBStackArg;
		u8 *			m_pBStackDst;
	};
#endif

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
		SInstruction *  m_pInstArgMin;		// first argument to push when executing a call instruction

		u8 *			m_pBStackMin;
		u8 *			m_pBStackMax;
		u8 *			m_pBStack;			// current stack bottom (grows down)
		SProcedure *	m_pProcCurDebug;	// current procedure being executed (not available in release)
		EWC::SStringBuffer *
						m_pStrbuf;			

		EWC::CHash<HV, SProcedure *>	m_hashHvMangledPProc;

#if DEBUG_PROC_CALL
		EWC::CDynAry<SDebugCall> 		m_aryDebCall;
#endif 
	};

	SProcedure * PProcLookup(CVirtualMachine * pVm, HV hv);

	void ExecuteBytecode(CVirtualMachine * pVm, SProcedure * pProc);
	void BuildTestByteCode(CWorkspace * pWork, EWC::CAlloc * pAlloc);

} // namespace BCode

void CalculateByteSizeAndAlign(SDataLayout * pDlay, STypeInfo * pTin, u64 * pcB, u64 * pcBAlign);
