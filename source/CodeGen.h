/* Copyright (C) 2015 Evan Christensen
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
#include "EwcHash.h"
#include "EwcString.h"

struct LLVMOpaqueBasicBlock;
struct LLVMOpaqueBuilder;
struct LLVMOpaqueDIBuilder;
struct LLVMOpaqueModule;
struct LLVMOpaqueTargetData;
struct LLVMOpaqueTargetMachine;
struct LLVMOpaqueType;
struct LLVMOpaqueValue;

class CIRBuilderErrorContext;
class CIRInstruction;
class CSTNode;
class CWorkspace;
struct SSymbol;
struct SErrorManager;
struct STypeInfo;
struct STypeInfoInteger;



class CIRBasicBlock		// tag = block
{
public:
						CIRBasicBlock(EWC::CAlloc * pAlloc);
						~CIRBasicBlock();

	void				Append(CIRInstruction * pInst);

	LLVMOpaqueBasicBlock *			m_pLblock;
	EWC::CDynAry<CIRInstruction *>	m_arypInst;		// BB - eventually this should be replaced with something that has 
													// better random insertion performance.
	bool				m_fIsTerminated;
	bool				m_fHasErrors;
};



enum VALK	// VALue Kind
{
	VALK_Constant,
	VALK_Argument,

	VALK_ProcedureDefinition,

	VALK_Instruction,
	VALK_Global,
	/*
	VALK_TerminatorInst,
	VALK_BinaryOpInst,
	VALK_UnaryOpInst,
	VALK_CastInst,
	VALK_CompareInst,
	VALK_ReturnInst,
	VALK_InvokeInst,

	VALK_EndInst,
	VALK_BeginInst = VALK_TerminatorInst,
	*/

	VALK_Max,
	VALK_Min = 0,
	VALK_Nil = -1,
};

EWC_ENUM_UTILS(VALK);



#define OPCODE_LIST \
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

#define OP(x) IROP_##x
#define OP_RANGE(range, PREV_VAL) IROP_##range##Max, IROP_##range##Min = IROP_##PREV_VAL, IROP_##range##Last = IROP_##range##Max - 1,
	enum IROP
	{
		OPCODE_LIST

		IROP_Max,
		IROP_Min = 0,
		IROP_Nil = -1,
	};
#undef OP_RANGE
#undef OP




class CIRValue		// tag = val
{
public:
						CIRValue(VALK valk);
	virtual				~CIRValue()
							{ ; }

	LLVMOpaqueValue *	m_pLval;
	CSTNode *			m_pStnod;
	VALK				m_valk;
};



class CIRConstant : public CIRValue // tag = const
{
public:
						CIRConstant()
						:CIRValue(VALK_Constant)
							{ ; }
};



class CIRArgument : public CIRValue // tag = arg
{
public:
						CIRArgument()
						:CIRValue(VALK_Argument)
							{ ; }
};



class CIRInstruction : public CIRValue	// tag = inst
{
public:
						CIRInstruction(IROP irop)
						:CIRValue(VALK_Instruction)
						,m_cpValOperand(0)
						,m_irop(irop)
							{ ; }

	bool				FIsError() const
							{ return (m_irop == IROP_Error); }

	s8					m_cpValOperand;		// current opcode count
	IROP				m_irop;

	// NOTE - opcode array needs to be at the end of the struct, we allocate room for extra elements if needed.
	CIRValue *			m_apValOperand[1];	
};



class CIRGlobal : public CIRValue // tag = glob
{
public:
						CIRGlobal()
						:CIRValue(VALK_Global)
						{ ; }
};

struct SInsertPoint		// tag = inspt
{
						SInsertPoint()
						:m_pBlock(nullptr)
						,m_iInst(0)
							{ ; }

	CIRBasicBlock *		m_pBlock;
	s32					m_iInst;
};

class CIRProcedure	: public CIRValue // tag = proc;
{
public:
						CIRProcedure(EWC::CAlloc * pAlloc)
						:CIRValue(VALK_ProcedureDefinition)
						,m_pAlloc(pAlloc)
						,m_pLvalDIFunction(nullptr)
						,m_pLvalFunction(nullptr)
						,m_pLvalDebugLocCur(nullptr)
						,m_pBlockLocals(nullptr)
						,m_pBlockFirst(nullptr)
						,m_arypBlockManaged(pAlloc, EWC::BK_IR)
							{ ; }

						~CIRProcedure();

	EWC::CAlloc *		m_pAlloc;
	LLVMOpaqueValue *	m_pLvalDIFunction;
	LLVMOpaqueValue *	m_pLvalFunction;		// null if anonymous function
	LLVMOpaqueValue *	m_pLvalDebugLocCur;
	CIRBasicBlock *		m_pBlockLocals;			// entry basic block containing argument stores and local variable allocas
	CIRBasicBlock *		m_pBlockFirst;			// first

	EWC::CDynAry<CIRBasicBlock *>
						m_arypBlockManaged;
};

#define NCMPPRED_LIST \
	MOE_PRED(NCmpEQ)  LLVM_PRED(LLVMIntEQ) \
	MOE_PRED(NCmpNE)  LLVM_PRED(LLVMIntNE) \
	MOE_PRED(NCmpUGT) LLVM_PRED(LLVMIntUGT) \
	MOE_PRED(NCmpUGE) LLVM_PRED(LLVMIntUGE) \
	MOE_PRED(NCmpULT) LLVM_PRED(LLVMIntULT) \
	MOE_PRED(NCmpULE) LLVM_PRED(LLVMIntULE) \
	MOE_PRED(NCmpSGT) LLVM_PRED(LLVMIntSGT) \
	MOE_PRED(NCmpSGE) LLVM_PRED(LLVMIntSGE) \
	MOE_PRED(NCmpSLT) LLVM_PRED(LLVMIntSLT) \
	MOE_PRED(NCmpSLE) LLVM_PRED(LLVMIntSLE)

#define GCMPPRED_LIST \
	MOE_PRED(GCmpOEQ) LLVM_PRED(LLVMRealOEQ) \
	MOE_PRED(GCmpOGT) LLVM_PRED(LLVMRealOGT) \
	MOE_PRED(GCmpOGE) LLVM_PRED(LLVMRealOGE) \
	MOE_PRED(GCmpOLT) LLVM_PRED(LLVMRealOLT) \
	MOE_PRED(GCmpOLE) LLVM_PRED(LLVMRealOLE) \
	MOE_PRED(GCmpONE) LLVM_PRED(LLVMRealONE) \

#define MOE_PRED(X) GCMPPRED_##X,
#define LLVM_PRED(X)
enum GCMPPRED
{
	GCMPPRED_LIST

	GCMPPRED_Max,
	GCMPPRED_Min = 0,
	GCMPPRED_Nil = 1,
};
#undef MOE_PRED
#undef LLVM_PRED

#define MOE_PRED(X) NCMPPRED_##X,
#define LLVM_PRED(X)
enum NCMPPRED
{
	NCMPPRED_LIST

	NCMPPRED_Max,
	NCMPPRED_Min = 0,
	NCMPPRED_Nil = 1,
};
#undef MOE_PRED
#undef LLVM_PRED



enum FCOMPILE
{
	FCOMPILE_PrintIR	= 0x1,
	FCOMPILE_FastIsel	= 0x2,

	FCOMPILE_None		= 0x0,
	FCOMPILE_All		= 0x3,
};

EWC_DEFINE_GRF(GRFCOMPILE, FCOMPILE, u32);



struct SJumpTargets // tag = jumpt
{
					SJumpTargets()
					:m_pBlockBreak(nullptr)
					,m_pBlockContinue(nullptr)
						{ ; }

	CIRBasicBlock * m_pBlockBreak;
	CIRBasicBlock * m_pBlockContinue;
	EWC::CString	m_strLabel;
};

enum INTFUNK // INTrinsic FUNction Kind
{
	INTFUNK_Memset,
	INTFUNK_Memcpy,

	EWC_MAX_MIN_NIL(INTFUNK)
};


class CIRBuilder		// tag = build
{
public:
						CIRBuilder(
							CWorkspace * pWork,
							EWC::CDynAry<CIRValue *> *	parypValManaged,
							const char * pChzFilename,
							GRFCOMPILE grfcompile
							);
						~CIRBuilder();
	
	void				PrintDump();

	CIRBasicBlock *		PBlockCreate(CIRProcedure * pProc, const char * pChzName);

	void				ActivateProcedure(CIRProcedure * pProc, CIRBasicBlock * pBlock);
	void				ActivateBlock(CIRBasicBlock * pBlock);
	void				AddManagedVal(CIRValue * pVal);

	CIRInstruction *	PInstCreateNCmp(NCMPPRED ncmppred, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGCmp(GCMPPRED gcmppred, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);

	CIRInstruction *	PInstCreateCondBranch(CIRValue * pValPred, CIRBasicBlock * pBlockTrue, CIRBasicBlock * pBlockFalse);
	CIRInstruction *	PInstCreateBranch(CIRBasicBlock * pBlock);

	CIRInstruction *	PInstCreateAlloca(LLVMOpaqueType * pLtype, u64 cElement, const char * pChzName);
	CIRInstruction *	PInstCreateGEP(CIRValue * pValLhs, LLVMOpaqueValue ** apLvalIndices, u32 cpIndices, const char * pChzName);
	CIRInstruction *	PInstCreatePhi(LLVMOpaqueType * pLtype, const char * pChzName);

	CIRInstruction *	PInstCreateRaw(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreatePtrToInt(CIRValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName);
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, const char * pChzName);
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateCast(IROP irop, CIRValue * pValLhs, STypeInfo * pTinDst, const char * pChzName);

	CIRValue *			PValFromSymbol(SSymbol * pSym);
	CIRInstruction *	PInstCreateStore(CIRValue * pValPT, CIRValue * pValT);

	CIRGlobal *			PGlobCreate(LLVMOpaqueType * pLtype, const char * pChzName);

	CIRBuilderErrorContext *			m_pBerrctx;

	LLVMOpaqueModule *					m_pLmoduleCur;
	LLVMOpaqueBuilder *					m_pLbuild;

	LLVMOpaqueTargetMachine *			m_pLtmachine;
	LLVMOpaqueTargetData *				m_pTargd;

	LLVMOpaqueValue *					m_mpIntfunkPLval[INTFUNK_Max];		// map from intrinsic function kind to llvm function

	// Debug info
	LLVMOpaqueDIBuilder *				m_pDib;				// Debug info builder
	unsigned							m_nRuntimeLanguage;
	LLVMOpaqueValue *					m_pLvalCompileUnit;
	LLVMOpaqueValue *					m_pLvalScope;
	LLVMOpaqueValue *					m_pLvalFile;

	EWC::CAlloc *						m_pAlloc;
	SInsertPoint						m_inspt;

	CIRProcedure *						m_pProcCur;
	CIRBasicBlock *						m_pBlockCur;

	EWC::CDynAry<CIRProcedure *>		m_arypProcVerify;	// all the procedures that need verification.
	EWC::CDynAry<CIRValue *> *			m_parypValManaged;
	EWC::CDynAry<SJumpTargets>			m_aryJumptStack;
};



enum VALGENK
{
	VALGENK_Instance,
	VALGENK_Reference	// return a reference for LHS store
};

CIRValue * PValGenerate(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod, VALGENK valgenk);



void InitLLVM(EWC::CAry<const char*> * paryPCozArgs);
void ShutdownLLVM();

bool FCompileModule(CWorkspace * pWork, GRFCOMPILE grfcompile, const char * pChzFilenameIn);