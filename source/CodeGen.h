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

class CBuilderIR;
class CIRBuilderErrorContext;
class CIRInstruction;
class CSTNode;
class CSTValue;
class CSymbolTable;
class CWorkspace;
struct SDataLayout;
struct SSymbol;
struct SErrorManager;
struct STypeInfo;
struct STypeInfoEnum;
struct STypeInfoInteger;
struct STypeInfoProcedure;
struct SWorkspaceEntry;
struct SLexerLocation;



namespace BCode
{
	class CBuilder;
	struct SBlock;
	struct SProcedure;
}

class CIRBlock		// tag = block
{
public:
						CIRBlock(EWC::CAlloc * pAlloc);
						~CIRBlock();

	void				Append(CIRInstruction * pInst);

	BCode::SBlock *					m_pBlockBc;
	LLVMOpaqueBasicBlock *			m_pLblock;
	EWC::CDynAry<CIRInstruction *>	m_arypInst;		// BB - eventually this should be replaced with something that has 
													// better random insertion performance.
	bool				m_fIsTerminated;
	bool				m_fHasErrors;
};



enum VALK : s8	// VALue Kind
{
	VALK_Constant,
	VALK_Argument,

	VALK_Procedure,

	VALK_Instruction,
	VALK_Global,
	VALK_BCodeRegister,

	VALK_Max,
	VALK_Min = 0,
	VALK_Nil = -1,
};

EWC_ENUM_UTILS(VALK);



// Stack		- OPK_Stack - will be looked up
// StackAddr	- OPK_Literal stores address of stack val
// Literal		- OPK_Literal stored in instruction stream

enum OPARG // OPerand ARGuments - define the expected in->out arguments for a given bytecode opcode
{
	OPARG_Error,
	OPARG_OnlyOpcode,
	OPARG_Unary,			// (Stack|Literal) -> Stack
	OPARG_UnaryNoResult,	// (Stack|Literal)
	OPARG_Binary,			// (Stack|Literal, Stack|Literal) -> Stack
	OPARG_BinaryNoResult,	// (Stack|Literal, Stack|Literal) 
	OPARG_Compare,			// (Stack|Literal, Stack|Literal) -> Stack(bool)
	OPARG_Store,			// (StackAddr, Stack|Literal) -> Stack(lhs)
	OPARG_Branch,			// (Stack|Literal predicate, pack(iInstTrue, iInstFalse) -> 0

	OPARG_Nil = -1,
};

enum OPSZ
{
	OPSZ_0,
	OPSZ_1,
	OPSZ_2,
	OPSZ_4,
	OPSZ_8,
	OPSZ_Ptr,
	OPSZ_CB,
	OPSZ_RegIdx,
};

enum CBSRC
{
	CBSRC_Lhs,
	CBSRC_Rhs,
	CBSRC_Nil = -1
};

struct OpSignature // tag = opsig
{
	OPSZ	m_opszLhs;
	OPSZ	m_opszRhs;
	OPSZ	m_opszRet;
	CBSRC	m_cbsrc;
};

#define OPCODE_LIST \
		OP(Error)		OPARG(Error)			OPSIZE(0, 0, 0), \
		/* Ret(cBStack+cBArg) -> pValRet */ \
		OP(Ret)			OPARG(Unary)			OPSIZE(4, 0, CB), \
		OP_RANGE(TerminalOp, Ret) \
		\
		/* Call(pProcNew, pProcOld) -> pValRet */ \
		OP(Call)		OPARG(BinaryNoResult)	OPSIZE(Ptr, Ptr, 0), \
		/* CondBranch(fPred, {iInstT,iInstF}) */ \
		OP(CondBranch)	OPARG(Branch)			OPSIZE(1, 8, 0), \
		OP(Branch)		OPARG(Branch)			OPSIZE(0, 0, 0), \
		OP(Phi)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP_RANGE(JumpOp, TerminalOpMax) \
		\
		OP(NAdd)		OPARG(Binary)			OPSIZE(CB, CB, CB), \
		OP(GAdd)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(NSub)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GSub)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(NMul)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GMul)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(SDiv)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(UDiv)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GDiv)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(SRem)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(URem)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GRem)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP_RANGE(BinaryOp, JumpOpMax) \
		\
		OP(NNeg)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GNeg)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(Not)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP_RANGE(UnaryOp, BinaryOpMax) \
		\
		OP(NCmp)		OPARG(Compare)			OPSIZE(CB, CB, 1), \
		OP(GCmp)		OPARG(Compare)			OPSIZE(CB, CB, 1), \
		OP_RANGE(CmpOp, UnaryOpMax) \
		\
		OP(Shl)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(AShr)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(LShr)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(And)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(Or)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(Xor)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP_RANGE(LogicOp, CmpOpMax) \
		\
		OP(Alloca)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(Load)		OPARG(Unary)			OPSIZE(CB, 0, CB), \
		/* Store(Reg(Pointer), Value) */ \
		OP(Store)		OPARG(Store)			OPSIZE(CB, CB, 0), \
		OP(GEP)			OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(PtrDiff)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(Memcpy)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP_RANGE(MemoryOp, LogicOpMax) \
		\
		OP(NTrunc)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(SignExt)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(ZeroExt)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GToS)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GToU)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(SToG)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(UToG)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GTrunc)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(GExtend)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(PtrToInt)	OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(IntToPtr)	OPARG(Error)			OPSIZE(0, 0, 0), \
		OP(Bitcast)		OPARG(Error)			OPSIZE(0, 0, 0), \
		OP_RANGE(CastOp, MemoryOpMax) \
		/* ---- bytecode only opcodes ----*/ \
		OP(NTrace)		OPARG(UnaryNoResult)	OPSIZE(CB, 0, 0), \
		/* TraceStore(Reg, pTin) */ \
		OP(TraceStore)	OPARG(UnaryNoResult)	OPSIZE(CB, Ptr, 0), \
		/* RegStore(RegDst, ValueSrc) */ \
		OP(RegStore)	OPARG(Store)			OPSIZE(RegIdx, CB, 0), \
		OP_RANGE(BytecodeOp, CastOpMax) \

#define OPARG(x) 
#define OPSIZE(A, B, RET) 
#define OP(x) IROP_##x
#define OP_RANGE(range, PREV_VAL) IROP_##range##Max, IROP_##range##Min = IROP_##PREV_VAL, IROP_##range##Last = IROP_##range##Max - 1,
	enum IROP
	{
		OPCODE_LIST

		// TODO: Add the ranges to our enum as a second pass (so the debugger will report the enum values rather than the range endpoints

		IROP_Max,
		IROP_Min = 0,
		IROP_Nil = -1,
	};
#undef OP_RANGE
#undef OP
#undef OPSIZE
#undef OPARG

s8 COperand(IROP irop);
const char * PChzFromIrop(IROP irop);



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

class CIRProcedure	: public CIRValue // tag = proc;
{
public:
						CIRProcedure(EWC::CAlloc * pAlloc)
						:CIRValue(VALK_Procedure)
						,m_pAlloc(pAlloc)
						,m_pLvalDIFunction(nullptr)
						,m_pLvalFunction(nullptr)
						,m_pLvalDebugLocCur(nullptr)
						,m_pBlockLocals(nullptr)
						,m_pBlockFirst(nullptr)
						,m_pProcBc(nullptr)
						,m_arypBlockManaged(pAlloc, EWC::BK_IR)
							{ ; }

						~CIRProcedure();

	EWC::CAlloc *		m_pAlloc;
	LLVMOpaqueValue *	m_pLvalDIFunction;
	LLVMOpaqueValue *	m_pLvalFunction;		// null if anonymous function
	LLVMOpaqueValue *	m_pLvalDebugLocCur;
	CIRBlock *		m_pBlockLocals;			// entry basic block containing argument stores and local variable allocas
	CIRBlock *		m_pBlockFirst;			// first
	BCode::SProcedure *	m_pProcBc;

	EWC::CDynAry<CIRBlock *>
						m_arypBlockManaged;
};

#define NPRED_LIST \
	MOE_PRED(EQ)  LLVM_PRED(LLVMIntEQ) \
	MOE_PRED(NE)  LLVM_PRED(LLVMIntNE) \
	MOE_PRED(UGT) LLVM_PRED(LLVMIntUGT) \
	MOE_PRED(UGE) LLVM_PRED(LLVMIntUGE) \
	MOE_PRED(ULT) LLVM_PRED(LLVMIntULT) \
	MOE_PRED(ULE) LLVM_PRED(LLVMIntULE) \
	MOE_PRED(SGT) LLVM_PRED(LLVMIntSGT) \
	MOE_PRED(SGE) LLVM_PRED(LLVMIntSGE) \
	MOE_PRED(SLT) LLVM_PRED(LLVMIntSLT) \
	MOE_PRED(SLE) LLVM_PRED(LLVMIntSLE)

#define GPRED_LIST \
	MOE_PRED(EQ) LLVM_PRED(LLVMRealOEQ) \
	MOE_PRED(GT) LLVM_PRED(LLVMRealOGT) \
	MOE_PRED(GE) LLVM_PRED(LLVMRealOGE) \
	MOE_PRED(LT) LLVM_PRED(LLVMRealOLT) \
	MOE_PRED(LE) LLVM_PRED(LLVMRealOLE) \
	MOE_PRED(NE) LLVM_PRED(LLVMRealONE) \

#define MOE_PRED(X) GPRED_##X,
#define LLVM_PRED(X)
enum GPRED
{
	GPRED_LIST

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
	NPRED_LIST

	NPRED_Max,
	NPRED_Min = 0,
	NPRED_Nil = 1,
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

	CIRBlock * m_pBlockBreak;
	CIRBlock * m_pBlockContinue;
	EWC::CString	m_strLabel;
};

enum INTFUNK // INTrinsic FUNction Kind
{
	INTFUNK_Memset,
	INTFUNK_Memcpy,

	EWC_MAX_MIN_NIL(INTFUNK)
};

struct SDIFile // tag = dif (debug info file)
{
	LLVMOpaqueValue *				m_pLvalScope;
	LLVMOpaqueValue *				m_pLvalFile;

	EWC::CDynAry<LLVMOpaqueValue *>	m_aryLvalScopeStack;
};



class CBuilderBase
{
public:
						CBuilderBase(CWorkspace * pWork);


	EWC::CAlloc *					m_pAlloc;
	CIRBuilderErrorContext *		m_pBerrctx;
};



class CIRBuilderErrorContext // tag = berrctx
{
public:
					CIRBuilderErrorContext(SErrorManager * pErrman, CBuilderBase * pBuild, CSTNode * pStnod);
					~CIRBuilderErrorContext();

	SErrorManager *				m_pErrman;
	CBuilderBase *				m_pBuild;
	SLexerLocation *			m_pLexloc;
	CIRBuilderErrorContext *	m_pBerrctxPrev;
};



enum VALGENK
{
	VALGENK_Instance,
	VALGENK_Reference	// return a reference for LHS store
};



class CBuilderIR : public CBuilderBase	// tag = buildir
{
public:
	typedef CIRBlock Block;
	typedef CIRConstant Constant;
	typedef CIRGlobal Global;
	typedef CIRInstruction Instruction;
	typedef CIRProcedure Proc;
	typedef CIRValue Value;
	typedef LLVMOpaqueValue LValue;
	typedef LLVMOpaqueType LType;
	typedef LLVMOpaqueValue GepIndex;
	typedef LLVMOpaqueValue ProcArg;

						CBuilderIR(
							CWorkspace * pWork,
							const char * pChzFilename,
							GRFCOMPILE grfcompile);
						~CBuilderIR();
	
	void				PrintDump();

	void				FinalizeBuild(CWorkspace * pWork);
	void				ComputeDataLayout(SDataLayout * pDlay);

	CIRProcedure *		PProcCreate(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod); 
	CIRProcedure *		PProcCreate(
							CWorkspace * pWork,
							STypeInfoProcedure * pTinproc,
							const char * pChzMangled,
							CSTNode * pStnod,
							CSTNode * pStnodBody,
							EWC::CDynAry<LLVMOpaqueType *> * parypLtype,
							LLVMOpaqueType * pLtypeReturn);
	void				SetupParamBlock(
							CWorkspace * pWork,
							CIRProcedure * pProc,
							CSTNode * pStnod,
							CSTNode * pStnodParamList, 
							EWC::CDynAry<LLVMOpaqueType *> * parypLtype);

	CIRBlock *			PBlockCreate(CIRProcedure * pProc, const char * pChzName);

	void				ActivateProc(CIRProcedure * pProc, CIRBlock * pBlock);
	void				ActivateBlock(CIRBlock * pBlock);
	void				FinalizeProc(CIRProcedure * pProc);

	static LType *		PLtypeFromPTin(STypeInfo * pTin);
	static LType *		PLtypeVoid();

	CIRInstruction *	PInstCreateNCmp(NPRED npred, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGCmp(GPRED gpred, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);

	CIRInstruction *	PInstCreateCondBranch(CIRValue * pValPred, CIRBlock * pBlockTrue, CIRBlock * pBlockFalse);
	void				CreateBranch(CIRBlock * pBlock);
	void				CreateReturn(CIRValue ** ppVal, int cpVal);

	CIRInstruction *	PInstCreatePhi(LLVMOpaqueType * pLtype, const char * pChzName);
	void				AddPhiIncoming(CIRInstruction * pInstPhi, CIRValue * pVal, CIRBlock * pBlock);
	CIRInstruction *	PInstCreateCall(LValue * pLvalProc, ProcArg ** apLvalArgs, unsigned cArg);

	CIRValue *			PValGenerateCall(
							CWorkspace * pWork,
							CSTNode * pStnod,
							EWC::CDynAry<ProcArg *> * parypArgs,
							bool fIsDirectCall,
							STypeInfoProcedure * pTinproc, 
							VALGENK valgenk);
	static ProcArg *	PProcArg(CIRValue * pVal);

	CIRInstruction *	PInstCreateRaw(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreatePtrToInt(CIRValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName);
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, const char * pChzName);
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateCast(IROP irop, CIRValue * pValLhs, STypeInfo * pTinDst, const char * pChzName);

	CIRValue *			PValFromSymbol(SSymbol * pSym);
	CIRInstruction *	PInstCreateStore(CIRValue * pValPT, CIRValue * pValT);
	CIRValue *			PValCreateAlloca(LLVMOpaqueType * pLtype, u64 cElement, const char * pChzName);
	CIRInstruction *	PInstCreateMemset(CWorkspace * pWork, CIRValue * pValLhs, s64 cBSize, s32 cBAlign, u8 bFill);
	CIRInstruction *	PInstCreateMemcpy(CWorkspace * pWork, STypeInfo * pTin, CIRValue * pValLhs, CIRValue * pValRhsRef);
	CIRInstruction *	PInstCreateLoopingInit(CWorkspace * pWork, STypeInfo * pTin, CIRValue * pValLhs, CSTNode * pStnodInit);

	CIRInstruction *	PInstCreateGEP(CIRValue * pValLhs, LLVMOpaqueValue ** apLvalIndices, u32 cpIndices, const char * pChzName);
	LLVMOpaqueValue *	PGepIndex(u64 idx);
	LLVMOpaqueValue *	PGepIndexFromValue(CIRValue * pVal);

	CIRConstant *		PConstInt(u64 nUnsigned, int cBit = 64, bool fIsSigned = true);
	CIRConstant *		PConstFloat(f64 g, int cBit);
	static LValue *		PLvalConstantInt(u64 nUnsigned, int cBit, bool fIsSigned);
	static LValue *		PLvalConstantFloat(f64 g, int cBit);
	LValue *			PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName);
	static LValue *		PLvalConstantNull(LType * pLtype);
	static LValue *		PLvalConstantArray(LType * pLtype, LValue ** apLval, u32 cpLval);

	CIRConstant *		PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval);

	CIRGlobal *			PGlobCreate(LLVMOpaqueType * pLtype, const char * pChzName);
	void				AddManagedVal(CIRValue * pVal);

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

	CIRProcedure *						m_pProcCur;
	CIRBlock *							m_pBlockCur;

	EWC::CDynAry<CIRProcedure *>		m_arypProcVerify;	// all the procedures that need verification.
	EWC::CDynAry<CIRValue *> *			m_parypValManaged;
	EWC::CDynAry<SJumpTargets>			m_aryJumptStack;
};




void InitLLVM(EWC::CAry<const char*> * paryPCozArgs);
void ShutdownLLVM();

bool FCompileModule(CWorkspace * pWork, GRFCOMPILE grfcompile, const char * pChzFilenameIn);
void CodeGenEntryPointsLlvm(
	CWorkspace * pWork,
	CBuilderIR * pBuild, 
	CSymbolTable * pSymtabTop,
	EWC::CAry<SWorkspaceEntry *> * parypEntryOrder);

void CodeGenEntryPointsBytecode(
	CWorkspace * pWork,
	BCode::CBuilder * pBuild, 
	CSymbolTable * pSymtabTop,
	EWC::CAry<SWorkspaceEntry *> * parypEntryOrder,
	CIRProcedure ** ppProcUnitTest);

int NExecuteAndWait(
	const char * pChzProgram,
	const char ** ppChzArgs,
	const char ** ppChzEnvp,
	unsigned tWait,
	unsigned cBMemoryLimit,
	EWC::CString * pStrError,
	bool * pFExecutionFailed);
