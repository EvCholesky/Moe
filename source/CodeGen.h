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
struct STypeInfoStruct;
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

	LLVMOpaqueBasicBlock *			m_pLblock;
	EWC::CDynAry<CIRInstruction *>	m_arypInst;		// BB - eventually this should be replaced with something that has 
													// better random insertion performance.
	bool				m_fIsTerminated;
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



enum OPSZ
{
	OPSZ_0,
	OPSZ_1,
	OPSZ_2,
	OPSZ_4,
	OPSZ_8,
	OPSZ_CB,
	OPSZ_PCB,	// stack index of pointer to a value cB in size
	OPSZ_Ptr,
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
		OPMN(Terminal,	Error)		OPSIZE(0, 0, 0) \
						/* Ret(cBStack+cBArg) -> regRet */ \
		OPMX(Terminal,	Ret)		OPSIZE(4, 4, 0) \
		\
						/* Call(pProcNew, cBReturn) -> regRet */ \
						/*  variadic: ExArgs(cArgVariadic, cBVariadic) */ \
		OPMN(JumpOp,	Call)		OPSIZE(Ptr, Ptr, CB) \
						/* CondBranch(fPred, {iInstT,iInstF}) */ \
		OP(				CondBranch)	OPSIZE(1, 8, 0) \
						/* Branch(0, iInst) */ \
		OP(				Branch)		OPSIZE(0, 0, 0) \
						/* Switch(Value, iInstElse) ExArgs(CmpValue, iInstBranch) */ \
		OP(				Switch)		OPSIZE(CB, 0, 0) \
						/* Phi(Value, iInstSrc,)->iBStack ExArgs(Value, iInstSrc) */ \
		OPMX(JumpOp,	Phi)		OPSIZE(CB, 0, 0) \
		\
		OPMN(BinaryOp,	NAdd)		OPSIZE(CB, CB, CB) \
		OP(				GAdd)		OPSIZE(CB, CB, CB) \
		OP(				NSub)		OPSIZE(CB, CB, CB) \
		OP(				GSub)		OPSIZE(CB, CB, CB) \
		OP(				NMul)		OPSIZE(CB, CB, CB) \
		OP(				GMul)		OPSIZE(CB, CB, CB) \
		OP(				SDiv)		OPSIZE(CB, CB, CB) \
		OP(				UDiv)		OPSIZE(CB, CB, CB) \
		OP(				GDiv)		OPSIZE(CB, CB, CB) \
		OP(				SRem)		OPSIZE(CB, CB, CB) \
		OP(				URem)		OPSIZE(CB, CB, CB) \
		OPMX(BinaryOp,	GRem)		OPSIZE(CB, CB, CB) \
		\
		OPMN(UnaryOp,	NNeg)		OPSIZE(0, 0, 0) \
		OP(				GNeg)		OPSIZE(0, 0, 0) \
		OPMX(UnaryOp,	Not)		OPSIZE(0, 0, 0) \
		\
		OPMN(CmpOp,		NCmp)		OPSIZE(CB, CB, 1) \
		OPMX(CmpOp,		GCmp)		OPSIZE(CB, CB, 1) \
		\
		OPMN(LogicOp,	Shl)		OPSIZE(CB, CB, CB) \
		OP(				AShr)		OPSIZE(CB, CB, CB) \
		OP(				LShr)		OPSIZE(CB, CB, CB) \
		OP(				And)		OPSIZE(CB, CB, CB) \
		OP(				Or)			OPSIZE(CB, CB, CB) \
		OPMX(LogicOp,	Xor)		OPSIZE(CB, CB, CB) \
		\
						/* Alloca(iBStackResult, pTinDebug)->iBStack(ref) */ \
		OPMN(MemoryOp,	Alloca)		OPSIZE(RegIdx, Ptr, PCB) \
						/* Load(Reg(Pointer)) -> RegIdx */ \
		OP(				Load)		OPSIZE(PCB, 0, CB) \
						/* Store(Src, cBOperand)->iBStackDest */ \
		OP(				Store)		OPSIZE(CB, 4, 0) \
						/* GEP(Reg(Pointer), dBOffset)->iBStack  ExArgs(val, cBStride) ...*/ \
		OP(				GEP)		OPSIZE(Ptr, 8, Ptr) \
		OP(				PtrDiff)	OPSIZE(0, 0, 0) \
						/* Memset(pDst, valByte);  ExArgs(cB)*/ \
		OP(				Memset)		OPSIZE(RegIdx, 1, 0) \
						/* Memcpy(pDst, pSrc);  ExArgs(cB)*/ \
		OPMX(MemoryOp,	Memcpy)		OPSIZE(RegIdx, RegIdx, 0) \
		\
						/* CastOp(reg, cBOperandRhs); */ \
		OPMN(CastOp,	NTrunc)		OPSIZE(0, 8, CB) \
		OP(				SignExt)	OPSIZE(0, 8, CB) \
		OP(				ZeroExt)	OPSIZE(0, 8, CB) \
		OP(				GToS)		OPSIZE(0, 8, CB) \
		OP(				GToU)		OPSIZE(0, 8, CB) \
		OP(				SToG)		OPSIZE(0, 8, CB) \
		OP(				UToG)		OPSIZE(0, 8, CB) \
		OP(				GTrunc)		OPSIZE(0, 8, CB) \
		OP(				GExtend)	OPSIZE(0, 8, CB) \
		OP(				PtrToInt)	OPSIZE(0, 0, 0) \
		OP(				IntToPtr)	OPSIZE(0, 0, 0) \
		OPMX(CastOp,	Bitcast)	OPSIZE(0, 8, CB) \
		/* ---- bytecode only opcodes ----*/ \
		OPMN(BCodeOp,	NTrace)		OPSIZE(CB, 0, 0) \
						/* TraceStore(Reg) */ \
		OP(				TraceStore)	OPSIZE(CB, Ptr, 0) \
						/* StoreToReg(Reg)->iBStackDest */ \
		OP(				StoreToReg)	OPSIZE(CB, 4, 0) \
						/* StoreToIdx(Reg)->iBStackDest */ \
		OP(				StoreToIdx)	OPSIZE(CB, 4, 0) \
						/* StoreAddress(RegIdx) ->iBStack */ \
		OP(				StoreAddress)	OPSIZE(RegIdx, 0, Ptr) \
						/* extra arguments for preceeding opcode */ \
		OPMX(BCodeOp,	ExArgs)	OPSIZE(0, 0, 0) \



#define OPSIZE(A, B, RET) 
	enum IROP : s8
	{
#define OP(X) IROP_##X,
#define OPMN(RANGE, X) IROP_##X,
#define OPMX(RANGE, X) IROP_##X,
		OPCODE_LIST
#undef OPMN
#undef OPMX
#undef OP

		IROP_Max,
		IROP_Min = 0,
		IROP_Nil = -1,

		// Add the range values in a second pass over OPCODE_LIST so that the debugger shows values rather than range endpoints
#define OP(X)
#define OPMN(RANGE, X) IROP_##RANGE##Min = IROP_##X,
#define OPMX(RANGE, X) IROP_##RANGE##Max = IROP_##X + 1,
		OPCODE_LIST
#undef OPMX
#undef OPMN
#undef OP
	};
#undef OPSIZE

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
						,m_pLvalDebugLocCur(nullptr)
						,m_pBlockLocals(nullptr)
						,m_pBlockFirst(nullptr)
						,m_arypBlockManaged(pAlloc, EWC::BK_IR)
							{ ; }

						~CIRProcedure();

	EWC::CAlloc *		m_pAlloc;
	LLVMOpaqueValue *	m_pLvalDIFunction;
	LLVMOpaqueValue *	m_pLvalDebugLocCur;
	CIRBlock *		m_pBlockLocals;			// entry basic block containing argument stores and local variable allocas
	CIRBlock *		m_pBlockFirst;			// first

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
	GPRED_Nil = -1,
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
	NPRED_Nil = -1,
};
#undef MOE_PRED
#undef LLVM_PRED

const char * PChzFromGpred(GPRED gpred);
const char * PChzFromNpred(NPRED npred);

enum FCOMPILE
{
	FCOMPILE_PrintIR	= 0x1,
	FCOMPILE_FastIsel	= 0x2,
	FCOMPILE_Native		= 0x4,
	FCOMPILE_Bytecode	= 0x8,

	FCOMPILE_None		= 0x0,
	FCOMPILE_All		= 0xF,
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

	struct SCodeGenStruct // tag = cgstruct
	{
								SCodeGenStruct()
								:m_pProcInitMethod(nullptr)
								,m_pLtype(nullptr)
								,m_pGlobInit(nullptr)
									{ ; }

		CIRProcedure *			m_pProcInitMethod;
		LLVMOpaqueType *		m_pLtype;			// type reference, here to avoid infinite recursion in
		CIRGlobal *				m_pGlobInit;		// global instance to use when CGINITK_MemcpyGlobal
	};

						CBuilderIR(
							CWorkspace * pWork,
							const char * pChzFilename,
							GRFCOMPILE grfcompile);
						~CBuilderIR();
	
	void				PrintDump();

	void				FinalizeBuild(CWorkspace * pWork);
	void				ComputeDataLayout(SDataLayout * pDlay);

	CIRProcedure *		PProcCreateImplicit(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod); 
	CIRProcedure *		PProcCreate(
							CWorkspace * pWork,
							STypeInfoProcedure * pTinproc,
							const EWC::CString & strMangled,
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

	LType *				PLtypeFromPTin(STypeInfo * pTin);
	LType *				PLtypeVoid();

	CIRInstruction *	PInstCreateNCmp(NPRED npred, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGCmp(GPRED gpred, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);

	CIRInstruction *	PInstCreateCondBranch(CIRValue * pValPred, CIRBlock * pBlockTrue, CIRBlock * pBlockFalse);
	void				CreateBranch(CIRBlock * pBlock);
	void				CreateReturn(CIRValue ** ppVal, int cpVal, const char * pChzName);

	CIRInstruction *	PInstCreatePhi(LLVMOpaqueType * pLtype, const char * pChzName);
	void				AddPhiIncoming(CIRInstruction * pInstPhi, CIRValue * pVal, CIRBlock * pBlock);
	CIRInstruction *	PInstCreateSwitch(CIRValue * pVal, CIRBlock * pBlockElse, u32 cCases);
	void				AddSwitchCase(CIRInstruction * pInstSwitch, CIRValue * pValOn, CIRBlock * pBlock);

	CIRInstruction *	PInstCreateCall(LValue * pLvalProc, STypeInfoProcedure * pTinproc, ProcArg ** apLvalArgs, unsigned cArg);

	CIRValue *			PValGenerateCall(
							CWorkspace * pWork,
							CSTNode * pStnod,
							SSymbol * pSym,
							EWC::CDynAry<ProcArg *> * parypArgs,
							bool fIsDirectCall,
							STypeInfoProcedure * pTinproc, 
							VALGENK valgenk);

	static ProcArg *	PProcArg(CIRValue * pVal);

	CIRInstruction *	PInstCreateRaw(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, const char * pChzName);
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);

	CIRInstruction *	PInstCreateCast(IROP irop, CIRValue * pValLhs, STypeInfo * pTinDst, const char * pChzName);
	CIRInstruction *	PInstCreatePtrToInt(CIRValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName);
	CIRInstruction *	PInstCreateStore(CIRValue * pValPT, CIRValue * pValT);

	CIRValue *			PValFromSymbol(SSymbol * pSym);
	void				SetSymbolValue(SSymbol * pSym, CIRValue * pVal);
	SCodeGenStruct *	PCgstructEnsure(STypeInfoStruct * pTinstruct);


	CIRValue *			PValCreateAlloca(LLVMOpaqueType * pLtype, u64 cElement, const char * pChzName);
	CIRInstruction *	PInstCreateMemset(CIRValue * pValLhs, s64 cBSize, s32 cBAlign, u8 bFill);
	CIRInstruction *	PInstCreateMemcpy(STypeInfo * pTin, CIRValue * pValLhs, CIRValue * pValRhsRef);

	CIRInstruction *	PInstCreateTraceStore(CIRValue * pVal, STypeInfo * pTin)
							{ return nullptr; }

	CIRInstruction *	PInstCreateGEP(CIRValue * pValLhs, LLVMOpaqueValue ** apLvalIndices, u32 cpIndices, const char * pChzName);
	LLVMOpaqueValue *	PGepIndex(u64 idx);
	LLVMOpaqueValue *	PGepIndexFromValue(CIRValue * pVal);

	CIRConstant *		PConstInt(u64 nUnsigned, int cBit = 64, bool fIsSigned = true);
	CIRConstant *		PConstFloat(f64 g, int cBit);
	static LValue *		PLvalConstantInt(u64 nUnsigned, int cBit, bool fIsSigned);
	static LValue *		PLvalConstantFloat(f64 g, int cBit);
	LValue *			PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName);
	LValue *			PLvalConstantNull(LType * pLtype);
	LValue *			PLvalConstantArray(LType * pLtype, LValue ** apLval, u32 cpLval);
	LValue *			PLvalConstantStruct(LType * pLtype, LValue ** apLval, u32 cpLval);

	CIRConstant *		PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval);

	CIRGlobal *			PGlobCreate(LLVMOpaqueType * pLtype, const char * pChzName);
	void				SetInitializer(CIRGlobal * pGlob, LValue * pLconst);
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
	EWC::CHash<SSymbol *, CIRValue *>	m_hashPSymPVal;
	EWC::CHash<STypeInfoStruct *, SCodeGenStruct *>	
										m_hashPTinstructPCgstruct;
};



inline bool FIsRegisterSize(u64 cB)
{
	return (cB == 1) | (cB == 2) | (cB == 4) | (cB == 8);
}

void InitLLVM(EWC::CAry<const char*> * paryPCozArgs);
void ShutdownLLVM();

bool FCompileModule(CWorkspace * pWork, GRFCOMPILE grfcompile, const char * pChzFilenameIn);

typedef EWC::CBlockList<SWorkspaceEntry, 128> BlockListEntry;
void CodeGenEntryPointsLlvm(
	CWorkspace * pWork,
	CBuilderIR * pBuild, 
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	EWC::CAry<SWorkspaceEntry *> * parypEntryOrder);

void CodeGenEntryPointsBytecode(
	CWorkspace * pWork,
	BCode::CBuilder * pBuild, 
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	EWC::CAry<SWorkspaceEntry *> * parypEntryOrder,
	BCode::SProcedure ** ppProcUnitTest);

int NExecuteAndWait(
	const char * pChzProgram,
	const char ** ppChzArgs,
	const char ** ppChzEnvp,
	unsigned tWait,
	unsigned cBMemoryLimit,
	EWC::CString * pStrError,
	bool * pFExecutionFailed);
