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

#include "ByteCode.h"
#include "CodeGen.h"
#include "EwcTypes.h"
#include "Parser.h"
#include "TypeInfo.h"
#include "Util.h"
#include "Workspace.h"

using namespace EWC;

#ifdef _WINDOWS
#pragma warning ( push )
#pragma warning(disable : 4141)
#pragma warning(disable : 4146)
#pragma warning(disable : 4267)
#pragma warning(disable : 4800)
#pragma warning(disable : 4996)
#endif
#include "llvm-c/Analysis.h"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Program.h"
#include "llvm/BinaryFormat/Dwarf.h"
#ifdef _WINDOWS
#pragma warning ( pop )
#endif

#include "MissingLlvmC/llvmcDIBuilder.h"
#include <stdio.h>
#include <string>

#define WARN_ON_LARGE_STORE 0
#define USE_OP_NAMES 1
#if USE_OP_NAMES
static constexpr const char * OPNAME(const char * pChz) { return pChz; }
#else
static constexpr const char * OPNAME(const char * pChz) { return ""; }
#endif

#define LLVM_ENABLE_DUMP _DEBUG

#define ASSERT_STNOD(PWORK, PSTNOD, PREDICATE, ... ) do { if (!(PREDICATE)) { \
		EWC::AssertHandler(__FILE__, __LINE__, #PREDICATE, __VA_ARGS__); \
		s32 iLine, iCol; \
		CalculateLinePosition(PWORK->m_pErrman->m_pWork, &PSTNOD->m_lexloc, &iLine, &iCol); \
		printf("compiling: %s:%u\n", PSTNOD->m_lexloc.m_strFilename.PCoz(), iLine); \
		EWC_DEBUG_BREAK(); \
		 } } while(0)


#define FVERIFY_STNOD(PWORK, PSTNOD, PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	(\
		EWC::AssertHandler( __FILE__, __LINE__, #PREDICATE, __VA_ARGS__ ),\
		s32 iLine, iCol; \
		CalculateLinePosition(PWORK->m_pErrman->m_pWork, &PSTNOD->m_lexloc, &iLine, &iCol); \
		printf("compiling: %s:%u\n", PSTNOD->m_lexloc.m_strFilename.PCoz(), iLine); \
		EWC_DEBUG_BREAK(), \
		false\
	)\
  )\
)

template <typename BUILD>
typename BUILD::Value * PValGenerate(CWorkspace * pWork, BUILD * pBuild, CSTNode * pStnod, VALGENK valgenk);

template <typename BUILD>
typename BUILD::Proc * PProcCodegenPrototype(CWorkspace * pWork, BUILD * pBuild, CSTNode * pStnod);

struct SReflectGlobalTable;
extern bool FIsDirectCall(CSTNode * pStnodCall);

template <typename BUILD>
typename BUILD::Proc * PProcCodegenInitializer(CWorkspace * pWork, BUILD * pBuild, CSTNode * pStnodStruct);
LLVMOpaqueValue * PLvalParentScopeForProcedure(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnodProc, SDIFile * pDif);
CIRValue * PValGenerateArrayLiteralReference(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnod);
LLVMOpaqueValue * PLvalEnsureReflectStruct(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	LLVMOpaqueType * pLtypeTin,
	STypeInfo * pTin,
	SReflectGlobalTable * pReftab);

CIRInstruction * PInstGenerateAssignmentFromRef(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs,
	CIRValue * pValLhs,
	CIRValue * pValRhsRef);

BCode::SInstruction * PInstGenerateAssignmentFromRef(
	CWorkspace * pWork,
	BCode::CBuilder * pBuild,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs,
	BCode::SValue * pValLhs,
	BCode::SValue * pValRhsRef);


template <typename BUILD>
static inline typename BUILD::Value * PValGenerateCast(
	CWorkspace * pWork,
	BUILD * pBuild,
	VALGENK valgenk,
	CSTNode * pStnodRhs,
	STypeInfo * pTinOut);

template <typename BUILD>
typename BUILD::Instruction * PInstGenerateAssignment(
	CWorkspace * pWork,
	BUILD * pBuild,
	STypeInfo * pTinLhs,
	typename BUILD::Value * pValLhs,
	CSTNode * pStnodRhs);

template <typename BUILD>
static inline typename BUILD::Value * PValInitialize(
	CWorkspace * pWork,
	BUILD * pBuild,
	STypeInfo * pTin,
	typename BUILD::Value * pValPT,	// pLhs
	CSTNode * pStnodInit);

enum CGINITK	// Code Gen INIT Kind
{
	CGINITK_NoInit,
	CGINITK_MemsetZero,
	CGINITK_AssignInitializer,
	CGINITK_MemcpyGlobal,
	CGINITK_LoopingInit,
	CGINITK_InitializerProc,

	EWC_MAX_MIN_NIL(CGINITK)
};

CGINITK CginitkCompute(STypeInfo * pTin, CSTNode * pStnodInit);

const char * STypeInfo::s_pChzGlobalTinTable =  "_tinTable";

const char * PChzFromIrop(IROP irop)
{
	if (!EWC_FVERIFY((irop >= IROP_Nil) & (irop < IROP_Max), "unknown IR operand"))
		return "(unknown)";

	if (irop <= IROP_Nil)
		return "nil";

	#define OP(X) #X,
	#define OPMN(RANGE, X) #X,
	#define OPMX(RANGE, X) #X,
	#define OPSIZE(LHS, RHS, RET)
	const char * s_mpIropPChz[] =
	{
		OPCODE_LIST
	};
	#undef OPMX
	#undef OPMN
	#undef OPSIZE
	#undef OP

	EWC_CASSERT(EWC_DIM(s_mpIropPChz) == IROP_Max, "missing IROP string");
	return s_mpIropPChz[irop];
}

const char * PChzFromGpred(GPRED gpred)
{
	if (!EWC_FVERIFY((gpred >= GPRED_Nil) & (gpred < GPRED_Max), "unknown predicate"))
		return "(unknown)";

	if (gpred <= GPRED_Nil)
		return "nil";

	#define MOE_PRED(X) #X,
	#define LLVM_PRED(X)
	const char * s_mpGpredPChz[] =
	{
		GPRED_LIST
	};
	#undef LLVM_PRED
	#undef MOE_PRED

	EWC_CASSERT(EWC_DIM(s_mpGpredPChz) == GPRED_Max, "missing GPRED string");
	return s_mpGpredPChz[gpred];
}

const char * PChzFromNpred(NPRED npred)
{
	if (!EWC_FVERIFY((npred >= NPRED_Nil) & (npred < NPRED_Max), "unknown predicate"))
		return "(unknown)";

	if (npred <= NPRED_Nil)
		return "nil";

	#define MOE_PRED(X) #X,
	#define LLVM_PRED(X)
	const char * s_mpNpredPChz[] =
	{
		NPRED_LIST
	};
	#undef LLVM_PRED
	#undef MOE_PRED

	EWC_CASSERT(EWC_DIM(s_mpNpredPChz) == NPRED_Max, "missing NPRED string");
	return s_mpNpredPChz[npred];
}

static inline llvm::CallingConv::ID CallingconvFromCallconv(CALLCONV callconv)
{
	static const llvm::CallingConv::ID s_mpCallconvCallingconv[] = 
	{
		llvm::CallingConv::C,				//CALLCONV_CX86,
		llvm::CallingConv::X86_StdCall,		//CALLCONV_StdcallX86,
		llvm::CallingConv::Win64,			//CALLCONV_X64,
	};
	static const int s_cCallconv = sizeof(s_mpCallconvCallingconv) / sizeof(s_mpCallconvCallingconv[0]);
	EWC_CASSERT(s_cCallconv == CALLCONV_Max, "Missing llvm calling convention");

	if (callconv < CALLCONV_Nil || callconv >= CALLCONV_Max)
		callconv = CALLCONV_Nil; 

	if (callconv == CALLCONV_Nil)
	{
#if EWC_X64
		return llvm::CallingConv::C;
#else
		return llvm::CallingConv::Win64;
#endif
	}

	return s_mpCallconvCallingconv[callconv];
}

static inline void DumpLtype(const char * pChzLabel, CIRValue * pVal)
{
	printf("%s: ", pChzLabel);

#if LLVM_ENABLE_DUMP
	if (pVal)
	{
		auto pLtype = LLVMTypeOf(pVal->m_pLval);
		LLVMDumpType(pLtype);
	}
	else
	{
		printf("null value");
	}
#else
	printf("LLVM_ENABLE_DUMP is not defined.");
#endif
}

// Debug Info Wrappers

LLVMValueRef PLvalDInfoCreateLexicalBlock(CBuilderIR * pBuild, LLVMValueRef pLvalScope, SDIFile * pDif, unsigned nLine, unsigned nCol)
	{ return LLVMDIBuilderCreateLexicalBlock(pBuild->m_pDib, pLvalScope, pDif->m_pLvalFile, nLine, nCol); }

BCode::SStub * PLvalDInfoCreateLexicalBlock(BCode::CBuilder * pBuild, BCode::SStub * pLvalScope, BCode::SStub * pDif, unsigned nLine, unsigned nCol)
	{ return nullptr; }

void DInfoCreateGlobalVariable(BCode::CBuilder * pBuild, STypeInfo * pTin, BCode::SStub * pLvalScope, BCode::SStub * pDif, const char * pChzName, const char * pChzMangled, unsigned nLine, bool fIsLocalToUnit, BCode::CBuilder::Global * pGlob)
	{ ; }

void DInfoCreateTypedef( BCode::CBuilder * pBuild, STypeInfo * pTin, const char * pChzTypedefName, BCode::SStub * pDif, unsigned nLine, BCode::SStub * pLvalScope)
	{ ; }


void DInfoCreateGlobalVariable(
	CBuilderIR * pBuild,
	STypeInfo * pTin,
	LLVMValueRef pLvalScope, 
	SDIFile * pDif,
	const char * pChzName,
	const char * pChzMangled,
	unsigned nLine,
	bool fIsLocalToUnit,
	CIRGlobal * pGlob)
{
	LLVMDIBuilderCreateGlobalVariable(
		pBuild->m_pDib,
		pLvalScope,
		pChzName,
		pChzMangled,
		pDif->m_pLvalFile,
		nLine,
		(LLVMValueRef)pTin->m_pCgvalDIType,
		true,
		pGlob->m_pLval);
}

LLVMValueRef PLvallDInfoCreateAutoVariable(
	CBuilderIR * pBuild,
	STypeInfo * pTin,
	LLVMValueRef pLvalScope,
	SDIFile * pDif,
	const char * pChzName,
	unsigned nLine,
	bool fIsPreservedWhenOptimized,
	unsigned nFlags)
{

	return LLVMDIBuilderCreateAutoVariable(
		pBuild->m_pDib,
		pLvalScope,
		pChzName,
		pDif->m_pLvalFile,
		nLine,
		(LLVMValueRef)pTin->m_pCgvalDIType,
		false,
		0);
}

BCode::SStub * PLvallDInfoCreateAutoVariable(
	BCode::CBuilder * pBuild,
	STypeInfo * pTin,
	BCode::SStub * pLvalScope,
	BCode::SStub * pDif,
	const char * pChzName,
	unsigned nLine,
	bool fIsPreservedWhenOptimized,
	unsigned nFlags)
{
	return nullptr;
}

void DInfoCreateTypedef(
	CBuilderIR * pBuild,
	STypeInfo * pTin,
	const char * pChzTypedefName,
	SDIFile * pDif,
	unsigned nLine,
	LLVMValueRef pLvalScope)
{

	(void) LLVMDIBuilderCreateTypeDef(
			pBuild->m_pDib,
			(LLVMValueRef)pTin->m_pCgvalDIType,
			pChzTypedefName,
		    pDif->m_pLvalFile,
			nLine,
		    pLvalScope);
}

void DInfoInsertDeclare(
	CBuilderIR * pBuild,
	CIRValue * pValAlloca,
	LLVMValueRef pLvalDIVariable,
	LLVMValueRef pLvalScope,
	unsigned nLine,
	unsigned nCol,
	CIRBlock * pBlock)
{
	(void)LLVMDIBuilderInsertDeclare(
		pBuild->m_pDib,
		pValAlloca->m_pLval,
		pLvalDIVariable,
		pLvalScope,
		nLine,
		nCol,
		pBuild->m_pBlockCur->m_pLblock);
}

void DInfoInsertDeclare(
	BCode::CBuilder * pBuild,
	BCode::SValue * pInstAllocA,
	BCode::SStub * pLvalDIVariable,
	BCode::SStub * pLvalScope,
	unsigned nLine,
	unsigned nCol,
	BCode::SBlock * pBlock)
{
	;
}

static inline LLVMValueRef PLvalDInfo(CIRProcedure * pProc)
	{ return pProc->m_pLvalDIFunction; }

static inline BCode::SStub * PLvalDInfo(BCode::SProcedure * pProc)
	{ return nullptr; }

static inline LLVMValueRef PLvalFromPVal(CIRValue * pVal)
	{ return pVal->m_pLval; }

static inline BCode::SValue * PLvalFromPVal(BCode::SValue * pVal)
	{ return pVal; }

static void SetInitializer(CIRGlobal * pGlob, LLVMValueRef pLconst)
{
	LLVMSetInitializer(pGlob->m_pLval, pLconst);
}

static void SetInitializer(BCode::SValue * pGlob, BCode::SValue * pConst)
{
	EWC_ASSERT(false, "bytecode TBD");
}

static unsigned CParamFromProc(CIRProcedure * pProc)
{
	return LLVMCountParams(pProc->m_pLval);
}

static unsigned CParamFromProc(BCode::SProcedure * pProc)
{
	return (unsigned)pProc->m_pTinproc->m_arypTinParams.C();
}

static inline bool FIsError(CIRInstruction * pInst)
{ 
	return (pInst->m_irop == IROP_Error); 
}

static inline bool FIsError(BCode::CBuilder::Instruction * pInst)
{ 
	EWC_ASSERT(false, "bytecode tbd");
	return false;
}

CIRValue::CIRValue(VALK valk)
:m_pLval(nullptr)
,m_pStnod(nullptr)
,m_valk(valk)
{
}



CIRBlock::CIRBlock(EWC::CAlloc * pAlloc)
:m_pBlockBc(nullptr)
,m_pLblock(nullptr)
,m_arypInst(pAlloc, EWC::BK_IR)
,m_fIsTerminated(false)
{
}

CIRBlock::~CIRBlock()
{
	m_arypInst.Clear();
}

void CIRBlock::Append(CIRInstruction * pInst)
{
	if (!EWC_FVERIFY(!m_fIsTerminated, "Appending instruction to a terminated block"))
		return;

	m_arypInst.Append(pInst);
	m_fIsTerminated = (pInst->m_irop == IROP_Ret) | (pInst->m_irop == IROP_CondBranch) | (pInst->m_irop == IROP_Branch);
}

s8 COperand(IROP irop)
{
	if (irop == IROP_Call)
		return 0;
	if (((irop >= IROP_BinaryOpMin) & (irop < IROP_BinaryOpMax)) | 
		((irop >= IROP_CmpOpMin) & (irop < IROP_CmpOpMax)) | 
		((irop >= IROP_LogicOpMin) & (irop < IROP_LogicOpMax)) | 
		(irop == IROP_Store) |
		(irop == IROP_TraceStore) | (irop == IROP_StoreToReg))
		return 2;
	return 1;
}



CIRProcedure::~CIRProcedure()
{
	size_t cpBlock = m_arypBlockManaged.C();
	for (size_t ipBlock = 0; ipBlock < cpBlock; ++ipBlock)
	{
		m_pAlloc->EWC_DELETE(m_arypBlockManaged[ipBlock]);
	}
	m_arypBlockManaged.Clear();
}



CIRBuilderErrorContext::CIRBuilderErrorContext(SErrorManager * pErrman, CBuilderBase * pBuild, CSTNode * pStnod)
:m_pErrman(pErrman)
,m_pBuild(pBuild)
,m_pLexloc(&pStnod->m_lexloc)
,m_pBerrctxPrev(pBuild->m_pBerrctx)
{
	pBuild->m_pBerrctx = this;
}

CIRBuilderErrorContext::~CIRBuilderErrorContext()
{
	EWC_ASSERT(m_pBuild->m_pBerrctx == this, "bad error context in builder");
	m_pBuild->m_pBerrctx = m_pBerrctxPrev;
}


/* WIP for oversized return/arg pass by reference support.
static inline bool FCanReturnByValue(CIRBuilder * pBuild, LLVMOpaqueType * pLtype)
{
	// In C++ this would be required to be a POD type, but we don't have constructors, destrctors, etc...
	u64 cBitSize;
	u64 cBitAlign;
	CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);
	return cBitSize < 64;
}*/


template <typename BUILD>
typename BUILD::LType * PLtypeFromPTin(BUILD * pBuild, STypeInfo * pTin, u64 * pCElement)
{
	if (pCElement)
	{
		switch (pTin->m_tink)
		{
		case TINK_Array:	*pCElement = ((STypeInfoArray *)pTin)->m_c;
		case TINK_Literal:	*pCElement = ((STypeInfoLiteral *)pTin)->m_c;
		default: 
			*pCElement = 1;
		}
	}

	return pBuild->PLtypeFromPTin(pTin);
}

LLVMOpaqueType * CBuilderIR::PLtypeFromPTin(STypeInfo * pTin)
{
	if (!pTin)
		return nullptr;

	switch (pTin->m_tink)
	{
		case TINK_Void:
		{
			return LLVMVoidType();
		}
		case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;

			size_t cpLtypeParam = pTinproc->m_arypTinParams.C();
			auto apLtypeParam = (LLVMTypeRef *)(alloca(sizeof(LLVMTypeRef *) * cpLtypeParam));
			for (size_t ipLtype = 0; ipLtype < cpLtypeParam; ++ipLtype)
			{
				apLtypeParam[ipLtype] = PLtypeFromPTin(pTinproc->m_arypTinParams[ipLtype]);
			}

			LLVMOpaqueType * pLtypeReturn = (pTinproc->m_arypTinReturns.C()) ? 
												PLtypeFromPTin(pTinproc->m_arypTinReturns[0]) : 
												LLVMVoidType();	

			auto pLtypeFunction = LLVMFunctionType(pLtypeReturn, apLtypeParam, (u32)cpLtypeParam, pTinproc->FHasVarArgs());

			// NOTE: actually a pointer to a function (not the function type itself)
			auto pLtypPtr = LLVMPointerType(pLtypeFunction, 0);
			return pLtypPtr;

		}
		case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			return PLtypeFromPTin(pTinqual->m_pTin);
		}
		case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;
			LLVMOpaqueType * pLtypePointedTo;
			if (pTinptr->m_pTinPointedTo->m_tink == TINK_Void)
			{
				// llvm doesn't support void pointers so we treat them as s8 pointers instead
				pLtypePointedTo = LLVMInt8Type();
			}
			else
			{
				pLtypePointedTo = PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			}
			return LLVMPointerType(pLtypePointedTo, 0);
		}
		case TINK_Array:
		{
			STypeInfoArray * pTinary = (STypeInfoArray *)pTin;
			auto pLtypeElement = PLtypeFromPTin(pTinary->m_pTin);

			switch (pTinary->m_aryk)
			{
				case ARYK_Fixed:
				{
					return LLVMArrayType(pLtypeElement, u32(pTinary->m_c));
				}
				case ARYK_Reference:
				{
					LLVMTypeRef apLtype[2]; // count, pointer
					apLtype[ARYMEMB_Count] = LLVMInt64Type();
					apLtype[ARYMEMB_Data] = LLVMPointerType(pLtypeElement, 0);

					return LLVMStructType(apLtype, EWC_DIM(apLtype), false);
				}
				default: EWC_ASSERT(false, "unhandled ARYK");
			}
		}
		case TINK_Bool:		return LLVMInt1Type();
	    case TINK_Integer:	
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			switch (pTinint->m_cBit)
			{
			case 8:		return LLVMInt8Type();
			case 16:	return LLVMInt16Type();
			case 32:	return LLVMInt32Type();
			case 64:	return LLVMInt64Type();
			default:	return nullptr;
			}
		}
	    case TINK_Float:
		{
			auto pTinfloat = (STypeInfoFloat *)pTin;
			switch (pTinfloat->m_cBit)
			{
			case 32:	return LLVMFloatType();
			case 64:	return LLVMDoubleType();
			default:	return nullptr;
			}
		}
		case TINK_Literal:
		{
			auto pTinlit = (STypeInfoLiteral *)pTin;
			switch (pTinlit->m_litty.m_litk)
			{
			case LITK_Integer:	return LLVMInt64Type();
			case LITK_Float:	return LLVMDoubleType();
			case LITK_Bool:		return LLVMInt1Type();
			case LITK_Char:		return nullptr;
			case LITK_String:	return LLVMPointerType(LLVMInt8Type(), 0);
			case LITK_Null:		return PLtypeFromPTin(pTinlit->m_pTinSource);
			case LITK_Array:
			{
				auto pLtypeElement = PLtypeFromPTin(pTinlit->m_pTinSource);
				return LLVMArrayType(pLtypeElement, u32(pTinlit->m_c));
			}
			default:			return nullptr;
			}
		}
		case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			return PLtypeFromPTin(pTinenum->m_pTinLoose);
		}
		case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;

			auto pCgstruct = PCgstructEnsure(pTinstruct);
			if (pCgstruct->m_pLtype)
			{
				return (LLVMTypeRef)pCgstruct->m_pLtype;
			}

			LLVMOpaqueType * pLtype = LLVMStructCreateNamed(LLVMGetGlobalContext(), PChzVerifyAscii(pTinstruct->m_strName.PCoz()));
			pCgstruct->m_pLtype = pLtype;

			int cTypemembField = (int)pTinstruct->m_aryTypemembField.C();
			auto apLtypeMember = (LLVMTypeRef *)(alloca(sizeof(LLVMTypeRef) * cTypemembField));
			LLVMTypeRef * ppLtypeMember = apLtypeMember;

			auto pTypemembMac = pTinstruct->m_aryTypemembField.PMac();
			for ( auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMac; ++pTypememb)
			{
				auto pLtypeMember = PLtypeFromPTin(pTypememb->m_pTin);
				EWC_ASSERT(pLtypeMember, "failed to compute type for structure member %s", pTypememb->m_strName.PCoz());
				*ppLtypeMember++ = pLtypeMember;
			}

			LLVMStructSetBody(pLtype, apLtypeMember, cTypemembField, false);

			return pLtype;
		}
		case TINK_Generic:
		{
			EWC_ASSERT(false, "encountered non-instantiated generic type '%s' during codegen", pTin->m_strName.PCoz());
			return nullptr;
		}
		default: return nullptr;
	}
}

void PathSplitDestructive(char * pCozFull, size_t cBMax, const char ** ppCozPath, const char ** ppCozFile, const char ** ppCozExt)
{
	const char * pCozPath = ".";
	char * pCozFile = pCozFull;

	char * pChLastSlash = nullptr;
	char * pChLastPeriod = nullptr;
	
	char * pChEnd = pCozFull + cBMax;
	for (char * pCozIt = pCozFull; pCozIt != pChEnd; ++pCozIt)
	{
		if ((*pCozIt == '/') | (*pCozIt == '\\'))
		{
			pChLastSlash = pCozIt;
		}

		if (*pCozIt == '.')
		{
			pChLastPeriod = pCozIt;
		}

		if (*pCozIt == '\0')
			break;
	}

	if (pChLastSlash)
	{
		*pChLastSlash = '\0';
		pCozPath = pCozFull;
		pCozFile = pChLastSlash + 1;
	}

	*ppCozPath = pCozPath;
	*ppCozFile = pCozFile;

	// if ppCozExt is non-null we trim and return the extension
	if (ppCozExt && pChLastPeriod)
	{
		if ((uintptr_t)pChLastPeriod >= (uintptr_t)pCozFile)
		{
			*pChLastPeriod = '\0';
			*ppCozExt = pChLastPeriod + 1;
		}
	}
}

SDIFile * PDifEnsure(CWorkspace * pWork, CBuilderIR * pBuild, const CString & strFilename)
{
	auto pFile = pWork->PFileLookup(strFilename.PCoz(), CWorkspace::FILEK_Source);
	if (!EWC_FVERIFY(pFile, "bad file lookup in PDifEnsure"))
		return nullptr;

	if (pFile->m_pDif)
		return pFile->m_pDif;

	auto pDif = EWC_NEW(pWork->m_pAlloc, SDIFile) SDIFile();
	pFile->m_pDif = pDif;

	size_t cBFilename = pFile->m_strFilename.CB() + 1;
	char * pCozCopy = (char *)alloca(sizeof(char) * cBFilename);
	EWC::SStringBuffer strbuf(pCozCopy, cBFilename);
	AppendCoz(&strbuf, pFile->m_strFilename.PCoz());

	const char * pCozPath;	
	const char * pCozFile;	
	PathSplitDestructive(pCozCopy, cBFilename, &pCozPath, &pCozFile, nullptr);

	pDif->m_pLvalScope = pBuild->m_pLvalCompileUnit;
	pDif->m_pLvalFile = LLVMDIBuilderCreateFile(pBuild->m_pDib, pCozFile, pCozPath);

	pDif->m_aryLvalScopeStack.SetAlloc(pWork->m_pAlloc, EWC::BK_WorkspaceFile);
	pDif->m_aryLvalScopeStack.Append(pDif->m_pLvalFile);

	return pDif;
}

BCode::SStub * PDifEnsure(CWorkspace * pWork, BCode::CBuilder * pBuild, const CString & strFilename)
{
	return nullptr;
}


LLVMOpaqueValue * PLvalFromDIFile(CBuilderIR * pBuild, SDIFile * pDif)
{
	if (pDif->m_aryLvalScopeStack.FIsEmpty())
	{
		return pBuild->m_pLvalFile;
	}

	return *pDif->m_aryLvalScopeStack.PLast();
}

BCode::SStub * PLvalFromDIFile(BCode::CBuilder * pBuild, BCode::SStub * pDif)
{
	return nullptr;
}

void PushDIScope(SDIFile * pDif, LLVMOpaqueValue * pLvalScope)
{
	EWC_ASSERT(pLvalScope, "null debug info scope");
	pDif->m_aryLvalScopeStack.Append(pLvalScope);
}

void PopDIScope(SDIFile * pDif, LLVMOpaqueValue * pLvalScope)
{
	pDif->m_aryLvalScopeStack.PopLast();
}

void PushDIScope(BCode::SStub * pDif, BCode::SStub * pLvalScope)
	{ ; }

void PopDIScope(BCode::SStub * pDif, BCode::SStub * pLvalScope)
	{ ; }


void PushLexicalBlock(CWorkspace * pWork, CBuilderIR * pBuild, const SLexerLocation & lexloc)
{
	auto pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);

	LLVMOpaqueValue * pLvalScopeParent = PLvalFromDIFile(pBuild, pDif);

	s32 iLine, iCol;
	CalculateLinePosition(pWork, &lexloc, &iLine, &iCol);

	LLVMValueRef pLvalScope = LLVMDIBuilderCreateLexicalBlock(pBuild->m_pDib, pLvalScopeParent, pDif->m_pLvalFile, iLine, iCol);
	pDif->m_aryLvalScopeStack.Append(pLvalScope);
}

void PopLexicalBlock(CWorkspace * pWork, CBuilderIR * pBuild, const SLexerLocation & lexloc)
{
	auto pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);
	pDif->m_aryLvalScopeStack.PopLast();
}

void EmitLocation(CWorkspace * pWork, CBuilderIR * pBuild, const SLexerLocation & lexloc)
{
	auto pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);

	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	s32 iLine, iCol;
	CalculateLinePosition(pWork, &lexloc, &iLine, &iCol);

	LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pLvalScope);
	LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pLvalLoc);
}

void EmitLocation(CWorkspace * pWork, BCode::CBuilder * pBuild, const SLexerLocation & lexloc)
{
}

SDIFile * PDifEmitLocation(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	const SLexerLocation & lexloc,
	s32 * piLine = nullptr,
	s32 * piCol = nullptr)
{
	auto pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);

	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	s32 iLine, iCol;
	CalculateLinePosition(pWork, &lexloc, &iLine, &iCol);

	LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pLvalScope);
	LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pLvalLoc);

	if (piLine) *piLine = iLine;
	if (piCol) *piCol = iCol;

	return pDif;
}

BCode::SStub * PDifEmitLocation(
	CWorkspace * pWork,
	BCode::CBuilder * pBuild,
	const SLexerLocation & lexloc,
	s32 * piLine = nullptr,
	s32 * piCol = nullptr)
{
	return nullptr;
}

void CalculateSizeAndAlign(CBuilderIR * pBuild, LLVMOpaqueType * pLtype, u64 * pCBitSize, u64 *pCBitAlign)
{
	*pCBitSize = LLVMSizeOfTypeInBits(pBuild->m_pTargd, pLtype);
	*pCBitAlign = LLVMABIAlignmentOfType(pBuild->m_pTargd, pLtype) * 8;
}

void CalculateSizeAndAlign(BCode::CBuilder * pBuild, BCode::CBuilder::LType * pTin, u64 * pCBitSize, u64 *pCBitAlign)
{
	u64 cByteSize;
	u64 cByteAlign;
	CalculateByteSizeAndAlign(pBuild->m_pDlay, pTin, &cByteSize, &cByteAlign);
	*pCBitSize = cByteSize * 8;
	*pCBitAlign = cByteAlign * 8;
}

template <typename BUILD>
typename BUILD::Proc * PProcTryEnsure(CWorkspace * pWork, BUILD * pBuild, SSymbol * pSym)
{
	auto pVal = pBuild->PValFromSymbol(pSym);
	if (pVal && EWC_FVERIFY(pVal->m_valk == VALK_Procedure, "symbol is not procedure definition"))
	{
		return (BUILD::Proc *)pVal;
	}

	// this happens when calling a method that is defined later
	CSTNode * pStnodProc = pSym->m_pStnodDefinition;
	auto pDif = PDifEnsure(pWork, pBuild, pStnodProc->m_lexloc.m_strFilename.PCoz());
	auto pLvalParentScope = PLvalParentScopeForProcedure(pWork, pBuild, pStnodProc, pDif);

	PushDIScope(pDif, pLvalParentScope);
	(void) PProcCodegenPrototype(pWork, pBuild, pSym->m_pStnodDefinition);
	PopDIScope(pDif, pLvalParentScope);

	if (pVal && EWC_FVERIFY(pVal->m_valk == VALK_Procedure, "symbol is not procedure definition"))
	{
		return (BUILD::Proc *)pVal;
	}
	return nullptr;
}

LLVMOpaqueValue * PLvalParentScopeForProcedure(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnodProc, SDIFile * pDif)
{
	auto pStproc = PStmapRtiCast<CSTProcedure *>(pStnodProc->m_pStmap);
	if (EWC_FVERIFY(pStproc, "function missing procedure") && pStproc->m_pStnodParentScope)
	{
		CIRProcedure * pProc = nullptr;
		auto pSymParentScope = pStproc->m_pStnodParentScope->m_pSym;
		if (EWC_FVERIFY(pSymParentScope, "expected symbol to be set during type check"))
		{
			pProc = (CIRProcedure *)PProcTryEnsure<CBuilderIR>(pWork, pBuild, pSymParentScope);

			if (!EWC_FVERIFY(pProc && pProc->m_valk == VALK_Procedure, "expected IR procedure"))
				pProc = nullptr;
		}

		if (pProc)
		{
			return pProc->m_pLvalDIFunction;
		}
	}

	return pBuild->m_pLvalCompileUnit;
}

BCode::SStub * PLvalParentScopeForProcedure(CWorkspace * pWork, BCode::CBuilder * pBuild, CSTNode * pStnodProc, BCode::SStub * pDif)
{
	return nullptr;
}

static inline LLVMOpaqueValue * PLvalCreateDebugFunction(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	const char * pChzName,
	const char * pChzMangled,
	CSTNode * pStnodFunction,	// BB - should be lexlocFunction
	CSTNode * pStnodBody,		// BB - should be lexlocBody
	LLVMOpaqueValue * pLvalDIFunctionType,
	LLVMOpaqueValue * pLvalFunction)
{
	s32 iLine, iCol;
	CalculateLinePosition(pWork, &pStnodFunction->m_lexloc, &iLine, &iCol);

	s32 iLineBody, iColBody;
	CalculateLinePosition(pWork, &pStnodBody->m_lexloc, &iLineBody, &iColBody);

	auto pDif = PDifEnsure(pWork, pBuild, pStnodBody->m_lexloc.m_strFilename.PCoz());
	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	return LLVMDIBuilderCreateFunction(
			pBuild->m_pDib,
			pLvalScope,
			pChzName,
			pChzMangled,
			pDif->m_pLvalFile,
			iLine,
			pLvalDIFunctionType,
			false, //fIsLocalToUnit
			true, //fIsDefinition
			iLineBody,
			DINODE_FLAG_Prototyped,
			false,	//fIsOptimized
			pLvalFunction,
			nullptr,	// pLvalTemplateParm
			nullptr);	// pValDecl
}
static inline void CreateDebugInfo(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnodRef, STypeInfo * pTin)
{
	if (pTin->m_pCgvalDIType)
		return;

	auto strPunyName = StrPunyEncode(pTin->m_strName.PCoz());
	auto pDib = pBuild->m_pDib;

	switch (pTin->m_tink)
	{
	case TINK_Integer: 
		{
			auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTin);
			unsigned nDwarf;
			if (pTinint->m_fIsSigned)
			{
				nDwarf = (pTinint->m_cBit == 8) ? llvm::dwarf::DW_ATE_signed_char : llvm::dwarf::DW_ATE_signed;
			}
			else
			{
				nDwarf = (pTinint->m_cBit == 8) ? llvm::dwarf::DW_ATE_unsigned_char : llvm::dwarf::DW_ATE_unsigned;
			}
			pTin->m_pCgvalDIType = LLVMDIBuilderCreateBasicType(pDib, strPunyName.PCoz(), pTinint->m_cBit, nDwarf);
		} break;
	case TINK_Float:
		{
			auto pTinfloat = PTinRtiCast<STypeInfoFloat *>(pTin);
			unsigned nDwarf = llvm::dwarf::DW_ATE_float;
			pTin->m_pCgvalDIType = LLVMDIBuilderCreateBasicType(pDib, strPunyName.PCoz(), pTinfloat->m_cBit, nDwarf);
		} break;
	case TINK_Bool:
		{
			unsigned nDwarf = llvm::dwarf::DW_ATE_boolean;
			pTin->m_pCgvalDIType = LLVMDIBuilderCreateBasicType(pDib, strPunyName.PCoz(), 8, nDwarf);
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			CreateDebugInfo(pWork, pBuild, pStnodRef, pTinqual->m_pTin);

			pTin->m_pCgvalDIType = pTinqual->m_pTin->m_pCgvalDIType;
		} break;
    case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;

			auto pTinPointedTo = pTinptr->m_pTinPointedTo;
			if (pTinPointedTo->m_tink == TINK_Void)
			{
				// llvm doesn't support void pointers so we treat them as s8 pointers instead
				pTinPointedTo->m_pCgvalDIType = LLVMDIBuilderCreateBasicType(pDib, "void", 8, llvm::dwarf::DW_ATE_signed_char);
			}
			else
			{
				CreateDebugInfo(pWork, pBuild, pStnodRef, pTinPointedTo);
			}

			u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
			u64 cBitAlign = cBitSize;
			pTin->m_pCgvalDIType = LLVMDIBuilderCreatePointerType(
										pDib,
										(LLVMValueRef)pTinptr->m_pTinPointedTo->m_pCgvalDIType,
										cBitSize,
										cBitAlign,
										strPunyName.PCoz());
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = PTinRtiCast<STypeInfoProcedure *>(pTin);

			int cpTinParam = (int)pTinproc->m_arypTinParams.C();
			auto apLvalParam = (LLVMValueRef *)(alloca(sizeof(LLVMValueRef) * cpTinParam));

			for (int ipTin = 0; ipTin < cpTinParam; ++ipTin)
			{
				auto pTin = pTinproc->m_arypTinParams[ipTin];

				CreateDebugInfo(pWork, pBuild, pStnodRef, pTin);
				apLvalParam[ipTin] = (LLVMValueRef)pTin->m_pCgvalDIType;
			}

			u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
			u64 cBitAlign = cBitSize;
			pTin->m_pCgvalDIType = LLVMDIBuilderCreateFunctionType(pDib, apLvalParam, cpTinParam, cBitSize, cBitAlign);
		} break;
	case TINK_Array:
		{
			auto pTinary = PTinRtiCast<STypeInfoArray *>(pTin);

			auto pTinElement = pTinary->m_pTin;
			CreateDebugInfo(pWork, pBuild, pStnodRef, pTinElement);

			auto pLtypeArray = pBuild->PLtypeFromPTin(pTinary);
			u64 cBitSizeArray, cBitAlignArray;
			CalculateSizeAndAlign(pBuild, pLtypeArray, &cBitSizeArray, &cBitAlignArray);

			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
				{
					LLVMOpaqueValue * apLvalSubscript[1];
					apLvalSubscript[0] = LLVMDIBuilderGetOrCreateRange(pDib, 0, pTinary->m_c);
					pTin->m_pCgvalDIType = LLVMDIBuilderCreateArrayType(
						pDib,
						cBitSizeArray,
						cBitAlignArray,
						(LLVMValueRef)pTinElement->m_pCgvalDIType,
						apLvalSubscript,
						1);
				} break;
		    case ARYK_Reference:
				{
					auto pDif = PDifEnsure(pWork, pBuild, pStnodRef->m_lexloc.m_strFilename);
					LLVMOpaqueValue * pLvalScope = pDif->m_pLvalFile;

					auto pLtypeMember =  pBuild->PLtypeFromPTin(pTinary->m_pTin);

					u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
					u64 cBitAlign = cBitSize;
					auto pLvalDITypePtr = LLVMDIBuilderCreatePointerType(pDib, (LLVMValueRef)pTinElement->m_pCgvalDIType, cBitSize, cBitAlign, "");

					LLVMTypeRef mpArymembPLtype[ARYMEMB_Max]; // pointer, count
					mpArymembPLtype[ARYMEMB_Count] = LLVMInt64Type();
					mpArymembPLtype[ARYMEMB_Data] = LLVMPointerType(pLtypeMember, 0);

					auto pTinCount = pWork->m_pSymtab->PTinBuiltin("s64");
					CreateDebugInfo(pWork, pBuild, pStnodRef, pTinCount);

					LLVMValueRef mpArymembPLvalDIType[ARYMEMB_Max]; // pointer, count
					mpArymembPLvalDIType[ARYMEMB_Count] = (LLVMValueRef)pTinCount->m_pCgvalDIType;
					mpArymembPLvalDIType[ARYMEMB_Data] = pLvalDITypePtr;

					u64 cBitSizeMember, cBitAlignMember;
					unsigned nFlagsMember = 0;
					LLVMOpaqueValue * apLvalMember[2]; // pointer, count	

					for (int arymemb = 0; arymemb < ARYMEMB_Max; ++arymemb)
					{
						auto pLtypeMember =  mpArymembPLtype[arymemb];
						u64 dBitMembOffset = 8 * LLVMOffsetOfElement(pBuild->m_pTargd, pLtypeArray, arymemb);
						CalculateSizeAndAlign(pBuild, pLtypeMember, &cBitSizeMember, &cBitAlignMember);

						apLvalMember[arymemb] = LLVMDIBuilderCreateMemberType(
													pDib,
													pLvalScope,
													PChzFromArymemb((ARYMEMB)arymemb),
													pDif->m_pLvalFile,
													0,
													cBitSizeMember,
													cBitAlignMember,
													dBitMembOffset,
													nFlagsMember,
													mpArymembPLvalDIType[arymemb]);
					}

					unsigned nFlags = 0;
					pTin->m_pCgvalDIType = LLVMDIBuilderCreateStructType(
											pDib,
											pLvalScope,
											"",
											pDif->m_pLvalFile,
											0,
											cBitSizeArray,
											cBitAlignArray,
											nFlags,
											nullptr, //pLvalDerivedFrom
											apLvalMember,
											ARYMEMB_Max,
											nullptr, //pLvalVTableHolder
											pBuild->m_nRuntimeLanguage);
				} break;
			default:
		    case ARYK_Dynamic:
				EWC_ASSERT(false, "debug info is for aryk %s is TBD", PChzFromAryk(pTinary->m_aryk));
				break;
			}
		} break;
	case TINK_Struct:
		{
			auto pTinstruct = PTinRtiCast<STypeInfoStruct *>(pTin);
			s32 iLineBody, iColBody;
			CalculateLinePosition(pWork, &pTinstruct->m_pStnodStruct->m_lexloc, &iLineBody, &iColBody);
			
			auto pDif = PDifEnsure(pWork, pBuild, pTinstruct->m_pStnodStruct->m_lexloc.m_strFilename);

			// BB - This will not work for nested structs (nested in methods or structs)
			auto pLvalScope = pDif->m_pLvalFile;

			u64 cBitSize, cBitAlign;
			auto pLtypeStruct =  pBuild->PLtypeFromPTin(pTinstruct);
			CalculateSizeAndAlign(pBuild, pLtypeStruct, &cBitSize, &cBitAlign);

			auto strPunyName = StrPunyEncode(pTinstruct->m_strName.PCoz()); // BB - should actually make unique name!
			const char * pChzUniqueName = strPunyName.PCoz(); // BB - should actually make unique name!
			unsigned nFlags = 0;

			auto pLvalDicomp = LLVMDIBuilderCreateReplacableComposite(
									pDib, 
									llvm::dwarf::DW_TAG_structure_type,
									pLvalScope, 
									pTinstruct->m_strName.PCoz(), 
									pDif->m_pLvalFile,
								    iLineBody, 
									pBuild->m_nRuntimeLanguage,
									cBitSize, 
									cBitAlign, 
									nFlags,
									pChzUniqueName);

			int cTypememb = (int)pTinstruct->m_aryTypemembField.C();
			auto apLvalMember = (LLVMOpaqueValue **)(alloca(sizeof(LLVMOpaqueValue *) * cTypememb));
			pTin->m_pCgvalDIType = pLvalDicomp;

			for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
			{
				auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

				auto pTinMember = pTypememb->m_pTin;
				CreateDebugInfo(pWork, pBuild, pStnodRef, pTinMember);

				auto pLtypeElement = pBuild->PLtypeFromPTin(pTinMember);
				u64 cBitSizeMember, cBitAlignMember;
				CalculateSizeAndAlign(pBuild, pLtypeElement, &cBitSizeMember, &cBitAlignMember);

				s32 iLineMember, iColMember;
				CalculateLinePosition(pWork, &pTypememb->m_pStnod->m_lexloc, &iLineMember, &iColMember);

				u64 dBitMembOffset = 8 * LLVMOffsetOfElement(pBuild->m_pTargd, pLtypeStruct, iTypememb);

				unsigned nFlagsMember = 0;
				apLvalMember[iTypememb] = LLVMDIBuilderCreateMemberType(
											pDib,
											pLvalDicomp,
											pTypememb->m_strName.PCoz(),
											pDif->m_pLvalFile,
											iLineMember,
											cBitSizeMember,
											cBitAlignMember,
											dBitMembOffset,
											nFlagsMember,
											(LLVMValueRef)pTinMember->m_pCgvalDIType);
			}

			LLVMDIBuilderReplaceCompositeElements(pDib, &pLvalDicomp, apLvalMember, cTypememb);

		} break;
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			auto pStnodDefinition = pTinenum->m_tinstructProduced.m_pStnodStruct;

			auto pDif = PDifEnsure(pWork, pBuild, pStnodDefinition->m_lexloc.m_strFilename);

			// BB - This will not work for nested structs (nested in methods or structs)
			auto pLvalScope = pDif->m_pLvalFile;

			s32 iLine, iCol;
			CalculateLinePosition(pWork, &pStnodDefinition->m_lexloc, &iLine, &iCol);

			CreateDebugInfo(pWork, pBuild, pStnodDefinition, pTinenum->m_pTinLoose);

			size_t cTinecon  = (int)pTinenum->m_aryTinecon.C();
			auto apLvalConstant = (LLVMOpaqueValue **)(alloca(sizeof(LLVMOpaqueValue *) * cTinecon));

			for (size_t iTinecon = 0; iTinecon < cTinecon; ++iTinecon)
			{
				auto pTinecon = &pTinenum->m_aryTinecon[iTinecon];
				s64 nValue = 0;
				auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinenum->m_pTinLoose);
				if (EWC_FVERIFY(pTinint, "expected enum loose type to be integer type") && pTinint->m_fIsSigned)
				{
					nValue = pTinecon->m_bintValue.S64Coerce();
				}
				else
				{
					nValue = pTinecon->m_bintValue.m_nAbs;
				}

				auto strPunyName = StrPunyEncode(pTinecon->m_strName.PCoz());
				apLvalConstant[iTinecon] = LLVMDIBuilderCreateEnumerator(pDib, strPunyName.PCoz(), nValue);
			}

			auto strPunyName = StrPunyEncode(pTinenum->m_strName.PCoz());
			
			u64 cBitSize, cBitAlign;
			auto pLtypeLoose = pBuild->PLtypeFromPTin(pTinenum->m_pTinLoose);
			CalculateSizeAndAlign(pBuild, pLtypeLoose, &cBitSize, &cBitAlign);
			pTin->m_pCgvalDIType = LLVMDIBuilderCreateEnumerationType(
								    pDib, 
									pLvalScope, 
									strPunyName.PCoz(), 
									pDif->m_pLvalFile,
								    iLine,
									cBitSize,
									cBitAlign,
								    apLvalConstant, 
									(u32)cTinecon,
									(LLVMValueRef)pTinenum->m_pTinLoose->m_pCgvalDIType);
		} break;
	case TINK_Literal:
	case TINK_Void:
		break;
	default: break;
	}
	EWC_ASSERT(pTin->m_pCgvalDIType != nullptr, "unhandled type info kind in debug info");
}

static inline void CreateDebugInfo(CWorkspace * pWork, BCode::CBuilder * pBuild, CSTNode * pStnodRef, STypeInfo * pTin)
{
}


void TokenizeTripleString(char * pChzTripleCopy, char ** ppChzArch, char ** ppChzVendor, char ** ppChzOs, char **ppChzEnv)
{
	char * apCh[3];
	for (char ** ppCh = apCh; ppCh != EWC_PMAC(apCh); ++ppCh)
		*ppCh = nullptr;

	int ipCh = 0;
	for (char * pCh = pChzTripleCopy; *pCh != '\0'; ++pCh)
	{
		if (*pCh == '-')
		{
			*pCh++ = '\0';

			if (*pCh != '\0')
			{
				apCh[ipCh++] = pCh;
				if (ipCh >= EWC_DIM(apCh))
					break;
			}
		}
	}

	if (ppChzArch)	*ppChzArch = pChzTripleCopy;
	if (ppChzVendor)*ppChzVendor = apCh[0];
	if (ppChzOs)	*ppChzOs = apCh[1];
	if (ppChzEnv)	*ppChzEnv = apCh[2];
}



CBuilderBase::CBuilderBase(CWorkspace * pWork)
:m_pAlloc(pWork->m_pAlloc)
,m_pBerrctx(nullptr)
	{ ; }
					


// Builder class Methods
CBuilderIR::CBuilderIR(CWorkspace * pWork, const char * pChzFilename, GRFCOMPILE grfcompile)
:CBuilderBase(pWork)
,m_pLmoduleCur(nullptr)
,m_pLbuild(nullptr)
,m_pTargd (nullptr)
,m_pDib(nullptr)
,m_pLvalCompileUnit(nullptr)
,m_pLvalScope(nullptr)
,m_pLvalFile(nullptr)
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
,m_arypProcVerify(pWork->m_pAlloc, EWC::BK_CodeGen)
,m_parypValManaged(&pWork->m_arypValManaged)
,m_aryJumptStack(pWork->m_pAlloc, EWC::BK_CodeGen)
,m_hashPSymPVal(pWork->m_pAlloc, BK_CodeGen, 256)
,m_hashPTinstructPCgstruct(pWork->m_pAlloc, BK_CodeGen,32)
{ 
	CAlloc * pAlloc = pWork->m_pAlloc;

	if (!pChzFilename || *pChzFilename == '\0')
	{
		pChzFilename = "stub";
	}

	size_t cBFilename = CCh(pChzFilename) + 1;
	char * pCozCopy = (char *)alloca(sizeof(char) * cBFilename);
	EWC::SStringBuffer strbuf(pCozCopy, cBFilename);
	AppendCoz(&strbuf, pChzFilename);

	const char * pCozPath;	
	const char * pCozFile;	
	PathSplitDestructive(pCozCopy, cBFilename, &pCozPath, &pCozFile, nullptr);


	for (int intfunk = 0; intfunk < INTFUNK_Max; ++intfunk)
	{
		m_mpIntfunkPLval[intfunk] = nullptr;
	}

	LLVMTarget * pLtarget = nullptr;
	//const char * pChzTriple = "x86_64-pc-windows-msvc"; //LLVMGetTarget(pLmodule);
	//const char * pChzTriple = "i686-pc-windows-msvc"; //LLVMGetTarget(pLmodule);
	char * pChzTriple = LLVMGetDefaultTargetTriple();

	{
		size_t cBTriple = CBCoz(pChzTriple);
		char * pChzTripleCopy = (char*)pAlloc->EWC_ALLOC_TYPE_ARRAY(char, cBTriple);
		EWC::SStringBuffer strbuf(pChzTripleCopy, cBTriple);
		AppendCoz(&strbuf, pChzTriple);

		char * pChzOs;
		TokenizeTripleString(pChzTripleCopy, nullptr, nullptr, &pChzOs, nullptr);
		pWork->m_targetos = (FAreCozEqual(pChzOs, "windows")) ? TARGETOS_Windows : TARGETOS_Nil;
		
		pAlloc->EWC_DELETE(pChzTripleCopy);
	}

	char * pChzError = nullptr;
	LLVMBool fFailed = LLVMGetTargetFromTriple(pChzTriple, &pLtarget, &pChzError);
	if (fFailed)
	{
		EWC_ASSERT(false, "Error generating llvm target. (triple = %s)\n%s", pChzTriple, pChzError);
		LLVMDisposeMessage(pChzError);
		return;
	}

	LLVMCodeGenOptLevel loptlevel = LLVMCodeGenLevelDefault;
	switch (pWork->m_optlevel)
	{
	default:				EWC_ASSERT(false, "unknown optimization level"); // fall through
	case OPTLEVEL_Debug:	loptlevel = LLVMCodeGenLevelNone;			break; // -O0
	case OPTLEVEL_Release:	loptlevel = LLVMCodeGenLevelAggressive;		break; // -O2
	}

	#if !_WINDOWS
	// BB - Mac builds want position independent relocation, should be a command line arg
	LLVMRelocMode lrelocmode = LLVMRelocPIC;
	#else
	LLVMRelocMode lrelocmode = LLVMRelocDefault;
	#endif
	LLVMCodeModel lcodemodel = LLVMCodeModelDefault;

	// NOTE: llvm-c doesn't expose functions to query these, but it seems to just return the empty string.
	//  This may not work for all target backends.
	const char * pChzCPU = "";
	const char * pChzFeatures = "";

	m_pLtmachine = LLVMCreateTargetMachine(pLtarget, pChzTriple, pChzCPU, pChzFeatures, loptlevel, lrelocmode, lcodemodel);
	if (grfcompile.FIsSet(FCOMPILE_FastIsel))
	{
		SetUseFastIsel(m_pLtmachine);
	}

	m_pLbuild = LLVMCreateBuilder();
	m_pLmoduleCur = LLVMModuleCreateWithName("MoeModule");

#if 0
#if EWC_X64
	const char * pChzDataLayout = "p:64:64";
#else
	const char * pChzDataLayout = "e-m:x-p:32:32-i64:64-f80:32-n8:16:32-a:0:32-S32";
#endif

	 LLVMSetDataLayout(m_pLmoduleCur, pChzDataLayout);

	m_pTargd = LLVMCreateTargetData(pChzDataLayout);
#else

	m_pTargd = LLVMCreateTargetDataLayout(m_pLtmachine);
	LLVMSetTarget(m_pLmoduleCur, pChzTriple);
	LLVMSetModuleDataLayout(m_pLmoduleCur, m_pTargd);
	
#endif

	m_pDib = LLVMCreateDIBuilder(m_pLmoduleCur);
	m_nRuntimeLanguage = llvm::dwarf::DW_LANG_C;
	
	/*CWorkspace::SFile * pFile = pWork->PFileLookup(pChzFilename, CWorkspace::FILEK_Source);
	EWC_ASSERT(pFile, "failed to find source CWorkspace::SFile");
	
	if (!pFile->m_pDif)
	{
		pFile->m_pDif = PDifEnsure(pWork, this, pChzFilename);
	}*/
	
	auto pDif = PDifEnsure(pWork, this, pChzFilename);
	if (EWC_FVERIFY(pDif, "FAILED creating debug file"))
	{
		m_pLvalCompileUnit = LLVMDIBuilderCreateCompileUnit(
								m_pDib,
								m_nRuntimeLanguage,
								pDif->m_pLvalFile,
								"Moe Compiler",		// pChzProducer
								false,				// fIsOptimized
								"",					// pChzFlags
								0);					// nRuntimeVersion
	}

	m_pLvalScope = m_pLvalCompileUnit;
}

void CBuilderIR::AddManagedVal(CIRValue * pVal)
{
	m_parypValManaged->Append(pVal);
}
	
CBuilderIR::~CBuilderIR()
{
	if (m_pLbuild)
	{
		LLVMDisposeBuilder(m_pLbuild);
		m_pLbuild = nullptr;
	}

	if (m_pLmoduleCur)
	{

		LLVMDisposeModule(m_pLmoduleCur);
		m_pLmoduleCur = nullptr;
	}

	if (m_pTargd)
	{
		LLVMDisposeTargetData(m_pTargd);
		m_pTargd = nullptr;
	}

	if (m_pLtmachine)
	{
		LLVMDisposeTargetMachine(m_pLtmachine);
	}

	if (m_pDib)
	{
		LLVMDisposeDIBuilder(m_pDib);
		m_pDib = nullptr;
	}
}

void CBuilderIR::PrintDump()
{
	if (!m_pLmoduleCur)
	{
		printf("No code generated.");
		return;
	}

	LLVMDumpModule(m_pLmoduleCur);
}

CIRInstruction * CBuilderIR::PInstCreateRaw(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	if (!EWC_FVERIFY(m_pBlockCur, "creating instruction with no active block"))
		return nullptr;

	if (m_pBlockCur->m_fIsTerminated)
	{
		if (irop != IROP_Branch && EWC_FVERIFY(m_pBerrctx, "trying to throw warning with no error context"))
		{
			EmitWarning(m_pBerrctx->m_pErrman, m_pBerrctx->m_pLexloc, ERRID_UnreachableInst, "Unreachable instruction detected");
		}
		irop = IROP_Error;
		pValLhs = nullptr;
		pValRhs = nullptr;
	}

	int cpValOperand = COperand(irop);
	size_t cB = sizeof(CIRInstruction) + ewcMax((cpValOperand - 1), 0) * sizeof(CIRValue *);

	CIRInstruction * pInst = (CIRInstruction *)m_pAlloc->EWC_ALLOC(cB, EWC_ALIGN_OF(CIRInstruction));
	new (pInst) CIRInstruction(irop);
	AddManagedVal(pInst);

	pInst->m_apValOperand[0] = pValLhs;
	if (cpValOperand > 1)
	{
		CIRValue ** ppValOperand = pInst->m_apValOperand;
		ppValOperand[1] = pValRhs;

	}
	else
	{
		EWC_ASSERT(pValRhs == nullptr, "unexpected second operand");
	}
	pInst->m_cpValOperand = cpValOperand;

	if (irop != IROP_Error)
	{
		m_pBlockCur->Append(pInst);
	}
	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreatePhi(LLVMOpaqueType * pLtype, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_Phi, nullptr, nullptr, pChzName);
	if (FIsError(pInst))
		return pInst;

	pInst->m_pLval = LLVMBuildPhi(m_pLbuild, pLtype, OPNAME(pChzName));
	return pInst;
}

void CBuilderIR::AddPhiIncoming(CIRInstruction * pInstPhi, CIRValue * pVal, CIRBlock * pBlock)
{
	LLVMAddIncoming(pInstPhi->m_pLval, &pVal->m_pLval, &pBlock->m_pLblock, 1);
}

CIRInstruction * CBuilderIR::PInstCreateCall(LValue * pLvalProc, LLVMTypeRef pLtypeProc, ProcArg ** apLvalArgs, unsigned cArg)
{
	auto pInst = PInstCreateRaw(IROP_Call, nullptr, nullptr, "RetTmp");
	if (FIsError(pInst))
		return pInst;

	pInst->m_pLval = LLVMBuildCall(m_pLbuild, pLvalProc, apLvalArgs, cArg, "");
	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreatePtrToInt(CIRValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_PtrToInt, pValOperand, nullptr, pChzName);
	if (FIsError(pInst))
		return pInst;

	auto pLtypeDst = PLtypeFromPTin(pTinint);
	pInst->m_pLval = LLVMBuildPtrToInt(m_pLbuild, pValOperand->m_pLval, pLtypeDst, OPNAME(pChzName));
	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreate(IROP irop, CIRValue * pValOperand, const char * pChzName)
{
	// Unary Ops
	CIRInstruction * pInst = PInstCreateRaw(irop, pValOperand, nullptr, pChzName);
	if (FIsError(pInst))
		return pInst;

	switch (irop)
	{
	case IROP_Ret:
	{
		if (pValOperand)	pInst->m_pLval = LLVMBuildRet(m_pLbuild, pValOperand->m_pLval);
		else				pInst->m_pLval = LLVMBuildRetVoid(m_pLbuild);
	} break;

	case IROP_NNeg:		pInst->m_pLval = LLVMBuildNeg(m_pLbuild, pValOperand->m_pLval, OPNAME(pChzName)); break;
	case IROP_GNeg:		pInst->m_pLval = LLVMBuildFNeg(m_pLbuild, pValOperand->m_pLval, OPNAME(pChzName)); break;
	case IROP_Not:		pInst->m_pLval = LLVMBuildNot(m_pLbuild, pValOperand->m_pLval, OPNAME(pChzName)); break;
	case IROP_Load:		pInst->m_pLval = LLVMBuildLoad(m_pLbuild, pValOperand->m_pLval, OPNAME(pChzName)); break;
	default: EWC_ASSERT(false, "%s is not a unary opcode supported by PInstCreate", PChzFromIrop(irop)); break;
	}

	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	// Binary Ops
	CIRInstruction * pInst = PInstCreateRaw(irop, pValLhs, pValRhs, pChzName);
	if (FIsError(pInst))
		return pInst;

	switch (irop)
	{
	case IROP_NAdd:		pInst->m_pLval = LLVMBuildAdd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_GAdd:		pInst->m_pLval = LLVMBuildFAdd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_NSub:		pInst->m_pLval = LLVMBuildSub(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_GSub:		pInst->m_pLval = LLVMBuildFSub(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_NMul:		pInst->m_pLval = LLVMBuildMul(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_GMul:		pInst->m_pLval = LLVMBuildFMul(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_SDiv:		pInst->m_pLval = LLVMBuildSDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_UDiv:		pInst->m_pLval = LLVMBuildUDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_GDiv:		pInst->m_pLval = LLVMBuildFDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_SRem:		pInst->m_pLval = LLVMBuildSRem(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_URem:		pInst->m_pLval = LLVMBuildURem(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_GRem:		pInst->m_pLval = LLVMBuildFRem(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_Shl:		pInst->m_pLval = LLVMBuildShl(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_AShr:		pInst->m_pLval = LLVMBuildAShr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_LShr:		pInst->m_pLval = LLVMBuildLShr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_And:		pInst->m_pLval = LLVMBuildAnd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName)); break;
	case IROP_Or:		pInst->m_pLval = LLVMBuildOr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName));	break;
	case IROP_Xor:		pInst->m_pLval = LLVMBuildXor(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName));	break;
	default: EWC_ASSERT(false, "%s is not a binary opcode supported by PInstCreate", PChzFromIrop(irop)); break;
	}

	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreateCast(IROP irop, CIRValue * pValLhs, STypeInfo * pTinDst, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(irop, pValLhs, nullptr, pChzName);
	if (FIsError(pInst))
		return pInst;

	auto pLtypeDst = PLtypeFromPTin(pTinDst);

	switch (irop)
	{
	case IROP_NTrunc:		pInst->m_pLval = LLVMBuildTrunc(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_SignExt:		pInst->m_pLval = LLVMBuildSExt(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_ZeroExt:		pInst->m_pLval = LLVMBuildZExt(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_GToS:			pInst->m_pLval = LLVMBuildFPToSI(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_GToU:			pInst->m_pLval = LLVMBuildFPToUI(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_SToG:			pInst->m_pLval = LLVMBuildSIToFP(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_UToG:			pInst->m_pLval = LLVMBuildUIToFP(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_GTrunc:		pInst->m_pLval = LLVMBuildFPTrunc(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_GExtend:		pInst->m_pLval = LLVMBuildFPExt(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	case IROP_Bitcast:		pInst->m_pLval = LLVMBuildBitCast(m_pLbuild, pValLhs->m_pLval, pLtypeDst, OPNAME(pChzName)); break;
	default: EWC_ASSERT(false, "IROP not supported by PInstCreateCast"); break;
	}

	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreateCondBranch(
	CIRValue * pValPred,
	CIRBlock * pBlockTrue,
	CIRBlock * pBlockFalse)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_CondBranch, pValPred, nullptr, "branch");
	if (FIsError(pInst))
		return pInst;

	auto pLblockFalse = (pBlockFalse == nullptr) ? nullptr : pBlockFalse->m_pLblock;
	pInst->m_pLval = LLVMBuildCondBr(m_pLbuild, pValPred->m_pLval, pBlockTrue->m_pLblock, pLblockFalse);
	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreateNCmp(
	NPRED npred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
#define MOE_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMIntPredicate s_mpNcmpredLpredicate[] =
	{
		NPRED_LIST
	};
#undef MOE_PRED
#undef LLVM_PRED
	EWC_CASSERT(EWC_DIM(s_mpNcmpredLpredicate) == NPRED_Max, "missing elements in int predicate map");
	auto lpredicate = s_mpNcmpredLpredicate[npred];

	CIRInstruction * pInst = PInstCreateRaw(IROP_NCmp, pValLhs, pValRhs, pChzName);
	if (FIsError(pInst))
		return pInst;

	pInst->m_pLval = LLVMBuildICmp(m_pLbuild, lpredicate, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName));
	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreateGCmp(
	GPRED gpred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
#define MOE_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMRealPredicate s_mpGcmpredLpredicate[] =
	{
		GPRED_LIST
	};
#undef MOE_PRED
#undef LLVM_PRED
	EWC_CASSERT(EWC_DIM(s_mpGcmpredLpredicate) == GPRED_Max, "missing elements in int predicate map");
	auto lpredicate = s_mpGcmpredLpredicate[gpred];

	CIRInstruction * pInst = PInstCreateRaw(IROP_GCmp, pValLhs, pValRhs, pChzName);
	if (FIsError(pInst))
		return pInst;

	pInst->m_pLval = LLVMBuildFCmp(m_pLbuild, lpredicate, pValLhs->m_pLval, pValRhs->m_pLval, OPNAME(pChzName));
	return pInst;
}

void CBuilderIR::CreateBranch(CIRBlock * pBlock)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_Branch, nullptr, nullptr, "branch");
	if (FIsError(pInst))
		return;

	pInst->m_pLval = LLVMBuildBr(m_pLbuild, pBlock->m_pLblock);
}

void CBuilderIR::CreateReturn(CIRValue ** ppVal, int cpVal, const char * pChzName)
{
	EWC_ASSERT(cpVal <= 1, "multiple returns not supported");

	if (cpVal == 0)
	{
		(void) PInstCreate(IROP_Ret, nullptr, pChzName);
	}
	else
	{
		(void) PInstCreate(IROP_Ret, *ppVal, pChzName);
	}
}

CIRGlobal * CBuilderIR::PGlobCreate(LLVMOpaqueType * pLtype, const char * pChzName)
{
	CIRGlobal * pGlob = EWC_NEW(m_pAlloc, CIRGlobal) CIRGlobal();

	pGlob->m_pLval = LLVMAddGlobal(m_pLmoduleCur, pLtype, pChzName);
	AddManagedVal(pGlob);
	return pGlob;
}

CIRValue * CBuilderIR::PValCreateAlloca(LLVMOpaqueType * pLtype, u64 cElement, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_Alloca, nullptr, nullptr, pChzName);
	if (FIsError(pInst))
		return pInst;

	if (cElement > 1)
	{
		auto pLvalCElement = LLVMConstInt(LLVMInt64Type(), cElement, false);
		pInst->m_pLval = LLVMBuildArrayAlloca(m_pLbuild, pLtype, pLvalCElement, OPNAME(pChzName));
	}
	else
	{
		pInst->m_pLval = LLVMBuildAlloca(m_pLbuild, pLtype, OPNAME(pChzName));
	}

	return pInst;
}

CIRInstruction * CBuilderIR::PInstCreateGEP(
	CIRValue * pValLhs,
	LLVMOpaqueValue ** apLvalIndices,
	u32 cpIndices,
	const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_GEP, nullptr, nullptr, pChzName);
	if (FIsError(pInst))
		return pInst;

	pInst->m_pLval = LLVMBuildGEP(m_pLbuild, pValLhs->m_pLval, apLvalIndices, cpIndices, OPNAME(pChzName));
	return pInst;
}

LLVMOpaqueValue * CBuilderIR::PGepIndex(u64 idx)
{
	return LLVMConstInt(LLVMInt32Type(), idx, false);
}

LLVMOpaqueValue * CBuilderIR::PGepIndexFromValue(CIRValue * pVal)
{
	return pVal->m_pLval;
}

CIRValue * CBuilderIR::PValFromSymbol(SSymbol * pSym)
{
	auto ppVal = m_hashPSymPVal.Lookup(pSym);
	if (!ppVal)
		return nullptr;

	auto pVal = *ppVal;

	if (pVal->m_valk == VALK_Instruction)
	{
		auto pInstSym = (CIRInstruction *)pVal;
		if (!EWC_FVERIFY(pInstSym->m_irop = IROP_Alloca, "expected alloca for symbol"))
			return nullptr;
	}

	return pVal;
}

void CBuilderIR::SetSymbolValue(SSymbol * pSym, CIRValue * pVal)
{
	auto fins = m_hashPSymPVal.FinsEnsureKeyAndValue(pSym, pVal);
	EWC_ASSERT(fins == FINS_Inserted, "multiple values for symbol");
}

CBuilderIR::SCodeGenStruct * CBuilderIR::PCgstructEnsure(STypeInfoStruct * pTinstruct)
{
	SCodeGenStruct ** ppCgstruct = nullptr;
	auto fins = m_hashPTinstructPCgstruct.FinsEnsureKey(pTinstruct, &ppCgstruct);
	if (fins == FINS_Inserted)
	{
		*ppCgstruct = EWC_NEW(m_pAlloc, SCodeGenStruct) SCodeGenStruct();
	}

	return *ppCgstruct;
}

CIRInstruction * CBuilderIR::PInstCreateStore(CIRValue * pValPT, CIRValue * pValT)
{
	//store t into address pointed at by pT

	CIRInstruction * pInstStore = PInstCreateRaw(IROP_Store, pValPT, pValT, "store");
	if (FIsError(pInstStore))
		return pInstStore;

	auto pLtypeT = LLVMTypeOf(pValT->m_pLval);
	auto pLtypePT = LLVMTypeOf(pValPT->m_pLval);
	bool fIsPointerKind = LLVMGetTypeKind(pLtypePT) == LLVMPointerTypeKind;
	bool fTypesMatch = false;
	if (fIsPointerKind)
	{
		auto pLtypeElem = LLVMGetElementType(pLtypePT);
		fTypesMatch = pLtypeElem == pLtypeT;
	}
	if (!fIsPointerKind || !fTypesMatch)
	{
#if LLVM_ENABLE_DUMP
		printf("(src) pLtypeT :"); LLVMDumpType(pLtypeT);
		printf("(dst) pLtypePT:)"); LLVMDumpType(pLtypePT);
#endif
		EmitError(m_pBerrctx->m_pErrman, m_pBerrctx->m_pLexloc, ERRID_BadStore, "bad store information\n");
	}

#if WARN_ON_LARGE_STORE
	u64 cBitSize;
	u64 cBitAlign;
	CalculateSizeAndAlign(this, pLtypeT, &cBitSize, &cBitAlign);
	if (cBitSize > 64)
	{
		printf("Warning - large store: %lld bytes\n", cBitSize / 8);
	}
#endif

    pInstStore->m_pLval = LLVMBuildStore(m_pLbuild, pValT->m_pLval, pValPT->m_pLval);
	return pInstStore;
}

bool FExtractNumericInfo(STypeInfo * pTin, u32 * pCBit, bool * pFSigned)
{
	// BB - Is there really any good reason not to make one type info for ints, bools and floats?
	switch (pTin->m_tink)
	{
	case TINK_Flag:
	case TINK_Bool:
		{
			*pCBit = 1;	
			*pFSigned = false;
		} break;
	case TINK_Integer:
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			*pCBit = pTinint->m_cBit;	
			*pFSigned = pTinint->m_fIsSigned;
		} break;
	case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTin;
			*pCBit = pTinfloat->m_cBit;	
			*pFSigned = true;
		} break;
	default: return false;
	}
	return true;
}

inline LLVMOpaqueValue * CBuilderIR::PLvalConstantInt(u64 nUnsigned, int cBit, bool fIsSigned)
{
	switch (cBit)
	{
	case 1:	 return LLVMConstInt(LLVMInt1Type(), nUnsigned != 0, fIsSigned);
	case 8:	 return LLVMConstInt(LLVMInt8Type(), U8Coerce(nUnsigned & 0xFF), fIsSigned);
	case 16: return LLVMConstInt(LLVMInt16Type(), U16Coerce(nUnsigned & 0xFFFF), fIsSigned);
	case 32: return LLVMConstInt(LLVMInt32Type(), U32Coerce(nUnsigned & 0xFFFFFFFF), fIsSigned);
	case -1: // fall through
	case 64: return LLVMConstInt(LLVMInt64Type(), nUnsigned, fIsSigned);
	default: EWC_ASSERT(false, "unhandled integer size");
		return nullptr;
	}
}

inline CIRConstant * CBuilderIR::PConstInt(u64 nUnsigned, int cBit, bool fIsSigned)
{
	CIRConstant * pConst = EWC_NEW(m_pAlloc, CIRConstant) CIRConstant();
	AddManagedVal(pConst);
	pConst->m_pLval = PLvalConstantInt(nUnsigned, cBit, fIsSigned);

	return pConst;
}

LLVMOpaqueValue * CBuilderIR::PLvalConstantFloat(f64 g, int cBit)
{
	switch (cBit)
	{
	case 32: return LLVMConstReal(LLVMFloatType(), g);
	case 64: return LLVMConstReal(LLVMDoubleType(), g);
	default:	EWC_ASSERT(false, "unhandled float size");
		return nullptr;
	}
}

LLVMOpaqueValue * CBuilderIR::PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName)
{
	return LLVMGlobalStringPtr(m_pLbuild, m_pLmoduleCur, pChzString, pChzName);
}

CBuilderIR::LValue * CBuilderIR::PLvalConstantNull(LType * pLtype)
{
	return LLVMConstNull(pLtype);
}

CBuilderIR::LValue * CBuilderIR::PLvalConstantArray(LType * pLtypeElem, LValue ** apLval, u32 cpLval)
{
	return LLVMConstArray(pLtypeElem, apLval, cpLval);
}


CIRConstant * CBuilderIR::PConstFloat(f64 g, int cBit)
{
	CIRConstant * pConst = EWC_NEW(m_pAlloc, CIRConstant) CIRConstant();
	AddManagedVal(pConst);
	pConst->m_pLval = PLvalConstantFloat(g, cBit); 

	return pConst;
}

LLVMOpaqueValue * PLvalFromEnumConstant(CBuilderIR * pBuild, STypeInfo * pTinLoose, CSTValue * pStval)
{
	auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinLoose);
	if (!EWC_FVERIFY(pTinint, "expected integer type for enum constant"))
		return nullptr;

	return pBuild->PLvalConstantInt(pStval->m_nUnsigned, pTinint->m_cBit, pTinint->m_fIsSigned);
}

CIRConstant * CBuilderIR::PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval)
{
	auto pLval = PLvalFromEnumConstant(this, pTinenum->m_pTinLoose, pStval);
	if (!pLval)
		return nullptr;
	
	auto pConst = EWC_NEW(m_pAlloc, CIRConstant) CIRConstant();
	AddManagedVal(pConst);
	pConst->m_pLval = pLval;
	return pConst;
}

LLVMOpaqueValue * PLvalZeroInType(CBuilderIR * pBuild, STypeInfo * pTin)
{
	switch (pTin->m_tink)
	{
	case TINK_Bool:		return LLVMConstInt(LLVMInt1Type(), 0, false);
	case TINK_Integer:	return CBuilderIR::PLvalConstantInt(0, ((STypeInfoInteger *)pTin)->m_cBit, false);
	case TINK_Float:	return CBuilderIR::PLvalConstantFloat(0.0f, ((STypeInfoFloat *)pTin)->m_cBit);
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			return PLvalZeroInType(pBuild, pTinenum->m_pTinLoose);
		} 
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			return PLvalZeroInType(pBuild, pTinqual->m_pTin);
		}
	case TINK_Pointer:
	case TINK_Procedure:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTin;
			auto * pLtype = pBuild->PLtypeFromPTin(pTinptr);
			if (pLtype)
				return LLVMConstNull(pLtype);
		} break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
			LLVMOpaqueType * pLtypeElement = pBuild->PLtypeFromPTin(pTinary->m_pTin);
			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
			{
				(void) PLvalZeroInType(pBuild, pTinary->m_pTin);

				auto apLval = (LLVMValueRef *)pBuild->m_pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, (size_t)pTinary->m_c);

				auto pLvalZero = PLvalZeroInType(pBuild, pTinary->m_pTin);
				EWC_ASSERT(pLvalZero != nullptr, "expected zero value");

				for (s64 iElement = 0; iElement < pTinary->m_c; ++iElement)
				{
					apLval[iElement] = pLvalZero;
				}

				auto pLvalReturn = LLVMConstArray(pLtypeElement, apLval, u32(pTinary->m_c));
				pBuild->m_pAlloc->EWC_DELETE(apLval);
				return pLvalReturn;
			}
			case ARYK_Reference:
			{
				LLVMOpaqueValue * apLvalMember[2]; // pointer, count
				apLvalMember[ARYMEMB_Count] = LLVMConstInt(LLVMInt64Type(), 0, false);
				apLvalMember[ARYMEMB_Data] = LLVMConstNull(LLVMPointerType(pLtypeElement, 0));

				return LLVMConstStruct(apLvalMember, EWC_DIM(apLvalMember), false);
			}
			default: EWC_ASSERT(false, "Unhandled ARYK");
			}
		}
	case TINK_Struct:
	{
		auto pTinstruct = (STypeInfoStruct *)pTin;

		int cpLvalField = (int)pTinstruct->m_aryTypemembField.C();
		size_t cB = sizeof(LLVMOpaqueValue *) * cpLvalField;
		auto apLvalMember = (LLVMOpaqueValue **)(alloca(cB));

		for (int ipLval = 0; ipLval < cpLvalField; ++ipLval)
		{
			apLvalMember[ipLval] = PLvalZeroInType(pBuild, pTinstruct->m_aryTypemembField[ipLval].m_pTin);
		}

		auto pCgstruct = pBuild->PCgstructEnsure(pTinstruct);
		return LLVMConstNamedStruct(pCgstruct->m_pLtype, apLvalMember, cpLvalField);
	}

	default: break;
	}

	return nullptr;
}

BCode::SValue * PLvalZeroInType(BCode::CBuilder * pBuild, STypeInfo * pTin)
{
	switch (pTin->m_tink)
	{
	case TINK_Bool:		return pBuild->PConstInt(0, 8, false);
	case TINK_Integer:	return pBuild->PConstInt(0, ((STypeInfoInteger *)pTin)->m_cBit, false);
	case TINK_Float:	return pBuild->PConstFloat(0.0f, ((STypeInfoFloat *)pTin)->m_cBit);
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			return PLvalZeroInType(pBuild, pTinenum->m_pTinLoose);
		} 
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			return PLvalZeroInType(pBuild, pTinqual->m_pTin);
		}
	case TINK_Pointer:
	case TINK_Procedure:
		{
			return pBuild->PConstPointer(nullptr);
		}
	case TINK_Array:
		{
			EWC_ASSERT(false, "bytecode tbd");
			return false;
			/*
			auto pTinary = (STypeInfoArray *)pTin;
			LLVMOpaqueType * pLtypeElement = pBuild->PLtypeFromPTin(pTinary->m_pTin);
			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
			{
				(void) PLvalZeroInType(pBuild, pTinary->m_pTin);

				auto apLval = (LLVMValueRef *)pBuild->m_pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, (size_t)pTinary->m_c);

				auto pLvalZero = PLvalZeroInType(pBuild, pTinary->m_pTin);
				EWC_ASSERT(pLvalZero != nullptr, "expected zero value");

				for (s64 iElement = 0; iElement < pTinary->m_c; ++iElement)
				{
					apLval[iElement] = pLvalZero;
				}

				auto pLvalReturn = LLVMConstArray(pLtypeElement, apLval, u32(pTinary->m_c));
				pBuild->m_pAlloc->EWC_DELETE(apLval);
				return pLvalReturn;
			}
			case ARYK_Reference:
			{
				LLVMOpaqueValue * apLvalMember[2]; // pointer, count
				apLvalMember[ARYMEMB_Count] = LLVMConstInt(LLVMInt64Type(), 0, false);
				apLvalMember[ARYMEMB_Data] = LLVMConstNull(LLVMPointerType(pLtypeElement, 0));

				return LLVMConstStruct(apLvalMember, EWC_DIM(apLvalMember), false);
			}
			default: EWC_ASSERT(false, "Unhandled ARYK");
			}*/
		}
	case TINK_Struct:
	{
		EWC_ASSERT(false, "bytecode tbd");
		return false;
		/*
		auto pTinstruct = (STypeInfoStruct *)pTin;

		int cpLvalField = (int)pTinstruct->m_aryTypemembField.C();
		size_t cB = sizeof(LLVMOpaqueValue *) * cpLvalField;
		auto apLvalMember = (LLVMOpaqueValue **)(alloca(cB));

		for (int ipLval = 0; ipLval < cpLvalField; ++ipLval)
		{
			apLvalMember[ipLval] = PLvalZeroInType(pBuild, pTinstruct->m_aryTypemembField[ipLval].m_pTin);
		}

		return LLVMConstNamedStruct((LLVMTypeRef)pTinstruct->m_pCgtype, apLvalMember, cpLvalField);
		*/
	}

	default: break;
	}

	return nullptr;
}

CIRConstant * PConstZeroInType(CBuilderIR * pBuild, STypeInfo * pTin)
{
	CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
	pBuild->AddManagedVal(pConst);

	pConst->m_pLval = PLvalZeroInType(pBuild, pTin);
	return pConst;
}

BCode::CBuilder::Constant * PConstZeroInType(BCode::CBuilder * pBuild, STypeInfo * pTin)
{
	auto pVal = PLvalZeroInType(pBuild, pTin);
	EWC_ASSERT(pVal->m_valk == VALK_Constant, "non-constant returned from PValZeroInType()");

	return (BCode::SConstant *)pVal;
}

struct SReflectTableEntry // reftent
{
	s32					m_ipTinNative;
	s32					m_cAliasChild;

	s32					m_ipTin;
};

struct SReflectGlobalTable	// reftab
{
public:
								SReflectGlobalTable(CAlloc * pAlloc)
								:m_mpPTinReftent(pAlloc, EWC::BK_ReflectTable, 256)
									{ ; }

	CHash<STypeInfo *, SReflectTableEntry>
								m_mpPTinReftent;
};

LLVMOpaqueValue * PLvalCreateReflectTin(CBuilderIR * pBuild, LLVMOpaqueType * pLtypeTin, STypeInfo * pTin, SReflectGlobalTable * pReftab)
{
	SReflectTableEntry * pReftent = pReftab->m_mpPTinReftent.Lookup(pTin);
	if (!EWC_FVERIFY(pReftent, "missing reflect table entry"))
		return nullptr;

	LLVMOpaqueValue * apLval[4];
	apLval[0] = LLVMGlobalStringPtr(pBuild->m_pLbuild, pBuild->m_pLmoduleCur, pTin->m_strName.PCoz(), "pCozName");	// m_pCozName
	apLval[1] = LLVMConstInt(LLVMInt8Type(), pTin->m_tink, true);	// m_tink
	apLval[2] = LLVMConstInt(LLVMInt32Type(), pReftent->m_ipTinNative, true);	// m_ipTinNative
	apLval[3] = LLVMConstInt(LLVMInt32Type(), pReftent->m_cAliasChild, true);	// m_cAliasTypeMax

	return LLVMConstNamedStruct(pLtypeTin, apLval, EWC_DIM(apLval));
}

LLVMOpaqueType * PLtypeForTypeInfo(CWorkspace * pWork, CBuilderIR * pBuild, const char * pChzTinName)
{
	SSymbol * pSymTin = nullptr;
	if (pChzTinName)
	{
		pSymTin = pWork->m_pSymtab->PSymLookup(pChzTinName, SLexerLocation());
	}

	if (pSymTin && pSymTin->m_pTin)
	{
		return pBuild->PLtypeFromPTin(pSymTin->m_pTin);
	}

	EWC_ASSERT(false, "Failed to lookup type info structure");
	return nullptr;
}

LLVMOpaqueType * PLtypeSizeInt(CBuilderIR * pBuild)
{
	return LLVMInt64Type();
}

LLVMOpaqueValue * PLvalBuildConstantGlobalArrayRef(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	LLVMOpaqueType * pLtypeElem, 
	LLVMOpaqueValue ** apLvalElem,
	u32 cElem)
{
	auto pLtypeArray = LLVMArrayType(pLtypeElem, cElem);

	auto pLvalGlobal = LLVMAddGlobal(pBuild->m_pLmoduleCur, pLtypeArray, "");
	LLVMSetGlobalConstant(pLvalGlobal, true);

	auto pLvalArrayInit = LLVMConstArray(pLtypeElem, apLvalElem, cElem);
	LLVMSetInitializer(pLvalGlobal, pLvalArrayInit);

	LLVMOpaqueValue * apLvalMember[2]; // count, pointer
	apLvalMember[ARYMEMB_Count] = LLVMConstInt(PLtypeSizeInt(pBuild), cElem, false);

	auto pLtypePElem = LLVMPointerType(pLtypeElem, 0);
	auto pLvalCast = LLVMConstPointerCast(pLvalGlobal, pLtypePElem);
	apLvalMember[ARYMEMB_Data] = pLvalCast;

	return LLVMConstStruct(apLvalMember, EWC_DIM(apLvalMember), false);
}

LLVMOpaqueValue * PLvalEnsureReflectStruct(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	LLVMOpaqueType * pLtypeTin,
	LLVMOpaqueType * pLtypePTin,
	STypeInfo * pTin,
	SReflectGlobalTable * pReftab)
{
	if ((LLVMValueRef)pTin->m_pCgvalReflectGlobal)
	{
		return LLVMConstPointerCast((LLVMValueRef)pTin->m_pCgvalReflectGlobal, pLtypePTin);
	}

	CDynAry<LLVMOpaqueValue *> arypLval(pBuild->m_pAlloc, BK_CodeGenReflect, 16);
	LLVMOpaqueValue * pLvalReflect = nullptr;

	LLVMOpaqueType * pLtypeTinDerived = nullptr;
	switch (pTin->m_tink)
	{
	case TINK_Integer:
		{
			auto pTinint = (STypeInfoInteger *)pTin;

			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));
			arypLval.Append(LLVMConstInt(LLVMInt1Type(), pTinint->m_fIsSigned, false));
			arypLval.Append(LLVMConstInt(LLVMInt32Type(), pTinint->m_cBit, false));

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoInteger");
			pLvalReflect =  LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Float:
		{
			auto pTinfloat = (STypeInfoFloat *)pTin;

			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));
			arypLval.Append(LLVMConstInt(LLVMInt32Type(), pTinfloat->m_cBit, false));

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoFloat");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Qualifier:
		{ 
			auto pTinqual = (STypeInfoQualifier *)pTin;
			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));

			auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);
			arypLval.Append(PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTinqual->m_pTin, pReftab));
			arypLval.Append(LLVMConstInt(LLVMInt8Type(), pTinqual->m_grfqualk.m_raw, false)); //m_grfqualk

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoQualifier");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;
			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));

			auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);
			arypLval.Append(PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTinptr->m_pTinPointedTo, pReftab));

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoPointer");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break; 
	case TINK_Struct:
		{ 
			auto pTinstruct = (STypeInfoStruct *)pTin;
			auto pLtypeStruct = pBuild->PLtypeFromPTin(pTin);

			u32 cTypememb = u32(pTinstruct->m_aryTypemembField.C());
			auto apLvalMemberArray = (LLVMOpaqueValue **)alloca(sizeof(LLVMOpaqueValue**) * cTypememb);
			auto pLtypeTinMember = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoMember");
			auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);

			for (u32 iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
			{
				auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];
				CFixAry<LLVMOpaqueValue *, 3> arypLvalMember;

				arypLvalMember.Append(LLVMGlobalStringPtr(pBuild->m_pLbuild, pBuild->m_pLmoduleCur, pTypememb->m_strName.PCoz(), "strMemb")); //m_pCozName
				arypLvalMember.Append(PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTypememb->m_pTin, pReftab));

				u64 dBMember = LLVMOffsetOfElement(pBuild->m_pTargd, pLtypeStruct, iTypememb);
				arypLvalMember.Append(LLVMConstInt(PLtypeSizeInt(pBuild), dBMember, false)); //m_iB

				apLvalMemberArray[iTypememb] = LLVMConstNamedStruct(pLtypeTinMember, arypLvalMember.A(), u32(arypLvalMember.C()));
			}

			auto pLvalAryMembers = PLvalBuildConstantGlobalArrayRef(pWork, pBuild, pLtypeTinMember, apLvalMemberArray, cTypememb);

			// build the STypeInfoStruct
			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));	//m_tin
			arypLval.Append(pLvalAryMembers);	//m_aryMembers
			arypLval.Append(LLVMGlobalStringPtr(pBuild->m_pLbuild, pBuild->m_pLmoduleCur, pTinstruct->m_strName.PCoz(), "strStruct")); //m_pCozName

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoStruct");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			u32 cTinecon = (u32)pTinenum->m_aryTinecon.C();
			auto apLvalEconArray = (LLVMOpaqueValue **)alloca(sizeof(LLVMOpaqueValue**) * cTinecon);
			auto pLtypeTinEcon = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoEnumConstant");

			for (u32 iTinecon = 0; iTinecon < cTinecon; ++iTinecon)
			{
				auto pTinecon = &pTinenum->m_aryTinecon[iTinecon];
				CFixAry<LLVMOpaqueValue *, 3> arypLvalEcon;

				u64 nUnsigned = 0;
				s64 nSigned = 0;
				if (PTinDerivedCast<STypeInfoInteger *>(pTinenum->m_pTinLoose)->m_fIsSigned)
				{
					nSigned = pTinecon->m_bintValue.S64Coerce();
				}
				else
				{
					nUnsigned = pTinecon->m_bintValue.U64Coerce();
				}

				arypLvalEcon.Append(LLVMGlobalStringPtr(pBuild->m_pLbuild, pBuild->m_pLmoduleCur, pTinecon->m_strName.PCoz(), "strStruct")); //m_pCozName
				arypLvalEcon.Append(LLVMConstInt(LLVMInt64Type(), nUnsigned, false)); // m_nUnsigned
				arypLvalEcon.Append(LLVMConstInt(LLVMInt64Type(), nSigned, true)); // m_nSigned

				apLvalEconArray[iTinecon] = LLVMConstNamedStruct(pLtypeTinEcon, arypLvalEcon.A(), u32(arypLvalEcon.C()));
			}

			auto pLvalAryEconArray = PLvalBuildConstantGlobalArrayRef(pWork, pBuild, pLtypeTinEcon, apLvalEconArray, cTinecon);

			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));	//m_tin

			auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);

			arypLval.Append(PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTinenum->m_pTinLoose, pReftab));
			arypLval.Append(LLVMGlobalStringPtr(pBuild->m_pLbuild, pBuild->m_pLmoduleCur, pTinenum->m_strName.PCoz(), "strEnum")); //m_pCozName
			arypLval.Append(pLvalAryEconArray);

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoEnum");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;

			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));	//m_tin

			auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);

			arypLval.Append(PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTinary->m_pTin, pReftab));
			arypLval.Append(LLVMConstInt(LLVMInt8Type(), pTinary->m_aryk, true)); //m_aryk
			arypLval.Append(LLVMConstInt(PLtypeSizeInt(pBuild), pTinary->m_c, true)); //m_c

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoArray");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;
			auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);

			u32 cParam = (u32)pTinproc->m_arypTinParams.C();
			auto apLvalParam = (LLVMOpaqueValue **)alloca(sizeof(LLVMOpaqueValue**) * cParam);
			for (u32 iParam = 0; iParam < cParam; ++iParam)
			{
				apLvalParam[iParam] = PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTinproc->m_arypTinParams[iParam], pReftab);
			}

			auto pLvalAryParam = PLvalBuildConstantGlobalArrayRef(pWork, pBuild, pLtypePTin, apLvalParam, cParam);

			u32 cReturn = (u32)pTinproc->m_arypTinReturns.C();
			auto apLvalReturn = (LLVMOpaqueValue **)alloca(sizeof(LLVMOpaqueValue**) * cReturn);
			for (u32 iReturn = 0; iReturn < cReturn; ++iReturn)
			{
				apLvalReturn[iReturn] = PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTinproc->m_arypTinReturns[iReturn], pReftab);
			}

			auto pLvalAryReturn = PLvalBuildConstantGlobalArrayRef(pWork, pBuild, pLtypePTin, apLvalReturn, cReturn);

			arypLval.Append(PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab));	//m_tin
			arypLval.Append(LLVMGlobalStringPtr(pBuild->m_pLbuild, pBuild->m_pLmoduleCur, pTinproc->m_strName.PCoz(), "strEnum")); //m_pCozName

			arypLval.Append(pLvalAryParam); //m_arypTinParam
			arypLval.Append(pLvalAryReturn); //m_arypTinReturn

			arypLval.Append(LLVMConstInt(LLVMInt1Type(), pTinproc->FHasVarArgs(), false)); //m_fHasVarArgs
			arypLval.Append(LLVMConstInt(LLVMInt8Type(), pTinproc->m_inlinek, true)); //m_inlinek
			arypLval.Append(LLVMConstInt(LLVMInt8Type(), pTinproc->m_callconv, true)); //m_callconv

			pLtypeTinDerived = PLtypeForTypeInfo(pWork, pBuild, "STypeInfoProcedure");
			pLvalReflect = LLVMConstNamedStruct(pLtypeTinDerived, arypLval.A(), (unsigned)arypLval.C());
		} break;
	case TINK_Bool:
	case TINK_Void:
	case TINK_Null:
	case TINK_Any:
	default:
		{
			pLtypeTinDerived = pLtypeTin;
			pLvalReflect = PLvalCreateReflectTin(pBuild, pLtypeTin, pTin, pReftab);
		} break;
	}

	EWC_ASSERT(pLvalReflect, "expected reflection type info");

	auto strName = pTin->m_strName;
	auto strPunyName = StrPunyEncode(strName.PCoz());

	pTin->m_pCgvalReflectGlobal = LLVMAddGlobal(pBuild->m_pLmoduleCur, pLtypeTinDerived, strPunyName.PCoz());
	LLVMSetGlobalConstant((LLVMValueRef)pTin->m_pCgvalReflectGlobal, true);

	LLVMSetInitializer((LLVMValueRef)pTin->m_pCgvalReflectGlobal, pLvalReflect);

	return LLVMConstPointerCast((LLVMValueRef)pTin->m_pCgvalReflectGlobal, pLtypePTin);
}

struct SReflectOrderData // tag = reord
{
	s32					m_ipTinParent;
	s32					m_cpTinChild;
	s32					m_cpTinDescend;

	s32					m_ipTinDest;		// index of this type in the final table
	s32					m_ipTinMax;			// current end of children in the final table
};

void ComputeReflectLayout(CAry<SReflectOrderData> * paryReord, SReflectOrderData * pReord, s32 * pipTinGlobal)
{
	if (pReord->m_ipTinParent == -1)
	{
		pReord->m_ipTinDest = *pipTinGlobal;
		pReord->m_ipTinMax = pReord->m_ipTinDest + 1;
		*pipTinGlobal += pReord->m_cpTinDescend + 1;
		return;
	}

	SReflectOrderData * pReordParent = &(*paryReord)[pReord->m_ipTinParent];
	if (pReordParent->m_ipTinDest == -1)
	{
		ComputeReflectLayout(paryReord, pReordParent, pipTinGlobal);
	}

	pReord->m_ipTinDest = pReordParent->m_ipTinMax;
	pReord->m_ipTinMax = pReord->m_ipTinDest + 1;
	pReordParent->m_ipTinMax += pReord->m_cpTinDescend + 1;
}

void ComputeTableOrder(SReflectGlobalTable * pReftab, CDynAry<SReflectOrderData> * paryReord, CDynAry<STypeInfo *> & arypTin)
{
	SReflectOrderData reord;
	reord.m_ipTinParent = -1;
	reord.m_cpTinChild = 0;
	reord.m_cpTinDescend = 0;
	reord.m_ipTinDest = -1;
	reord.m_ipTinMax = -1;

	int cReord = pReftab->m_mpPTinReftent.C();
	paryReord->Clear();
	paryReord->EnsureSize(cReord);
	paryReord->AppendFill(cReord, reord);

	for (int iReord = 0; iReord < cReord; ++iReord)
	{
		STypeInfo * pTin = arypTin[iReord];
		
		if (pTin->m_pTinNative)
		{
			SReflectTableEntry * pReftent = pReftab->m_mpPTinReftent.Lookup(pTin->m_pTinNative);
			if (EWC_FVERIFY(pReftent, "cannot find type during reflect table generation"))
			{
				(*paryReord)[iReord].m_ipTinParent = pReftent->m_ipTin;
			}
		}
	}

	// count up immediate children
	SReflectOrderData * pReordMax = paryReord->PMac();
	for (SReflectOrderData * pReordIt = paryReord->A(); pReordIt != pReordMax; ++pReordIt)
	{
		if (pReordIt->m_ipTinParent != -1)
		{
			auto pReordParent = &(*paryReord)[pReordIt->m_ipTinParent];
			++pReordParent->m_cpTinChild;
		}
	}

	// count up descendants
	for (SReflectOrderData * pReordIt = paryReord->A(); pReordIt != pReordMax; ++pReordIt)
	{
		int ipTin = pReordIt->m_ipTinParent;
		while (ipTin != -1)
		{
			auto pReord = &(*paryReord)[ipTin];
			pReord->m_cpTinDescend += pReordIt->m_cpTinChild + 1;
			ipTin = pReord->m_ipTinParent;
		}
	}

	s32 ipTinGlobal = 0;

	// layout self and ancestors
	s32 ipTin = 0;;
	for (SReflectOrderData * pReordIt = paryReord->A(); pReordIt != pReordMax; ++pReordIt, ++ipTin)
	{
		STypeInfo * pTin = arypTin[ipTin];

		SReflectTableEntry * pReftent = pReftab->m_mpPTinReftent.Lookup(pTin);
		if (!EWC_FVERIFY(pReftent, "cannot find type during reflect table generation"))
			continue;

		pReftent->m_cAliasChild = pReordIt->m_cpTinDescend;

		if (pReordIt->m_ipTinDest != -1)
			continue;

		ComputeReflectLayout(paryReord, pReordIt, &ipTinGlobal);


		if (pReordIt->m_ipTinParent != -1)
		{
			SReflectOrderData * pReordParent = &(*paryReord)[pReordIt->m_ipTinParent];
			pReftent->m_ipTinNative = pReordParent->m_ipTinDest;
		}
	}

	EWC_ASSERT(ipTinGlobal == cReord, "Bad table layout calculations");
}

void EnsureReftent(SReflectGlobalTable * pReftab, STypeInfo * pTin, CDynAry<STypeInfo *> * parypTin)
{
	EWC_ASSERT(pTin, "bad type info");

	SReflectTableEntry * pReftent;
	if (pReftab->m_mpPTinReftent.FinsEnsureKey(pTin, &pReftent) != FINS_Inserted)
		return;

	pReftent->m_ipTin = S32Coerce(parypTin->C());
	parypTin->Append(pTin);
	pReftent->m_ipTinNative = -1;
	pReftent->m_cAliasChild = 0;

	switch (pTin->m_tink)
	{
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
			EnsureReftent(pReftab, pTinary->m_pTin, parypTin);
		} break;
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			EnsureReftent(pReftab, pTinenum->m_pTinLoose, parypTin);
		} break;
	case TINK_Literal:
		{
			auto pTinlit = (STypeInfoLiteral *)pTin;
			if (pTinlit->m_pTinSource)
			{
				EnsureReftent(pReftab, pTinlit->m_pTinSource, parypTin);
			}
		} break;
	case TINK_Qualifier:
		{
			auto pTinqual = (STypeInfoQualifier *)pTin;
			EnsureReftent(pReftab, pTinqual->m_pTin, parypTin);
		} break;
	case TINK_Pointer:
		{
			auto pTinptr = (STypeInfoPointer *)pTin;
			EnsureReftent(pReftab, pTinptr->m_pTinPointedTo, parypTin);
		} break;
	case TINK_Procedure:
		{
			auto pTinproc = (STypeInfoProcedure *)pTin;

			{
				auto ppTinMax = pTinproc->m_arypTinParams.PMac();
				for (auto ppTin = pTinproc->m_arypTinParams.A(); ppTin != ppTinMax; ++ppTin)
				{
					EnsureReftent(pReftab, (*ppTin), parypTin);
				}
			}

			{
				auto ppTinMax = pTinproc->m_arypTinReturns.PMac();
				for (auto ppTin = pTinproc->m_arypTinReturns.A(); ppTin != ppTinMax; ++ppTin)
				{
					EnsureReftent(pReftab, (*ppTin), parypTin);
				}
			}
		} break;
	case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;

			auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
			for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb) 
			{
				EnsureReftent(pReftab, pTypememb->m_pTin, parypTin);
			}

		} break;
	default:
		//EWC_ASSERT(false, "Unhandled type info kind in EnsureReftent, %s", PChzFromTink(pTin->m_tink));
		break;
	}
}

static inline BCode::CBuilder::LValue * PLvalGenerateReflectTypeTable(CWorkspace * pWork, BCode::CBuilder * pBuild)
{
	EWC_ASSERT(false, "bytecode TBD");
	return nullptr;
}

static inline LLVMOpaqueValue * PLvalGenerateReflectTypeTable(CWorkspace * pWork, CBuilderIR * pBuild)
{
	CDynAry<STypeInfo *> arypTin(pBuild->m_pAlloc, BK_CodeGenReflect, 256);
	SReflectGlobalTable reftab(pBuild->m_pAlloc);

	auto pLtypeTin = PLtypeForTypeInfo(pWork, pBuild, "STypeInfo");
	auto pLtypePTin = LLVMPointerType(pLtypeTin, 0);
	CSymbolTable * pSymtab = pWork->m_pSymtab;

	STypeInfo ** ppTin;
	EWC::CHash<HV, STypeInfo *>::CIterator iter(pSymtab->m_phashHvPTinUnique);
	while ((ppTin = iter.Next()))
	{
		auto pTin = *ppTin;
		if (!pTin || pTin->m_tink >= TINK_ReflectedMax)
			continue;

		EnsureReftent(&reftab, pTin, &arypTin);
	}

	{
		CDynAry<SReflectOrderData> aryReord(pWork->m_pAlloc, BK_CodeGenReflect);
		ComputeTableOrder(&reftab, &aryReord, arypTin);

		CDynAry<STypeInfo *> arypTinDest(pWork->m_pAlloc, BK_CodeGenReflect);
		arypTinDest.AppendFill(arypTin.C(), nullptr);

		SReflectOrderData * pReordMax = aryReord.PMac();
		auto ppTinSrc = arypTin.A();
		for (auto pReordIt = aryReord.A(); pReordIt != pReordMax; ++pReordIt, ++ppTinSrc)
		{
			arypTinDest[pReordIt->m_ipTinDest] = *ppTinSrc;
		}
		arypTin.Swap(&arypTinDest);
	}

	CDynAry<LLVMOpaqueValue *> arypLval(pBuild->m_pAlloc, BK_CodeGenReflect, 256);
	for (STypeInfo ** ppTin = arypTin.A(); ppTin != arypTin.PMac(); ++ppTin)
	{
		STypeInfo * pTin = *ppTin;
		arypLval.Append(PLvalEnsureReflectStruct(pWork, pBuild, pLtypeTin, pLtypePTin, pTin, &reftab));
	}

	// array of pointers 
	auto pLtypeArray = LLVMArrayType(pLtypePTin, u32(arypLval.C()));

	auto pLvalArray = LLVMAddGlobal(pBuild->m_pLmoduleCur, pLtypeArray, "_tinTable_array");
	LLVMSetGlobalConstant(pLvalArray, true);

	auto pLvalArrayInit = LLVMConstArray(pLtypePTin, arypLval.A(), (unsigned)arypLval.C());
	LLVMSetInitializer(pLvalArray, pLvalArrayInit);

	auto pLtypePPTin = LLVMPointerType(pLtypePTin, 0);
	auto pLvalArrayCast = LLVMConstPointerCast(pLvalArray, pLtypePPTin);

	LLVMOpaqueValue * apLvalMember[2]; // count, pointer
	apLvalMember[ARYMEMB_Count] = LLVMConstInt(LLVMInt64Type(), arypLval.C(), false);
	apLvalMember[ARYMEMB_Data] = pLvalArrayCast;

	return LLVMConstStruct(apLvalMember, EWC_DIM(apLvalMember), false);
}

template <typename BUILD>
typename BUILD::LValue * PLvalFromLiteral(BUILD * pBuild, STypeInfoLiteral * pTinlit, CSTNode * pStnod)
{
	CSTValue * pStval = pStnod->m_pStval;

	bool fIsContainerLiteral = pTinlit->m_litty.m_litk == LITK_Array;
	if (!fIsContainerLiteral)
	{
		if (!EWC_FVERIFY(pStval, "literal missing value"))
			return nullptr;

		// containers are not finalized, just their contents
		if (!EWC_FVERIFY(pTinlit->m_fIsFinalized, "non-finalized literal type encountered during code gen"))
			return nullptr;
	}

	// NOTE: if we're implicit casting literals the STValue's kind won't match the literal kind!

	BUILD::LValue * pLval = nullptr;
	switch (pTinlit->m_litty.m_litk)
	{
	case LITK_Integer:
		{
			if (pStval->m_stvalk == STVALK_ReservedWord)
			{
				EWC_ASSERT(
					(pStval->m_rword == RWORD_LineDirective) |
					((pStval->m_nUnsigned == 1) & (pStval->m_rword == RWORD_True)) |
					((pStval->m_nUnsigned == 0) & (pStval->m_rword == RWORD_False)), "bad boolean reserved word");
			}
			else
			{
				EWC_ASSERT(pStval->m_stvalk == STVALK_SignedInt || pStval->m_stvalk == STVALK_UnsignedInt, "Integer literal kind mismatch");
			}

			bool fIsStvalSigned = pStval->m_stvalk == STVALK_SignedInt;
			if (fIsStvalSigned != pTinlit->m_litty.m_fIsSigned)
			{
				if (pTinlit->m_litty.m_fIsSigned)
				{
					EWC_ASSERT(pStval->m_nUnsigned <= LLONG_MAX, "Literal too large to fit in destination type");
				}
				else
				{
					EWC_ASSERT(pStval->m_nSigned >= 0, "Negative literal being assigned to unsigned value");
				}
			}

			pLval = pBuild->PLvalConstantInt(pStval->m_nUnsigned, pTinlit->m_litty.m_cBit, pTinlit->m_litty.m_fIsSigned);
		}break;
	case LITK_Float:
		{
			f64 g = 0;
			switch (pStval->m_stvalk)
			{
			case STVALK_UnsignedInt:	g = (float)pStval->m_nUnsigned;	break;
			case STVALK_SignedInt:		g = (float)pStval->m_nSigned;	break;
			case STVALK_Float:			g = pStval->m_g;				break;
			default: EWC_ASSERT(false, "Float literal kind mismatch");	break;
			}

			pLval = pBuild->PLvalConstantFloat(g, pTinlit->m_litty.m_cBit);
		}break;
	case LITK_Bool:
	{
			u64 nUnsigned = 0;
			switch (pStval->m_stvalk)
			{
			case STVALK_ReservedWord:
				{
					EWC_ASSERT(
						((pStval->m_nUnsigned == 1) & (pStval->m_rword == RWORD_True)) | 
						((pStval->m_nUnsigned == 0) & (pStval->m_rword == RWORD_False)), "bad boolean reserved word");

					nUnsigned = pStval->m_nUnsigned;
				} break;
			case STVALK_UnsignedInt:	nUnsigned = pStval->m_nUnsigned;		break;
			case STVALK_SignedInt:		nUnsigned = (pStval->m_nSigned != 0);	break;
			case STVALK_Float:			nUnsigned = (pStval->m_g != 0);			break;
			default: EWC_ASSERT(false, "bool literal kind mismatch");			break;
			}

			pLval = pBuild->PLvalConstantInt(nUnsigned, 1, false);
		} break;
	case LITK_Char:		EWC_ASSERT(false, "TBD"); return nullptr;
	case LITK_String:
		{
			if (!EWC_FVERIFY(pStval->m_stvalk == STVALK_String, "bad value in string literal"))
				return nullptr;

			// string literals aren't really constants in the eyes of llvm, but it'll work for now
			// because the global is a pointer - which it doesn't want to make constant - 

			pLval = pBuild->PLvalConstantGlobalStringPtr(pStval->m_str.PCoz(), "strlit");
		} break;
	case LITK_Null:
		{
			auto pLtype = pBuild->PLtypeFromPTin(pTinlit->m_pTinSource);
			if (!EWC_FVERIFY(pLtype, "could not find llvm type for null pointer"))
				return nullptr;

			pLval = BUILD::PLvalConstantNull(pLtype);
		} break;
	case LITK_Array:
		{
			CSTNode * pStnodLit = pStnod;
			if (EWC_FVERIFY(pTinlit->m_pStnodDefinition, "bad array literal definition"))
			{
				pStnodLit = pTinlit->m_pStnodDefinition;
			}

			CSTNode * pStnodList = nullptr;
			CSTDecl * pStdecl = PStmapRtiCast<CSTDecl *>(pStnodLit->m_pStmap);
			if (EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "array literal with no values"))
			{
				pStnodList = pStnodLit->PStnodChild(pStdecl->m_iStnodInit);
			}

			if (!pStnodList || !EWC_FVERIFY(pStnodList->CStnodChild() == pTinlit->m_c, "missing values for array literal"))
				return nullptr;

			size_t cB = sizeof(BUILD::LValue *) * (size_t)pTinlit->m_c;
			auto apLval = (BUILD::LValue **)(alloca(cB));

			for (int iStnod = 0; iStnod < pTinlit->m_c; ++iStnod)
			{
				auto pStnodChild = pStnodList->PStnodChild(iStnod);
				STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodChild->m_pTin;
				EWC_ASSERT(pTinlit->m_tink == TINK_Literal, "Bad array literal element");

				apLval[iStnod] = PLvalFromLiteral(pBuild, pTinlit, pStnodChild);
			}

			auto pLtypeElement = pBuild->PLtypeFromPTin(pTinlit->m_pTinSource);
			return BUILD::PLvalConstantArray(pLtypeElement, apLval, u32(pTinlit->m_c));
		} break;
	default:
		break;
	}

	EWC_ASSERT(pLval, "unknown LITK in PLValueFromLiteral");
	return pLval;
}

template <typename BUILD>
typename BUILD::Value * PValCreateCast(
	CWorkspace * pWork, 
	BUILD * pBuild,
	typename BUILD::Value * pValSrc, 
	STypeInfo * pTinSrc,
	STypeInfo * pTinDst)
{
	pTinSrc = PTinStripQualifiers(pTinSrc);
	pTinDst = PTinStripQualifiers(pTinDst);

	u32 cBitSrc = 0;
	u32 cBitDst;
	bool fSignedSrc = false;
	bool fSignedDst;
	if (FTypesAreSame(pTinSrc, pTinDst))
		return pValSrc;
	if (pTinSrc->m_tink == TINK_Literal)
	{
		// BB - should we finalize array literals as references to avoid this?

		auto pTinlitSrc = (STypeInfoLiteral *)pTinSrc;
		auto pTinaryDst = PTinRtiCast<STypeInfoArray *>(pTinDst);
		if (pTinlitSrc->m_litty.m_litk == LITK_Array && pTinaryDst && pTinaryDst->m_aryk == ARYK_Reference)
		{
			// BB - Allocating this on the stack feels a little dicey, but so does returning a reference to an
			//  array that happens to be static and isn't read-only.
				
			u64 cElement;
			auto pLtype = PLtypeFromPTin<BUILD>(pBuild, pTinlitSrc, &cElement);
			if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
				return nullptr;

			auto pValAllocaLit = pBuild->PValCreateAlloca(pLtype, cElement, "aryLit");

			// copy the literal into memory

			pBuild->PInstCreateMemcpy(pWork, pTinDst, pValAllocaLit, pValSrc);

			auto pLtypeDst = pBuild->PLtypeFromPTin(pTinDst);
			auto pValAllocaDst = pBuild->PValCreateAlloca(pLtypeDst, 1, "aryDst");

			// copy the fixed array into the array reference
			STypeInfoArray tinaryFixed;
			tinaryFixed.m_aryk = ARYK_Fixed;
			tinaryFixed.m_c = pTinlitSrc->m_c;
			tinaryFixed.m_pTin = pTinlitSrc->m_pTinSource;

			(void)PInstGenerateAssignmentFromRef(pWork, pBuild, pTinaryDst, &tinaryFixed, pValAllocaDst, pValAllocaLit);
			return pBuild->PInstCreate(IROP_Load, pValAllocaDst, "aryRefLoad");
		}

		return pValSrc;
	}

	if (pTinSrc->m_tink == TINK_Enum)
	{
		auto pTinenum = (STypeInfoEnum *)pTinSrc;
		pTinSrc = pTinenum->m_pTinLoose;
	}
	if (pTinDst->m_tink == TINK_Enum)
	{
		auto pTinenum = (STypeInfoEnum *)pTinDst;
		pTinDst = pTinenum->m_pTinLoose;
	}

	if (pTinSrc->m_tink == TINK_Pointer || pTinSrc->m_tink == TINK_Procedure)
	{
		if (pTinDst->m_tink != TINK_Bool)
		{
			if (EWC_FVERIFY(pTinDst->m_tink == pTinSrc->m_tink, "trying to cast pointer to non-pointer. (not supported yet)"))
			{
				return pBuild->PInstCreateCast(IROP_Bitcast, pValSrc, pTinDst, "Bitcast");
			}
			return pValSrc;
		}
	}
	else
	{
		if (!FExtractNumericInfo(pTinSrc, &cBitSrc, &fSignedSrc))
			return nullptr;
	}

	if (!FExtractNumericInfo(pTinDst, &cBitDst, &fSignedDst))
		return nullptr;

	BUILD::Instruction * pInst = nullptr;
	switch (pTinDst->m_tink)
	{
	case TINK_Integer:
		{
			switch (pTinSrc->m_tink)
			{
			case TINK_Integer: // fall through
			case TINK_Bool:
				{
					if (cBitDst < cBitSrc)				{ return pBuild->PInstCreateCast(IROP_NTrunc, pValSrc, pTinDst, "NTrunc"); }
					else if (fSignedSrc & fSignedDst)	{ return pBuild->PInstCreateCast(IROP_SignExt, pValSrc, pTinDst, "SignExt"); }
					else								{ return pBuild->PInstCreateCast(IROP_ZeroExt, pValSrc, pTinDst, "ZeroExt"); }
				} break;
			case TINK_Float:
				{
					if (fSignedSrc) { return pBuild->PInstCreateCast(IROP_GToS, pValSrc, pTinDst, "GToS"); }
					else			{ return pBuild->PInstCreateCast(IROP_GToU, pValSrc, pTinDst, "GToU"); }
				} break;
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			default:
				EWC_ASSERT(false, "unexpected type info kind in integer cast");
				break;
			}
		} break;
	case TINK_Float:
		{
			switch (pTinSrc->m_tink)
			{
			case TINK_Integer: // fall through
			case TINK_Bool:
				{
					if (fSignedSrc) { return pBuild->PInstCreateCast(IROP_SToG, pValSrc, pTinDst, "SToG"); }
					else			{ return pBuild->PInstCreateCast(IROP_UToG, pValSrc, pTinDst, "UToG"); }
				}
			case TINK_Float:
					if (cBitDst > cBitSrc)	{ return pBuild->PInstCreateCast(IROP_GExtend, pValSrc, pTinDst, "GExtend"); }
					else					{ return pBuild->PInstCreateCast(IROP_GTrunc, pValSrc, pTinDst, "GTrunc"); }
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			default:
				EWC_ASSERT(false, "unexpected type info kind in float cast");
				break;
			}

		}break;
	case TINK_Bool:
			switch (pTinSrc->m_tink)
			{
			case TINK_Bool:	
			case TINK_Flag:	
				return pValSrc;
			case TINK_Integer:
				{
					auto pConstZero = PConstZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreateNCmp(NPRED_NE, pValSrc, pConstZero, "NToBool");
					return pInst;
				} 
			case TINK_Float:
				{
					auto pConstZero = PConstZeroInType(pBuild, pTinSrc);
					pBuild->PInstCreateGCmp(GPRED_LE, pValSrc, pConstZero, "GToBool");
					return pInst;
				}
			case TINK_Pointer:
				{
					auto pConstZero = PConstZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreateNCmp(NPRED_NE, pValSrc, pConstZero, "PToBool");
					return pInst;
				}
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			default:
				EWC_ASSERT(false, "unexpected type info kind in boo cast");
				break;
			} break;
	case TINK_Literal:
		EWC_ASSERT(false, "can't cast to literal");
		return nullptr;
	default:
		EWC_ASSERT(false, "unsupported cast destination type in PValCreateCast");
	}

	return pValSrc;
}

CIRInstruction * CBuilderIR::PInstCreateMemcpy(
	CWorkspace * pWork,
	STypeInfo * pTin,
	CIRValue * pValLhs,
	CIRValue * pValRhsRef)
{
	auto pLtypePInt8 = LLVMPointerType(LLVMInt8Type(), 0);
	auto pLtypeInt64 = LLVMInt64Type();
	if (!m_mpIntfunkPLval[INTFUNK_Memcpy])
	{
		LLVMTypeRef apLtypeArgs[] = { pLtypePInt8, pLtypePInt8, pLtypeInt64, LLVMInt32Type(), LLVMInt1Type() };
		LLVMTypeRef pLtypeFunction = LLVMFunctionType(LLVMVoidType(), apLtypeArgs, EWC_DIM(apLtypeArgs), false);
		m_mpIntfunkPLval[INTFUNK_Memcpy] = LLVMAddFunction(m_pLmoduleCur, "llvm.memcpy.p0i8.p0i8.i64", pLtypeFunction);
	}

	u64 cBitSize;
	u64 cBitAlign;

	auto pLtype = PLtypeFromPTin(pTin);
	CalculateSizeAndAlign(this, pLtype, &cBitSize, &cBitAlign);

	LLVMOpaqueValue * apLvalArgs[5];
	apLvalArgs[0] = LLVMBuildBitCast(m_pLbuild, pValLhs->m_pLval, pLtypePInt8, OPNAME("memDst")); // dest


	apLvalArgs[1] = LLVMBuildBitCast(m_pLbuild, pValRhsRef->m_pLval, pLtypePInt8, OPNAME("memSrc")); // source
	apLvalArgs[2] =	LLVMConstInt(LLVMInt64Type(), cBitSize / 8, false);		// cB
	apLvalArgs[3] =	LLVMConstInt(LLVMInt32Type(), cBitAlign / 8, false);	// cBAlign
	apLvalArgs[4] = LLVMConstInt(LLVMInt1Type(), false, false);	// fIsVolitile

	CIRInstruction * pInstMemcpy = PInstCreateRaw(IROP_Memcpy, nullptr, nullptr, "memcpy");
	pInstMemcpy->m_pLval = LLVMBuildCall(
							m_pLbuild,
							m_mpIntfunkPLval[INTFUNK_Memcpy],
							apLvalArgs,
							EWC_DIM(apLvalArgs),
							"");
	return pInstMemcpy;
}

CIRInstruction * CBuilderIR::PInstCreateMemset( CWorkspace * pWork, CIRValue * pValLhs, s64 cBSize, s32 cBAlign, u8 bFill)
{
	auto pLtypePInt8 = LLVMPointerType(LLVMInt8Type(), 0);
	if (!m_mpIntfunkPLval[INTFUNK_Memset])
	{
		LLVMTypeRef apLtypeArgs[] = { pLtypePInt8, LLVMInt8Type(), LLVMInt64Type(), LLVMInt32Type(), LLVMInt1Type() };
		LLVMTypeRef pLtypeFunction = LLVMFunctionType(LLVMVoidType(), apLtypeArgs, EWC_DIM(apLtypeArgs), false);
		m_mpIntfunkPLval[INTFUNK_Memset] = LLVMAddFunction(m_pLmoduleCur, "llvm.memset.p0i8.i64", pLtypeFunction);
	}

	LLVMOpaqueValue * apLvalArgs[5];
	apLvalArgs[0] = LLVMBuildBitCast(m_pLbuild, pValLhs->m_pLval, pLtypePInt8, OPNAME("memDst")); // dest
	apLvalArgs[1] = LLVMConstInt(LLVMInt8Type(), bFill, false);		// bFill
	apLvalArgs[2] =	LLVMConstInt(LLVMInt64Type(), cBSize, false);		// cB
	apLvalArgs[3] =	LLVMConstInt(LLVMInt32Type(), cBAlign, false);	// cBAlign
	apLvalArgs[4] = LLVMConstInt(LLVMInt1Type(), false, false);	// fIsVolitile

	CIRInstruction * pInstMemcpy = PInstCreateRaw(IROP_Memcpy, nullptr, nullptr, "memcpy");
	pInstMemcpy->m_pLval = LLVMBuildCall(
							m_pLbuild,
							m_mpIntfunkPLval[INTFUNK_Memset],
							apLvalArgs,
							EWC_DIM(apLvalArgs),
							"");
	return pInstMemcpy;
}

CIRInstruction * CBuilderIR::PInstCreateLoopingInit(CWorkspace * pWork, STypeInfo * pTin, CIRValue * pValLhs, CSTNode * pStnodInit)
{
	// This should be an array, we need to loop over the elements and either memcpy a global or call an 
	//  initializer proc:
		
	CIRProcedure * pProc = m_pProcCur;
	auto pTinary = (STypeInfoArray *)pTin;
	EWC_ASSERT(pTinary->m_aryk == ARYK_Fixed, "unexpected ARYK");

	auto pLvalZero = PLvalConstantInt(0, 64, false);
	auto pLvalOne = PLvalConstantInt(1, 64, false);
	auto pLvalCount = PLvalConstantInt(pTinary->m_c, 64, false);
	
	auto pLvalAlloca = LLVMBuildAlloca(m_pLbuild, LLVMInt64Type(), "iInit");
	LLVMBuildStore(m_pLbuild, pLvalZero, pLvalAlloca);

	CIRBlock *	pBlockPred = PBlockCreate(pProc, "initPred");
	CIRBlock *	pBlockBody = PBlockCreate(pProc, "initBody");
	CIRBlock * pBlockPost = PBlockCreate(pProc, "initPost");

	CreateBranch(pBlockPred);	

	ActivateBlock(pBlockPred);
	auto pLvalLoadIndex = LLVMBuildLoad(m_pLbuild, pLvalAlloca, "iLoad");
	auto pLvalCmp = LLVMBuildICmp(m_pLbuild, LLVMIntULT, pLvalLoadIndex, pLvalCount, "NCmp");

	LLVMBuildCondBr(m_pLbuild, pLvalCmp, pBlockBody->m_pLblock, pBlockPost->m_pLblock);

	ActivateBlock(pBlockBody);

	LLVMOpaqueValue * apLvalIndex[2] = {};
	apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);
	apLvalIndex[1] = pLvalLoadIndex;
	auto pInstGEP = PInstCreateGEP(pValLhs, apLvalIndex, 2, "initGEP");

	EWC_ASSERT(!pStnodInit, "expected null initializer - ignoring");
	(void) PValInitialize(pWork, this, pTinary->m_pTin, pInstGEP, nullptr);

	auto pLvalInc = LLVMBuildAdd(m_pLbuild, pLvalLoadIndex, pLvalOne, "iInc");
	LLVMBuildStore(m_pLbuild, pLvalInc, pLvalAlloca);
	LLVMBuildBr(m_pLbuild, pBlockPred->m_pLblock);

	ActivateBlock(pBlockPost);
	return nullptr;
}


// allocate a global constant used to initialize type with memcpy
LLVMOpaqueValue * PLvalBuildConstantInitializer(CWorkspace * pWork, CBuilderIR * pBuild, STypeInfo * pTin, CSTNode * pStnodInit)
{
	if (pTin->m_tink == TINK_Struct)
	{
		auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pTin);

		int cTypememb = (int)pTinstruct->m_aryTypemembField.C();
		size_t cB = sizeof(LLVMOpaqueValue *) * cTypememb;
		auto apLvalMember = (LLVMOpaqueValue **)(alloca(cB));
		EWC::ZeroAB(apLvalMember, cB);

		CSTNode * pStnodStruct = pTinstruct->m_pStnodStruct;
		EWC_ASSERT(pStnodStruct, "missing definition in struct type info");

		for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
		{
			STypeStructMember * pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];
			CSTNode * pStnodDecl = pTypememb->m_pStnod;
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);
			if (!EWC_FVERIFY(pStdecl, "expected decl"))
				continue;

			LLVMOpaqueValue * pLvalMember = nullptr;
			auto pStnodInitMemb = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);

			if (pStnodInitMemb && pStnodInitMemb->m_park != PARK_Uninitializer)
			{
				auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInitMemb->m_pTin);
				if (EWC_FVERIFY(pTinlit, "Expected literal"))
				{
					pLvalMember = PLvalFromLiteral(pBuild, pTinlit, pStnodInitMemb);
				}
			}

			if (!pLvalMember)
			{
				pLvalMember = PLvalBuildConstantInitializer(pWork, pBuild, pTypememb->m_pTin, nullptr);
			}
			apLvalMember[iTypememb] = pLvalMember;
		}

		auto pCgstruct = pBuild->PCgstructEnsure(pTinstruct);
		return LLVMConstNamedStruct(pCgstruct->m_pLtype, apLvalMember, cTypememb);
	}
	else if (pTin->m_tink == TINK_Array)
	{
		auto pTinary = (STypeInfoArray *)pTin;
		switch (pTinary->m_aryk)
		{
			case ARYK_Fixed:
			{
				if (pStnodInit && pStnodInit->m_park != PARK_Uninitializer)
				{
					auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInit->m_pTin);
					if (EWC_FVERIFY(pTinlit, "Expected literal"))
					{
						return PLvalFromLiteral(pBuild, pTinlit, pStnodInit);
					}
				}
				else
				{
					auto cginitk = CginitkCompute(pTinary->m_pTin, nullptr);
					if (cginitk == CGINITK_MemcpyGlobal)
					{
						auto pLvalInit = PLvalBuildConstantInitializer(pWork, pBuild, pTinary->m_pTin, nullptr);
						auto apLval = (LLVMValueRef *)pBuild->m_pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, (size_t)pTinary->m_c);

						for (s64 iElement = 0; iElement < pTinary->m_c; ++iElement)
						{
							apLval[iElement] = pLvalInit;
						}

						LLVMOpaqueType * pLtypeElement = pBuild->PLtypeFromPTin(pTinary->m_pTin);
						auto pLvalReturn = LLVMConstArray(pLtypeElement, apLval, u32(pTinary->m_c));
						pBuild->m_pAlloc->EWC_DELETE(apLval);

						return pLvalReturn;
					}
				}

				return PLvalZeroInType(pBuild, pTin);

			} break;
			case ARYK_Reference:
			{
				if (pStnodInit && pStnodInit->m_park != PARK_Uninitializer)
				{
					EWC_ASSERT(false, "constant array reference?");
				}

				return PLvalZeroInType(pBuild, pTin);
			} break;
		default:
			EWC_ASSERT(false, "Unhandled array kind");
			break;
		}
	}

	if (pStnodInit && pStnodInit->m_park != PARK_Uninitializer)
	{
		auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInit->m_pTin);
		if (EWC_FVERIFY(pTinlit, "Expected literal"))
		{
			return PLvalFromLiteral(pBuild, pTinlit, pStnodInit);
		}
	}

	return PLvalZeroInType(pBuild, pTin);
}

BCode::CBuilder::LValue * PLvalBuildConstantInitializer(CWorkspace * pWork, BCode::CBuilder * pBuild, STypeInfo * pTin, CSTNode * pStnodInit)
{
	EWC_ASSERT(false, "bytecode TBD");
	return nullptr;
}

CGINITK CginitkCompute(STypeInfo * pTin, CSTNode * pStnodInit)
{
	if (!EWC_FVERIFY(pTin, "null type in CginitkCompute"))
		return CGINITK_NoInit;

	if (pStnodInit && pStnodInit->m_park == PARK_Uninitializer)
	{
		return CGINITK_NoInit;
	}

	if (pTin->m_tink == TINK_Struct)
	{
		if (pStnodInit)
		{
			return CGINITK_AssignInitializer;
		}

		// if all members have uninitializer return noInit
		// if no members have any initializer memset zero
		// if all members have memcpyGlobal or lower we can build a global constant and assign it
		// otherwise construct an init function

		auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pTin);

		CSTNode * pStnodStruct = pTinstruct->m_pStnodStruct;
		EWC_ASSERT(pStnodStruct, "missing definition in struct type info");

		CGINITK cginitkMax = CGINITK_NoInit;
		auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
		for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb)
		{
			CSTNode * pStnodDecl = pTypememb->m_pStnod;
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);
			if (!EWC_FVERIFY(pStdecl, "expected decl"))
				continue;

			auto pStnodInitMemb = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
			auto cginitkIt = CginitkCompute(pStnodDecl->m_pTin, pStnodInitMemb);
			if (cginitkIt > cginitkMax)
			{
				cginitkMax = cginitkIt;
			}
		}

		if (cginitkMax <= CGINITK_MemsetZero)
			return cginitkMax;

		if (cginitkMax <= CGINITK_MemcpyGlobal)
			return CGINITK_MemcpyGlobal;

		return CGINITK_InitializerProc;
	}
	else if (pTin->m_tink == TINK_Array)
	{
		auto pTinary = (STypeInfoArray *)pTin;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed:
			{
				if (pStnodInit)
				{
					return CGINITK_MemcpyGlobal;
				}

				auto cginitkElement = CginitkCompute(pTinary->m_pTin, nullptr);
				if (cginitkElement <= CGINITK_AssignInitializer)
					return cginitkElement;

				return CGINITK_LoopingInit;
			}
		case ARYK_Reference:	
			{
				if (pStnodInit)
					return CGINITK_AssignInitializer;
				return CGINITK_MemsetZero;
			}
		default: EWC_ASSERT(false, "unhandled ARYK");
		}
	}

	if (pStnodInit)
		return CGINITK_AssignInitializer;
	return CGINITK_MemsetZero;
}

static inline bool FIsRegisterSize(int cB)
{
	return (cB == 1) | (cB == 2) | (cB == 4) | (cB == 8);
}

template <typename BUILD>
static inline typename BUILD::Value * PValInitialize(
	CWorkspace * pWork,
	BUILD * pBuild,
	STypeInfo * pTin,
	typename BUILD::Value * pValPT,	// pLhs
	CSTNode * pStnodInit)
{
	auto cginitk = CginitkCompute(pTin, pStnodInit);

	switch (cginitk)
	{
	case CGINITK_NoInit: 
		break;
	case CGINITK_MemsetZero:
		{
			u64 cBitSize;
			u64 cBitAlign;
			auto pLtype = pBuild->PLtypeFromPTin(pTin);
			CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);

			// if register-sized type just store
			if (FIsRegisterSize(int(cBitSize/8)))
			{
				auto pConstZero = PConstZeroInType(pBuild, pTin);
				return pBuild->PInstCreateStore(pValPT, pConstZero);
			}
			else
			{
				return pBuild->PInstCreateMemset(pWork, pValPT, cBitSize/8, int(cBitAlign/8), 0);
			}

		} break;
	case CGINITK_AssignInitializer:
		{
			return PInstGenerateAssignment(pWork, pBuild, pTin, pValPT, pStnodInit);
		} break;
	case CGINITK_MemcpyGlobal:
		{
			// if there is an initializer
				// if it is a literal - make a global and memcpy it
				// else... just memcpy it

			if (pStnodInit && pStnodInit->m_park != PARK_Uninitializer)
			{
				BUILD::Value * pValInit;
				if (pStnodInit->m_pTin->m_tink == TINK_Literal)
				{
					auto strName = pTin->m_strName;
					auto strPunyName = StrPunyEncode(strName.PCoz());

					auto pLvalInit = PLvalBuildConstantInitializer(pWork, pBuild, pTin, pStnodInit);

					auto pLtype = pBuild->PLtypeFromPTin(pTin);

					auto pGlob = pBuild->PGlobCreate(pLtype, strPunyName.PCoz());
					SetInitializer(pGlob, pLvalInit);
					pValInit = pGlob;
				}
				else
				{
					pValInit = PValGenerate(pWork, pBuild, pStnodInit, VALGENK_Reference);
				}

				return pBuild->PInstCreateMemcpy(pWork, pTin, pValPT, pValInit);
			}

			// if no initializer 
			//		if struct make the default global initializer and save it for reuse

			auto pTinstruct = PTinRtiCast<STypeInfoStruct *>(pTin);
			if (EWC_FVERIFY(pTinstruct, "non-struct value without initializer should not be MemmcpyGlobal"))
			{

				if (!pTinstruct->m_pVGlobInit)
				{
					auto strName = pTin->m_strName;
					auto strPunyName = StrPunyEncode(strName.PCoz());

					auto pLvalInit = PLvalBuildConstantInitializer(pWork, pBuild, pTin, pStnodInit);

					auto pLtype = pBuild->PLtypeFromPTin(pTin);

					auto pGlobInit = pBuild->PGlobCreate(pLtype, strPunyName.PCoz());
					pTinstruct->m_pVGlobInit = pGlobInit;

					SetInitializer(pGlobInit, pLvalInit);
				}

				return pBuild->PInstCreateMemcpy(pWork, pTin, pValPT, (BUILD::Global*)pTinstruct->m_pVGlobInit);
			}
		} break;
	case CGINITK_LoopingInit:
		{
			return pBuild->PInstCreateLoopingInit(pWork, pTin, pValPT, pStnodInit);

		} break;
	case CGINITK_InitializerProc:
		{
			if (!EWC_FVERIFY(pTin->m_tink == TINK_Struct, "expected structure"))
			{
				auto pTinstruct = (STypeInfoStruct*)pTin;
				auto pCgstruct = pBuild->PCgstructEnsure(pTinstruct);

				// create an init function and call it
				if (!pCgstruct->m_pProcInitMethod)
				{
					auto pProc = PProcCodegenInitializer(pWork, pBuild, pTinstruct->m_pStnodStruct);

					pCgstruct->m_pProcInitMethod = pProc;
					if (!EWC_FVERIFY(pProc, "failed to create init method"))
						return nullptr;
				}

				auto pProcInit = pCgstruct->m_pProcInitMethod;
				if (!EWC_FVERIFY(CParamFromProc(pProcInit) == 1, "unexpected number of arguments"))
					return nullptr;

				BUILD::LValue * apLvalArgs[1];
				apLvalArgs[0] = BUILD::PProcArg(pValPT);

				auto pInst = pBuild->PInstCreateCall(PLvalFromPVal(pProcInit), pCgstruct->m_pLtype, apLvalArgs, 1);
				return pInst;
			}
		} break;
	default:
		EWC_ASSERT(false, "unhandled codegen init kind");
		break;
	}
	return nullptr;
}

template <typename BUILD>
typename BUILD::Value * PValFromArrayMember(
	CWorkspace * pWork,
	BUILD * pBuild,
	typename BUILD::Value * pValAryRef,
	STypeInfoArray * pTinary,
	ARYMEMB arymemb,
	VALGENK valgenk)
{
	EWC_ASSERT(pTinary->m_tink == TINK_Array, "expected array");

	if (pTinary->m_aryk == ARYK_Fixed)
	{
		EWC_ASSERT(valgenk == VALGENK_Instance, "expected instance");
		if (arymemb == ARYMEMB_Count)
		{
			return pBuild->PConstInt(pTinary->m_c, 64, true);
		}

		// the type of aN.data is &N so a reference would need to be a &&N which we don't have
		EWC_ASSERT(arymemb == ARYMEMB_Data, "unexpected array member '%s'", PChzFromArymemb(arymemb));

		auto pTinptr = pWork->m_pSymtab->PTinptrAllocate(pTinary->m_pTin);
		return pBuild->PInstCreateCast(IROP_Bitcast, pValAryRef, pTinptr, "Bitcast");
	}

	BUILD::GepIndex * apLvalIndex[2] = {};
	apLvalIndex[0] = pBuild->PGepIndex(0);
	apLvalIndex[1] = pBuild->PGepIndex(arymemb);
	auto pValRef = pBuild->PInstCreateGEP(pValAryRef, apLvalIndex, EWC_DIM(apLvalIndex), "aryGep");

	if (valgenk == VALGENK_Reference)
		return pValRef;

	return pBuild->PInstCreate(IROP_Load, pValRef, "arymembLoad");
}

template <typename BUILD>
static inline typename BUILD::Value * PValGenerateCast(
	CWorkspace * pWork,
	BUILD * pBuild,
	VALGENK valgenk,
	CSTNode * pStnodRhs,
	STypeInfo * pTinOut)
{
	STypeInfo * pTinRhs = pStnodRhs->m_pTin;
	pTinOut = PTinStripQualifiers(pTinOut);

	bool rhsIsArray = false;
	if (pTinRhs)
	{
		rhsIsArray = pTinRhs->m_tink == TINK_Array;
		if (auto pTinRhsLit = PTinRtiCast<STypeInfoLiteral *>(pTinRhs))
		{
			rhsIsArray = pTinRhsLit->m_litty.m_litk == LITK_Array;
		}
	}

	if (rhsIsArray)
	{
		auto pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Reference);

		// special case for assigning arrays to pointers, need reference to the array type.
		if (pTinOut->m_tink == TINK_Pointer)
		{
			auto pValData = pValRhsRef;
			auto pTinptr = (STypeInfoPointer *)pTinOut;
			if (pTinRhs->m_tink == TINK_Array)
			{
				auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
				pValData = PValFromArrayMember(pWork, pBuild, pValRhsRef, pTinaryRhs, ARYMEMB_Data, VALGENK_Instance);
				return pBuild->PInstCreateCast(IROP_Bitcast, pValData, pTinptr, "Bitcast");

				if (FTypesAreSame(pTinaryRhs->m_pTin, pTinptr->m_pTinPointedTo))
					return pValData;

			}

			// else is array literal
			pValData = pBuild->PInstCreateCast(IROP_Bitcast, pValData, pTinptr, "Bitcast");

			return pValData;
		}
		if (pTinOut->m_tink == TINK_Array && pTinRhs->m_tink == TINK_Array)
		{
			auto pTinaryLhs = (STypeInfoArray *)pTinOut;
			auto pTinaryRhs = (STypeInfoArray *)pTinRhs;

			if (pTinaryRhs->m_aryk != pTinaryLhs->m_aryk)
			{
				if (pTinaryLhs->m_aryk == ARYK_Reference)
				{
					EWC_ASSERT(pTinaryRhs->m_aryk == ARYK_Fixed, "expected ARYK_Fixed");

					u64 cElement;
					auto pLtype = PLtypeFromPTin(pBuild, pTinaryLhs, &cElement);
					if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for cast"))
						return nullptr;

					auto * pValAlloca = pBuild->PValCreateAlloca(pLtype, cElement, "aryCast");
					(void)PInstGenerateAssignmentFromRef(pWork, pBuild, pTinaryLhs, pTinRhs, pValAlloca, pValRhsRef);

					if (valgenk == VALGENK_Reference)
						return pValAlloca;
					return pBuild->PInstCreate(IROP_Load, pValAlloca, "deref");
				}

				EWC_ASSERT(false, "no implicit cast from ARYK %s to ARYK %s", PChzFromAryk(pTinaryRhs->m_aryk), PChzFromAryk(pTinaryLhs->m_aryk));
			}
		}

		{
			if (pTinRhs->m_tink == TINK_Literal)
			{
				// BB - should we finalize array literals as references to avoid this?

				auto pTinlitRhs = (STypeInfoLiteral *)pTinRhs;
				auto pTinaryLhs = PTinRtiCast<STypeInfoArray *>(pTinOut);
				if (pTinlitRhs->m_litty.m_litk == LITK_Array && pTinaryLhs && pTinaryLhs->m_aryk == ARYK_Reference)
				{
					// BB - Allocating this on the stack feels a little dicey, but so does returning a reference to an
					//  array that happens to be static and isn't read-only.
						
					u64 cElement;
					auto pLtype = PLtypeFromPTin(pBuild, pTinlitRhs, &cElement);
					if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
						return nullptr;

					auto pValAllocaLit = pBuild->PValCreateAlloca(pLtype, cElement, "aryLit");

					// copy the literal into memory

					pBuild->PInstCreateMemcpy(pWork, pTinlitRhs, pValAllocaLit, pValRhsRef);

					auto pLtypeDst = pBuild->PLtypeFromPTin(pTinOut);
					auto pValAllocaDst = pBuild->PValCreateAlloca(pLtypeDst, 1, "aryDst");

					// copy the fixed array into the array reference
					STypeInfoArray tinaryFixed;
					tinaryFixed.m_aryk = ARYK_Fixed;
					tinaryFixed.m_c = pTinlitRhs->m_c;
					tinaryFixed.m_pTin = pTinlitRhs->m_pTinSource;

					(void)PInstGenerateAssignmentFromRef(pWork, pBuild, pTinaryLhs, &tinaryFixed, pValAllocaDst, pValAllocaLit);

					if (valgenk == VALGENK_Reference)
						return pValAllocaDst;
					return pBuild->PInstCreate(IROP_Load, pValAllocaDst, "deref");
				}
			}
		}

		ASSERT_STNOD(pWork, pStnodRhs, valgenk != VALGENK_Reference, "only pointers can be lValues after a cast");

		auto pValSrc = pBuild->PInstCreate(IROP_Load, pValRhsRef, "castLoad");
		auto pVal = PValCreateCast(pWork, pBuild, pValSrc, pTinRhs, pTinOut);
		if (!pVal)
		{
			EmitError(pWork, &pStnodRhs->m_lexloc, ERRID_BadCastGen, "INTERNAL ERROR: trying to codegen unsupported numeric cast.");
		}
		return pVal;
	}

	ASSERT_STNOD(pWork, pStnodRhs, (valgenk != VALGENK_Reference) || (pTinRhs->m_tink == TINK_Pointer), "only pointers can be LValues after a cast");

	auto pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, valgenk);
	if (pTinOut->m_tink == TINK_Bool && pTinRhs->m_tink == TINK_Flag)
	{
		return pValRhs;
	}

	auto pVal = PValCreateCast(pWork, pBuild, pValRhs, pTinRhs, pTinOut);
	if (!pVal)
	{
		EmitError(pWork, &pStnodRhs->m_lexloc, ERRID_BadCastGen, "INTERNAL ERROR: trying to codegen unsupported numeric cast.");
	}
	return pVal;
}

CIRInstruction * PInstGenerateAssignmentFromRef(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs,
	CIRValue * pValLhs,
	CIRValue * pValRhsRef)
{
	switch (pTinLhs->m_tink)
	{
		case TINK_Array:
		{
			ARYK arykRhs;
			s64 cRhs;
			switch (pTinRhs->m_tink)
			{
			case TINK_Array:
				{
					auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
					arykRhs = pTinaryRhs->m_aryk;
					cRhs = pTinaryRhs->m_c;
				} break;
			case TINK_Literal:
				{
					auto pTinlitRhs = (STypeInfoLiteral *)pTinRhs;
					EWC_ASSERT(pTinlitRhs->m_litty.m_litk == LITK_Array, "bad literal type in array assignment");

					arykRhs = ARYK_Fixed;
					cRhs = pTinlitRhs->m_c;
				} break;
			default:
				EWC_ASSERT(false, "assigning non-array (%s) to an array", PChzFromTink(pTinRhs->m_tink));
				 return nullptr;
			}

			auto pTinaryLhs = (STypeInfoArray *)pTinLhs;
			switch (pTinaryLhs->m_aryk)
			{
			case ARYK_Fixed:
				{
					EWC_ASSERT(arykRhs == ARYK_Fixed, "cannot copy mixed array kinds to fixed array");

					CIRProcedure * pProc = pBuild->m_pProcCur;

					auto pValZero = pBuild->PConstInt(0, 64, false);
					auto pValOne = pBuild->PConstInt(0,  64, false);
					auto pValCount = pBuild->PConstInt(pTinaryLhs->m_c, 64, false);
					
					auto pValAlloca = pBuild->PValCreateAlloca(LLVMInt64Type(), 0, "iInit");
					(void) pBuild->PInstCreateStore(pValAlloca, pValZero);

					CIRBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "copyPred");
					CIRBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "copyBody");
					CIRBlock * pBlockPost = pBuild->PBlockCreate(pProc, "copyPost");

					pBuild->CreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPred);
					auto pInstLoadIndex = pBuild->PInstCreate(IROP_Load, pValAlloca, "iLoad");
					auto pInstCmp = pBuild->PInstCreateNCmp(NPRED_ULT, pInstLoadIndex, pValCount, "NCmp");

					(void) pBuild->PInstCreateCondBranch(pInstCmp, pBlockBody, pBlockPost);

					pBuild->ActivateBlock(pBlockBody);

					LLVMOpaqueValue * apLvalIndex[2] = {};
					apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);
					apLvalIndex[1] = pInstLoadIndex->m_pLval;
					auto pInstGEPLhs = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "GEPLhs");
					auto pInstGEPRhs = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "GEPRhs");
					auto pInstLoadRhs = pBuild->PInstCreate(IROP_Load, pInstGEPRhs, "loadRhs");
					(void) pBuild->PInstCreateStore(pInstGEPLhs, pInstLoadRhs);

					auto pInstInc = pBuild->PInstCreate(IROP_NAdd, pInstLoadIndex, pValOne, "iInc");
					(void) pBuild->PInstCreateStore(pValAlloca, pInstInc);
					pBuild->CreateBranch(pBlockPred);

					pBuild->ActivateBlock(pBlockPost);

					return nullptr;

				} break;
			case ARYK_Reference:
				{
					LLVMOpaqueValue * apLvalIndex[2] = {};
					apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);

					CIRValue * pValCount = nullptr;
					CIRValue * pValData = nullptr;
					switch (arykRhs)
					{
					case ARYK_Fixed:
						{
							pValCount = pBuild->PConstInt(cRhs);

							apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 0, false);
							pValData = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, EWC_DIM(apLvalIndex), "aryGep");
						} break;
					case ARYK_Reference:
						{
							apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), ARYMEMB_Count, false);
							auto pInstGepCount = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "gepCount");
							pValCount = pBuild->PInstCreate(IROP_Load, pInstGepCount, "loadC");

							apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), ARYMEMB_Data, false);
							auto pInstGepData = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "gepData");
							pValData = pBuild->PInstCreate(IROP_Load, pInstGepData, "loadData");
						} break;
					default: EWC_ASSERT(false, "Unhandled ARYK"); 
					}

					apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 0, false);
					auto pInstGepCount = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "gepCount");
					(void) pBuild->PInstCreateStore(pInstGepCount, pValCount);

					apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 1, false);
					auto pInstGepData = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "gepData");
					return pBuild->PInstCreateStore(pInstGepData, pValData);

				} break;
			default: EWC_ASSERT(false, "Unhandled ARYK"); 
			}
		} break;
		case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTinLhs;

			return pBuild->PInstCreateMemcpy(pWork, pTinstruct, pValLhs, pValRhsRef);
		} break;
		default:
		{
			auto pInstRhsLoad = pBuild->PInstCreate(IROP_Load, pValRhsRef, "loadRhs");
			return pBuild->PInstCreateStore(pValLhs, pInstRhsLoad);
		}
	}

	return nullptr;
}

BCode::SInstruction * PInstGenerateAssignmentFromRef(
	CWorkspace * pWork,
	BCode::CBuilder * pBuild,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs,
	BCode::SValue * pValLhs,
	BCode::SValue * pValRhsRef)
{
	switch (pTinLhs->m_tink)
	{
		case TINK_Array:
		{
			EWC_ASSERT(false, "bytecode TBD");
			return nullptr;
			/*
			ARYK arykRhs;
			s64 cRhs;
			switch (pTinRhs->m_tink)
			{
			case TINK_Array:
				{
					auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
					arykRhs = pTinaryRhs->m_aryk;
					cRhs = pTinaryRhs->m_c;
				} break;
			case TINK_Literal:
				{
					auto pTinlitRhs = (STypeInfoLiteral *)pTinRhs;
					EWC_ASSERT(pTinlitRhs->m_litty.m_litk == LITK_Array, "bad literal type in array assignment");

					arykRhs = ARYK_Fixed;
					cRhs = pTinlitRhs->m_c;
				} break;
			default:
				EWC_ASSERT(false, "assigning non-array (%s) to an array", PChzFromTink(pTinRhs->m_tink));
				 return nullptr;
			}

			auto pTinaryLhs = (STypeInfoArray *)pTinLhs;
			switch (pTinaryLhs->m_aryk)
			{
			case ARYK_Fixed:
				{
					EWC_ASSERT(arykRhs == ARYK_Fixed, "cannot copy mixed array kinds to fixed array");

					CIRProcedure * pProc = pBuild->m_pProcCur;

					auto pValZero = pBuild->PConstInt(0, 64, false);
					auto pValOne = pBuild->PConstInt(0,  64, false);
					auto pValCount = pBuild->PConstInt(pTinaryLhs->m_c, 64, false);
					
					auto pValAlloca = pBuild->PValCreateAlloca(LLVMInt64Type(), 0, "iInit");
					(void) pBuild->PInstCreateStore(pValAlloca, pValZero);

					CIRBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "copyPred");
					CIRBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "copyBody");
					CIRBlock * pBlockPost = pBuild->PBlockCreate(pProc, "copyPost");

					pBuild->CreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPred);
					auto pInstLoadIndex = pBuild->PInstCreate(IROP_Load, pValAlloca, "iLoad");
					auto pInstCmp = pBuild->PInstCreateNCmp(NPRED_ULT, pInstLoadIndex, pValCount, "NCmp");

					(void) pBuild->PInstCreateCondBranch(pInstCmp, pBlockBody, pBlockPost);

					pBuild->ActivateBlock(pBlockBody);

					LLVMOpaqueValue * apLvalIndex[2] = {};
					apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);
					apLvalIndex[1] = pInstLoadIndex->m_pLval;
					auto pInstGEPLhs = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "GEPLhs");
					auto pInstGEPRhs = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "GEPRhs");
					auto pInstLoadRhs = pBuild->PInstCreate(IROP_Load, pInstGEPRhs, "loadRhs");
					(void) pBuild->PInstCreateStore(pInstGEPLhs, pInstLoadRhs);

					auto pInstInc = pBuild->PInstCreate(IROP_NAdd, pInstLoadIndex, pValOne, "iInc");
					(void) pBuild->PInstCreateStore(pValAlloca, pInstInc);
					pBuild->CreateBranch(pBlockPred);

					pBuild->ActivateBlock(pBlockPost);

					return nullptr;

				} break;
			case ARYK_Reference:
				{
					LLVMOpaqueValue * apLvalIndex[2] = {};
					apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);

					CIRValue * pValCount = nullptr;
					CIRValue * pValData = nullptr;
					switch (arykRhs)
					{
					case ARYK_Fixed:
						{
							pValCount = pBuild->PConstInt(cRhs);

							apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 0, false);
							pValData = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, EWC_DIM(apLvalIndex), "aryGep");
						} break;
					case ARYK_Reference:
						{
							apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), ARYMEMB_Count, false);
							auto pInstGepCount = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "gepCount");
							pValCount = pBuild->PInstCreate(IROP_Load, pInstGepCount, "loadC");

							apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), ARYMEMB_Data, false);
							auto pInstGepData = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "gepData");
							pValData = pBuild->PInstCreate(IROP_Load, pInstGepData, "loadData");
						} break;
					default: EWC_ASSERT(false, "Unhandled ARYK"); 
					}

					apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 0, false);
					auto pInstGepCount = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "gepCount");
					(void) pBuild->PInstCreateStore(pInstGepCount, pValCount);

					apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 1, false);
					auto pInstGepData = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "gepData");
					return pBuild->PInstCreateStore(pInstGepData, pValData);

				} break;
			default: EWC_ASSERT(false, "Unhandled ARYK"); 
			}
			*/
		} break;
		case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTinLhs;

			return pBuild->PInstCreateMemcpy(pWork, pTinstruct, pValLhs, pValRhsRef);
		} break;
		default:
		{
			auto pInstRhsLoad = pBuild->PInstCreate(IROP_Load, pValRhsRef, "loadRhs");
			return pBuild->PInstCreateStore(pValLhs, pInstRhsLoad);
		}
	}

	return nullptr;
}

template <typename BUILD>
typename BUILD::Instruction * PInstGenerateAssignment(
	CWorkspace * pWork,
	BUILD * pBuild,
	STypeInfo * pTinLhs,
	typename  BUILD::Value * pValLhs,
	CSTNode * pStnodRhs)
{
	switch (pTinLhs->m_tink)
	{
	case TINK_Array:
		{
			if (pStnodRhs->m_pTin->m_tink == TINK_Literal)
			{
				auto pTinlitRhs = (STypeInfoLiteral *)pStnodRhs->m_pTin;
				auto pValRhsRef = PValGenerateArrayLiteralReference(pWork, pBuild, pStnodRhs);

				auto pTinaryLhs = (STypeInfoArray *)pTinLhs;
				if (pTinaryLhs->m_aryk == ARYK_Reference)
				{
					// BB - Allocating this on the stack feels a little dicey, but so does returning a reference to an
					//  array that happens to be static and isn't read-only.
						
					u64 cElement;
					auto pLtype = PLtypeFromPTin(pBuild, pTinlitRhs, &cElement);
					if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
						return nullptr;

					auto pValAlloca = pBuild->PValCreateAlloca(pLtype, cElement, "aryLit");

					// copy the literal into memory
					pBuild->PInstCreateMemcpy(pWork, pStnodRhs->m_pTin, pValAlloca, pValRhsRef);

					return PInstGenerateAssignmentFromRef(pWork, pBuild, pTinLhs, pStnodRhs->m_pTin, pValLhs, pValAlloca);
				}

				return pBuild->PInstCreateMemcpy(pWork, pStnodRhs->m_pTin, pValLhs, pValRhsRef);
			}

			auto pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Reference);
			return PInstGenerateAssignmentFromRef(pWork, pBuild, pTinLhs, pStnodRhs->m_pTin, pValLhs, pValRhsRef);
		} break;
	case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTinLhs;
			auto ivalkRhs = IvalkCompute(pStnodRhs);
			if (ivalkRhs < IVALK_LValue)
			{
				u64 cElement;
				auto pLtypeLhs = PLtypeFromPTin(pBuild, pTinLhs, &cElement);
				if (!EWC_FVERIFY(pLtypeLhs, "couldn't find llvm type for declaration"))
					return nullptr;

				auto pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);
				return pBuild->PInstCreateStore(pValLhs, pValRhs);
			}

			auto pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Reference);
			return pBuild->PInstCreateMemcpy(pWork, pTinstruct, pValLhs, pValRhsRef);
		} break;
	case TINK_Flag:
		{
			// we can't handle flags here, as we don't have the flag constant or loose enum type
			EWC_ASSERT(false, "enum_flag instance assignments must be handled rather than callin PInstGenerateAssignment");

		} break;
	default: 
		{
			auto pValRhsCast = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodRhs, pTinLhs);
			EWC_ASSERT(pValRhsCast, "bad cast");

			(void) pBuild->PInstCreateTraceStore(pValRhsCast, pTinLhs);
			return pBuild->PInstCreateStore(pValLhs, pValRhsCast);
		}
	}
	return nullptr;
}



template <typename BUILD>
void GeneratePredicate(
	CWorkspace * pWork,
	BUILD * pBuild,
	CSTNode * pStnodPred,
	typename BUILD::Block * pBlockTrue,
	typename BUILD::Block * pBlockPost,
	STypeInfo * pTinBool)
{
	EWC_ASSERT(pTinBool->m_tink == TINK_Bool, "expected bool type for predicate");

	// short circuiting: 
	//  jump to true if either operand to || is true
	//  jump to post if either operand to && is false 

	CSTNode * pStnodOp = pStnodPred;
	if (pStnodOp->m_park == PARK_LogicalAndOrOp)
	{
		BUILD::Block *	pBlockRhs = pBuild->PBlockCreate(pBuild->m_pProcCur, "predRhs");
		EWC_ASSERT(pStnodOp->CStnodChild() == 2, "expected two children for logical op");

		auto pStnodChildLhs = pStnodPred->PStnodChild(0);
		auto pStnodChildRhs = pStnodPred->PStnodChild(1);

		switch (pStnodOp->m_tok)
		{
			case TOK_AndAnd:
				{
					GeneratePredicate(pWork, pBuild, pStnodChildLhs, pBlockRhs, pBlockPost, pTinBool);
					pBuild->ActivateBlock(pBlockRhs);
					GeneratePredicate(pWork, pBuild, pStnodChildRhs, pBlockTrue, pBlockPost, pTinBool);
				} break;
			case TOK_OrOr:
				{
					GeneratePredicate(pWork, pBuild, pStnodChildLhs, pBlockTrue, pBlockRhs, pTinBool);
					pBuild->ActivateBlock(pBlockRhs);
					GeneratePredicate(pWork, pBuild, pStnodChildRhs, pBlockTrue, pBlockPost, pTinBool);
				} break;
			default: EWC_ASSERT(false, "unknown logical op");
		}
	}
	else
	{
		BUILD::Value * pValPred = PValGenerate(pWork, pBuild, pStnodPred, VALGENK_Instance);
		BUILD::Value * pValPredCast = PValCreateCast(pWork, pBuild, pValPred, pStnodPred->m_pTin, pTinBool);
		if (!pValPredCast)
		{
			EmitError(pWork, &pStnodPred->m_lexloc, ERRID_BadCastGen, 
				"INTERNAL ERROR: trying to codegen unsupported numeric cast in predicate.");
		}
		else
		{
			(void)pBuild->PInstCreateCondBranch(pValPredCast, pBlockTrue, pBlockPost);
		}
	}
}

template <typename BUILD>
void GenerateMethodBody(
	CWorkspace * pWork,
	BUILD * pBuild,
	typename BUILD::Proc * pProc,
	CSTNode ** apStnodBody, 
	int cpStnodBody,
	bool fNeedsNullReturn)
{
	EWC_ASSERT(pProc && apStnodBody && cpStnodBody, "bad parameters to GenerateMethodBody");

	auto pDif = PDifEnsure(pWork, pBuild, apStnodBody[0]->m_lexloc.m_strFilename.PCoz());
	PushDIScope(pDif, PLvalDInfo(pProc));

	pBuild->ActivateProc(pProc, pProc->m_pBlockLocals);
	pBuild->ActivateBlock(pProc->m_pBlockFirst);

	for (int ipStnodBody = 0; ipStnodBody < cpStnodBody; ++ipStnodBody)
	{
		(void) PValGenerate(pWork, pBuild, apStnodBody[ipStnodBody], VALGENK_Instance);
	}

	if (fNeedsNullReturn)
	{
		pBuild->CreateReturn(nullptr, 0, "RetTmp");
	}


	pBuild->ActivateBlock(pProc->m_pBlockLocals);
	pBuild->CreateBranch(pProc->m_pBlockFirst);

	PopDIScope(pDif, PLvalDInfo(pProc));

	pBuild->ActivateProc(nullptr, nullptr);

	pBuild->FinalizeProc(pProc);
}

void CBuilderIR::FinalizeProc(CIRProcedure * pProc)
{
	m_arypProcVerify.Append(pProc);
}

// helper routine for generating operators, used to make sure type checking errors are in sync with the code generator
struct SOperatorInfo // tag = opinfo
{
					SOperatorInfo()
					:m_irop(IROP_Nil)
					,m_npred(NPRED_Nil)
					,m_gpred(GPRED_Nil)
					,m_fNegateFirst(false)
					,m_pChzName(nullptr)
						{ ; }

	IROP			m_irop;
	NPRED			m_npred;
	GPRED			m_gpred;
	bool			m_fNegateFirst;
	const char *	m_pChzName;
};

void CreateOpinfo(IROP irop, const char * pChzName, SOperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = irop;
	pOpinfo->m_pChzName = pChzName;
}

void CreateOpinfo(NPRED npred, const char * pChzName, SOperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = IROP_NCmp;
	pOpinfo->m_npred = npred;
	pOpinfo->m_pChzName = pChzName;
}

void CreateOpinfo(GPRED gpred, const char * pChzName, SOperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = IROP_GCmp;
	pOpinfo->m_gpred = gpred;
	pOpinfo->m_pChzName = pChzName;
}

static void GenerateOperatorInfo(TOK tok, const SOpTypes * pOptype, SOperatorInfo * pOpinfo)
{
	STypeInfo * apTin[2] = {PTinStripQualifiers(pOptype->m_pTinLhs), PTinStripQualifiers(pOptype->m_pTinRhs)};
	bool aFIsSigned[2];
	TINK aTink[2];

	for (int iOperand = 0; iOperand < 2; ++iOperand)
	{
		bool fIsSigned = true;
		TINK tink = apTin[iOperand]->m_tink;

		if (tink == TINK_Literal)
		{
			STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)apTin[iOperand];
			fIsSigned = pTinlit->m_litty.m_fIsSigned;

			switch (pTinlit->m_litty.m_litk)
			{
			case LITK_Integer:	tink = TINK_Integer;	break;
			case LITK_Float:	tink = TINK_Float;		break;
			case LITK_Enum:		tink = TINK_Enum;		break;
			case LITK_Bool:		tink = TINK_Bool;		break;
			case LITK_Array:	tink = TINK_Array;		break;
			default:			tink = TINK_Nil;
			}
		}
		else if (tink == TINK_Enum)
		{
			auto pTinenum = (STypeInfoEnum*)apTin[iOperand];
			if (EWC_FVERIFY(pTinenum->m_pTinLoose && pTinenum->m_pTinLoose->m_tink == TINK_Integer, "expected integer loose type"))
			{
				fIsSigned = ((STypeInfoInteger *)pTinenum->m_pTinLoose)->m_fIsSigned;
			}
		}
		else if (tink == TINK_Integer)
		{
			fIsSigned = ((STypeInfoInteger *)apTin[iOperand])->m_fIsSigned;
		}
		
		aFIsSigned[iOperand] = fIsSigned;
		aTink[iOperand] = tink;
	}

	if (aTink[0] != aTink[1])
	{
		TINK tinkMin = aTink[0];
		TINK tinkMax = aTink[1];
		if (tinkMin > tinkMax)
		{
			ewcSwap(tinkMin, tinkMax);
		}

		if (tinkMin == TINK_Pointer && tinkMax == TINK_Array)
		{
			// BB- check that it's a pointer to the array type.
			switch((u32)tok)
			{
			case '=':
				{
					CreateOpinfo(IROP_Store, "store", pOpinfo);
				} break;
			case '-': 				
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			case TOK_EqualEqual:
				CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); 
				break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			}
		}
		else if (tinkMin == TINK_Integer && tinkMax == TINK_Array)
		{
			switch((u32)tok)
			{
			case '+':				
				{
					CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo);
				} break;
			case '-': 				
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			}
		}
		else if (tinkMin == TINK_Integer && tinkMax == TINK_Pointer)
		{
			switch((u32)tok)
			{
			case '+':				
				{
					CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo);
				} break;
			case '-': 				
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			case TOK_PlusEqual:
				{
					CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo);
				} break;
			case TOK_MinusEqual:
				{
					CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
					pOpinfo->m_fNegateFirst = true;
				} break;
			}
		}
		else if (tinkMin == TINK_Integer && tinkMax == TINK_Enum)
		{
			bool fIsSigned = (aTink[0] == tinkMax) ? aFIsSigned[0] : aFIsSigned[1];
			switch (tok)
			{
				case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
										CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
				case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
				case TOK_AndEqual:
				case '&':				CreateOpinfo(IROP_And, "rAndTmp", pOpinfo); break;
				case TOK_OrEqual:
				case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
				case TOK_XorEqual:
				case '^':				CreateOpinfo(IROP_Xor, "nXorTmp", pOpinfo); break;
				default:
					EWC_ASSERT(false, "Unhandled TOK");
					break;
			}
		}

		return;
	}

	TINK tink = aTink[0];
	bool fIsSigned = aFIsSigned[0];

	switch (tink)
	{
	case TINK_Flag:
		switch ((u32)tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			case TOK_AndAnd:		CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
			case TOK_OrOr:			CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
		} break;
	case TINK_Bool:
		switch ((u32)tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			case TOK_AndEqual:
			case '&':				CreateOpinfo(IROP_And, "nAndTmp", pOpinfo); break;
			case TOK_OrEqual:
			case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
			case TOK_AndAnd:		CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
			case TOK_OrOr:			CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
		} break;
	case TINK_Integer:
		switch ((u32)tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_PlusEqual:
			case '+': 				CreateOpinfo(IROP_NAdd, "nAddTmp", pOpinfo); break;
			case TOK_MinusEqual:
			case '-': 				CreateOpinfo(IROP_NSub, "nSubTmp", pOpinfo); break;
			case TOK_MulEqual:
			case '*': 				CreateOpinfo(IROP_NMul, "nMulTmp", pOpinfo); break;
			case TOK_DivEqual:
			case '/':				CreateOpinfo((fIsSigned) ? IROP_SDiv : IROP_UDiv, "nDivTmp", pOpinfo); break;
			case TOK_ModEqual:
			case '%':				CreateOpinfo((fIsSigned) ? IROP_SRem : IROP_URem, "nRemTmp", pOpinfo); break;
			case TOK_AndEqual:
			case '&':				CreateOpinfo(IROP_And, "rAndTmp", pOpinfo); break;
			case TOK_OrEqual:
			case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
			case TOK_XorEqual:
			case '^':				CreateOpinfo(IROP_Xor, "nXorTmp", pOpinfo); break;
			case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
									CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
			case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			case TOK_LessEqual:
				if (fIsSigned)	CreateOpinfo(NPRED_SLE, "CmpSLE", pOpinfo);
				else			CreateOpinfo(NPRED_ULE, "NCmpULE", pOpinfo);
				break;
			case TOK_GreaterEqual:
				if (fIsSigned)	CreateOpinfo(NPRED_SGE, "NCmpSGE", pOpinfo);
				else			CreateOpinfo(NPRED_UGE, "NCmpUGE", pOpinfo);
				break;
			case '<':
				if (fIsSigned)	CreateOpinfo(NPRED_SLT, "NCmpSLT", pOpinfo);
				else			CreateOpinfo(NPRED_ULT, "NCmpULT", pOpinfo);
				break;
			case '>':
				if (fIsSigned)	CreateOpinfo(NPRED_SGT, "NCmpSGT", pOpinfo);
				else			CreateOpinfo(NPRED_UGT, "NCmpUGT", pOpinfo);
				break;
		} break;
	case TINK_Float:
		switch ((u32)tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_PlusEqual:
			case '+': 				CreateOpinfo(IROP_GAdd, "gAddTmp", pOpinfo); break;
			case TOK_MinusEqual:
			case '-': 				CreateOpinfo(IROP_GSub, "SubTmp", pOpinfo); break;
			case TOK_MulEqual:
			case '*': 				CreateOpinfo(IROP_GMul, "gMulTmp", pOpinfo); break;
			case TOK_DivEqual:
			case '/': 				CreateOpinfo(IROP_GDiv, "gDivTmp", pOpinfo); break;
			case TOK_ModEqual:
			case '%': 				CreateOpinfo(IROP_GRem, "gRemTmp", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(GPRED_EQ, "GCmpEQ", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(GPRED_NE, "GCmpNE", pOpinfo); break;
			case TOK_LessEqual:		CreateOpinfo(GPRED_LE, "GCGpLE", pOpinfo); break;
			case TOK_GreaterEqual:	CreateOpinfo(GPRED_GE, "GCmpGE", pOpinfo); break;
			case '<': 				CreateOpinfo(GPRED_LT, "GCmpLT", pOpinfo); break;
			case '>': 				CreateOpinfo(GPRED_GT, "GCmpGT", pOpinfo); break;
		} break;
	case TINK_Procedure:
		{
			switch ((u32)tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
			}
		} break;
	case TINK_Struct:
		{
			switch ((u32)tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			}
		} break;
	case TINK_Pointer:
	case TINK_Array:
		{
			switch ((u32)tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case '-': 			
					{
						CreateOpinfo(IROP_PtrDiff, "ptrDif", pOpinfo);
					} break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
/*				case TOK_PlusEqual:		CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_MinusEqual:	CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
				case '+=':				CreateOpinfo(IROP_GEP, "ptrAdd", pOpinfo); break;
				case '-=': 				
					{
						CreateOpinfo(IROP_GEP, "ptrSub", pOpinfo);
						pOpinfo->m_fNegateFirst = true;
					} break;
					*/
			}
		} break;
	case TINK_Enum:
		{
			auto pTinenum = PTinRtiCast<STypeInfoEnum *>(apTin[0]);
			if (pTinenum->m_enumk == ENUMK_Basic)
			{
				// BB - why is the RHS still a literal here?
				//EWC_ASSERT(FTypesAreSame(pTinLhs, pTinRhs), "enum comparison type mismatch");

				// BB - Why no plus equals here?

				switch ((u32)tok)
				{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
				case '+': 				CreateOpinfo(IROP_NAdd, "nAddTmp", pOpinfo); break;
				case '-': 				CreateOpinfo(IROP_NSub, "nSubTmp", pOpinfo); break;
				case '%':				CreateOpinfo((fIsSigned) ? IROP_SRem : IROP_URem, "nRemTmp", pOpinfo); break;
				case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
					CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
				case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
				case TOK_LessEqual:
					if (fIsSigned)		CreateOpinfo(NPRED_SLE, "CmpSLE", pOpinfo);
					else				CreateOpinfo(NPRED_ULE, "NCmpULE", pOpinfo);
					break;
				case TOK_GreaterEqual:
					if (fIsSigned)		CreateOpinfo(NPRED_SGE, "NCmpSGE", pOpinfo);
					else				CreateOpinfo(NPRED_UGE, "NCmpUGE", pOpinfo);
					break;
				case '<':
					if (fIsSigned)		CreateOpinfo(NPRED_SLT, "NCmpSLT", pOpinfo);
					else				CreateOpinfo(NPRED_ULT, "NCmpULT", pOpinfo);
					break;
				case '>':
					if (fIsSigned)		CreateOpinfo(NPRED_SGT, "NCmpSGT", pOpinfo);
					else				CreateOpinfo(NPRED_UGT, "NCmpUGT", pOpinfo);
					break;
				}
			}
			else
			{
				EWC_ASSERT(pTinenum->m_enumk == ENUMK_FlagEnum, "Unhandled enumk");

				switch ((u32)tok)
				{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NPRED_EQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NPRED_NE, "NCmpNq", pOpinfo); break;
				case TOK_ShiftRight:	// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
					CreateOpinfo((fIsSigned) ? IROP_AShr : IROP_LShr, "nShrTmp", pOpinfo); break;
				case TOK_ShiftLeft:		CreateOpinfo(IROP_Shl, "nShlTmp", pOpinfo); break;
				case TOK_AndEqual:
				case '&':				CreateOpinfo(IROP_And, "rAndTmp", pOpinfo); break;
				case TOK_OrEqual:
				case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
				case TOK_XorEqual:
				case '^':				CreateOpinfo(IROP_Xor, "nXorTmp", pOpinfo); break;
				}
			}
		} break;
	default: 
		break;
	}
}

bool FDoesOperatorExist(TOK tok, const SOpTypes * pOptype)
{
	SOperatorInfo opinfo;
	GenerateOperatorInfo(tok, pOptype, &opinfo);

	return opinfo.m_irop != IROP_Nil;
}

template <typename BUILD>
static inline typename BUILD::Instruction * PInstGenerateOperator(
	BUILD * pBuild,
	TOK tok,
	const SOpTypes * pOptype,
	typename BUILD::Value * pValLhs,
	typename BUILD::Value * pValRhs)
{
	SOperatorInfo opinfo;
	GenerateOperatorInfo(tok, pOptype, &opinfo);
	if (!EWC_FVERIFY(opinfo.m_irop != IROP_Store, "bad optype"))
	{
		// IROP_Store is just used to signal that a store operation exists, but codegen should call CreateStore rather
		//  than this function
		return nullptr;
	}

	BUILD::Instruction * pInstOp = nullptr;
	switch (opinfo.m_irop)
	{
	case IROP_GEP:		// for pointer arithmetic
		{
			if (opinfo.m_fNegateFirst)
			{
				pValRhs = pBuild->PInstCreate(IROP_NNeg, pValRhs, "NNeg");
			}

			auto pLvalIndex = pBuild->PGepIndexFromValue(pValRhs);
			pInstOp = pBuild->PInstCreateGEP(pValLhs, &pLvalIndex, 1, opinfo.m_pChzName); break;
		} break;
	case IROP_PtrDiff:
		{
			u64 cBitSize;
			u64 cBitAlign;

			auto pTinptr = PTinDerivedCast<STypeInfoPointer*>(pOptype->m_pTinLhs);
			auto pLtype = pBuild->PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);

			if (!EWC_FVERIFY(pOptype->m_pTinResult->m_tink == TINK_Integer, "Expected integer type result"))
				return nullptr;

			auto pTinintResult = PTinRtiCast<STypeInfoInteger *>(pOptype->m_pTinResult);

			pValLhs = pBuild->PInstCreatePtrToInt(pValLhs, pTinintResult, "PDifL");
			pValRhs = pBuild->PInstCreatePtrToInt(pValRhs, pTinintResult, "PDifR");
			pInstOp = pBuild->PInstCreate(IROP_NSub, pValLhs, pValRhs, "PtrSub");

			if (cBitSize > 8)
			{
				auto pConst = pBuild->PConstInt(cBitSize / 8, pTinintResult->m_cBit, false);
				pInstOp = pBuild->PInstCreate(IROP_SDiv, pInstOp, pConst, "PtrDif");
			}
		} break;
	case IROP_NCmp:		pInstOp = pBuild->PInstCreateNCmp(opinfo.m_npred, pValLhs, pValRhs, opinfo.m_pChzName); break;
	case IROP_GCmp:		pInstOp = pBuild->PInstCreateGCmp(opinfo.m_gpred, pValLhs, pValRhs, opinfo.m_pChzName); break;
	default:			pInstOp = pBuild->PInstCreate(opinfo.m_irop, pValLhs, pValRhs, opinfo.m_pChzName); break;
	}

	// Note: This should be caught by the type checker! This function should match FDoesOperatorExist
	EWC_ASSERT(pInstOp, "unexpected op in PInstGenerateOperator '%'", PCozFromTok(tok));
	return pInstOp;
}

static inline bool FIsOverloadedOp(CSTNode * pStnod)
{
	return pStnod->m_pOptype && pStnod->m_pOptype->m_pTinprocOverload;
}

template <typename BUILD>
typename BUILD::Value * PValGenerateDecl(
	CWorkspace * pWork,
	BUILD * pBuild,
	CSTNode * pStnod,
	CSTNode * pStnodInit,
	VALGENK valgenk)
{
	auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
	if (!pStdecl || !EWC_FVERIFY(pStnod->m_pSym, "declaration without symbol"))
		return nullptr;

	u64 cElement;
	auto pLtype = PLtypeFromPTin(pBuild, pStnod->m_pTin, &cElement);
	if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
		return nullptr;

	u64 cBitSize, cBitAlign;
	CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);
	if (cBitSize == 0)
	{
		EmitError(pWork, &pStnod->m_lexloc, ERRID_ZeroSizeInstance,
			"Could not instantiate %s, zero-sized instances are not allowed", pStnod->m_pSym->m_strName.PCoz());
		return nullptr;
	}

	s32 iLine, iCol;
	auto pDif = PDifEmitLocation(pWork, pBuild, pStnod->m_lexloc, &iLine, &iCol);

	auto pLvalScope = PLvalFromDIFile(pBuild, pDif);

	CreateDebugInfo(pWork, pBuild, pStnod, pStnod->m_pTin);

	bool fIsGlobal = pBuild->m_pProcCur == nullptr;
	auto strName = pStnod->m_pSym->m_strName;
	auto strPunyName = StrPunyEncode(strName.PCoz());
	if (fIsGlobal)
	{
		auto pGlob = pBuild->PGlobCreate(pLtype, strPunyName.PCoz());
		pBuild->SetSymbolValue(pStnod->m_pSym, pGlob);

		DInfoCreateGlobalVariable(pBuild, pStnod->m_pTin, pLvalScope, pDif, strName.PCoz(), strPunyName.PCoz(), iLine, true, pGlob);

		BUILD::LValue * pLvalInit = nullptr;
		if (pStnodInit)
		{
			auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInit->m_pTin);
			if (pTinlit && pStnodInit->m_park != PARK_Uninitializer)
			{
				pLvalInit = PLvalFromLiteral(pBuild, pTinlit, pStnodInit);
			}

			// Not supporting globals that require some runtime init
			//return PValInitialize(pWork, pBuild, pStnod->m_pTin, pGlob, pStnodInit);

			// Also - not handling globals that need to call an overloaded := operator
		}

		// yuck, we're doing this check for every global variable?!?

		if (FAreCozEqual(strName.PCoz(),STypeInfo::s_pChzGlobalTinTable))
		{
			pLvalInit = PLvalGenerateReflectTypeTable(pWork, pBuild);
		}

		if (!pLvalInit)
		{
			pLvalInit = PLvalBuildConstantInitializer(pWork, pBuild, pStnod->m_pTin, pStnodInit);
		}

		SetInitializer(pGlob, pLvalInit);

		return pGlob;
	}
	else
	{
		// generate the local variable into the first basic block for procedure

		auto pBlockCur = pBuild->m_pBlockCur;
		pBuild->ActivateBlock(pBuild->m_pProcCur->m_pBlockLocals);
		auto pValAlloca = pBuild->PValCreateAlloca(pLtype, cElement, strPunyName.PCoz());
		pBuild->ActivateBlock(pBlockCur);


		pBuild->SetSymbolValue(pStnod->m_pSym, pValAlloca);
	
		auto pLvalDIVariable = PLvallDInfoCreateAutoVariable(pBuild, pStnod->m_pTin, pLvalScope, pDif, strPunyName.PCoz(), iLine, false, 0);

		DInfoInsertDeclare(pBuild, pValAlloca, pLvalDIVariable, pLvalScope, iLine, iCol, pBuild->m_pBlockCur);

		if (FIsOverloadedOp(pStnod))
		{
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
			if (EWC_FVERIFY(pStdecl && pStdecl->m_iStnodIdentifier >= 0 && pStdecl->m_iStnodInit >= 0,
					"bad declaration"))
			{
				CDynAry<BUILD::ProcArg *> arypLvalArgs(pBuild->m_pAlloc, EWC::BK_Stack);

				auto pTinproc = pStnod->m_pOptype->m_pTinprocOverload;

				//BB - This won't work with generic procedure overloads
				auto pSym = pTinproc->m_pStnodDefinition->m_pSym;

				auto pTinptr = PTinRtiCast<STypeInfoPointer*>(pTinproc->m_arypTinParams[0]);
				EWC_ASSERT(pTinptr, "exected pointer type for implicit reference");

				auto pValArg = PValGenerate(pWork, pBuild, pStnod->m_arypStnodChild[pStdecl->m_iStnodIdentifier], VALGENK_Reference);
				arypLvalArgs.Append(PLvalFromPVal(pValArg));

				auto pTinRhs = pTinproc->m_arypTinParams[1];
				pValArg = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnod->m_arypStnodChild[pStdecl->m_iStnodInit], pTinRhs);
				arypLvalArgs.Append(PLvalFromPVal(pValArg));

				return pBuild->PValGenerateCall(pWork, pStnod, pSym, &arypLvalArgs, true, pTinproc, valgenk);
			}
		}

		return PValInitialize(pWork, pBuild, pStnod->m_pTin, pValAlloca, pStnodInit);
	}
}

CIRValue * PValGenerateArrayLiteralReference(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnod)
{
	// constant decl's don't actually generate anything until referenced.
	if (pStnod->m_park == PARK_ConstantDecl)
		return nullptr;

	STypeInfoLiteral * pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnod->m_pTin);
	if (!pTinlit)
		return nullptr;

	if (!EWC_FVERIFY(pTinlit->m_litty.m_litk == LITK_Array, "expected array literal"))
		return nullptr;

	if (pTinlit->m_pGlob == nullptr)
	{
		auto pLvalConstInit = PLvalFromLiteral(pBuild, pTinlit, pStnod);
		if (!pLvalConstInit)
			return nullptr;

		auto pLtypeArray = pBuild->PLtypeFromPTin(pTinlit);

		auto pGlob = pBuild->PGlobCreate(pLtypeArray, "");
		LLVMSetGlobalConstant(pGlob->m_pLval, true);

		SetInitializer(pGlob, pLvalConstInit);
		pTinlit->m_pGlob = pGlob;
	}
	return pTinlit->m_pGlob;
}

BCode::SValue * PValGenerateArrayLiteralReference(CWorkspace * pWork, BCode::CBuilder * pBuild, CSTNode * pStnod)
{
	EWC_ASSERT(false, "Bytecode TBD");
	return nullptr;
}

CIRValue * PValGenerateLiteral(CWorkspace * pWork, CBuilderIR * pBuildIr, CSTNode * pStnod)
{
	// constant decl's don't actually generate anything until referenced.
	if (pStnod->m_park == PARK_ConstantDecl)
		return nullptr;

	STypeInfoLiteral * pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnod->m_pTin);
	if (!pTinlit)
		return nullptr;

	auto pLval = PLvalFromLiteral(pBuildIr, pTinlit, pStnod);
	if (!pLval)
		return nullptr;

	CIRConstant * pConst = EWC_NEW(pBuildIr->m_pAlloc, CIRConstant) CIRConstant();
	pBuildIr->AddManagedVal(pConst);
	pConst->m_pLval = pLval;
	return pConst;
}

BCode::SValue * PValGenerateLiteral(CWorkspace * pWork, BCode::CBuilder * pBuildBc, CSTNode * pStnod)
{
	// constant decl's don't actually generate anything until referenced.
	if (pStnod->m_park == PARK_ConstantDecl)
		return nullptr;

	STypeInfoLiteral * pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnod->m_pTin);
	if (!pTinlit)
		return nullptr;

	auto pLval = PLvalFromLiteral(pBuildBc, pTinlit, pStnod);
	return pLval;
}

void GenerateArguments(
	CWorkspace * pWork,
	CBuilderIR * pBuild,
	GRFSTNOD grfstnod,
	STypeInfoProcedure * pTinproc, 
	size_t cpStnodArg,
	CSTNode ** ppStnodArg,
	CDynAry<LLVMValueRef> * parypLvalArgs,
	CDynAry<CIRValue *> * parypValArgs = nullptr)
{
	// what's with passing both a val array and a LVal array arguments? we need the LVal array for llvm, the val for postInc op overload

	for (size_t ipStnodChild = 0; ipStnodChild < cpStnodArg; ++ipStnodChild)
	{
		CSTNode * pStnodArg = ppStnodArg[ipStnodChild];

		STypeInfo * pTinParam = pStnodArg->m_pTin;

		size_t cpTinParam = pTinproc->m_arypTinParams.C();
		if (ipStnodChild < cpTinParam)
		{
			size_t ipStnodChildAdj = (grfstnod.FIsSet(FSTNOD_CommutativeCall)) ? (cpTinParam - 1 - ipStnodChild) : ipStnodChild;
			pTinParam = pTinproc->m_arypTinParams[ipStnodChildAdj];
		}

		CIRValue * pValRhsCast = nullptr;
		if (ipStnodChild < pTinproc->m_mpIptinGrfparmq.C() && 
			pTinproc->m_mpIptinGrfparmq[ipStnodChild].FIsSet(FPARMQ_ImplicitRef))
		{
			auto pTinptr = PTinRtiCast<STypeInfoPointer*>(pTinproc->m_arypTinParams[ipStnodChild]);
			EWC_ASSERT(pTinptr, "exected pointer type for implicit reference");

			pValRhsCast = PValGenerate(pWork, pBuild, pStnodArg, VALGENK_Reference);
		}
		else
		{
			pValRhsCast = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodArg, pTinParam);
		}

		parypLvalArgs->Append(pValRhsCast->m_pLval);
		if (parypValArgs)
		{
			parypValArgs->Append(pValRhsCast);
		}

		EWC_ASSERT(*parypLvalArgs->PLast(), "missing argument value");
	}

	if (grfstnod.FIsSet(FSTNOD_CommutativeCall))
	{
		ReverseArray(parypLvalArgs->A(), parypLvalArgs->C());
		if (parypValArgs)
		{
			ReverseArray(parypValArgs->A(), parypValArgs->C());
		}
	}
}

void GenerateArguments(
	CWorkspace * pWork,
	BCode::CBuilder * pBuild,
	GRFSTNOD grfstnod,
	STypeInfoProcedure * pTinproc,
	size_t cpStnodArg,
	CSTNode ** ppStnodArg,
	CDynAry<BCode::CBuilder::ProcArg *> * parypLvalArgs,
	CDynAry<BCode::SValue *> * parypValArgs = nullptr)
{
	// what's with passing both a val array and a LVal array arguments? we need the LVal array for llvm, the val for postInc op overload
	EWC_ASSERT(false, "bytecode tbd");
}

CBuilderIR::ProcArg * CBuilderIR::PProcArg(CIRValue * pVal)
{
	return pVal->m_pLval;
}

CIRValue * CBuilderIR::PValGenerateCall(
	CWorkspace * pWork,
	CSTNode * pStnod,
	SSymbol * pSym,
	EWC::CDynAry<LLVMValueRef> * parypLvalArgs,
	bool fIsDirectCall,
	STypeInfoProcedure * pTinproc, 
	VALGENK valgenk)
{
	if (!EWC_FVERIFY(pTinproc->m_pStnodDefinition, "procedure missing definition"))
		return nullptr;

	auto pValSym = PValFromSymbol(pSym);

	if (fIsDirectCall)
	{
		if (!EWC_FVERIFY(pSym, "calling function without generated code"))
			return nullptr;

		EWC_ASSERT(pSym->m_symdep == SYMDEP_Used, "Calling function thought to be unused '%s'", pTinproc->m_strName.PCoz());

		PProcTryEnsure(pWork, this, pSym);

		if (!pValSym)
			return nullptr;
	}

	EmitLocation(pWork, this, pStnod->m_lexloc);
	EWC_ASSERT(valgenk != VALGENK_Reference, "cannot return reference value for procedure call");

	CIRInstruction * pInst = PInstCreateRaw(IROP_Call, nullptr, nullptr, "RetTmp");
	if (FIsError(pInst))
		return pInst;

	if (fIsDirectCall)
	{
		CIRProcedure * pProc = (CIRProcedure *)pValSym;
		auto pLvalFunction = pProc->m_pLval;
		if (LLVMCountParams(pLvalFunction) != parypLvalArgs->C())
		{
			if (!EWC_FVERIFY(pTinproc->FHasVarArgs(), "unexpected number of arguments"))
				return nullptr;
		}

		s32 iLine;
		s32 iCol;
		CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLine, &iCol);

		pInst->m_pLval = LLVMBuildCall(
							m_pLbuild,
							pProc->m_pLval,
							parypLvalArgs->A(),
							(u32)parypLvalArgs->C(),
							"");


		if (pTinproc->m_callconv != CALLCONV_Nil)
		{
			LLVMSetInstructionCallConv(pInst->m_pLval, CallingconvFromCallconv(pTinproc->m_callconv));
		}
		return pInst;
	}
	else
	{
		CSTNode * pStnodProcref = pStnod->PStnodChildSafe(0);
		if (!EWC_FVERIFY(pStnodProcref, "expected procedure reference"))
			return nullptr;

		auto pValProcref = PValGenerate(pWork, this, pStnodProcref, VALGENK_Instance);

		pInst->m_pLval = LLVMBuildCall(
							m_pLbuild,
							pValProcref->m_pLval,
							parypLvalArgs->A(),
							(u32)parypLvalArgs->C(),
							"");
		if (pTinproc->m_callconv != CALLCONV_Nil)
		{
			LLVMSetInstructionCallConv(pInst->m_pLval, CallingconvFromCallconv(pTinproc->m_callconv));
		}
		return pInst;
	}
}

BCode::SValue * BCode::CBuilder::PValGenerateCall(
	CWorkspace * pWork,
	CSTNode * pStnod,
	SSymbol * pSym,
	EWC::CDynAry<ProcArg *> * parypValArgs,
	bool fIsDirectCall,
	STypeInfoProcedure * pTinproc,
	VALGENK valgenk)
{
	SValue * pValProc;
	if (fIsDirectCall)
	{
		if (!EWC_FVERIFY(pSym, "calling function without generated code"))
			return nullptr;

		EWC_ASSERT(pSym->m_symdep == SYMDEP_Used, "Calling function thought to be unused %p %s", pSym, pSym->m_strName.PCoz());

		PProcTryEnsure(pWork, this, pSym);

		if (pTinproc->m_arypTinParams.C() != parypValArgs->C())
		{
			if (!EWC_FVERIFY(pTinproc->FHasVarArgs(), "unexpected number of arguments"))
				return nullptr;
		}

		auto pProc = (BCode::SProcedure *)PValFromSymbol(pSym);
		if (!EWC_FVERIFY(pProc, "missing proc codegen"))
			return nullptr;

		pValProc = PConstPointer(pProc);
	}
	else
	{
		CSTNode * pStnodProcref = pStnod->PStnodChildSafe(0);
		if (!EWC_FVERIFY(pStnodProcref, "expected procedure reference"))
			return nullptr;

		pValProc = PValGenerate(pWork, this, pStnodProcref, VALGENK_Instance);
	}

	auto pInst = PInstCreateCall(pValProc, pTinproc, parypValArgs->A(), (int)parypValArgs->C());
	if (pInst->m_irop == IROP_Error)
		return pInst;

	if (pTinproc->m_callconv != CALLCONV_Nil)
	{
		//LLVMSetInstructionCallConv(pInst->m_pLval, CallingconvFromCallconv(pTinproc->m_callconv));
	}
	return pInst;
}

CIRValue * PValGenerateTypeInfo(CBuilderIR * pBuild, CSTNode * pStnod, CSTNode * pStnodChild)
{
	CIRGlobal * pGlob = EWC_NEW(pBuild->m_pAlloc, CIRGlobal) CIRGlobal();
	pBuild->AddManagedVal(pGlob);

	auto pLtypePTin = pBuild->PLtypeFromPTin(pStnod->m_pTin);

	pGlob->m_pLval = LLVMConstPointerCast((LLVMValueRef)pStnodChild->m_pTin->m_pCgvalReflectGlobal, pLtypePTin);
	return pGlob;
}

BCode::SValue * PValGenerateTypeInfo(BCode::CBuilder * pBuild, CSTNode * pStnod, CSTNode * pStnodChild)
{
	EWC_ASSERT(false, "bytecode TBD");
	return nullptr;
}

void GenerateSwitch(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnod, CIRValue * pValExp)
{
	CIRProcedure * pProc = pBuild->m_pProcCur;
	auto pBlockPost = pBuild->PBlockCreate(pProc, "PostSw");
	auto pBlockDefault = pBlockPost;

	auto pJumpt = pBuild->m_aryJumptStack.AppendNew();
	pJumpt->m_pBlockBreak = pBlockPost;
	pJumpt->m_pBlockContinue = nullptr;

	if (pStnod->m_pStident)
	{
		pJumpt->m_strLabel = pStnod->m_pStident->m_str;
	}

	CDynAry<LLVMOpaqueValue *> arypLval(pBuild->m_pAlloc, BK_CodeGen, pStnod->CStnodChild()-1);
	CDynAry<CIRBlock *> arypBlock(pBuild->m_pAlloc, BK_CodeGen, pStnod->CStnodChild()-1);

	u32 cStnodCase = 0;
	for (int iStnodChild = 1; iStnodChild < pStnod->CStnodChild(); ++iStnodChild)
	{
		auto pStnodCase = pStnod->PStnodChild(iStnodChild);
		if (!EWC_FVERIFY(pStnod->m_park == PARK_ReservedWord && pStnod->m_pStval, "bad case statement"))
			continue;

		switch(pStnodCase->m_pStval->m_rword)
		{
			case RWORD_Case:
			{
				auto pStnodCase = pStnod->PStnodChild(iStnodChild);

				int cStnodLit = pStnodCase->CStnodChild()-1; // The last child is the case body (not a literal)
				cStnodCase += cStnodLit;

			} break;
			case RWORD_Else:	
				pBlockDefault = pBuild->PBlockCreate(pProc, "Default");
				break;
			default:
				EWC_ASSERT(false, "Unexpected reserved word in switch");
				break;
		}
	}

	auto pLvalSw = LLVMBuildSwitch(pBuild->m_pLbuild, pValExp->m_pLval, pBlockDefault->m_pLblock, cStnodCase);

	bool fPrevCaseFallsThrough = false;
	for (int iStnodChild = 1; iStnodChild < pStnod->CStnodChild(); ++iStnodChild)
	{
		auto pStnodCase = pStnod->PStnodChild(iStnodChild);
		if (!EWC_FVERIFY(pStnod->m_park == PARK_ReservedWord && pStnod->m_pStval, "bad case statement"))
			continue;

		RWORD rword = pStnodCase->m_pStval->m_rword;

		CIRBlock * pBlockBody = nullptr;
		CSTNode * pStnodBody = nullptr;
		switch (rword)
		{
			case RWORD_Case:
			{
				if (!EWC_FVERIFY(pStnodCase->CStnodChild() >= 2, "expected case (literal, (opt literals...), body)"))
					break;	

				auto pBlockCase = pBuild->PBlockCreate(pProc, "Case");
				int cStnodLit = pStnodCase->CStnodChild()-1; // The last child is the case body (not a literal)
				for (int iStnodLit = 0; iStnodLit < cStnodLit; ++iStnodLit)
				{
					auto pValLit = PValGenerate(pWork, pBuild, pStnodCase->PStnodChild(iStnodLit), VALGENK_Instance);
					arypBlock.Append(pBlockCase);
					arypLval.Append(pValLit->m_pLval);
				}

				pBlockBody = pBlockCase;
				pStnodBody = pStnodCase->PStnodChild(cStnodLit);
			} break;
			case RWORD_Else:
			{
				if (!EWC_FVERIFY(pStnodCase->CStnodChild() == 1, "expected default case to have one child (body)"))
					break;

				pBlockBody = pBlockDefault;
				pStnodBody = pStnodCase->PStnodChild(0);
			} break;
			default:
				EWC_ASSERT(false, "unexpected reserved word during case statement code gen (%s)", PCozFromRword(rword));
		}
		
		if (fPrevCaseFallsThrough)
		{
			pBuild->CreateBranch(pBlockBody);	
		}
		fPrevCaseFallsThrough = pStnodCase->m_grfstnod.FIsSet(FSTNOD_Fallthrough);

		pBuild->ActivateBlock(pBlockBody);
		(void) PValGenerate(pWork, pBuild, pStnodBody, VALGENK_Instance);

		bool fIsLastCase = iStnodChild+1 == pStnod->CStnodChild();
		if (!fPrevCaseFallsThrough || fIsLastCase)

		{
			pBuild->CreateBranch(pBlockPost);	
		}
	}

	for (int ipLval = 0; ipLval < arypLval.C(); ++ipLval)
	{
		LLVMAddCase(pLvalSw, arypLval[ipLval], arypBlock[ipLval]->m_pLblock);
	}
	
	pBuild->m_aryJumptStack.PopLast();
	pBuild->ActivateBlock(pBlockPost);
}

void GenerateSwitch(CWorkspace * pWork, BCode::CBuilder * pBuild, CSTNode * pStnod, BCode::SValue * pValExp)
{
	EWC_ASSERT(false, "bytecode TBD");
}

static inline bool FIsNull(CIRValue * pVal)
{
	return pVal == nullptr || pVal->m_pLval == nullptr;
}

static inline bool FIsNull(BCode::SValue * pVal)
{
	return pVal == nullptr;
}



template <typename BUILD>
typename BUILD::Value * PValGenerate(CWorkspace * pWork, BUILD * pBuild, CSTNode * pStnod, VALGENK valgenk)
{
	CIRBuilderErrorContext berrctx(pWork->m_pErrman, pBuild, pStnod);

	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
	{
		STypeInfoLiteral * pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnod->m_pTin);
		if (valgenk == VALGENK_Reference)
		{
			if (pTinlit && pTinlit->m_litty.m_litk == LITK_Array)
			{
				EWC_ASSERT(valgenk == VALGENK_Reference, "Trying to generate a reference from a literal");
				return PValGenerateArrayLiteralReference(pWork, pBuild, pStnod);
			}
			EWC_ASSERT(false, "references to literals are only allowed on arrays");
		}

		return PValGenerateLiteral(pWork, pBuild, pStnod);
	}

	switch (pStnod->m_park)
	{
	case PARK_ProcedureDefinition:
		{ 
			if (!EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check, (implicit function?)"))
				return nullptr;

			/*
			if (EWC_FVERIFY(pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Procedure, "expected tinproc"))
			{
				auto pTinproc = (STypeInfoProcedure *)pStnod->m_pTin;
				if (pTinproc->m_fHasGenericArgs)
					return nullptr;
			}*/

			BUILD::Proc * pProc = nullptr;
			auto pValSym = pBuild->PValFromSymbol(pStnod->m_pSym);
			if (!pValSym)
			{
				pProc = PProcCodegenPrototype<BUILD>(pWork, pBuild, pStnod);
			}
			else
			{
				pProc = (BUILD::Proc *)pValSym;
				if (!EWC_FVERIFY(pProc->m_valk == VALK_Procedure, "expected IR procedure"))
					return nullptr;
			}

			CSTNode * pStnodBody = nullptr;
			auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
			if (!pStproc->m_fIsForeign && 
				EWC_FVERIFY(pStproc && pProc->m_pBlockFirst, "Encountered procedure without CSTProcedure"))
			{
				pStnodBody = pStnod->PStnodChildSafe(pStproc->m_iStnodBody);
			}

			if (pProc && pStnodBody)
			{
				GenerateMethodBody(pWork, pBuild, pProc, &pStnodBody, 1, false);
			}
		} break;
	case PARK_List:
		{

			auto pDif = PDifEnsure(pWork, pBuild, pStnod->m_lexloc.m_strFilename.PCoz());
			auto pLvalScope = PLvalFromDIFile(pBuild, pDif);

			s32 iLine;
			s32 iCol;
			CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLine, &iCol);

			auto pLvalDiBlock = PLvalDInfoCreateLexicalBlock(pBuild, pLvalScope, pDif, iLine, iCol);
			PushDIScope(pDif, pLvalDiBlock);

			int cStnodChild = pStnod->CStnodChild();
			for (int iStnodChild = 0; iStnodChild < cStnodChild; ++iStnodChild)
			{
				PValGenerate(pWork, pBuild, pStnod->PStnodChild(iStnodChild), VALGENK_Instance);
			}

			PopDIScope(pDif, pLvalDiBlock);

		} break;
	case PARK_ExpressionList:
		{
			int cStnodChild = pStnod->CStnodChild();
			for (int iStnodChild = 0; iStnodChild < cStnodChild; ++iStnodChild)
			{
				PValGenerate(pWork, pBuild, pStnod->PStnodChild(iStnodChild), VALGENK_Instance);
			}

		}break;
	case PARK_Decl:
		{
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
			auto pStnodInit = pStnod->PStnodChildSafe(pStdecl->m_iStnodInit);
			if (pStdecl->m_iStnodChildMin != -1)
			{
				// compound decl
				for (int iStnod = pStdecl->m_iStnodChildMin; iStnod < pStdecl->m_iStnodChildMax; ++iStnod)
				{
					auto pStnodChild = pStnod->PStnodChild(iStnod);
					(void) PValGenerateDecl(pWork, pBuild, pStnodChild, pStnodInit, valgenk);
				}
			}
			else
			{
				(void) PValGenerateDecl(pWork, pBuild, pStnod, pStnodInit, valgenk);
			}

		} break;
	case PARK_Cast:
		{
			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
			if (!EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "expected init child for cast"))
				return nullptr;

			return PValGenerateCast(pWork, pBuild, valgenk, pStnod->PStnodChild(pStdecl->m_iStnodInit), pStnod->m_pTin);
		} break;
	case PARK_Literal:
		{
			EWC_ASSERT(false, "encountered literal AST node during codegen, should have encountered literal type first");
		} break;
	case PARK_ReservedWord:
		{
			if (!EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
				return nullptr;

			RWORD rword = pStnod->m_pStval->m_rword;
			switch (rword)
			{
			case RWORD_Sizeof:
			case RWORD_Alignof:
				{
					u64 cBitSize;
					u64 cBitAlign;

					auto pStnodChild = pStnod->PStnodChildSafe(0);
					if (!EWC_FVERIFY(pStnodChild && pStnodChild->m_pTin, "bad alignof/typeof"))
						return nullptr;

					auto pLtype = pBuild->PLtypeFromPTin(pStnodChild->m_pTin);
					CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);

					u64 cBit = (rword == RWORD_Sizeof) ? cBitSize : cBitAlign;

					auto pTinint = PTinDerivedCast<STypeInfoInteger *>(pStnod->m_pTin);
					if (!pTinint)
						return nullptr;

					return pBuild->PConstInt(cBit / 8, pTinint->m_cBit, false);

				} break;
			case RWORD_Typeinfo:
				{
					auto pStnodChild = pStnod->PStnodChildSafe(0);
					if (!EWC_FVERIFY(pStnodChild && pStnodChild->m_pTin, "bad Typeinfo directive"))
						return nullptr;

					if (!EWC_FVERIFY(pStnodChild->m_pTin->m_pCgvalReflectGlobal, "expected global type info to already be emitted"))
						return nullptr;

					return PValGenerateTypeInfo(pBuild, pStnod, pStnodChild);
				} break;
			case RWORD_Fallthrough:
				break;
			case RWORD_Switch:
				{
					auto pValExp = PValGenerate(pWork, pBuild, pStnod->PStnodChild(0), VALGENK_Instance);
					GenerateSwitch(pWork, pBuild, pStnod, pValExp);
				} break;
			case RWORD_If:
				{

					//		NCmp
					//		CondBr then, elifA
					//	}
					//	{ then:
					//		...do stuff
					//		Br post
					//	}
					//	{ :elifA
					//		NCmp	
					//		CondBr thenA, else
					//	}
					//	{ :thenA
					//		...do stuff A	
					//		Br post
					//	}
					//	{ :else
					//		...do stuff C
					//		Br post
					//	}
					//	{ :post

					if (pStnod->CStnodChild() < 2)
						return nullptr;

					BUILD::Proc * pProc = pBuild->m_pProcCur;
					BUILD::Block * pBlockPost = nullptr;	
					CSTNode * pStnodCur = pStnod;

					while (pStnodCur)
					{
						// BB - should handle declarations inside conditional statement? ie, does it need a new block

						CSTNode * pStnodIf = pStnodCur;
						CSTNode * pStnodPred = pStnodIf->PStnodChild(0);

						STypeInfo * pTinBool = pStnodIf->m_pTin;

						BUILD::Block *	pBlockTrue = pBuild->PBlockCreate(pProc, "ifThen");
						BUILD::Block * pBlockFalse = nullptr;
						pStnodCur = nullptr;
						CSTNode * pStnodElseChild = nullptr;
						if (pStnodIf->CStnodChild() == 3)
						{
							CSTNode * pStnodElse = pStnodIf->PStnodChild(2);
							if (FIsReservedWord(pStnodElse, RWORD_Else))
							{
								CSTNode * pStnodChild = nullptr;
								if (EWC_FVERIFY(pStnodElse->CStnodChild() == 1, "expected one child for else statement")) //statement or list
								{
									pStnodChild = pStnodElse->PStnodChild(0);
								}

								if (pStnodChild && FIsReservedWord(pStnodChild, RWORD_If))
								{
									pBlockFalse = pBuild->PBlockCreate(pProc, "ifElif");
									pStnodCur = pStnodChild;
								}
								else
								{
									pBlockFalse = pBuild->PBlockCreate(pProc, "else");
									pStnodElseChild = pStnodChild;
								}
							}
						}

						if (!pBlockPost)
						{
							pBlockPost = pBuild->PBlockCreate(pProc, "postIf");
						}

						if (!pBlockFalse)
						{
							pBlockFalse = pBlockPost;
						}
						GeneratePredicate(pWork, pBuild, pStnodPred, pBlockTrue, pBlockFalse, pTinBool);

						pBuild->ActivateBlock(pBlockTrue);
						(void) PValGenerate(pWork, pBuild, pStnodIf->PStnodChild(1), VALGENK_Instance);
						pBuild->CreateBranch(pBlockPost);	
						pBlockTrue = pBuild->m_pBlockCur;	// could have changed during this' codegen

						if (pBlockFalse != pBlockPost)
						{
							pBuild->ActivateBlock(pBlockFalse);
						}

						if (pStnodElseChild)
						{
							(void) PValGenerate(pWork, pBuild, pStnodElseChild, VALGENK_Instance);
							pBlockFalse = pBuild->m_pBlockCur;	// could have changed during else's codegen

							pBuild->CreateBranch(pBlockPost);	
						}
					}

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_Else:
				EWC_ASSERT(false, "Else reserved word should be handled during codegen for if");
				return nullptr;
			case RWORD_For:
				{
					auto pStfor = PStmapRtiCast<CSTFor *>(pStnod->m_pStmap);
					if (!EWC_FVERIFY(pStfor, "bad for loop"))
						return nullptr;

					EmitLocation(pWork, pBuild, pStnod->m_lexloc);

					CSTNode * pStnodFor = pStnod;
					if (pStfor->m_iStnodDecl >= 0)
					{
						(void) PValGenerate(pWork, pBuild, pStnodFor->PStnodChild(pStfor->m_iStnodDecl), VALGENK_Instance);
					}

					BUILD::Proc * pProc = pBuild->m_pProcCur;
					BUILD::Block *	pBlockBody = pBuild->PBlockCreate(pProc, "fbody");
					BUILD::Block * pBlockPost = pBuild->PBlockCreate(pProc, "fpost");

					BUILD::Block *	pBlockPred = pBlockBody;
					CSTNode * pStnodPred = pStnodFor->PStnodChildSafe(pStfor->m_iStnodPredicate);
					if (pStnodPred)
					{
						pBlockPred = pBuild->PBlockCreate(pProc, "fpred");
					}

					BUILD::Block * pBlockIncrement = pBlockPred;
					CSTNode * pStnodIncrement = pStnodFor->PStnodChildSafe(pStfor->m_iStnodIncrement);
					if (pStnodIncrement)
					{
						pBlockIncrement = pBuild->PBlockCreate(pProc, "finc");
					}

					pBuild->CreateBranch(pBlockPred);	

					if (pStnodPred)
					{
						pBuild->ActivateBlock(pBlockPred);

						STypeInfo * pTinBool = pWork->m_pSymtab->PTinBuiltin("bool");
						GeneratePredicate(pWork, pBuild, pStnodPred, pBlockBody, pBlockPost, pTinBool);
					}

					auto pJumpt = pBuild->m_aryJumptStack.AppendNew();
					pJumpt->m_pBlockBreak = pBlockPost;
					pJumpt->m_pBlockContinue = (pStnodIncrement) ? pBlockIncrement : pBlockBody;
					if (pStnodFor->m_pStident)
					{
						pJumpt->m_strLabel = pStnodFor->m_pStident->m_str;
					}

					pBuild->ActivateBlock(pBlockBody);
					(void) PValGenerate(pWork, pBuild, pStnodFor->PStnodChild(pStfor->m_iStnodBody), VALGENK_Instance);

					pBuild->CreateBranch(pBlockIncrement);	
					pBuild->m_aryJumptStack.PopLast();

					if (pStnodIncrement)
					{

						pBuild->ActivateBlock(pBlockIncrement);
						(void) PValGenerate(pWork, pBuild, pStnodIncrement, VALGENK_Instance);

						pBuild->CreateBranch(pBlockPred);	
					}

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_ForEach:
				{
					auto pStfor = PStmapRtiCast<CSTFor *>(pStnod->m_pStmap);
					if (!EWC_FVERIFY(pStfor, "bad for_each loop"))
						return nullptr;

					EmitLocation(pWork, pBuild, pStnod->m_lexloc);

					CSTNode * pStnodFor = pStnod;
					if (pStfor->m_iStnodDecl >= 0)
					{
						(void) PValGenerate(pWork, pBuild, pStnodFor->PStnodChild(pStfor->m_iStnodDecl), VALGENK_Instance);
					}
					else if (pStfor->m_iStnodInit >= 0 && pStfor->m_iStnodIterator >= 0)
					{
						auto pStnodIterator = pStnodFor->PStnodChild(pStfor->m_iStnodIterator);
						auto pStnodInit = pStnodFor->PStnodChild(pStfor->m_iStnodInit);

						auto pValIterator = PValGenerate(pWork, pBuild, pStnodIterator, VALGENK_Reference);
						(void) PInstGenerateAssignment(pWork, pBuild, pStnodIterator->m_pTin, pValIterator, pStnodInit);
					}

					BUILD::Proc * pProc = pBuild->m_pProcCur;
					BUILD::Block *	pBlockPred = pBuild->PBlockCreate(pProc, "fpred");
					BUILD::Block *	pBlockBody = pBuild->PBlockCreate(pProc, "fbody");
					BUILD::Block * pBlockPost = pBuild->PBlockCreate(pProc, "fpost");
					BUILD::Block * pBlockIncrement = pBuild->PBlockCreate(pProc, "finc");
					pBuild->CreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPred);

					CSTNode * pStnodPred = pStnodFor->PStnodChild(pStfor->m_iStnodPredicate);

					STypeInfo * pTinBool = pStnodPred->m_pTin;
					EWC_ASSERT(pTinBool->m_tink == TINK_Bool, "expected bool type for for loop predicate");

					// NOTE: we're swapping the true/false blocks here because the predicate is reversed, ie fIsDone
					GeneratePredicate(pWork, pBuild, pStnodPred, pBlockPost, pBlockBody, pTinBool);

					auto pJumpt = pBuild->m_aryJumptStack.AppendNew();
					pJumpt->m_pBlockBreak = pBlockPost;
					pJumpt->m_pBlockContinue = pBlockIncrement;
					if (pStnodFor->m_pStident)
					{
						pJumpt->m_strLabel = pStnodFor->m_pStident->m_str;
					}

					pBuild->ActivateBlock(pBlockBody);
					(void) PValGenerate(pWork, pBuild, pStnodFor->PStnodChild(pStfor->m_iStnodBody), VALGENK_Instance);
					pBuild->CreateBranch(pBlockIncrement);	
					pBuild->m_aryJumptStack.PopLast();

					pBuild->ActivateBlock(pBlockIncrement);
					CSTNode * pStnodIncrement = pStnodFor->PStnodChild(pStfor->m_iStnodIncrement);
					(void) PValGenerate(pWork, pBuild, pStnodIncrement, VALGENK_Instance);

					pBuild->CreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_While:
				{
					if (pStnod->CStnodChild() < 2)
						return nullptr;

					EmitLocation(pWork, pBuild, pStnod->m_lexloc);

					BUILD::Proc * pProc = pBuild->m_pProcCur;
					BUILD::Block * pBlockPred = pBuild->PBlockCreate(pProc, "wpred");
					BUILD::Block * pBlockBody = pBuild->PBlockCreate(pProc, "wbody");
					BUILD::Block * pBlockPost = pBuild->PBlockCreate(pProc, "wpost");
					pBuild->CreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPred);

					// BB - should handle declarations inside conditional statement? ie, does it need a new block

					CSTNode * pStnodWhile = pStnod;
					CSTNode * pStnodPred = pStnodWhile->PStnodChild(0);

					STypeInfo * pTinBool = pStnodWhile->m_pTin;
					EWC_ASSERT(pTinBool->m_tink == TINK_Bool, "expected bool type for while predicate");

					GeneratePredicate(pWork, pBuild, pStnodPred, pBlockBody, pBlockPost, pTinBool);

					pBuild->ActivateBlock(pBlockBody);

					auto pJumpt = pBuild->m_aryJumptStack.AppendNew();
					pJumpt->m_pBlockBreak = pBlockPost;
					pJumpt->m_pBlockContinue = pBlockPred;
					if (pStnodWhile->m_pStident)
					{
						pJumpt->m_strLabel = pStnodWhile->m_pStident->m_str;
					}

					(void) PValGenerate(pWork, pBuild, pStnodWhile->PStnodChild(1), VALGENK_Instance);
					pBuild->m_aryJumptStack.PopLast();

					pBuild->CreateBranch(pBlockPred);	


					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_Return:
				{
					EmitLocation(pWork, pBuild, pStnod->m_lexloc);

					BUILD::Value * pValRhs = nullptr;
					if (pStnod->CStnodChild() == 1)
					{
						pValRhs = PValGenerate(pWork, pBuild, pStnod->PStnodChild(0), VALGENK_Instance);
					}

					pBuild->CreateReturn(&pValRhs, 1, "retTmp");

				} break;
			case RWORD_Break:
			case RWORD_Continue:
				{
					BUILD::Block * pBlock = nullptr;
					CString * pString = nullptr;
					if (pStnod->m_pStident)
					{
						pString = &pStnod->m_pStident->m_str;
					}

					if (rword == RWORD_Break)
					{
						for (int iJumpt = (int)pBuild->m_aryJumptStack.C(); --iJumpt >= 0; )
						{
							auto pJumpt = &pBuild->m_aryJumptStack[iJumpt];
							if (pJumpt->m_pBlockBreak && (pString == nullptr || pJumpt->m_strLabel == *pString))
							{
								pBlock = pJumpt->m_pBlockBreak;
								break;
							}
						}
					}
					else //RWORD_Continue
					{
						for (size_t iJumpt = pBuild->m_aryJumptStack.C()-1; true; --iJumpt)
						{
							auto pJumpt = &pBuild->m_aryJumptStack[iJumpt];
							if (pJumpt->m_pBlockContinue && (pString == nullptr || pJumpt->m_strLabel == *pString))
							{
								pBlock = pJumpt->m_pBlockContinue;
								break;
							}
						}
					}

					if (pBlock)
					{
						pBuild->CreateBranch(pBlock);	
					}
					else
					{
						if (pString)
						{
							EmitError(pWork, &pStnod->m_lexloc, ERRID_UnknownError,
								"Could not %s to label matching '%s'", PCozFromRword(rword), pString->PCoz());
						}
						else
							EmitError(pWork, &pStnod->m_lexloc, ERRID_UnknownError,
								"Encountered %s statement outside of a loop or switch", PCozFromRword(rword));
					}

				} break;
			default:
				EWC_ASSERT(false, "Unhandled reserved word in code gen %s", PCozFromRword(rword));
				break;
			}
		} break;
	case PARK_ProcedureCall:
		{
			auto pStnodTarget = pStnod->PStnodChildSafe(0);
			STypeInfoProcedure * pTinproc = (pStnodTarget) ? PTinRtiCast<STypeInfoProcedure *>(pStnodTarget->m_pTin) : nullptr;

			if (!EWC_FVERIFY(pTinproc, "expected type info procedure"))
				return nullptr;
	
			CDynAry<BUILD::ProcArg *> arypLvalArgs(pBuild->m_pAlloc, EWC::BK_Stack);

			size_t cStnodArgs = pStnod->CStnodChild() - 1; // don't count the identifier
			for (size_t iStnodChild = 0; iStnodChild < cStnodArgs; ++iStnodChild)
			{
				CSTNode * pStnodArg = pStnod->PStnodChild((int)iStnodChild + 1);
				STypeInfo * pTinParam = pStnodArg->m_pTin;
				if (iStnodChild < pTinproc->m_arypTinParams.C())
				{
					pTinParam = pTinproc->m_arypTinParams[iStnodChild];
				}

				auto pValRhsCast = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodArg, pTinParam);

				arypLvalArgs.Append(pBuild->PProcArg(pValRhsCast));
				if (!EWC_FVERIFY(*arypLvalArgs.PLast(), "missing argument value"))
					return nullptr;
			}

			bool fIsDirectCall = FIsDirectCall(pStnod);
			return pBuild->PValGenerateCall(pWork, pStnod, pStnod->m_pSym, &arypLvalArgs, fIsDirectCall, pTinproc, valgenk);
		} 

	case PARK_Identifier:
		{
			if (!pStnod->m_pSym || !pBuild->PValFromSymbol(pStnod->m_pSym))
			{
				CString strName(StrFromIdentifier(pStnod));
				EmitError(pWork, &pStnod->m_lexloc, ERRID_UnknownError, "INTERNAL ERROR: Missing value for symbol %s", strName.PCoz());
			}

			auto pVal = pBuild->PValFromSymbol(pStnod->m_pSym);
			if (!EWC_FVERIFY(pVal, "unknown identifier in codegen"))
				return nullptr;

			auto valgenkSym = (pVal->m_valk == VALK_Procedure) ? VALGENK_Instance : VALGENK_Reference;
			if (valgenk == VALGENK_Instance)
			{
				if (pVal->m_valk != VALK_Procedure)
				{
					auto strPunyName = StrPunyEncode(pStnod->m_pSym->m_strName.PCoz());
					return pBuild->PInstCreate(IROP_Load, pVal, strPunyName.PCoz());
				}
			}
			else if (valgenk == VALGENK_Reference)
			{
				ASSERT_STNOD(pWork, pStnod, valgenkSym == VALGENK_Reference, "Cannot return reference for instance symbol type");
			}
			return pVal;
		}
	case PARK_MemberLookup:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);

			// check the symbol because we need to differentiate between enum namespacing and enum struct member.
			BUILD::Constant * pConstEnum = nullptr;
			if (auto pSym = pStnodLhs->m_pSym)
			{
				auto pTinLhs = PTinStripQualifiers(pSym->m_pTin);

				VALGENK valgenkLhs = VALGENK_Reference;
				if (pTinLhs)
				{
					if (pTinLhs->m_tink == TINK_Pointer)
					{
						pTinLhs = ((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo;
						valgenkLhs = VALGENK_Instance;
					}
				}

				if (pTinLhs && pTinLhs->m_tink == TINK_Enum)
				{
					auto pTinenum = (STypeInfoEnum *)pTinLhs;
					if (EWC_FVERIFY(pStnod->m_pStval, "Enum constant lookup without value"))
					{
						pConstEnum = pBuild->PConstEnumLiteral(pTinenum, pStnod->m_pStval);

						if (pTinenum->m_enumk != ENUMK_FlagEnum)
							return pConstEnum;
					}
				}
			}
			STypeInfoStruct * pTinstruct = nullptr;
			auto pTinLhs = PTinStripQualifiers(pStnodLhs->m_pTin);

			VALGENK valgenkLhs = VALGENK_Reference;
			if (pTinLhs)
			{
				if (pTinLhs->m_tink == TINK_Pointer)
				{
					pTinLhs = ((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo;
					valgenkLhs = VALGENK_Instance;
				}

				pTinstruct = PTinRtiCast<STypeInfoStruct *>(pTinLhs);
			}

			BUILD::Value * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, valgenkLhs);
			if (pConstEnum)
			{
				if (valgenk == VALGENK_Reference)
				{
					// set val: (n & ~flag) | (sext(fNew) & flag)
					// return the instance here, the assignment operator will have to flip the right bits 
					return pValLhs;
				}

				// get val: (n & flag) == flag;
				auto pInstLhsLoad = pBuild->PInstCreate(IROP_Load, pValLhs, "membLoad");
				auto pInstAnd = pBuild->PInstCreate(IROP_And, pInstLhsLoad, pConstEnum, "and");
				auto pInstCmp = pBuild->PInstCreateNCmp(NPRED_EQ, pInstAnd, pConstEnum, "cmpEq");
				return pInstCmp;
			}

			CString strMemberName = StrFromIdentifier(pStnod->PStnodChild(1));

			if (pTinLhs && pTinLhs->m_tink == TINK_Array)
			{
				auto pTinary = (STypeInfoArray *)pTinLhs;
				ARYMEMB arymemb = ArymembLookup(strMemberName.PCoz());
				return PValFromArrayMember(pWork, pBuild, pValLhs, pTinary, arymemb, valgenk);
			}
			if (pTinLhs && pTinLhs->m_tink == TINK_Literal)
			{
				auto pTinlit = (STypeInfoLiteral *)pTinLhs;
				if (EWC_FVERIFY(pTinlit->m_pGlob, "expected global instance for array literal"))
				{
					// BB - cleanup this (BUILD::Global *) cast
					BUILD::Global * pGlobLit = (BUILD::Global *)pTinlit->m_pGlob;

					if (pTinlit->m_litty.m_litk == LITK_Array)
					{
						ARYMEMB arymemb = ArymembLookup(strMemberName.PCoz());

						EWC_ASSERT(valgenk == VALGENK_Instance, "expected instance");
						if (arymemb == ARYMEMB_Count)
						{
							return pBuild->PConstInt(pTinlit->m_c, 64, true);
						}

						// the type of aN.data is &N so a reference would need to be a &&N which we don't have
						EWC_ASSERT(arymemb == ARYMEMB_Data, "unexpected array member '%s'", PChzFromArymemb(arymemb));

						auto pTinptr = pWork->m_pSymtab->PTinptrAllocate(pTinlit->m_pTinSource);

						return pBuild->PInstCreateCast(IROP_Bitcast, pGlobLit, pTinptr, "Bitcast");
					}
					return pGlobLit;
				}
			}
			
			auto pTypememb = PTypemembLookup(pTinstruct, strMemberName);
			if (!EWC_FVERIFY(pTypememb, "cannot find structure member %s", strMemberName.PCoz()))
				return nullptr;

			BUILD::GepIndex * apLvalIndex[3] = {};
			int cpLvalIndex = 0;
			apLvalIndex[cpLvalIndex++] = pBuild->PGepIndex(0);
			apLvalIndex[cpLvalIndex++] = pBuild->PGepIndex(pTinstruct->m_aryTypemembField.IFromP(pTypememb));
			auto pInst = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, cpLvalIndex, "aryGep");

			if (EWC_FVERIFY(pInst, "member dereference failure in codegen") && valgenk != VALGENK_Reference)
			{
				pInst = pBuild->PInstCreate(IROP_Load, pInst, "membLoad");
			}
			return pInst;
		}
	case PARK_ArrayElement:
		{ 
			if (!EWC_FVERIFY(pStnod->CStnodChild() == 2, "expected (array, index) for array element node"))
				return nullptr;

			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodIndex = pStnod->PStnodChild(1);

			BUILD::Value * pValLhs;
			auto pValIndex = PValGenerate(pWork, pBuild, pStnodIndex, VALGENK_Instance);
			if (!EWC_FVERIFY(!FIsNull(pValIndex), "null index llvm value"))
				return nullptr;

			BUILD::GepIndex * apLvalIndex[2] = {};
			TINK tinkLhs = PTinStripQualifiers(pStnodLhs->m_pTin)->m_tink;
			int cpLvalIndex = 0;

			if (tinkLhs == TINK_Literal)
			{
				auto pTinlit = PTinDerivedCast<STypeInfoLiteral *>(pStnodLhs->m_pTin);
				EWC_ASSERT(pTinlit->m_litty.m_litk == LITK_Array, "non-array literal being indexed like an array, missed during typecheck?");

				pValLhs = PValGenerateArrayLiteralReference(pWork, pBuild, pStnodLhs);
				apLvalIndex[cpLvalIndex++] = pBuild->PGepIndex(0);
				apLvalIndex[cpLvalIndex++] = pBuild->PGepIndexFromValue(pValIndex);
			}
			else if (tinkLhs == TINK_Array)
			{
				auto pTinary = (STypeInfoArray *)pStnodLhs->m_pTin;
				pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);

				if (pTinary->m_aryk != ARYK_Fixed)
				{
					pValLhs = PValFromArrayMember(pWork, pBuild, pValLhs, pTinary, ARYMEMB_Data, VALGENK_Instance);	// BB - why is this instance??
				}
				else
				{
					apLvalIndex[cpLvalIndex++] = pBuild->PGepIndex(0);
				}
				apLvalIndex[cpLvalIndex++] = pBuild->PGepIndexFromValue(pValIndex);
			}
			else if (tinkLhs == TINK_Pointer)
			{
				pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Instance);
				apLvalIndex[0] = pBuild->PGepIndexFromValue(pValIndex);
				cpLvalIndex = 1;
			}
			else
			{
				EWC_ASSERT(false, "unexpected type on left hand side of array element");
				return nullptr;
			}

			auto pInst = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, cpLvalIndex, "aryGep");

			if (EWC_FVERIFY(pInst, "unknown identifier in codegen") && valgenk != VALGENK_Reference)
			{
				return pBuild->PInstCreate(IROP_Load, pInst, "aryLoad");
			}
			return pInst;
		} break;
	case PARK_LogicalAndOrOp:
		{
			// BB - What is the right thing here, C++ logical op overloads don't short circuit, C# breaks
			// it into two operators (false and &)

			auto pTinBool = pWork->m_pSymtab->PTinBuiltin("bool");
			auto pLtypeBool = pBuild->PLtypeFromPTin(pTinBool);

			BUILD::Proc * pProc = pBuild->m_pProcCur;
			auto pBlockTrue = pBuild->PBlockCreate(pProc, "predTrue");
			auto pBlockFalse = pBuild->PBlockCreate(pProc, "predFalse");
			auto pBlockPost = pBuild->PBlockCreate(pProc, "predPost");

			GeneratePredicate(pWork, pBuild, pStnod, pBlockTrue, pBlockFalse, pTinBool);

			pBuild->ActivateBlock(pBlockTrue);
			auto pValTrue = pBuild->PConstInt(1, 1, false);
			pBuild->CreateBranch(pBlockPost);	

			pBuild->ActivateBlock(pBlockFalse);
			auto pValFalse = pBuild->PConstInt(0, 1, false);
			pBuild->CreateBranch(pBlockPost);	

			pBuild->ActivateBlock(pBlockPost);
			auto pValPhi = pBuild->PInstCreatePhi(pLtypeBool, "predPhi"); 
			pBuild->AddPhiIncoming(pValPhi, pValTrue, pBlockTrue);
			pBuild->AddPhiIncoming(pValPhi, pValFalse, pBlockFalse);

			EWC_ASSERT(valgenk != VALGENK_Reference, "taking the address of a temporary (?)");
			return pValPhi;

		} break;
	case PARK_AssignmentOp:
		{
			if (FIsOverloadedOp(pStnod))
			{
				auto pTinproc = pStnod->m_pOptype->m_pTinprocOverload;

				//BB - This won't work with generic procedure overloads
				auto pSym = pTinproc->m_pStnodDefinition->m_pSym;

				CDynAry<BUILD::ProcArg *> arypLvalArgs(pBuild->m_pAlloc, EWC::BK_Stack);
				GenerateArguments(pWork, pBuild, pStnod->m_grfstnod, pTinproc, 2, pStnod->m_arypStnodChild.A(), &arypLvalArgs);
				return pBuild->PValGenerateCall(pWork, pStnod, pSym, &arypLvalArgs, true, pTinproc, valgenk);
			}

			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);

			BUILD::Value * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);
			if (pStnod->m_tok == TOK('='))
			{
				if (pStnodLhs->m_pTin->m_tink == TINK_Flag)
				{
					STypeInfo * pTinLoose = nullptr;
					BUILD::Constant * pConstEnum = nullptr;
					if (EWC_FVERIFY(pStnodLhs->m_park == PARK_MemberLookup, "expected member lookup"))
					{
						CSTNode * pStnodEnum = pStnodLhs->PStnodChildSafe(0);
						STypeInfo * pTinLhs = (pStnodEnum) ? pStnodEnum->m_pTin : nullptr;
						if (pTinLhs->m_tink == TINK_Pointer)
						{
							pTinLhs = ((STypeInfoPointer *)pTinLhs)->m_pTinPointedTo;
						}

						if (EWC_FVERIFY(pTinLhs && pTinLhs->m_tink == TINK_Enum, "expected enum type"))
						{
							auto pTinenum = (STypeInfoEnum *)pTinLhs;
							pTinLoose = pTinenum->m_pTinLoose;

							pConstEnum = pBuild->PConstEnumLiteral(pTinenum, pStnodLhs->m_pStval);
						}

					}

					// set val: (n & ~flag) | (sext(rhs) & flag)

					auto pTinBool = pWork->m_pSymtab->PTinBuiltin("bool");
					auto pValRhs = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodRhs, pTinBool);

					auto pInstSExt = pBuild->PInstCreateCast(IROP_SignExt, pValRhs, pTinLoose, "SignExt");
					auto pInstAndRhs = pBuild->PInstCreate(IROP_And, pInstSExt, pConstEnum, "andRhs");

					auto pInstLhsLoad = pBuild->PInstCreate(IROP_Load, pValLhs, "lhsLoad");
					auto pInstNotEnum =  pBuild->PInstCreate(IROP_Not, pConstEnum, "notEn");
					auto pInstAndLhs = pBuild->PInstCreate(IROP_And, pInstLhsLoad, pInstNotEnum, "andLhs");

					auto pInstOr = pBuild->PInstCreate(IROP_Or, pInstAndLhs, pInstAndRhs, "orFlags");
					return pBuild->PInstCreateStore(pValLhs, pInstOr);
				}

				auto pInstOp = PInstGenerateAssignment(pWork, pBuild, pStnodLhs->m_pTin, pValLhs, pStnodRhs);
				return pInstOp;
			}

			EmitLocation(pWork, pBuild, pStnod->m_lexloc);

			auto pTinOperandRhs = (EWC_FVERIFY(pStnod->m_pOptype, "missing operator types")) ? pStnod->m_pOptype->m_pTinRhs : pStnodLhs->m_pTin;

			auto pValLhsLoad = pBuild->PInstCreate(IROP_Load, pValLhs, "lhsLoad");
			auto pValRhsCast = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodRhs, pTinOperandRhs);
			EWC_ASSERT(pValRhsCast, "bad cast");

			SOpTypes optype(pStnodLhs->m_pTin, pStnodRhs->m_pTin, pStnod->m_pTin);
			auto pInstOp = PInstGenerateOperator(pBuild, pStnod->m_tok, &optype, pValLhsLoad, pValRhsCast);
			return pBuild->PInstCreateStore(pValLhs, pInstOp);
		}
	case PARK_AdditiveOp:
		{
			SOpTypes * pOptype = pStnod->m_pOptype;
			EWC_ASSERT(pOptype && pOptype->m_pTinLhs && pOptype->m_pTinRhs, "missing operand types for AdditiveOp");

			STypeInfo * pTinLhs = PTinStripQualifiers(pOptype->m_pTinLhs);
			STypeInfo * pTinRhs = PTinStripQualifiers(pOptype->m_pTinRhs);
			TINK tinkLhs = pTinLhs->m_tink;
			TINK tinkRhs = pTinRhs->m_tink;
			if (!FIsOverloadedOp(pStnod))
			{
				if (((tinkLhs == TINK_Pointer) & (tinkRhs == TINK_Integer)) | 
					((tinkLhs == TINK_Integer) & (tinkRhs == TINK_Pointer)))
				{
					//handle pointer arithmetic

					CSTNode * pStnodLhs = pStnod->PStnodChild(0);
					auto pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Instance);

					CSTNode * pStnodRhs = pStnod->PStnodChild(1);
					auto pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);

					BUILD::Value * pValPtr;
					BUILD::Value * pValIndex;
					if (pTinLhs->m_tink == TINK_Pointer)
					{
						pValPtr = pValLhs;
						pValIndex = pValRhs;
					}
					else
					{
						pValPtr = pValRhs;
						pValIndex = pValLhs;
					}
					if (pStnod->m_tok == TOK('-'))
					{
						pValIndex = pBuild->PInstCreate(IROP_NNeg, pValIndex, "NNeg");
					}

					BUILD::GepIndex * pGepIndex = pBuild->PGepIndexFromValue(pValIndex);
					auto pInstGep = pBuild->PInstCreateGEP(pValPtr, &pGepIndex, 1, "ptrGep");
					return pInstGep; 
				}
			}
		} // fallthrough
	case PARK_MultiplicativeOp:
	case PARK_ShiftOp:
	case PARK_BitwiseAndOrOp:
	case PARK_RelationalOp:
	case PARK_EqualityOp:
		{
			ASSERT_STNOD(pWork, pStnod, valgenk != VALGENK_Reference, "operand result is not a valid LValue");

			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);

			SOpTypes * pOptype = pStnod->m_pOptype;
			EWC_ASSERT(pOptype && pOptype->FIsValid(), "missing operand types");
			STypeInfo * pTinOperandLhs = pOptype->m_pTinLhs;
			STypeInfo * pTinOperandRhs = pOptype->m_pTinRhs;
			auto pValLhsCast = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodLhs, pTinOperandLhs);
			auto pValRhsCast = PValGenerateCast(pWork, pBuild, VALGENK_Instance, pStnodRhs, pTinOperandRhs);
			if (!EWC_FVERIFY((pValLhsCast != nullptr) & (pValRhsCast != nullptr), "null operand"))
				return nullptr;

			STypeInfo * pTinLhs = pStnodLhs->m_pTin;
			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY(
					(pOptype->m_pTinResult != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), 
					"bad cast"))
			{
				return nullptr;
			}

			if (FIsOverloadedOp(pStnod))
			{
				auto pTinproc = pStnod->m_pOptype->m_pTinprocOverload;

				//BB - This won't work with generic procedure overloads
				auto pSym = pTinproc->m_pStnodDefinition->m_pSym;

				CDynAry<BUILD::ProcArg *> arypLvalArgs(pBuild->m_pAlloc, EWC::BK_Stack);
				GenerateArguments(pWork, pBuild, pStnod->m_grfstnod, pTinproc, 2, pStnod->m_arypStnodChild.A(), &arypLvalArgs);
				return pBuild->PValGenerateCall(pWork, pStnod, pSym, &arypLvalArgs, true, pTinproc, valgenk);
			}
	
			/*
			if (LLVMTypeOf(pValLhsCast->m_pLval) != LLVMTypeOf(pValRhsCast->m_pLval))
			{
				EmitError(pWork, &pStnod->m_lexloc, ERRID_BadCastGen, "INTERNAL ERROR: bad cast");
				DumpLtype("Lhs", pValLhsCast);
				DumpLtype("Rhs", pValRhsCast);
				return nullptr;
			}*/

			auto pInstOp = PInstGenerateOperator(pBuild, pStnod->m_tok, pOptype, pValLhsCast, pValRhsCast);

			EWC_ASSERT(pInstOp, "%s operator unsupported in codegen", PCozFromTok(pStnod->m_tok));
			return pInstOp;
		}
	case PARK_PostfixUnaryOp:
	case PARK_UnaryOp:
		{
			CSTNode * pStnodOperand = pStnod->PStnodChild(0);

			if (FIsOverloadedOp(pStnod))
			{
				auto pTinproc = pStnod->m_pOptype->m_pTinprocOverload;

				//BB - This won't work with generic procedure overloads
				auto pSym = pTinproc->m_pStnodDefinition->m_pSym;

				CDynAry<BUILD::ProcArg *> arypLvalArgs(pBuild->m_pAlloc, EWC::BK_Stack);
				CDynAry<BUILD::Value *> arypValArgs(pBuild->m_pAlloc, EWC::BK_Stack);
				GenerateArguments(pWork, pBuild, pStnod->m_grfstnod, pTinproc, 1, pStnod->m_arypStnodChild.A(), &arypLvalArgs, &arypValArgs);

				BUILD::Value * pValReturn = nullptr;
				if (pStnod->m_park == PARK_PostfixUnaryOp)
				{
					pValReturn = pBuild->PInstCreate(IROP_Load, arypValArgs[0], "");
				}

				auto pValCall = pBuild->PValGenerateCall(pWork, pStnod, pSym, &arypLvalArgs, true, pTinproc, valgenk);

				return (pValReturn) ? pValReturn : pValCall;;
			}

			if (pStnod->m_tok == TOK_Reference)
			{
				return PValGenerate(pWork, pBuild, pStnodOperand, VALGENK_Reference);
			}

			TOK tok = pStnod->m_tok;
			VALGENK valgenkUnary = ((tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) ? VALGENK_Reference : VALGENK_Instance;
			auto pValOperand = PValGenerate(pWork, pBuild, pStnodOperand, valgenkUnary);
			if (!EWC_FVERIFY(!FIsNull(pValOperand), "null operand"))
				return nullptr;

			EWC_ASSERT(pStnod->m_pOptype, "mising operator types in unary op");
			STypeInfo * pTinOutput = PTinStripQualifiers(pStnod->m_pOptype->m_pTinResult);
			STypeInfo * pTinOperand = PTinStripQualifiers(pStnodOperand->m_pTin);

			if (pTinOutput->m_tink == TINK_Enum)
			{
				auto pTinenum = (STypeInfoEnum *)pTinOutput;
				pTinOutput = pTinenum->m_pTinLoose;
			}

			if (pTinOperand->m_tink == TINK_Enum)
			{
				auto pTinenum = (STypeInfoEnum *)pTinOperand;
				pTinOperand = pTinenum->m_pTinLoose;
			}

			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinOperand != nullptr), "bad cast"))
				return nullptr;
	
			BUILD::Value * pValOp = nullptr;

			bool fIsSigned = true;
			TINK tink = pTinOutput->m_tink;
			if (pTinOutput->m_tink == TINK_Literal)
			{
				STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTinOutput;
				fIsSigned = pTinlit->m_litty.m_fIsSigned;
				switch (pTinlit->m_litty.m_litk)
				{
				case LITK_Integer:	tink = TINK_Integer;	break;
				case LITK_Float:	tink = TINK_Float;		break;
				case LITK_Enum:		tink = TINK_Enum;		break;
				default:			tink = TINK_Nil;
				}
			}
			else if (pTinOutput->m_tink == TINK_Integer)
			{
				fIsSigned = ((STypeInfoInteger *)pTinOutput)->m_fIsSigned;
			}

			switch ((u32)pStnod->m_tok)
			{
			case '!':				
				{
					EWC_ASSERT(tink == TINK_Bool, "expected value cannot be cast to bool for '!' operand");

					// OPTIMIZE: could probably save an instruction here by not comparing (for the cast to bool)
					//  then inverting with a FNot

					auto pValOperandCast = PValCreateCast(pWork, pBuild, pValOperand, pTinOperand, pTinOutput);
					if (!pValOperandCast)
					{
						EmitError(pWork, &pStnod->m_lexloc, ERRID_BadCastGen, "INTERNAL ERROR: trying to codegen unsupported numeric cast.");
						return nullptr;
					}

					pValOp = pBuild->PInstCreate(IROP_Not, pValOperandCast, "NCmpEq");
				} break;
			case '+':
				{
					pValOp = pValOperand;
				} break;
			case '-':
				{
					switch (tink)
					{
					case TINK_Float:	pValOp = pBuild->PInstCreate(IROP_GNeg, pValOperand, "GNeg"); break;
					case TINK_Integer:	pValOp = pBuild->PInstCreate(IROP_NNeg, pValOperand, "NNeg"); break;
					default: EWC_ASSERT(false, "unexpected type '%s' for negate operator", PChzFromTink(tink));
					}
				} break;
			case TOK_Dereference:
				{
					if (valgenk != VALGENK_Reference)
					{
						pValOp = pBuild->PInstCreate(IROP_Load, pValOperand, "Deref");
					}
					else
					{
						pValOp = pValOperand;
					}
				} break;
			case TOK_PlusPlus:
			case TOK_MinusMinus:
				{
					if (!EWC_FVERIFY((pTinOutput == pTinOperand), "increment type mismatch (?)"))
						return nullptr;
			
					BUILD::Instruction * pInstLoad = pBuild->PInstCreate(IROP_Load, pValOperand, "IncLoad");
					BUILD::Instruction * pInstAdd = nullptr;
					switch (tink)
					{
					case TINK_Float:	
						{
							auto pConst = pBuild->PConstFloat(1.0f, ((STypeInfoFloat *)pTinOperand)->m_cBit);

							if (pStnod->m_tok == TOK_PlusPlus)
								pInstAdd = pBuild->PInstCreate(IROP_GAdd, pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreate(IROP_GSub, pInstLoad, pConst, "gDec");

						} break;
					case TINK_Enum:
					case TINK_Integer:
						{
							auto pTinint = (STypeInfoInteger *)pTinOperand;
							auto pConst = pBuild->PConstInt(1, pTinint->m_cBit, fIsSigned);

							if (pStnod->m_tok == TOK_PlusPlus)
								pInstAdd = pBuild->PInstCreate(IROP_NAdd, pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreate(IROP_NSub, pInstLoad, pConst, "gDec");
						} break;
					case TINK_Pointer:
						{
							int nDelta = (pStnod->m_tok == TOK_PlusPlus) ? 1 : -1;
							auto pConstDelta = pBuild->PConstInt(nDelta, 64, true);
							BUILD::GepIndex * pGepIndex = pBuild->PGepIndexFromValue(pConstDelta);

							auto pValLoad = pBuild->PInstCreate(IROP_Load, pValOperand, "incLoad");
							pInstAdd = pBuild->PInstCreateGEP(pValLoad, &pGepIndex, 1, "incGep");
						} break;
					default: EWC_ASSERT(false, "unexpected type '%s' for increment/decrement operator", PChzFromTink(tink));
					}

					(void) pBuild->PInstCreateStore(pValOperand, pInstAdd);
					pValOp = (pStnod->m_park == PARK_PostfixUnaryOp) ? pInstLoad : pInstAdd;
				}
			default: break;
			}

			EWC_ASSERT(
				pValOp != nullptr,
				"bad operand '%s' for type '%s'",
				PCozFromTok(pStnod->m_tok),
				PChzFromTink(tink));

			EmitLocation(pWork, pBuild, pStnod->m_lexloc);

			return pValOp;
		}
	case PARK_Typedef:
		{
			auto pSym = pStnod->m_pSym;
			if (!EWC_FVERIFY(pSym, "typedef symbol not resolved before codeGen"))
				return  nullptr;

			auto pDif = PDifEnsure(pWork, pBuild, pStnod->m_lexloc.m_strFilename);
			auto pLvalScope = PLvalFromDIFile(pBuild, pDif);

			s32 iLine, iCol;
			CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLine, &iCol);

			CreateDebugInfo(pWork, pBuild, pStnod, pSym->m_pTin);

			auto strPunyName = StrPunyEncode(pSym->m_strName.PCoz());

			DInfoCreateTypedef(pBuild, pSym->m_pTin, strPunyName.PCoz(), pDif, iLine, pLvalScope);
		} break;
	case PARK_StructDefinition:
	case PARK_EnumDefinition:
	case PARK_Nop: 
		break;
	default:
		EWC_ASSERT(false, "unhandled PARK (%s) in code generation.", PChzFromPark(pStnod->m_park));
	}
	return nullptr;
}

CIRBlock * CBuilderIR::PBlockCreate(CIRProcedure * pProc, const char * pChzName)
{
	CIRBlock * pBlock = EWC_NEW(m_pAlloc, CIRBlock) CIRBlock(m_pAlloc);

	EWC_ASSERT(pProc->m_pLval, "expected function value");
	auto pLblock = LLVMAppendBasicBlock(pProc->m_pLval, pChzName);
	pBlock->m_pLblock = pLblock;

	if (EWC_FVERIFY(pProc, "missing procedure in PBlockCreate"))
	{
		pProc->m_arypBlockManaged.Append(pBlock);
	}
	return pBlock;
}

void CBuilderIR::ActivateProc(CIRProcedure * pProc, CIRBlock * pBlock)
{
	if (m_pProcCur)
	{
		m_pProcCur->m_pLvalDebugLocCur = LLVMGetCurrentDebugLocation(m_pLbuild);
	}

	m_pProcCur = pProc;

	if (pProc)
	{
		LLVMSetCurrentDebugLocation(m_pLbuild, pProc->m_pLvalDebugLocCur);
	}
	ActivateBlock(pBlock);
}

void CBuilderIR::ActivateBlock(CIRBlock * pBlock)
{
	if (pBlock == m_pBlockCur)
		return;

	if (pBlock)
	{
		EWC_ASSERT(pBlock->m_pLblock, "bad block ptr");
		auto pLvalBlockParent = LLVMGetBasicBlockParent(pBlock->m_pLblock);

		EWC_ASSERT(pLvalBlockParent == m_pProcCur->m_pLval, "block with multiple parents?");
		LLVMPositionBuilder(m_pLbuild, pBlock->m_pLblock, nullptr);
	}

	m_pBlockCur = pBlock;
}

CBuilderIR::LType * CBuilderIR::PLtypeVoid()
{
	return LLVMVoidType();
}

BCode::SProcedure * PProcCodegenInitializer(CWorkspace * pWork, BCode::CBuilder * pBuild, CSTNode * pStnodStruct)
{
	EWC_ASSERT(false, "bytecode tbd");
	return nullptr;
}

CIRProcedure * PProcCodegenInitializer(CWorkspace * pWork, CBuilderIR * pBuild, CSTNode * pStnodStruct)
{
	STypeInfoStruct * pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pStnodStruct->m_pTin);
	if (!pTinstruct)
		return nullptr;

	LLVMOpaqueType * pLtypeVoid = LLVMVoidType();
	LLVMOpaqueType * apLtype[1];
	apLtype[0] = LLVMPointerType(pBuild->PLtypeFromPTin(pTinstruct), 0);
	auto pLtypeFunction = LLVMFunctionType(pLtypeVoid, apLtype, 1, false);

	char aChName[128];
	EWC::SStringBuffer strbufName(aChName, EWC_DIM(aChName));
	auto strPunyName = StrPunyEncode(pTinstruct->m_strName.PCoz());
	FormatCoz(&strbufName, "_%s_INIT", strPunyName.PCoz());

	LLVMOpaqueValue * pLvalFunc = LLVMAddFunction(pBuild->m_pLmoduleCur, aChName, pLtypeFunction);
	LLVMSetLinkage(pLvalFunc, LLVMPrivateLinkage);

	CAlloc * pAlloc = pBuild->m_pAlloc;
	CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);
	pBuild->AddManagedVal(pProc);

	auto pCgstruct = pBuild->PCgstructEnsure(pTinstruct);
	pCgstruct->m_pProcInitMethod = pProc;

	pProc->m_pLval = pLvalFunc;
	pProc->m_pBlockFirst = pBuild->PBlockCreate(pProc, aChName);
	
	auto pBlockPrev = pBuild->m_pBlockCur;
	auto pProcPrev = pBuild->m_pProcCur;

	pBuild->ActivateProc(pProc, pProc->m_pBlockFirst);

	{ // create debug info
		CreateDebugInfo(pWork, pBuild, pStnodStruct, pTinstruct);

		int cpTinParam = 1;
		LLVMValueRef apLvalParam[1];

		u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
		u64 cBitAlign = cBitSize;
		apLvalParam[0] = LLVMDIBuilderCreatePointerType(pBuild->m_pDib, (LLVMValueRef)pTinstruct->m_pCgvalDIType, cBitSize, cBitAlign, "");

		auto pLvalDIType = LLVMDIBuilderCreateFunctionType(pBuild->m_pDib, apLvalParam, cpTinParam, cBitSize, cBitAlign);

		pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
										pWork,
										pBuild,
										aChName,
										aChName,
										pStnodStruct,
										pStnodStruct,
										pLvalDIType,
										pProc->m_pLval);

		s32 iLine, iCol;
		CalculateLinePosition(pWork, &pStnodStruct->m_lexloc, &iLine, &iCol);

		pProc->m_pLvalDebugLocCur = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pProc->m_pLvalDIFunction);
		LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pProc->m_pLvalDebugLocCur);
	}

	// load our 'this' argument
	int cpLvalParams = LLVMCountParams(pProc->m_pLval);
	EWC_ASSERT(cpLvalParams == 1, "only expected 'this' parameter");

	auto apLvalParam = (LLVMValueRef *)alloca(sizeof(LLVMValueRef) * cpLvalParams);
	LLVMGetParams(pProc->m_pLval, apLvalParam);
	LLVMSetValueName(apLvalParam[0], "this");

	CIRArgument * pArgThis = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
	pArgThis->m_pLval = apLvalParam[0];
	pBuild->AddManagedVal(pArgThis);

	LLVMOpaqueValue * apLvalIndex[2] = {};
	apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);

	CSTNode * pStnodList = pStnodStruct->PStnodChildSafe(1);
	if (pStnodList && !EWC_FVERIFY(pStnodList->m_park == PARK_List, "expected member decl list"))
		pStnodList = nullptr;

	int cTypemembField = (int)pTinstruct->m_aryTypemembField.C();
	for (int iTypememb = 0; iTypememb < cTypemembField; ++iTypememb)
	{
		apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), iTypememb, false);

		CSTNode * pStnodInit = nullptr;
		if (pStnodList)
		{
			CSTNode * pStnodDecl = pStnodList->PStnodChild(iTypememb);
			if (EWC_FVERIFY(pStnodDecl->m_park == PARK_Decl, "expected member declaration"))
			{
				auto pStdecl = PStmapDerivedCast<CSTDecl *>(pStnodDecl->m_pStmap);
				pStnodInit = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
			}
		}

		auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

		auto pInstGEP = pBuild->PInstCreateGEP(pArgThis, apLvalIndex, 2, "initGEP");
		(void)PValInitialize(pWork, pBuild, pTypememb->m_pTin, pInstGEP, pStnodInit);
	}

	LLVMBuildRetVoid(pBuild->m_pLbuild);
	pBuild->ActivateProc(pProcPrev, pBlockPrev);

	pBuild->m_arypProcVerify.Append(pProc);
	return pProc;
}

CIRProcedure * CBuilderIR::PProcCreateImplicit(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod)
{
	auto pProc = EWC_NEW(m_pAlloc, CIRProcedure) CIRProcedure(m_pAlloc);
	AddManagedVal(pProc);

	auto pLtypeFunction = LLVMFunctionType(LLVMVoidType(), nullptr, 0, false);
	const char * pCozName = pTinproc->m_strName.PCoz();
	pProc->m_pLval = LLVMAddFunction(m_pLmoduleCur, pCozName, pLtypeFunction);

	pProc->m_pBlockLocals = PBlockCreate(pProc, pCozName);
	pProc->m_pBlockFirst = PBlockCreate(pProc, pCozName);

	u64 cBitSize = LLVMPointerSize(m_pTargd) * 8;
	u64 cBitAlign = cBitSize;
	auto pLvalDIFunctionType = LLVMDIBuilderCreateFunctionType(m_pDib, nullptr, 0, cBitSize, cBitAlign);

	pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
									pWork,
									this,
									pCozName,
									nullptr,
									pStnod,
									pStnod,
									pLvalDIFunctionType,
									pProc->m_pLval);
	return pProc;
}

CIRProcedure * CBuilderIR::PProcCreate(
	CWorkspace * pWork,
	STypeInfoProcedure * pTinproc,
	const char * pChzMangled,
	CSTNode * pStnod,
	CSTNode * pStnodBody,
	CDynAry<LLVMOpaqueType *> * parypLtype,
	LLVMOpaqueType * pLtypeReturn)
{

	bool fHasVarArgs = pTinproc->m_grftinproc.FIsSet(FTINPROC_HasVarArgs);
	auto pProc = EWC_NEW(m_pAlloc, CIRProcedure) CIRProcedure(m_pAlloc);
	AddManagedVal(pProc);

	auto pLtypeFunction = LLVMFunctionType(pLtypeReturn, parypLtype->A(), (u32)parypLtype->C(), fHasVarArgs);

	pProc->m_pLval = LLVMAddFunction(m_pLmoduleCur, pChzMangled, pLtypeFunction);

	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
	if (!EWC_FVERIFY(pStproc, "expected stproc"))
		return nullptr;

	if (!pStproc->m_fIsForeign && !pStproc->m_fUseUnmangledName)
	{
		LLVMSetLinkage(pProc->m_pLval, LLVMPrivateLinkage);
	}

	pProc->m_pLval = pProc->m_pLval; // why is this redundant?

	if (pTinproc->m_callconv != CALLCONV_Nil)
	{
		LLVMSetFunctionCallConv(pProc->m_pLval, CallingconvFromCallconv(pTinproc->m_callconv));
	}

	const char * pChzInlineAttr = nullptr;
	switch (pTinproc->m_inlinek)
	{
		case INLINEK_NoInline:		pChzInlineAttr = "noinline"; break;
		case INLINEK_AlwaysInline:	pChzInlineAttr = "alwaysinline";	break;
		default: break;
	}

	if (pChzInlineAttr)
	{
		auto pLctx = LLVMGetModuleContext(m_pLmoduleCur);
		u32 attrKind = LLVMGetEnumAttributeKindForName(pChzInlineAttr, CCh(pChzInlineAttr));
		if (EWC_FVERIFY(attrKind != 0, "unknown inline attr kind"))
		{ 
			LLVMCreateEnumAttribute(pLctx, attrKind, 0);
		}
		//LLVMAddFunctionAttr(pProc->m_pLval, LLVMNoInlineAttribute);	
	}

	if (!pStproc->m_fIsForeign)
	{
		pProc->m_pBlockLocals = PBlockCreate(pProc, pChzMangled);
		pProc->m_pBlockFirst = PBlockCreate(pProc, pChzMangled);

		if (EWC_FVERIFY(pTinproc, "expected type info procedure"))
		{
			if (pTinproc->m_pCgvalDIType == nullptr)
			{
				CreateDebugInfo(pWork, this, pStnod, pTinproc);
			}

			pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
											pWork,
											this,
											pTinproc->m_strName.PCoz(),
											PChzVerifyAscii(pTinproc->m_strMangled.PCoz()),
											pStnod,
											pStnodBody,
											(LLVMValueRef)pTinproc->m_pCgvalDIType,
											pProc->m_pLval);

			s32 iLine, iCol;
			CalculateLinePosition(pWork, &pStnodBody->m_lexloc, &iLine, &iCol);

			pProc->m_pLvalDebugLocCur = LLVMCreateDebugLocation(m_pLbuild, iLine, iCol, pProc->m_pLvalDIFunction);
		}
	}

	if (EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check"))
	{
		SetSymbolValue(pStnod->m_pSym, pProc);
	}

	return pProc;
}

void CBuilderIR::SetupParamBlock(
	CWorkspace * pWork,
	CIRProcedure * pProc,
	CSTNode * pStnod,
	CSTNode * pStnodParamList,
	EWC::CDynAry<LLVMOpaqueType *> * parypLtype)
{
	// set up llvm argument values for our formal parameters

	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
	int cpStnodParam = pStnodParamList->CStnodChild();
	int cpLvalParams = LLVMCountParams(pProc->m_pLval);

	CAlloc * pAlloc = m_pAlloc;
	LLVMValueRef * appLvalParams = (LLVMValueRef*)pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, cpLvalParams);
	LLVMGetParams(pProc->m_pLval, appLvalParams);

	auto pDif = PDifEnsure(pWork, this, pStnod->m_lexloc.m_strFilename);

	int ipLvalParam = 0;
	for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
	{
		CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
		if (pStnodParam->m_park == PARK_VariadicArg)
			continue;

		CSTDecl * pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
		if (pStdecl && pStdecl->m_fIsBakedConstant)
			continue;

		EWC_ASSERT(ipLvalParam < cpLvalParams, "parameter count mismatch");

		if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
		{
			LLVMValueRef  pLvalParam = appLvalParams[ipLvalParam];
			auto strArgName = StrPunyEncode(pStnodParam->m_pSym->m_strName.PCoz());
			LLVMSetValueName(pLvalParam, strArgName.PCoz());

			CIRArgument * pArg = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
			pArg->m_pLval = pLvalParam;
			AddManagedVal(pArg);

			if (!pStproc->m_fIsForeign)
			{
				auto pInstAlloca = PValCreateAlloca((*parypLtype)[ipLvalParam], 1, strArgName.PCoz());
				SetSymbolValue(pStnodParam->m_pSym, pInstAlloca);

				s32 iLine;
				s32 iCol;
				CalculateLinePosition(pWork, &pStnodParam->m_lexloc, &iLine, &iCol);

				CreateDebugInfo(pWork, this, pStnod, pStnodParam->m_pTin);

				auto pLvalDIVariable = LLVMDIBuilderCreateParameterVariable(
					m_pDib,
					pProc->m_pLvalDIFunction,
					strArgName.PCoz(),
					ipLvalParam + 1,
					pDif->m_pLvalFile,
					iLine,
					(LLVMValueRef)pStnodParam->m_pTin->m_pCgvalDIType,
					true,
					0);

				(void)LLVMDIBuilderInsertDeclare(
					m_pDib,
					pInstAlloca->m_pLval,
					pLvalDIVariable,
					pProc->m_pLvalDIFunction,
					iLine,
					iCol,
					m_pBlockCur->m_pLblock);

				auto pValSym = PValFromSymbol(pStnodParam->m_pSym);
				(void) PInstCreateStore(pValSym, pArg);
			}
		}
		++ipLvalParam;
	}

	m_pAlloc->EWC_FREE(appLvalParams);
}

template <typename BUILD>
typename BUILD::Proc * PProcCodegenPrototype(CWorkspace * pWork, BUILD * pBuild, CSTNode * pStnod)
{
	CSTNode * pStnodParamList = nullptr;
	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodName = nullptr;
	CSTNode * pStnodAlias = nullptr;
	CSTNode * pStnodBody = nullptr;
	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
	if (EWC_FVERIFY(pStproc, "Encountered procedure without CSTProcedure"))
	{
		pStnodParamList = pStnod->PStnodChildSafe(pStproc->m_iStnodParameterList);
		pStnodReturn = pStnod->PStnodChildSafe(pStproc->m_iStnodReturnType);
		pStnodName = pStnod->PStnodChildSafe(pStproc->m_iStnodProcName);
		pStnodAlias = pStnod->PStnodChildSafe(pStproc->m_iStnodForeignAlias);
		pStnodBody = pStnod->PStnodChildSafe(pStproc->m_iStnodBody);
	}

	bool fHasVarArgs = false;
	CDynAry<BUILD::LType *> arypLtype(pBuild->m_pAlloc, EWC::BK_CodeGen);
	if (pStnodParamList && EWC_FVERIFY(pStnodParamList->m_park == PARK_ParameterList, "expected parameter list"))
	{
		int cpStnodParams = pStnodParamList->CStnodChild();
		for (int ipStnod = 0; ipStnod < cpStnodParams; ++ipStnod)
		{
			CSTNode * pStnodDecl = pStnodParamList->PStnodChild(ipStnod);
			if (pStnodDecl->m_park == PARK_VariadicArg)
			{
				fHasVarArgs = true;
				continue;
			}

			if (!EWC_FVERIFY(pStnodDecl->m_park == PARK_Decl, "bad parameter"))
				continue;

			auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnodDecl->m_pStmap);
			if (pStdecl && pStdecl->m_fIsBakedConstant)
				continue;

			auto pLtype = pBuild->PLtypeFromPTin(pStnodDecl->m_pTin);
			if (EWC_FVERIFY(pLtype, "Could not compute LLVM type for parameter"))
			{
				arypLtype.Append(pLtype);
			}
		}
	}

	BUILD::LType * pLtypeReturn = nullptr;
	if (pStnodReturn)
	{
		pLtypeReturn = pBuild->PLtypeFromPTin(pStnodReturn->m_pTin);
		EWC_FVERIFY(pLtypeReturn, "Could not compute LLVM type for return type");
	}
	if (!pLtypeReturn)
	{
		pLtypeReturn = pBuild->PLtypeVoid();
	}

	auto pTinproc = PTinRtiCast<STypeInfoProcedure *>(pStnod->m_pTin);
	EWC_ASSERT(pTinproc, "Exected procedure type");

	char aCh[256];
	EWC_ASSERT(!pTinproc->FHasGenericArgs(), "generic arg should not make it to codegen");
	EWC_ASSERT(pTinproc->m_strMangled.PCoz(), "missing procedure mangled name in tinproc '%s', pStnod = %p", pTinproc->m_strName.PCoz(), pStnod);
	const char * pChzMangled = PChzVerifyAscii(pTinproc->m_strMangled.PCoz());

	if (pStproc->m_fIsForeign)
	{
		if (pStnodAlias)
		{
			CString strProcAlias = StrFromIdentifier(pStnodAlias);
			pChzMangled = PChzVerifyAscii(strProcAlias.PCoz());	// BB - should this be punnycoded? needs to match C
		}
	}

	if (!pChzMangled)
	{
		GenerateUniqueName(&pWork->m_unset, "__AnnonFunc__", aCh, EWC_DIM(aCh));
		pChzMangled = aCh;
	}

	auto pProc = pBuild->PProcCreate(pWork, pTinproc, pChzMangled, pStnod, pStnodBody, &arypLtype, pLtypeReturn);
	auto pBlockPrev = pBuild->m_pBlockCur;
	auto pProcPrev = pBuild->m_pProcCur;

	pBuild->ActivateProc(pProc, (pStproc->m_fIsForeign) ? pBlockPrev : pProc->m_pBlockLocals);
	if (pStnodParamList)
	{
		pBuild->SetupParamBlock(pWork, pProc, pStnod, pStnodParamList, &arypLtype);
	}
	pBuild->ActivateProc(pProcPrev, pBlockPrev);

	return pProc;
}

enum CGEPK // Code Generation Entry Point Kind
{
	CGEPK_None,
	CGEPK_ImplicitProc,	// code will be added in order to an implicit proc for unit tests
	CGEPK_Normal,
};

CGEPK CgepkCompute(CWorkspace * pWork, CSTNode * pStnod)
{
	if (pStnod->m_grfstnod.FIsSet(FSTNOD_NoCodeGeneration))
		return CGEPK_None;

	bool fGlobalTypeDeclaration = false;

	bool fImplicitFunction = (pWork->m_globmod == GLOBMOD_UnitTest);
	switch (pStnod->m_park)
	{
	case PARK_StructDefinition:		fGlobalTypeDeclaration = true;	break;
	case PARK_EnumDefinition:		fGlobalTypeDeclaration = true;	break;
	case PARK_Typedef:				fGlobalTypeDeclaration = true;	break;
	case PARK_Nop:					fGlobalTypeDeclaration = true;	break;
	case PARK_ProcedureDefinition:	fImplicitFunction = false;		break;

	case PARK_Decl:
	case PARK_ConstantDecl:
		break;
	default:
		EWC_ASSERT(fImplicitFunction, "Unexpected top level PARK (%s)", PChzFromPark(pStnod->m_park));
		break;
	}

	if (fGlobalTypeDeclaration)
		return CGEPK_None;

	if (fImplicitFunction)
		return CGEPK_ImplicitProc;
	return CGEPK_Normal;
}

template <typename BUILD>
void CodeGenEntryPoints(
	CWorkspace * pWork,
	BUILD * pBuild, 
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	CAry<SWorkspaceEntry *> * parypEntryOrder,
	typename BUILD::Proc ** ppProcUnitTest)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	BUILD::Proc * pProcImplicit = nullptr;

	CDynAry<CSTNode *> arypStnodUnitTest(pWork->m_pAlloc, BK_UnitTest, (int)parypEntryOrder->C());	// entry points from unit tests that are not procedure definitions

	// Two passes so that entry points added to an implicit procedure are added in lexical order, not 
	//  Type-checked order.

	SWorkspaceEntry ** ppEntryMac = parypEntryOrder->PMac();
	for (SWorkspaceEntry ** ppEntry = parypEntryOrder->A(); ppEntry != ppEntryMac; ++ppEntry)
	{
		SWorkspaceEntry * pEntry = *ppEntry;
		CSTNode * pStnod = pEntry->m_pStnod;
		auto cgepk = CgepkCompute(pWork, pStnod);

		if (cgepk == CGEPK_Normal)
		{
			if (pStnod->m_park == PARK_ProcedureDefinition && pStnod->m_pSym->m_symdep != SYMDEP_Used)
			{
				EWC_ASSERT(
					pStnod->m_pSym->m_symdep == SYMDEP_Unused,
					"unexpected symbol dependency type (%d)",
					pStnod->m_pSym->m_symdep);

				//printf("Skipping dead code %s\n", pStnod->m_pSym->m_strName.PCoz());
				continue;
			}

			PValGenerate(pWork, pBuild, pStnod, VALGENK_Instance);

			if (pStnod->m_park == PARK_ProcedureDefinition)
			{
				auto pVal = pBuild->PValFromSymbol(pStnod->m_pSym);
				if (!EWC_FVERIFY(pVal && pVal->m_valk, "Expected procedure"))
					return;

				//pEntry->m_pProc = pProc;
			}
		}
	}

	BlockListEntry::CIterator iter(pblistEntry);
	while (SWorkspaceEntry * pEntry = iter.Next()) 
	{
		CSTNode * pStnod = pEntry->m_pStnod;
		auto cgepk = CgepkCompute(pWork, pStnod);

		if (cgepk == CGEPK_ImplicitProc)	
		{
			arypStnodUnitTest.Append(pStnod);
		}
	}

	if (!arypStnodUnitTest.FIsEmpty() && !pProcImplicit)
	{
		char aCh[128];
		GenerateUniqueName(&pWork->m_unset, "__AnonFunc__", aCh, EWC_DIM(aCh));
		auto pTinproc = PTinprocAlloc(pWork->m_pSymtab, 0, 0, aCh);
		pProcImplicit = pBuild->PProcCreateImplicit(pWork, pTinproc, arypStnodUnitTest[0]);

		GenerateMethodBody(pWork, pBuild, pProcImplicit, arypStnodUnitTest.A(), (int)arypStnodUnitTest.C(), true);
	}

	pBuild->FinalizeBuild(pWork);

	if (ppProcUnitTest)
	{
		*ppProcUnitTest = pProcImplicit;
	}
}

void CodeGenEntryPointsLlvm(
	CWorkspace * pWork,
	CBuilderIR * pBuildir,
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	EWC::CAry<SWorkspaceEntry *> * parypEntryOrder)
{
	CodeGenEntryPoints(pWork, pBuildir, pSymtabTop, pblistEntry, parypEntryOrder, nullptr);
}

void CodeGenEntryPointsBytecode(
	CWorkspace * pWork,
	BCode::CBuilder * pBuildBc, 
	CSymbolTable * pSymtabTop,
	BlockListEntry * pblistEntry,
	EWC::CAry<SWorkspaceEntry *> * parypEntryOrder,
	BCode::SProcedure ** pProcUnitTest)
{
	CodeGenEntryPoints(pWork, pBuildBc, pSymtabTop, pblistEntry, parypEntryOrder, pProcUnitTest);
}

void CBuilderIR::ComputeDataLayout(SDataLayout * pDlay)
{
	auto pLtypeBool = LLVMInt1Type();
	auto pLtypeInt = LLVMInt64Type();
	auto pLtypeFloat = LLVMFloatType();

	pDlay->m_cBBool = (s32)(LLVMSizeOfTypeInBits(m_pTargd, pLtypeBool) + 7) / 8;
	pDlay->m_cBInt = (s32)LLVMSizeOfTypeInBits(m_pTargd, pLtypeInt) / 8;
	pDlay->m_cBFloat = (s32)LLVMSizeOfTypeInBits(m_pTargd, pLtypeFloat) / 8;
	pDlay->m_cBPointer = (s32)LLVMPointerSize(m_pTargd);
	pDlay->m_cBStackAlign = pDlay->m_cBPointer;
}

void CBuilderIR::FinalizeBuild(CWorkspace * pWork)
{
	LLVMDIBuilderFinalize(m_pDib);

	LLVMBool fHaveAnyFailed = false;
	CIRProcedure ** ppProcVerifyEnd = m_arypProcVerify.PMac();
	for (CIRProcedure ** ppProcVerifyIt = m_arypProcVerify.A(); ppProcVerifyIt != ppProcVerifyEnd; ++ppProcVerifyIt)
	{
		auto pProc = *ppProcVerifyIt;
		LLVMBool fFunctionFailed = LLVMVerifyFunction(pProc->m_pLval, LLVMPrintMessageAction);
		if (fFunctionFailed)
		{
			CString strName("unknown");
			if (pProc->m_pStnod && pProc->m_pStnod->m_pTin)
			{
				strName = pProc->m_pStnod->m_pTin->m_strName;
			}
			printf("\n\n Internal compiler error during codegen for '%s'\n", strName.PCoz());
		}
		fHaveAnyFailed |= fFunctionFailed;
	}

	if (fHaveAnyFailed)
	{
		printf("\n\n LLVM IR:\n");
		PrintDump();
		EmitError(pWork, nullptr, ERRID_UnknownError, "Code generation for entry point is invalid");
	}
}

int NExecuteAndWait(
	const char * pChzProgram,
	const char ** ppChzArgs,
	const char ** ppChzEnvp,
	unsigned tWait,
	unsigned cBMemoryLimit,
	EWC::CString * pStrError,
	bool * pFExecutionFailed)
{
	std::string strError;
	llvm::StringRef strrProgram(pChzProgram);
	int nReturn = llvm::sys::ExecuteAndWait(
								strrProgram, 
								ppChzArgs, 
								ppChzEnvp,
			                    0, 	// const StringRef ** ppStrrRedirects
			                    tWait,
			                    cBMemoryLimit,
			                    &strError,
			                    pFExecutionFailed);
	if (pStrError && !strError.empty())
	{
		*pStrError = strError.c_str();	
	}
	return nReturn;
}


bool FTestUniqueNames(CAlloc * pAlloc)
{
#ifdef EWC_TRACK_ALLOCATION
	u8 aBAltrac[1024 * 100];
	CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

	CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
	pAlloc->SetAltrac(pAltrac);
#endif

	size_t cbFreePrev = pAlloc->CB();
	{
		const char * pChzIn;
		char aCh[128];
		SUniqueNameSet unset(pAlloc, EWC::BK_Workspace, 0);

		pChzIn = "funcName";
		GenerateUniqueName(&unset, pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual(pChzIn, aCh), "bad unique name");

		GenerateUniqueName(&unset, pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("funcName1", aCh), "bad unique name");

		pChzIn = "funcName20";
		GenerateUniqueName(&unset, pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("funcName20", aCh), "bad unique name");

		pChzIn = "234";
		GenerateUniqueName(&unset, pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("234", aCh), "bad unique name");

		pChzIn = "test6000";
		GenerateUniqueName(&unset, pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("test6000", aCh), "bad unique name");

		pChzIn = "test6000";
		GenerateUniqueName(&unset, pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("test6001", aCh), "bad unique name");
	}

	size_t cbFreePost = pAlloc->CB();
#ifdef EWC_TRACK_ALLOCATION
	if (cbFreePrev != cbFreePost)
	{
		pAlloc->PrintAllocations();
	}

	DeleteAltrac(&allocAltrac, pAltrac);
	pAlloc->SetAltrac(nullptr);
#endif
	EWC_ASSERT(cbFreePrev == cbFreePost, "memory leak testing unique names");

	return true;
}

size_t CChConstructFilename(const char * pChzFilenameIn, const char * pChzExtension, char * pChzFilenameOut, size_t cChOutMax)
{
	// remove the last extension (if one exists) and replace it with the supplied one.

	char * pChzPeriod = nullptr;
	char * pChzOut = pChzFilenameOut;
	char * pChzOutMax = &pChzFilenameOut[cChOutMax];
	const char * pChIt = pChzFilenameIn;
	for ( ; *pChIt != '\0' && pChzOut != pChzOutMax; ++pChIt)
	{
		if (*pChIt == '.')
		{
			pChzPeriod = pChzOut;
		}

		*pChzOut++ = *pChIt;
	}

	if (pChzPeriod)
		pChzOut = pChzPeriod;

	pChIt = pChzExtension; 
	for ( ; *pChIt != '\0' && pChzOut != pChzOutMax; ++pChIt)
	{
		*pChzOut++ = *pChIt;
	}

	*pChzOut++ = '\0';
	return pChzOut - pChzFilenameOut;
}

void CompileToObjectFile(CWorkspace * pWork, CBuilderIR * pBuild, const char * pChzFilenameIn)
{
	char * pChzTriple = LLVMGetDefaultTargetTriple();

	const char * pChzExtension;
	if (pWork->m_targetos == TARGETOS_Windows)
      pChzExtension = ".obj";
    else
      pChzExtension = ".o";

	char aChFilenameOut[CWorkspace::s_cBFilenameMax];
	size_t cCh = CChConstructFilename(pChzFilenameIn, pChzExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));
	pWork->SetObjectFilename(aChFilenameOut, cCh);

	char * pChzError = nullptr;
	LLVMBool fFailed = LLVMTargetMachineEmitToFile(pBuild->m_pLtmachine, pBuild->m_pLmoduleCur, aChFilenameOut, LLVMObjectFile, &pChzError);

	if (fFailed)
	{
		EmitError(pWork, nullptr, ERRID_ObjFileFail, "Error generating object file\n%s", pChzError);
		LLVMDisposeMessage(pChzError);
	}

	LLVMDisposeMessage(pChzTriple);
	pChzTriple = nullptr;
}

void InitLLVM(EWC::CAry<const char*> * paryPCozArgs)
{
	llvm::cl::ParseCommandLineOptions(S32Coerce(paryPCozArgs->C()), paryPCozArgs->A());

	// Initialize targets first, so that --version shows registered targets.
	LLVMInitializeNativeTarget();
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86Target();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmPrinter();
	//LLVMInitializeX86AsmParser();

	// Initialize codegen and IR passes used by llc so that the -print-after,
	// -print-before, and -stop-after options work.

	LLVMPassRegistryRef lpassregistry = LLVMGetGlobalPassRegistry();
	LLVMInitializeCore(lpassregistry);
	//LLVMInitializeCodeGen(*lpassregistry);
	//LLVMInitializeLoopStrengthReducePass(*lpassregistry);
	//LLVMInitializeLowerIntrinsicsPass(*lpassregistry);
	//LLVMInitializeUnreachableBlockElimPass(*lpassregistry);
}

void ShutdownLLVM()
{
	LLVMShutdown();
}

bool FCompileModule(CWorkspace * pWork, GRFCOMPILE grfcompile, const char * pChzFilenameIn)
{
	SLexer lex;

	(void) pWork->PFileEnsure(pChzFilenameIn, CWorkspace::FILEK_Source);

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		CWorkspace::SFile * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != CWorkspace::FILEK_Source)
			continue;

		char aChFilenameOut[CWorkspace::s_cBFilenameMax];
		(void)CChConstructFilename(pFile->m_strFilename.PCoz(), CWorkspace::s_pCozSourceExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));

		pFile->m_pChzFileBody = pWork->PChzLoadFile(aChFilenameOut, pWork->m_pAlloc);
		if (!pFile->m_pChzFileBody)
			continue;

		const char * pCozFileBody = PCozSkipUnicodeBOM(pFile->m_pChzFileBody);

		printf("Parsing %s\n", pFile->m_strFilename.PCoz());
		BeginParse(pWork, &lex, (char *)pCozFileBody);
		lex.m_pCozFilename = pFile->m_strFilename.PCoz();

		ParseGlobalScope(pWork, &lex, true);

		EndParse(pWork, &lex);

	}

	if (!pWork->m_pErrman->FHasErrors())
	{
		printf("Type Check:\n");
		PerformTypeCheck(
			pWork->m_pAlloc,
			pWork->m_pErrman,
			pWork->m_pSymtab,
			&pWork->m_blistEntry,
			&pWork->m_arypEntryChecked,
			pWork->m_globmod);

		if (!pWork->m_pErrman->FHasErrors())
		{

#if EWC_X64
			printf("Code Generation (x64):\n");
#else
			printf("Code Generation (x86):\n");
#endif
			CBuilderIR build(pWork, pChzFilenameIn, grfcompile);
			
			CodeGenEntryPointsLlvm(pWork, &build, pWork->m_pSymtab, &pWork->m_blistEntry, &pWork->m_arypEntryChecked);

			CompileToObjectFile(pWork, &build, pChzFilenameIn);

			
			if (grfcompile.FIsSet(FCOMPILE_PrintIR))
			{
				build.PrintDump();
			}
		}
	}

	int cError, cWarning;
	pWork->m_pErrman->ComputeErrorCounts(&cError, &cWarning);
	if (cError != 0)
	{
		printf("_______]  Compile FAILED: %d errors, %d warnings  [_______\n", cError, cWarning);
	}
	else
	{
		printf("+++ Success: 0 errors, %d warnings +++\n", cWarning);
	}

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		CWorkspace::SFile * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != CWorkspace::FILEK_Source)
			continue;

		if (pFile->m_pChzFileBody)
		{
			pWork->m_pAlloc->EWC_DELETE((u8 *)pFile->m_pChzFileBody);
			pFile->m_pChzFileBody = nullptr;
		}

		if (pFile->m_pDif)
		{
			pWork->m_pAlloc->EWC_DELETE(pFile->m_pDif);
			pFile->m_pDif = nullptr;
		}
	}

	return cError == 0;
}


