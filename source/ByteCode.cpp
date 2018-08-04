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
#include "CodeGen.h"
#include "parser.h"
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Util.h"
#include "workspace.h"
#include "dyncall\dynload\dynload.h"
#include "dyncall\dyncall\dyncall.h"
#include "dyncall\dyncall\dyncall_signature.h"

// Remaining bytecode tasks
// [ ] Clean up remaining TBDs
// [x]  Phi nodes
// [ ]  Reflection
// [x]  Switch nodes
// [ ]  FFI 
// [x]		FFI basics
// [x]		FFI variadic functions
// [ ]		FFI tests
// [x] Passing large arguments (hidden reference args)
// [x] Returning large arguments (hidden reference args)


//#define EWC_TRACE_FOREIGN_SYMBOLS 
//#define EWC_TRACE_GLOBAL
//3660gg#define EWC_TRACE_RETURN_STACK

// Stack layout (grows down)
//	4 |___null____________________| +
//	3 |___childRet________________| | 
//	2 |___working_________________| |
//	1 |___Local var_______________| | 
//	0 |___Local var_______________|_v_	iBStack(0) for main                                 ___
//	  |.................. pad arg.|						                                     ^
// a4 |___return__________________| |	iBStack for return storage in main stack frame       |   pProc->m_cBArg
// a3 |___arg 1___________________| |                                                        |
// a2 |___arg 0___________________| | 														 | 
// a1 |___pProcCall ______________|_v_	Address of SProcedure of the calling proc			 |
// a0 |___pInstCall ______________|_v_	Address of call instruction from calling proc       _v_
//	  |.................. pad ....|                                                          ^
//	3 |___________________________| +                                                        |   pProc->m_cBStack
//	2 |___________________________| |                                                        |
//	1 |___working_________________| |  childProc (a,b) -> childRet                           |
//	0 |___local var_______________|_v_														_v_



// Stack layout varargs (grows down)
//	4 |___null____________________| +
//	3 |___childRet________________| | 
//	2 |___working_________________| |
//	1 |___Local var_______________| | 
//	0 |___Local var_______________|_v_	iBStack(0) for main                                 ___
//	  |.................. pad arg.|	|					                                     ^   cBArgVariadic
// a6 |___SBoxedArg 2 ____________| |                                                        |
// a5 |___SBoxedArg 1 ____________| |                                                       _v_
// a4 |___return__________________| |	iBStack for return storage in main stack frame       ^   pProc->m_cBArg
// a3 |___arg 1___________________| |                                                        |
// a2 |___arg 0___________________| |                                                        |
// a1 |___pProcCall ______________|_v_	Address of SProcedure of the calling proc			 |
// a0 |___pInstCall_______________|_v_	Address of call instruction from calling proc       _v_
//	  |.................. pad ....|                                                          ^
//	3 |___________________________| +                                                        |   pProc->m_cBStack
//	2 |___________________________| |                                                        |
//	1 |___working_________________| |  childProc (a,b) -> childRet                           |
//	0 |___local var_______________|_v_														_v_



using namespace EWC;

void CalculateByteSizeAndAlign(SDataLayout * pDlay, STypeInfo * pTin, u64 * pcB, u64 * pcBAlign);

void CalculateStructAlignment(SDataLayout * pDlay, STypeInfoStruct * pTinstruct)
{
	u64 cB = 0;
	u64 cBAlignStruct = 1;

	auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
	for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb)
	{
		u64 cBField;
		u64 cBAlignField;
		CalculateByteSizeAndAlign(pDlay, pTypememb->m_pTin, &cBField, &cBAlignField);

		cB = CBAlign(cB, cBAlignField);
		pTypememb->m_dBOffset = s32(cB);

		cB += cBField;
		cBAlignStruct = ewcMax(cBAlignStruct, cBAlignField);
	}

	pTinstruct->m_cB = cB;
	pTinstruct->m_cBAlign = cBAlignStruct;
}

void CalculateByteSizeAndAlign(SDataLayout * pDlay, STypeInfo * pTin, u64 * pcB, u64 * pcBAlign)
{
	// return the embeded size of a type (ie. how many bytes would be needed to include it in a struct)

	s64 cB = 1;
	switch (pTin->m_tink)
	{
	case TINK_Integer:	cB = ((STypeInfoInteger*)pTin)->m_cBit >> 0x3;		break;
	case TINK_Float:	cB = ((STypeInfoFloat*)pTin)->m_cBit >> 0x3;		break;
	case TINK_Bool:		cB = pDlay->m_cBBool;								break;
    case TINK_Enum:		CalculateByteSizeAndAlign(pDlay, ((STypeInfoEnum *)pTin)->m_pTinLoose, pcB, pcBAlign);	return;
	case TINK_Qualifier: CalculateByteSizeAndAlign(pDlay, ((STypeInfoQualifier *)pTin)->m_pTin, pcB, pcBAlign);	return;

    case TINK_Null:		cB = pDlay->m_cBPointer;	break;
	case TINK_Pointer:	cB = pDlay->m_cBPointer;	break;
	case TINK_Procedure:cB = pDlay->m_cBPointer;	break;

    case TINK_Struct: 
	{
		auto pTinstruct = (STypeInfoStruct *)pTin;
		if (pTinstruct->m_cB < 0)
		{
			CalculateStructAlignment(pDlay, pTinstruct);
		}

		*pcB = pTinstruct->m_cB;
		*pcBAlign = pTinstruct->m_cBAlign;
		return;	
	}

    case TINK_Array:
	{
		auto pTinary = (STypeInfoArray *)pTin;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed: 
		{
			CalculateByteSizeAndAlign(pDlay, pTinary->m_pTin, pcB, pcBAlign);
			*pcB = *pcB * pTinary->m_c;
			return;
		}
		case ARYK_Reference:
		{
			*pcB = 2 * pDlay->m_cBPointer;	//(count, pointer) 
			*pcBAlign = pDlay->m_cBPointer;
			return;
		}
		case ARYK_Dynamic:
			EWC_ASSERT(false, "dyn array is TBD");
		} break;
	}

	case TINK_Literal:
	{
		auto pTinlit = (STypeInfoLiteral *)pTin;

		bool fIsContainerLiteral = pTinlit->m_litty.m_litk == LITK_Array;
		EWC_ASSERT(pTinlit->m_fIsFinalized || fIsContainerLiteral, "cannot calculate size of unfinalized literal");

		switch (pTinlit->m_litty.m_litk)
		{
		case LITK_Array:
			{
				CalculateByteSizeAndAlign(pDlay, pTinlit->m_pTinSource, pcB, pcBAlign);
				*pcB = pTinlit->m_c * *pcB;
				return;
			}
		case LITK_String:	cB = pDlay->m_cBPointer;				break;
		default:			cB = pTinlit->m_litty.m_cBit >> 0x3;	break;
		}

	} break;

	case TINK_Generic:
		EWC_ASSERT(false, "generic types should be resolved prior to codegen");
		break;

    case TINK_Void:
	{
		*pcB = 0;
		*pcBAlign = 1;
		return;
	}break;
	case TINK_ForwardDecl:
    case TINK_Any:
	default:
		EWC_ASSERT(false, "unhandled type kind in CBFromTin. TINK_%s", PChzFromTink(pTin->m_tink));
		break;
	}

	*pcB = cB;
	*pcBAlign = cB;
}

void BuildStubDataLayout(SDataLayout * pDlay)
{
	pDlay->m_cBBool = 1;
	pDlay->m_cBInt = sizeof(int);
	pDlay->m_cBFloat = sizeof(float);
	pDlay->m_cBPointer = sizeof(void *);
	pDlay->m_cBStackAlign = sizeof(void *);
}

namespace BCode 
{
inline bool FUseLargeOperand(IROP irop)
{
	// some instructions use register sized args (1,2,4,8 bytes)
	// but these instructions use m_wordRhs.m_s32 to handle large structures

	switch (irop)
	{
	case IROP_Store:
	case IROP_Load:
	case IROP_StoreToIdx:
	case IROP_StoreToReg:
		return true;
	default:
		return false;
	}
}

// number of bytes used by this instruction's operands
static inline s32 CBOperand(const SInstruction * pInst)
{
	if (FUseLargeOperand(pInst->m_irop))
	{
		return pInst->m_wordRhs.m_s32;
	}
	if (pInst->m_irop == IROP_Call)
	{
		auto pProcsig = (SProcedureSignature*)pInst->m_wordRhs.m_pV;
		return pProcsig->m_cBArgReturn;
	}
	return pInst->m_cBRegister;
}

// number of bytes 'returned' by this instruction
static inline s32 CBResult(const SInstruction * pInst)
{
	switch (pInst->m_irop)
	{
	case IROP_NCmp:
	case IROP_GCmp:
		return 1;
	default:
		return CBOperand(pInst);
	}
}

struct SBoxedArg
{
	STypeInfo *		m_pTin;
	SWord			m_word;
};

struct SValueOutput // tag = valout
{
					SValueOutput()
					:m_cBRegister(0)
					,m_pTin(nullptr)
						{ ; }

	s32				m_cBRegister;
	STypeInfo *		m_pTin;
};



inline void SetOperandFromValue(
	SDataLayout * pDlay,
	SValue * pValSrc,
	OPK * pOpkOut,
	SWord * pWordOut,
	OPSZ opsz,
	SValueOutput * pValout);



const OpSignature * POpsig(IROP irop)
{
	#define OP(x) 
	#define OPMN(RANGE, x) 
	#define OPMX(RANGE, x) 
	#define OPSIZE(LHS, RHS, RET) {OPSZ_##LHS, OPSZ_##RHS, OPSZ_##RET, (OPSZ_##LHS == OPSZ_CB || OPSZ_##LHS == OPSZ_PCB) ? CBSRC_Lhs : ((OPSZ_##RHS == OPSZ_CB || OPSZ_##RHS == OPSZ_PCB) ? CBSRC_Rhs : CBSRC_Nil) },
	static const OpSignature s_mpIropOpsig [] =
	{
		OPCODE_LIST
	};
	#undef OPSIZE
	#undef OPMX
	#undef OPMN
	#undef OP

	EWC_CASSERT(EWC_DIM(s_mpIropOpsig) == IROP_Max, "missing OpSignature string");
	return &s_mpIropOpsig[irop];
}

template <typename T>
T * PValRtiCast(SValue * pVal)
{
	if (pVal && pVal->m_valk == EWC::SStripPointer<T>::Type::s_valk)
		return (T)pVal;
	return nullptr;
}

template <typename T>
T PValDerivedCast(SValue * pVal)
{
	EWC_ASSERT(pVal && pVal->m_valk == EWC::SStripPointer<T>::Type::s_valk, "illegal derived cast");
	return (T)pVal;
}

SProcedureSignature::SProcedureSignature(STypeInfoProcedure * pTinproc)
:m_sIBStackVariadic(-1)
,m_cBArgReturn(0)
,m_cBArgNamed(0)
,m_pTinproc(pTinproc)
,m_aParamArg(nullptr)
,m_aParamRet(nullptr)
{

}

SProcedure::SProcedure(EWC::CAlloc * pAlloc)
:SValue(VALK_Procedure)
,m_pProcsig(nullptr)
,m_cBStack(0)
,m_pBlockLocals(nullptr)
,m_pBlockFirst(nullptr)
,m_arypBlock(pAlloc, BK_ByteCodeCreator, 16)
,m_aryInst(pAlloc, BK_ByteCode, 0)
{
}

CBuilder::CBuilder(CWorkspace * pWork, SDataLayout * pDlay, EWC::CHash<HV, void*> * pHashHvPFnForeign)
:CBuilderBase(pWork)
,m_pSymtab(pWork->m_pSymtab)
,m_pAlloc(pWork->m_pAlloc)
,m_pBerrctx(nullptr)
,m_pDlay(pDlay)
,m_hashHvMangledPProc(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_arypBlockManaged(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_aryJumptStack(pWork->m_pAlloc, EWC::BK_ByteCodeCreator)
,m_arypValManaged(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_arypProcManaged(pWork->m_pAlloc, BK_ByteCodeCreator, 128)
,m_dataseg(pWork->m_pAlloc)
,m_hashPSymPVal(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_hashPTinlitPGlob(pWork->m_pAlloc, BK_ByteCodeCreator, 256)
,m_hashPTinstructPCgstruct(pWork->m_pAlloc, BK_ByteCodeCreator, 32)
,m_hashPTinprocPProcsig(pWork->m_pAlloc, BK_ByteCodeCreator, 32)
,m_phashHvPFnForeign(pHashHvPFnForeign)
,m_blistConst(pWork->m_pAlloc, BK_ByteCodeCreator)
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
{
}

CBuilder::~CBuilder()
{
	Clear();
}

void CBuilder::Clear()
{
	auto ppValMac = m_arypValManaged.PMac();
	for (auto ppVal = m_arypValManaged.A(); ppVal != ppValMac; ++ppVal)
	{
		m_pAlloc->EWC_DELETE(*ppVal);
	}
	m_arypValManaged.Clear();

	auto ppProcMac = m_arypProcManaged.PMac();
	for (auto ppProc = m_arypProcManaged.A(); ppProc != ppProcMac; ++ppProc)
	{
		m_pAlloc->EWC_DELETE(*ppProc);
	}
	m_arypProcManaged.Clear();

	auto ppBlockMac = m_arypBlockManaged.PMac();
	for (auto ppBlock = m_arypBlockManaged.A(); ppBlock != ppBlockMac; ++ppBlock)
	{
		m_pAlloc->EWC_DELETE(*ppBlock);
	}
	m_arypBlockManaged.Clear();

	{
		EWC::CHash<STypeInfoStruct *, SCodeGenStruct *>::CIterator iter(&m_hashPTinstructPCgstruct);
		while (SCodeGenStruct ** ppCgstruct = iter.Next())
		{
			m_pAlloc->EWC_DELETE(*ppCgstruct);
		}

		m_hashPTinstructPCgstruct.Clear(0);
	}

	{
		EWC::CHash<STypeInfoProcedure *, SProcedureSignature *>::CIterator iter(&m_hashPTinprocPProcsig);
		while (SProcedureSignature ** ppProcsig = iter.Next())
		{
			m_pAlloc->EWC_FREE(*ppProcsig);
		}

		m_hashPTinprocPProcsig.Clear(0);
	}
}

static inline s64 IBArgAlloc(s64 * pcBArg, s64 cB, s64 cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	s64 cBStack = U32Coerce((*pcBArg + cBMasked) & ~cBMasked);
	*pcBArg = cBStack + cB;
	return cBStack;
}

SProcedure * CBuilder::PProcCreateImplicit(CWorkspace * pWork, STypeInfoProcedure * pTinproc, CSTNode * pStnod)
{
	auto pProc = EWC_NEW(m_pAlloc, SProcedure) SProcedure(m_pAlloc);
	pProc->m_pProcsig = PProcsigEnsure(pTinproc);
	AddManagedVal(pProc);

	auto fins = m_hashHvMangledPProc.FinsEnsureKeyAndValue(pTinproc->m_strMangled.Hv(), pProc);
	EWC_ASSERT(fins == FINS_Inserted, "adding procedure that already exists");

	const char * pCozName = pTinproc->m_strName.PCoz();
	pProc->m_pBlockLocals = PBlockCreate(pProc, pCozName);
	pProc->m_pBlockFirst = PBlockCreate(pProc, pCozName);
	
	return pProc;
}

SValue * CBuilder::PValCreateProc(
	CWorkspace * pWork,
	STypeInfoProcedure * pTinproc,
	const EWC::CString & strMangled,
	CSTNode * pStnod,
	CSTNode * pStnodBody,
	EWC::CDynAry<LType *> * parypLtype,
	LType * pLtypeReturn)
{
	SValue * pValReturn = nullptr;
	if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
	{
		void ** ppFn = m_phashHvPFnForeign->Lookup(strMangled.Hv());
		if (!ppFn)
		{
			EmitError(pWork->m_pErrman, &pStnod->m_lexloc, ERRID_UndefinedForeignFunction, "undefined foreign function '%s'", pTinproc->m_strName.PCoz());
		}
		else
		{
			pValReturn = PConstPointer(*ppFn, pTinproc);
		}
	}
	else
	{
		pValReturn = PProcCreateImplicit(pWork, pTinproc, pStnod);
	}

	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
	if (!EWC_FVERIFY(pStproc, "expected stproc"))
		return nullptr;

	if (EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check"))
	{
		SetSymbolValue(pStnod->m_pSym, pValReturn);
	}

	return pValReturn;
}

void CBuilder::SetupParamBlock(
	CWorkspace * pWork,
	SProcedure * pProc,
	CSTNode * pStnod,
	CSTNode * pStnodParamList, 
	EWC::CDynAry<LType *> * parypLtype)
{
	// BB - Could we merge this with the CodeGen version?
	auto pStproc = PStmapDerivedCast<CSTProcedure *>(pStnod->m_pStmap);
	auto pProcsig = pProc->m_pProcsig;
	int cpStnodParam = pStnodParamList->CStnodChild();

	auto pTinproc = pProcsig->m_pTinproc;
	int iParam = 0;
	for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
	{
		CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
		if (pStnodParam->m_park == PARK_VariadicArg)
			continue;

		CSTDecl * pStdecl = PStmapRtiCast<CSTDecl *>(pStnodParam->m_pStmap);
		if (pStdecl && pStdecl->m_fIsBakedConstant)
			continue;

		EWC_ASSERT(iParam < pTinproc->m_arypTinParams.C(), "parameter count mismatch");
		auto strArgName = StrPunyEncode(pStnodParam->m_pSym->m_strName.PCoz());

		if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
		{
			if (!pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				auto pParam = &pProcsig->m_aParamArg[iParam];
				auto cB = pParam->m_cB;
				if (!FIsRegisterSize(cB))
				{
					auto pInstvalAddr = PInstCreateRaw(IROP_StoreAddress, m_pDlay->m_cBPointer, PRegArg(pParam->m_iBStack, 32), nullptr);
					pInstvalAddr->m_pTinOperand = m_pSymtab->PTinptrAllocate(pTinproc->m_arypTinParams[iParam]);
					SetSymbolValue(pStnodParam->m_pSym, pInstvalAddr);
				}
				else
				{
					auto pInstvalAddr = PInstCreateRaw(IROP_StoreAddress, m_pDlay->m_cBPointer, PRegArg(pParam->m_iBStack, cB*8), nullptr);
					pInstvalAddr->m_pTinOperand = m_pSymtab->PTinptrAllocate(pTinproc->m_arypTinParams[iParam]);
					SetSymbolValue(pStnodParam->m_pSym, pInstvalAddr);
				}
			}
		}
		++iParam;
	}
}

void CBuilder::ActivateProc(SProcedure * pProc, SBlock * pBlock)
{
	if (m_pBlockCur)
	{
		m_pBlockCur = nullptr;
	}

	if (m_pProcCur)
	{
		m_pProcCur = nullptr;
	}

	m_pProcCur = pProc;
	m_pBlockCur = pBlock;
}

inline void RemoveOpkArg(OPK * pOpk)
{
	EWC_CASSERT(OPK_Literal == OPK(OPK_LiteralArg - 1), "bad Arg opk value"); 
	EWC_CASSERT(OPK_Register == OPK(OPK_RegisterArg - 1), "bad Arg opk value"); 

	OPK opk = *pOpk;
	EWC_ASSERT(FIsArg(opk), "non-argument passed into RemoveOpkArg");
	*pOpk = OPK(opk - 1);
}

void CBuilder::FinalizeProc(SProcedure * pProc)
{
	s32 cInst = 0;
	for (auto ppBlock = pProc->m_arypBlock.A(); ppBlock != pProc->m_arypBlock.PMac(); ++ppBlock)
	{
		auto pBlock = *ppBlock;
		pBlock->m_iInstFinal = cInst;
		cInst += (s32)pBlock->m_aryInst.C();
	}

	pProc->m_cBStack = CBAlign(pProc->m_cBStack, m_pDlay->m_cBStackAlign);

	auto iBArgFFrame = (s32)pProc->m_cBStack;
	pProc->m_aryInst.EnsureSize(cInst);
	for (auto ppBlock = pProc->m_arypBlock.A(); ppBlock != pProc->m_arypBlock.PMac(); ++ppBlock)
	{
		auto pBlock = *ppBlock;

		// change all branch instructions from pBlock to proc relative iInst
		for (auto pBranch = pBlock->m_aryBranch.A(); pBranch != pBlock->m_aryBranch.PMac(); ++pBranch)
		{
			auto iInstFinal = pBranch->m_pBlockDest->m_iInstFinal;

			EWC_ASSERT(iInstFinal >= 0, "block was not finalized");
			*pBranch->m_pIInstDst = iInstFinal;
		}

		auto pInstMac = pBlock->m_aryInst.PMac(); 
		for (auto pInst = pBlock->m_aryInst.A(); pInst != pInstMac; ++pInst)
		{
			if (pInst->m_irop == IROP_Phi)
			{
				auto pInstIt = pInst;
				do 
				{
					auto pBlock = (SBlock *)pInstIt->m_wordRhs.m_pV;
					pInstIt->m_wordRhs.m_s32 = pBlock->m_iInstFinal;
					++pInstIt;
				} while (pInstIt->m_irop == IROP_ExArgs);
			}

			pProc->m_aryInst.Append(*pInst);
			auto pInstNew = pProc->m_aryInst.PLast();
			
			if (FIsArg(pInstNew->m_opkLhs))
			{
				pInstNew->m_wordLhs.m_s32 += iBArgFFrame;
				RemoveOpkArg(&pInstNew->m_opkLhs);
			}

			if (FIsArg(pInstNew->m_opkRhs))
			{
				pInstNew->m_wordRhs.m_s32 += iBArgFFrame;
				RemoveOpkArg(&pInstNew->m_opkRhs);
			}

			pBlock->m_aryInstval.Clear();
		}
	}
}



static const size_t s_cBDataBlockAlign = 8;

void CDataSegment::Clear()
{
	auto pDatabIt = m_pDatabFirst;
	while (pDatabIt)
	{
		auto pDatab = pDatabIt;
		pDatabIt = pDatabIt->m_pDatabNext;

		m_pAlloc->EWC_FREE(pDatab);
	}
		
	m_pDatabFirst = nullptr;
	m_pDatabCur = nullptr;

	m_aryIBPointer.Clear();
}

u8 * CDataSegment::PBFromIndex(s32 iB)
{	
	if (iB == 0)	
		return nullptr;
	
	auto pDatab = m_pDatabFirst;
	while (1)
	{
		auto dB = iB - pDatab->m_iBStart;
		if (dB < pDatab->m_cB)
		{
			return &pDatab->m_pB[dB];

		}
		if (!pDatab->m_pDatabNext)
		{
			EWC_ASSERT(iB < pDatab->m_iBStart + pDatab->m_cB, "bad index in PBFromIndex()");
			return nullptr;
		}
		pDatab = pDatab->m_pDatabNext;
	}
}

u8 * CDataSegment::PBFromGlobal(SConstant * pConstGlob)
{
	EWC_ASSERT(pConstGlob->m_opk == OPK_Global, "expected global value");

	// iBPointer is the index that stores a pointer in the data segment (ie. a global value lhs reference)
	//  if we haven't adjusted pointers yet it's just an index, otherwise it's the address

	auto pBPointer = PBFromIndex(pConstGlob->m_word.m_s32);
	s32 iBValue = *(s32*)pBPointer;
	return PBFromIndex(iBValue);
}

size_t CDataSegment::CB()
{
	if (!m_pDatabCur)
		return 0;
	return m_pDatabCur->m_iBStart + m_pDatabCur->m_cB;
}

u8 * CDataSegment::PBBakeCopy(EWC::CAlloc * pAlloc, SDataLayout * pDlay)
{
	if (!m_pDatabFirst)
		return nullptr;

	size_t cB = CB();
	u8 * pB = (u8*)m_pAlloc->EWC_ALLOC(cB, s_cBDataBlockAlign);

	for (auto pDatabIt = m_pDatabFirst; pDatabIt; pDatabIt = pDatabIt->m_pDatabNext)
	{
		EWC_ASSERT(pDatabIt->m_iBStart + pDatabIt->m_cB <= cB, "bad size calculation");
		memcpy(&pB[pDatabIt->m_iBStart], pDatabIt->m_pB, pDatabIt->m_cB);
	}

	EWC_ASSERT(pDlay->m_cBPointer == sizeof(u8*), "not handling cBPointer not matching sizeof(u8*) properly here");

	auto piBMax = m_aryIBPointer.PMac();
	for (auto piB = m_aryIBPointer.A(); piB != piBMax; ++piB)
	{
		s32 iB = *(s32*)piB;
		u8 * pBSrc = &pB[iB];
		s32 iBTarget = *(s32*)pBSrc;

#ifdef EWC_TRACE_GLOBAL
		printf("g[%d] -> g[%d], 0x%p\n", iB, iBTarget, &pB[iBTarget]);
#endif
		*(void**)pBSrc = &pB[iBTarget];
	}

	return pB;
}
void CDataSegment::AllocateDataBlock(size_t cBMin)
{
	size_t cBDatabStride = CBAlign(sizeof(SDataBlock), s_cBDataBlockAlign);
	size_t cBBlock = ewcMax(cBMin, m_cBBlockMin);
	auto pB = (u8*)m_pAlloc->EWC_ALLOC(cBDatabStride + cBBlock, 16);

	SDataBlock * pDatabNew = (SDataBlock *)pB;
	pDatabNew->m_cB = 0;
	pDatabNew->m_cBMax = cBBlock;
	pDatabNew->m_pB = pB + cBDatabStride;
	pDatabNew->m_pDatabNext = nullptr;

	if (m_pDatabFirst == nullptr)
	{
		static const size_t s_iBStart = 16; // skip the first qword to allow us to use ib == 0 as a nullptr
		m_pDatabFirst = pDatabNew;
		pDatabNew->m_iBStart = s_iBStart;
	}
	else
	{
		pDatabNew->m_iBStart = m_pDatabCur->m_iBStart + CBAlign(m_pDatabCur->m_cB, s_cBDataBlockAlign);
		EWC_ASSERT((uintptr_t(pDatabNew->m_iBStart) & (s_cBDataBlockAlign - 1)) == 0, "bad alignment calculations");
		m_pDatabCur->m_pDatabNext = pDatabNew;
	}

	m_pDatabCur = pDatabNew;
}

void CDataSegment::AllocateData(size_t cB, size_t cBAlign, u8 ** ppB, s64 * piB, const char * pChzLabel)
{
	size_t cBAlignment = 0;
	size_t cBStride = ewcMax(cB, cBAlign);
	auto cBPrev = (m_pDatabCur) ? m_pDatabCur->m_cB : 0;
	bool fAdded = false;
	if (m_pDatabCur == nullptr)
	{
		EWC_ASSERT(m_pDatabFirst == nullptr, "bad data block init");
		AllocateDataBlock(cBStride);
	}
	else
	{
		auto pBCur = m_pDatabCur->m_pB + m_pDatabCur->m_cB;
		auto pBAlign = (u8*)PVAlign(pBCur, cBAlign);
		cBAlignment = (pBAlign - pBCur);

		if (uintptr_t(pBAlign + cB + cBAlignment) > uintptr_t(m_pDatabCur->m_pB + m_pDatabCur->m_cBMax))
		{
			AllocateDataBlock(cBStride);
		}
		else
		{
			fAdded = true;
			m_pDatabCur->m_cB += cBAlignment;
		}
	}

	*piB = m_pDatabCur->m_iBStart + m_pDatabCur->m_cB;
	auto pBStart = &m_pDatabCur->m_pB[m_pDatabCur->m_cB];
	EWC_ASSERT((uintptr_t(pBStart) & (cBAlign - 1)) == 0, "bad alignment calculations");
	*ppB = pBStart;

	auto pVEnd = pBStart + cB;
	m_pDatabCur->m_cB += pVEnd - pBStart;
	EWC_ASSERT(m_pDatabCur->m_cB <= m_pDatabCur->m_cBMax, "data block overflow")

#ifdef EWC_TRACE_GLOBAL
	printf("%s: g[%lld] %zd bytes\n", pChzLabel, *piB, cB);
#endif
}

void CDataSegment::AddRelocatedPointer(SDataLayout * pDlay, s32 iBPointer, s32 iBSrc)
{
	u8 * pBSrc = PBFromIndex(iBSrc);
	if (*(s32*)pBSrc == 0)
		return;

	u8 * pBPointer = PBFromIndex(iBPointer);
	switch (pDlay->m_cBPointer)
	{
	case 4: *(s32*)pBPointer = iBSrc;  break;
	case 8: *(s64*)pBPointer = iBSrc;  break;
	default:
		EWC_ASSERT(false, "unexpected pointer size");
	}

	m_aryIBPointer.Append(iBPointer);
}

void CDataSegment::AddDeepCopyPointers(SDataLayout * pDlay, STypeInfo * pTin, s32 iBDst, s32 iBSrc)
{
	while (pTin)
	{
		switch (pTin->m_tink)
		{
		case TINK_Pointer:
			{
				auto pBSrc = PBFromIndex(iBSrc);
				AddRelocatedPointer(pDlay, iBDst, iBSrc);

				s32 iBSrcTarget = *(s32*)pBSrc;
				pTin = (iBSrcTarget == 0) ? nullptr : ((STypeInfoPointer *)pTin)->m_pTinPointedTo;
			} break;
		case TINK_Qualifier:
			{
				pTin = ((STypeInfoQualifier *)pTin)->m_pTin;
			} break;
		case TINK_Array:
			{
				auto pTinary = (STypeInfoArray *)pTin;
				s64 cElement = 0;
				switch (pTinary->m_aryk)
				{
				case ARYK_Reference:
					{
						//pTin = m_pSymtab->PTinBuiltin(CSymbolTable::s_strS64);
						cElement = *(s64*)PBFromIndex(iBSrc);
						iBSrc += sizeof(s64);
						iBDst += sizeof(s64);

						auto pBSrc = PBFromIndex(iBSrc);
						AddRelocatedPointer(pDlay, iBDst, *(s32*)pBSrc);

					} break;
				case ARYK_Fixed:
					{
						cElement = pTinary->m_c;

						u64 cB;
						u64 cBAlign;
						CalculateByteSizeAndAlign(pDlay, pTinary->m_pTin, &cB, &cBAlign);
						s32 cBStride = S32Coerce(EWC::CBAlign(cB, cBAlign));

						for (int iElement = 0; iElement < cElement; ++iElement)
						{
							AddDeepCopyPointers(pDlay, pTinary->m_pTin, iBDst, iBSrc);
							iBSrc += cBStride;
							iBDst += cBStride;
						}
					} break;
				default:
					EWC_ASSERT(false, "unhandled array kind");
				}

				pTin = nullptr;
			} break;
		case TINK_Struct:
			{
				auto pTinstruct = (STypeInfoStruct *)pTin;
				auto pTypemembMac = pTinstruct->m_aryTypemembField.PMac();
				for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMac; ++pTypememb)
				{
					AddDeepCopyPointers(pDlay, pTypememb->m_pTin, iBDst + pTypememb->m_dBOffset, iBSrc + pTypememb->m_dBOffset);
				}

				pTin = nullptr;
			} break;
		case TINK_Literal:
			{
				auto pTinlit = (STypeInfoLiteral*)pTin;
				switch (pTinlit->m_litty.m_litk)
				{
				case LITK_String:
					{
						AddRelocatedPointer(pDlay, iBDst, iBSrc);
					} break;
				case LITK_Array:
					{
						u64 cB;
						u64 cBAlign;
						CalculateByteSizeAndAlign(pDlay, pTinlit->m_pTinSource, &cB, &cBAlign);
						s32 cBStride = S32Coerce(EWC::CBAlign(cB, cBAlign));

						for (int iElement = 0; iElement < pTinlit->m_c; ++iElement)
						{
							AddDeepCopyPointers(pDlay, pTinlit->m_pTinSource, iBDst, iBSrc);
							iBSrc += cBStride;
							iBDst += cBStride;
						}

					} break;
				}
				
				pTin = nullptr;
			} break;
		default:
			pTin = nullptr;
			break;
		}
	}
}

void CDataSegment::DeepCopy(SDataLayout * pDlay, STypeInfo * pTin, s32 iBDst, s32 iBSrc)
{
	// Add relocated pointer entries
	AddDeepCopyPointers(pDlay, pTin, iBDst, iBSrc);

	u64 cB;
	u64 cBAlign;
	CalculateByteSizeAndAlign(pDlay, pTin, &cB, &cBAlign);

	auto pBSrc = PBFromIndex(iBSrc);
	auto pBDst = PBFromIndex(iBDst);
	memcpy(pBDst, pBSrc, cB);
}

static void PrintFloatOperand(SStringBuffer * pStrbuf, int cBOperand, OPK opk, SWord word)
{
	switch(opk)
	{
	case OPK_Literal:
	case OPK_LiteralArg:
		{
			switch (cBOperand)
			{
				case 4: FormatCoz(pStrbuf, "%f", word.m_f32);	break;
				case 8: FormatCoz(pStrbuf, "%f", word.m_f64);	break;
			}
		} break;
	case OPK_Register:
	case OPK_RegisterArg:
		{
			switch (cBOperand)
			{
				case 1: FormatCoz(pStrbuf, "[%d]", word.m_s8);		break;
				case 2: FormatCoz(pStrbuf, "[%d]", word.m_s16);		break;
				case 4: FormatCoz(pStrbuf, "[%d]", word.m_s32);		break;
				case 8: FormatCoz(pStrbuf, "[%lld]", word.m_s64);	break;
			}
		} break;
	default:
		FormatCoz(pStrbuf, "ERR(0x%x) 0x%x", opk, word.m_u64);
	}
}

static void PrintIntOperand(SStringBuffer * pStrbuf, int cBOperand, OPK opk, SWord word, bool fIsSigned)
{
	switch(opk)
	{
	case OPK_Literal:
	case OPK_LiteralArg:
		{
			if (fIsSigned)
			{
				switch (cBOperand)
				{
					case 1: FormatCoz(pStrbuf, "%d", word.m_s8);	break;
					case 2: FormatCoz(pStrbuf, "%d", word.m_s16);	break;
					case 4: FormatCoz(pStrbuf, "%d", word.m_s32);	break;
					case 8: FormatCoz(pStrbuf, "%lld", word.m_s64);	break;
					default: EWC_ASSERT(false, "unexpected operand size");
				}
			}
			else
			{
				switch (cBOperand)
				{
					case 1: FormatCoz(pStrbuf, "%u", word.m_u8);	break;
					case 2: FormatCoz(pStrbuf, "%u", word.m_u16);	break;
					case 4: FormatCoz(pStrbuf, "%u", word.m_u32);	break;
					case 8: FormatCoz(pStrbuf, "%llu", word.m_u64);	break;
					default: EWC_ASSERT(false, "unexpected operand size");
				}
			}
		} break;
	case OPK_Register:
		{
			FormatCoz(pStrbuf, "[%d]", word.m_s32);
		} break;
	case OPK_RegisterArg:
		{
			FormatCoz(pStrbuf, "a[%d]", word.m_s32);
		} break;
	case OPK_Global:
		{
			FormatCoz(pStrbuf, "g[%d]", word.m_s32);
		} break;
	default:
		FormatCoz(pStrbuf, "ERR(0x%x) 0x%x", opk, word.m_u64);
	}
}

int CBOperandFromOpsz(OPSZ opsz, int cBOperand, SDataLayout * pDlay)
{
	switch(opsz)
	{
	case OPSZ_0:	return 0;
	case OPSZ_1:	return 1;
	case OPSZ_2:	return 2;
	case OPSZ_4:	return 4;
	case OPSZ_8:	return 8;
	case OPSZ_CB:	return cBOperand;
	case OPSZ_PCB:	return 4;
	case OPSZ_Ptr:	return pDlay->m_cBPointer;
	case OPSZ_RegIdx: return 4;
	default:
		EWC_ASSERT(false, "unhandled OPSZ");
		return 0;
	}
}

void CBuilder::PrintDump()
{
	const int s_operandPos = 22;
	char aCh[128];

	SProcedure ** ppProc;
	EWC::CHash<HV, SProcedure *>::CIterator iter(&m_hashHvMangledPProc);
	while (ppProc = iter.Next())
	{
		auto pProc = *ppProc;
		auto pProcsig = pProc->m_pProcsig;
		printf("%s()\t\t\t\t\tcBStack=%lld, cBGlobal=%lld, cBArg=%lld\n",
			pProcsig->m_pTinproc->m_strName.PCoz(),
			pProc->m_cBStack,
			m_dataseg.CB(),
			pProcsig->m_cBArgNamed);
		auto pInstMac = pProc->m_aryInst.PMac();

		auto aInst = pProc->m_aryInst.A();
		for (auto pInst = aInst; pInst != pInstMac; ++pInst)
		{
			auto pOpsig = POpsig(pInst->m_irop);

			SStringBuffer strbuf(aCh, EWC_DIM(aCh));
			ptrdiff_t iInst = pInst - aInst;
			FormatCoz(&strbuf, "i%d ", iInst);
			AppendToCch(&strbuf, ' ', 6);

			FormatCoz(&strbuf, "%s", PChzFromIrop(pInst->m_irop));
			auto cBOperand = CBOperand(pInst);
			if (cBOperand != 0)
			{
				FormatCoz(&strbuf, "_%d", cBOperand);
			}
			AppendCoz(&strbuf, " ");

			auto cBLhs = CBOperandFromOpsz(pOpsig->m_opszLhs, cBOperand, m_pDlay);
			auto cBRhs = CBOperandFromOpsz(pOpsig->m_opszRhs, cBOperand, m_pDlay);
			switch (pInst->m_irop)
			{
			case IROP_Call:
				{
					if (FIsRegister(pInst->m_opkLhs) == 0) // if not call by reference
					{
						auto pProcsig = (SProcedureSignature *)pInst->m_wordRhs.m_pV;
						FormatCoz(&strbuf, "%s()->[%d]", pProcsig->m_pTinproc->m_strName.PCoz(), pInst->m_iBStackOut);

						if ((pInst + 1)->m_irop == IROP_ExArgs)
						{
							++pInst;
							FormatCoz(&strbuf, "     %d variadic args, cBArgVariadic(%d)", pInst->m_wordLhs.m_s32, pInst->m_wordRhs.m_s32);
						}
					}
				} break;
			case IROP_Branch:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					FormatCoz(&strbuf, "i%d", pInst->m_wordRhs.m_s32);
				} break;
			case IROP_NTrunc:
			case IROP_SignExt:
			case IROP_ZeroExt:
			case IROP_GToS:
			case IROP_GToU:
			case IROP_SToG:
			case IROP_UToG:
			case IROP_GTrunc:
			case IROP_GExtend:
			case IROP_PtrToInt:
			case IROP_IntToPtr:
			case IROP_Bitcast:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					auto cBLhs = pInst->m_wordRhs.m_s32;
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
					FormatCoz(&strbuf, " %d bytes ->[%d] %d bytes", cBLhs, pInst->m_iBStackOut, cBOperand);
				} break;
			case IROP_CondBranch:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					s32 * pIInst = (s32*)&pInst->m_wordRhs;
					FormatCoz(&strbuf, " ? i%d : i%d", pIInst[1], pIInst[0]);
				} break;
			case IROP_GCmp:
				{
					FormatCoz(&strbuf, "(%s) ", PChzFromGpred((GPRED)pInst->m_pred));

					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintFloatOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs);
					AppendCoz(&strbuf, ", ");
					PrintFloatOperand(&strbuf, cBRhs, pInst->m_opkRhs, pInst->m_wordRhs);

					FormatCoz(&strbuf, " ->[%d]", pInst->m_iBStackOut);
				} break;
			case IROP_NCmp:
				{
					FormatCoz(&strbuf, "(%s) ", PChzFromNpred((NPRED)pInst->m_pred));

					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
					AppendCoz(&strbuf, ", ");
					PrintIntOperand(&strbuf, cBRhs, pInst->m_opkRhs, pInst->m_wordRhs, true);

					FormatCoz(&strbuf, " ->[%d]", pInst->m_iBStackOut);
				} break;
			case IROP_Memset:
			case IROP_Memcpy:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					if (cBLhs)
					{
						PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
					}

					if (cBRhs)
					{
						AppendCoz(&strbuf, ", ");
						PrintIntOperand(&strbuf, cBRhs, pInst->m_opkRhs, pInst->m_wordRhs, true);
					}
					++pInst; 
					if (!EWC_FVERIFY(pInst->m_irop == IROP_ExArgs, "expected exArgs"))
						break;

					AppendCoz(&strbuf, ", ");
					PrintIntOperand(&strbuf, 8, pInst->m_opkLhs, pInst->m_wordLhs, true);
					AppendCoz(&strbuf, " bytes");
				} break;
			case IROP_TraceStore:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);

					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					auto pTin = (STypeInfo*)pInst->m_wordRhs.m_pV;
					auto strTin = StrFromTypeInfo(pTin);
					FormatCoz(&strbuf, " : %s", strTin.PCoz());
				} break;
			case IROP_Alloca:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					AppendCoz(&strbuf, "outIdx = ");
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					auto pTin = (STypeInfo*)pInst->m_wordRhs.m_pV;
					auto strTin = StrFromTypeInfo(pTin);
					FormatCoz(&strbuf, " : %s", strTin.PCoz());

					FormatCoz(&strbuf, " ->[%d]", pInst->m_iBStackOut);
				} break;
			case IROP_Phi:
				{
					FormatCoz(&strbuf, " ->[%d]", pInst->m_iBStackOut);

					do
					{
						FormatCoz(&strbuf, "\n        Location i%d = ", pInst->m_wordRhs.m_s32);
						PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
						++pInst;

					} while(pInst->m_irop == IROP_ExArgs);

				} break;
			case IROP_Switch:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
					auto pInstSw = pInst;

					while ((pInst + 1)->m_irop == IROP_ExArgs)
					{
						++pInst;
						AppendCoz(&strbuf, "\n");
						auto cChBase = CCh(strbuf.m_pCozBegin);

						AppendToCch(&strbuf, ' ', 6 + cChBase);
						AppendCoz(&strbuf, "  case:");

						AppendToCch(&strbuf, ' ', s_operandPos + cChBase);
						PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
						FormatCoz(&strbuf, " i%d", pInst->m_wordRhs.m_s32);
					}

					AppendCoz(&strbuf, "\n");
					auto cChBase = CCh(strbuf.m_pCozBegin);

					AppendToCch(&strbuf, ' ', 6 + cChBase);
					AppendCoz(&strbuf, "  else:");

					AppendToCch(&strbuf, ' ', s_operandPos + cChBase);
					FormatCoz(&strbuf, " i%d", pInstSw->m_wordRhs.m_s32);

				} break;
			case IROP_GEP:
				{
					auto pInstGep = pInst;
					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					while ((pInst + 1)->m_irop == IROP_ExArgs)
					{
						++pInst;
						AppendCoz(&strbuf, " + ");
						PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
						AppendCoz(&strbuf, "*");
						PrintIntOperand(&strbuf, cBLhs, pInst->m_opkRhs, pInst->m_wordRhs, true);
					}

					AppendCoz(&strbuf, " + ");
					PrintIntOperand(&strbuf, cBRhs, pInstGep->m_opkRhs, pInstGep->m_wordRhs, true);
					
					FormatCoz(&strbuf, " ->[%d]", pInstGep->m_iBStackOut);
				} break;
			case IROP_Store:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					FormatCoz(&strbuf, " ->*[%d]", pInst->m_iBStackOut);
				} break;
			case IROP_StoreToReg:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					FormatCoz(&strbuf, " ->[%d]", pInst->m_iBStackOut);
				} break;
			case IROP_StoreToIdx:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);

					FormatCoz(&strbuf, " ->[[%d]]", pInst->m_iBStackOut);
				} break;
			default:
				{
					AppendToCch(&strbuf, ' ', s_operandPos);
					if (cBLhs)
					{
						PrintIntOperand(&strbuf, cBLhs, pInst->m_opkLhs, pInst->m_wordLhs, true);
					}

					if (cBRhs)
					{
						AppendCoz(&strbuf, ", ");
						PrintIntOperand(&strbuf, cBRhs, pInst->m_opkRhs, pInst->m_wordRhs, true);
					}

					if (pOpsig->m_opszRet != OPSZ_0 || FUseLargeOperand(pInst->m_irop))
					{
						FormatCoz(&strbuf, " ->[%d]", pInst->m_iBStackOut);
					}
				} break;
			}
			AppendCoz(&strbuf, "\n");
			printf(aCh);
		}
		printf("\n");
	}
}

void CBuilder::FinalizeBuild(CWorkspace * pWork)
{
}

CBuilder::LType * CBuilder::PLtypeVoid()
{
	return m_pSymtab->PTinBuiltin(CSymbolTable::s_strVoid);
}

SBlock * CBuilder::PBlockCreate(SProcedure * pProc, const char * pChzName)
{
	auto pBlock = EWC_NEW(m_pAlloc, SBlock) SBlock();
	pBlock->m_aryInstval.SetAlloc(m_pAlloc, BK_ByteCode, 128);
	pBlock->m_aryInst.SetAlloc(m_pAlloc, BK_ByteCode, 128);
	pBlock->m_aryBranch.SetAlloc(m_pAlloc, BK_ByteCode, 4);
	pBlock->m_pProc = pProc;
	pProc->m_arypBlock.Append(pBlock);
	m_arypBlockManaged.Append(pBlock);

	return pBlock;
}

void CBuilder::ActivateBlock(SBlock * pBlock)
{
	if (m_pBlockCur)
	{
		m_pBlockCur = nullptr;
	}

	EWC_ASSERT(m_pProcCur, "cannot begin basic block without active procedure");
	EWC_ASSERT(m_pProcCur == pBlock->m_pProc, "block activated inside wrong procedure");
	EWC_ASSERT(!m_pBlockCur, "cannot begin basic block; previous block was not ended");
	m_pBlockCur = pBlock;

	if (!m_pProcCur->m_pBlockFirst)
	{
		m_pProcCur->m_pBlockFirst = pBlock;
	}
}

CBuilder::Instruction * CBuilder::PInstCreateNCmp(NPRED npred, SValue * pValLhs, SValue * pValRhs, const char * pChz)
{
	auto pInstval = PInstCreateRaw(IROP_NCmp, pValLhs, pValRhs);
	EWC_ASSERT(npred >= NPRED_Min && npred < NPRED_Max, "invalid predicate");
	pInstval->m_pInst->m_pred = (u8)npred;
	return pInstval;
}

CBuilder::Instruction * CBuilder::PInstCreateGCmp(GPRED gpred, SValue * pValLhs, SValue * pValRhs, const char * pChz)
{
	auto pInstval = PInstCreateRaw(IROP_GCmp, pValLhs, pValRhs);
	EWC_ASSERT(gpred >= GPRED_Min && gpred < GPRED_Max, "invalid predicate");
	pInstval->m_pInst->m_pred = (u8)gpred;
	return pInstval;
}

u64 DBOffsetFromField(STypeInfoStruct * pTinstruct, u64 idx)
{
	if (idx < pTinstruct->m_aryTypemembField.C())
	{
		auto dBOffset = pTinstruct->m_aryTypemembField[idx].m_dBOffset;
		if (EWC_FVERIFY(dBOffset >= 0, "bad struct offset"))
			return dBOffset;
		return 0;
	}

	EWC_ASSERT(false, "invalid field index %d", idx);
	return 0;
}

static inline void AllocateStackOut(CBuilder * pBuild, SInstruction * pInst, s64 cB, s64 cBAlign)
{
	EWC_ASSERT(pInst->m_iBStackOut == 0, "redundant stack allocation, leaking stack space");

	s32 iBStackPointer = S32Coerce(pBuild->IBStackAlloc(cB, cBAlign));
	pInst->m_iBStackOut = iBStackPointer;
}

CBuilder::Instruction * CBuilder::PInstCreateGEP(SValue * pValLhs, SValue ** apLvalIndices, u32 cpValIndices, const char * pChzName)
{
	// GEP(ptr,	offs)					(u8*)ptr + (val*dBStride1) + ... + offs
	// ExArgs(val1, dBStride1)

	SValueOutput valout;

	auto pInstvalGep = PInstAlloc();
	auto pInstGep = pInstvalGep->m_pInst;
	pInstGep->m_irop =  IROP_GEP;	
	SetOperandFromValue(m_pDlay, pValLhs, &pInstGep->m_opkLhs, &pInstGep->m_wordLhs, OPSZ_Ptr, &valout);

	auto pInstval = pInstvalGep;
	STypeInfo * pTin = valout.m_pTin;

	u64 cB;
	u64 cBAlign;
	u64 dBOffset = 0;
	for (u32 ipValIndices = 0; ipValIndices < cpValIndices; ++ipValIndices)
	{
		STypeInfoStruct * pTinstruct = nullptr;
		STypeInfoArray * pTinaryRef = nullptr;
		u32 cBStride = 1;
		auto pValIndex = apLvalIndices[ipValIndices]; 	
		switch (pTin->m_tink)
		{
		case TINK_Struct:
			{
				pTinstruct = (STypeInfoStruct *)pTin;
			} break;
		case TINK_Pointer:
			{
				auto pTinptr = (STypeInfoPointer*)pTin;

				CalculateByteSizeAndAlign(m_pDlay, pTinptr->m_pTinPointedTo, &cB, &cBAlign);
				cBStride = U32Coerce(EWC::CBAlign(cB, cBAlign));
				pTin = pTinptr->m_pTinPointedTo;
			} break;
		case TINK_Array:
			{
				auto pTinary = (STypeInfoArray*)pTin;

				switch (pTinary->m_aryk)
				{
				case ARYK_Fixed:
					{
						pTin = pTinary->m_pTin;
						CalculateByteSizeAndAlign(m_pDlay, pTinary->m_pTin, &cB, &cBAlign);
						cBStride = U32Coerce(EWC::CBAlign(cB, cBAlign));
					} break;
				case ARYK_Reference:
					{
						pTinaryRef = pTinary;
					} break;
				default:
					EWC_ASSERT(false, "unhandled ARYK in BCode::PInstCreateGEP");
					break;
				}

			} break;
		case TINK_Literal:
			{
				auto pTinlit = (STypeInfoLiteral *)pTin;

				if (EWC_FVERIFY(pTinlit->m_litty.m_litk == LITK_Array, "non-array literal types should be finalized away by here"))
				{
					CalculateByteSizeAndAlign(m_pDlay, pTinlit->m_pTinSource, &cB, &cBAlign);
					cBStride = U32Coerce(EWC::CBAlign(cB, cBAlign));
					pTin = pTinlit->m_pTinSource;
				}
			} break;
		default:
			EWC_ASSERT(false, "unexpected type info kind TINK_%s", PChzFromTink(pTin->m_tink));
		}

		switch (pValIndex->m_valk)
		{
		case VALK_Constant:
			{
				auto pConst = PValDerivedCast<SConstant *>(pValIndex);
				if (!EWC_FVERIFY(pConst && pConst->m_litty.m_litk == LITK_Integer, "struct indices must be const int"))	
					break;
			
				if (pTinstruct)
				{
					EWC_ASSERT(pConst->m_litty.m_cBit == 32 && pConst->m_litty.m_fIsSigned == false, "unexpected int type");
					if (EWC_FVERIFY(pConst->m_word.m_u64 < pTinstruct->m_aryTypemembField.C()))
					{
						auto pTypememb = &pTinstruct->m_aryTypemembField[pConst->m_word.m_u64];
						dBOffset += pTypememb->m_dBOffset;
						pTin = pTypememb->m_pTin;
					}
				}
				else if (pTinaryRef)
				{
					EWC_ASSERT(pConst->m_litty.m_cBit == 32 && pConst->m_litty.m_fIsSigned == false, "unexpected int type");
					if (EWC_FVERIFY(pConst->m_word.m_u64 < ARYMEMB_Max))
					{
						switch (ARYMEMB(pConst->m_word.m_u64))
						{
						case ARYMEMB_Count:
							{
								//dBOffset += 0;
								pTin = m_pSymtab->PTinBuiltin(CSymbolTable::s_strS64);
							} break;
						case ARYMEMB_Data:
							{
								dBOffset += sizeof(s64);

								// NOTE: ptr to pTin, not pTin... need one more GEP index for array element
								pTin = m_pSymtab->PTinptrAllocate(pTinaryRef->m_pTin);
							} break;
						}
					}
				}
				else
				{
					#define MASHLIT(CBIT, SIGNED) (SIGNED << 16) | (CBIT)
					switch (MASHLIT(pConst->m_litty.m_cBit, pConst->m_litty.m_fIsSigned))
					{
						case MASHLIT(8,  false):	dBOffset += cBStride * pConst->m_word.m_u8; break;
						case MASHLIT(16, false):	dBOffset += cBStride * pConst->m_word.m_u16; break;
						case MASHLIT(32, false):	dBOffset += cBStride * pConst->m_word.m_u32; break;
						case MASHLIT(64, false):	dBOffset += cBStride * pConst->m_word.m_u64; break;
						case MASHLIT(8,  true):		dBOffset += cBStride * pConst->m_word.m_s8; break;
						case MASHLIT(16, true):		dBOffset += cBStride * pConst->m_word.m_s16; break;
						case MASHLIT(32, true):		dBOffset += cBStride * pConst->m_word.m_s32; break;
						case MASHLIT(64, true):		dBOffset += cBStride * pConst->m_word.m_s64; break;
					}
					#undef MASHLIT
				}
			} break;
		case VALK_BCodeRegister:
		case VALK_Instruction:
			{
				auto pInstval = PInstAlloc();
				auto pInst = pInstval->m_pInst;

				pInst->m_irop =  IROP_ExArgs;	
				pInst->m_opkRhs = OPK_Literal;
				pInst->m_wordRhs.m_u64 = cBStride;

				SetOperandFromValue(m_pDlay, pValIndex, &pInst->m_opkLhs, &pInst->m_wordLhs, OPSZ_8, &valout);
				pInst->m_cBRegister = valout.m_cBRegister;
			} break;
		default:
			EWC_ASSERT(false, "unexpected value kind in GEP instruction");
		}
	}

	pInstvalGep->m_pTinOperand = m_pSymtab->PTinptrAllocate(pTin);
	pInstGep->m_opkRhs = OPK_Literal;
	pInstGep->m_wordRhs.m_u64 = dBOffset;
	pInstGep->m_cBRegister = sizeof(u8 *);

	AllocateStackOut(this, pInstGep, sizeof(u8 *), EWC_ALIGN_OF(u8 *));
	return pInstvalGep;
}

SValue * CBuilder::PValCreateAlloca(STypeInfo * pTin, const char * pChzName)
{
	// Alloca returns a pointer to stack mem (but the actual address isn't known until runtime)
	//  allocate both the requested space and room for the pointer

	EWC_ASSERT(m_pProcCur, "no active procedure");

	auto pInstval = PInstCreateRaw(IROP_Alloca, m_pDlay->m_cBPointer, nullptr, PConstPointer(pTin), pChzName);
	if (pInstval->FIsError())
		return pInstval;

	u64 cB; 
	u64 cBAlign;
	CalculateByteSizeAndAlign(m_pDlay, pTin, &cB, &cBAlign);
	auto pInst = pInstval->m_pInst;

	pInstval->m_pTinOperand = m_pSymtab->PTinptrAllocate(pTin);
	pInst->m_wordLhs.m_s32 = S32Coerce(IBStackAlloc(cB, cBAlign));

	return pInstval;
}

CBuilder::Instruction * CBuilder::PInstCreateMemset(SValue * pValLhs, s64 cBSize, s32 cBAlign, u8 bFill)
{
	auto pConstFill = PConstInt(bFill, 8);  
	auto pInstval = PInstCreateRaw(IROP_Memset, pValLhs, pConstFill);

	auto pInstvalEx = PInstAlloc();
	auto pInstEx = pInstvalEx->m_pInst;
	pInstEx->m_irop =  IROP_ExArgs;	
	pInstEx->m_opkLhs = OPK_Literal;
	pInstEx->m_wordLhs.m_s64 = cBSize;
	return pInstval;
}

CBuilder::Instruction * CBuilder::PInstCreateMemcpy(u64 cB, SValue * pValLhs, SValue * pValRhsRef)
{
	auto pInstval = PInstCreateRaw(IROP_Memcpy, pValLhs, pValRhsRef);

	auto pInstvalEx = PInstAlloc();
	auto pInstEx = pInstvalEx->m_pInst;
	pInstEx->m_irop =  IROP_ExArgs;	
	pInstEx->m_opkLhs = OPK_Literal;
	pInstEx->m_wordLhs.m_s64 = cB;
	return pInstval;
}

CBuilder::Instruction * CBuilder::PInstCreateMemcpy(STypeInfo * pTin, SValue * pValLhs, SValue * pValRhsRef)
{
	u64 cB;
	u64 cBAlign;
	CalculateByteSizeAndAlign(m_pDlay, pTin, &cB, &cBAlign);
	return PInstCreateMemcpy(cB, pValLhs, pValRhsRef);
}

CBuilder::GepIndex * CBuilder::PGepIndex(u64 idx)
{
	auto pConst = PConstInt(idx, 32, false);
	return pConst;
}

CBuilder::GepIndex * CBuilder::PGepIndexFromValue(SValue * pVal)
{
	return pVal;
}

CBuilder::Instruction * CBuilder::PInstCreatePhi(LType * pLtype, const char * pChzName)
{
	auto pInstvalPhi = PInstAlloc();
	EWC_ASSERT(m_pBlockCur->m_aryInstval.C() == 1, "Phi node must be the first node in a basic block");

	auto pInstPhi = pInstvalPhi->m_pInst;
	pInstPhi->m_irop = IROP_Phi;
	pInstPhi->m_cBRegister = 0; // this will be filled in by add phi incoming

	return pInstvalPhi;
}

void CBuilder::AddPhiIncoming(SValue * pInstPhi, SValue * pVal, SBlock * pBlock)
{
	SInstructionValue * pInstvalInc = m_pBlockCur->m_aryInstval.PLast();
	auto pInstInc = pInstvalInc->m_pInst;
	bool fIsPhiNode = pInstInc->m_irop == IROP_Phi && pInstInc->m_cBRegister == 0;
	if (!fIsPhiNode)
	{
		pInstvalInc = PInstAlloc();
		pInstInc = pInstvalInc->m_pInst;
		pInstInc->m_irop = IROP_ExArgs;
	}

	SValueOutput valout;
	SetOperandFromValue(m_pDlay, pVal, &pInstInc->m_opkLhs, &pInstInc->m_wordLhs, OPSZ_CB, &valout);
	pInstInc->m_cBRegister = valout.m_cBRegister;
	pInstInc->m_wordRhs.m_pV = pBlock;

	if (fIsPhiNode)
	{
		AllocateStackOut(this, pInstInc, valout.m_cBRegister, valout.m_cBRegister);
	}
}

SInstructionValue * CBuilder::PInstCreateSwitch(SValue * pVal, SBlock * pBlockElse, u32 cSwitchCase)
{
	auto pInstvalSwitch = PInstCreateRaw(IROP_Switch, pVal, nullptr);
	auto pInstSwitch = pInstvalSwitch->m_pInst;

	s32 * pIInstElse = &pInstSwitch->m_wordRhs.m_s32;
	auto pBranchElse = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchElse->m_pBlockDest = pBlockElse;
	pBranchElse->m_pIInstDst = pIInstElse;

	for (u32 iSwitchCase = 0; iSwitchCase < cSwitchCase; ++iSwitchCase)
	{
		auto pInstvalCase = PInstAlloc();
		auto pInstCase = pInstvalCase->m_pInst;
		pInstCase->m_irop = IROP_ExArgs;
		pInstCase->m_wordLhs.m_pV = m_pBlockCur;	// make sure we use the right block when adding this case
	}

	return pInstvalSwitch;
}

void CBuilder::AddSwitchCase(SInstructionValue * pInstvalSwitch, SValue * pValCmp, SBlock * pBlock, int iInstCase)
{
	SValueOutput valout;
	SInstruction * pInstEx = (pInstvalSwitch->m_pInst + (iInstCase+1));
	if (EWC_FVERIFY(pInstEx->m_irop == IROP_ExArgs, "bad switch case"))
	{
		SBlock * pBlockSwitch = (SBlock*)pInstEx->m_wordLhs.m_pV;

		SetOperandFromValue(m_pDlay, pValCmp, &pInstEx->m_opkLhs, &pInstEx->m_wordLhs, OPSZ_CB, &valout);
		EWC_ASSERT(valout.m_cBRegister == pInstvalSwitch->m_pInst->m_cBRegister, "switch operand size mismatch");
		
		s32 * pIInst = &pInstEx->m_wordRhs.m_s32;
		auto pBranch = pBlockSwitch->m_aryBranch.AppendNew();
		pBranch->m_pBlockDest = pBlock;
		pBranch->m_pIInstDst = pIInst;

		*pBranch->m_pIInstDst = 1234;
		EWC_ASSERT(pInstEx->m_wordRhs.m_s32 == 1234, "wtf");
	}
}

u8 * CBuilder::PBAllocateGlobalWithPointer(size_t cB, size_t cBAlign, u8 ** ppBPointer, s64 * piBPointer, s64 * piBGlobal, const char * pChzLabel)
{

	u8 * pBGlobal;
	s64 iBGlobal;
	m_dataseg.AllocateData(cB, cBAlign, &pBGlobal, &iBGlobal, pChzLabel);

	const char * pChzLabelPtr = "";
#ifdef EWC_TRACE_GLOBAL
	char aCh[256];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
	FormatCoz(&strbuf, "&%s->%d", pChzLabel, iBGlobal);
	EnsureTerminated(&strbuf, '\0');
	pChzLabelPtr = aCh;
#endif 

	u8 * pBPointer;
	s64 iBPointer;
	m_dataseg.AllocateData(m_pDlay->m_cBPointer, m_pDlay->m_cBPointer, &pBPointer, &iBPointer, pChzLabelPtr);
	m_dataseg.AddRelocatedPointer(m_pDlay, S32Coerce(iBPointer), S32Coerce(iBGlobal));
	*piBGlobal = iBGlobal;
	*piBPointer = iBPointer;
	*ppBPointer = pBPointer;

	return pBGlobal;
}


CBuilder::Global * CBuilder::PGlobCreate(STypeInfo * pTin, const char * pChzName)
{
	// Globals need to store both the value and space for an address as LHS values expect an address
	//   (It's possible we could optimize this out, but trying to special case it has been a mess, if you're trying
	//   this remember to test pointers to globals, constant string pointers, etc)

	u64 cB;
	u64 cBAlign;
	CalculateByteSizeAndAlign(m_pDlay, pTin, &cB, &cBAlign);

	auto pConst = m_blistConst.AppendNew();
	pConst->m_pTin = m_pSymtab->PTinptrAllocate(pTin);
	pConst->m_opk = OPK_Global;

	u8 * pBPointer;
	s64 iBPointer;
	s64 iBGlobal;
	(void) PBAllocateGlobalWithPointer(cB, cBAlign, &pBPointer, &iBPointer, &iBGlobal, pChzName);
	pConst->m_word.m_s64 = iBPointer;

	return pConst;
}

void CBuilder::SetInitializer(BCode::SValue * pValGlob, BCode::SValue * pValInit)
{
	BCode::SConstant * pConstGlob = PValDerivedCast<SConstant *>(pValGlob);
	if (!EWC_FVERIFY(pConstGlob || pConstGlob->m_opk != OPK_Global, "initializing non global"))
		return;

	BCode::SConstant * pConstInit = PValDerivedCast<SConstant *>(pValInit);
	if (!EWC_FVERIFY(pConstInit, "initializer must be constant"))
		return;
	
	EWC_ASSERT(pConstGlob->m_opk == OPK_Global, "expected global");
	u64 cBGlob;
	u64 cBGlobAlign;
	if (pConstInit->m_opk == OPK_Global)
	{
		auto pTinptrGlob = PTinRtiCast<STypeInfoPointer *>(pConstGlob->m_pTin);
		auto pTinDst = pTinptrGlob->m_pTinPointedTo;
		
		{
			CalculateByteSizeAndAlign(m_pDlay, pTinptrGlob->m_pTinPointedTo, &cBGlob, &cBGlobAlign);

			auto pIGlobIndex = (s32*)m_dataseg.PBFromIndex(pConstGlob->m_word.m_s32);
			auto pIInitIndex = (s32*)m_dataseg.PBFromIndex(pConstInit->m_word.m_s32);
			m_dataseg.DeepCopy(m_pDlay, pTinptrGlob->m_pTinPointedTo, *pIGlobIndex, *pIInitIndex);
			return;
		}
	}

	CalculateByteSizeAndAlign(m_pDlay, pConstGlob->m_pTin, &cBGlob, &cBGlobAlign);
	auto pBGlob = m_dataseg.PBFromGlobal(pConstGlob);

	if (EWC_FVERIFY(FIsLiteral(pConstInit->m_opk), "expected literal"))
	{
		switch (cBGlob)
		{
		case 1:	*(u8*)pBGlob = pConstInit->m_word.m_u8;			break;
		case 2:	*(u16*)pBGlob = pConstInit->m_word.m_u16;		break;
		case 4:	*(u32*)pBGlob = pConstInit->m_word.m_u32;		break;
		case 8:	*(u64*)pBGlob = pConstInit->m_word.m_u64;		break;
		default:
			EWC_ASSERT(false, "unexpected constant literal size");
			break;
		}
	}
}


CBuilder::ProcArg *	CBuilder::PProcArg(SValue * pVal)
{
	return pVal;
}

inline STypeInfo * PTinFromVal(SValue * pVal)
{
	switch (pVal->m_valk)
	{
	case VALK_Constant:
	case VALK_BCodeRegister:
		{
			auto pConst = (SConstant *)pVal;
			return pConst->m_pTin;
		}
	case VALK_Instruction:
		{
			auto pInstval = (SInstructionValue *)pVal;
			return pInstval->m_pTinOperand;
		} break;
	case VALK_Procedure:
		{
			// note - pFn's valk is only VALK_Procedure if fIsForeign == false
			auto pProc = (SProcedure *)pVal;
			auto pProcsig = pProc->m_pProcsig;
			return pProcsig->m_pTinproc;
		}
	}

	return nullptr;
}

SInstructionValue * CBuilder::PInstCreateCall(SValue * pValProc, STypeInfoProcedure * pTinproc, ProcArg ** apLvalArgs, int cpLvalArg)
{
	if (!EWC_FVERIFY(m_pProcCur, "Cannot add a procedure call outside of a procedure"))
		return PInstCreateError();

	// BB - should be PProcsigTryLookup() ??
	auto pProcsig = PProcsigEnsure(pTinproc);
	if (!EWC_FVERIFY(pProcsig, "procedure signature was not computed"))
		return PInstCreateError();

	s32 iBStackReturnStore = 0;
	auto cBArg = pProcsig->m_cBArgNamed;

	s64 cArgVariadic = 0;
	s64 cBArgVariadic = 0;
	int cParamNamed = (int)pTinproc->m_arypTinParams.C();
	if (pTinproc->m_grftinproc.FIsSet(FTINPROC_HasVarArgs))
	{
		cArgVariadic = cpLvalArg - cParamNamed;
		s32 iBStackArg = pProcsig->m_sIBStackVariadic + s32(cArgVariadic * sizeof(SBoxedArg));
		iBStackArg = s32(CBAlign(iBStackArg, m_pDlay->m_cBStackAlign));

		cBArgVariadic = iBStackArg - pProcsig->m_sIBStackVariadic;
		cBArg += cBArgVariadic;
	}

	u64 cBReturn = 0;
	u64 cBAlignReturn = 1;
	u32 iBStackOut = 0;
	int cpTinReturn = (int)pTinproc->m_arypTinReturns.C();
	for (int ipTinReturn = 0; ipTinReturn < cpTinReturn; ++ipTinReturn)
	{
		auto pTinRet = pTinproc->m_arypTinReturns[ipTinReturn];
		if (pTinRet->m_tink == TINK_Void)
			break;

		CalculateByteSizeAndAlign(m_pDlay, pTinRet, &cBReturn, &cBAlignReturn);
		if (cBReturn)
		{
			iBStackOut = S32Coerce(IBStackAlloc(cBReturn, cBAlignReturn));
#ifdef EWC_TRACE_RETURN_STACK
			printf("%s() -> ibStackOut = %d, %llu bytes\n", pTinproc->m_strName.PCoz(), iBStackOut, cBReturn);
#endif
		}

		auto pParam = &pProcsig->m_aParamRet[ipTinReturn];

		// subtract cBArg here because we're still in the caller's stack frame
		(void) PInstCreateStoreToReg(S32Coerce(pParam->m_iBStack - cBArg), PConstInt(iBStackOut, 32));
	}

	for (int iParam = 0; iParam < cParamNamed; ++iParam)
	{
		auto pParam = &pProcsig->m_aParamArg[iParam];

		// NOTE: Lhs is relative to called function stack frame, Rhs is relative to calling stack frame

		(void) PInstCreateStoreToReg(S32Coerce(pParam->m_iBStack - cBArg), apLvalArgs[iParam]);

	}

	if (cArgVariadic)
	{
		// store CVarArg
		cArgVariadic = cpLvalArg - cParamNamed;

		s32 iBStackArg = pProcsig->m_sIBStackVariadic;
		for (int iArgVar = 0; iArgVar < cArgVariadic; ++iArgVar)
		{
			auto pValArg = apLvalArgs[cParamNamed + iArgVar];
			auto pTinArg = PTinFromVal(pValArg);
			EWC_ASSERT(pTinArg, "unable to determine var arg type");

			// subtract cBArg here because we're still in the caller's stack frame
			(void) PInstCreateStoreToReg(S32Coerce(iBStackArg + EWC_OFFSET_OF(SBoxedArg, m_pTin) - cBArg), PConstPointer(pTinArg));
			(void) PInstCreateStoreToReg(S32Coerce(iBStackArg + EWC_OFFSET_OF(SBoxedArg, m_word) - cBArg), pValArg);
			iBStackArg += sizeof(SBoxedArg);
		}

		iBStackArg = s32(CBAlign(iBStackArg, m_pDlay->m_cBStackAlign));
		EWC_ASSERT(cBArgVariadic == iBStackArg - pProcsig->m_sIBStackVariadic, "bad cBArgVariadic calculation");
	}

	auto pValProcsig = PConstPointer(pProcsig);
	auto pInstvalCall = PInstCreateRaw(IROP_Call, 0, pValProc, pValProcsig);
	pInstvalCall->m_pInst->m_iBStackOut = iBStackOut;

	if (cArgVariadic)
	{
		auto pInstvalEx = PInstAlloc();
		auto pInstEx = pInstvalEx->m_pInst;
		pInstEx->m_irop =  IROP_ExArgs;	
		pInstEx->m_opkLhs = OPK_Literal;
		pInstEx->m_opkRhs = OPK_Literal;
		pInstEx->m_wordLhs.m_s64 = cArgVariadic;
		pInstEx->m_wordRhs.m_s64 = cBArgVariadic;
	}

	return pInstvalCall;
}

void CBuilder::CreateReturn(SValue ** apVal, int cpVal, const char * pChzName)
{
	if (!EWC_FVERIFY(m_pProcCur, "Cannot add a return opcode outside of a procedure"))
		return;

	if (cpVal && apVal[0] == nullptr)
	{
		cpVal = 0;
	}

	auto pProcsig = m_pProcCur->m_pProcsig;
	auto pTinproc = pProcsig->m_pTinproc;
	for (int iReturn = 0; iReturn < cpVal; ++iReturn)
	{
		// add the returnIdx(callee relative) stored as an argument to (cBArg + cBStack)

		// add the calling stack relative return index to (cBArg + cBStack)
		// BB - doesn't take variadic args into account
		auto pInstOffset = PInstCreateRaw(IROP_NAdd, PRegArg(pProcsig->m_aParamRet[0].m_iBStack, 32), PConstArg(pProcsig->m_cBArgNamed, 32));

		auto pInstval = (SInstructionValue *)apVal[iReturn];
		(void) PInstCreateStoreToIdx(pInstOffset, apVal[iReturn]);
	}

	s32 cBReturnOp = 0;
	if (!pTinproc->m_arypTinReturns.FIsEmpty())
	{
		cBReturnOp = pProcsig->m_aParamRet[0].m_cB;
	}

	PInstCreateRaw(IROP_Ret, 0, PConstArg(pProcsig->m_cBArgNamed, 32), PConstArg(0, 32));
}

SInstructionValue * CBuilder::PInstCreatePtrToInt(SValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName)
{
	return PInstCreateCast(IROP_PtrToInt, pValOperand, pTinint, pChzName);
}

SInstructionValue * CBuilder::PInstCreateTraceStore(SValue * pVal, STypeInfo * pTin)
{
	return PInstCreateRaw(IROP_TraceStore, pVal, PConstPointer(pTin));
}

void CBuilder::CreateBranch(SBlock * pBlock)
{
	EWC_ASSERT(m_pBlockCur && !m_pBlockCur->FIsFinalized(), "cannot allocate instructions without a unfinalized basic block");

	auto pInstval = PInstCreateRaw(IROP_Branch, nullptr, nullptr);
	auto pInst = pInstval->m_pInst;

	s32 * pIInstDst = (s32*)&pInst->m_wordRhs;
	auto pBranch = m_pBlockCur->m_aryBranch.AppendNew();
	pBranch->m_pBlockDest = pBlock;
	pBranch->m_pIInstDst = pIInstDst;
}

SInstructionValue * CBuilder::PInstCreateCondBranch(SValue * pValPred, SBlock * pBlockTrue, SBlock * pBlockFalse)
{
	auto pInstval = PInstCreateRaw(IROP_CondBranch, pValPred, nullptr);
	auto pInst = pInstval->m_pInst;

	s32 * pIInstDst = (s32*)&pInst->m_wordRhs;
	auto pBranchTrue = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchTrue->m_pBlockDest = pBlockTrue;
	pBranchTrue->m_pIInstDst = &pIInstDst[true];

	auto pBranchFalse = m_pBlockCur->m_aryBranch.AppendNew();
	pBranchFalse->m_pBlockDest = pBlockFalse;
	pBranchFalse->m_pIInstDst = &pIInstDst[false];

	return pInstval;
}

s32	CBuilder::IBStackAlloc(s64 cB, s64 cBAlign)
{
	if (!EWC_FVERIFY(m_pProcCur, "Allocating from the stack without an active procedure"))
		return 0;
	if (cB == 0)
		return 0;
	size_t cBMasked = cBAlign - 1;
	u32 cBStack = U32Coerce((m_pProcCur->m_cBStack + cBMasked) & ~cBMasked);
	m_pProcCur->m_cBStack = cBStack + cB;
	return cBStack;
}

SInstructionValue * CBuilder::PInstAlloc()
{
	EWC_ASSERT(m_pBlockCur && !m_pBlockCur->FIsFinalized(), "cannot allocate instructions without a unfinalized basic block");
	auto pInstval = m_pBlockCur->m_aryInstval.AppendNew();
	auto pInst = m_pBlockCur->m_aryInst.AppendNew();
	pInstval->m_pInst = pInst;
	return pInstval;
}

SValue * CBuilder::PValFromSymbol(SSymbol * pSym)
{
	auto ppVal = m_hashPSymPVal.Lookup(pSym);
	if (!ppVal)
		return nullptr;
	auto pVal = *ppVal;

	if (pVal && pVal->m_valk == VALK_Instruction)
	{
		IROP irop = ((SInstructionValue *)pVal)->m_pInst->m_irop;
	}

	return pVal;
}

void CBuilder::SetSymbolValue(SSymbol * pSym, SValue * pVal)
{
	(void) m_hashPSymPVal.FinsEnsureKeyAndValue(pSym, pVal);
}

CBuilder::SCodeGenStruct * CBuilder::PCgstructEnsure(STypeInfoStruct * pTinstruct)
{
	SCodeGenStruct ** ppCgstruct = nullptr;
	auto fins = m_hashPTinstructPCgstruct.FinsEnsureKey(pTinstruct, &ppCgstruct);
	if (fins == FINS_Inserted)
	{
		*ppCgstruct = EWC_NEW(m_pAlloc, SCodeGenStruct) SCodeGenStruct();
		(*ppCgstruct)->m_pLtype = pTinstruct;
	}

	return *ppCgstruct;
}

SProcedureSignature * CBuilder::PProcsigEnsure(STypeInfoProcedure * pTinproc)
{
	SProcedureSignature ** ppProcsig;
	auto fins = m_hashPTinprocPProcsig.FinsEnsureKey(pTinproc, &ppProcsig);
	if (fins == FINS_Inserted)
	{
		size_t cArg = pTinproc->m_arypTinParams.C();
		size_t cRet = pTinproc->m_arypTinReturns.C();
		size_t cBAlloc = sizeof(SProcedureSignature) + sizeof(SParameter) * (cArg + cRet);
		cBAlloc = EWC::CBAlign(cBAlloc, ewcMax(EWC_ALIGN_OF(SProcedureSignature), EWC_ALIGN_OF(SParameter)));

		u8 * pBAlloc = (u8 *)m_pAlloc->EWC_ALLOC(cBAlloc, EWC_ALIGN_OF(SProcedureSignature));

		auto pProcsig = new(pBAlloc) SProcedureSignature(pTinproc);
		*ppProcsig = pProcsig;

		SParameter * aParamArg = (SParameter *)PVAlign(pBAlloc + sizeof(SProcedureSignature), EWC_ALIGN_OF(SParameter));
		if (cArg)
		{
			pProcsig->m_aParamArg = aParamArg;
		}

		if (cRet)
		{
			pProcsig->m_aParamRet = aParamArg + cArg;
		}

		pProcsig->m_cBArgNamed += sizeof(SInstruction *);
		pProcsig->m_cBArgNamed += sizeof(SProcedure *);

		u64 cB; 
		u64 cBAlign;
		u64 cBReturn;
		u64 cBReturnSum = 0;
		for (size_t iArg = 0; iArg < cArg; ++iArg)
		{
			auto pTinParam = pTinproc->m_arypTinParams[iArg];
			CalculateByteSizeAndAlign(m_pDlay, pTinParam, &cB, &cBAlign);

			auto pParam = &pProcsig->m_aParamArg[iArg];
			pParam->m_cB = S32Coerce(cB);
			pParam->m_cBAlign = S32Coerce(cBAlign);

			pParam->m_iBStack = S32Coerce(IBArgAlloc(&pProcsig->m_cBArgNamed, cB, cBAlign));
		}

		for (size_t iRet = 0; iRet < cRet; ++iRet)
		{
			auto pTinParam = pTinproc->m_arypTinReturns[iRet];

			// allocate space for index of return storage
			cB = (pTinParam->m_tink == TINK_Void) ? 0 : sizeof(s32);
			cBAlign = EWC_ALIGN_OF(s32);

			CalculateByteSizeAndAlign(m_pDlay, pTinParam, &cBReturn, &cBAlign);
			cBReturnSum += cBReturn;

			auto pParam = &pProcsig->m_aParamRet[iRet];
			pParam->m_cB = S32Coerce(cB);
			pParam->m_cBAlign = S32Coerce(cBAlign);
			pParam->m_iBStack = S32Coerce(IBArgAlloc(&pProcsig->m_cBArgNamed, cB, cBAlign));
		}
		pProcsig->m_cBArgReturn = S32Coerce(cBReturnSum);

		if (pTinproc->m_grftinproc.FIsSet(FTINPROC_HasVarArgs))
		{
			pProcsig->m_cBArgNamed = CBAlign(pProcsig->m_cBArgNamed, EWC_ALIGN_OF(SBoxedArg)); 
			pProcsig->m_sIBStackVariadic = s32(pProcsig->m_cBArgNamed);
		}
		else
		{
			pProcsig->m_cBArgNamed = CBAlign(pProcsig->m_cBArgNamed, m_pDlay->m_cBStackAlign);
		}
	}

	return *ppProcsig;
}

SInstructionValue * CBuilder::PInstCreate(IROP irop, SValue * pValLhs, const char * pChzName)
{
	switch (irop)
	{
	case IROP_Load:
	case IROP_Ret:
	case IROP_NNeg:
	case IROP_GNeg:
	case IROP_Not:
		return PInstCreateRaw(irop, pValLhs, nullptr, pChzName);
	default: 
		EWC_ASSERT(false, "%s is not a unary opcode supported by PInstCreate", PChzFromIrop(irop)); break;
	}
	return PInstCreateError();
}

SInstructionValue* CBuilder::PInstCreate(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName)
{
	// switch through cases to make sure we use custom functions where needed
	switch (irop)
	{
	case IROP_NAdd:
	case IROP_GAdd:
	case IROP_NSub:
	case IROP_GSub:
	case IROP_NMul:
	case IROP_GMul:
	case IROP_SDiv:
	case IROP_UDiv:
	case IROP_GDiv:
	case IROP_SRem:
	case IROP_URem:
	case IROP_GRem:
	case IROP_Shl:
	case IROP_AShr:
	case IROP_LShr:
	case IROP_And:
	case IROP_Or:
	case IROP_Xor:
		return PInstCreateRaw(irop, pValLhs, pValRhs, pChzName);
	default: 
		EWC_ASSERT(false, "%s is not a binary opcode supported by PInstCreate", PChzFromIrop(irop)); 
		break;
	}

	return PInstCreateError();
}

static inline bool FIsValidCBRegister(int cBRegister)
{
	return (cBRegister == 1) | (cBRegister == 2) | (cBRegister == 4) | (cBRegister == 8);
}

static inline bool FDefinesCB(OPSZ opsz)
{
	return (opsz == OPSZ_CB) || (opsz == OPSZ_PCB);
}


inline void SetOperandFromValue(
	SDataLayout * pDlay,
	SValue * pValSrc,
	OPK * pOpkOut,
	SWord * pWordOut,
	OPSZ opsz,
	SValueOutput * pValout)
{
	if (!pValSrc)
	{
		*pValout = SValueOutput();
		return;
	}

	switch (pValSrc->m_valk)
	{
	case VALK_Constant:
	case VALK_BCodeRegister:
	{
		auto pConst = PValDerivedCast<SConstant *>(pValSrc);

		*pOpkOut = pConst->m_opk;
		pWordOut->m_u64 = pConst->m_word.m_u64;
		pValout->m_pTin = pConst->m_pTin;

		switch (pConst->m_opk)
		{
		case OPK_Literal:
		case OPK_LiteralArg:
			{
				pValout->m_cBRegister = (pConst->m_litty.m_cBit + 7) / 8;
				EWC_ASSERT(FIsValidCBRegister(pValout->m_cBRegister), "unexpected operand size.");
			} break;
		case OPK_Global:
			{
				pValout->m_cBRegister = pDlay->m_cBPointer;
			} break;
		default:
			{
				u64 cB; 
				u64 cBAlign;
				CalculateByteSizeAndAlign(pDlay, pValout->m_pTin, &cB, &cBAlign);
				pValout->m_cBRegister = U8Coerce(cB);
			} break;
		}
	} break;
	case VALK_Instruction:
	{
		auto pInstval = PValDerivedCast<SInstructionValue *>(pValSrc);
		auto pInst = pInstval->m_pInst;

		*pOpkOut = OPK_Register; // BB - is this ever an arg register?
		pValout->m_cBRegister = CBResult(pInst);
		pWordOut->m_s32 = pInst->m_iBStackOut;

		pValout->m_pTin = pInstval->m_pTinOperand;

		//EWC_ASSERT(FIsValidCBRegister(pValout->m_cBRegister), "unexpected operand size from instruction.");
	} break;
	case VALK_Procedure:
	{
		auto pProc = (SProcedure *)pValSrc;
		auto pProcsig = pProc->m_pProcsig;
		EWC_ASSERT(pProcsig, "Expected procedure signature.")

		*pOpkOut = OPK_Literal; // BB - is this ever an arg register?
		pWordOut->m_pV = pValSrc;

		pValout->m_cBRegister = sizeof(pProc);
		pValout->m_pTin = pProcsig->m_pTinproc;
	} break;
	default: 
		EWC_ASSERT(false, "unhandled VALK");
		break;
	}

	if (opsz == OPSZ_PCB)
	{
		auto pTinptr = PTinRtiCast<STypeInfoPointer *>(pValout->m_pTin);
		EWC_ASSERT(pTinptr, "operand type should be pointer type: is %s", StrFromTypeInfo(pValout->m_pTin).PCoz());
		pValout->m_pTin = pTinptr->m_pTinPointedTo;

		u64 cB; 
		u64 cBAlign;
		CalculateByteSizeAndAlign(pDlay, pValout->m_pTin, &cB, &cBAlign);
		pValout->m_cBRegister = U8Coerce(cB);
	}
}

static inline void VerifyCanAssignTypes(STypeInfo * pTinDst, STypeInfo * pTinSrc)
{
#ifdef CHECK_INST_TYPES
	if (pTinSrc->m_tink == TINK_Literal)
	{
		auto pTinlit = (STypeInfoLiteral *)pTinSrc;
		switch (pTinlit->m_litty.m_litk)
		{
			case LITK_Integer:
			{
				auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinDst);
				if (pTinint && pTinint->m_cBit == pTinlit->m_litty.m_cBit && pTinint->m_fIsSigned == pTinlit->m_litty.m_fIsSigned)
					return;
			}break;
			case LITK_Float:
			{
				auto pTinfloat = PTinRtiCast<STypeInfoFloat *>(pTinDst);
				if (pTinfloat && pTinfloat->m_cBit == pTinlit->m_litty.m_cBit)
					return;
			}break;
			case LITK_Bool:
				if (pTinDst->m_tink == TINK_Bool)
					return;
				return;
			case LITK_Null:
				if (pTinDst->m_tink == TINK_Pointer)
					return;
				break;
			case LITK_String:
			{
				auto pTinptr = PTinRtiCast<STypeInfoPointer *>(pTinDst);
				if (pTinptr)
				{
					auto pTinqual = PTinRtiCast<STypeInfoQualifier *>(pTinptr->m_pTinPointedTo);
					if (pTinqual && pTinqual->m_pTin->m_tink == TINK_Integer)
					{
						auto pTinint = (STypeInfoInteger *)pTinqual->m_pTin;
						if (pTinint->m_cBit == 8)
							return;
					}
				}
			} break;
			case LITK_Char:
			case LITK_Enum:
			case LITK_Array:
			case LITK_Pointer:
				break;
		}

		EWC_ASSERT(false, 
			"Cannot assign %s literal to %s", PChzFromLitk(pTinlit->m_litty.m_litk), StrFromTypeInfo(pTinDst).PCoz());
	}

	EWC_ASSERT(FTypesAreSame(pTinDst, pTinSrc),
		"Type mismatch %s != %s", StrFromTypeInfo(pTinDst).PCoz(), StrFromTypeInfo(pTinSrc).PCoz());
#endif
}


SInstructionValue * CBuilder::PInstCreateRaw(IROP irop, SValue * pValLhs, SValue * pValRhs, const char * pChzName)
{
	return PInstCreateRaw(irop, -1, pValLhs, pValRhs, pChzName);
}

SInstructionValue * CBuilder::PInstCreateRaw(IROP irop, s64 cBOperandArg, SValue * pValLhs, SValue * pValRhs, const char * pChzName)
{
	if (!EWC_FVERIFY(m_pBlockCur, "creating instruction with no active block"))
		return nullptr;

	if (m_pBlockCur->FIsFinalized())
	{
		if (irop != IROP_Branch && EWC_FVERIFY(m_pBerrctx, "trying to throw warning with no error context"))
		{
			EmitWarning(m_pBerrctx->m_pErrman, m_pBerrctx->m_pLexloc, ERRID_UnreachableInst, "Unreachable instruction detected");
		}
		irop = IROP_Error;
		pValLhs = nullptr;
		pValRhs = nullptr;
	}

	auto pOpsig = POpsig(irop);
	auto pInstval = PInstAlloc();
	auto pInst = pInstval->m_pInst;
	pInst->m_irop = irop;

	SValueOutput valoutLhs, valoutRhs;
	if (pValLhs)
	{
		EWC_ASSERT(pOpsig->m_opszLhs != OPSZ_0, "unexpected LHS operand to IROP_%s", PChzFromIrop(irop));
		SetOperandFromValue(m_pDlay, pValLhs, &pInst->m_opkLhs, &pInst->m_wordLhs, pOpsig->m_opszLhs, &valoutLhs);

		// BB - ugly exception for void returns 
//		EWC_ASSERT(cBLhs != 0 || (irop == IROP_Ret), "expected LHS operand, but has zero size (irop = %s)", PChzFromIrop(irop));
	}
	else
	{
		pInst->m_wordLhs.m_u64 = 0;
	}

	if (pValRhs)
	{
		EWC_ASSERT(pOpsig->m_opszRhs != OPSZ_0, "unexpected RHS operand to IROP_%s", PChzFromIrop(irop));
		SetOperandFromValue(m_pDlay, pValRhs, &pInst->m_opkRhs, &pInst->m_wordRhs, pOpsig->m_opszRhs, &valoutRhs);

		//EWC_ASSERT(valoutRhs.m_cBRegister != 0, "expected RHS operand, but has zero size");
	}
	else
	{
		pInst->m_wordRhs.m_u64 = 0;
	}

	EWC_ASSERT(pOpsig->m_opszLhs != OPSZ_CB || pOpsig->m_opszRhs != OPSZ_CB || 
		valoutLhs.m_cBRegister == valoutRhs.m_cBRegister, "operand size mismatch");

	if (irop == IROP_Store)
	{
		VerifyCanAssignTypes(valoutLhs.m_pTin, valoutRhs.m_pTin);
	}

	SValueOutput valout;
	switch (pOpsig->m_cbsrc)
	{
	case CBSRC_Lhs:	
		EWC_ASSERT(cBOperandArg < 0, "passing cBOperand into irop with CBSRC");
		valout = valoutLhs;
		break;
	case CBSRC_Rhs:		
		EWC_ASSERT(cBOperandArg < 0, "passing cBOperand into irop with CBSRC");
		valout = valoutRhs;
		break;
	default:			
		if (cBOperandArg < 0)
		{
			EWC_ASSERT(pOpsig->m_opszRet != OPSZ_CB, "unable to determine OPSZ_CB");
		}
		else
		{
			valout.m_cBRegister = S8Coerce(cBOperandArg);	
		}
		break;
	}

	switch (pOpsig->m_opszRet)
	{
		case OPSZ_1:	AllocateStackOut(this, pInst, 1, 1);						break;
		case OPSZ_2:	AllocateStackOut(this, pInst, 2, 2);						break;
		case OPSZ_4:	AllocateStackOut(this, pInst, 4, 4);						break;
		case OPSZ_8:	AllocateStackOut(this, pInst, 8, 8);						break;
		case OPSZ_CB:	AllocateStackOut(this, pInst, valout.m_cBRegister, valout.m_cBRegister);		break;
		case OPSZ_PCB:	AllocateStackOut(this, pInst, m_pDlay->m_cBPointer, m_pDlay->m_cBPointer);		break;
		case OPSZ_Ptr:	AllocateStackOut(this, pInst, m_pDlay->m_cBPointer, m_pDlay->m_cBPointer);		break;
		case OPSZ_RegIdx:AllocateStackOut(this, pInst, 4, 4);						break;

		case OPSZ_0:	// fallthrough
		default:
			break;
	}
	
	if (FUseLargeOperand(irop))
	{
		EWC_ASSERT(!pValRhs, "trying to set rhs for large operand instruction %s", PChzFromIrop(irop));
		pInst->m_wordRhs.m_s32 = valout.m_cBRegister;

	}
	else
	{
		EWC_ASSERT(FIsValidCBRegister(valout.m_cBRegister) || (valout.m_cBRegister == 0 && pOpsig->m_cbsrc == CBSRC_Nil), 
			"unexpected operand size. (%d bytes) for IROP_%s", valout.m_cBRegister, PChzFromIrop(irop));
		pInst->m_cBRegister = valout.m_cBRegister;
	}

	pInstval->m_pTinOperand = valout.m_pTin;
	return pInstval;
}

SInstructionValue * CBuilder::PInstCreateError()
{
	auto pInstval = PInstAlloc();
	auto pInst = pInstval->m_pInst;

	pInst->m_irop = IROP_Error;
	pInst->m_cBRegister = 0;
	return pInstval;
}

SInstructionValue * CBuilder::PInstCreateCast(IROP irop, SValue * pValSrc, STypeInfo * pTinDst, const char * pChzName)
{
	u64 cBDst;
	u64 cBAlignDst;
	CalculateByteSizeAndAlign(m_pDlay, pTinDst, &cBDst, &cBAlignDst);

	OPK opkLhs;
	SWord wordLhs;
	SValueOutput valout;
	SetOperandFromValue(m_pDlay, pValSrc, &opkLhs, &wordLhs, OPSZ_CB, &valout);

	SInstructionValue * pInstval = PInstCreateRaw(irop, cBDst, nullptr, PConstInt(valout.m_cBRegister, 8), pChzName);
	if (pInstval->FIsError())
		return pInstval;

	SInstruction * pInst = pInstval->m_pInst;
	pInstval->m_pTinOperand = pTinDst;
	pInst->m_wordLhs = wordLhs;
	pInst->m_opkLhs = opkLhs;

	return pInstval;
}

SInstructionValue * CBuilder::PInstCreateStore(SValue * pValPDst, SValue * pValSrc)
{
	OPK opkDst;
	SWord wordDst;
	SValueOutput valoutDst;
	SetOperandFromValue(m_pDlay, pValPDst, &opkDst, &wordDst, OPSZ_PCB, &valoutDst);

	if (opkDst == OPK_Global)
	{
		// IROP_Store instruction can't directly address indices into the global block so we create an 
		//  instruction to get the address and store there.

		auto pInstvalAddr = PInstCreateRaw(IROP_StoreAddress, m_pDlay->m_cBPointer, pValPDst, nullptr);
		
		// The global index refers to a pre-relocate pointer to the global data we're storing into - 
		//   we need to 'dereference' it here:
		s32 * pIBDest = (s32*)m_dataseg.PBFromIndex(wordDst.m_s32);
		pInstvalAddr->m_pInst->m_wordLhs.m_s32 = *pIBDest;

		auto pInstvalStore = PInstCreateRaw(IROP_Store, pValSrc, nullptr, "");
		pInstvalStore->m_pInst->m_iBStackOut = pInstvalAddr->m_pInst->m_iBStackOut;
		return pInstvalStore;
	}

	// store pValSrc into the address pointed to by pValPDst

	auto pInstval = PInstCreateRaw(IROP_Store, pValSrc, nullptr, "");
	auto pInst = pInstval->m_pInst;


	EWC_ASSERT(pInst->m_cBRegister == 0 && pInst->m_iBStackOut == 0, "expected no return pos");

	if (EWC_FVERIFY(opkDst == OPK_Register, "expected register value for store destination"))
	{
		pInst->m_iBStackOut = wordDst.m_s32;
	}

	return pInstval;
}

SInstructionValue * CBuilder::PInstCreateStoreToReg(s32 dstIdx, SValue * pValSrc, const char * pChzName)
{
	// store pValSrc into the address pointed to by pValPDst
	auto pInstval = PInstCreateRaw(IROP_StoreToReg, pValSrc, nullptr, pChzName);
	auto pInst = pInstval->m_pInst;


	EWC_ASSERT(pInst->m_cBRegister == 0 && pInst->m_iBStackOut == 0, "expected no return pos");

	pInst->m_iBStackOut = dstIdx;
	return pInstval;
}

SInstructionValue * CBuilder::PInstCreateStoreToIdx(SValue * pValPDst, SValue * pValSrc, const char * pChzName)
{
	// store pValSrc into the address pointed to by pValPDst
	auto pInstval = PInstCreateRaw(IROP_StoreToIdx, pValSrc, nullptr, pChzName);
	auto pInst = pInstval->m_pInst;


	EWC_ASSERT(pInst->m_cBRegister == 0 && pInst->m_iBStackOut == 0, "expected no return pos");

	OPK opkDst;
	SWord wordDst;
	SValueOutput valoutDst;
	SetOperandFromValue(m_pDlay, pValPDst, &opkDst, &wordDst, OPSZ_CB, &valoutDst);

	pInst->m_iBStackOut = wordDst.m_s32;
	return pInstval;
}

void CBuilder::AddManagedVal(SValue * pVal)
{
	if(pVal->m_valk == VALK_Procedure)
	{
		m_arypProcManaged.Append((SProcedure*)pVal);
		return;
	}
	m_arypValManaged.Append(pVal);
}

static inline void LoadWord(u8 * aB, SWord * pWord, s32 iB, int cB)
{
	u8 * pB = &aB[iB];
	switch (cB)
	{
		case 1:	pWord->m_u8 = *pB;			break;
		case 2:	pWord->m_u16 = *(u16*)pB;	break;
		case 4:	pWord->m_u32 = *(u32*)pB;	break;
		case 8:	pWord->m_u64 = *(u64*)pB;	break;
		default: 
			EWC_ASSERT(false, "unexpected operand byte count (%d)", cB);
	}
}

static inline void ReadOpcode(CVirtualMachine * pVm, SInstruction * pInst, int cB, SWord * pWordLhs)
{
	switch (pInst->m_opkLhs)
	{
	case OPK_Literal:
	case OPK_LiteralArg:	*pWordLhs = pInst->m_wordLhs;										break;
	case OPK_Register:
	case OPK_RegisterArg:	LoadWord(pVm->m_pBStack, pWordLhs, pInst->m_wordLhs.m_s32, cB);		break;
	case OPK_Global:		LoadWord(pVm->m_pBGlobal, pWordLhs, pInst->m_wordLhs.m_s32, cB);	break;
	}
}

static inline void ReadOpcodes(CVirtualMachine * pVm, SInstruction * pInst, int cB, SWord * pWordLhs, SWord * pWordRhs)
{
	switch (pInst->m_opkLhs)
	{
	case OPK_Literal:
	case OPK_LiteralArg:	*pWordLhs = pInst->m_wordLhs;										break;
	case OPK_Register:
	case OPK_RegisterArg:	LoadWord(pVm->m_pBStack, pWordLhs, pInst->m_wordLhs.m_s32, cB);		break;
	case OPK_Global:		LoadWord(pVm->m_pBGlobal, pWordLhs, pInst->m_wordLhs.m_s32, cB);	break;
	}

	switch (pInst->m_opkRhs)
	{
	case OPK_Literal:
	case OPK_LiteralArg:	*pWordRhs = pInst->m_wordRhs;										break;
	case OPK_Register:
	case OPK_RegisterArg:	LoadWord(pVm->m_pBStack, pWordRhs, pInst->m_wordRhs.m_s32, cB);		break;
	case OPK_Global:		LoadWord(pVm->m_pBGlobal, pWordRhs, pInst->m_wordRhs.m_s32, cB);	break;
	}
}

static inline void ReadCastOpcodes(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordLhs, SWord * pWordRhs)
{
	// Rhs is always 8 bytes, and is cBOperand for Lhs

	switch (pInst->m_opkRhs)
	{
	case OPK_Literal:
	case OPK_LiteralArg:	*pWordRhs = pInst->m_wordRhs;										break;
	case OPK_Register:
	case OPK_RegisterArg:	LoadWord(pVm->m_pBStack, pWordRhs, pInst->m_wordRhs.m_s32, 8);		break;
	case OPK_Global:		LoadWord(pVm->m_pBGlobal, pWordRhs, pInst->m_wordRhs.m_s32, 8);		break;
	}

	switch (pInst->m_opkLhs)
	{
	case OPK_Literal:
	case OPK_LiteralArg:	*pWordLhs = pInst->m_wordLhs;															break;
	case OPK_Register:
	case OPK_RegisterArg:	LoadWord(pVm->m_pBStack, pWordLhs, pInst->m_wordLhs.m_s32, int(pWordRhs->m_u64));		break;
	case OPK_Global:		LoadWord(pVm->m_pBGlobal, pWordLhs, pInst->m_wordLhs.m_s32, int(pWordRhs->m_u64));		break;
	}
}

static inline void * PVReadAddressLhs(CVirtualMachine * pVm, SInstruction * pInst, SWord * pWordTemp)
{
	switch (pInst->m_opkLhs)
	{
	case OPK_Literal:
	case OPK_LiteralArg:	
		return &pInst->m_wordLhs;
	case OPK_Register:
	case OPK_RegisterArg:	
		return &pVm->m_pBStack[pInst->m_wordLhs.m_s32];
	case OPK_Global:		
		return &pVm->m_pBGlobal[pInst->m_wordLhs.m_s32];
	default: 
		EWC_ASSERT(false, "unhandled OPK");
		return nullptr;
	}
}

/*
template <typename T> struct SWordElement		{ T Lookup(SWord & word) { return word.m_u64; } };
template <> struct SWordElement<s8>				{ T Lookup(SWord & word) { return word.m_s8; } };
template <> struct SWordElement<s16>			{ T Lookup(SWord & word) { return word.m_s16; } };
template <> struct SWordElement<s32>			{ T Lookup(SWord & word) { return word.m_s32; } };
template <> struct SWordElement<s64>			{ T Lookup(SWord & word) { return word.m_s64; } };
template <> struct SWordElement<u8>				{ T Lookup(SWord & word) { return word.m_u8; } };
template <> struct SWordElement<u16>			{ T Lookup(SWord & word) { return word.m_u16; } };
template <> struct SWordElement<u32>			{ T Lookup(SWord & word) { return word.m_u32; } };
template <> struct SWordElement<u64>			{ T Lookup(SWord & word) { return word.m_u64; } };
template <> struct SWordElement<f32>			{ T Lookup(SWord & word) { return word.m_f32; } };
template <> struct SWordElement<f64>			{ T Lookup(SWord & word) { return word.m_f64; } };
*/

// partial specialization to help write op handlers
template <s32 CB> struct SWordOpsize			{ };
template <> struct SWordOpsize<1>			
	{ 
		static s8 Signed(SWord & word)		{ return word.m_s8; }
		static u8 Unsigned(SWord & word)	{ return word.m_u8; }
	};
template <> struct SWordOpsize<2>			
	{ 
		static s16 Signed(SWord & word)		{ return word.m_s16; }
		static u16 Unsigned(SWord & word)	{ return word.m_u16; }
	};
template <> struct SWordOpsize<4>
	{ 
		static s32 Signed(SWord & word)		{ return word.m_s32; }
		static u32 Unsigned(SWord & word)	{ return word.m_u32; }
		static f32 Float(SWord & word)		{ return word.m_f32; }
	};
template <> struct SWordOpsize<8>
	{ 
		static s64 Signed(SWord & word)		{ return word.m_s64; }
		static u64 Unsigned(SWord & word)	{ return word.m_u64; }
		static f64 Float(SWord & word)		{ return word.m_f64; }
	};

template <s32 CB>
bool FEvaluateNCmp(NPRED npred, SWord & wordLhs, SWord & wordRhs)
{
	switch (npred)
	{
	case NPRED_EQ:	return SWordOpsize<CB>::Unsigned(wordLhs) == SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_NE:  return SWordOpsize<CB>::Unsigned(wordLhs) != SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SGT:	return SWordOpsize<CB>::Signed(wordLhs) > SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_UGT:	return SWordOpsize<CB>::Unsigned(wordLhs) > SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SGE:	return SWordOpsize<CB>::Signed(wordLhs) >= SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_UGE:	return SWordOpsize<CB>::Unsigned(wordLhs) >= SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SLT:	return SWordOpsize<CB>::Signed(wordLhs) < SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_ULT:	return SWordOpsize<CB>::Unsigned(wordLhs) < SWordOpsize<CB>::Unsigned(wordRhs);
	case NPRED_SLE:	return SWordOpsize<CB>::Signed(wordLhs) <= SWordOpsize<CB>::Signed(wordRhs);
	case NPRED_ULE:	return SWordOpsize<CB>::Unsigned(wordLhs) <= SWordOpsize<CB>::Unsigned(wordRhs);
	}

	EWC_ASSERT(false, "unhandled predicate type");
	return false;
}

template <s32 CB>
bool FEvaluateGCmp(GPRED gpred, SWord & wordLhs, SWord & wordRhs)
{
	switch (gpred)
	{
	case GPRED_EQ:	return SWordOpsize<CB>::Float(wordLhs) == SWordOpsize<CB>::Float(wordRhs);
	case GPRED_NE:	return SWordOpsize<CB>::Float(wordLhs) != SWordOpsize<CB>::Float(wordRhs);
	case GPRED_GT:	return SWordOpsize<CB>::Float(wordLhs) > SWordOpsize<CB>::Float(wordRhs);
	case GPRED_GE:	return SWordOpsize<CB>::Float(wordLhs) >= SWordOpsize<CB>::Float(wordRhs);
	case GPRED_LT:	return SWordOpsize<CB>::Float(wordLhs) < SWordOpsize<CB>::Float(wordRhs);
	case GPRED_LE:	return SWordOpsize<CB>::Float(wordLhs) <= SWordOpsize<CB>::Float(wordRhs);
	}

	EWC_ASSERT(false, "unhandled predicate type");
	return false;
}

void PrintInstance(CVirtualMachine * pVm, STypeInfo * pTin, u8 * pData)
{
	switch (pTin->m_tink)
	{
    case TINK_Integer:
	{
		auto pTinint = (STypeInfoInteger *)pTin;
		if (pTinint->m_fIsSigned)
		{
			switch (pTinint->m_cBit)
			{
			case 8: FormatCoz(pVm->m_pStrbuf, "%d", *(s8*)pData);	break;
			case 16: FormatCoz(pVm->m_pStrbuf, "%d", *(s16*)pData); break;
			case 32: FormatCoz(pVm->m_pStrbuf, "%d", *(s32*)pData); break;
			case 64: FormatCoz(pVm->m_pStrbuf, "%lld", *(s64*)pData); break;
			default: EWC_ASSERT(false, "unexpected float size");
			}
		}
		else
		{
			switch (pTinint->m_cBit)
			{
			case 8: FormatCoz(pVm->m_pStrbuf, "%u", *(u8*)pData);	break;
			case 16: FormatCoz(pVm->m_pStrbuf, "%u", *(u16*)pData); break;
			case 32: FormatCoz(pVm->m_pStrbuf, "%u", *(u32*)pData); break;
			case 64: FormatCoz(pVm->m_pStrbuf, "%llu", *(u64*)pData); break;
			default: EWC_ASSERT(false, "unexpected float size");
			}
		}
	} break;
    case TINK_Float:
	{
		auto pTinfloat = (STypeInfoFloat *)pTin;
		switch (pTinfloat->m_cBit)
		{
		case 32: FormatCoz(pVm->m_pStrbuf, "%f", *(f32*)pData);	break;
		case 64: FormatCoz(pVm->m_pStrbuf, "%f", *(f64*)pData); break;
		default: EWC_ASSERT(false, "unexpected float size");
		}
	} break;
    case TINK_Bool:
	{
		EWC_ASSERT(pVm->m_pDlay->m_cBBool == sizeof(bool), "unexpected bool size");
		FormatCoz(pVm->m_pStrbuf, "%s", (*(bool*)pData) ? "true" : "false");
	} break;
    case TINK_Pointer:
	{
		auto pTinptr = (STypeInfoPointer *)pTin;
		if (*(u8**)pData == nullptr)
		{
			AppendCoz(pVm->m_pStrbuf, "null");
		}
		else
		{
			AppendCoz(pVm->m_pStrbuf, "&");
			PrintInstance(pVm, pTinptr->m_pTinPointedTo, *(u8**)pData);
		}

	} break;
    case TINK_Procedure:
	{
		auto str = StrFromTypeInfo(pTin);		
		AppendCoz(pVm->m_pStrbuf, str.PCoz());
	} break;
    case TINK_Struct:
	{
		auto pTinstruct = (STypeInfoStruct *)pTin;
		AppendCoz(pVm->m_pStrbuf, "{");
		for (int iTypememb = 0; iTypememb < pTinstruct->m_aryTypemembField.C(); ++iTypememb)
		{
			if (iTypememb > 0)
			{
				AppendCoz(pVm->m_pStrbuf, ", ");
			}

			auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];
			FormatCoz(pVm->m_pStrbuf, "`%s ", pTypememb->m_strName.PCoz());

			PrintInstance(pVm, pTypememb->m_pTin, (pData + pTypememb->m_dBOffset));
		}
		AppendCoz(pVm->m_pStrbuf, "}");
	} break;
	case TINK_Enum:
	{
		auto pTinenum = (STypeInfoEnum *)pTin;
		auto pTinintLoose = PTinRtiCast<STypeInfoInteger *>(pTinenum->m_pTinLoose);
		if (!EWC_FVERIFY(pTinintLoose, "expected enum loose type to be an integer"))
			break;

		const char * pCozEnumName = pTinenum->m_strName.PCoz();

		// could add code here to print by names, it's not clear that's worth it.
		switch(pTinenum->m_enumk)
		{
		case ENUMK_Basic:
			{
				if (pTinintLoose->m_fIsSigned)
				{
					switch (pTinintLoose->m_cBit)
					{
					case 8: FormatCoz(pVm->m_pStrbuf, "%s(%d)", pCozEnumName, *(s8*)pData);	break;
					case 16: FormatCoz(pVm->m_pStrbuf, "%s(%d)", pCozEnumName, *(s16*)pData); break;
					case 32: FormatCoz(pVm->m_pStrbuf, "%s(%d)", pCozEnumName, *(s32*)pData); break;
					case 64: FormatCoz(pVm->m_pStrbuf, "%s(%lld)", pCozEnumName, *(s64*)pData); break;
					default: EWC_ASSERT(false, "unexpected float size");
					}
				}
				else
				{
					switch (pTinintLoose->m_cBit)
					{
					case 8: FormatCoz(pVm->m_pStrbuf, "%s(%u)", pCozEnumName, *(u8*)pData);	break;
					case 16: FormatCoz(pVm->m_pStrbuf, "%s(%u)", pCozEnumName, *(u16*)pData); break;
					case 32: FormatCoz(pVm->m_pStrbuf, "%s(%u)", pCozEnumName, *(u32*)pData); break;
					case 64: FormatCoz(pVm->m_pStrbuf, "%s(%llu)", pCozEnumName, *(u64*)pData); break;
					default: EWC_ASSERT(false, "unexpected float size");
					}
				}
			} break;
		case ENUMK_FlagEnum:
			{
				switch (pTinintLoose->m_cBit)
				{
				case 8: FormatCoz(pVm->m_pStrbuf, "%s(0x%x)", pCozEnumName, *(u8*)pData);	break;
				case 16: FormatCoz(pVm->m_pStrbuf, "%s(0x%x)", pCozEnumName, *(u16*)pData); break;
				case 32: FormatCoz(pVm->m_pStrbuf, "%s(0x%x)", pCozEnumName, *(u32*)pData); break;
				case 64: FormatCoz(pVm->m_pStrbuf, "%s(0x%llx)", pCozEnumName, *(u64*)pData); break;
				default: EWC_ASSERT(false, "unexpected float size");
				}
			} break;
		}
	} break;
    case TINK_Array:
	{
		auto pTinary = (STypeInfoArray *)pTin;
		s64 c;
		u8 * pDataAdj = pData;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed:
			{
				FormatCoz(pVm->m_pStrbuf, "[%d]{", pTinary->m_c);
				c = pTinary->m_c;
			} break;
		case ARYK_Dynamic:
			{
				AppendCoz(pVm->m_pStrbuf, "[..]{");
					
			} break;
		case ARYK_Reference:
			{
				AppendCoz(pVm->m_pStrbuf, "[]{");
				c = *(s64 *)pData;
				pDataAdj = pData + sizeof(s64);
			} break;
		default: 
			EWC_ASSERT(false, "unhandled ARYK");
			break;
		}

		u64 cBElement;
		u64 cBAlignElement;
		CalculateByteSizeAndAlign(pVm->m_pDlay, pTinary->m_pTin, &cBElement, &cBAlignElement);
		size_t cBStride = EWC::CBAlign(cBElement, cBAlignElement);
		for (int i = 0; i < c; ++i)
		{
			PrintInstance(pVm, pTinary->m_pTin, pDataAdj);
			pDataAdj += cBStride;
		}
		AppendCoz(pVm->m_pStrbuf, "}");
	} break;
	case TINK_Qualifier:
	{	
		auto pTinqual = (STypeInfoQualifier *)pTin;
		PrintInstance(pVm, pTinqual->m_pTin, pData);
	} break;
    case TINK_Null:		AppendCoz(pVm->m_pStrbuf, "null"); break;
    case TINK_Void:		AppendCoz(pVm->m_pStrbuf, "void"); break;
    case TINK_Any:		AppendCoz(pVm->m_pStrbuf, "any(tbd)"); break;
	default:
		EWC_ASSERT(false, "unhandled type info kind");
		break;
	}
}

void PrintParameter(CVirtualMachine * pVm, STypeInfo * pTin, SParameter * pParam)
{
	PrintInstance(pVm, pTin, &pVm->m_pBStack[pParam->m_iBStack]);
}

SInstruction * PInstFindPhiIncoming(SInstruction ** ppInstPhi, s32 iInstSource)
{
	SInstruction * pInstIncoming = nullptr;
	auto pInst = *ppInstPhi;
	EWC_ASSERT(pInst->m_irop == IROP_Phi, "expected phi node.");

	do
	{
		if (pInst->m_wordRhs.m_s32 == iInstSource)
		{
			pInstIncoming = pInst;
			break;
		}
		++pInst;
	}
	while (pInst->m_irop == IROP_ExArgs);

	EWC_ASSERT(pInstIncoming, "didn't find incoming branch");
	return pInstIncoming;
}

inline DCstruct * PDcstructFromTinstruct(STypeInfoStruct * pTinstruct, SDataLayout * pDlay)
{
	auto pDcstruct = dcNewStruct(DCint(pTinstruct->m_aryTypemembField.C()), DCint(pTinstruct->m_cBAlign));

	auto pTypemembMac = pTinstruct->m_aryTypemembField.PMac();
	for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMac; ++pTypememb)
	{
		auto pTinMember = pTypememb->m_pTin;
		switch (pTinMember->m_tink)
		{
		case TINK_Bool:		dcStructField(pDcstruct, DC_SIGCHAR_BOOL, pDlay->m_cBBool, 0);			break;
		case TINK_Pointer:	dcStructField(pDcstruct, DC_SIGCHAR_POINTER, pDlay->m_cBPointer, 0);	break;
		case TINK_Integer:
		{
			auto pTinint = (STypeInfoInteger *)pTinMember;
			switch (pTinint->m_cBit)
			{
			case 8: dcStructField(pDcstruct, DC_SIGCHAR_CHAR, 1, 0);	break;
			case 16: dcStructField(pDcstruct, DC_SIGCHAR_SHORT, 2, 0);	break;
			case 32: dcStructField(pDcstruct, DC_SIGCHAR_INT, 4, 0);	break;
			case 64: dcStructField(pDcstruct, DC_SIGCHAR_LONGLONG, 8, 0);	break;
			default:
				EWC_ASSERT(false, "unhanded int size in dyncall struct member");
			}
		} break;
		case TINK_Float:
		{
			auto pTinfloat = (STypeInfoFloat *)pTinMember;
			switch (pTinfloat->m_cBit)
			{
			case 32: dcStructField(pDcstruct, DC_SIGCHAR_FLOAT, 4, 0);	break;
			case 64: dcStructField(pDcstruct, DC_SIGCHAR_DOUBLE, 8, 0);	break;
			default:
				EWC_ASSERT(false, "unhanded float size in dyncall struct member");
			}
		} break;
		default:
			EWC_ASSERT(false, "unhandled dyncall struct member type");
		}
	}

	dcCloseStruct(pDcstruct);
	return pDcstruct;
}

inline void SetupForeignProcParam(DCCallVM * pDcvm, SDataLayout * pDlay, STypeInfo * pTinParam, void * pVArg)
{
	switch (pTinParam->m_tink)
	{
	case TINK_Bool:			dcArgBool(pDcvm, *(bool*)pVArg);		break;
	case TINK_Pointer:		dcArgPointer(pDcvm, *(void**)pVArg);	break;
	case TINK_Procedure:	dcArgPointer(pDcvm, *(void**)pVArg);	break;
	case TINK_Integer:	
		{
			auto pTinintParam = (STypeInfoInteger*)pTinParam;
			switch (pTinintParam->m_cBit)
			{
			case 8:		dcArgChar(pDcvm, *(u8*)pVArg);			break;
			case 16:	dcArgShort(pDcvm, *(u16*)pVArg);		break;
			case 32:	dcArgInt(pDcvm, *(u32*)pVArg);			break;
			case 64:	dcArgLongLong(pDcvm, *(u64*)pVArg);		break;
			default: EWC_ASSERT(false, "unhandled size dyncall int");
			}
		} break;
	case TINK_Float:	
		{
			auto pTinfloatParam = (STypeInfoFloat*)pTinParam;
			switch (pTinfloatParam->m_cBit)
			{
			case 32:	dcArgFloat(pDcvm, *(f32*)pVArg);		break;
			case 64:	dcArgDouble(pDcvm, *(f64*)pVArg);		break;
			default: EWC_ASSERT(false, "unhandled size dyncall float");
			}
		} break;
	case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTinParam;
			auto pDcstruct = PDcstructFromTinstruct(pTinstruct, pDlay);

			dcArgStruct(pDcvm, pDcstruct, pVArg);
			dcFreeStruct(pDcstruct);
		} break;
	case TINK_Literal:
		{
			auto pTinlit = (STypeInfoLiteral *)pTinParam;
			switch (pTinlit->m_litty.m_litk)
			{
				case LITK_Integer:
					{
						switch (pTinlit->m_litty.m_cBit)
						{
						case 8:		dcArgChar(pDcvm, *(u8*)pVArg);			break;
						case 16:	dcArgShort(pDcvm, *(u16*)pVArg);		break;
						case 32:	dcArgInt(pDcvm, *(u32*)pVArg);			break;
						case 64:	dcArgLongLong(pDcvm, *(u64*)pVArg);		break;
						default: EWC_ASSERT(false, "unhandled size dyncall int literal");
						}
					} break;
				case LITK_Float:
					{
						switch (pTinlit->m_litty.m_cBit)
						{
						case 32:	dcArgFloat(pDcvm, *(f32*)pVArg);		break;
						case 64:	dcArgDouble(pDcvm, *(f64*)pVArg);		break;
						default: EWC_ASSERT(false, "unhandled size dyncall float literal");
						}
					} break;
				case LITK_Char:		dcArgChar(pDcvm, *(u8*)pVArg);			break;
				case LITK_String:	dcArgPointer(pDcvm, *(void**)pVArg);	break;
				case LITK_Bool:		dcArgBool(pDcvm, *(bool*)pVArg);		break;
				case LITK_Enum:
					EWC_ASSERT(false, "unhandled type kind in foreign function args");
					break;
				case LITK_Array:
					EWC_ASSERT(false, "unhandled type kind in foreign function args");
					break;
				case LITK_Null:		dcArgPointer(pDcvm, *(void**)pVArg);	break;
				case LITK_Pointer:	dcArgPointer(pDcvm, *(void**)pVArg);	break;
			}

		} break;
	default:
		EWC_ASSERT(false, "unhandled type kind in foreign function args");
	}
}

void CallForeignFunction(CVirtualMachine * pVm, void * pFnForeign, SProcedureSignature * pProcsig, u8 * pBStack, s32 cArgVariadic, s32 cBArgVariadic)
{
	u8 * pBArg = pBStack - (pProcsig->m_cBArgNamed + cBArgVariadic);

	if (!EWC_FVERIFY(pVm->m_pDcvm, "null DynCall VM"))
		return;

	//EWC_CASSERT(sizeof(DCbool) == sizeof(bool), "size mismatch");
	EWC_CASSERT(sizeof(DCchar) == sizeof(u8), "size mismatch");
	EWC_CASSERT(sizeof(DCshort) == sizeof(u16), "size mismatch");
	EWC_CASSERT(sizeof(DCint) == sizeof(u32), "size mismatch");
	EWC_CASSERT(sizeof(DClonglong) == sizeof(u64), "size mismatch");
	EWC_CASSERT(sizeof(DCfloat) == sizeof(f32), "size mismatch");
	EWC_CASSERT(sizeof(DCdouble) == sizeof(f64), "size mismatch");

	if (cArgVariadic)
	{
		dcMode(pVm->m_pDcvm, DC_CALL_C_ELLIPSIS );
	}

	auto pDcvm = pVm->m_pDcvm;
	auto pTinproc = pProcsig->m_pTinproc;
	size_t cParamNamed = pTinproc->m_arypTinParams.C();
	for (size_t iParam = 0; iParam < cParamNamed; ++iParam)
	{
		auto pTinParam = pTinproc->m_arypTinParams[iParam];
		auto pParam = &pProcsig->m_aParamArg[iParam];
		if (pTinParam->m_tink == TINK_Qualifier)
		{
			auto pTinqual = (STypeInfoQualifier *)pTinParam;
			pTinParam = pTinqual->m_pTin;
		}

		SetupForeignProcParam(pDcvm, pVm->m_pDlay, pTinParam, &pBArg[pParam->m_iBStack]);
	}

	if (cArgVariadic && EWC_FVERIFY(pProcsig->m_sIBStackVariadic >= 0, "variadic proc missing stack offset"))
	{
		if (cArgVariadic)
		{
			dcMode(pVm->m_pDcvm, DC_CALL_C_ELLIPSIS_VARARGS);
		}

		auto pBoxarg = (SBoxedArg*)&pBArg[pProcsig->m_sIBStackVariadic];
		for (size_t iParam = 0; iParam < cArgVariadic; ++iParam, ++pBoxarg)
		{
			SetupForeignProcParam(pDcvm, pVm->m_pDlay, pBoxarg->m_pTin, &pBoxarg->m_word);
		}

		if (cArgVariadic)
		{
			dcMode(pVm->m_pDcvm, DC_CALL_C_DEFAULT);
		}
	}

	auto cParamRet = pTinproc->m_arypTinReturns.C();
	EWC_ASSERT(cParamRet <= 1, "multiple returns not supported by foreign functions (yet)");
	u8 * pBReturn = nullptr;
	TINK tinkReturn = TINK_Void;
	if (cParamRet)
	{
		s32 iBReturn = *(s32*)&pBArg[pProcsig->m_aParamRet->m_iBStack];
		pBReturn = &pBStack[iBReturn];
		tinkReturn = pTinproc->m_arypTinReturns[0]->m_tink;
	}

	switch(tinkReturn)
	{
	case TINK_Void:		dcCallVoid(pDcvm, pFnForeign);							break;
	case TINK_Bool:		*(bool *)pBReturn = dcCallBool(pDcvm, pFnForeign) != 0;	break;
	case TINK_Pointer:	*(void **)pBReturn = dcCallPointer(pDcvm, pFnForeign);	break;
	case TINK_Integer:
	{
		auto pTinint = (STypeInfoInteger*)pTinproc->m_arypTinReturns[0];
		switch (pTinint->m_cBit)
		{
		case 8:		*(u8 *)pBReturn = dcCallChar(pDcvm, pFnForeign);			break;
		case 16:	*(u16 *)pBReturn = dcCallShort(pDcvm, pFnForeign);			break;
		case 32:	*(u32 *)pBReturn = dcCallInt(pDcvm, pFnForeign);			break;
		case 64:	*(u64 *)pBReturn = dcCallLongLong(pDcvm, pFnForeign);		break;
		default: EWC_ASSERT(false, "unhandled size dyncall return int");
		}
	} break;
	case TINK_Float:
	{
		auto pTinfloat = (STypeInfoFloat*)pTinproc->m_arypTinReturns[0];
		switch (pTinfloat->m_cBit)
		{
		case 32:	*(f32 *)pBReturn = dcCallFloat(pDcvm, pFnForeign);			break;
		case 64:	*(f64 *)pBReturn = dcCallDouble(pDcvm, pFnForeign);			break;
		default: EWC_ASSERT(false, "unhandled size dyncall return float");
		}
	} break;
	case TINK_Procedure:
	{
		*(void **)pBReturn = dcCallPointer(pDcvm, pFnForeign);
	} break;
	case TINK_Struct:
	default:
		EWC_ASSERT(false, "unhandled return type in foreign function");
	}

	dcReset(pVm->m_pDcvm);
}

void ExecuteBytecode(CVirtualMachine * pVm, SProcedure * pProcEntry)
{
#if DEBUG_PROC_CALL
	auto pDebcall = pVm->m_aryDebCall.AppendNew();
	pDebcall->m_ppInstCall = nullptr;
	pDebcall->m_pBStackSrc = pVm->m_pBStack;
	pDebcall->m_pBStackArg = pVm->m_pBStack - pProcEntry->m_pProcsig->m_cBArgNamed;
	pDebcall->m_pBStackDst = pVm->m_pBStack - (pProcEntry->m_pProcsig->m_cBArgNamed + pProcEntry->m_cBStack);
	pDebcall->m_pBReturnStorage = nullptr;
#endif

	static const int s_cBDynCallStack = 4096;
	EWC_ASSERT(pVm->m_pDcvm == nullptr, "expected null VM");	
	pVm->m_pDcvm = dcNewCallVM(s_cBDynCallStack);
	dcMode(pVm->m_pDcvm, DC_CALL_C_DEFAULT );

	pVm->m_pProcCurDebug = pProcEntry;

	// build the stack frame for our top level procedure
	// allocate space for the return type
	auto pProcsigEntry = pProcEntry->m_pProcsig;
	int cBReturn = 0;
	auto pTinprocEntry = pProcEntry->m_pProcsig->m_pTinproc;
	int * aiBReturn = (int *)alloca(sizeof(int) * pTinprocEntry->m_arypTinReturns.C());
	for (int ipTin = 0; ipTin < pTinprocEntry->m_arypTinReturns.C(); ++ipTin)
	{
		u64 cBReturn;
		u64 cBAlignReturn;
		CalculateByteSizeAndAlign(pVm->m_pDlay, pTinprocEntry->m_arypTinReturns[ipTin], &cBReturn, &cBAlignReturn);

		SParameter * pParam = &pProcsigEntry->m_aParamRet[ipTin];
		EWC_ASSERT(pParam->m_cB, "return type size mismatch");
		aiBReturn[ipTin] = S32Coerce(cBReturn);
		cBReturn += CBAlign(cBReturn, cBAlignReturn);
	}

	{
		auto pBStack = pVm->m_pBStack - cBReturn;

		pBStack -= pProcsigEntry->m_cBArgNamed;
		*(SInstruction **)(pBStack) = nullptr;	 // fill in the null pInstCall 

		// fill out our return locations
		for (int ipTin = 0; ipTin < pTinprocEntry->m_arypTinReturns.C(); ++ipTin)
		{
			SParameter * pParam = &pProcsigEntry->m_aParamRet[ipTin];
			*((s32*)&pBStack[pParam->m_iBStack]) = aiBReturn[ipTin];
		}

		pBStack -= pProcEntry->m_cBStack;

		if (pVm->m_pStrbuf)
		{
			AppendCoz(pVm->m_pStrbuf, "{");
		}

		pVm->m_pBStack = pBStack;
	}

	#define MASHOP(OP, CB)						(u32)(OP | (CB << 16))
	#define FETCH(IB, TYPE)						*(TYPE *)&pVm->m_pBStack[IB]
	#define STORE(IBOUT, TYPE, VALUE)			*(TYPE *)&pVm->m_pBStack[IBOUT] = VALUE

	SWord wordLhs, wordRhs, wordLhsEx;

	SInstruction * pInstMin = pProcEntry->m_aryInst.A();
	SInstruction * pInst = pInstMin;
	while (1)
	{
		switch (MASHOP(pInst->m_irop, pInst->m_cBRegister))
		{
		case MASHOP(IROP_Alloca, 4):
		case MASHOP(IROP_Alloca, 8):
		{
			u8 * pB = &pVm->m_pBStack[pInst->m_wordLhs.m_s32];
			*(u8 **)&pVm->m_pBStack[pInst->m_iBStackOut] = pB;

		} break;

		case MASHOP(IROP_Load, 0):
		{
			u8 * pBDst = &pVm->m_pBStack[pInst->m_iBStackOut];
			ReadOpcode(pVm, pInst, sizeof(u8*), &wordLhs); 
			memcpy(pBDst, wordLhs.m_pV, pInst->m_wordRhs.m_s32);
			break;
		}

		case MASHOP(IROP_NAdd, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 + wordRhs.m_u8);		break;
		case MASHOP(IROP_NAdd, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 + wordRhs.m_u16);	break;
		case MASHOP(IROP_NAdd, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 + wordRhs.m_u32);	break;
		case MASHOP(IROP_NAdd, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 + wordRhs.m_u64);	break;

		case MASHOP(IROP_NSub, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 - wordRhs.m_u8);		break;
		case MASHOP(IROP_NSub, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 - wordRhs.m_u16);	break;
		case MASHOP(IROP_NSub, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 - wordRhs.m_u32);	break;
		case MASHOP(IROP_NSub, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 - wordRhs.m_u64);	break;

		case MASHOP(IROP_NMul, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 * wordRhs.m_u8);		break;
		case MASHOP(IROP_NMul, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 * wordRhs.m_u16);	break;
		case MASHOP(IROP_NMul, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 * wordRhs.m_u32);	break;
		case MASHOP(IROP_NMul, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 * wordRhs.m_u64);	break;

		case MASHOP(IROP_UDiv, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 / wordRhs.m_u8);		break;
		case MASHOP(IROP_UDiv, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 / wordRhs.m_u16);	break;
		case MASHOP(IROP_UDiv, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 / wordRhs.m_u32);	break;
		case MASHOP(IROP_UDiv, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_s64 / wordRhs.m_s64);	break;

		case MASHOP(IROP_SDiv, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_s8 / wordRhs.m_s8);		break;
		case MASHOP(IROP_SDiv, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_s16 / wordRhs.m_s16);	break;
		case MASHOP(IROP_SDiv, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_s32 / wordRhs.m_s32); 	break;
		case MASHOP(IROP_SDiv, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 / wordRhs.m_u64);	break;

		case MASHOP(IROP_URem, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 % wordRhs.m_u8);		break;
		case MASHOP(IROP_URem, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 % wordRhs.m_u16);	break;
		case MASHOP(IROP_URem, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 % wordRhs.m_u32);	break;
		case MASHOP(IROP_URem, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_s64 % wordRhs.m_s64);	break;

		case MASHOP(IROP_SRem, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_s8 % wordRhs.m_s8);		break;
		case MASHOP(IROP_SRem, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_s16 % wordRhs.m_s16);	break;
		case MASHOP(IROP_SRem, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_s32 % wordRhs.m_s32); 	break;
		case MASHOP(IROP_SRem, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 % wordRhs.m_u64);	break;

		case MASHOP(IROP_GAdd, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f32, wordLhs.m_f32 + wordRhs.m_f32);	break;
		case MASHOP(IROP_GAdd, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f64, wordLhs.m_f64 + wordRhs.m_f64);	break;

		case MASHOP(IROP_GSub, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f32, wordLhs.m_f32 - wordRhs.m_f32);	break;
		case MASHOP(IROP_GSub, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f64, wordLhs.m_f64 - wordRhs.m_f64);	break;

		case MASHOP(IROP_GMul, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f32, wordLhs.m_f32 * wordRhs.m_f32);	break;
		case MASHOP(IROP_GMul, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f64, wordLhs.m_f64 * wordRhs.m_f64);	break;

		case MASHOP(IROP_GDiv, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f32, wordLhs.m_f32 / wordRhs.m_f32);	break;
		case MASHOP(IROP_GDiv, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f64, wordLhs.m_f64 / wordRhs.m_f64);	break;

		case MASHOP(IROP_GRem, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f32, fmodf(wordLhs.m_f32, wordRhs.m_f32));	break;
		case MASHOP(IROP_GRem, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, f64, fmod(wordLhs.m_f64, wordRhs.m_f64));	break;

		case MASHOP(IROP_Shl, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 << wordRhs.m_u8);	break;
		case MASHOP(IROP_Shl, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 << wordRhs.m_u16);	break;
		case MASHOP(IROP_Shl, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 << wordRhs.m_u32);	break;
		case MASHOP(IROP_Shl, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 << wordRhs.m_u64);	break;

		case MASHOP(IROP_LShr, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u8, wordLhs.m_u8 >> wordRhs.m_u8);	break;
		case MASHOP(IROP_LShr, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u16, wordLhs.m_u16 >> wordRhs.m_u16);	break;
		case MASHOP(IROP_LShr, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u32, wordLhs.m_u32 >> wordRhs.m_u32);	break;
		case MASHOP(IROP_LShr, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, u64, wordLhs.m_u64 >> wordRhs.m_u64);	break;

		case MASHOP(IROP_AShr, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s8, wordLhs.m_s8 >> wordRhs.m_s8);	break;
		case MASHOP(IROP_AShr, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s16, wordLhs.m_s16 >> wordRhs.m_s16);	break;
		case MASHOP(IROP_AShr, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s32, wordLhs.m_s32 >> wordRhs.m_s32);	break;
		case MASHOP(IROP_AShr, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s64, wordLhs.m_s64 >> wordRhs.m_s64);	break;

		case MASHOP(IROP_And, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s8, wordLhs.m_s8 & wordRhs.m_s8);	break;
		case MASHOP(IROP_And, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s16, wordLhs.m_s16 & wordRhs.m_s16);	break;
		case MASHOP(IROP_And, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s32, wordLhs.m_s32 & wordRhs.m_s32);	break;
		case MASHOP(IROP_And, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s64, wordLhs.m_s64 & wordRhs.m_s64);	break;

		case MASHOP(IROP_Or, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s8, wordLhs.m_s8 | wordRhs.m_s8);	break;
		case MASHOP(IROP_Or, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s16, wordLhs.m_s16 | wordRhs.m_s16);	break;
		case MASHOP(IROP_Or, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s32, wordLhs.m_s32 | wordRhs.m_s32);	break;
		case MASHOP(IROP_Or, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s64, wordLhs.m_s64 | wordRhs.m_s64);	break;

		case MASHOP(IROP_Xor, 1): ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s8, wordLhs.m_s8 ^ wordRhs.m_s8);	break;
		case MASHOP(IROP_Xor, 2): ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s16, wordLhs.m_s16 ^ wordRhs.m_s16);	break;
		case MASHOP(IROP_Xor, 4): ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s32, wordLhs.m_s32 ^ wordRhs.m_s32);	break;
		case MASHOP(IROP_Xor, 8): ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs); STORE(pInst->m_iBStackOut, s64, wordLhs.m_s64 ^ wordRhs.m_s64);	break;

		case MASHOP(IROP_NNeg, 1): ReadOpcode(pVm, pInst, 1, &wordLhs); STORE(pInst->m_iBStackOut, s8, -wordLhs.m_s8);		break;
		case MASHOP(IROP_NNeg, 2): ReadOpcode(pVm, pInst, 2, &wordLhs); STORE(pInst->m_iBStackOut, s16, -wordLhs.m_s16);	break;
		case MASHOP(IROP_NNeg, 4): ReadOpcode(pVm, pInst, 4, &wordLhs); STORE(pInst->m_iBStackOut, s32, -wordLhs.m_s32);	break;
		case MASHOP(IROP_NNeg, 8): ReadOpcode(pVm, pInst, 8, &wordLhs); STORE(pInst->m_iBStackOut, s64, -wordLhs.m_s64);	break;

		case MASHOP(IROP_GNeg, 4): ReadOpcode(pVm, pInst, 4, &wordLhs); STORE(pInst->m_iBStackOut, f32, -wordLhs.m_f32);	break;
		case MASHOP(IROP_GNeg, 8): ReadOpcode(pVm, pInst, 8, &wordLhs); STORE(pInst->m_iBStackOut, f64, -wordLhs.m_f64);	break;

		case MASHOP(IROP_Not, 1): ReadOpcode(pVm, pInst, 1, &wordLhs); STORE(pInst->m_iBStackOut, u8, !wordLhs.m_s8);	break;
		case MASHOP(IROP_Not, 2): ReadOpcode(pVm, pInst, 2, &wordLhs); STORE(pInst->m_iBStackOut, u16, !wordLhs.m_s16);	break;
		case MASHOP(IROP_Not, 4): ReadOpcode(pVm, pInst, 4, &wordLhs); STORE(pInst->m_iBStackOut, u32, !wordLhs.m_s32);	break;
		case MASHOP(IROP_Not, 8): ReadOpcode(pVm, pInst, 8, &wordLhs); STORE(pInst->m_iBStackOut, u64, !wordLhs.m_s64);	break;
#define STORE_CAST(TYPE, SIGN) \
			ReadCastOpcodes(pVm, pInst, &wordLhs, &wordRhs); \
			switch(wordRhs.m_s32) \
			{\
			case 1: STORE(pInst->m_iBStackOut, TYPE, TYPE(wordLhs.m_##SIGN##8));	break; \
			case 2: STORE(pInst->m_iBStackOut, TYPE, TYPE(wordLhs.m_##SIGN##16));	break; \
			case 4: STORE(pInst->m_iBStackOut, TYPE, TYPE(wordLhs.m_##SIGN##32));	break; \
			case 8: STORE(pInst->m_iBStackOut, TYPE, TYPE(wordLhs.m_##SIGN##64));	break; \
			} 

		case MASHOP(IROP_Bitcast, 1): STORE_CAST(u8, u)		break;
		case MASHOP(IROP_Bitcast, 2): STORE_CAST(u16, u)	break;
		case MASHOP(IROP_Bitcast, 4): STORE_CAST(u32, u)	break;
		case MASHOP(IROP_Bitcast, 8): STORE_CAST(u64, u)	break;

		case MASHOP(IROP_NTrunc, 1): STORE_CAST(u8, u)		break;
		case MASHOP(IROP_NTrunc, 2): STORE_CAST(u16, u)		break;
		case MASHOP(IROP_NTrunc, 4): STORE_CAST(u32, u)		break;
		case MASHOP(IROP_NTrunc, 8): STORE_CAST(u64, u)		break;

		case MASHOP(IROP_ZeroExt, 1): STORE_CAST(u8, u)		break;
		case MASHOP(IROP_ZeroExt, 2): STORE_CAST(u16, u)	break;
		case MASHOP(IROP_ZeroExt, 4): STORE_CAST(u32, u)	break;
		case MASHOP(IROP_ZeroExt, 8): STORE_CAST(u64, u)	break;

		case MASHOP(IROP_SignExt, 1): STORE_CAST(s8, s)		break;
		case MASHOP(IROP_SignExt, 2): STORE_CAST(s16, s)	break;
		case MASHOP(IROP_SignExt, 4): STORE_CAST(s32, s)	break;
		case MASHOP(IROP_SignExt, 8): STORE_CAST(s64, s)	break;

#define STORE_F2INT(TYPE) \
			ReadCastOpcodes(pVm, pInst, &wordLhs, &wordRhs); \
			switch(wordRhs.m_s32) \
			{\
			case 4: STORE(pInst->m_iBStackOut, TYPE, TYPE(wordLhs.m_f32));	break; \
			case 8: STORE(pInst->m_iBStackOut, TYPE, TYPE(wordLhs.m_f64));	break; \
			} 

		case MASHOP(IROP_GToS, 1): STORE_F2INT(s8)	break;
		case MASHOP(IROP_GToS, 2): STORE_F2INT(s16)	break;
		case MASHOP(IROP_GToS, 4): STORE_F2INT(s32)	break;
		case MASHOP(IROP_GToS, 8): STORE_F2INT(s64)	break;

		case MASHOP(IROP_GToU, 1): STORE_F2INT(u8)	break;
		case MASHOP(IROP_GToU, 2): STORE_F2INT(u16)	break;
		case MASHOP(IROP_GToU, 4): STORE_F2INT(u32)	break;
		case MASHOP(IROP_GToU, 8): STORE_F2INT(u64)	break;

		case MASHOP(IROP_SToG, 4): STORE_CAST(f32, s)	break;
		case MASHOP(IROP_SToG, 8): STORE_CAST(f64, s)	break;

		case MASHOP(IROP_UToG, 4): STORE_CAST(f32, u)	break;
		case MASHOP(IROP_UToG, 8): STORE_CAST(f64, u)	break;

		case MASHOP(IROP_GTrunc, 4):
		{
			ReadCastOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			EWC_ASSERT(wordRhs.m_s64 == 8, "unexpected float truncate source size");
			STORE(pInst->m_iBStackOut, f32, f32(wordLhs.m_f64));
		} break;
		case MASHOP(IROP_GExtend, 8):
		{
			ReadCastOpcodes(pVm, pInst, &wordLhs, &wordRhs);
			EWC_ASSERT(wordRhs.m_s64 == 4, "unexpected float extend source size");
			STORE(pInst->m_iBStackOut, f64, f64(wordLhs.m_f32));
		} break;

#define STORE_PCAST(TYPE, SIGN) \
			ReadCastOpcodes(pVm, pInst, &wordLhs, &wordRhs); \
			switch(wordRhs.m_s32) \
			{\
			case 1: STORE(pInst->m_iBStackOut, TYPE, (TYPE)uintptr_t(wordLhs.m_##SIGN##8));		break; \
			case 2: STORE(pInst->m_iBStackOut, TYPE, (TYPE)uintptr_t(wordLhs.m_##SIGN##16));	break; \
			case 4: STORE(pInst->m_iBStackOut, TYPE, (TYPE)uintptr_t(wordLhs.m_##SIGN##32));	break; \
			case 8: STORE(pInst->m_iBStackOut, TYPE, (TYPE)uintptr_t(wordLhs.m_##SIGN##64));	break; \
			} 

#define STORE_PTRTOINT(TYPE) \
			ReadCastOpcodes(pVm, pInst, &wordLhs, &wordRhs); \
			STORE(pInst->m_iBStackOut, TYPE, (TYPE)reinterpret_cast<uintptr_t>(wordLhs.m_pV));

		case MASHOP(IROP_IntToPtr, 4): STORE_PCAST(void*, u)	break;
		case MASHOP(IROP_IntToPtr, 8): STORE_PCAST(void*, u)	break;

		case MASHOP(IROP_PtrToInt, 1): STORE_PTRTOINT(u8)	break;
		case MASHOP(IROP_PtrToInt, 2): STORE_PTRTOINT(u16)	break;
		case MASHOP(IROP_PtrToInt, 4): STORE_PTRTOINT(u32)	break;
		case MASHOP(IROP_PtrToInt, 8): STORE_PTRTOINT(u64)	break;

		case MASHOP(IROP_TraceStore, 0):
		case MASHOP(IROP_TraceStore, 1):
		case MASHOP(IROP_TraceStore, 2):
		case MASHOP(IROP_TraceStore, 4):
		case MASHOP(IROP_TraceStore, 8):
		{
			if (pVm->m_pStrbuf)
			{
				{
					auto pTin = (STypeInfo *)pInst->m_wordRhs.m_pV;
					u64 cB;
					u64 cBAlign;
					CalculateByteSizeAndAlign(pVm->m_pDlay, pTin, &cB, &cBAlign);

					ReadOpcode(pVm, pInst, int(cB), &wordLhs);
					PrintInstance(pVm, pTin, (u8*)&wordLhs);
					AppendCoz(pVm->m_pStrbuf, ";");
				}
			}
		} break;

		//Store: Copy cBytes from src to the address at idxDst;			*pBStack[idxDst] = val
		case MASHOP(IROP_Store, 0):
		{
			u8 * pBDst = *(u8**)&pVm->m_pBStack[pInst->m_iBStackOut];
			auto pVLhs = PVReadAddressLhs(pVm, pInst, &wordLhs);

			EWC_ASSERT(pInst->m_wordRhs.m_s32 > 0, "trying to copy %d bytes.", pInst->m_wordRhs.m_s32);
			memcpy(pBDst, pVLhs, pInst->m_wordRhs.m_s32);

		} break;
		// StoreToReg: Copy cBytes from src to index dest;				pBStack[idxDst] = val
		case MASHOP(IROP_StoreToReg, 0):
			{
				auto pVSrc = PVReadAddressLhs(pVm, pInst, &wordLhs);
				auto pBDst = (u8*)&pVm->m_pBStack[pInst->m_iBStackOut];

				EWC_ASSERT(pInst->m_wordRhs.m_s32 > 0, "trying to copy %d bytes.", pInst->m_wordRhs.m_s32);
				memcpy(pBDst, pVSrc, pInst->m_wordRhs.m_s32);
			} break;

		//StoreToIdx: Copy cBytes to index specified at index dest;		pBStack[pBStack[idxDst]] = val
		case MASHOP(IROP_StoreToIdx, 0):
		{
			auto pVSrc = PVReadAddressLhs(pVm, pInst, &wordLhs);
			auto idx = *(s32*)&pVm->m_pBStack[pInst->m_iBStackOut];

			EWC_ASSERT(pInst->m_wordRhs.m_s32 > 0, "trying to copy %d bytes.", pInst->m_wordRhs.m_s32);
			memcpy(&pVm->m_pBStack[idx], pVSrc, pInst->m_wordRhs.m_s32);
		} break;

		// Store value to virtual register (pV, value)
		case MASHOP(IROP_StoreAddress, 4):
			{
				EWC_ASSERT(pVm->m_pDlay->m_cBPointer == 4, "storing wrong pointer size");
				void * pV = PVReadAddressLhs(pVm, pInst, &wordLhs);
				*(u32*)&pVm->m_pBStack[pInst->m_iBStackOut] = (u32)uintptr_t(pV);
			} break;
		case MASHOP(IROP_StoreAddress, 8):
			{
				EWC_ASSERT(pVm->m_pDlay->m_cBPointer == 8, "storing wrong pointer size");
				void * pV = PVReadAddressLhs(pVm, pInst, &wordLhs);
				*(u64*)&pVm->m_pBStack[pInst->m_iBStackOut] = (u64)uintptr_t(pV);
			} break;

		case MASHOP(IROP_NCmp, 1):
		{
			ReadOpcodes(pVm, pInst, 1, &wordLhs, &wordRhs);
			bool fEval = FEvaluateNCmp<1>((NPRED)pInst->m_pred, wordLhs, wordRhs);
			STORE(pInst->m_iBStackOut, u8, fEval);
		} break;
		case MASHOP(IROP_NCmp, 2):
			ReadOpcodes(pVm, pInst, 2, &wordLhs, &wordRhs);
			STORE(pInst->m_iBStackOut, u16, FEvaluateNCmp<2>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(IROP_NCmp, 4):
			ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs);
			STORE(pInst->m_iBStackOut, u32, FEvaluateNCmp<4>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(IROP_NCmp, 8):
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs);
			STORE(pInst->m_iBStackOut, u64, FEvaluateNCmp<8>((NPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(IROP_GCmp, 4):
			ReadOpcodes(pVm, pInst, 4, &wordLhs, &wordRhs);
			STORE(pInst->m_iBStackOut, u32, FEvaluateGCmp<4>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;
		case MASHOP(IROP_GCmp, 8):
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs);
			STORE(pInst->m_iBStackOut, u64, FEvaluateGCmp<8>((GPRED)pInst->m_pred, wordLhs, wordRhs));
			break;

		case MASHOP(IROP_Call, 0):
		case MASHOP(IROP_Call, 1):
		case MASHOP(IROP_Call, 2):
		case MASHOP(IROP_Call, 4):
		case MASHOP(IROP_Call, 8):
		{
			ReadOpcodes(pVm, pInst, sizeof(SProcedure *), &wordLhs, &wordRhs);

			auto pProcsig = (SProcedureSignature*)wordRhs.m_pV;

			s32 cArgVariadic = 0;
			s32 cBArgVariadic = 0;
			auto pInstEx = (pInst + 1);
			if (pInstEx->m_irop == IROP_ExArgs)
			{
				cArgVariadic = pInstEx->m_wordLhs.m_s32;
				cBArgVariadic = pInstEx->m_wordRhs.m_s32;
			}

			auto pTinproc = pProcsig->m_pTinproc;
			if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				CallForeignFunction(pVm, wordLhs.m_pV, pProcsig, pVm->m_pBStack, cArgVariadic, cBArgVariadic);

				if (cArgVariadic)
					++pInst;
				break;
			}

			auto pProc = (SProcedure *)wordLhs.m_pV;
			SInstruction ** ppInstRet = (SInstruction **)(pVm->m_pBStack - (pProcsig->m_cBArgNamed + cBArgVariadic));

#if DEBUG_PROC_CALL
			auto pDebcall = pVm->m_aryDebCall.AppendNew();
			pDebcall->m_ppInstCall = ppInstRet;
			pDebcall->m_pBStackSrc = pVm->m_pBStack;
			pDebcall->m_pBStackArg = pVm->m_pBStack - (cBArgVariadic + pProcsig->m_cBArgNamed);
			pDebcall->m_pBStackDst = pVm->m_pBStack - (cBArgVariadic + pProcsig->m_cBArgNamed + pProc->m_cBStack);
			pDebcall->m_pBReturnStorage = &pVm->m_pBStack[pInst->m_iBStackOut];
#endif 
			pVm->m_pBStack -= (pProcsig->m_cBArgNamed + cBArgVariadic);
			if (pVm->m_pStrbuf)
			{
				STypeInfoProcedure * pTinproc = pProcsig->m_pTinproc;
				FormatCoz(pVm->m_pStrbuf, "%s(", pTinproc->m_strName.PCoz());

				// don't print the parameters to initializer procs, it's uninitialized memory.
				if (pTinproc->m_grftinproc.FIsSet(FTINPROC_Initializer) == false)
				{
					for (int iParam = 0; iParam < pTinproc->m_arypTinParams.C(); ++iParam)
					{

						if (iParam > 0)
							AppendCoz(pVm->m_pStrbuf, ", ");

						SParameter * pParam = &pProcsig->m_aParamArg[iParam];
						PrintParameter(pVm, pTinproc->m_arypTinParams[iParam], pParam);
					}
				}
				AppendCoz(pVm->m_pStrbuf, "){");
			}

			pVm->m_pBStack -= pProc->m_cBStack;
			EWC_ASSERT((uintptr_t(pVm->m_pBStack) & (pVm->m_pDlay->m_cBStackAlign - 1)) == 0,
				"stack frame should be %d byte aligned.", pVm->m_pDlay->m_cBStackAlign);
			EWC_ASSERT(uintptr_t(pVm->m_pBStack) >= uintptr_t(pVm->m_pBStackMin), "stack overflow");

			//printf("IROP_Call) pBStack = %p, ppInst = %p, cBStack = %lld, cBArg = %lld\n", pVm->m_pBStack, ppInstRet, pProc->m_cBStack, pProc->m_cBArg);

			*ppInstRet = pInst;
			*((SProcedure **)(ppInstRet + 1)) = pVm->m_pProcCurDebug;

			pInstMin = pProc->m_aryInst.A();
			pInst = pInstMin - 1; // this will be incremented below
			pVm->m_pProcCurDebug = pProc;

		} break;

		case MASHOP(IROP_Ret, 0):
		{
			u8 * pBStackCalled = pVm->m_pBStack;

			s32 cBStack = pInst->m_wordRhs.m_s32;
			SInstruction ** ppInstRet = (SInstruction **)(pVm->m_pBStack + cBStack);

			auto pBStack = pVm->m_pBStack;
			pVm->m_pBStack += pInst->m_wordLhs.m_s32; // cBArgNamed + cBStack

			auto pProcsigCur = pVm->m_pProcCurDebug->m_pProcsig;
			if (pProcsigCur->m_sIBStackVariadic >= 0)
			{
				if (EWC_FVERIFY(*ppInstRet, "variadic entry proc not suppported"))
				{
					auto pInstEx = *ppInstRet + 1;
					if(EWC_FVERIFY(pInstEx->m_irop == IROP_ExArgs, "variadic proc call with no exArgs"))
					{
						pVm->m_pBStack += pInstEx->m_wordRhs.m_s32;
					}
				}
			}

			auto pProcCalled = pVm->m_pProcCurDebug;
			auto pProcsigCalled = pProcCalled->m_pProcsig;
#if DEBUG_PROC_CALL
			auto debcall = pVm->m_aryDebCall.TPopLast();
			EWC_ASSERT(debcall.m_pBStackSrc == pVm->m_pBStack, "source proc stack frame mismatch");
			EWC_ASSERT(*ppInstRet == nullptr || debcall.m_ppInstCall == ppInstRet, "bad return instruction");
			EWC_ASSERT(debcall.m_pBStackDst == pBStackCalled, "called proc stack frame mismatch");
			EWC_ASSERT(debcall.m_pBStackArg == pBStackCalled + pProcCalled->m_cBStack, "called proc argument stack frame mismatch");
#endif

			if (pVm->m_pStrbuf && EWC_FVERIFY(pProcCalled, "missing called proc"))
			{
				u8 * pBStackArg = pBStackCalled + pProcCalled->m_cBStack;
				auto pTinproc = pProcsigCalled->m_pTinproc;

				AppendCoz(pVm->m_pStrbuf, "}");
				if (pTinproc->m_arypTinReturns.C())
				{
					for (int ipTin = 0; ipTin < pTinproc->m_arypTinReturns.C(); ++ipTin)
					{
						SParameter * pParam = &pProcsigCalled->m_aParamRet[ipTin];
						if (pParam->m_cB == 0)
							continue;

						auto iBStackRet = *(s32*)&pBStackArg[pParam->m_iBStack];

						if (ipTin == 0)
						{
#if DEBUG_PROC_CALL
							EWC_ASSERT(debcall.m_pBReturnStorage == &pVm->m_pBStack[iBStackRet], "bad return storage calculation");
#endif
							AppendCoz(pVm->m_pStrbuf, "->");
						}
						PrintInstance(pVm, pTinproc->m_arypTinReturns[ipTin], &pVm->m_pBStack[iBStackRet]);
					}
				}

				if (*ppInstRet != nullptr)
				{
					AppendCoz(pVm->m_pStrbuf, "; ");
				}
			}

			if (*ppInstRet == nullptr)
			{
				return; // halt
			}

			auto pInstCall = *ppInstRet;
			EWC_ASSERT(pInstCall->m_irop == IROP_Call, "procedure return did not return to call instruction");

			// skip exArgs (if this proc is variadic)
			if ((pInstCall + 1)->m_irop == IROP_ExArgs)
				++pInstCall;

			auto pProcPrev = *((SProcedure **)(ppInstRet + 1));

			// NOTE: This is the calling procedure, NOT the procedure being called (ie pInstCall->mm_wordLhs.m_pV)
			pVm->m_pProcCurDebug = pProcPrev;
			pInstMin = pProcPrev->m_aryInst.A();
			pInst = pInstCall;

		} break;

		case MASHOP(IROP_CondBranch, 0):
		{
			ReadOpcode(pVm, pInst, 1, &wordLhs);
			EWC_ASSERT(wordLhs.m_u8 >= 0 && wordLhs.m_u8 <= 1, "expected 0 or 1");

			u8 iOp = (wordLhs.m_u8 != 0);
			s32 * pIInst = (s32*)&pInst->m_wordRhs;
			s32 iInst = pIInst[iOp];

			pVm->m_iInstSource = s32(pInst - pInstMin);
			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;
		case MASHOP(IROP_Branch, 0):
		{
			s32 iInst = pInst->m_wordRhs.m_s32;

			pVm->m_iInstSource = s32(pInst - pInstMin);
			pInst = &pInstMin[iInst - 1]; // -1 because it is incremented below
		} break;

#define EXEC_SWITCH(CB, VAR)	\
			{					\
				ReadOpcode(pVm, pInst, CB, &wordLhs);			\
				s32 iInst = pInst->m_wordRhs.m_s32;				\
																\
				while ((pInst + 1)->m_irop == IROP_ExArgs)		\
				{												\
					++pInst;									\
					ReadOpcode(pVm, pInst, CB, &wordLhsEx);		\
					if (wordLhs.m_##VAR == wordLhsEx.m_##VAR)	\
						iInst = pInst->m_wordRhs.m_s32;			\
				}												\
				pVm->m_iInstSource = s32(pInst - pInstMin);							\
				pInst = &pInstMin[iInst - 1]; /* -1 because it is incremented below */\
			}

		case MASHOP(IROP_Switch, 1): EXEC_SWITCH(1, u8)		break;
		case MASHOP(IROP_Switch, 2): EXEC_SWITCH(2, u16)	break;
		case MASHOP(IROP_Switch, 4): EXEC_SWITCH(4, u32)	break;
		case MASHOP(IROP_Switch, 8): EXEC_SWITCH(8, u64)	break;

		case MASHOP(IROP_Phi, 1):
		{
			auto pInstPhi = pInst;
			SInstruction * pInstIncoming = PInstFindPhiIncoming(&pInst, pVm->m_iInstSource);
			if (EWC_FVERIFY(pInstIncoming, "couldn't find phi node incoming for source block"))
			{
				ReadOpcode(pVm, pInstIncoming, 1, &wordLhs);
				STORE(pInstPhi->m_iBStackOut, u8, wordLhs.m_u8); 
			}

			while ((pInst + 1)->m_irop == IROP_ExArgs)
			{
				++pInst;
			}
		} break;
		case MASHOP(IROP_Phi, 2):
		{
			auto pInstPhi = pInst;
			SInstruction * pInstIncoming = PInstFindPhiIncoming(&pInst, pVm->m_iInstSource);
			if (EWC_FVERIFY(pInstIncoming, "couldn't find phi node incoming for source block"))
			{
				ReadOpcode(pVm, pInstIncoming, 2, &wordLhs);
				STORE(pInstPhi->m_iBStackOut, u16, wordLhs.m_u16); 
			}

			while ((pInst + 1)->m_irop == IROP_ExArgs)
			{
				++pInst;
			}
		} break;
		case MASHOP(IROP_Phi, 4):
		{
			auto pInstPhi = pInst;
			SInstruction * pInstIncoming = PInstFindPhiIncoming(&pInst, pVm->m_iInstSource);
			if (EWC_FVERIFY(pInstIncoming, "couldn't find phi node incoming for source block"))
			{
				ReadOpcode(pVm, pInstIncoming, 4, &wordLhs);
				STORE(pInstPhi->m_iBStackOut, u32, wordLhs.m_u32); 
			}

			while ((pInst + 1)->m_irop == IROP_ExArgs)
			{
				++pInst;
			}
		} break;
		case MASHOP(IROP_Phi, 8):
		{
			auto pInstPhi = pInst;
			SInstruction * pInstIncoming = PInstFindPhiIncoming(&pInst, pVm->m_iInstSource);
			if (EWC_FVERIFY(pInstIncoming, "couldn't find phi node incoming for source block"))
			{
				ReadOpcode(pVm, pInstIncoming, 8, &wordLhs);
				STORE(pInstPhi->m_iBStackOut, u64, wordLhs.m_u64); 
			}

			while ((pInst + 1)->m_irop == IROP_ExArgs)
			{
				++pInst;
			}
		} break;
		case MASHOP(IROP_Memset, 0):
		{
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs);
			++pInst;
			if (!EWC_FVERIFY(pInst->m_irop == IROP_ExArgs, "expected extended argument block"))
				break;
			ReadOpcode(pVm, pInst, 8, &wordLhsEx);
			memset(wordLhs.m_pV, wordRhs.m_u8, wordLhsEx.m_u64);
		} break;
		case MASHOP(IROP_Memcpy, 0):
		{
			ReadOpcodes(pVm, pInst, 8, &wordLhs, &wordRhs);
			++pInst;
			if (!EWC_FVERIFY(pInst->m_irop == IROP_ExArgs, "expected extended argument block"))
				break;
			ReadOpcode(pVm, pInst, 8, &wordLhsEx);
			memcpy(wordLhs.m_pV, wordRhs.m_pV, wordLhsEx.m_u64);
		} break;
		case MASHOP(IROP_GEP, 4):
		case MASHOP(IROP_GEP, 8):
		{
			auto pInstGep = pInst;
			ReadOpcode(pVm, pInst, sizeof(u8*), &wordLhs); 

			u64 dB = pInst->m_wordRhs.m_u64;
			while ((pInst + 1)->m_irop == IROP_ExArgs)
			{
				++pInst;
				ReadOpcode(pVm, pInst, pInst->m_cBRegister, &wordLhsEx); 
				switch(pInst->m_cBRegister)
				{
				case 1: dB += wordLhsEx.m_s8 * pInst->m_wordRhs.m_s64;		break;
				case 2: dB += wordLhsEx.m_s16 * pInst->m_wordRhs.m_s64;		break;
				case 4: dB += wordLhsEx.m_s32 * pInst->m_wordRhs.m_s64;		break;
				case 8: dB += wordLhsEx.m_s64 * pInst->m_wordRhs.m_s64;		break;
				}
			}

			*(u8 **)&pVm->m_pBStack[pInstGep->m_iBStackOut] = (u8*)wordLhs.m_pV + dB;
		} break;
		default:

			EWC_ASSERT(false, "unhandled opcode IROP_%s %d\n", PChzFromIrop(pInst->m_irop), pInst->m_cBRegister);
		}
		++pInst;
	}

	#undef MASHOP
	#undef FETCH
	#undef MASHOP

	dcFree(pVm->m_pDcvm);
	pVm->m_pDcvm = nullptr;
}

void CBuilder::SwapToVm(CVirtualMachine * pVm)
{
	pVm->m_arypBlockManaged.Swap(&m_arypBlockManaged);
	pVm->m_arypProcManaged.Swap(&m_arypProcManaged);
	pVm->m_hashHvMangledPProc.Swap(&m_hashHvMangledPProc);
	pVm->m_hashPTinprocPProcsig.Swap(&m_hashPTinprocPProcsig);
	Clear();
}

SProcedure * PProcLookup(CVirtualMachine * pVm, HV hv)
{
	SProcedure ** ppProc = pVm->m_hashHvMangledPProc.Lookup(hv);
	if (!ppProc)
		return nullptr;

	return *ppProc;
}

void UnloadForeignLibraries(CDynAry<void *> * paryDll)
{
	auto pDllMac = paryDll->PMac();
	for (auto pDllIt = paryDll->A(); pDllIt != pDllMac; ++pDllIt)
	{
		DLLib * pDll = (DLLib *)pDllIt;
		dlFreeLibrary(pDll);
	}

	paryDll->Clear();
}

bool LoadForeignLibraries(CWorkspace * pWork, CHash<HV, void*> * pHashHvPFn, CDynAry<void *> * parypDll)
{
	char aCozWorking[2048];
	bool fLoadError = false;  
	static const char * s_pChzLibraryDirDebug = "..\\x64\\DebugDLL";
	static const char * s_pChzLibraryDirRelease = "..\\x64\\ReleaseDLL";
#if EWC_X64
	static const char * s_pChzCrtLibraryDir = "C:\\Program Files (x86)\\Windows Kits\\10\\Redist\\ucrt\\DLLs\\x64";
#else
	static const char * s_pChzCrtLibraryDir = "C:\\Program Files (x86)\\Windows Kits\\10\\Redist\\ucrt\\DLLs\\x64";
#endif

	CFileSearch filser(pWork->m_pAlloc);
	filser.AddDirectory((pWork->m_optlevel == OPTLEVEL_Release) ? s_pChzLibraryDirRelease  : s_pChzLibraryDirDebug);
	filser.AddDirectory(s_pChzCrtLibraryDir);
	filser.AddDirectory("C:\\Windows\\System32");

	CWorkspace::SFile ** ppFileMac = pWork->m_arypFile.PMac();
	for (CWorkspace::SFile ** ppFile = pWork->m_arypFile.A(); ppFile != ppFileMac; ++ppFile)
	{
		const CWorkspace::SFile & file = **ppFile;
		if (file.m_filek != CWorkspace::FILEK_Library && file.m_filek != CWorkspace::FILEK_DynamicLibrary)
			continue;

		{
			EWC::SStringBuffer strbufLib(aCozWorking, EWC_PMAC(aCozWorking) - aCozWorking);
			FormatCoz(&strbufLib, "%s.DLL", file.m_strFilename.PCoz());
			EnsureTerminated(&strbufLib, '\0');
		}
		auto pFile = filser.PFileFind(aCozWorking);
		if (!pFile)
		{
			printf("unable to locate library '%s'\n", aCozWorking);
			fLoadError = true;
			continue;
		}

		EWC::SStringBuffer strbufLib(aCozWorking, EWC_PMAC(aCozWorking) - aCozWorking);
		FormatCoz(&strbufLib, "%s%\\%s.dll", pFile->m_pChzDirectory, file.m_strFilename.PCoz());
		EnsureTerminated(&strbufLib, '\0');
	
		auto pDll = dlLoadLibrary(aCozWorking);
		if (!pDll)
		{
			printf("failed loading %s\n", aCozWorking);	
			fLoadError = true;
		}
		else
		{
			parypDll->Append(pDll);

			auto pDllsym = dlSymsInit(aCozWorking);
			int cSym = dlSymsCount(pDllsym);
			for (int iSym = 0; iSym < cSym; ++iSym)
			{
				auto pChzSymName = dlSymsName(pDllsym, iSym);
				if (!pChzSymName)
				{
					printf("bad symbol lookup\n");
				}
				else
				{

					void * pFn = dlFindSymbol(pDll, pChzSymName);
					EWC_ASSERT(pFn, "failed looking up reported foreign fufnction");

#ifdef EWC_TRACE_FOREIGN_SYMBOLS
					printf("sym: %s = 0x%p\n", pChzSymName, pFn);
#endif
					CString strSymName(pChzSymName);
					FINS fins = pHashHvPFn->FinsEnsureKeyAndValue(strSymName.Hv(), pFn);
					if (fins != FINS_Inserted)
					{
						printf("symbol '%s' was already loaded\n", strSymName.PCoz());
						fLoadError = false;
					}
				}
			}

			dlSymsCleanup(pDllsym);
		}
	}

	if (fLoadError)
	{
		UnloadForeignLibraries(parypDll);
		return false;
	}

	return true;
}


CVirtualMachine::CVirtualMachine(u8 * pBStackMin, u8 * pBStackMax, CBuilder * pBuild)
:m_pAlloc(pBuild->m_pAlloc)
,m_pDlay(pBuild->m_pDlay)
,m_pBStackMin(pBStackMin)
,m_pBStackMax(pBStackMax)
,m_pBStack(pBStackMax)
,m_pProcCurDebug(nullptr)
,m_pDcvm(nullptr)
,m_pStrbuf(nullptr)
,m_iInstSource(-1)
,m_arypBlockManaged()
,m_arypProcManaged()
,m_hashHvMangledPProc(pBuild->m_pAlloc, BK_ByteCode, pBuild->m_hashHvMangledPProc.CCapacity())
,m_hashPTinprocPProcsig(pBuild->m_pAlloc, BK_ByteCode, 4)
#if DEBUG_PROC_CALL
,m_aryDebCall()
#endif
{
	m_pBGlobal = pBuild->m_dataseg.PBBakeCopy(m_pAlloc, m_pDlay);
}

void CVirtualMachine::Clear()
{
	auto ppProcMac = m_arypProcManaged.PMac();
	for (auto ppProc = m_arypProcManaged.A(); ppProc != ppProcMac; ++ppProc)
	{
		m_pAlloc->EWC_DELETE(*ppProc);
	}
	m_arypProcManaged.Clear();

	auto ppBlockMac = m_arypBlockManaged.PMac();
	for (auto ppBlock = m_arypBlockManaged.A(); ppBlock != ppBlockMac; ++ppBlock)
	{
		m_pAlloc->EWC_DELETE(*ppBlock);
	}
	m_arypBlockManaged.Clear();
	m_hashHvMangledPProc.Clear(0);

	if (m_pBGlobal)
	{
		m_pAlloc->EWC_FREE(m_pBGlobal);
		m_pBGlobal = nullptr;
	}

	m_iInstSource = -1;

	{
		EWC::CHash<STypeInfoProcedure *, SProcedureSignature *>::CIterator iter(&m_hashPTinprocPProcsig);
		while (SProcedureSignature ** ppProcsig = iter.Next())
		{
			m_pAlloc->EWC_FREE(*ppProcsig);
		}

		m_hashPTinprocPProcsig.Clear(0);
	}
}

SConstant * CBuilder::PConstPointer(void * pV, STypeInfo * pTin)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_opk = OPK_Literal;
	pConst->m_word.m_pV = pV;
	pConst->m_pTin = pTin;

	pConst->m_litty.m_litk = (pV == nullptr) ? LITK_Null : LITK_Pointer;
	pConst->m_litty.m_cBit = sizeof(pV) * 8;
	pConst->m_litty.m_fIsSigned = false;
	return pConst;
}

SConstant * CBuilder::PConstInt(u64 nUnsigned, int cBit, bool fIsSigned)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_pTin = (cBit == 1) ? 
		m_pSymtab->PTinlitFromLitk(LITK_Bool) :
		m_pSymtab->PTinlitFromLitk(LITK_Integer, cBit, fIsSigned);
	pConst->m_opk = OPK_Literal;
	pConst->m_word.m_u64 = nUnsigned;

	pConst->m_litty.m_litk = LITK_Integer;
	pConst->m_litty.m_cBit = cBit;
	pConst->m_litty.m_fIsSigned = fIsSigned;
	return pConst;
}

SConstant * CBuilder::PConstFloat(f64 g, int cBit)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_pTin = m_pSymtab->PTinlitFromLitk(LITK_Float, cBit, true);
	pConst->m_opk = OPK_Literal;
	if (cBit == 32) 
		pConst->m_word.m_f32 = (f32)g;
	else
		pConst->m_word.m_f64 = g;

	pConst->m_litty.m_litk = LITK_Float;
	pConst->m_litty.m_cBit = cBit;
	pConst->m_litty.m_fIsSigned = true;
	return pConst;
}

CBuilder::LValue * CBuilder::PLvalConstantGlobalStringPtr(const char * pChzString, const char * pChzName)
{
	auto pConst = m_blistConst.AppendNew();

	auto pTinlit = m_pSymtab->PTinlitFromLitk(LITK_String);
	pConst->m_pTin = pTinlit;
	pConst->m_opk = OPK_Global;

	auto cBString = CBCoz(pChzString);

	const char * pChzNameAdj = pChzName;
#ifdef EWC_TRACE_GLOBAL
	char aCh[256];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
	FormatCoz(&strbuf, "%s('%s')", pChzName, pChzString);
	EnsureTerminated(&strbuf, '\0');
	pChzNameAdj = aCh;
#endif 

	u8 * pBPointer;
	s64 iBPointer;
	s64 iBGlobal;
	u8 * pBGlobal = PBAllocateGlobalWithPointer(cBString, 1, &pBPointer, &iBPointer, &iBGlobal, pChzNameAdj);

#if 1 
	// NOTE: This returns a pointer to a block of characters - NOT a global lhs char * reference (not a ppChz)
	pConst->m_word.m_s64 = iBPointer;

#else
	const char * pChzNamePtr = pChzName;
#ifdef EWC_TRACE_GLOBAL
	char aCh[256];
	EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
	FormatCoz(&strbuf, "&&%s->%d", pChzName, iBPointer);
	EnsureTerminated(&strbuf, '\0');
	pChzNamePtr = aCh;
#endif 

	// globals are refered to by pointer, so a pChz should return a ppChz
	u8 * pBPP;
	s64 iBPP;
	m_dataseg.AllocateData(m_pDlay->m_cBPointer, m_pDlay->m_cBPointer, &pBPP, &iBPP, pChzNamePtr);
	m_dataseg.AddRelocatedPointer(m_pDlay, S32Coerce(iBPP), S32Coerce(iBPointer));

	pConst->m_word.m_s64 = iBPP;
#endif

	CBCopyCoz(pChzString, (char*)pBGlobal, cBString);
	return pConst;
}

CBuilder::LValue * CBuilder::PLvalConstantNull(LType * pLtype)
{
	return PConstPointer(nullptr, pLtype);
}

CBuilder::LValue * CBuilder::PLvalConstantArray(STypeInfo * pTinElement, LValue ** apLval, u32 cpLval)
{
	// BB - this array type info is not unique
	STypeInfoArray * pTinary = EWC_NEW(m_pSymtab->m_pAlloc, STypeInfoArray) STypeInfoArray();
	m_pSymtab->AddManagedTin(pTinary);
	pTinary->m_pTin = pTinElement;
	pTinary->m_c = cpLval;
	pTinary->m_aryk = ARYK_Fixed;

	auto pConst = m_blistConst.AppendNew();
	pConst->m_pTin = pTinary;
	pConst->m_opk = OPK_Global;

	u64 cBElement;
	u64 cBAlignElement;
	CalculateByteSizeAndAlign(m_pDlay, pTinElement, &cBElement, &cBAlignElement);

	u8 * pBPointer;
	s64 iBPointer;
	s64 iBGlobal;
	u64 cBStride = CBAlign(cBElement, cBAlignElement);
	u8 * pBGlobal = PBAllocateGlobalWithPointer(cBStride * cpLval, cBAlignElement, &pBPointer, &iBPointer, &iBGlobal, "constAry");
	pConst->m_word.m_s64 = iBPointer;

	for (u32 ipLval = 0; ipLval < cpLval; ++ipLval)
	{
		auto pConstElem = (SConstant *)apLval[ipLval];
		if (!EWC_FVERIFY(pConstElem->m_valk == VALK_Constant, "must initialize constant array with constant literals"))
			continue; 

		void * pVSrc;
		switch (pConstElem->m_opk)
		{
		case OPK_Literal:
		//case OPK_LiteralArg: // LiteralArg doesn't make sense, the arg will be adjusted after it's copied here 
			EWC_ASSERT(pConstElem->m_litty.m_cBit == cBElement * 8, "element size mismatch");	
			pVSrc = &pConstElem->m_word.m_u64;	
			memcpy(pBGlobal, pVSrc, cBElement);
			break;
		case OPK_Global:
		{
			auto pTinElem = pConstElem->m_pTin;
			bool fIsPointer = pTinElem->m_tink == TINK_Pointer;
			bool fIsString = pTinElem->m_tink == TINK_Literal && ((STypeInfoLiteral*)pTinElem)->m_litty.m_litk == LITK_String;
			
			// not dereferencing both sides here
			s32 iBElem;
			switch (pTinElem->m_tink)
			{
			case TINK_Literal:
				{
					auto pTinlit = (STypeInfoLiteral *)pTinElem;

					switch (pTinlit->m_litty.m_litk)
					{
					case LITK_Array:
						iBElem = *(s32*)m_dataseg.PBFromIndex(pConstElem->m_word.m_s32);
						break;
					case LITK_String:
						iBElem =  pConstElem->m_word.m_s32;
						break;
					default:
						EWC_ASSERT(false, "unexpected literal kind");
					}

				} break;
			case TINK_Pointer:
				{
					iBElem = *(s32*)m_dataseg.PBFromIndex(pConstElem->m_word.m_s32);
				} break;
			case TINK_Struct:
				{
					iBElem = *(s32*)m_dataseg.PBFromIndex(pConstElem->m_word.m_s32);
					//iBElem =  pConstElem->m_word.m_s32;
				} break;
			default:
				EWC_ASSERT(false, "unexpected global type (%s)", PChzFromTink(pTinElem->m_tink));
			}

			m_dataseg.DeepCopy(m_pDlay, pConstElem->m_pTin, S32Coerce(iBGlobal), iBElem);
		}	break;
		default:
			EWC_ASSERT(false, "unexpected operand kind when initializing a constant array");
			continue;
		}

		pBGlobal += cBElement;
		iBGlobal += cBElement;
	}

	return pConst;
}

CBuilder::LValue * CBuilder::PLvalConstantStruct(STypeInfo * pTin, LValue ** apLval, u32 cpLval)
{
	auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pTin);
	if (!pTinstruct)
		return nullptr;

	auto pConst = m_blistConst.AppendNew();
	pConst->m_pTin = pTinstruct;
	pConst->m_opk = OPK_Global;

	u64 cB;
	u64 cBAlign;
	CalculateByteSizeAndAlign(m_pDlay, pTinstruct, &cB, &cBAlign);

	u8 * pBPointer;
	s64 iBPointer;
	s64 iBGlobal;
	u8 * pBGlobal = PBAllocateGlobalWithPointer(cB, cBAlign, &pBPointer, &iBPointer, &iBGlobal, pTin->m_strName.PCoz());
	auto pBGlobalStart = pBGlobal;
	pConst->m_word.m_s64 = iBPointer;

	for (u32 ipLval = 0; ipLval < cpLval; ++ipLval)
	{
		auto pConst = (SConstant *)apLval[ipLval];
		if (!EWC_FVERIFY(pConst->m_valk == VALK_Constant, "initializer must be evaluated at bytecode build time"))
			continue;

		auto pTypememb = &pTinstruct->m_aryTypemembField[ipLval];

		u64 cBField;
		u64 cBAlignField;
		CalculateByteSizeAndAlign(m_pDlay, pTypememb->m_pTin, &cBField, &cBAlignField);

		pBGlobal = (u8 *)PVAlign(pBGlobal, cBAlignField);
		EWC_ASSERT(ptrdiff_t(pTypememb->m_dBOffset) == (pBGlobal - pBGlobalStart), "element layout error");	

		if (pConst->m_opk == OPK_Global)
		{
			auto pBSource = m_dataseg.PBFromGlobal(pConst);
			// BB - needs deep copy
			memcpy(pBGlobal, pBSource, cBField);
		}
		else
		{
			EWC_ASSERT((pConst->m_litty.m_cBit == cBField * 8) || (pConst->m_litty.m_cBit == 1 && cBField == 1), "element size mismatch");	
			if (!EWC_FVERIFY(FIsLiteral(pConst->m_opk) , "expected constant literals"))
				continue;

			switch (cBField)
			{
			case 1: *(u8*)pBGlobal = pConst->m_word.m_u8;		break;
			case 2: *(u16*)pBGlobal = pConst->m_word.m_u16;		break;
			case 4: *(u32*)pBGlobal = pConst->m_word.m_u32;		break;
			case 8: *(u64*)pBGlobal = pConst->m_word.m_u64;		break;
			default:
				EWC_ASSERT(false, "unexpected element size in pLvalConstantArray")
				break;
			}
		}

		pBGlobal += cBField;
	}

	return pConst;
}

CBuilder::Constant * CBuilder::PConstEnumLiteral(STypeInfoEnum * pTinenum, CSTValue * pStval)
{
	auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinenum->m_pTinLoose);
	if (!EWC_FVERIFY(pTinint, "expected integer type for enum constant"))
		return nullptr;

	return PConstInt(pStval->m_nUnsigned, pTinint->m_cBit, pTinint->m_fIsSigned);
}

SConstant * CBuilder::PConstArg(s64 n, int cBit, bool fIsSigned)
{
	auto pConst = m_blistConst.AppendNew();
	pConst->m_pTin = (cBit == 1) ? 
		m_pSymtab->PTinlitFromLitk(LITK_Bool) :
		m_pSymtab->PTinlitFromLitk(LITK_Integer, cBit, fIsSigned);
	pConst->m_opk = OPK_LiteralArg;
	pConst->m_word.m_s64 = n;

	pConst->m_litty.m_litk = LITK_Integer;
	pConst->m_litty.m_cBit = cBit;
	pConst->m_litty.m_fIsSigned = fIsSigned;
	return pConst;
}

SRegister * CBuilder::PReg(s64 n, int cBit, bool fIsSigned)
{
	EWC_CASSERT(sizeof(SRegister) == sizeof(SConstant), "size mismatch between SRegister and SConstant");
	auto pReg = (SRegister *)m_blistConst.AppendNew();
	pReg->m_pTin = (cBit == 1) ? 
		m_pSymtab->PTinlitFromLitk(LITK_Bool) :
		m_pSymtab->PTinlitFromLitk(LITK_Integer, cBit, fIsSigned);
	EWC_ASSERT(pReg->m_pTin, "couldn't find register type");

	pReg->m_opk = OPK_Register;
	pReg->m_word.m_s64 = n;

	pReg->m_litty.m_litk = LITK_Integer;
	pReg->m_litty.m_cBit = cBit;
	pReg->m_litty.m_fIsSigned = fIsSigned;
	return pReg;
}

SRegister * CBuilder::PRegArg(s64 n, int cBit, bool fIsSigned)
{
	EWC_CASSERT(sizeof(SRegister) == sizeof(SConstant), "size mismatch between SRegister and SConstant");
	auto pReg = (SRegister *)m_blistConst.AppendNew();
	pReg->m_pTin = (cBit == 1) ? 
		m_pSymtab->PTinlitFromLitk(LITK_Bool) :
		m_pSymtab->PTinlitFromLitk(LITK_Integer, cBit, fIsSigned);
	EWC_ASSERT(pReg->m_pTin, "couldn't find register type");

	pReg->m_opk = OPK_RegisterArg;
	pReg->m_word.m_s64 = n;

	pReg->m_litty.m_litk = LITK_Integer;
	pReg->m_litty.m_cBit = cBit;
	pReg->m_litty.m_fIsSigned = fIsSigned;
	return pReg;
}

SRegister * CBuilder::PRegArg(s64 iBStack, STypeInfo * pTin)
{
	EWC_CASSERT(sizeof(SRegister) == sizeof(SConstant), "size mismatch between SRegister and SConstant");
	auto pReg = (SRegister *)m_blistConst.AppendNew();
	pReg->m_pTin = pTin;
	pReg->m_opk = OPK_RegisterArg;
	pReg->m_word.m_s64 = iBStack;

	pReg->m_litty.m_litk = LITK_Integer;
	pReg->m_litty.m_cBit = 32;
	pReg->m_litty.m_fIsSigned = true;
	return pReg;
}



void TestDataSegment(CAlloc * pAlloc, SDataLayout * pDlay)
{
	CDataSegment dataseg(pAlloc);
	dataseg.m_cBBlockMin = 20;

	s64 aiB[10];
	for (int i = 0; i < EWC_DIM(aiB); ++i)
	{
		s64 * pN;
		dataseg.AllocateData(sizeof(s64), EWC_ALIGN_OF(s64), (u8**)&pN, &aiB[i], "test"); 
		*pN = i;
	}

	u8 * pBCopy = dataseg.PBBakeCopy(pAlloc, pDlay);
	for (int i = 0; i < EWC_DIM(aiB); ++i)
	{
		s64 * pN = (s64 *)&pBCopy[aiB[i]];
		EWC_ASSERT(i == *pN, "data segment write fail");
	}
	
}

} // namespace BCode 

// [x] generate instructions into basic blocks
// [ ] plumb #bctrace through other phases for unit testing
// [ ] add bcode string to unit test system