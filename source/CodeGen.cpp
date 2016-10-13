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

#include "CodeGen.h"
#include "EwcTypes.h"
#include "Parser.h"
#include "TypeInfo.h"
#include "Util.h"
#include "Workspace.h"

using namespace EWC;

#pragma warning ( push )
#pragma warning(disable : 4141)
#include "llvm-c/Analysis.h"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/IR/CallingConv.h"
#pragma warning ( pop )

#include "MissingLlvmC/llvmcDIBuilder.h"
#include <stdio.h>

extern bool FIsDirectCall(CSTNode * pStnodCall);
CIRProcedure * PProcCodegenInitializer(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodStruct);
CIRProcedure * PProcCodegenPrototype(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod);
LLVMOpaqueValue * PLvalParentScopeForProcedure(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodProc, SDIFile * pDif);

CIRInstruction * PInstGenerateAssignmentFromRef(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	STypeInfo * pTinLhs,
	STypeInfo * pTinRhs,
	CIRValue * pValLhs,
	CIRValue * pValRhsRef);

const char * PChzFromIrop(IROP irop)
{
	if (!EWC_FVERIFY((irop >= IROP_Nil) & (irop < IROP_Max), "unknown IR operand"))
		return "(unknown)";

	if (irop <= IROP_Nil)
		return "nil";

	#define OP(x) #x
	#define OP_RANGE(x, y)
	const char * s_mpIropPChz[] =
	{
		OPCODE_LIST
	};
	#undef OP_RANGE
	#undef op

	EWC_CASSERT(EWC_DIM(s_mpIropPChz) == IROP_Max, "missing IROP string");
	return s_mpIropPChz[irop];
}

static inline llvm::CallingConv::ID CallingconvFromCallconv(CALLCONV callconv)
{
	static const llvm::CallingConv::ID s_mpCallconvCallingconv[] = 
	{
		llvm::CallingConv::C,				//CALLCONV_CX86,
		llvm::CallingConv::X86_StdCall,		//CALLCONV_StdcallX86,
		llvm::CallingConv::X86_64_Win64,	 //CALLCONV_X64,
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
		return llvm::CallingConv::X86_64_Win64;
#endif
	}

	return s_mpCallconvCallingconv[callconv];
}

static inline void DumpLtype(const char * pChzLabel, CIRValue * pVal)
{
	printf("%s: ", pChzLabel);

	auto pLtype = LLVMTypeOf(pVal->m_pLval);
	LLVMDumpType(pLtype);
}



CIRValue::CIRValue(VALK valk)
:m_pLval(nullptr)
,m_pStnod(nullptr)
,m_valk(valk)
{
}



CIRBasicBlock::CIRBasicBlock(EWC::CAlloc * pAlloc)
:m_pLblock(nullptr)
,m_arypInst(pAlloc, EWC::BK_IR)
,m_fIsTerminated(false)
{
}

CIRBasicBlock::~CIRBasicBlock()
{
	m_arypInst.Clear();
}

void CIRBasicBlock::Append(CIRInstruction * pInst)
{
	if (!EWC_FVERIFY(!m_fIsTerminated, "Appending instruction to a terminated block"))
		return;

	m_arypInst.Append(pInst);
	m_fIsTerminated = (pInst->m_irop == IROP_Ret) | (pInst->m_irop == IROP_CondBranch) | (pInst->m_irop == IROP_Branch);
}

static inline s8 COperand(IROP irop)
{
	if (irop == IROP_Call)
		return 0;
	if (((irop >= IROP_BinaryOpMin) & (irop < IROP_BinaryOpMax)) | 
		((irop >= IROP_CmpOpMin) & (irop < IROP_CmpOpMax)) | 
		((irop >= IROP_LogicOpMin) & (irop < IROP_LogicOpMax)) | 
		(irop == IROP_Store))
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



class CIRBuilderErrorContext // tag = berrctx
{
public:
					CIRBuilderErrorContext(SErrorManager * pErrman, CIRBuilder * pBuild, CSTNode * pStnod);
					~CIRBuilderErrorContext();

	SErrorManager * m_pErrman;
	CIRBuilder *	m_pBuild;
	SLexerLocation	m_lexloc;
	CIRBuilderErrorContext * 
					m_pBerrctxPrev;
};


CIRBuilderErrorContext::CIRBuilderErrorContext(SErrorManager * pErrman, CIRBuilder * pBuild, CSTNode * pStnod)
:m_pErrman(pErrman)
,m_pBuild(pBuild)
,m_lexloc(pStnod->m_lexloc)
,m_pBerrctxPrev(pBuild->m_pBerrctx)
{
	pBuild->m_pBerrctx = this;
}

CIRBuilderErrorContext::~CIRBuilderErrorContext()
{
	EWC_ASSERT(m_pBuild->m_pBerrctx == this, "bad error context in builder");
	m_pBuild->m_pBerrctx = m_pBerrctxPrev;
}



static inline LLVMOpaqueType * PLtypeFromPTin(STypeInfo * pTin, u64 * pCElement = nullptr)
{
	if (!pTin)
		return nullptr;

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

			auto pLtypeFunction = LLVMFunctionType(pLtypeReturn, apLtypeParam, (u32)cpLtypeParam, pTinproc->m_fHasVarArgs);

			// NOTE: actually a pointer to a function (not the function type itself)
			auto pLtypPtr = LLVMPointerType(pLtypeFunction, 0);
			return pLtypPtr;

		}
		case TINK_Pointer:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTin;
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
					LLVMTypeRef apLtype[2]; // pointer, count
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

			if (pTinstruct->m_pLtype)
			{
				return pTinstruct->m_pLtype;
			}

			LLVMOpaqueType * pLtype = LLVMStructCreateNamed(LLVMGetGlobalContext(), PChzVerifyAscii(pTinstruct->m_strName.PCoz()));
			pTinstruct->m_pLtype = pLtype;

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
		default: return nullptr;
	}
}



struct SDIFile // tag = dif
{
	LLVMOpaqueValue *				m_pLvalScope;
	LLVMOpaqueValue *				m_pLvalFile;

	EWC::CDynAry<LLVMOpaqueValue *>	m_aryLvalScopeStack;
};

void PathSplitDestructive(char * pCozFull, size_t cBMax, const char ** ppCozPath, const char ** ppCozFile)
{
	char * pCozPath = ".";
	char * pCozFile = pCozFull;

	char * pChLastSlash = nullptr;
	
	char * pChEnd = pCozFull + cBMax;
	for (char * pCozIt = pCozFull; pCozIt != pChEnd; ++pCozIt)
	{
		if ((*pCozIt == '/') | (*pCozIt == '\\'))
		{
			pChLastSlash = pCozIt;
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
}

SDIFile * PDifEnsure(CWorkspace * pWork, CIRBuilder * pBuild, const CString & strFilename)
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
	PathSplitDestructive(pCozCopy, cBFilename, &pCozPath, &pCozFile);

	pDif->m_pLvalScope = pBuild->m_pLvalCompileUnit;
	pDif->m_pLvalFile = LLVMDIBuilderCreateFile(pBuild->m_pDib, pCozFile, pCozPath);

	pDif->m_aryLvalScopeStack.SetAlloc(pWork->m_pAlloc, EWC::BK_WorkspaceFile);
	pDif->m_aryLvalScopeStack.Append(pDif->m_pLvalFile);

	return pDif;
}


LLVMOpaqueValue * PLvalFromDIFile(CIRBuilder * pBuild, SDIFile * pDif)
{
	if (pDif->m_aryLvalScopeStack.FIsEmpty())
	{
		return pBuild->m_pLvalFile;
	}

	return *pDif->m_aryLvalScopeStack.PLast();
}

void PushDIScope(SDIFile * pDif, LLVMOpaqueValue * pLvalScope)
{
	pDif->m_aryLvalScopeStack.Append(pLvalScope);
}

void PopDIScope(SDIFile * pDif, LLVMOpaqueValue * pLvalScope)
{
	pDif->m_aryLvalScopeStack.PopLast();
}

void PushLexicalBlock(CWorkspace * pWork, CIRBuilder * pBuild, const SLexerLocation & lexloc)
{
	SDIFile * pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);

	LLVMOpaqueValue * pLvalScopeParent = PLvalFromDIFile(pBuild, pDif);

	s32 iLine, iCol;
	CalculateLinePosition(pWork, &lexloc, &iLine, &iCol);

	LLVMValueRef pLvalScope = LLVMDIBuilderCreateLexicalBlock(pBuild->m_pDib, pLvalScopeParent, pDif->m_pLvalFile, iLine, iCol);
	pDif->m_aryLvalScopeStack.Append(pLvalScope);
}

void PopLexicalBlock(CWorkspace * pWork, CIRBuilder * pBuild, const SLexerLocation & lexloc)
{
	SDIFile * pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);
	pDif->m_aryLvalScopeStack.PopLast();
}

void EmitLocation(CWorkspace * pWork, CIRBuilder * pBuild, const SLexerLocation & lexloc)
{
	SDIFile * pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);

	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	s32 iLine, iCol;
	CalculateLinePosition(pWork, &lexloc, &iLine, &iCol);

	LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pLvalScope);
	LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pLvalLoc);
}

SDIFile * PDifEmitLocation(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	const SLexerLocation & lexloc,
	s32 * piLine = nullptr,
	s32 * piCol = nullptr)
{
	SDIFile * pDif = PDifEnsure(pWork, pBuild, lexloc.m_strFilename);

	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	s32 iLine, iCol;
	CalculateLinePosition(pWork, &lexloc, &iLine, &iCol);

	LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pLvalScope);
	LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pLvalLoc);

	if (piLine) *piLine = iLine;
	if (piCol) *piCol = iCol;

	return pDif;
}

void CalculateSizeAndAlign(CIRBuilder * pBuild, LLVMOpaqueType * pLtype, u64 * pCBitSize, u64 *pCBitAlign)
{
	*pCBitSize = LLVMSizeOfTypeInBits(pBuild->m_pTargd, pLtype);
	*pCBitAlign = LLVMABIAlignmentOfType(pBuild->m_pTargd, pLtype) * 8;
}

CIRProcedure * PProcTryEnsure(CWorkspace * pWork, CIRBuilder * pBuild, SSymbol * pSym)
{
	if (pSym->m_pVal && EWC_FVERIFY(pSym->m_pVal->m_valk == VALK_ProcedureDefinition))
	{
		return (CIRProcedure *)pSym->m_pVal;
	}

	// this happens when calling a method that is defined later
	CSTNode * pStnodProc = pSym->m_pStnodDefinition;
	auto pDif = PDifEnsure(pWork, pBuild, pStnodProc->m_lexloc.m_strFilename.PCoz());
	auto pLvalParentScope = PLvalParentScopeForProcedure(pWork, pBuild, pStnodProc, pDif);

	PushDIScope(pDif, pLvalParentScope);
	(void) PProcCodegenPrototype(pWork, pBuild, pSym->m_pStnodDefinition);
	PopDIScope(pDif, pLvalParentScope);

	if (pSym->m_pVal && EWC_FVERIFY(pSym->m_pVal->m_valk == VALK_ProcedureDefinition))
	{
		return (CIRProcedure *)pSym->m_pVal;
	}
	return nullptr;
}

LLVMOpaqueValue * PLvalParentScopeForProcedure(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodProc, SDIFile * pDif)
{
	auto pStproc = pStnodProc->m_pStproc;
	if (EWC_FVERIFY(pStproc, "function missing procedure") && pStproc->m_pStnodParentScope)
	{
		CIRProcedure * pProc = nullptr;
		auto pSymParentScope = pStproc->m_pStnodParentScope->m_pSym;
		if (EWC_FVERIFY(pSymParentScope, "expected symbol to be set during type check"))
		{
			pProc = (CIRProcedure *)PProcTryEnsure(pWork, pBuild, pSymParentScope);

			if (!EWC_FVERIFY(pProc && pProc->m_valk == VALK_ProcedureDefinition, "expected IR procedure"))
				pProc = nullptr;
		}

		if (pProc)
		{
			return pProc->m_pLvalDIFunction;
		}
	}

	return pBuild->m_pLvalCompileUnit;
}

static inline LLVMOpaqueValue * PLvalCreateDebugFunction(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	const char * pChzName,
	const char * pChzMangled,
	CSTNode * pStnodFunction,
	CSTNode * pStnodBody,
	LLVMOpaqueValue * pLvalDIFunctionType,
	LLVMOpaqueValue * pLvalFunction)
{
	s32 iLine, iCol;
	CalculateLinePosition(pWork, &pStnodFunction->m_lexloc, &iLine, &iCol);

	s32 iLineBody, iColBody;
	CalculateLinePosition(pWork, &pStnodBody->m_lexloc, &iLineBody, &iColBody);

	auto pDif = PDifEnsure(pWork, pBuild, pStnodBody->m_lexloc.m_strFilename.PCoz());
	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
	u64 cBitAlign = cBitSize;
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

static inline void CreateDebugInfo(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodRef, STypeInfo * pTin)
{
	if (pTin->m_pLvalDIType)
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
			pTin->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, strPunyName.PCoz(), pTinint->m_cBit, pTinint->m_cBit, nDwarf);
		} break;
	case TINK_Float:
		{
			auto pTinfloat = PTinRtiCast<STypeInfoFloat *>(pTin);
			unsigned nDwarf = llvm::dwarf::DW_ATE_float;
			pTin->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, strPunyName.PCoz(), pTinfloat->m_cBit, pTinfloat->m_cBit, nDwarf);
		} break;
	case TINK_Bool:
		{
			unsigned nDwarf = llvm::dwarf::DW_ATE_boolean;
			pTin->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, strPunyName.PCoz(), 8, 8, nDwarf);
		} break;
    case TINK_Pointer:
		{
			auto pTinptr = PTinRtiCast<STypeInfoPointer *>(pTin);

			auto pTinPointedTo = pTinptr->m_pTinPointedTo;
			if (pTinPointedTo->m_tink == TINK_Void)
			{
				// llvm doesn't support void pointers so we treat them as s8 pointers instead
				pTinPointedTo->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, "void", 8, 8, llvm::dwarf::DW_ATE_signed);
			}
			else
			{
				CreateDebugInfo(pWork, pBuild, pStnodRef, pTinPointedTo);
			}

			u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
			u64 cBitAlign = cBitSize;
			pTin->m_pLvalDIType = LLVMDIBuilderCreatePointerType(pDib, pTinptr->m_pTinPointedTo->m_pLvalDIType, cBitSize, cBitAlign, strPunyName.PCoz());
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
				apLvalParam[ipTin] = pTin->m_pLvalDIType;
			}

			u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
			u64 cBitAlign = cBitSize;
			pTin->m_pLvalDIType = LLVMDIBuilderCreateFunctionType(pDib, apLvalParam, cpTinParam, cBitSize, cBitAlign);
		} break;
	case TINK_Array:
		{
			auto pTinary = PTinRtiCast<STypeInfoArray *>(pTin);

			auto pTinElement = pTinary->m_pTin;
			CreateDebugInfo(pWork, pBuild, pStnodRef, pTinElement);

			auto pLtypeArray =  PLtypeFromPTin(pTinary);
			u64 cBitSizeArray, cBitAlignArray;
			CalculateSizeAndAlign(pBuild, pLtypeArray, &cBitSizeArray, &cBitAlignArray);

			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
				{
					LLVMOpaqueValue * apLvalSubscript[1];
					apLvalSubscript[0] = LLVMDIBuilderGetOrCreateRange(pDib, 0, pTinary->m_c);
					pTin->m_pLvalDIType = LLVMDIBuilderCreateArrayType(
						pDib,
						cBitSizeArray,
						cBitAlignArray,
						pTinElement->m_pLvalDIType,
						apLvalSubscript,
						1);
				} break;
		    case ARYK_Reference:
				{
					SDIFile * pDif = PDifEnsure(pWork, pBuild, pStnodRef->m_lexloc.m_strFilename);
					LLVMOpaqueValue * pLvalScope = pDif->m_pLvalFile;

					auto pLtypeMember =  PLtypeFromPTin(pTinary->m_pTin);

					LLVMTypeRef mpArymembPLtype[ARYMEMB_Max]; // pointer, count
					mpArymembPLtype[ARYMEMB_Count] = LLVMInt64Type();
					mpArymembPLtype[ARYMEMB_Data] = LLVMPointerType(pLtypeMember, 0);

					u64 cBitSizeMember, cBitAlignMember;
					unsigned nFlagsMember = 0;
					LLVMOpaqueValue * apLvalMember[2]; // pointer, count	

					for (int arymemb = 0; arymemb < ARYMEMB_Max; ++arymemb)
					{
						auto pLtypeMember =  mpArymembPLtype[arymemb];
						u64 dBitMembOffset = LLVMOffsetOfElement(pBuild->m_pTargd, pLtypeArray, ARYMEMB_Count);
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
													pTinElement->m_pLvalDIType);
					}

					unsigned nFlags = 0;
					pTin->m_pLvalDIType = LLVMDIBuilderCreateStructType(
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
		    case ARYK_Dynamic:
				EWC_ASSERT(false, "debug info is for aryk %s is TBD", PChzFromAryk(pTinary->m_aryk));
			}
		} break;
	case TINK_Struct:
		{
			auto pTinstruct = PTinRtiCast<STypeInfoStruct *>(pTin);
			s32 iLineBody, iColBody;
			CalculateLinePosition(pWork, &pTinstruct->m_pStnodStruct->m_lexloc, &iLineBody, &iColBody);
			
			SDIFile * pDif = PDifEnsure(pWork, pBuild, pTinstruct->m_pStnodStruct->m_lexloc.m_strFilename);

			// BB - This will not work for nested structs (nested in methods or structs)
			auto pLvalScope = pDif->m_pLvalFile;

			u64 cBitSize, cBitAlign;
			auto pLtypeStruct =  PLtypeFromPTin(pTinstruct);
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
			LLVMValueRef * ppLvalMember = apLvalMember;
			pTin->m_pLvalDIType = pLvalDicomp;

			for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
			{
				auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

				auto pTinMember = pTypememb->m_pTin;
				CreateDebugInfo(pWork, pBuild, pStnodRef, pTinMember);

				auto pLtypeElement = PLtypeFromPTin(pTinMember);
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
											pTinMember->m_pLvalDIType);
			}

			LLVMDIBuilderReplaceCompositeElements(pDib, &pLvalDicomp, apLvalMember, cTypememb);

		} break;
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			auto pStnodDefinition = pTinenum->m_tinstructProduced.m_pStnodStruct;

			SDIFile * pDif = PDifEnsure(pWork, pBuild, pStnodDefinition->m_lexloc.m_strFilename);

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
				s64 nValue = pTinecon->m_bintValue.S64Coerce();

				auto strPunyName = StrPunyEncode(pTinecon->m_strName.PCoz());
				apLvalConstant[iTinecon] = LLVMDIBuilderCreateEnumerator(pDib, strPunyName.PCoz(), nValue);
			}

			auto strPunyName = StrPunyEncode(pTinenum->m_strName.PCoz());
			
			u64 cBitSize, cBitAlign;
			auto pLtypeLoose = PLtypeFromPTin(pTinenum->m_pTinLoose);
			CalculateSizeAndAlign(pBuild, pLtypeLoose, &cBitSize, &cBitAlign);
			pTin->m_pLvalDIType = LLVMDIBuilderCreateEnumerationType(
								    pDib, 
									pLvalScope, 
									strPunyName.PCoz(), 
									pDif->m_pLvalFile,
								    iLine,
									cBitSize,
									cBitAlign,
								    apLvalConstant, 
									(u32)cTinecon,
									pTinenum->m_pTinLoose->m_pLvalDIType);
		} break;
	case TINK_Literal:
	case TINK_Void:
	case TINK_String:
		break;
	default: break;
	}
	EWC_ASSERT(pTin->m_pLvalDIType != nullptr, "unhandled type info kind in debug info");
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



// Builder class Methods
CIRBuilder::CIRBuilder(CWorkspace * pWork, EWC::CDynAry<CIRValue *> *	parypValManaged, const char * pChzFilename)
:m_pBerrctx(nullptr)
,m_pLmoduleCur(nullptr)
,m_pLbuild(nullptr)
,m_pTargd (nullptr)
,m_pDib(nullptr)
,m_pLvalCompileUnit(nullptr)
,m_pLvalScope(nullptr)
,m_pLvalFile(nullptr)
,m_pAlloc(pWork->m_pAlloc)
,m_inspt()
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
,m_arypProcVerify(pWork->m_pAlloc, EWC::BK_CodeGen)
,m_parypValManaged(parypValManaged)
,m_aryJumptStack(pWork->m_pAlloc, EWC::BK_CodeGen)
,m_hashHvNUnique(pWork->m_pAlloc)
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
	PathSplitDestructive(pCozCopy, cBFilename, &pCozPath, &pCozFile);



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

	LLVMRelocMode lrelocmode = LLVMRelocDefault;
	LLVMCodeModel lcodemodel = LLVMCodeModelJITDefault;

	// NOTE: llvm-c doesn't expose functions to query these, but it seems to just return the empty string.
	//  This may not work for all target backends.
	const char * pChzCPU = "";
	const char * pChzFeatures = "";

	m_pLtmachine = LLVMCreateTargetMachine(pLtarget, pChzTriple, pChzCPU, pChzFeatures, loptlevel, lrelocmode, lcodemodel);

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
	LLVMSetModuleDataLayout(m_pLmoduleCur, m_pTargd);
	
#endif

	m_pDib = LLVMCreateDIBuilder(m_pLmoduleCur);
	m_nRuntimeLanguage = llvm::dwarf::DW_LANG_C;
	m_pLvalCompileUnit = LLVMDIBuilderCreateCompileUnit(
							m_pDib,
							m_nRuntimeLanguage,
							pCozFile,
							pCozPath,
							"Moe compiler",
							false,
							"",
							0);

	m_pLvalScope = m_pLvalCompileUnit;
}
	
CIRBuilder::~CIRBuilder()
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

void CIRBuilder::GenerateUniqueName(const char * pCozIn, char * pCozOut, size_t cBOutMax)
{
	size_t iCh = CBCoz(pCozIn) - 2;

	// not handling whitespace...
	u32 nIn = 0;
	u32 nMultiple = 1;
	while (iCh >= 0 && ((pCozIn[iCh] >= '0') & (pCozIn[iCh] <= '9')))
	{
		nIn = (pCozIn[iCh] - '0') * nMultiple + nIn;
		nMultiple *= 10;
		--iCh;
	}

	HV hv = 0;
	if (iCh >= 0)
	{
		hv = HvFromPCoz(pCozIn, iCh+1);
	}

	u32 * pN = nullptr;
	FINS fins = m_hashHvNUnique.FinsEnsureKey(hv, &pN);
	EWC::SStringBuffer strbufOut(pCozOut, cBOutMax);
	if (fins == FINS_Inserted)
	{
		*pN = nIn;
		AppendCoz(&strbufOut, pCozIn);
	}
	else
	{
		*pN = ewcMax(nIn, *pN + 1);
		AppendCoz(&strbufOut, pCozIn);

		strbufOut.m_pCozAppend = &strbufOut.m_pCozBegin[iCh+1];
		FormatCoz(&strbufOut, "%d", *pN); 
	}
}

void CIRBuilder::PrintDump()
{
	if (!m_pLmoduleCur)
	{
		printf("No code generated.");
		return;
	}

	LLVMDumpModule(m_pLmoduleCur);
}

CIRInstruction * CIRBuilder::PInstCreateRaw(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	if (!EWC_FVERIFY(m_pBlockCur, "creating instruction with no active block"))
		return nullptr;

	if (m_pBlockCur->m_fIsTerminated)
	{
		if (irop != IROP_Branch && EWC_FVERIFY(m_pBerrctx, "trying to throw warning with no error context"))
		{
			EmitWarning(m_pBerrctx->m_pErrman, &m_pBerrctx->m_lexloc, "Unreachable instruction detected");
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
		pInst->m_apValOperand[1] = pValRhs;
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

CIRInstruction * CIRBuilder::PInstCreatePhi(LLVMOpaqueType * pLtype, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_Phi, nullptr, nullptr, pChzName);
	if (pInst->FIsError())
		return pInst;

	pInst->m_pLval = LLVMBuildPhi(m_pLbuild, pLtype, pChzName);
	return pInst;
}

void AddPhiIncoming(CIRInstruction * pInstPhi, CIRValue * pVal, CIRBasicBlock * pBlock)
{
	LLVMAddIncoming(pInstPhi->m_pLval, &pVal->m_pLval, &pBlock->m_pLblock, 1);
}

CIRInstruction * CIRBuilder::PInstCreatePtrToInt(CIRValue * pValOperand, STypeInfoInteger * pTinint, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_PtrToInt, pValOperand, nullptr, pChzName);
	if (pInst->FIsError())
		return pInst;

	auto pLtypeDst = PLtypeFromPTin(pTinint);
	pInst->m_pLval = LLVMBuildPtrToInt(m_pLbuild, pValOperand->m_pLval, pLtypeDst, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreate(IROP irop, CIRValue * pValOperand, const char * pChzName)
{
	// Unary Ops
	CIRInstruction * pInst = PInstCreateRaw(irop, pValOperand, nullptr, pChzName);
	if (pInst->FIsError())
		return pInst;

	switch (irop)
	{
	case IROP_Ret:
	{
		if (pValOperand)	pInst->m_pLval = LLVMBuildRet(m_pLbuild, pValOperand->m_pLval);
		else				pInst->m_pLval = LLVMBuildRetVoid(m_pLbuild);
	} break;

	case IROP_NNeg:		pInst->m_pLval = LLVMBuildNeg(m_pLbuild, pValOperand->m_pLval, pChzName); break;
	case IROP_GNeg:		pInst->m_pLval = LLVMBuildFNeg(m_pLbuild, pValOperand->m_pLval, pChzName); break;
	case IROP_Not:		pInst->m_pLval = LLVMBuildNot(m_pLbuild, pValOperand->m_pLval, pChzName); break;
	case IROP_Load:		pInst->m_pLval = LLVMBuildLoad(m_pLbuild, pValOperand->m_pLval, pChzName); break;
	default: EWC_ASSERT(false, "%s is not a unary opcode supported by PInstCreate", PChzFromIrop(irop)); break;
	}

	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	// Binary Ops
	CIRInstruction * pInst = PInstCreateRaw(irop, pValLhs, pValRhs, pChzName);
	if (pInst->FIsError())
		return pInst;

	switch (irop)
	{
	case IROP_NAdd:		pInst->m_pLval = LLVMBuildAdd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_GAdd:		pInst->m_pLval = LLVMBuildFAdd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_NSub:		pInst->m_pLval = LLVMBuildSub(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_GSub:		pInst->m_pLval = LLVMBuildFSub(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_NMul:		pInst->m_pLval = LLVMBuildMul(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_GMul:		pInst->m_pLval = LLVMBuildFMul(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_SDiv:		pInst->m_pLval = LLVMBuildSDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_UDiv:		pInst->m_pLval = LLVMBuildUDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_GDiv:		pInst->m_pLval = LLVMBuildFDiv(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_SRem:		pInst->m_pLval = LLVMBuildSRem(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_URem:		pInst->m_pLval = LLVMBuildURem(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_GRem:		pInst->m_pLval = LLVMBuildFRem(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_Shl:		pInst->m_pLval = LLVMBuildShl(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_AShr:		pInst->m_pLval = LLVMBuildAShr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_LShr:		pInst->m_pLval = LLVMBuildLShr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_And:		pInst->m_pLval = LLVMBuildAnd(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName); break;
	case IROP_Or:		pInst->m_pLval = LLVMBuildOr(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);	break;
	case IROP_Xor:		pInst->m_pLval = LLVMBuildXor(m_pLbuild, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);	break;
	default: EWC_ASSERT(false, "%s is not a binary opcode supported by PInstCreate", PChzFromIrop(irop)); break;
	}

	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateCast(IROP irop, CIRValue * pValLhs, STypeInfo * pTinDst, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(irop, pValLhs, nullptr, pChzName);
	if (pInst->FIsError())
		return pInst;

	auto pLtypeDst = PLtypeFromPTin(pTinDst);

	switch (irop)
	{
	case IROP_NTrunc:		pInst->m_pLval = LLVMBuildTrunc(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_SignExt:		pInst->m_pLval = LLVMBuildSExt(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_ZeroExt:		pInst->m_pLval = LLVMBuildZExt(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_GToS:			pInst->m_pLval = LLVMBuildFPToSI(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_GToU:			pInst->m_pLval = LLVMBuildFPToUI(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_SToG:			pInst->m_pLval = LLVMBuildSIToFP(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_UToG:			pInst->m_pLval = LLVMBuildUIToFP(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_GTrunc:		pInst->m_pLval = LLVMBuildFPTrunc(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_GExtend:		pInst->m_pLval = LLVMBuildFPExt(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	case IROP_Bitcast:		pInst->m_pLval = LLVMBuildBitCast(m_pLbuild, pValLhs->m_pLval, pLtypeDst, pChzName); break;
	default: EWC_ASSERT(false, "IROP not supported by PInstCreateCast"); break;
	}

	return pInst;
}

void CIRBuilder::AddManagedVal(CIRValue * pVal)
{
	m_parypValManaged->Append(pVal);
}
					
CIRInstruction * CIRBuilder::PInstCreateCondBranch(
	CIRValue * pValPred,
	CIRBasicBlock * pBlockTrue,
	CIRBasicBlock * pBlockFalse)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_CondBranch, pValPred, nullptr, "branch");
	if (pInst->FIsError())
		return pInst;

	auto pLblockFalse = (pBlockFalse == nullptr) ? nullptr : pBlockFalse->m_pLblock;
	pInst->m_pLval = LLVMBuildCondBr(m_pLbuild, pValPred->m_pLval, pBlockTrue->m_pLblock, pLblockFalse);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNCmp(
	NCMPPRED ncmppred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
#define MOE_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMIntPredicate s_mpNcmpredLpredicate[] =
	{
		NCMPPRED_LIST
	};
#undef MOE_PRED
#undef LLVM_PRED
	EWC_CASSERT(EWC_DIM(s_mpNcmpredLpredicate) == NCMPPRED_Max, "missing elements in int predicate map");
	auto lpredicate = s_mpNcmpredLpredicate[ncmppred];

	CIRInstruction * pInst = PInstCreateRaw(IROP_NCmp, pValLhs, pValRhs, pChzName);
	if (pInst->FIsError())
		return pInst;

	pInst->m_pLval = LLVMBuildICmp(m_pLbuild, lpredicate, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGCmp(
	GCMPPRED gcmppred,
	CIRValue * pValLhs,
	CIRValue * pValRhs,
	const char * pChzName)
{
#define MOE_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMRealPredicate s_mpGcmpredLpredicate[] =
	{
		GCMPPRED_LIST
	};
#undef MOE_PRED
#undef LLVM_PRED
	EWC_CASSERT(EWC_DIM(s_mpGcmpredLpredicate) == GCMPPRED_Max, "missing elements in int predicate map");
	auto lpredicate = s_mpGcmpredLpredicate[gcmppred];

	CIRInstruction * pInst = PInstCreateRaw(IROP_GCmp, pValLhs, pValRhs, pChzName);
	if (pInst->FIsError())
		return pInst;

	pInst->m_pLval = LLVMBuildFCmp(m_pLbuild, lpredicate, pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateBranch(CIRBasicBlock * pBlock)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_Branch, nullptr, nullptr, "branch");
	if (pInst->FIsError())
		return pInst;

	pInst->m_pLval = LLVMBuildBr(m_pLbuild, pBlock->m_pLblock);
	return pInst;
}

CIRGlobal * CIRBuilder::PGlobCreate(LLVMOpaqueType * pLtype, const char * pChzName)
{
	CIRGlobal * pGlob = EWC_NEW(m_pAlloc, CIRGlobal) CIRGlobal();

	pGlob->m_pLval = LLVMAddGlobal(m_pLmoduleCur, pLtype, pChzName);
	AddManagedVal(pGlob);
	return pGlob;
}

CIRInstruction * CIRBuilder::PInstCreateAlloca(LLVMOpaqueType * pLtype, u64 cElement, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_Alloca, nullptr, nullptr, pChzName);
	if (pInst->FIsError())
		return pInst;

	if (cElement > 1)
	{
		auto pLvalCElement = LLVMConstInt(LLVMInt64Type(), cElement, false);
		pInst->m_pLval = LLVMBuildArrayAlloca(m_pLbuild, pLtype, pLvalCElement, pChzName);
	}
	else
	{
		pInst->m_pLval = LLVMBuildAlloca(m_pLbuild, pLtype, pChzName);
	}

	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGEP(
	CIRValue * pValLhs,
	LLVMOpaqueValue ** apLvalIndices,
	u32 cpIndices,
	const char * pChzName)
{
	CIRInstruction * pInst = PInstCreateRaw(IROP_GEP, nullptr, nullptr, pChzName);
	if (pInst->FIsError())
		return pInst;

	pInst->m_pLval = LLVMBuildGEP(m_pLbuild, pValLhs->m_pLval, apLvalIndices, cpIndices, pChzName);
	return pInst;
}

CIRValue * CIRBuilder::PValFromSymbol(SSymbol * pSym)
{
	if (!EWC_FVERIFY(pSym && pSym->m_pVal, "missing value for symbol"))
		return nullptr;

	auto pVal = pSym->m_pVal;
	if (pVal->m_valk == VALK_Instruction)
	{
		auto pInstSym = (CIRInstruction *)pVal;
		if (!EWC_FVERIFY(pInstSym->m_irop = IROP_Alloca, "expected alloca for symbol"))
			return nullptr;
	}
	else if (pVal->m_valk == VALK_ProcedureDefinition)
	{
		auto pProc = (CIRProcedure *)pVal;
		return pVal;
	}

	return pSym->m_pVal;
}

CIRInstruction * CIRBuilder::PInstCreateStore(CIRValue * pValPT, CIRValue * pValT)
{
	//store t into address pointed at by pT

	if (!EWC_FVERIFY(pValPT && (pValPT->m_valk == VALK_Instruction || pValPT->m_valk == VALK_Global), "expected alloca value for symbol"))
		return nullptr;

	CIRInstruction * pInstStore = PInstCreateRaw(IROP_Store, pValPT, pValT, "store");
	if (pInstStore->FIsError())
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
		printf("bad store information\n");
		printf("pLtypeT:"); LLVMDumpType(pLtypeT);
		printf("pLtypePT: (dest)"); LLVMDumpType(pLtypePT);
		printf("\n");
	}

    pInstStore->m_pLval = LLVMBuildStore(m_pLbuild, pValT->m_pLval, pValPT->m_pLval);
	return pInstStore;
}

bool FExtractNumericInfo(STypeInfo * pTin, u32 * pCBit, bool * pFSigned)
{
	// BB - Is there really any good reason not to make one type info for ints, bools and floats?
	switch (pTin->m_tink)
	{
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

inline LLVMOpaqueValue * PLvalConstantInt(int cBit, bool fIsSigned, u64 nUnsigned)
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

inline CIRConstant * PValConstantInt(CIRBuilder * pBuild, int cBit, bool fIsSigned, u64 nUnsigned)
{
	CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
	pBuild->AddManagedVal(pConst);
	pConst->m_pLval = PLvalConstantInt(cBit, fIsSigned, nUnsigned);

	return pConst;
}

inline LLVMOpaqueValue * PLvalConstantFloat(int cBit, F64 g)
{
	switch (cBit)
	{
	case 32: return LLVMConstReal(LLVMFloatType(), g);
	case 64: return LLVMConstReal(LLVMDoubleType(), g);
	default:	EWC_ASSERT(false, "unhandled float size");
		return nullptr;
	}
}

LLVMOpaqueValue * PLvalZeroInType(CIRBuilder * pBuild, STypeInfo * pTin)
{
	auto pLbuild = pBuild->m_pLbuild;
	switch (pTin->m_tink)
	{
	case TINK_Bool:		return LLVMConstInt(LLVMInt1Type(), 0, false);
	case TINK_Integer:	return PLvalConstantInt(((STypeInfoInteger *)pTin)->m_cBit, false, 0);
	case TINK_Float:	return PLvalConstantFloat(((STypeInfoFloat *)pTin)->m_cBit, 0.0);
	case TINK_Enum:
		{
			auto pTinenum = (STypeInfoEnum *)pTin;
			return PLvalZeroInType(pBuild, pTinenum->m_pTinLoose);
		} 
	case TINK_Pointer:
	case TINK_Procedure:
		{
			STypeInfoPointer * pTinptr = (STypeInfoPointer *)pTin;
			auto * pLtype = PLtypeFromPTin(pTinptr);
			if (pLtype)
				return LLVMConstNull(pLtype);
		} break;
	case TINK_Array:
		{
			auto pTinary = (STypeInfoArray *)pTin;
			LLVMOpaqueType * pLtypeElement = PLtypeFromPTin(pTinary->m_pTin);
			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:
			{
				auto pLvalElement = PLvalZeroInType(pBuild, pTinary->m_pTin);

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

		return LLVMConstNamedStruct(pTinstruct->m_pLtype, apLvalMember, cpLvalField);
	}

	default: break;
	}

	return nullptr;
}

CIRConstant * PConstZeroInType(CIRBuilder * pBuild, STypeInfo * pTin)
{
	CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
	pBuild->AddManagedVal(pConst);

	pConst->m_pLval = PLvalZeroInType(pBuild, pTin);
	return pConst;
}

LLVMOpaqueValue * PLvalFromEnumConstant(CIRBuilder * pBuild, STypeInfo * pTinLoose, CSTValue * pStval)
{
	auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTinLoose);
	if (!EWC_FVERIFY(pTinint, "expected integer type for enum constant"))
		return nullptr;

	return PLvalConstantInt(pTinint->m_cBit, pTinint->m_fIsSigned, pStval->m_nUnsigned);
}

LLVMOpaqueValue * PLvalFromLiteral(CIRBuilder * pBuild, STypeInfoLiteral * pTinlit, CSTNode * pStnod)
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

	LLVMOpaqueValue * pLval = nullptr;
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
			u64 nUnsigned = pStval->m_nUnsigned;

			pLval = PLvalConstantInt(pTinlit->m_litty.m_cBit, pTinlit->m_litty.m_fIsSigned, pStval->m_nUnsigned);
		}break;
	case LITK_Float:
		{
			F64 g = 0;
			switch (pStval->m_stvalk)
			{
			case STVALK_UnsignedInt:	g = (float)pStval->m_nUnsigned;	break;
			case STVALK_SignedInt:		g = (float)pStval->m_nSigned;	break;
			case STVALK_Float:			g = pStval->m_g;				break;
			default: EWC_ASSERT(false, "Float literal kind mismatch");
			}

			pLval = PLvalConstantFloat(pTinlit->m_litty.m_cBit, g);
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
			}

			pLval = LLVMConstInt(LLVMInt1Type(), nUnsigned, false);
		} break;
	case LITK_Char:		EWC_ASSERT(false, "TBD"); return nullptr;
	case LITK_String:
		{
			if (!EWC_FVERIFY(pStval->m_stvalk == STVALK_String, "bad value in string literal"))
				return nullptr;

			// string literals aren't really constants in the eyes of llvm, but it'll work for now
			pLval = LLVMBuildGlobalStringPtr(pBuild->m_pLbuild, pStval->m_str.PCoz(), "strlit");
		} break;
	case LITK_Null:
		{
			auto pLtype = PLtypeFromPTin(pTinlit->m_pTinSource);
			if (!EWC_FVERIFY(pLtype, "could not find llvm type for null pointer"))
				return nullptr;

			pLval = LLVMConstNull(pLtype);
		} break;
	case LITK_Array:
		{
			CSTNode * pStnodLit = pStnod;
			if (EWC_FVERIFY(pTinlit->m_pStnodDefinition, "bad array literal definition"))
			{
				pStnodLit = pTinlit->m_pStnodDefinition;
			}

			CSTNode * pStnodList = nullptr;
			CSTDecl * pStdecl = pStnodLit->m_pStdecl;
			if (EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "array literal with no values"))
			{
				pStnodList = pStnodLit->PStnodChild(pStdecl->m_iStnodInit);
			}

			if (!pStnodList || !EWC_FVERIFY(pStnodList->CStnodChild() == pTinlit->m_c, "missing values for array literal"))
				return nullptr;

			size_t cB = sizeof(LLVMOpaqueValue *) * (size_t)pTinlit->m_c;
			auto apLval = (LLVMOpaqueValue **)(alloca(cB));

			for (int iStnod = 0; iStnod < pTinlit->m_c; ++iStnod)
			{
				auto pStnodChild = pStnodList->PStnodChild(iStnod);
				STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnodChild->m_pTin;
				EWC_ASSERT(pTinlit->m_tink == TINK_Literal, "Bad array literal element");

				apLval[iStnod] = PLvalFromLiteral(pBuild, pTinlit, pStnodChild);
			}

			LLVMOpaqueType * pLtypeElement = PLtypeFromPTin(pTinlit->m_pTinSource);
			auto pLvalReturn = LLVMConstArray(pLtypeElement, apLval, u32(pTinlit->m_c));
			return pLvalReturn;
		} break;
	}

	EWC_ASSERT(pLval, "unknown LITK in PLValueFromLiteral");
	return pLval;
}

CIRValue * PValCreateCast(CWorkspace * pWork, CIRBuilder * pBuild, CIRValue * pValSrc, STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
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
			auto pLtype = PLtypeFromPTin(pTinlitSrc, &cElement);
			if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
				return nullptr;

			auto pInstAllocaLit = pBuild->PInstCreateAlloca(pLtype, cElement, "aryLit");

			// copy the literal into memory
			pBuild->PInstCreateStore(pInstAllocaLit, pValSrc);

			LLVMOpaqueType * pLtypeDst = PLtypeFromPTin(pTinDst, nullptr);
			auto pInstAllocaDst = pBuild->PInstCreateAlloca(pLtypeDst, 1, "aryDst");

			// copy the fixed array into the array reference
			STypeInfoArray tinaryFixed;
			tinaryFixed.m_aryk = ARYK_Fixed;
			tinaryFixed.m_c = pTinlitSrc->m_c;
			tinaryFixed.m_pTin = pTinlitSrc->m_pTinSource;

			(void)PInstGenerateAssignmentFromRef(pWork, pBuild, pTinaryDst, &tinaryFixed, pInstAllocaDst, pInstAllocaLit);
			return pBuild->PInstCreate(IROP_Load, pInstAllocaDst, "aryRefLoad");
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

	//auto pLtypeDst = PLtypeFromPTin(pTinDst);

	CIRInstruction * pInst = nullptr;
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
			}

		}break;
	case TINK_Bool:
			switch (pTinSrc->m_tink)
			{
			case TINK_Bool:	
				return pValSrc;
			case TINK_Integer:
				{
					auto pConstZero = PConstZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValSrc, pConstZero, "NToBool");
					return pInst;
				} 
			case TINK_Float:
				{
					auto pLvalZero = PLvalZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreateRaw(IROP_GCmp, pValSrc, nullptr, "GCmp");
					if (pInst->FIsError())
						return pInst;

					pInst->m_pLval = LLVMBuildFCmp(pBuild->m_pLbuild, LLVMRealONE, pValSrc->m_pLval, pLvalZero, "GToBool");
					return pInst;
				}
			case TINK_Pointer:
				{
					auto pConstZero = PConstZeroInType(pBuild, pTinSrc);
					pInst = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValSrc, pConstZero, "PToBool");
					return pInst;
				}
			case TINK_Literal:
				{
					return pValSrc;
				} break;
			} break;
	case TINK_Literal:
		EWC_ASSERT(false, "can't cast to literal");
		return nullptr;
	default:
		EWC_ASSERT(false, "unsupported cast destination type in PValCreateCast");
	}

	return pValSrc;
}

bool FHasConstInitializer(STypeInfo * pTin)
{
	if (!EWC_FVERIFY(pTin, "bad type in FHasConstInitializer"))
		return true;

	if (pTin->m_tink == TINK_Struct)
	{
		auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pTin);

		CSTNode * pStnodStruct = pTinstruct->m_pStnodStruct;
		EWC_ASSERT(pStnodStruct, "missing definition in struct type info");
		CSTNode * pStnodList = pStnodStruct->PStnodChildSafe(1);

		if (pStnodList && EWC_FVERIFY(pStnodList->m_park == PARK_List, "expected member list"))
		{
			auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
			int ipLvalMember = 0;
			for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb, ++ipLvalMember)
			{
				CSTNode * pStnodDecl = pTypememb->m_pStnod;
				if (!EWC_FVERIFY(pStnodDecl->m_pStdecl, "expected decl"))
					continue;

				auto pStnodInit = pStnodDecl->PStnodChildSafe(pStnodDecl->m_pStdecl->m_iStnodInit);
				if (pStnodInit && pStnodInit->m_park == PARK_Uninitializer)
					return false;

				if (!FHasConstInitializer(pStnodDecl->m_pTin))
					return false;
			}
		}
	}
	else if (pTin->m_tink == TINK_Array)
	{
		auto pTinary = (STypeInfoArray *)pTin;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed:		return FHasConstInitializer(pTinary->m_pTin);
		case ARYK_Reference:	return true;
		default: EWC_ASSERT(false, "unhandled ARYK");
		}
	}

	return true;
}

static inline LLVMOpaqueValue * PLvalConstInitializer(CIRBuilder * pBuild, STypeInfo * pTin)
{
	// NOTE: This routine should come up with a default constant even if it has to ignore uninitializers.
	//  This may be a global  which can set uninitialized values to default because they have no runtime cost.

	LLVMOpaqueValue * pLval;
	if (pTin->m_tink == TINK_Struct)
	{
		auto pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pTin);

		int cpLvalField = (int)pTinstruct->m_aryTypemembField.C();
		size_t cB = sizeof(LLVMOpaqueValue *) * cpLvalField;
		auto apLvalMember = (LLVMOpaqueValue **)(alloca(cB));
		EWC::ZeroAB(apLvalMember, cB);

		CSTNode * pStnodStruct = pTinstruct->m_pStnodStruct;
		EWC_ASSERT(pStnodStruct, "missing definition in struct type info");
		CSTNode * pStnodList = pStnodStruct->PStnodChildSafe(1);

		if (pStnodList && EWC_FVERIFY(pStnodList->m_park == PARK_List, "expected member list"))
		{
			auto pTypemembMax = pTinstruct->m_aryTypemembField.PMac();
			int ipLvalMember = 0;
			for (auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMax; ++pTypememb, ++ipLvalMember)
			{
				CSTNode * pStnodDecl = pTypememb->m_pStnod;
				if (!EWC_FVERIFY(pStnodDecl->m_pStdecl, "expected decl"))
					continue;

				auto pStnodInit = pStnodDecl->PStnodChildSafe(pStnodDecl->m_pStdecl->m_iStnodInit);

				if (pStnodInit)
				{
					auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInit->m_pTin);
					if (pTinlit && pStnodInit->m_park != PARK_Uninitializer)
					{
						apLvalMember[ipLvalMember] = PLvalFromLiteral(pBuild, pTinlit, pStnodInit);
					}
				}

				if (!apLvalMember[ipLvalMember])
				{
					apLvalMember[ipLvalMember] = PLvalConstInitializer(pBuild, pTypememb->m_pTin);
					EWC_ASSERT(apLvalMember[ipLvalMember], "expected valid initializer");
				}
			}
		}

		pLval = LLVMConstNamedStruct(pTinstruct->m_pLtype, apLvalMember, cpLvalField);
	}
	else if (pTin->m_tink == TINK_Array)
	{
		auto pTinary = (STypeInfoArray *)pTin;
		switch (pTinary->m_aryk)
		{
		case ARYK_Fixed:
			{
				auto pLvalElement = PLvalZeroInType(pBuild, pTinary->m_pTin);

				size_t cB = sizeof(LLVMOpaqueValue *) * (size_t)pTinary->m_c;
				auto apLval = (LLVMOpaqueValue **)(alloca(cB));

				auto pLvalInit = PLvalConstInitializer(pBuild, pTinary->m_pTin);
				if (!EWC_FVERIFY(pLvalInit, "expected valid initializer"))
					return nullptr;

				for (s64 iElement = 0; iElement < pTinary->m_c; ++iElement)
				{
					apLval[iElement] = pLvalInit;
				}

				LLVMOpaqueType * pLtypeElement = PLtypeFromPTin(pTinary->m_pTin);
				pLval = LLVMConstArray(pLtypeElement, apLval, u32(pTinary->m_c));
			} break;
		case ARYK_Reference:
			{
				pLval = PLvalZeroInType(pBuild, pTin);
			} break;
		default: EWC_ASSERT(false, "unhandled ARYK");
		}
	}
	else
	{
		pLval = PLvalZeroInType(pBuild, pTin);
	}

	EWC_ASSERT(pLval, "unexpected type in PLvalConstInitializer");
	return pLval;
}

static inline CIRValue * PValInitializeToDefault(CWorkspace * pWork, CIRBuilder * pBuild, STypeInfo * pTin, CIRInstruction * pInstPT)
{
	bool fHasConstInit = FHasConstInitializer(pTin);
	if (fHasConstInit)
	{
		auto pLvalConstInit = PLvalConstInitializer(pBuild, pTin);
		CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
		pConst->m_pLval = pLvalConstInit;
		pBuild->AddManagedVal(pConst);

		return pBuild->PInstCreateStore(pInstPT, pConst);
	}

	switch (pTin->m_tink)
	{
	case TINK_Struct:
		{
			auto pTinstruct = (STypeInfoStruct *)pTin;

			// create an init function and call it
			if (!pTinstruct->m_pLvalInitMethod)
			{
				auto pProc = PProcCodegenInitializer(pWork, pBuild, pTinstruct->m_pStnodStruct);
				pBuild->AddManagedVal(pProc);

				if (!EWC_FVERIFY(pTinstruct->m_pLvalInitMethod, "failed to create init method"))
					return nullptr;
			}

			auto pLvalFunction = pTinstruct->m_pLvalInitMethod;
			if (!EWC_FVERIFY(LLVMCountParams(pLvalFunction) == 1, "unexpected number of arguments"))
				return nullptr;

			LLVMOpaqueValue * apLvalArgs[1];
			apLvalArgs[0] = pInstPT->m_pLval;

			CIRInstruction * pInst = pBuild->PInstCreateRaw(IROP_Call, nullptr, nullptr, "RetTmp");
			if (pInst->FIsError())
				return pInst;

			pInst->m_pLval = LLVMBuildCall( pBuild->m_pLbuild, pLvalFunction, apLvalArgs, 1, "");
			return pInst;
		} break;
	case TINK_Array:
		{
			// BB - this code doesn't build a CIRValue structure representing the loop, if that system 
			//  persists this will need to be rewritten.
				
			auto pLbuild = pBuild->m_pLbuild;
			CIRProcedure * pProc = pBuild->m_pProcCur;
			auto pTinary = (STypeInfoArray *)pTin;
			EWC_ASSERT(pTinary->m_aryk == ARYK_Fixed, "unexpected ARYK");

			auto pLvalZero = PLvalConstantInt(64, false, 0);
			auto pLvalOne = PLvalConstantInt(64, false, 1);
			auto pLvalCount = PLvalConstantInt(64, false, pTinary->m_c);
			
			auto pLvalAlloca = LLVMBuildAlloca(pLbuild, LLVMInt64Type(), "iInit");
			LLVMBuildStore(pLbuild, pLvalZero, pLvalAlloca);

			CIRBasicBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "initPred");
			CIRBasicBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "initBody");
			CIRBasicBlock * pBlockPost = pBuild->PBlockCreate(pProc, "initPost");

			(void) pBuild->PInstCreateBranch(pBlockPred);	

			pBuild->ActivateBlock(pBlockPred);
			auto pLvalLoadIndex = LLVMBuildLoad(pLbuild, pLvalAlloca, "iLoad");
			auto pLvalCmp = LLVMBuildICmp(pLbuild, LLVMIntULT, pLvalLoadIndex, pLvalCount, "NCmp");

			LLVMBuildCondBr(pLbuild, pLvalCmp, pBlockBody->m_pLblock, pBlockPost->m_pLblock);

			pBuild->ActivateBlock(pBlockBody);

			LLVMOpaqueValue * apLvalIndex[2] = {};
			apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);
			apLvalIndex[1] = pLvalLoadIndex;
			auto pInstGEP = pBuild->PInstCreateGEP(pInstPT, apLvalIndex, 2, "initGEP");
			(void) PValInitializeToDefault(pWork, pBuild, pTinary->m_pTin, pInstGEP);

			auto pLvalInc = LLVMBuildAdd(pLbuild, pLvalLoadIndex, pLvalOne, "iInc");
			LLVMBuildStore(pLbuild, pLvalInc, pLvalAlloca);
			LLVMBuildBr(pLbuild, pBlockPred->m_pLblock);

			pBuild->ActivateBlock(pBlockPost);

			return nullptr;
		}
	default:
		EWC_ASSERT(false, "expected valid const init for type %s", PChzFromTink(pTin->m_tink));
		return nullptr;
	}
}

CIRValue * PValFromArrayMember(CWorkspace * pWork, CIRBuilder * pBuild, CIRValue * pValAryRef, STypeInfoArray * pTinary, ARYMEMB arymemb)
{
	EWC_ASSERT(pTinary->m_tink == TINK_Array, "expected array");
	LLVMOpaqueValue * apLvalIndex[2] = {};
	apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);

	if (pTinary->m_aryk == ARYK_Fixed)
	{
		if (arymemb == ARYMEMB_Count)
		{
			return PValConstantInt(pBuild, 64, true, pTinary->m_c);
		}

		EWC_ASSERT(arymemb == ARYMEMB_Data, "unexpected array member '%s'", PChzFromArymemb(arymemb));
		apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), 0, false);

		auto pInstGep = pBuild->PInstCreateGEP(pValAryRef, apLvalIndex, EWC_DIM(apLvalIndex), "aryGep");
		return pInstGep; 
	}

	apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), arymemb, false);

	auto pValReturn = pBuild->PInstCreateGEP(pValAryRef, apLvalIndex, EWC_DIM(apLvalIndex), "aryGep");
	return pBuild->PInstCreate(IROP_Load, pValReturn, "arymembLoad");
}

static inline CIRValue * PValGenerateRefCast(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodRhs, STypeInfo * pTinOut)
{
	STypeInfo * pTinRhs = pStnodRhs->m_pTin;

	if (pTinRhs && pTinRhs->m_tink == TINK_Array)
	{
		CIRValue * pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Reference);

		// special case for assigning arrays to pointers, need reference to the array type.
		if (pTinOut->m_tink == TINK_Pointer)
		{
			auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
			auto pValData = PValFromArrayMember(pWork, pBuild, pValRhsRef, pTinaryRhs, ARYMEMB_Data);

			auto pTinptr = (STypeInfoPointer *)pTinOut;
			if (FTypesAreSame(pTinaryRhs->m_pTin, pTinptr->m_pTinPointedTo))
				return pValData;

			return pBuild->PInstCreateCast(IROP_Bitcast, pValData, pTinptr, "Bitcast");
		}
		if (pTinOut->m_tink == TINK_Array)
		{
			auto pTinaryRhs = (STypeInfoArray *)pTinRhs;
			auto pTinaryLhs = (STypeInfoArray *)pTinOut;

			if (pTinaryRhs->m_aryk != pTinaryLhs->m_aryk)
			{
				if (pTinaryLhs->m_aryk == ARYK_Reference)
				{
					EWC_ASSERT(pTinaryRhs->m_aryk == ARYK_Fixed, "expected ARYK_Fixed");

					u64 cElement;
					auto pLtype = PLtypeFromPTin(pTinaryLhs, &cElement);
					if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for cast"))
						return nullptr;

					auto * pInstAlloca = pBuild->PInstCreateAlloca(pLtype, cElement, "aryCast");
					auto * pInst = PInstGenerateAssignmentFromRef(pWork, pBuild, pTinaryLhs, pTinaryRhs, pInstAlloca, pValRhsRef);
					return pInstAlloca;
				}

				EWC_ASSERT(false, "no implicit cast from ARYK %s to ARYK %s", PChzFromAryk(pTinaryRhs->m_aryk), PChzFromAryk(pTinaryLhs->m_aryk));
			}
		}

		CIRValue * pValSrc = pBuild->PInstCreate(IROP_Load, pValRhsRef, "castLoad");
		auto pVal = PValCreateCast(pWork, pBuild, pValSrc, pTinRhs, pTinOut);
		if (!pVal)
		{
			EmitError(pWork, &pStnodRhs->m_lexloc, "INTERNAL ERROR: trying to codegen unsupported numeric cast.");
		}
		return pVal;
	}

	CIRValue * pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);
	auto pVal = PValCreateCast(pWork, pBuild, pValRhs, pTinRhs, pTinOut);
	if (!pVal)
	{
		EmitError(pWork, &pStnodRhs->m_lexloc, "INTERNAL ERROR: trying to codegen unsupported numeric cast.");
	}
	return pVal;
}

CIRInstruction * PInstGenerateAssignmentFromRef(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
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

					auto pLbuild = pBuild->m_pLbuild;
					CIRProcedure * pProc = pBuild->m_pProcCur;

					auto pValZero = PValConstantInt(pBuild, 64, false, 0);
					auto pValOne = PValConstantInt(pBuild, 64, false, 1);
					auto pValCount = PValConstantInt(pBuild, 64, false, pTinaryLhs->m_c);
					
					auto pInstAlloca = pBuild->PInstCreateAlloca(LLVMInt64Type(), 0, "iInit");
					(void) pBuild->PInstCreateStore(pInstAlloca, pValZero);

					CIRBasicBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "copyPred");
					CIRBasicBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "copyBody");
					CIRBasicBlock * pBlockPost = pBuild->PBlockCreate(pProc, "copyPost");

					(void) pBuild->PInstCreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPred);
					auto pInstLoadIndex = pBuild->PInstCreate(IROP_Load, pInstAlloca, "iLoad");
					auto pInstCmp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpULT, pInstLoadIndex, pValCount, "NCmp");

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
					(void) pBuild->PInstCreateStore(pInstAlloca, pInstInc);
					(void) pBuild->PInstCreateBranch(pBlockPred);

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
							pValCount = PValConstantInt(pBuild, 64, true, cRhs);

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

			LLVMOpaqueValue * apLvalIndex[2] = {};
			apLvalIndex[0] = LLVMConstInt(LLVMInt32Type(), 0, false);

			CIRInstruction * pInstReturn = nullptr;
			int cTypemembField = (int)pTinstruct->m_aryTypemembField.C();
			for (int iTypememb = 0; iTypememb < cTypemembField; ++iTypememb)
			{
				auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];
				apLvalIndex[1] = LLVMConstInt(LLVMInt32Type(), iTypememb, false);

				auto pInstLhs = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, 2, "gepLhs");
				auto pInstRhs = pBuild->PInstCreateGEP(pValRhsRef, apLvalIndex, 2, "gepRhs");

				auto pTin = pTypememb->m_pTin;
				pInstReturn = PInstGenerateAssignmentFromRef(pWork, pBuild, pTin, pTin, pInstLhs, pInstRhs);
			}
		} break;
		default:
		{
			auto pInstRhsLoad = pBuild->PInstCreate(IROP_Load, pValRhsRef, "loadRhs");
			return pBuild->PInstCreateStore(pValLhs, pInstRhsLoad);
		}
	}

	return nullptr;
}

CIRInstruction * PInstGenerateAssignment(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	STypeInfo * pTinLhs,
	CIRValue * pValLhs,
	CSTNode * pStnodRhs)
{
	switch (pTinLhs->m_tink)
	{
	case TINK_Array:
		{
			if (pStnodRhs->m_pTin->m_tink == TINK_Literal)
			{
				auto pTinlitRhs = (STypeInfoLiteral *)pStnodRhs->m_pTin;
				auto pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);

				auto pTinaryLhs = (STypeInfoArray *)pTinLhs;
				if (pTinaryLhs->m_aryk == ARYK_Reference)
				{
					// BB - Allocating this on the stack feels a little dicey, but so does returning a reference to an
					//  array that happens to be static and isn't read-only.
						
					u64 cElement;
					auto pLtype = PLtypeFromPTin(pTinlitRhs, &cElement);
					if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
						return nullptr;

					auto pInstAlloca = pBuild->PInstCreateAlloca(pLtype, cElement, "aryLit");

					// copy the literal into memory
					pBuild->PInstCreateStore(pInstAlloca, pValRhsRef);
					return PInstGenerateAssignmentFromRef(pWork, pBuild, pTinLhs, pStnodRhs->m_pTin, pValLhs, pInstAlloca);
				}

				return pBuild->PInstCreateStore(pValLhs, pValRhsRef);
			}

			auto pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Reference);
			return PInstGenerateAssignmentFromRef(pWork, pBuild, pTinLhs, pStnodRhs->m_pTin, pValLhs, pValRhsRef);
		} break;
	case TINK_Struct:
		{
			CIRValue * pValRhsRef = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);

			u64 cElement;
			auto pLtypeLhs = PLtypeFromPTin(pTinLhs, &cElement);
			if (!EWC_FVERIFY(pLtypeLhs, "couldn't find llvm type for declaration"))
				return nullptr;

			pBuild->PInstCreateStore(pValLhs, pValRhsRef);
		} break;
	default: 
		{
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pTinLhs);
			EWC_ASSERT(pValRhsCast, "bad cast");
			return pBuild->PInstCreateStore(pValLhs, pValRhsCast);
		}
	}
	return nullptr;
}

void GeneratePredicate(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	CSTNode * pStnodPred,
	CIRBasicBlock * pBlockTrue,
	CIRBasicBlock * pBlockPost,
	STypeInfo * pTinBool)
{
	EWC_ASSERT(pTinBool->m_tink == TINK_Bool, "expected bool type for predicate");

	// short circuiting: 
	//  jump to true if either operand to || is true
	//  jump to post if either operand to && is false 

	CSTNode * pStnodOp = pStnodPred;
	if (pStnodOp->m_park == PARK_LogicalAndOrOp)
	{
		CIRBasicBlock *	pBlockRhs = pBuild->PBlockCreate(pBuild->m_pProcCur, "predRhs");
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
		CIRValue * pValPred = PValGenerate(pWork, pBuild, pStnodPred, VALGENK_Instance);
		CIRValue * pValPredCast = PValCreateCast(pWork, pBuild, pValPred, pStnodPred->m_pTin, pTinBool);
		if (!pValPredCast)
		{
			EmitError(pWork, &pStnodPred->m_lexloc, "INTERNAL ERROR: trying to codegen unsupported numeric cast in predicate.");
		}
		else
		{
			(void)pBuild->PInstCreateCondBranch(pValPredCast, pBlockTrue, pBlockPost);
		}
	}
}

static inline CIRValue * PValGenerateMethodBody(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	CIRProcedure * pProc,
	CSTNode * pStnodBody, 
	bool fNeedsNullReturn)
{
	EWC_ASSERT(pProc && pStnodBody, "bad parameters to PValGenerateMethodBody");
	EWC_ASSERT(pProc->m_pLvalDIFunction, "Missing debug info function");

	auto pDif = PDifEnsure(pWork, pBuild, pStnodBody->m_lexloc.m_strFilename.PCoz());
	PushDIScope(pDif, pProc->m_pLvalDIFunction);

	pBuild->ActivateProcedure(pProc, pProc->m_pBlockEntry);
	CIRValue * pValRet = PValGenerate(pWork, pBuild, pStnodBody, VALGENK_Instance);

	if (fNeedsNullReturn)
	{
		(void) pBuild->PInstCreate(IROP_Ret, nullptr, "RetTmp");
	}

	PopDIScope(pDif, pProc->m_pLvalDIFunction);

	pBuild->ActivateProcedure(nullptr, nullptr);

	pBuild->m_arypProcVerify.Append(pProc);
	return pValRet;
}

// helper routine for generating operators, used to make sure type checking errors are in sync with the code generator
struct SOperatorInfo // tag = opinfo
{
					SOperatorInfo()
					:m_irop(IROP_Nil)
					,m_ncmppred(NCMPPRED_Nil)
					,m_gcmppred(GCMPPRED_Nil)
					,m_fNegateFirst(false)
					,m_pChzName(nullptr)
						{ ; }

	IROP			m_irop;
	NCMPPRED		m_ncmppred;
	GCMPPRED		m_gcmppred;
	bool			m_fNegateFirst;
	const char *	m_pChzName;
};

void CreateOpinfo(IROP irop, const char * pChzName, SOperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = irop;
	pOpinfo->m_pChzName = pChzName;
}

void CreateOpinfo(NCMPPRED ncmppred, const char * pChzName, SOperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = IROP_NCmp;
	pOpinfo->m_ncmppred = ncmppred;
	pOpinfo->m_pChzName = pChzName;
}

void CreateOpinfo(GCMPPRED gcmppred, const char * pChzName, SOperatorInfo * pOpinfo)
{
	pOpinfo->m_irop = IROP_GCmp;
	pOpinfo->m_gcmppred = gcmppred;
	pOpinfo->m_pChzName = pChzName;
}

static void GenerateOperatorInfo(TOK tok, const SOpTypes * pOptype, SOperatorInfo * pOpinfo)
{
	STypeInfo * apTin[2] = {pOptype->m_pTinLhs, pOptype->m_pTinRhs};
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
			default:			tink = TINK_Nil;
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
			switch(tok)
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
			}
		}
		else if (tinkMin == TINK_Integer && tinkMax == TINK_Array)
		{
			switch(tok)
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
			switch(tok)
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

		return;
	}

	TINK tink = aTink[0];
	bool fIsSigned = aFIsSigned[0];

	CIRInstruction * pInstOp = nullptr;
	switch (tink)
	{
	case TINK_Bool:
		switch (tok)
		{
			case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			case TOK_EqualEqual:	CreateOpinfo(NCMPPRED_NCmpEQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NCMPPRED_NCmpNE, "NCmpNq", pOpinfo); break;
			case TOK_AndEqual:
			case '&':				CreateOpinfo(IROP_And, "nAndTmp", pOpinfo); break;
			case TOK_OrEqual:
			case '|':				CreateOpinfo(IROP_Or, "nOrTmp", pOpinfo); break;
			case TOK_AndAnd:		CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
			case TOK_OrOr:			CreateOpinfo(IROP_Phi, "Phi", pOpinfo); break;	// only useful for FDoesOperatorExist, codegen is more complicated
		} break;
	case TINK_Integer:
		switch (tok)
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
			case TOK_EqualEqual:	CreateOpinfo(NCMPPRED_NCmpEQ, "NCmpEq", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(NCMPPRED_NCmpNE, "NCmpNq", pOpinfo); break;
			case TOK_LessEqual:
				if (fIsSigned)	CreateOpinfo(NCMPPRED_NCmpSLE, "CmpSLE", pOpinfo);
				else			CreateOpinfo(NCMPPRED_NCmpULE, "NCmpULE", pOpinfo);
				break;
			case TOK_GreaterEqual:
				if (fIsSigned)	CreateOpinfo(NCMPPRED_NCmpSGE, "NCmpSGE", pOpinfo);
				else			CreateOpinfo(NCMPPRED_NCmpUGE, "NCmpUGE", pOpinfo);
				break;
			case '<':
				if (fIsSigned)	CreateOpinfo(NCMPPRED_NCmpSLT, "NCmpSLT", pOpinfo);
				else			CreateOpinfo(NCMPPRED_NCmpULT, "NCmpULT", pOpinfo);
				break;
			case '>':
				if (fIsSigned)	CreateOpinfo(NCMPPRED_NCmpSGT, "NCmpSGT", pOpinfo);
				else			CreateOpinfo(NCMPPRED_NCmpUGT, "NCmpUGT", pOpinfo);
				break;
		} break;
	case TINK_Float:
		switch (tok)
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
			case TOK_EqualEqual:	CreateOpinfo(GCMPPRED_GCmpOEQ, "NCmpOEQ", pOpinfo); break;
			case TOK_NotEqual:		CreateOpinfo(GCMPPRED_GCmpONE, "NCmpONE", pOpinfo); break;
			case TOK_LessEqual:		CreateOpinfo(GCMPPRED_GCmpOLE, "NCmpOLE", pOpinfo); break;
			case TOK_GreaterEqual:	CreateOpinfo(GCMPPRED_GCmpOGE, "NCmpOGE", pOpinfo); break;
			case '<': 				CreateOpinfo(GCMPPRED_GCmpOLT, "NCmpOLT", pOpinfo); break;
			case '>': 				CreateOpinfo(GCMPPRED_GCmpOGT, "NCmpOGT", pOpinfo); break;
		} break;
	case TINK_Procedure:
		{
			switch (tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NCMPPRED_NCmpEQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NCMPPRED_NCmpNE, "NCmpNq", pOpinfo); break;
			}
		} break;
	case TINK_Struct:
		{
			switch (tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
			}
		} break;
	case TINK_Pointer:
	case TINK_Array:
		{
			switch (tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case '-': 			
					{
						CreateOpinfo(IROP_PtrDiff, "ptrDif", pOpinfo);
					} break;
				case TOK_EqualEqual:	CreateOpinfo(NCMPPRED_NCmpEQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NCMPPRED_NCmpNE, "NCmpNq", pOpinfo); break;
/*				case TOK_PlusEqual:		CreateOpinfo(NCMPPRED_NCmpEQ, "NCmpEq", pOpinfo); break;
				case TOK_MinusEqual:	CreateOpinfo(NCMPPRED_NCmpNE, "NCmpNq", pOpinfo); break;
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
			// BB - why is the RHS still a literal here?
			//EWC_ASSERT(FTypesAreSame(pTinLhs, pTinRhs), "enum comparison type mismatch");

			switch (tok)
			{
				case '=':				CreateOpinfo(IROP_Store, "store", pOpinfo); break;
				case TOK_EqualEqual:	CreateOpinfo(NCMPPRED_NCmpEQ, "NCmpEq", pOpinfo); break;
				case TOK_NotEqual:		CreateOpinfo(NCMPPRED_NCmpNE, "NCmpNq", pOpinfo); break;
				case '+': 				CreateOpinfo(IROP_NAdd, "nAddTmp", pOpinfo); break;
				case '-': 				CreateOpinfo(IROP_NSub, "nSubTmp", pOpinfo); break;
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

static inline CIRInstruction * PInstGenerateOperator(
	CIRBuilder * pBuild,
	TOK tok,
	const SOpTypes * pOptype,
	CIRValue * pValLhs,
	CIRValue * pValRhs)
{
	SOperatorInfo opinfo;
	GenerateOperatorInfo(tok, pOptype, &opinfo);
	if (!EWC_FVERIFY(opinfo.m_irop != IROP_Store, "bad optype"))
	{
		// IROP_Store is just used to signal that a store operation exists, but codegen should call CreateStore rather
		//  than this function
		return nullptr;
	}

	CIRInstruction * pInstOp = nullptr;
	switch (opinfo.m_irop)
	{
	case IROP_GEP:		// for pointer arithmetic
		{
			if (opinfo.m_fNegateFirst)
			{
				pValRhs = pBuild->PInstCreate(IROP_NNeg, pValRhs, "NNeg");
			}
			LLVMOpaqueValue * pLvalIndex = pValRhs->m_pLval;
			pInstOp = pBuild->PInstCreateGEP(pValLhs, &pLvalIndex, 1, opinfo.m_pChzName); break;
		} break;
	case IROP_PtrDiff:
		{
			u64 cBitSize;
			u64 cBitAlign;

			auto pTinptr = PTinDerivedCast<STypeInfoPointer*>(pOptype->m_pTinLhs);
			auto pLtype = PLtypeFromPTin(pTinptr->m_pTinPointedTo);
			CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);

			if (!EWC_FVERIFY(pOptype->m_pTinResult->m_tink == TINK_Integer))
				return nullptr;

			auto pTinintResult = PTinRtiCast<STypeInfoInteger *>(pOptype->m_pTinResult);

			pValLhs = pBuild->PInstCreatePtrToInt(pValLhs, pTinintResult, "PDifL");
			pValRhs = pBuild->PInstCreatePtrToInt(pValRhs, pTinintResult, "PDifR");
			pInstOp = pBuild->PInstCreate(IROP_NSub, pValLhs, pValRhs, "PtrSub");

			if (cBitSize > 8)
			{
				auto pLval = PLvalConstantInt(pTinintResult->m_cBit, false, cBitSize / 8);
				CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
				pConst->m_pLval = pLval;
				pBuild->AddManagedVal(pConst);

				pInstOp = pBuild->PInstCreate(IROP_SDiv, pInstOp, pConst, "PtrDif");
			}
		} break;
	case IROP_NCmp:		pInstOp = pBuild->PInstCreateNCmp(opinfo.m_ncmppred, pValLhs, pValRhs, opinfo.m_pChzName); break;
	case IROP_GCmp:		pInstOp = pBuild->PInstCreateGCmp(opinfo.m_gcmppred, pValLhs, pValRhs, opinfo.m_pChzName); break;
	default:			pInstOp = pBuild->PInstCreate(opinfo.m_irop, pValLhs, pValRhs, opinfo.m_pChzName); break;
	}

	// Note: This should be caught by the type checker! This function should match FDoesOperatorExist
	EWC_ASSERT(pInstOp, "unexpected op in PInstGenerateOperator '%'", PCozFromTok(tok));
	return pInstOp;
}

CIRValue * PValGenerateDecl(
	CWorkspace * pWork,
	CIRBuilder * pBuild,
	CSTNode * pStnod,
	CSTNode * pStnodInit,
	VALGENK valgenk)
{
	CSTDecl * pStdecl = pStnod->m_pStdecl;
	if (!pStdecl || !EWC_FVERIFY(pStnod->m_pSym, "declaration without symbol"))
		return nullptr;

	u64 cElement;
	auto pLtype = PLtypeFromPTin(pStnod->m_pTin, &cElement);
	if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
		return nullptr;

	s32 iLine, iCol;
	auto pDif = PDifEmitLocation(pWork, pBuild, pStnod->m_lexloc, &iLine, &iCol);

	LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

	CreateDebugInfo(pWork, pBuild, pStnod, pStnod->m_pTin);

	bool fIsGlobal = pBuild->m_pProcCur == nullptr;
	auto strName = pStnod->m_pSym->m_strName;
	auto strPunyName = StrPunyEncode(strName.PCoz());
	if (fIsGlobal)
	{
		auto pGlob = pBuild->PGlobCreate(pLtype, strPunyName.PCoz());
		pStnod->m_pSym->m_pVal = pGlob;

		auto pLvalDIVariable = LLVMDIBuilderCreateGlobalVariable(
								pBuild->m_pDib,
								pLvalScope, 
								strName.PCoz(),
								strPunyName.PCoz(),
								pDif->m_pLvalFile,
								iLine,
								pStnod->m_pTin->m_pLvalDIType,
								true,
								pGlob->m_pLval);

		LLVMOpaqueValue * pLvalInit = nullptr;
		if (pStnodInit)
		{
			auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInit->m_pTin);
			if (pTinlit && pStnodInit->m_park != PARK_Uninitializer)
			{
				pLvalInit = PLvalFromLiteral(pBuild, pTinlit, pStnodInit);
			}
		}

		if (!pLvalInit)
		{
			pLvalInit = PLvalConstInitializer(pBuild, pStnod->m_pTin);
		}

		LLVMSetInitializer(pGlob->m_pLval, pLvalInit);
		return pGlob;
	}
	else
	{
		auto pInstAlloca = pBuild->PInstCreateAlloca(pLtype, cElement, strPunyName.PCoz());
		pStnod->m_pSym->m_pVal = pInstAlloca;
	
		auto pLvalDIVariable = LLVMDIBuilderCreateAutoVariable(
								pBuild->m_pDib,
								pLvalScope, 
								strPunyName.PCoz(),
								pDif->m_pLvalFile,
								iLine,
								pStnod->m_pTin->m_pLvalDIType,
								false,
								0);

		(void) LLVMDIBuilderInsertDeclare(
				pBuild->m_pDib,
				pInstAlloca->m_pLval,
				pLvalDIVariable,
				pLvalScope,
				iLine, 
				iCol, 
				pBuild->m_pBlockCur->m_pLblock);

		if (pStnodInit)
		{
			if (pStnodInit->m_park != PARK_Uninitializer)
			{
				PInstGenerateAssignment(pWork, pBuild, pStnod->m_pTin, pInstAlloca, pStnodInit);
			}
		}
		else
		{
			return PValInitializeToDefault(pWork, pBuild, pStnod->m_pTin, pInstAlloca);
		}
		return pInstAlloca;
	}
}



CIRValue * PValGenerate(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod, VALGENK valgenk)
{
	CIRBuilderErrorContext berrctx(pWork->m_pErrman, pBuild, pStnod);

	if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
	{
		// constant decl's don't actually generate anything until referenced.
		if (pStnod->m_park == PARK_ConstantDecl)
			return nullptr;

		STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pStnod->m_pTin;
		if (!pTinlit || pTinlit->m_tink != TINK_Literal)
			return nullptr;

		auto pLval = PLvalFromLiteral(pBuild, pTinlit, pStnod);
		if (!pLval)
			return nullptr;

		CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
		pBuild->AddManagedVal(pConst);
		pConst->m_pLval = pLval;
		return pConst;
	}

	switch (pStnod->m_park)
	{
	case PARK_ProcedureDefinition:
		{ 
			if (!EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check, (implicit function?)"))
				return nullptr;

			CIRProcedure * pProc = nullptr;
			if (!pStnod->m_pSym->m_pVal)
			{
				pProc = PProcCodegenPrototype(pWork, pBuild, pStnod);
			}
			else
			{
				pProc = (CIRProcedure *)pStnod->m_pSym->m_pVal;
				if (!EWC_FVERIFY(pProc->m_valk == VALK_ProcedureDefinition, "expected IR procedure"))
					return nullptr;
			}

			CSTNode * pStnodBody = nullptr;
			CSTProcedure * pStproc = pStnod->m_pStproc;
			if (!pStproc->m_fIsForeign && EWC_FVERIFY(pStproc && pProc->m_pBlockEntry, "Encountered procedure without CSTProcedure"))
			{
				pStnodBody = pStnod->PStnodChildSafe(pStproc->m_iStnodBody);
			}

			if (pProc && pStnodBody)
			{
				(void) PValGenerateMethodBody(pWork, pBuild, pProc, pStnodBody, false);
			}
		} break;
	case PARK_List:
		{
			int cStnodChild = pStnod->CStnodChild();
			for (int iStnodChild = 0; iStnodChild < cStnodChild; ++iStnodChild)
			{
				PValGenerate(pWork, pBuild, pStnod->PStnodChild(iStnodChild), VALGENK_Instance);
			}

		}break;
	case PARK_Decl:
		{
			CSTDecl * pStdecl = pStnod->m_pStdecl;
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
			auto pStdecl = pStnod->m_pStdecl;
			if (!EWC_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "expected init child for cast"))
				return nullptr;

			return PValGenerateRefCast(pWork, pBuild, pStnod->PStnodChild(pStdecl->m_iStnodInit), pStnod->m_pTin);
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

					auto pLtype = PLtypeFromPTin(pStnodChild->m_pTin);
					CalculateSizeAndAlign(pBuild, pLtype, &cBitSize, &cBitAlign);

					u64 cBit = (rword == RWORD_Sizeof) ? cBitSize : cBitAlign;

					auto pTinint = PTinDerivedCast<STypeInfoInteger *>(pStnod->m_pTin);
					if (!pTinint)
						return nullptr;

					auto pLval = PLvalConstantInt(pTinint->m_cBit, false, cBit / 8);

					CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
					pConst->m_pLval = pLval;
					pBuild->AddManagedVal(pConst);

					return pConst;
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

					CIRProcedure * pProc = pBuild->m_pProcCur;
					CIRBasicBlock * pBlockPost = nullptr;	
					CSTNode * pStnodCur = pStnod;

					while (pStnodCur)
					{
						// BB - should handle declarations inside conditional statement? ie, does it need a new block

						CSTNode * pStnodIf = pStnodCur;
						CSTNode * pStnodPred = pStnodIf->PStnodChild(0);

						STypeInfo * pTinBool = pStnodIf->m_pTin;

						CIRBasicBlock *	pBlockTrue = pBuild->PBlockCreate(pProc, "ifThen");
						CIRBasicBlock * pBlockFalse = nullptr;
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
						auto pValThen = PValGenerate(pWork, pBuild, pStnodIf->PStnodChild(1), VALGENK_Instance);
						(void) pBuild->PInstCreateBranch(pBlockPost);	
						pBlockTrue = pBuild->m_pBlockCur;	// could have changed during this' codegen

						if (pBlockFalse != pBlockPost)
						{
							pBuild->ActivateBlock(pBlockFalse);
						}

						if (pStnodElseChild)
						{
							auto pValThen = PValGenerate(pWork, pBuild, pStnodElseChild, VALGENK_Instance);
							pBlockFalse = pBuild->m_pBlockCur;	// could have changed during else's codegen

							(void) pBuild->PInstCreateBranch(pBlockPost);	
						}
					}

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_Else:
				EWC_ASSERT(false, "Else reserved word should be handled during codegen for if");
				return nullptr;
			case RWORD_For:
				{
					auto pStfor = pStnod->m_pStfor;
					if (!EWC_FVERIFY(pStfor, "bad for loop"))
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

						CIRValue * pValIterator = PValGenerate(pWork, pBuild, pStnodIterator, VALGENK_Reference);
						PInstGenerateAssignment(pWork, pBuild, pStnodIterator->m_pTin, pValIterator, pStnodInit);
					}

					CIRProcedure * pProc = pBuild->m_pProcCur;
					CIRBasicBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "fpred");
					CIRBasicBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "fbody");
					CIRBasicBlock * pBlockPost = pBuild->PBlockCreate(pProc, "fpost");
					CIRBasicBlock * pBlockIncrement = pBuild->PBlockCreate(pProc, "finc");
					(void) pBuild->PInstCreateBranch(pBlockPred);	

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
					(void) pBuild->PInstCreateBranch(pBlockIncrement);	
					pBuild->m_aryJumptStack.PopLast();

					pBuild->ActivateBlock(pBlockIncrement);
					CSTNode * pStnodIncrement = pStnodFor->PStnodChild(pStfor->m_iStnodIncrement);
					(void) PValGenerate(pWork, pBuild, pStnodIncrement, VALGENK_Instance);

					(void) pBuild->PInstCreateBranch(pBlockPred);	

					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_While:
				{
					if (pStnod->CStnodChild() < 2)
						return nullptr;

					EmitLocation(pWork, pBuild, pStnod->m_lexloc);

					CIRProcedure * pProc = pBuild->m_pProcCur;
					CIRBasicBlock *	pBlockPred = pBuild->PBlockCreate(pProc, "wpred");
					CIRBasicBlock *	pBlockBody = pBuild->PBlockCreate(pProc, "wbody");
					CIRBasicBlock * pBlockPost = pBuild->PBlockCreate(pProc, "wpost");
					(void) pBuild->PInstCreateBranch(pBlockPred);	

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

					(void) pBuild->PInstCreateBranch(pBlockPred);	


					pBuild->ActivateBlock(pBlockPost);

				} break;
			case RWORD_Return:
				{
					EmitLocation(pWork, pBuild, pStnod->m_lexloc);

					CIRValue * pValRhs = nullptr;
					if (pStnod->CStnodChild() == 1)
					{
						pValRhs = PValGenerate(pWork, pBuild, pStnod->PStnodChild(0), VALGENK_Instance);
					}

					(void) pBuild->PInstCreate(IROP_Ret, pValRhs, "RetTmp");

				} break;
			case RWORD_Break:
			case RWORD_Continue:
				{
					CIRBasicBlock * pBlock = nullptr;
					CString * pString = nullptr;
					if (pStnod->m_pStident)
					{
						pString = &pStnod->m_pStident->m_str;
					}

					if (rword == RWORD_Break)
					{
						for (size_t iJumpt = pBuild->m_aryJumptStack.C(); --iJumpt >= 0; )
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
						for (size_t iJumpt = pBuild->m_aryJumptStack.C(); --iJumpt >= 0; )
						{
							auto pJumpt = &pBuild->m_aryJumptStack[iJumpt];
							if (pJumpt->m_pBlockBreak && (pString == nullptr || pJumpt->m_strLabel == *pString))
							{
								pBlock = pJumpt->m_pBlockContinue;
								break;
							}
						}
					}

					if (pBlock)
					{
						(void) pBuild->PInstCreateBranch(pBlock);	
					}
					else
					{
						if (pString)
							EmitError(pWork, &pStnod->m_lexloc, "Could not loop with %s label matching '%s'", PCozFromRword(rword), *pString);
						else
							EmitError(pWork, &pStnod->m_lexloc, "Encountered %s statement outside of a loop or switch", PCozFromRword(rword));
					}

				} break;
			default:
				EWC_ASSERT(false, "Unhandled reserved word in code gen");
				break;
			}
		} break;
	case PARK_ProcedureCall:
		{
			auto pStnodTarget = pStnod->PStnodChildSafe(0);
			STypeInfoProcedure * pTinproc = (pStnodTarget) ? PTinRtiCast<STypeInfoProcedure *>(pStnodTarget->m_pTin) : nullptr;

			if (!EWC_FVERIFY(pTinproc, "expected type info procedure"))
				return nullptr;

			bool fIsDirectCall = FIsDirectCall(pStnod);
			auto pSym = pStnod->m_pSym;
			if (fIsDirectCall)
			{
				if (!EWC_FVERIFY(pStnod->m_pSym, "calling function without generated code"))
					return nullptr;


				PProcTryEnsure(pWork, pBuild, pSym);

				if (!pSym->m_pVal)
					return nullptr;
			}

			size_t cStnodArgs = pStnod->CStnodChild() - 1; // don't count the identifier

			EmitLocation(pWork, pBuild, pStnod->m_lexloc);

			CDynAry<LLVMValueRef> arypLvalArgs(pBuild->m_pAlloc, EWC::BK_Stack);
			for (size_t iStnodChild = 0; iStnodChild < cStnodArgs; ++iStnodChild)
			{
				CSTNode * pStnodArg = pStnod->PStnodChild((int)iStnodChild + 1);
				STypeInfo * pTinParam = pStnodArg->m_pTin;
				if (iStnodChild < pTinproc->m_arypTinParams.C())
				{
					pTinParam = pTinproc->m_arypTinParams[iStnodChild];
				}

				auto pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodArg, pTinParam);

				arypLvalArgs.Append(pValRhsCast->m_pLval);
				if (!EWC_FVERIFY(*arypLvalArgs.PLast(), "missing argument value"))
					return nullptr;
			}

			EWC_ASSERT(valgenk != VALGENK_Reference, "cannot return reference value for procedure call");

			CIRInstruction * pInst = pBuild->PInstCreateRaw(IROP_Call, nullptr, nullptr, "RetTmp");
			if (pInst->FIsError())
				return pInst;

			if (fIsDirectCall)
			{
				CIRProcedure * pProc = (CIRProcedure *)pSym->m_pVal;
				auto pLvalFunction = pProc->m_pLvalFunction;
				if (LLVMCountParams(pLvalFunction) != cStnodArgs)
				{
					if (!EWC_FVERIFY(pTinproc->m_fHasVarArgs, "unexpected number of arguments"))
						return nullptr;
				}

				s32 iLine;
				s32 iCol;
				CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLine, &iCol);

				pInst->m_pLval = LLVMBuildCall(
									pBuild->m_pLbuild,
									pProc->m_pLvalFunction,
									arypLvalArgs.A(),
									(u32)arypLvalArgs.C(),
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

				auto pValProcref = PValGenerate(pWork, pBuild, pStnodProcref, VALGENK_Instance);

				pInst->m_pLval = LLVMBuildCall(
									pBuild->m_pLbuild,
									pValProcref->m_pLval,
									arypLvalArgs.A(),
									(u32)arypLvalArgs.C(),
									"");
				if (pTinproc->m_callconv != CALLCONV_Nil)
				{
					LLVMSetInstructionCallConv(pInst->m_pLval, CallingconvFromCallconv(pTinproc->m_callconv));
				}
				return pInst;
			}
		}
	case PARK_Identifier:
		{
			if (!pStnod->m_pSym || !pStnod->m_pSym->m_pVal)
			{
				CString strName(StrFromIdentifier(pStnod));
				EmitError(pWork, &pStnod->m_lexloc, "INTERNAL ERROR: Missing value for symbol %s", strName.PCoz());
			}

			CIRValue * pVal = pBuild->PValFromSymbol(pStnod->m_pSym);
			if (EWC_FVERIFY(pVal, "unknown identifier in codegen") && valgenk != VALGENK_Reference && pVal->m_valk != VALK_ProcedureDefinition)
			{
				auto strPunyName = StrPunyEncode(pStnod->m_pSym->m_strName.PCoz());
				return pBuild->PInstCreate(IROP_Load, pVal, strPunyName.PCoz());
			}
			return pVal;
		}
	case PARK_MemberLookup:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);

			// check the symbol because we need to differentiate between enum namespacing and enum struct member.
			if (auto pSym = pStnodLhs->m_pSym)
			{
				if (pSym->m_pTin && pSym->m_pTin->m_tink == TINK_Enum)
				{
					EWC_ASSERT(pStnod->m_pOptype, "missing operand type for enum member");
					auto pTinenum = (STypeInfoEnum *)pStnod->m_pOptype->m_pTinLhs;
					if (EWC_FVERIFY(pStnod->m_pStval, "Enum constant lookup without value"))
					{
						auto pLval = PLvalFromEnumConstant(pBuild, pTinenum->m_pTinLoose, pStnod->m_pStval);
						if (!pLval)
							return nullptr;

						CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
						pBuild->AddManagedVal(pConst);
						pConst->m_pLval = pLval;
						return pConst;
					}
				}
			}
			STypeInfoStruct * pTinstruct = nullptr;
			auto pTinLhs = pStnodLhs->m_pTin;

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
			CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, valgenkLhs);

			CString strMemberName = StrFromIdentifier(pStnod->PStnodChild(1));

			if (pTinLhs && pTinLhs->m_tink == TINK_Array)
			{
				auto pTinary = (STypeInfoArray *)pTinLhs;
				ARYMEMB arymemb = ArymembLookup(strMemberName.PCoz());
				return PValFromArrayMember(pWork, pBuild, pValLhs, pTinary, arymemb);
			}
			if (pTinLhs && pTinLhs->m_tink == TINK_Literal)
			{
				auto pTinlit = (STypeInfoLiteral *)pTinLhs;
				if (EWC_FVERIFY(pTinlit->m_litty.m_litk == LITK_Array, "unexpected literal type in member lookup"))
				{
					u64 cElement;
					auto pLtype = PLtypeFromPTin(pTinlit, &cElement);
					if (!EWC_FVERIFY(pLtype, "couldn't find llvm type for declaration"))
						return nullptr;

					auto pInstAllocaLit = pBuild->PInstCreateAlloca(pLtype, cElement, "aryLit");

					// copy the literal into memory
					pBuild->PInstCreateStore(pInstAllocaLit, pValLhs);
					return pInstAllocaLit;
				}
			}
			
			auto pTypememb = PTypemembLookup(pTinstruct, strMemberName);
			if (!EWC_FVERIFY(pTypememb, "cannot find structure member %s", strMemberName.PCoz()))
				return nullptr;

			LLVMOpaqueValue * apLvalIndex[3] = {};
			int cpLvalIndex = 0;
			apLvalIndex[cpLvalIndex++] = LLVMConstInt(LLVMInt32Type(), 0, false);
			apLvalIndex[cpLvalIndex++] = LLVMConstInt(LLVMInt32Type(), pTinstruct->m_aryTypemembField.IFromP(pTypememb), false);
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

			CIRValue * pValLhs;
			CIRValue * pValIndex = PValGenerate(pWork, pBuild, pStnodIndex, VALGENK_Instance);
			if (!EWC_FVERIFY(pValIndex->m_pLval, "null index llvm value"))
				return nullptr;

			LLVMOpaqueValue * apLvalIndex[2] = {};
			TINK tinkLhs = pStnodLhs->m_pTin->m_tink;
			int cpLvalIndex = 0;
			if (tinkLhs == TINK_Array)
			{
				auto pTinary = (STypeInfoArray *)pStnodLhs->m_pTin;
				pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);

				if (pTinary->m_aryk != ARYK_Fixed)
				{
					pValLhs = PValFromArrayMember(pWork, pBuild, pValLhs, pTinary, ARYMEMB_Data);
				}
				else
				{
					apLvalIndex[cpLvalIndex++] = LLVMConstInt(LLVMInt32Type(), 0, false);
				}

				apLvalIndex[cpLvalIndex++] = pValIndex->m_pLval;
			}
			else if (tinkLhs == TINK_Pointer)
			{
				pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Instance);
				apLvalIndex[0] = pValIndex->m_pLval;
				cpLvalIndex = 1;
			}
			else
			{
				EWC_ASSERT(false, "unexpected type on left hand side of array element");
				return nullptr;
			}

			CIRInstruction * pInst = pBuild->PInstCreateGEP(pValLhs, apLvalIndex, cpLvalIndex, "aryGep");

			if (EWC_FVERIFY(pInst, "unknown identifier in codegen") && valgenk != VALGENK_Reference)
			{
				return pBuild->PInstCreate(IROP_Load, pInst, "aryLoad");
			}
			return pInst;
		} break;
	case PARK_LogicalAndOrOp:
		{
			auto pTinBool = pWork->m_pSymtab->PTinBuiltin("bool");
			auto pLtypeBool = PLtypeFromPTin(pTinBool);

			CIRProcedure * pProc = pBuild->m_pProcCur;
			auto pBlockTrue = pBuild->PBlockCreate(pProc, "predTrue");
			auto pBlockFalse = pBuild->PBlockCreate(pProc, "predFalse");
			auto pBlockPost = pBuild->PBlockCreate(pProc, "predPost");

			GeneratePredicate(pWork, pBuild, pStnod, pBlockTrue, pBlockFalse, pTinBool);

			pBuild->ActivateBlock(pBlockTrue);
			auto pValTrue = PValConstantInt(pBuild, 1, false, 1);
			(void) pBuild->PInstCreateBranch(pBlockPost);	

			pBuild->ActivateBlock(pBlockFalse);
			auto pValFalse = PValConstantInt(pBuild, 1, false, 0);
			(void) pBuild->PInstCreateBranch(pBlockPost);	

			pBuild->ActivateBlock(pBlockPost);
			auto pValPhi = pBuild->PInstCreatePhi(pLtypeBool, "predPhi"); 
			AddPhiIncoming(pValPhi, pValTrue, pBlockTrue);
			AddPhiIncoming(pValPhi, pValFalse, pBlockFalse);

			EWC_ASSERT(valgenk != VALGENK_Reference, "taking the address of a temporary (??)");
			return pValPhi;

		} break;
	case PARK_AssignmentOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);
			CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Reference);
			if (pStnod->m_tok == TOK('='))
			{
				auto pInstOp = PInstGenerateAssignment(pWork, pBuild, pStnodLhs->m_pTin, pValLhs, pStnodRhs);
				return pInstOp;
			}

			EmitLocation(pWork, pBuild, pStnod->m_lexloc);

			auto pTinOperandRhs = (EWC_FVERIFY(pStnod->m_pOptype, "missing operator types")) ? pStnod->m_pOptype->m_pTinRhs : pStnodLhs->m_pTin;

			CIRValue * pValLhsLoad = pBuild->PInstCreate(IROP_Load, pValLhs, "lhsLoad");
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pTinOperandRhs);
			EWC_ASSERT(pValRhsCast, "bad cast");

			SOpTypes optype(pStnodLhs->m_pTin, pStnodRhs->m_pTin, pStnod->m_pTin);
			auto pInstOp = PInstGenerateOperator(pBuild, pStnod->m_tok, &optype, pValLhsLoad, pValRhsCast);
			return pBuild->PInstCreateStore(pValLhs, pInstOp);
		}
	case PARK_AdditiveOp:
		{
			SOpTypes * pOptype = pStnod->m_pOptype;
			EWC_ASSERT(pOptype && pOptype->m_pTinLhs && pOptype->m_pTinRhs, "missing operand types for AdditiveOp");
			TINK tinkLhs = pOptype->m_pTinLhs->m_tink;
			TINK tinkRhs = pOptype->m_pTinRhs->m_tink;
			if (((tinkLhs == TINK_Pointer) & (tinkRhs == TINK_Integer)) | 
				((tinkLhs == TINK_Integer) & (tinkRhs == TINK_Pointer)))
			{
				//handle pointer arithmetic

				CSTNode * pStnodLhs = pStnod->PStnodChild(0);
				CIRValue * pValLhs = PValGenerate(pWork, pBuild, pStnodLhs, VALGENK_Instance);

				CSTNode * pStnodRhs = pStnod->PStnodChild(1);
				CIRValue * pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);

				CIRValue * pValPtr;
				CIRValue * pValIndex;
				if (pStnodLhs->m_pTin->m_tink == TINK_Pointer)
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

				auto pInstGep = pBuild->PInstCreateGEP(pValPtr, &pValIndex->m_pLval, 1, "ptrGep");
				return pInstGep; 
			}
		} // fallthrough
	case PARK_MultiplicativeOp:
	case PARK_ShiftOp:
	case PARK_BitwiseAndOrOp:
	case PARK_RelationalOp:
	case PARK_EqualityOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);

			SOpTypes * pOptype = pStnod->m_pOptype;
			EWC_ASSERT(pOptype && pOptype->FIsValid(), "missing operand types");
			STypeInfo * pTinOperandLhs = pOptype->m_pTinLhs;
			STypeInfo * pTinOperandRhs = pOptype->m_pTinRhs;
			CIRValue * pValLhsCast = PValGenerateRefCast(pWork, pBuild, pStnodLhs, pTinOperandLhs);
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pTinOperandRhs);
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
	
			if (LLVMTypeOf(pValLhsCast->m_pLval) != LLVMTypeOf(pValRhsCast->m_pLval))
			{
				EmitError(pWork, &pStnod->m_lexloc, "INTERNAL ERROR: bad cast");
				DumpLtype("Lhs", pValLhsCast);
				DumpLtype("Rhs", pValRhsCast);
				return nullptr;
			}

			auto pInstOp = PInstGenerateOperator(pBuild, pStnod->m_tok, pOptype, pValLhsCast, pValRhsCast);

			EWC_ASSERT(pInstOp, "%s operator unsupported in codegen", PCozFromTok(pStnod->m_tok));
			return pInstOp;
		}
	case PARK_PostfixUnaryOp:
	case PARK_UnaryOp:
		{
			CSTNode * pStnodOperand = pStnod->PStnodChild(0);

			if (pStnod->m_tok == TOK_Reference)
			{
				return PValGenerate(pWork, pBuild, pStnodOperand, VALGENK_Reference);
			}

			TOK tok = pStnod->m_tok;
			VALGENK valgenkUnary = ((tok == TOK_PlusPlus) | (tok == TOK_MinusMinus)) ? VALGENK_Reference : VALGENK_Instance;
			CIRValue * pValOperand = PValGenerate(pWork, pBuild, pStnodOperand, valgenkUnary);
			if (!EWC_FVERIFY(pValOperand != nullptr, "null operand"))
				return nullptr;

			if (!EWC_FVERIFY(pValOperand->m_pLval != nullptr, "null llvm operand"))
				return nullptr;

			EWC_ASSERT(pStnod->m_pOptype, "mising operator types in unary op");
			STypeInfo * pTinOutput = pStnod->m_pOptype->m_pTinResult;
			STypeInfo * pTinOperand = pStnodOperand->m_pTin;

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
	
			CIRValue * pValOp = nullptr;

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

			switch (pStnod->m_tok)
			{
			case '!':				
				{
					EWC_ASSERT(tink == TINK_Bool, "expected value cannot be cast to bool for '!' operand");

					// OPTIMIZE: could probably save an instruction here by not comparing (for the cast to bool)
					//  then inverting with a FNot

					CIRValue * pValOperandCast = PValCreateCast(pWork, pBuild, pValOperand, pTinOperand, pTinOutput);
					if (!pValOperandCast)
					{
						EmitError(pWork, &pStnod->m_lexloc, "INTERNAL ERROR: trying to codegen unsupported numeric cast.");
						return nullptr;
					}

					pValOp = pBuild->PInstCreate(IROP_Not, pValOperandCast, "NCmpEq");
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
			
					CIRInstruction * pInstLoad = pBuild->PInstCreate(IROP_Load, pValOperand, "IncLoad");
					CIRInstruction * pInstAdd = nullptr;
					switch (tink)
					{
					case TINK_Float:	
						{
							CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
							pBuild->AddManagedVal(pConst);
							pConst->m_pLval = PLvalConstantFloat(((STypeInfoFloat *)pTinOperand)->m_cBit, 1.0);

							if (pStnod->m_tok == TOK_PlusPlus)
								pInstAdd = pBuild->PInstCreate(IROP_GAdd, pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreate(IROP_GSub, pInstLoad, pConst, "gDec");

						} break;
					case TINK_Enum:
					case TINK_Integer:
						{
							auto pTinint = (STypeInfoInteger *)pTinOperand;
							CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
							pBuild->AddManagedVal(pConst);
							pConst->m_pLval = PLvalConstantInt(pTinint->m_cBit, fIsSigned, 1);

							if (pStnod->m_tok == TOK_PlusPlus)
								pInstAdd = pBuild->PInstCreate(IROP_NAdd, pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreate(IROP_NSub, pInstLoad, pConst, "gDec");
						} break;
					case TINK_Pointer:
						{
							int nDelta = (pStnod->m_tok == TOK_PlusPlus) ? 1 : -1;
							LLVMOpaqueValue * pLvalIndex = PLvalConstantInt(64, fIsSigned, nDelta);

							auto pValLoad = pBuild->PInstCreate(IROP_Load, pValOperand, "incLoad");
							pInstAdd = pBuild->PInstCreateGEP(pValLoad, &pLvalIndex, 1, "incGep");
						} break;
					default: EWC_ASSERT(false, "unexpected type '%s' for increment/decrement operator", PChzFromTink(tink));
					}

					auto pInstStore = pBuild->PInstCreateStore(pValOperand, pInstAdd);
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

			SDIFile * pDif = PDifEnsure(pWork, pBuild, pStnod->m_lexloc.m_strFilename);
			LLVMOpaqueValue * pLvalScope = PLvalFromDIFile(pBuild, pDif);

			s32 iLine, iCol;
			CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLine, &iCol);

			CreateDebugInfo(pWork, pBuild, pStnod, pSym->m_pTin);

			auto strPunyName = StrPunyEncode(pSym->m_strName.PCoz());
			(void) LLVMDIBuilderCreateTypeDef(
					pBuild->m_pDib,
					pSym->m_pTin->m_pLvalDIType,
					strPunyName.PCoz(),
				    pDif->m_pLvalFile,
					iLine,
				    pLvalScope);

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

CIRBasicBlock * CIRBuilder::PBlockCreate(CIRProcedure * pProc, const char * pChzName)
{
	CIRBasicBlock * pBlock = EWC_NEW(m_pAlloc, CIRBasicBlock) CIRBasicBlock(m_pAlloc);

	EWC_ASSERT(pProc->m_pLvalFunction, "expected function value");
	auto pLblock = LLVMAppendBasicBlock(pProc->m_pLvalFunction, pChzName);
	pBlock->m_pLblock = pLblock;

	if (EWC_FVERIFY(pProc, "missing procedure in PBlockCreate"))
	{
		pProc->m_arypBlockManaged.Append(pBlock);
	}
	return pBlock;
}

void CIRBuilder::ActivateProcedure(CIRProcedure * pProc, CIRBasicBlock * pBlock)
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

void CIRBuilder::ActivateBlock(CIRBasicBlock * pBlock)
{
	if (pBlock == m_pBlockCur)
		return;

	if (pBlock)
	{
		EWC_ASSERT(pBlock->m_pLblock);
		auto pLvalBlockParent = LLVMGetBasicBlockParent(pBlock->m_pLblock);

		EWC_ASSERT(pLvalBlockParent == m_pProcCur->m_pLvalFunction, "block with multiple parents?");
		LLVMPositionBuilder(m_pLbuild, pBlock->m_pLblock, nullptr);
	}

	m_pBlockCur = pBlock;
}

CIRProcedure * PProcCodegenInitializer(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodStruct)
{
	STypeInfoStruct * pTinstruct = PTinDerivedCast<STypeInfoStruct *>(pStnodStruct->m_pTin);
	if (!pTinstruct)
		return nullptr;

	LLVMOpaqueType * pLtypeVoid = LLVMVoidType();
	LLVMOpaqueType * apLtype[1];
	apLtype[0] = LLVMPointerType(PLtypeFromPTin(pTinstruct), 0);
	auto pLtypeFunction = LLVMFunctionType(pLtypeVoid, apLtype, 1, false);

	char aChName[128];
	EWC::SStringBuffer strbufName(aChName, EWC_DIM(aChName));
	auto strPunyName = StrPunyEncode(pTinstruct->m_strName.PCoz());
	FormatCoz(&strbufName, "_%s_INIT", strPunyName.PCoz());

	LLVMOpaqueValue * pLvalFunc = LLVMAddFunction(pBuild->m_pLmoduleCur, aChName, pLtypeFunction);
	pTinstruct->m_pLvalInitMethod = pLvalFunc;

	CAlloc * pAlloc = pBuild->m_pAlloc;
	CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);

	pProc->m_pLvalFunction = pLvalFunc;
	pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, aChName);
	
	auto pBlockPrev = pBuild->m_pBlockCur;
	auto pProcPrev = pBuild->m_pProcCur;

	pBuild->ActivateProcedure(pProc, pProc->m_pBlockEntry);

	{ // create debug info
		CreateDebugInfo(pWork, pBuild, pStnodStruct, pTinstruct);

		int cpTinParam = 1;
		LLVMValueRef apLvalParam[1];

		u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
		u64 cBitAlign = cBitSize;
		apLvalParam[0] = LLVMDIBuilderCreatePointerType(pBuild->m_pDib, pTinstruct->m_pLvalDIType, cBitSize, cBitAlign, "");

		auto pLvalDIType = LLVMDIBuilderCreateFunctionType(pBuild->m_pDib, apLvalParam, cpTinParam, cBitSize, cBitAlign);

		pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
										pWork,
										pBuild,
										aChName,
										aChName,
										pStnodStruct,
										pStnodStruct,
										pLvalDIType,
										pProc->m_pLvalFunction);

		s32 iLine, iCol;
		CalculateLinePosition(pWork, &pStnodStruct->m_lexloc, &iLine, &iCol);

		pProc->m_pLvalDebugLocCur = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pProc->m_pLvalDIFunction);
		LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pProc->m_pLvalDebugLocCur);
	}

	// load our 'this' argument
	int cpLvalParams = LLVMCountParams(pProc->m_pLvalFunction);
	EWC_ASSERT(cpLvalParams == 1, "only expected 'this' parameter");

	auto apLvalParam = (LLVMValueRef *)alloca(sizeof(LLVMValueRef) * cpLvalParams);
	LLVMGetParams(pProc->m_pLvalFunction, apLvalParam);
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
				CSTDecl * pStdecl = pStnodDecl->m_pStdecl;
				pStnodInit = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
			}
		}

		auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

		if (pStnodInit)
		{
			if (pStnodInit->m_park != PARK_Uninitializer)
			{
				auto pInstGEP = pBuild->PInstCreateGEP(pArgThis, apLvalIndex, 2, "initGEP");

				CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodInit, pTypememb->m_pTin);
				pBuild->PInstCreateStore(pInstGEP, pValRhsCast);
			}
		}
		else
		{
			auto pInstGEP = pBuild->PInstCreateGEP(pArgThis, apLvalIndex, 2, "initGEP");
			(void)PValInitializeToDefault(pWork, pBuild, pTypememb->m_pTin, pInstGEP);
		}
	}

	LLVMBuildRetVoid(pBuild->m_pLbuild);
	pBuild->ActivateProcedure(pProcPrev, pBlockPrev);

	pBuild->m_arypProcVerify.Append(pProc);
	return pProc;
}

CIRProcedure * PProcCodegenPrototype(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod)
{
	CSTProcedure * pStproc = pStnod->m_pStproc;
	CSTNode * pStnodParamList = nullptr;
	CSTNode * pStnodReturn = nullptr;
	CSTNode * pStnodName = nullptr;
	CSTNode * pStnodAlias = nullptr;
	CSTNode * pStnodBody = nullptr;
	if (EWC_FVERIFY(pStproc, "Encountered procedure without CSTProcedure"))
	{
		pStnodParamList = pStnod->PStnodChildSafe(pStproc->m_iStnodParameterList);
		pStnodReturn = pStnod->PStnodChildSafe(pStproc->m_iStnodReturnType);
		pStnodName = pStnod->PStnodChildSafe(pStproc->m_iStnodProcName);
		pStnodAlias = pStnod->PStnodChildSafe(pStproc->m_iStnodForeignAlias);
		pStnodBody = pStnod->PStnodChildSafe(pStproc->m_iStnodBody);
	}

	bool fHasVarArgs = false;
	CDynAry<LLVMTypeRef> arypLtype(pBuild->m_pAlloc, EWC::BK_CodeGen);
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

			auto pLtype = PLtypeFromPTin(pStnodDecl->m_pTin);
			if (EWC_FVERIFY(pLtype, "Could not compute LLVM type for parameter"))
			{
				arypLtype.Append(pLtype);
			}
		}
	}

	LLVMOpaqueType * pLtypeReturn = nullptr;
	if (pStnodReturn)
	{
		pLtypeReturn = PLtypeFromPTin(pStnodReturn->m_pTin);
		EWC_FVERIFY(pLtypeReturn, "Could not compute LLVM type for return type");
	}
	if (!pLtypeReturn)
	{
		pLtypeReturn = LLVMVoidType();
	}

	auto pTinproc = PTinRtiCast<STypeInfoProcedure *>(pStnod->m_pTin);

	char aCh[256];
	const char * pChzMangled = PChzVerifyAscii(pTinproc->m_strMangled.PCoz());

	EWC_ASSERT(pTinproc, "Exected procedure type");

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
		pBuild->GenerateUniqueName("__AnnonFunc__", aCh, EWC_DIM(aCh));
		pChzMangled = aCh;
	}

	CAlloc * pAlloc = pBuild->m_pAlloc;
	CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);

	auto pLtypeFunction = LLVMFunctionType(pLtypeReturn, arypLtype.A(), (u32)arypLtype.C(), fHasVarArgs);
	pProc->m_pLvalFunction = LLVMAddFunction(pBuild->m_pLmoduleCur, pChzMangled, pLtypeFunction);
	pProc->m_pLval = pProc->m_pLvalFunction; // why is this redundant?

	if (pTinproc->m_callconv != CALLCONV_Nil)
	{
		LLVMSetFunctionCallConv(pProc->m_pLvalFunction, CallingconvFromCallconv(pTinproc->m_callconv));
	}

	switch (pTinproc->m_inlinek)
	{
		case INLINEK_NoInline:		LLVMAddFunctionAttr(pProc->m_pLvalFunction, LLVMNoInlineAttribute);	break;
		case INLINEK_AlwaysInline:	LLVMAddFunctionAttr(pProc->m_pLvalFunction, LLVMAlwaysInlineAttribute);	break;
		default: break;
	}

	if (!pStproc->m_fIsForeign)
	{
		pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, pChzMangled);

		if (EWC_FVERIFY(pTinproc))
		{
			if (pTinproc->m_pLvalDIType == nullptr)
			{
				CreateDebugInfo(pWork, pBuild, pStnod, pTinproc);
			}

			pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
											pWork,
											pBuild,
											pTinproc->m_strName.PCoz(),
											PChzVerifyAscii(pTinproc->m_strMangled.PCoz()),
											pStnod,
											pStnodBody,
											pTinproc->m_pLvalDIType,
											pProc->m_pLvalFunction);

			s32 iLine, iCol;
			CalculateLinePosition(pWork, &pStnodBody->m_lexloc, &iLine, &iCol);

			pProc->m_pLvalDebugLocCur = LLVMCreateDebugLocation(pBuild->m_pLbuild, iLine, iCol, pProc->m_pLvalDIFunction);
		}
	}

	if (EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check"))
	{
		pStnod->m_pSym->m_pVal = pProc;
	}

	auto pBlockPrev = pBuild->m_pBlockCur;
	auto pProcPrev = pBuild->m_pProcCur;

	pBuild->ActivateProcedure(pProc, (pStproc->m_fIsForeign) ? pBlockPrev : pProc->m_pBlockEntry);
	if (pStnodParamList)
	{
		// set up llvm argument values for our formal parameters
		
		int cpStnodParam = pStnodParamList->CStnodChild();
		int cpLvalParams = LLVMCountParams(pProc->m_pLvalFunction);

		CAlloc * pAlloc = pBuild->m_pAlloc;
		LLVMValueRef * appLvalParams = (LLVMValueRef*)pAlloc->EWC_ALLOC_TYPE_ARRAY(LLVMValueRef, cpLvalParams);
		LLVMGetParams(pProc->m_pLvalFunction, appLvalParams);
		
		SDIFile * pDif = PDifEnsure(pWork, pBuild, pStnod->m_lexloc.m_strFilename);

		LLVMValueRef * ppLvalParam = appLvalParams;
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
		{
			CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
			if (pStnodParam->m_park == PARK_VariadicArg)
				continue;

			EWC_ASSERT((ppLvalParam - appLvalParams) < cpLvalParams, "parameter count mismatch");

			if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
			{
				auto strArgName = StrPunyEncode(pStnodParam->m_pSym->m_strName.PCoz());
				LLVMSetValueName(*ppLvalParam, strArgName.PCoz());

				CIRArgument * pArg = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
				pArg->m_pLval = *ppLvalParam;
				pBuild->AddManagedVal(pArg);

				if (!pStproc->m_fIsForeign)
				{
					auto pInstAlloca = pBuild->PInstCreateAlloca(arypLtype[ipStnodParam], 1, strArgName.PCoz());
					pStnodParam->m_pSym->m_pVal = pInstAlloca;

					(void)pBuild->PInstCreateStore(pStnodParam->m_pSym->m_pVal, pArg);

					s32 iLine;
					s32 iCol;
					CalculateLinePosition(pWork, &pStnodParam->m_lexloc, &iLine, &iCol);

					CreateDebugInfo(pWork, pBuild, pStnod, pStnodParam->m_pTin);
					
					auto pLvalDIVariable = LLVMDIBuilderCreateParameterVariable(
											pBuild->m_pDib,
											pProc->m_pLvalDIFunction, 
											strArgName.PCoz(),
											ipStnodParam + 1,
											pDif->m_pLvalFile,
											iLine,
											pStnodParam->m_pTin->m_pLvalDIType,
											true,
											0);

					(void) LLVMDIBuilderInsertDeclare(
							pBuild->m_pDib,
							pInstAlloca->m_pLval,
							pLvalDIVariable,
							pProc->m_pLvalDIFunction,
							iLine, 
							iCol, 
							pBuild->m_pBlockCur->m_pLblock);
				}
			}
			++ppLvalParam;
		}

		pAlloc->EWC_FREE(appLvalParams);
	}
	pBuild->ActivateProcedure(pProcPrev, pBlockPrev);

	return pProc;
}


void CodeGenEntryPoint(
	CWorkspace * pWork,
	CIRBuilder * pBuild, 
	CSymbolTable * pSymtabTop,
	CAry<CWorkspace::SEntry> * paryEntry,
	CAry<int> * paryiEntryOrder)
{
	CAlloc * pAlloc = pWork->m_pAlloc;
	int * piEntryMax = paryiEntryOrder->PMac();
	for (int * piEntry = paryiEntryOrder->A(); piEntry != piEntryMax; ++piEntry)
	{
		CWorkspace::SEntry *pEntry = &(*paryEntry)[*piEntry];
		CSTNode * pStnod = pEntry->m_pStnod;

		EWC_ASSERT(pBuild->m_pProcCur == nullptr, "expected null procedure for entry point.");

		bool fGlobalTypeDeclaration = false;
		bool fImplicitFunction = (pWork->m_globmod == GLOBMOD_UnitTest);

		switch (pStnod->m_park)
		{
			case PARK_StructDefinition:		fGlobalTypeDeclaration = true;	break;
			case PARK_EnumDefinition:		fGlobalTypeDeclaration = true;	break;
			case PARK_Typedef:				fGlobalTypeDeclaration = true;	break;
			case PARK_Nop:					fGlobalTypeDeclaration = true;	break;
			case PARK_ProcedureDefinition:	fImplicitFunction = false;		break;
		}

		char aCh[128];
		if (fGlobalTypeDeclaration)
		{
			continue;
		}
		else if (fImplicitFunction)
		{
			CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);
			pEntry->m_pProc = pProc;

			pBuild->GenerateUniqueName("__AnonFunc__", aCh, EWC_DIM(aCh));

			auto pLtypeFunction = LLVMFunctionType(LLVMVoidType(), nullptr, 0, false);
			pProc->m_pLvalFunction = LLVMAddFunction(pBuild->m_pLmoduleCur, aCh, pLtypeFunction);

			pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, aCh);

			s32 iLineBody, iColBody;
			CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLineBody, &iColBody);

			u64 cBitSize = LLVMPointerSize(pBuild->m_pTargd) * 8;
			u64 cBitAlign = cBitSize;
			auto pLvalDIFunctionType = LLVMDIBuilderCreateFunctionType(pBuild->m_pDib, nullptr, 0, cBitSize, cBitAlign);

			pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
										pWork,
										pBuild,
										aCh,
										nullptr,
										pStnod,
										pStnod,
										pLvalDIFunctionType,
										pProc->m_pLvalFunction);

			(void) PValGenerateMethodBody(pWork, pBuild, pProc, pStnod, true);
		}
		else
		{
			PValGenerate(pWork, pBuild, pStnod, VALGENK_Instance);

			if (pStnod->m_park == PARK_ProcedureDefinition)
			{
				CIRProcedure * pProc = (CIRProcedure *)pStnod->m_pSym->m_pVal;
				if (!EWC_FVERIFY(pProc && pProc->m_valk, "Expected procedure"))
					return;

				pEntry->m_pProc = pProc;
			}
		}
	}

	LLVMDIBuilderFinalize(pBuild->m_pDib);

	LLVMBool fHaveAnyFailed = false;
	CIRProcedure ** ppProcVerifyEnd = pBuild->m_arypProcVerify.PMac();
	for (CIRProcedure ** ppProcVerifyIt = pBuild->m_arypProcVerify.A(); ppProcVerifyIt != ppProcVerifyEnd; ++ppProcVerifyIt)
	{
		auto pProc = *ppProcVerifyIt;
		LLVMBool fFunctionFailed = LLVMVerifyFunction(pProc->m_pLvalFunction, LLVMPrintMessageAction);
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
		pBuild->PrintDump();
		EmitError(pWork, nullptr, "Code generation for entry point is invalid");
	}
}

void TestUniqueNames(CAlloc * pAlloc)
{
	size_t cbFreePrev = pAlloc->CB();
	{
		SErrorManager errman;
		CWorkspace work(pAlloc, &errman);

		CIRBuilder build(&work, nullptr, "");

		const char * pChzIn;
		char aCh[128];

		pChzIn = "funcName";
		build.GenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual(pChzIn, aCh), "bad unique name");

		build.GenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("funcName1", aCh), "bad unique name");

		pChzIn = "funcName20";
		build.GenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("funcName20", aCh), "bad unique name");

		pChzIn = "234";
		build.GenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("234", aCh), "bad unique name");

		pChzIn = "test6000";
		build.GenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("test6000", aCh), "bad unique name");

		pChzIn = "test6000";
		build.GenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(FAreCozEqual("test6001", aCh), "bad unique name");
	}

	size_t cbFreePost = pAlloc->CB();
	EWC_ASSERT(cbFreePrev == cbFreePost, "memory leak testing unique names");
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

void CompileToObjectFile(CWorkspace * pWork, CIRBuilder * pBuild, const char * pChzFilenameIn)
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
		EmitError(pWork, nullptr, "Error generating object file\n%s", pChzError);
		LLVMDisposeMessage(pChzError);
	}

	LLVMDisposeMessage(pChzTriple);
	pChzTriple = nullptr;
}

void InitLLVM()
{
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
	//LLVMInitializeCodeGen(*Registry);
	//LLVMInitializeLoopStrengthReducePass(*Registry);
	//LLVMInitializeLowerIntrinsicsPass(*Registry);
	//LLVMInitializeUnreachableBlockElimPass(*Registry);
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

		// skip a unicode byte order mark
		const u8 * pCozFileBody = (u8 *)pFile->m_pChzFileBody;
		// utf8 BOM
		if (pCozFileBody[0] == 0xEF && pCozFileBody[1] == 0xBB && pCozFileBody[2] == 0xBF)
		{
			pCozFileBody += 3;
		}

		printf("Parsing %s\n", pFile->m_strFilename.PCoz());
		BeginParse(pWork, &lex, (char *)pCozFileBody);
		lex.m_pCozFilename = pFile->m_strFilename.PCoz();

		ParseGlobalScope(pWork, &lex, true);
		EWC_ASSERT(pWork->m_aryEntry.C() > 0);

		EndParse(pWork, &lex);

	}

	if (pWork->m_pErrman->m_cError == 0)
	{
		printf("Type Check:\n");
		PerformTypeCheck(pWork->m_pAlloc, pWork->m_pErrman, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

		if (pWork->m_pErrman->m_cError == 0)
		{

#if EWC_X64
			printf("Code Generation (x64):\n");
#else
			printf("Code Generation (x86):\n");
#endif
			CIRBuilder build(pWork, &pWork->m_arypValManaged, pChzFilenameIn);
			

			CodeGenEntryPoint(pWork, &build, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

			CompileToObjectFile(pWork, &build, pChzFilenameIn);

			if (grfcompile.FIsSet(FCOMPILE_PrintIR))
			{
				build.PrintDump();
			}
		}
	}

	bool fSuccess = (pWork->m_pErrman->m_cError == 0);
	if (!fSuccess)
	{
		printf("Compilation failed: %d errors\n", pWork->m_pErrman->m_cError);
	}

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		CWorkspace::SFile * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != CWorkspace::FILEK_Source)
			continue;

		if (pFile->m_pChzFileBody)
		{
			pWork->m_pAlloc->EWC_DELETE((void *)pFile->m_pChzFileBody);
			pFile->m_pChzFileBody = nullptr;
		}

		if (pFile->m_pDif)
		{
			pWork->m_pAlloc->EWC_DELETE(pFile->m_pDif);
			pFile->m_pDif = nullptr;
		}
	}

	return fSuccess;
}

void AssertTestCodeGen(
	CWorkspace * pWork,
	const char * pChzIn)
	//const char * pChzOut)
{
	const char * s_pChzUnitTestFilename = "unit.test";

	SLexer lex;
	BeginWorkspace(pWork);
	auto pFile = pWork->PFileEnsure(s_pChzUnitTestFilename, CWorkspace::FILEK_Source);
	pFile->m_pChzFileBody = pChzIn;

	BeginParse(pWork, &lex, pChzIn, s_pChzUnitTestFilename);

	EWC_ASSERT(pWork->m_pErrman->m_cError == 0, "parse errors detected");
	pWork->m_pErrman->Clear();

	ParseGlobalScope(pWork, &lex, true);
	EWC_ASSERT(pWork->m_aryEntry.C() > 0);

	EndParse(pWork, &lex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pErrman, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);
	{
		CIRBuilder build(pWork, &pWork->m_arypValManaged, "");
		CodeGenEntryPoint(pWork, &build, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);
	}

	if (pFile->m_pDif)
	{
		pWork->m_pAlloc->EWC_DELETE(pFile->m_pDif);
		pFile->m_pDif = nullptr;
	}

	EndWorkspace(pWork);
}

void TestCodeGen()
{
	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 200];
	CAlloc alloc(aB, sizeof(aB));

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);
	work.m_globmod = GLOBMOD_UnitTest;

	TestUniqueNames(&alloc);

	const char * pChzIn;

	pChzIn = "{ n1 := 6; n := 2 * n1}";		// procedure reference declaration
	AssertTestCodeGen(&work, pChzIn);

	//pChzIn = " g := 2.2; pG := &g; "; // not handling this properly as globals
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "fooFunc :: (n: s32) -> s64 { return 2 }     func: (n: s32)->s64 = fooFunc";		// procedure reference declaration
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ pFunc: (n: s32)->s64;   pFunc(33) }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{apFunc: [4] (n: s32)->s64;   apFunc[1](33) }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ g := 2.2; pG := &g; pN := cast(& int) pG }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "aN := {:int: 2, 4, 5} ";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ aN : [2] int; aNUnsized : [] int = aN }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ ENUMK :: enum s32 { Ick : 1, Foo, Bah : 3 } n := ENUMK.Ick }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ SomeConst :: 0xFF; n:=SomeConst }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "SFoo :: struct { m_n : s32 } { foo : SFoo; pFoo := &foo; pFoo.m_n = 2 } ";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "SFoo :: struct { m_n : s32; m_g := 1.2 } foo : SFoo";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ aN : [4] s32;  pN : & s32; fTest := aN.data == pN }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & s32; ++pN; --pN }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & s32; pN = pN + 2; pN = pN - 2 }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ aN : [4] s32; n := aN[2] }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & s32; n := pN[0] }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ i:=0; while i < 5 { i = i + 1 } }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "printf :: (pCh : & u8, ..) -> s32 #foreign";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pChz := \"testString\" }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & int; n := 2;   if (!pN) { pN = &n }  if pN { @pN = 2 } }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ n : int = 32; pN : & int; pN = &n; n2 := @pN; @pN = 6}";
	AssertTestCodeGen(&work, pChzIn);
	//pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n) nRet = 4; else nRet =1; return nRet }";
	//pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n) nRet = 4; return nRet }";
	pChzIn =	"Foo :: (n : s64) -> int { nRet : int = 5; if (n == 4) { nRet = 4 } else if (n == 3) { nRet =3 } else { nRet = 2 } return nRet }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ i:=5 + 2 * 3 }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"AddNums :: (nA : int, nB : int) -> int { return nA + nB }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"AddLocal :: (nA : int) -> int { nFoo : int; nFoo = 2; return nA + nFoo }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "Foo :: (nA : s8, nB : s8) -> bool { return (nA < nB) == (nB >= nA) }";
	AssertTestCodeGen(&work, pChzIn);

	//pChzIn =	"GetTwo :: ()-> int { return 2; } AddTwo :: (nA : int) -> int { return nA + GetTwo() }";
	//pChzIn =	"AddTwo :: (nA : int) -> int { return nA + GetTwo(); } GetTwo :: ()-> int { return 2 }";
	pChzIn =	"Foo :: () -> int { return Bah() } Bah :: ()-> int { return Foo() }";
	AssertTestCodeGen(&work, pChzIn);

	StaticShutdownStrings(&allocString);
}
