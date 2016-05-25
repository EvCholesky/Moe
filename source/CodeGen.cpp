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
#include "JaiParse.h"
#include "JaiTypes.h"
#include "Workspace.h"

using namespace EWC;

#pragma warning ( push )
#pragma warning(disable : 4141)
#include "llvm-c/Analysis.h"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/Support/Dwarf.h"
#pragma warning ( pop )

#include "MissingLlvmC/llvmcDIBuilder.h"
#include <stdio.h>

CIRProcedure * PProcCodegenInitializer(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodStruct);
CIRProcedure * PProcCodegenPrototype(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnod);

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

static inline size_t CChGetTypeString(LLVMOpaqueType * pLtype, char * pCh, const char * pChEnd)
{
	switch (LLVMGetTypeKind(pLtype))
	{
	case LLVMIntegerTypeKind:
		{
			switch (LLVMGetIntTypeWidth(pLtype))
			{
			case 1: return CChCopy("bool", pCh, pChEnd-pCh);
			case 8: return CChCopy("s8", pCh, pChEnd-pCh);
			case 16: return CChCopy("s16", pCh, pChEnd-pCh);
			case 32: return CChCopy("s32", pCh, pChEnd-pCh);
			case 64: return CChCopy("s64", pCh, pChEnd-pCh);
			}
		}
	case LLVMFloatTypeKind:		return CChCopy("f32", pCh, pChEnd-pCh);
	case LLVMDoubleTypeKind:	return CChCopy("f64", pCh, pChEnd-pCh);
	case LLVMVoidTypeKind:		return CChCopy("void", pCh, pChEnd-pCh);
	case LLVMPointerTypeKind:
		{
			size_t cChPrefix = CChCopy("* ", pCh, pChEnd-pCh);
			LLVMOpaqueType * pLtypeElement = LLVMGetElementType(pLtype);
			return cChPrefix + CChGetTypeString(pLtypeElement, pCh+cChPrefix, pChEnd);
		}
	}

	return CChCopy("unknown", pCh, pChEnd-pCh);
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
,m_arypInst(pAlloc)
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

			LLVMOpaqueType * pLtype = LLVMStructCreateNamed(LLVMGetGlobalContext(), pTinstruct->m_strName.PChz());
			pTinstruct->m_pLtype = pLtype;

			int cTypemembField = (int)pTinstruct->m_aryTypemembField.C();
			auto apLtypeMember = (LLVMTypeRef *)(alloca(sizeof(LLVMTypeRef) * cTypemembField));
			LLVMTypeRef * ppLtypeMember = apLtypeMember;

			auto pTypemembMac = pTinstruct->m_aryTypemembField.PMac();
			for ( auto pTypememb = pTinstruct->m_aryTypemembField.A(); pTypememb != pTypemembMac; ++pTypememb)
			{
				auto pLtypeMember = PLtypeFromPTin(pTypememb->m_pTin);
				EWC_ASSERT(pLtypeMember, "failed to compute type for structure member %s", pTypememb->m_strName.PChz());
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

void PathSplitDestructive(char * pChzFull, size_t cCh, const char ** ppChzPath, const char ** ppChzFile)
{
	char * pChzPath = ".";
	char * pChzFile = pChzFull;

	char * pChLastSlash = nullptr;
	
	char * pChEnd = pChzFull + cCh;
	for (char * pChIt = pChzFull; pChIt != pChEnd; ++pChIt)
	{
		if ((*pChIt == '/') | (*pChIt == '\\'))
		{
			pChLastSlash = pChIt;
		}

		if (*pChIt == '\0')
			break;
	}

	if (pChLastSlash)
	{
		*pChLastSlash = '\0';
		pChzPath = pChzFull;
		pChzFile = pChLastSlash + 1;
	}

	*ppChzPath = pChzPath;
	*ppChzFile = pChzFile;
}

SDIFile * PDifEnsure(CWorkspace * pWork, CIRBuilder * pBuild, const CString & strFilename)
{
	auto pFile = pWork->PFileLookup(strFilename.Hv(), CWorkspace::FILEK_Source);
	if (!EWC_FVERIFY(pFile, "bad file lookup in PDifEnsure"))
		return nullptr;

	if (pFile->m_pDif)
		return pFile->m_pDif;

	auto pDif = EWC_NEW(pWork->m_pAlloc, SDIFile) SDIFile();
	pFile->m_pDif = pDif;

	size_t cBFilename = pFile->m_strFilename.CB() + 1;
	char * pChzCopy = (char *)alloca(sizeof(char) * cBFilename);
	CChCopy(pFile->m_strFilename.PChz(), pChzCopy, cBFilename);

	const char * pChzPath;	
	const char * pChzFile;	
	PathSplitDestructive(pChzCopy, cBFilename, &pChzPath, &pChzFile);

	pDif->m_pLvalScope = pBuild->m_pLvalCompileUnit;
	pDif->m_pLvalFile = LLVMDIBuilderCreateFile(pBuild->m_pDib, pChzFile, pChzPath);

	pDif->m_aryLvalScopeStack.SetAlloc(pWork->m_pAlloc);
	pDif->m_aryLvalScopeStack.Append(pDif->m_pLvalFile);

	return pDif;
}


LLVMOpaqueValue * PLvalFromDIFile(CIRBuilder * pBuild, SDIFile * pDif)
{
	if (pDif->m_aryLvalScopeStack.FIsEmpty())
	{
		return pBuild->m_pLvalCompileUnit;
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

	LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(iLine, iCol, pLvalScope);
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

	LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(iLine, iCol, pLvalScope);
	LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pLvalLoc);

	if (piLine) *piLine = iLine;
	if (piCol) *piCol = iCol;

	return pDif;
}

void CalculateSizeAndAlign(CIRBuilder * pBuild, LLVMOpaqueType * pLtype, u64 * pCBitSize, u64 *pCBitAlign)
{
	*pCBitSize = LLVMSizeOfTypeInBits(pBuild->m_pTargd, pLtype);
	*pCBitAlign = *pCBitSize;
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

	auto pDif = PDifEnsure(pWork, pBuild, pStnodBody->m_lexloc.m_strFilename.PChz());
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

static inline void CreateDebugInfo(CWorkspace * pWork, CIRBuilder * pBuild, CSTNode * pStnodRef, STypeInfo * pTin)
{
	if (pTin->m_pLvalDIType)
		return;

	const char * pChzName = pTin->m_strName.PChz();
	auto pDib = pBuild->m_pDib;

	switch (pTin->m_tink)
	{
	case TINK_Integer: 
		{
			auto pTinint = PTinRtiCast<STypeInfoInteger *>(pTin);
			unsigned nDwarf = (pTinint->m_fIsSigned) ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned;
			pTin->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, pChzName, pTinint->m_cBit, pTinint->m_cBit, nDwarf);
		} break;
	case TINK_Float:
		{
			auto pTinfloat = PTinRtiCast<STypeInfoFloat *>(pTin);
			unsigned nDwarf = llvm::dwarf::DW_ATE_float;
			pTin->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, pChzName, pTinfloat->m_cBit, pTinfloat->m_cBit, nDwarf);
		} break;
	case TINK_Bool:
		{
			unsigned nDwarf = llvm::dwarf::DW_ATE_boolean;
			pTin->m_pLvalDIType = LLVMDIBuilderCreateBasicType(pDib, pChzName, 1, 1, nDwarf);
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
			pTin->m_pLvalDIType = LLVMDIBuilderCreatePointerType(pDib, pTinptr->m_pTinPointedTo->m_pLvalDIType, cBitSize, cBitAlign, pChzName);
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

			pTin->m_pLvalDIType = LLVMDIBuilderCreateFunctionType(pDib, apLvalParam, cpTinParam);

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
					apLvalSubscript[0] = LLVMDIBuilderGetOrCreateRange(pDib, 0, pTinary->m_c-1);
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

			const char * pChzUniqueName = pTinstruct->m_strName.PChz(); // BB - should actually make unique name!
			unsigned nFlags = 0;

			auto pLvalDicomp = LLVMDIBuilderCreateReplacableComposite(
									pDib, 
									llvm::dwarf::DW_TAG_structure_type,
									pLvalScope, 
									pTinstruct->m_strName.PChz(), 
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

			for (int iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
			{
				auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

				auto pLtypeElement = PLtypeFromPTin(pTypememb->m_pTin);
				u64 cBitSizeMember, cBitAlignMember;
				CalculateSizeAndAlign(pBuild, pLtypeStruct, &cBitSizeMember, &cBitAlignMember);

				s32 iLineMember, iColMember;
				CalculateLinePosition(pWork, &pTypememb->m_pStnod->m_lexloc, &iLineMember, &iColMember);

				u64 dBitMembOffset = LLVMOffsetOfElement(pBuild->m_pTargd, pLtypeStruct, iTypememb);

				unsigned nFlagsMember = 0;
				apLvalMember[iTypememb] = LLVMDIBuilderCreateMemberType(
											pDib,
											pLvalDicomp,
											pTypememb->m_strName.PChz(),
											pDif->m_pLvalFile,
											iLineMember,
											cBitSizeMember,
											cBitAlignMember,
											dBitMembOffset,
											nFlagsMember,
											pLvalDicomp);
			}

			LLVMDIBuilderReplaceCompositeElements(pDib, &pLvalDicomp, apLvalMember, cTypememb);
			pTin->m_pLvalDIType = pLvalDicomp;

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

				apLvalConstant[iTinecon] = LLVMDIBuilderCreateEnumerator(pDib, pTinecon->m_strName.PChz(), nValue);
			}

			
			u64 cBitSize, cBitAlign;
			auto pLtypeLoose = PLtypeFromPTin(pTinenum->m_pTinLoose);
			CalculateSizeAndAlign(pBuild, pLtypeLoose, &cBitSize, &cBitAlign);
			pTin->m_pLvalDIType = LLVMDIBuilderCreateEnumerationType(
								    pDib, 
									pLvalScope, 
									pTinenum->m_strName.PChz(), 
									pDif->m_pLvalFile,
								    iLine,
									cBitSize,
									cBitAlign,
								    apLvalConstant, 
									cTinecon,
									pTinenum->m_pTinLoose->m_pLvalDIType);
		} break;
	case TINK_Literal:
	case TINK_Void:
	case TINK_String:
		break;
	default: EWC_ASSERT(false, "unhandled type info kind in debug info");
	}
}



// Builder class Methods
CIRBuilder::CIRBuilder(EWC::CAlloc * pAlloc, EWC::CDynAry<CIRValue *> *	parypValManaged, const char * pChzFilename)
:m_pBerrctx(nullptr)
,m_pLmoduleCur(nullptr)
,m_pLbuild(nullptr)
,m_pTargd (nullptr)
,m_pDib(nullptr)
,m_pLvalCompileUnit(nullptr)
,m_pLvalScope(nullptr)
,m_pLvalFile(nullptr)
,m_pAlloc(pAlloc)
,m_inspt()
,m_pProcCur(nullptr)
,m_pBlockCur(nullptr)
,m_parypValManaged(parypValManaged)
,m_hashHvNUnique(pAlloc)
{ 
	m_pLbuild = LLVMCreateBuilder();
	m_pLmoduleCur = LLVMModuleCreateWithName("JaiModule");

	if (!pChzFilename || *pChzFilename == '\0')
	{
		pChzFilename = "stub";
	}

	size_t cBFilename = CCh(pChzFilename) + 1;
	char * pChzCopy = (char *)alloca(sizeof(char) * cBFilename);
	CChCopy(pChzFilename, pChzCopy, cBFilename);

	const char * pChzPath;	
	const char * pChzFile;	
	PathSplitDestructive(pChzCopy, cBFilename, &pChzPath, &pChzFile);

	m_pTargd = LLVMCreateTargetData(LLVMGetDataLayout(m_pLmoduleCur));

	m_pDib = LLVMCreateDIBuilder(m_pLmoduleCur);
	m_nRuntimeLanguage = llvm::dwarf::DW_LANG_C;
	m_pLvalCompileUnit = LLVMDIBuilderCreateCompileUnit(
							m_pDib,
							m_nRuntimeLanguage,
							pChzFile,
							pChzPath,
							"Jailang compiler",
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

	if (m_pDib)
	{
		LLVMDisposeDIBuilder(m_pDib);
		m_pDib = nullptr;
	}
}

size_t CIRBuilder::CChGenerateUniqueName(const char * pChzIn, char * pChzOut, size_t cChMax)
{
	size_t cChIn = CCh(pChzIn);
	size_t iCh = cChIn - 1;

	// not handling whitespace...
	u32 nIn = 0;
	u32 nMultiple = 1;
	while (iCh >= 0 && ((pChzIn[iCh] >= '0') & (pChzIn[iCh] <= '9')))
	{
		nIn = (pChzIn[iCh] - '0') * nMultiple + nIn;
		nMultiple *= 10;
		--iCh;
	}

	HV hv = 0;
	if (iCh >= 0)
	{
		hv = HvFromPChz(pChzIn, iCh+1);
	}

	u32 * pN = nullptr;
	FINS fins = m_hashHvNUnique.FinsEnsureKey(hv, &pN);
	if (fins == FINS_Inserted)
	{
		*pN = nIn;
		return CChCopy(pChzIn, pChzOut, cChMax);
	}
	else
	{
		*pN = ewcMax(nIn, *pN + 1);
		CChCopy(pChzIn, pChzOut, cChMax);

		size_t iChNumber = iCh+1;
		size_t cChNumber = CChFormat(&pChzOut[iChNumber], cChMax - (iChNumber), "%d", *pN); 	
		return iChNumber + cChNumber;
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
#define JAI_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMIntPredicate s_mpNcmpredLpredicate[] =
	{
		NCMPPRED_LIST
	};
#undef JAI_PRED
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
#define JAI_PRED(X) 
#define LLVM_PRED(X) X,
	static const LLVMRealPredicate s_mpGcmpredLpredicate[] =
	{
		GCMPPRED_LIST
	};
#undef JAI_PRED
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

	if (pSym->m_pVal->m_valk == VALK_Instruction)
	{
		CIRInstruction * pInstSym = (CIRInstruction *)pSym->m_pVal;
		if (!EWC_FVERIFY(pInstSym->m_irop = IROP_Alloca, "expected alloca for symbol"))
			return nullptr;
	}

	return pSym->m_pVal;
}

CIRInstruction * CIRBuilder::PInstCreateStore(CIRValue * pValPT, CIRValue * pValT)
{
	//store t into address pointed at by pT

	if (!EWC_FVERIFY(pValPT && pValPT->m_valk == VALK_Instruction, "expected alloca value for symbol"))
		return nullptr;

	CIRInstruction * pInstPT = (CIRInstruction *)pValPT;
	if (!EWC_FVERIFY(pInstPT->m_irop = IROP_Alloca, "expected alloca for symbol"))
		return nullptr;

	CIRInstruction * pInstStore = PInstCreateRaw(IROP_Store, pInstPT, pValT, "store");
	if (pInstStore->FIsError())
		return pInstStore;

	auto pLtypeT = LLVMTypeOf(pValT->m_pLval);
	auto pLtypePT = LLVMTypeOf(pInstPT->m_pLval);
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
		printf("\npLtypePT: (dest)"); LLVMDumpType(pLtypePT);
		printf("\n");
	}

    pInstStore->m_pLval = LLVMBuildStore(m_pLbuild, pValT->m_pLval, pInstPT->m_pLval);
	return pInstStore;
}

void ExtractNumericInfo(STypeInfo * pTin, u32 * pCBit, bool * pFSigned)
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
	default: EWC_ASSERT(false, "non-numeric type info");
	}
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
			u64 nUnsigned;
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
			pLval = LLVMBuildGlobalStringPtr(pBuild->m_pLbuild, pStval->m_str.PChz(), "strlit");
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

	if (pTinSrc->m_tink == TINK_Pointer)
	{
		if (pTinDst->m_tink != TINK_Bool)
		{
			if (EWC_FVERIFY(pTinDst->m_tink == TINK_Pointer, "trying to cast pointer to non-pointer. (not supported yet)"))
			{
				return pBuild->PInstCreateCast(IROP_Bitcast, pValSrc, pTinDst, "Bitcast");
			}
			return pValSrc;
		}
	}
	else
	{
		ExtractNumericInfo(pTinSrc, &cBitSrc, &fSignedSrc);
	}
	ExtractNumericInfo(pTinDst, &cBitDst, &fSignedDst);
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
					pInst = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValSrc, pConstZero, "NToBool"); break;
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
		return PValCreateCast(pWork, pBuild, pValSrc, pTinRhs, pTinOut);
	}

	CIRValue * pValRhs = PValGenerate(pWork, pBuild, pStnodRhs, VALGENK_Instance);
	return PValCreateCast(pWork, pBuild, pValRhs, pTinRhs, pTinOut);
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

		switch (pStnodOp->m_jtok)
		{
			case JTOK_AndAnd:
				{
					GeneratePredicate(pWork, pBuild, pStnodChildLhs, pBlockRhs, pBlockPost, pTinBool);
					pBuild->ActivateBlock(pBlockRhs);
					GeneratePredicate(pWork, pBuild, pStnodChildRhs, pBlockTrue, pBlockPost, pTinBool);
				} break;
			case JTOK_OrOr:
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
		(void)pBuild->PInstCreateCondBranch(pValPredCast, pBlockTrue, pBlockPost);
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

	auto pDif = PDifEnsure(pWork, pBuild, pStnodBody->m_lexloc.m_strFilename.PChz());
	PushDIScope(pDif, pProc->m_pLvalDIFunction);

	pBuild->ActivateProcedure(pProc, pProc->m_pBlockEntry);
	CIRValue * pValRet = PValGenerate(pWork, pBuild, pStnodBody, VALGENK_Instance);

	if (fNeedsNullReturn)
	{
		(void) pBuild->PInstCreate(IROP_Ret, nullptr, "RetTmp");
	}

	PopDIScope(pDif, pProc->m_pLvalDIFunction);

	pBuild->ActivateProcedure(nullptr, nullptr);
	return pValRet;
}

static inline CIRInstruction * PInstGenerateOperator(
	CIRBuilder * pBuild,
	JTOK jtok,
	STypeInfo * pTin,
	CIRValue * pValLhs,
	CIRValue * pValRhs)
{
	bool fIsSigned = true;
	TINK tink = pTin->m_tink;
	if (tink == TINK_Literal)
	{
		STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTin;
		fIsSigned = pTinlit->m_litty.m_fIsSigned;
		switch (pTinlit->m_litty.m_litk)
		{
		case LITK_Integer:	tink = TINK_Integer;	break;
		case LITK_Float:	tink = TINK_Float;		break;
		default:			tink = TINK_Nil;
		}
	}
	else if (pTin->m_tink == TINK_Integer)
	{
		fIsSigned = ((STypeInfoInteger *)pTin)->m_fIsSigned;
	}

	CIRInstruction * pInstOp = nullptr;
	switch (tink)
	{
	case TINK_Bool:
		switch (jtok)
		{
			case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
			case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
			case '&':				pInstOp = pBuild->PInstCreate(IROP_And, pValLhs, pValRhs, "nAndTmp"); break;
			case '|':				pInstOp = pBuild->PInstCreate(IROP_Or, pValLhs, pValRhs, "nOrTmp"); break;
		} break;
	case TINK_Integer:
		switch (jtok)
		{
			case '+': 				pInstOp = pBuild->PInstCreate(IROP_NAdd, pValLhs, pValRhs, "nAddTmp"); break;
			case '-': 				pInstOp = pBuild->PInstCreate(IROP_NSub, pValLhs, pValRhs, "nSubTmp"); break;
			case '*': 				pInstOp = pBuild->PInstCreate(IROP_NMul, pValLhs, pValRhs, "nMulTmp"); break;
			case '/':
			{
				IROP irop = (fIsSigned) ? IROP_SDiv : IROP_UDiv;
				pInstOp = pBuild->PInstCreate(irop, pValLhs, pValRhs, "nDivTmp");
			} break;
			case '%':
			{
				IROP irop = (fIsSigned) ? IROP_SRem : IROP_URem;
				pInstOp = pBuild->PInstCreate(irop, pValLhs, pValRhs, "nRemTmp");
			} break;
			case '&':				pInstOp = pBuild->PInstCreate(IROP_And, pValLhs, pValRhs, "nAndTmp"); break;
			case '|':				pInstOp = pBuild->PInstCreate(IROP_Or, pValLhs, pValRhs, "nOrTmp"); break;
			case '^':				pInstOp = pBuild->PInstCreate(IROP_Xor, pValLhs, pValRhs, "nXorTmp"); break;
			case JTOK_ShiftRight:	
			{
				// NOTE: AShr = arithmetic shift right (sign fill), LShr == zero fill
				IROP irop = (fIsSigned) ? IROP_AShr : IROP_LShr;
				pInstOp = pBuild->PInstCreate(irop, pValLhs, pValRhs, "nShrTmp"); break;
			} break;
			case JTOK_ShiftLeft:	pInstOp = pBuild->PInstCreate(IROP_Shl, pValLhs, pValRhs, "nShlTmp"); break;
			case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
			case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
			case JTOK_LessEqual:
				if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSLE, pValLhs, pValRhs, "NCmpSLE");
				else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpULE, pValLhs, pValRhs, "NCmpULE");
				break;
			case JTOK_GreaterEqual:
				if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSGE, pValLhs, pValRhs, "NCmpSGE");
				else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpUGE, pValLhs, pValRhs, "NCmpUGE");
				break;
			case '<':
				if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSLT, pValLhs, pValRhs, "NCmpSLT");
				else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpULT, pValLhs, pValRhs, "NCmpULT");
				break;
			case '>':
				if (fIsSigned)	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpSGT, pValLhs, pValRhs, "NCmpSGT");
				else			pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpUGT, pValLhs, pValRhs, "NCmpUGT");
				break;
		} break;
	case TINK_Float:
		switch (jtok)
		{
			case '+': 				pInstOp = pBuild->PInstCreate(IROP_GAdd, pValLhs, pValRhs, "gAddTmp"); break;
			case '-': 				pInstOp = pBuild->PInstCreate(IROP_GSub, pValLhs, pValRhs, "gSubTmp"); break;
			case '*': 				pInstOp = pBuild->PInstCreate(IROP_GMul, pValLhs, pValRhs, "gMulTmp"); break;
			case '/': 				pInstOp = pBuild->PInstCreate(IROP_GDiv, pValLhs, pValRhs, "gDivTmp"); break;
			case '%': 				pInstOp = pBuild->PInstCreate(IROP_GRem, pValLhs, pValRhs, "gRemTmp"); break;
			case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOEQ, pValLhs, pValRhs, "NCmpOEQ"); break;
			case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpONE, pValLhs, pValRhs, "NCmpONE"); break;
			case JTOK_LessEqual:	pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOLE, pValLhs, pValRhs, "NCmpOLE"); break;
			case JTOK_GreaterEqual:	pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOGE, pValLhs, pValRhs, "NCmpOGE"); break;
			case '<': 				pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOLT, pValLhs, pValRhs, "NCmpOLT"); break;
			case '>': 				pInstOp = pBuild->PInstCreateGCmp(GCMPPRED_GCmpOGT, pValLhs, pValRhs, "NCmpOGT"); break;
		} break;
	case TINK_Pointer:
		switch (jtok)
		{
			case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
			case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
			case '-': 				
				pValRhs = pBuild->PInstCreate(IROP_NNeg, pValRhs, "NNeg");
				// fallthrough
			case '+': 				
			{
				LLVMOpaqueValue * pLvalIndex = pValRhs->m_pLval;
				pInstOp = pBuild->PInstCreateGEP(pValLhs, &pLvalIndex, 1, "ptrAdd"); break;
			} break;
		}
	case TINK_Enum:
	{
		// BB - why is the RHS still a literal here?
		//EWC_ASSERT(FTypesAreSame(pTinLhs, pTinRhs), "enum comparison type mismatch");

		switch (jtok)
		{
			case JTOK_EqualEqual:	pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpEQ, pValLhs, pValRhs, "NCmpEq"); break;
			case JTOK_NotEqual:		pInstOp = pBuild->PInstCreateNCmp(NCMPPRED_NCmpNE, pValLhs, pValRhs, "NCmpNq"); break;
		}
	}
	default: 
		break;
	}

	EWC_ASSERT(pInstOp, "unexpected op in PInstGenerateOperator '%'", PChzFromJtok(jtok));
	return pInstOp;
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
			const char * pChzName = pStnod->m_pSym->m_strName.PChz();
			if (fIsGlobal)
			{
				auto pGlob = pBuild->PGlobCreate(pLtype, pChzName);
				pStnod->m_pSym->m_pVal = pGlob;

				auto pLvalDIVariable = LLVMDIBuilderCreateGlobalVariable(
										pBuild->m_pDib,
										pLvalScope, 
										pChzName,
										pChzName,
										pDif->m_pLvalFile,
										iLine,
										pStnod->m_pTin->m_pLvalDIType,
										true,
										pGlob->m_pLval);

				// need to generate code for local var, just running init, leg for now.

				LLVMOpaqueValue * pLvalInit = nullptr;
				if (pStdecl->m_iStnodInit >= 0)
				{
					CSTNode * pStnodInit = pStnod->PStnodChild(pStdecl->m_iStnodInit);

					if (pStnodInit)
					{
						auto pTinlit = PTinRtiCast<STypeInfoLiteral *>(pStnodInit->m_pTin);
						if (pTinlit && pStnodInit->m_park != PARK_Uninitializer)
						{
							pLvalInit = PLvalFromLiteral(pBuild, pTinlit, pStnodInit);
						}
					}
				}

				if (!pLvalInit)
				{
					pLvalInit = PLvalConstInitializer(pBuild, pStnod->m_pTin);
				}

				LLVMSetInitializer(pGlob->m_pLval, pLvalInit);
			}
			else
			{
				auto pInstAlloca = pBuild->PInstCreateAlloca(pLtype, cElement, pChzName);
				pStnod->m_pSym->m_pVal = pInstAlloca;
			
				auto pLvalDIVariable = LLVMDIBuilderCreateAutoVariable(
										pBuild->m_pDib,
										pLvalScope, 
										pChzName,
										pDif->m_pLvalFile,
										iLine,
										pStnod->m_pTin->m_pLvalDIType,
										true,
										0);

				(void) LLVMDIBuilderInsertDeclare(
						pBuild->m_pDib,
						pInstAlloca->m_pLval,
						pLvalDIVariable,
						pLvalScope,
						iLine, 
						iCol, 
						pBuild->m_pBlockCur->m_pLblock);

				// need to generate code for local var, just running init, leg for now.
				if (pStdecl->m_iStnodInit >= 0)
				{
					CSTNode * pStnodRhs = pStnod->PStnodChild(pStdecl->m_iStnodInit);
					if (pStnodRhs->m_park != PARK_Uninitializer)
					{
						PInstGenerateAssignment(pWork, pBuild, pStnod->m_pTin, pInstAlloca, pStnodRhs);
					}
				}
				else
				{
					return PValInitializeToDefault(pWork, pBuild, pStnod->m_pTin, pInstAlloca);
				}
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
					(void) PValGenerate(pWork, pBuild, pStnodWhile->PStnodChild(1), VALGENK_Instance);

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

				}break;
			default:
				EWC_ASSERT(false, "Unhandled reserved word in code gen");
				break;
			}
		} break;
	case PARK_ProcedureCall:
		{
			if (!EWC_FVERIFY(pStnod->m_pSym, "calling function without generated code"))
				return nullptr;

			if (!pStnod->m_pSym->m_pVal)
			{
				// this happens when calling a method that is defined later
				(void) PProcCodegenPrototype(pWork, pBuild, pStnod->m_pSym->m_pStnodDefinition);

				if (!pStnod->m_pSym->m_pVal)
					return nullptr;
			}

			CIRProcedure * pProc = (CIRProcedure *)pStnod->m_pSym->m_pVal;
			if (!EWC_FVERIFY(pProc->m_valk == VALK_ProcedureDefinition, "expected procedure value type"))
				return nullptr;

			auto pLvalFunction = pProc->m_pLvalFunction;
			size_t cStnodArgs = pStnod->CStnodChild() - 1; // don't count the identifier

			auto pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pStnod->m_pSym->m_pTin);
			if (LLVMCountParams(pLvalFunction) != cStnodArgs)
			{
				if (!EWC_FVERIFY(pTinproc->m_fHasVarArgs, "unexpected number of arguments"))
					return nullptr;
			}

			EmitLocation(pWork, pBuild, pStnod->m_lexloc);

			CDynAry<LLVMValueRef> arypLvalArgs(pBuild->m_pAlloc);
			for (size_t iStnodChild = 0; iStnodChild < cStnodArgs; ++iStnodChild)
			{
				CSTNode * pStnodArg = pStnod->PStnodChild(iStnodChild + 1);
				STypeInfo * pTinParam = pStnodArg->m_pTin;
				if (iStnodChild < pTinproc->m_arypTinParams.C())
				{
					pTinParam = pTinproc->m_arypTinParams[iStnodChild];
				}

				auto pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodArg, pTinParam);

				arypLvalArgs.Append(pValRhsCast->m_pLval);
				if (!EWC_FVERIFY(*arypLvalArgs.PLast(), "missing argument value"))
					return 0;
			}

			CIRInstruction * pInst = pBuild->PInstCreateRaw(IROP_Call, nullptr, nullptr, "RetTmp");
			if (pInst->FIsError())
				return pInst;

			pInst->m_pLval = LLVMBuildCall(
								pBuild->m_pLbuild,
								pProc->m_pLvalFunction,
								arypLvalArgs.A(),
								(u32)arypLvalArgs.C(),
								"");

			EWC_ASSERT(valgenk != VALGENK_Reference, "cannot return reference value for procedure call");
			return pInst;
		}
	case PARK_Identifier:
		{
			CIRValue * pVal = pBuild->PValFromSymbol(pStnod->m_pSym);
			if (EWC_FVERIFY(pVal, "unknown identifier in codegen") && valgenk != VALGENK_Reference)
			{
				return pBuild->PInstCreate(IROP_Load, pVal, pStnod->m_pSym->m_strName.PChz());
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
					auto pTinenum = (STypeInfoEnum *)pStnod->m_pTinOperand;
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
				ARYMEMB arymemb = ArymembLookup(strMemberName.PChz());
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
			if (!EWC_FVERIFY(pTypememb, "cannot find structure member %s", strMemberName.PChz()))
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
			if (pStnod->m_jtok == JTOK('='))
			{
				auto pInstOp = PInstGenerateAssignment(pWork, pBuild, pStnodLhs->m_pTin, pValLhs, pStnodRhs);
				return pInstOp;
			}

			JTOK jtok = JTOK_Nil;
			switch (pStnod->m_jtok)
			{
			case JTOK_PlusEqual:	jtok = JTOK('+');	break;
			case JTOK_MinusEqual:	jtok = JTOK('-');	break;
			case JTOK_MulEqual:		jtok = JTOK('*');	break;
			case JTOK_DivEqual:		jtok = JTOK('~');	break;
			case JTOK_ModEqual:		jtok = JTOK('%');	break;
			case JTOK_OrEqual:		jtok = JTOK('|');	break;
			case JTOK_AndEqual:		jtok = JTOK('&');	break;
			case JTOK_TildeEqual:	jtok = JTOK('~');	break;
			case JTOK_XorEqual:		jtok = JTOK('^');	break;
			default: EWC_ASSERT(false, "unexpected assignment operator '%s'", PChzFromJtok(pStnod->m_jtok));
			}

			EmitLocation(pWork, pBuild, pStnod->m_lexloc);

			CIRValue * pValLhsLoad = pBuild->PInstCreate(IROP_Load, pValLhs, "lhsLoad");
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pStnodLhs->m_pTin);
			EWC_ASSERT(pValRhsCast, "bad cast");

			auto pInstOp = PInstGenerateOperator(pBuild, jtok, pStnodLhs->m_pTin, pValLhsLoad, pValRhsCast);
			return pBuild->PInstCreateStore(pValLhs, pInstOp);
		}
	case PARK_AdditiveOp:
	case PARK_MultiplicativeOp:
	case PARK_ShiftOp:
	case PARK_BitwiseAndOrOp:
	case PARK_RelationalOp:
	case PARK_EqualityOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);

			STypeInfo * pTinOutput = pStnod->m_pTinOperand;
			CIRValue * pValLhsCast = PValGenerateRefCast(pWork, pBuild, pStnodLhs, pTinOutput);
			CIRValue * pValRhsCast = PValGenerateRefCast(pWork, pBuild, pStnodRhs, pTinOutput);
			if (!EWC_FVERIFY((pValLhsCast != nullptr) & (pValRhsCast != nullptr), "null operand"))
				return nullptr;

			STypeInfo * pTinLhs = pStnodLhs->m_pTin;
			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), "bad cast"))
				return nullptr;
	
			auto pInstOp = PInstGenerateOperator(pBuild, pStnod->m_jtok, pTinOutput, pValLhsCast, pValRhsCast);

			EWC_ASSERT(pInstOp, "%s operator unsupported in codegen", PChzFromJtok(pStnod->m_jtok));
			return pInstOp;
		}
	case PARK_PostfixUnaryOp:
	case PARK_UnaryOp:
		{
			CSTNode * pStnodOperand = pStnod->PStnodChild(0);

			if (pStnod->m_jtok == JTOK_Reference)
			{
				return PValGenerate(pWork, pBuild, pStnodOperand, VALGENK_Reference);
			}

			JTOK jtok = pStnod->m_jtok;
			VALGENK valgenkUnary = ((jtok == JTOK_PlusPlus) | (jtok == JTOK_MinusMinus)) ? VALGENK_Reference : VALGENK_Instance;
			CIRValue * pValOperand = PValGenerate(pWork, pBuild, pStnodOperand, valgenkUnary);
			if (!EWC_FVERIFY(pValOperand != nullptr, "null operand"))
				return nullptr;

			if (!EWC_FVERIFY(pValOperand->m_pLval != nullptr, "null llvm operand"))
				return nullptr;

			STypeInfo * pTinOutput = pStnod->m_pTinOperand;
			STypeInfo * pTinOperand = pStnodOperand->m_pTin;
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
				default:			tink = TINK_Nil;
				}
			}
			else if (pTinOutput->m_tink == TINK_Integer)
			{
				fIsSigned = ((STypeInfoInteger *)pTinOutput)->m_fIsSigned;
			}

			switch (pStnod->m_jtok)
			{
			case '!':				
				{
					EWC_ASSERT(tink == TINK_Bool, "expected value cannot be cast to bool for '!' operand");

					// OPTIMIZE: could probably save an instruction here by not comparing (for the cast to bool)
					//  then inverting with a FNot

					CIRValue * pValOperandCast = PValCreateCast(pWork, pBuild, pValOperand, pTinOperand, pTinOutput);
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
			case JTOK_Dereference:
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
			case JTOK_PlusPlus:
			case JTOK_MinusMinus:
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

							if (pStnod->m_jtok == JTOK_PlusPlus)
								pInstAdd = pBuild->PInstCreate(IROP_GAdd, pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreate(IROP_GSub, pInstLoad, pConst, "gDec");

						} break;
					case TINK_Integer:
						{
							auto pTinint = (STypeInfoInteger *)pTinOperand;
							CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
							pBuild->AddManagedVal(pConst);
							pConst->m_pLval = PLvalConstantInt(pTinint->m_cBit, fIsSigned, 1);

							if (pStnod->m_jtok == JTOK_PlusPlus)
								pInstAdd = pBuild->PInstCreate(IROP_NAdd, pInstLoad, pConst, "gInc");
							else
								pInstAdd = pBuild->PInstCreate(IROP_NSub, pInstLoad, pConst, "gDec");
						} break;
					case TINK_Pointer:
						{
							int nDelta = (pStnod->m_jtok == JTOK_PlusPlus) ? 1 : -1;
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
				PChzFromJtok(pStnod->m_jtok),
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

			(void) LLVMDIBuilderCreateTypeDef(
					pBuild->m_pDib,
					pSym->m_pTin->m_pLvalDIType,
					pSym->m_strName.PChz(),
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
	m_pProcCur = pProc;
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
	size_t cChName = CChFormat(aChName, EWC_DIM(aChName), "_%s_INIT", pTinstruct->m_strName.PChz());

	LLVMOpaqueValue * pLvalFunc = LLVMAddFunction(pBuild->m_pLmoduleCur, aChName, pLtypeFunction);
	pTinstruct->m_pLvalInitMethod = pLvalFunc;

	CAlloc * pAlloc = pBuild->m_pAlloc;
	CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);
	pProc->m_pLvalFunction = pLvalFunc;
	
	pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, aChName);

	auto pBlockPrev = pBuild->m_pBlockCur;
	auto pProcPrev = pBuild->m_pProcCur;

	pBuild->ActivateProcedure(pProc, pProc->m_pBlockEntry);

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

	/* LLVMBool fFailed = LLVMVerifyFunction(pProc->m_pLvalFunction, LLVMPrintMessageAction);
	if (fFailed)
	{
		pBuild->PrintDump();
		EWC_ASSERT(false, "Code generation failed during creation of initializer for %s", pTinstruct->m_strName.PChz());
	}*/
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
	CDynAry<LLVMTypeRef> arypLtype(pBuild->m_pAlloc);
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

	const char * pChzName;
	if (pStproc->m_fIsForeign && pStnodAlias)
	{
		CString strProcAlias = StrFromIdentifier(pStnodAlias);
		pChzName = strProcAlias.PChz();
	}
	else if (pStnodName)
	{
		CString strProcName = StrFromIdentifier(pStnodName);
		pChzName = strProcName.PChz();
	}

	char aCh[128];
	if (!pStnodName)
	{
		(void) pBuild->CChGenerateUniqueName("__AnnonFunc__", aCh, EWC_DIM(aCh));
		pChzName = aCh;
	}

	CAlloc * pAlloc = pBuild->m_pAlloc;
	CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);

	auto pLtypeFunction = LLVMFunctionType(pLtypeReturn, arypLtype.A(), (u32)arypLtype.C(), fHasVarArgs);
	pProc->m_pLvalFunction = LLVMAddFunction(pBuild->m_pLmoduleCur, pChzName, pLtypeFunction);

	if (!pStproc->m_fIsForeign)
	{
		pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, pChzName);

		auto pTinproc = PTinDerivedCast<STypeInfoProcedure *>(pStnod->m_pTin);
		if (EWC_FVERIFY(pTinproc))
		{
			if (pTinproc->m_pLvalDIType == nullptr)
			{
				CreateDebugInfo(pWork, pBuild, pStnod, pTinproc);
			}

			pProc->m_pLvalDIFunction = PLvalCreateDebugFunction(
											pWork,
											pBuild,
											pChzName,
											pTinproc->m_strMangled.PChz(),
											pStnod,
											pStnodBody,
											pTinproc->m_pLvalDIType,
											pProc->m_pLvalFunction);

			s32 iLine, iCol;
			CalculateLinePosition(pWork, &pStnodBody->m_lexloc, &iLine, &iCol);

			LLVMOpaqueValue * pLvalLoc = LLVMCreateDebugLocation(iLine, iCol, pProc->m_pLvalDIFunction);
			LLVMSetCurrentDebugLocation(pBuild->m_pLbuild, pLvalLoc);
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
				const char * pChzArgName = pStnodParam->m_pSym->m_strName.PChz();
				LLVMSetValueName(*ppLvalParam, pChzArgName);

				CIRArgument * pArg = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
				pArg->m_pLval = *ppLvalParam;
				pBuild->AddManagedVal(pArg);

				if (!pStproc->m_fIsForeign)
				{
					auto pInstAlloca = pBuild->PInstCreateAlloca(arypLtype[ipStnodParam], 1, pChzArgName);
					pStnodParam->m_pSym->m_pVal = pInstAlloca;

					(void)pBuild->PInstCreateStore(pStnodParam->m_pSym->m_pVal, pArg);

					s32 iLine;
					s32 iCol;
					CalculateLinePosition(pWork, &pStnodParam->m_lexloc, &iLine, &iCol);

					CreateDebugInfo(pWork, pBuild, pStnod, pStnodParam->m_pTin);
					
					auto pLvalDIVariable = LLVMDIBuilderCreateParameterVariable(
											pBuild->m_pDib,
											pProc->m_pLvalDIFunction, 
											pStnodParam->m_pSym->m_strName.PChz(),
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

			(void) pBuild->CChGenerateUniqueName("__AnonFunc__", aCh, EWC_DIM(aCh));

			auto pLtypeFunction = LLVMFunctionType(LLVMVoidType(), nullptr, 0, false);
			pProc->m_pLvalFunction = LLVMAddFunction(pBuild->m_pLmoduleCur, aCh, pLtypeFunction);

			pProc->m_pBlockEntry = pBuild->PBlockCreate(pProc, aCh);

			s32 iLineBody, iColBody;
			CalculateLinePosition(pWork, &pStnod->m_lexloc, &iLineBody, &iColBody);

			auto pLvalDIFunctionType = LLVMDIBuilderCreateFunctionType(pBuild->m_pDib, nullptr, 0);

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
	for (int * piEntry = paryiEntryOrder->A(); piEntry != piEntryMax; ++piEntry)
	{
		CWorkspace::SEntry *pEntry = &(*paryEntry)[*piEntry];
		auto pProc = pEntry->m_pProc;
		if (!pProc)
			continue;

		auto pStnod = pEntry->m_pStnod;
		if (pStnod->m_pStproc && pStnod->m_pStproc->m_fIsForeign)
			continue;

		fHaveAnyFailed |= LLVMVerifyFunction(pEntry->m_pProc->m_pLvalFunction, LLVMPrintMessageAction);
	}

	if (fHaveAnyFailed)
	{
		pBuild->PrintDump();
		EmitError(pWork, nullptr, "Code generation for entry point is invalid");
	}
}

void TestUniqueNames(CAlloc * pAlloc)
{
	size_t cbFreePrev = pAlloc->CB();
	{
		CIRBuilder build(pAlloc, nullptr, "");

		const char * pChzIn;
		char aCh[128];
		size_t cCh;

		pChzIn = "funcName";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame(pChzIn, aCh), "bad unique name");

		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("funcName1", aCh), "bad unique name");

		pChzIn = "funcName20";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("funcName20", aCh), "bad unique name");

		pChzIn = "234";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("234", aCh), "bad unique name");

		pChzIn = "test6000";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("test6000", aCh), "bad unique name");

		pChzIn = "test6000";
		cCh = build.CChGenerateUniqueName(pChzIn, aCh, EWC_DIM(aCh));
		EWC_ASSERT(cCh = CCh(aCh), "bad return size");
		EWC_ASSERT(FAreSame("test6001", aCh), "bad unique name");
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

void CompileToObjectFile(CWorkspace * pWork, LLVMModuleRef pLmodule, const char * pChzFilenameIn)
{
	LLVMTarget * pLtarget = nullptr;
	//const char * pChzTriple = "x86_64-pc-windows-msvc"; //LLVMGetTarget(pLmodule);
	char * pChzTriple = LLVMGetDefaultTargetTriple();

	bool fUsingWindows;
	{
		size_t cBTriple = CCh(pChzTriple)+1;
		char * pChzTripleCopy = (char*)pWork->m_pAlloc->EWC_ALLOC_TYPE_ARRAY(char, cBTriple);
		CChCopy(pChzTriple, pChzTripleCopy, cBTriple);

		char * pChzOs;
		TokenizeTripleString(pChzTripleCopy, nullptr, nullptr, &pChzOs, nullptr);
		fUsingWindows = FAreSame(pChzOs, "windows");
		
		pWork->m_pAlloc->EWC_DELETE(pChzTripleCopy);
	}

	char * pChzError = nullptr;
	LLVMBool fFailed = LLVMGetTargetFromTriple(pChzTriple, &pLtarget, &pChzError);
	if (fFailed)
	{
		EmitError(pWork, nullptr, "Error generating llvm target. (triple = %s)\n%s", pChzTriple, pChzError);
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

	auto pLtmachine = LLVMCreateTargetMachine(pLtarget, pChzTriple, pChzCPU, pChzFeatures, loptlevel, lrelocmode, lcodemodel);

	const char * pChzExtension;
	if (fUsingWindows)
      pChzExtension = ".obj";
    else
      pChzExtension = ".o";

	char aChFilenameOut[256];
	size_t cCh = CChConstructFilename(pChzFilenameIn, pChzExtension, aChFilenameOut, EWC_DIM(aChFilenameOut));
	pWork->SetObjectFilename(aChFilenameOut, cCh);

	fFailed = LLVMTargetMachineEmitToFile(pLtmachine, pLmodule, aChFilenameOut, LLVMObjectFile, &pChzError);

	LLVMDisposeTargetMachine(pLtmachine);
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
	LLVMInitializeX86AsmParser();

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
	SJaiLexer jlex;

	(void) pWork->PFileEnsure(pChzFilenameIn, CWorkspace::FILEK_Source);

	for (size_t ipFile = 0; ipFile < pWork->m_arypFile.C(); ++ipFile)
	{
		CWorkspace::SFile * pFile = pWork->m_arypFile[ipFile];
		if (pFile->m_filek != CWorkspace::FILEK_Source)
			continue;

		char aChFilenameOut[256];
		(void)CChConstructFilename(pFile->m_strFilename.PChz(), ".jai", aChFilenameOut, EWC_DIM(aChFilenameOut));

		pFile->m_pChzFileBody = pWork->PChzLoadFile(aChFilenameOut, pWork->m_pAlloc);
		if (!pFile->m_pChzFileBody)
			continue;

		printf("Parsing %s\n", pFile->m_strFilename.PChz());
		BeginParse(pWork, &jlex, pFile->m_pChzFileBody);
		jlex.m_pChzFilename = pFile->m_strFilename.PChz();

		ParseGlobalScope(pWork, &jlex, true);
		EWC_ASSERT(pWork->m_aryEntry.C() > 0);

		EndParse(pWork, &jlex);

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
			CIRBuilder build(pWork->m_pAlloc, &pWork->m_arypValManaged, pChzFilenameIn);
			
			CodeGenEntryPoint(pWork, &build, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);

			CompileToObjectFile(pWork, build.m_pLmoduleCur, pChzFilenameIn);

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

	SJaiLexer jlex;
	BeginWorkspace(pWork);
	auto pFile = pWork->PFileEnsure(s_pChzUnitTestFilename, CWorkspace::FILEK_Source);
	pFile->m_pChzFileBody = pChzIn;

	BeginParse(pWork, &jlex, pChzIn, s_pChzUnitTestFilename);

	EWC_ASSERT(pWork->m_pErrman->m_cError == 0, "parse errors detected");
	pWork->m_pErrman->Clear();

	ParseGlobalScope(pWork, &jlex, true);
	EWC_ASSERT(pWork->m_aryEntry.C() > 0);

	EndParse(pWork, &jlex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pErrman, pWork->m_pSymtab, &pWork->m_aryEntry, &pWork->m_aryiEntryChecked);
	{
		CIRBuilder build(pWork->m_pAlloc, &pWork->m_arypValManaged, "");
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

	//pChzIn = " g := 2.2; pG := &g; "; // not handling this properly as globals

	pChzIn = "{ g := 2.2; pG := &g; pN := cast(& int) pG; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "aN := {:int: 2, 4, 5}; ";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ aN : [2] int; aNUnsized : [] int = aN; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ ENUMK :: enum s32 { Ick : 1, Foo, Bah : 3 } n := ENUMK.Ick; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "{ SomeConst :: 0xFF; n:=SomeConst; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "SFoo :: struct { m_n : s32; } { foo : SFoo; pFoo := &foo; pFoo.m_n = 2; } ";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "SFoo :: struct { m_n : s32; m_g := 1.2; } foo : SFoo;";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ aN : [4] s32;  pN : & s32; fTest := aN.data == pN; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & s32; ++pN; --pN; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & s32; pN = pN + 2; pN = pN - 2; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ aN : [4] s32; n := aN[2]; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & s32; n := pN[0]; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ i:=0; while i < 5 { i = i + 1; } }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "printf :: (pCh : & u8, ..) -> s32 #foreign;";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pChz := \"testString\"; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ pN : & int; n := 2;   if (!pN) pN = &n;  if pN @pN = 2; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ n : int = 32; pN : & int; pN = &n; n2 := @pN; @pN = 6;}";
	AssertTestCodeGen(&work, pChzIn);
	//pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n) nRet = 4; else nRet =1; return nRet; }";
	//pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n) nRet = 4; return nRet; }";
	pChzIn =	"Foo :: (n : s64) -> int { nRet : s64 = 5; if (n == 4) nRet = 4; else if (n == 3) nRet =3; else nRet = 2; return nRet; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"{ i:=5 + 2 * 3; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"AddNums :: (nA : int, nB : int) -> int { return nA + nB; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"AddLocal :: (nA : int) -> int { nFoo : int; nFoo = 2; return nA + nFoo; }";
	AssertTestCodeGen(&work, pChzIn);

	pChzIn = "Foo :: (nA : s8, nB : s8) -> bool { return (nA < nB) == (nB >= nA); }";
	AssertTestCodeGen(&work, pChzIn);

	//pChzIn =	"GetTwo :: ()-> int { return 2; } AddTwo :: (nA : int) -> int { return nA + GetTwo(); }";
	//pChzIn =	"AddTwo :: (nA : int) -> int { return nA + GetTwo(); } GetTwo :: ()-> int { return 2; }";
	pChzIn =	"Foo :: () -> int { return Bah(); } Bah :: ()-> int { return Foo(); }";
	AssertTestCodeGen(&work, pChzIn);

	StaticShutdownStrings(&allocString);
}
