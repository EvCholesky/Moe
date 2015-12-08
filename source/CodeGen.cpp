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
#pragma warning(disable : 4100)
#pragma warning(disable : 4127)
#pragma warning(disable : 4146)
#pragma warning(disable : 4180)
#pragma warning(disable : 4204)
#pragma warning(disable : 4244)
#pragma warning(disable : 4245)
#pragma warning(disable : 4258)
#pragma warning(disable : 4267)
#pragma warning(disable : 4291)
#pragma warning(disable : 4310)
#pragma warning(disable : 4324)
#pragma warning(disable : 4345)
#pragma warning(disable : 4351)
#pragma warning(disable : 4355)
#pragma warning(disable : 4389)
#pragma warning(disable : 4456)
#pragma warning(disable : 4457)
#pragma warning(disable : 4458)
#pragma warning(disable : 4459)
#pragma warning(disable : 4503)
#pragma warning(disable : 4505)
#pragma warning(disable : 4510)
#pragma warning(disable : 4512)
#pragma warning(disable : 4610)
#pragma warning(disable : 4611)
#pragma warning(disable : 4624)
#pragma warning(disable : 4701)
#pragma warning(disable : 4702)
#pragma warning(disable : 4703)
#pragma warning(disable : 4706)
#pragma warning(disable : 4722)
#pragma warning(disable : 4800)
#pragma warning(disable : 4805)
#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_WARNINGS
#define _SCL_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_WARNINGS
#define __STDC_CONSTANT_MACROS
#define __STDC_FORMAT_MACROS
#define __STDC_LIMIT_MACROS
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#undef _CRT_SECURE_NO_DEPRECATE
#undef _CRT_SECURE_NO_WARNINGS
#undef _CRT_NONSTDC_NO_DEPRECATE
#undef _CRT_NONSTDC_NO_WARNINGS
#undef _SCL_SECURE_NO_DEPRECATE
#undef _SCL_SECURE_NO_WARNINGS
#undef __STDC_CONSTANT_MACROS
#undef __STDC_FORMAT_MACROS
#undef __STDC_LIMIT_MACROS
#pragma warning ( pop )

// wrapper class to avoid the forward decl template mess.
class LlvmIRBuilder : public llvm::IRBuilder<>
{
public:
					LlvmIRBuilder(llvm::LLVMContext * pLctx)
					:llvm::IRBuilder<>(*pLctx)
						{ ; }
};



CIRValue::CIRValue(VALK valk)
:m_pLval(nullptr)
,m_pStnod(nullptr)
,m_valk(valk)
{
}



CIRBasicBlock::CIRBasicBlock(EWC::CAlloc * pAlloc)
:m_pLblock(nullptr)
,m_arypInst(pAlloc)
{
}

CIRBasicBlock::~CIRBasicBlock()
{
	m_arypInst.Clear();
}

void CIRBasicBlock::Append(CIRInstruction * pInst)
{
	m_arypInst.Append(pInst);
}

static inline s8 COperand(IROP irop)
{
	if (irop == IROP_Call)
		return 0;
	if (irop >= IROP_BinaryOpMin && irop < IROP_BinaryOpMax)
		return 2;
	return 1;
}

s8 CIRInstruction::CpValOperandMax()
{
	return COperand(m_irop);
}



CIRProcedure::~CIRProcedure()
{
	if (m_pBlockEntry)
	{
		m_pAlloc->EWC_DELETE(m_pBlockEntry);
		m_pBlockEntry = nullptr;
	}

	size_t cpVal = m_arypValManaged.C();
	for (size_t ipVal = 0; ipVal < cpVal; ++ipVal)
	{
		m_pAlloc->EWC_DELETE(m_arypValManaged[ipVal]);
	}
	m_arypValManaged.Clear();
}



// Builder class Methods
CIRBuilder::CIRBuilder(EWC::CAlloc * pAlloc)
:m_pLbuild(nullptr)
,m_pAlloc(pAlloc)
,m_inspt()
,m_pBlockRoot(nullptr)
,m_pProc(nullptr)
,m_hashHvNUnique(pAlloc)
{ 
	llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
	m_pLbuild = EWC_NEW(m_pAlloc, LlvmIRBuilder) LlvmIRBuilder(pLctx);
	m_pLmoduleCur = EWC_NEW(m_pAlloc, llvm::Module) llvm::Module("Jai Jit", *pLctx);
}
	
CIRBuilder::~CIRBuilder()
{
	if (m_pLbuild)
	{
		m_pAlloc->EWC_DELETE(m_pLbuild);
		m_pLbuild = nullptr;
	}

	if (m_pLmoduleCur)
	{
		m_pAlloc->EWC_DELETE(m_pLmoduleCur);
		m_pLmoduleCur = nullptr;
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
		hv = HvFromPchz(pChzIn, iCh+1);
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

	m_pLmoduleCur->dump();
}

CIRInstruction * CIRBuilder::PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	int cpValOperand = COperand(irop);
	size_t cB = sizeof(CIRInstruction) + ewcMax((cpValOperand - 1), 0) * sizeof(CIRValue *);

	CIRInstruction * pInst = (CIRInstruction *)m_pAlloc->EWC_ALLOC(cB, EWC_ALIGN_OF(CIRInstruction));
	new (pInst) CIRInstruction(irop);
	AddManagedVal(pInst);

	pInst->m_apValOperand[0] = pValLhs;
	pInst->m_apValOperand[1] = pValRhs;
	pInst->m_cpValOperand = cpValOperand;

	return pInst;
}

void CIRBuilder::AddManagedVal(CIRValue * pVal)
{
	if (EWC_FVERIFY(m_pProc, "adding managed value with no active procedure"))
	{
		m_pProc->m_arypValManaged.Append(pVal);
	}
}

CIRInstruction * CIRBuilder::PInstCreateNAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SAdd : IROP_UAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateAdd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFAdd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SSub : IROP_USub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateSub(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GSub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFSub(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst = PInstCreate((fSigned) ? IROP_SMul : IROP_UMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateMul(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFMul(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName, bool fSigned)
{
	CIRInstruction * pInst;
	if (fSigned)
	{
		pInst = PInstCreate(IROP_UDiv, pValLhs, pValRhs, pChzName);
		pInst->m_pLval = m_pLbuild->CreateUDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	else
	{
		pInst = PInstCreate(IROP_SDiv, pValLhs, pValRhs, pChzName);
		pInst->m_pLval = m_pLbuild->CreateUDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GDiv, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateRet(CIRValue * pValRhs)
{
	CIRInstruction * pInst = PInstCreate(IROP_Ret, pValRhs, nullptr, "RetTmp");
	pInst->m_pLval = m_pLbuild->CreateRet(pValRhs->m_pLval);
	return pInst;
}

CIRValue * PValImplicitCast(CIRBuilder * pBuild, CIRValue * pValSrc, STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
	// BB - hard to check because type info's aren't unique... also TypeInfo is not sufficient for the literal case.
	//EWC_ASSERT(pTinSrc == pTinDst, "implicit casting is not done yet");	

	return pValSrc;
}

CIRValue * PValGenerate(CIRBuilder * pBuild, CSTNode * pStnod)
{
	switch (pStnod->m_park)
	{
		case PARK_List:
		{
			int cStnodChild = pStnod->CStnodChild();
			for (int iStnodChild = 0; iStnodChild < cStnodChild; ++iStnodChild)
			{
				CIRValue * pVal = PValGenerate(pBuild, pStnod->PStnodChild(iStnodChild));
				// not adding 
			}

		}break;
		case PARK_Decl:
		{
			CSTDecl * pStdecl = pStnod->m_pStdecl;
			if (!pStdecl)
				return nullptr;
			
			// need to generate code for local var, just running init, leg for now.
			if (pStdecl->m_iStnodInit)
			{
				CIRValue * pVal = PValGenerate(pBuild, pStnod->PStnodChild(pStdecl->m_iStnodInit));
			}
		}break;
		case PARK_Literal:
		{
			CSTValue * pStval = pStnod->m_pStval;
			if (!pStval)
				return nullptr;
		
			llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
			llvm::Constant * pLconst = nullptr;
			LlvmIRBuilder * pLbuild = pBuild->m_pLbuild;

			switch (pStval->m_litty.m_litk)
			{
			case LITK_Integer:
				{
					switch (pStval->m_litty.m_litsize)
					{
						case LITSIZE_8:		pLconst = pLbuild->getInt8(U8Coerce(pStval->m_nUnsigned));		break;
						case LITSIZE_16:	pLconst = pLbuild->getInt16(U16Coerce(pStval->m_nUnsigned));	break;
						case LITSIZE_32:	pLconst = pLbuild->getInt32(U32Coerce(pStval->m_nUnsigned));	break;
						case LITSIZE_Nil: // fall through
						case LITSIZE_64:	pLconst = pLbuild->getInt64(pStval->m_nUnsigned);				break;
						default: EWC_ASSERT(false, "unhandled LITSIZE");
					}
				}break;
			case LITK_Float:
				{
					pLconst = llvm::ConstantFP::get(*pLctx, llvm::APFloat(pStval->m_g));
				}break;
			case LITK_Bool:
				{
					pLconst = (pStval->m_nUnsigned) ? 
								llvm::ConstantInt::getTrue(*pLctx) :
								llvm::ConstantInt::getFalse(*pLctx);
				}break;
			case LITK_Char:		EWC_ASSERT(false, "TBD"); return nullptr;
			case LITK_String:	EWC_ASSERT(false, "TBD"); return nullptr;
			case LITK_Null:		EWC_ASSERT(false, "TBD"); return nullptr;
			}
	
			if (!pLconst)
			{
				EWC_ASSERT(false, "unknown LITK in PValGenerate");
				return nullptr;
			}

			CIRConstant * pConst = EWC_NEW(pBuild->m_pAlloc, CIRConstant) CIRConstant();
			pBuild->AddManagedVal(pConst);
			pConst->m_pLval = pLconst;
			return pConst;
		}
		case PARK_ReservedWord:
		{
			if (!EWC_FVERIFY(pStnod->m_pStval, "reserved word without value"))
				return nullptr;
			
			RWORD rword = pStnod->m_pStval->m_rword;
			switch (rword)
			{
				case RWORD_Return:
				{
					CIRValue * pValRhs = nullptr;
					if (pStnod->CStnodChild() == 1)
					{
						pValRhs = PValGenerate(pBuild, pStnod->PStnodChild(0));
					}

					if (pValRhs)
					{
						CIRInstruction * pInstRet = pBuild->PInstCreateRet(pValRhs);
						pBuild->PBlockEnsure("block")->Append(pInstRet);
					}
				}break;
				default:
				{
					EWC_ASSERT(false, "Unhandled reserved word in code gen");
				}
			}
		} break;
		case PARK_ProcedureCall:
		{
			if (!EWC_FVERIFY(pStnod->m_pSym && pStnod->m_pSym->m_pVal, "calling function without generated code"))
				return nullptr;

			CIRProcedure * pProc = (CIRProcedure *)pStnod->m_pSym->m_pVal;
			if (!EWC_FVERIFY(pProc->m_valk == VALK_ProcedureDefinition, "expected procedure value type"))
				return nullptr;

			llvm::Function * pLfunc = pProc->m_pLfunc;
			int cStnodArgs = pStnod->CStnodChild();

			if (!EWC_FVERIFY(pLfunc->arg_size() != cStnodArgs, "unexpected number of arguments"))
				return nullptr;

			std::vector<llvm::Value *> aryPLvalArgs;
			for (int iStnodChild = 1; iStnodChild < cStnodArgs; ++iStnodChild)
			{
				CIRValue * pVal = PValGenerate(pBuild, pStnod->PStnodChild(iStnodChild));
				aryPLvalArgs.push_back(pVal->m_pLval);
				if (aryPLvalArgs.back() == 0)
					return 0;
			}

			CIRInstruction * pInst = pBuild->PInstCreate(IROP_Call, nullptr, nullptr, "RetTmp");
			pInst->m_pLval = pBuild->m_pLbuild->CreateCall(pProc->m_pLfunc, aryPLvalArgs);

			return pInst;

		} break;
		case PARK_Identifier:
		{
			if (EWC_FVERIFY(pStnod->m_pSym, "unknown identifier in codeGen"))
			{
				return pStnod->m_pSym->m_pVal;
			}
		} break;
		case PARK_AdditiveOp:
		case PARK_MultiplicativeOp:
		case PARK_ShiftOp:
		case PARK_BitwiseAndOrOp:
		case PARK_RelationalOp:
		case PARK_EqualityOp:
		case PARK_LogicalAndOrOp:
		{
			CSTNode * pStnodLhs = pStnod->PStnodChild(0);
			CSTNode * pStnodRhs = pStnod->PStnodChild(1);
			CIRValue * pValLhs = PValGenerate(pBuild, pStnodLhs);
			CIRValue * pValRhs = PValGenerate(pBuild, pStnodRhs);
			if (!EWC_FVERIFY((pValLhs != nullptr) & (pValRhs != nullptr), "null operand"))
				return nullptr;

			if (!EWC_FVERIFY((pValLhs->m_pLval != nullptr) & (pValRhs->m_pLval != nullptr), "null llvm operand"))
				return nullptr;

			STypeInfo * pTinOutput = pStnod->m_pTin;
			STypeInfo * pTinLhs = pStnodLhs->m_pTin;

			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), "bad cast"))
				return nullptr;
	
			CIRValue * pValLhsCast = PValImplicitCast(pBuild, pValLhs, pTinLhs, pTinOutput);
			CIRValue * pValRhsCast = PValImplicitCast(pBuild, pValRhs, pTinRhs, pTinOutput);
			CIRInstruction * pInstOp = nullptr;

			bool fSigned = true;
			TINK tink = pTinOutput->m_tink;
			if (pTinOutput->m_tink == TINK_Literal)
			{
				if (EWC_FVERIFY(pStnod->m_pStval, "literal with no value"))
				{
					fSigned = pStnod->m_pStval->m_litty.m_litsign == LITSIGN_Signed;
					switch (pStnod->m_pStval->m_litty.m_litk)
					{
					case LITK_Integer:	tink = TINK_Integer;	break;
					case LITK_Float:	tink = TINK_Float;		break;
					default:			tink = TINK_Nil;
					}
				}
			}
			else if (pTinOutput->m_tink == TINK_Literal)
			{
				fSigned = ((STypeInfoInteger *)pValLhs->m_pStnod->m_pTin)->m_fSigned;
			}

			switch (pStnod->m_jtok)
			{
				case '+':
				{
					switch (tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNAdd(pValLhs, pValRhs, "nAddTmp", fSigned); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGAdd(pValLhs, pValRhs, "gAddTmp"); break;
					}
				}break;
				case '-':
				{
					switch (tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNSub(pValLhs, pValRhs, "nSubTmp", fSigned); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGSub(pValLhs, pValRhs, "gSubTmp"); break;
					}
				}break;
				case '*':
				{
					switch (tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNMul(pValLhs, pValRhs, "nMulTmp", fSigned); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGMul(pValLhs, pValRhs, "gMulTmp"); break;
					}
				}break;
				case '/':
				{
					switch (tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNDiv(pValLhs, pValRhs, "nDivTmp", fSigned); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGDiv(pValLhs, pValRhs, "gDivTmp"); break;
					}
				}break;
				default: break;
			}

			if (EWC_FVERIFY(pInstOp, "%s operator unsupported in codegen", PChzFromJtok(pStnod->m_jtok)))
			{
				pBuild->PBlockEnsure("block")->Append(pInstOp);
			}

			return pInstOp;

		}break;
		default:
			EWC_ASSERT(false, "unhandled PARK (%s) in code generation.", PChzFromPark(pStnod->m_park));
	}
	return nullptr;
}

CIRBasicBlock * CIRBuilder::PBlockEnsure(const char * pChzName)
{
	llvm::Function * pLfunc = (m_pProc) ? m_pProc->m_pLfunc : nullptr;
	if (!m_pBlockRoot)
	{
		llvm::BasicBlock * pLblock = llvm::BasicBlock::Create(llvm::getGlobalContext(), pChzName, pLfunc);
		m_pLbuild->SetInsertPoint(pLblock);

		m_pBlockRoot = EWC_NEW(m_pAlloc, CIRBasicBlock) CIRBasicBlock(m_pAlloc);
	}

	return m_pBlockRoot;
}

static inline llvm::Type * PLtypeFromPTin(STypeInfo * pTin)
{
	if (!pTin)
		return false;

	llvm::LLVMContext & lctx = llvm::getGlobalContext();

	switch (pTin->m_tink)
	{
	    case TINK_Integer:	
		{
			STypeInfoInteger * pTinint = (STypeInfoInteger *)pTin;
			switch (pTinint->m_cBit)
			{
			case 8:		return llvm::Type::getInt8Ty(lctx);
			case 16:	return llvm::Type::getInt16Ty(lctx);
			case 32:	return llvm::Type::getInt32Ty(lctx);
			case 64:	return llvm::Type::getInt64Ty(lctx);
			default:	return nullptr;
			}
		}
		case TINK_Bool:		return llvm::Type::getInt1Ty(lctx);
	    case TINK_Float:
		{
			STypeInfoFloat * pTinfloat = (STypeInfoFloat *)pTin;
			switch (pTinfloat->m_cBit)
			{
			case 32:	return llvm::Type::getFloatTy(lctx);
			case 64:	return llvm::Type::getDoubleTy(lctx);
			default:	return nullptr;
			}
		}
		case TINK_Literal:
		{
			STypeInfoLiteral * pTinlit = (STypeInfoLiteral *)pTin;
			switch (pTinlit->m_stval.m_litty.m_litk)
			{
			case LITK_Integer:	return llvm::Type::getInt64Ty(lctx);
			case LITK_Float:	return llvm::Type::getDoubleTy(lctx);
			case LITK_Bool:		return llvm::Type::getInt1Ty(lctx);
			case LITK_Char:		return nullptr;
			case LITK_String:	return nullptr;
			case LITK_Null:		return nullptr;
			default:			return nullptr;
			}
		}
		default: return nullptr;
	}
}

void CodeGenEntryPoint(CAlloc * pAlloc, CSymbolTable * pSymtabTop, CAry<CWorkspace::SEntry> * paryEntry)
{
	CIRBuilder build(pAlloc);

	CWorkspace::SEntry * pEntryMax = paryEntry->PMac();
	int ipTcfram = 0;
	for (CWorkspace::SEntry * pEntry = paryEntry->A(); pEntry != pEntryMax; ++pEntry, ++ipTcfram)
	{
		CSTNode * pStnod = pEntry->m_pStnod;
		if (pStnod->m_park != PARK_ProcedureDefinition)
		{
			EWC_ASSERT(build.m_pProc == nullptr, "expected null procedure for entry point.");
			CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);
			pEntry->m_pProc = pProc;
			build.m_pProc = pProc;

			char aCh[128];
			(void) build.CChGenerateUniqueName("__AnonFunc__", aCh, EWC_DIM(aCh));

			std::vector<llvm::Type *> argsEmpty;
			llvm::FunctionType * pLfunctype = llvm::FunctionType::get(build.m_pLbuild->getVoidTy(), argsEmpty, false);

			pProc->m_pLfunc = llvm::Function::Create(pLfunctype, llvm::Function::ExternalLinkage, aCh, build.m_pLmoduleCur);
			pProc->m_pBlockEntry = build.PBlockEnsure(aCh);

			(void) PValGenerate(&build, pStnod);
		}
		else
		{
			// BB - this should move into PValGenerate, under a PARK_ProcedureDefinition case.
			CSTProcedure * pStproc = pStnod->m_pStproc;
			CSTNode * pStnodParamList = nullptr;
			CSTNode * pStnodBody = nullptr;
			CSTNode * pStnodReturn = nullptr;
			CSTNode * pStnodName = nullptr;
			if (EWC_FVERIFY(pStproc, "Encountered procedure without CSTProcedure"))
			{
				pStnodParamList = pStnod->PStnodChildSafe(pStproc->m_iStnodParameterList);
				pStnodBody = pStnod->PStnodChildSafe(pStproc->m_iStnodBody);
				pStnodReturn = pStnod->PStnodChildSafe(pStproc->m_iStnodReturnType);
				pStnodName = pStnod->PStnodChildSafe(pStproc->m_iStnodProcName);
			}

			std::vector<llvm::Type*> aryPLtype;
			if (pStnodParamList && EWC_FVERIFY(pStnodParamList->m_park == PARK_ParameterList, "expected parameter list"))
			{
				int cpStnodParams = pStnodParamList->CStnodChild();
				for (int ipStnod = 0; ipStnod < cpStnodParams; ++ipStnod)
				{
					CSTNode * pStnodDecl = pStnodParamList->PStnodChild(ipStnod);
					if (!EWC_FVERIFY(pStnodDecl->m_park == PARK_Decl, "bad parameter"))
						continue;

					llvm::Type * pLtype = PLtypeFromPTin(pStnodDecl->m_pTin);
					if (EWC_FVERIFY(pLtype, "Could not compute LLVM type for parameter"))
					{
						aryPLtype.push_back(pLtype);
					}
				}
			}

			llvm::Type * pLtypeReturn;
			if (pStnodReturn)
			{
				pLtypeReturn = PLtypeFromPTin(pStnodReturn->m_pTin);
				EWC_FVERIFY(pLtypeReturn, "Could not compute LLVM type for return type");
			}
			if (!pLtypeReturn)
			{
				pLtypeReturn = build.m_pLbuild->getVoidTy();
			}

			const char * pChzName;
			if (pStnodName)
			{
				CString strProcName = StrFromIdentifier(pStnodName);
				pChzName = strProcName.PChz();
			}

			char aCh[128];
			if (!pStnodName)
			{
				(void) build.CChGenerateUniqueName("__AnnonFunc__", aCh, EWC_DIM(aCh));
				pChzName = aCh;
			}

			EWC_ASSERT(build.m_pProc == nullptr, "expected null procedure for entry point.");
			CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure(pAlloc);
			pEntry->m_pProc = pProc;
			build.m_pProc = pProc;

			llvm::FunctionType * pLfunctype = llvm::FunctionType::get(pLtypeReturn, aryPLtype, false);
			pProc->m_pLfunc = llvm::Function::Create(
												pLfunctype,
												llvm::Function::ExternalLinkage,
												pChzName,
												build.m_pLmoduleCur);

			pProc->m_pBlockEntry = build.PBlockEnsure(pChzName);

			if (EWC_FVERIFY(pStnod->m_pSym, "expected symbol to be set during type check"))
			{
				pStnod->m_pSym->m_pVal = pProc;
			}

			if (pStnodParamList)
			{
				u32 iArg = 0;
				int cpStnodParam = pStnodParamList->CStnodChild();
				llvm::Function::arg_iterator argIt = pProc->m_pLfunc->arg_begin();
				for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam, ++argIt)
				{
					CSTNode * pStnodParam = pStnodParamList->PStnodChild(ipStnodParam);
					if (EWC_FVERIFY(pStnodParam->m_pSym, "missing symbol for argument"))
					{
						argIt->setName(pStnodParam->m_pSym->m_strName.PChz());

						CIRArgument * pArg = EWC_NEW(pAlloc, CIRArgument) CIRArgument();
						pArg->m_pLval = argIt;

						build.AddManagedVal(pArg);
						pStnodParam->m_pSym->m_pVal = pArg;
					}
				}
			}

			(void) PValGenerate(&build, pStnodBody);
		}

	    llvm::verifyFunction(*build.m_pProc->m_pLfunc);
		build.m_pProc = nullptr;
		build.m_pBlockRoot = nullptr;
	}

	build.PrintDump();
}

void TestUniqueNames(CAlloc * pAlloc)
{
	size_t cbFreePrev = pAlloc->CB();
	{
		CIRBuilder build(pAlloc);

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

void AssertTestCodeGen(
	CWorkspace * pWork,
	const char * pChzIn)
	//const char * pChzOut)
{
	SJaiLexer jlex;
	BeginWorkspace(pWork);
	BeginParse(pWork, &jlex, pChzIn);

	EWC_ASSERT(pWork->m_pParctx->m_cError == 0, "parse errors detected");
	pWork->m_pParctx->m_cError = 0;

	ParseGlobalScope(pWork, &jlex, true);
	EWC_ASSERT(pWork->m_aryEntry.C() > 0);

	EndParse(pWork, &jlex);

	PerformTypeCheck(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_aryEntry);
	CodeGenEntryPoint(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_aryEntry);

	/*
	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type);

	EWC_ASSERT(EWC::FAreSame(aCh, pChzOut), "type check debug string doesn't match expected value");
	*/

	EndWorkspace(pWork);
}

void TestCodeGen()
{
	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	TestUniqueNames(&alloc);

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);
	const char * pChzIn;

//	pChzIn =	"{ i:=5 + 2 * 3; }";
//	pChzIn =	"AddNums :: (nA : int, nB : int) -> int { return nA + nB; }";
//	AssertTestCodeGen(&work, pChzIn);

	pChzIn =	"GetTwo :: ()-> int { return 2; } AddTwo :: (nA : int) -> int { return nA + GetTwo(); }";
	AssertTestCodeGen(&work, pChzIn);

	StaticShutdownStrings(&allocString);
}
