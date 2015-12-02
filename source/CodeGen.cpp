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

/*
#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
*/
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
	EWC::CAlloc * pAlloc = m_arypInst.m_pAlloc;
	for (size_t ipInst = 0; ipInst < m_arypInst.C(); ++ipInst)
	{
		pAlloc->EWC_DELETE(m_arypInst[ipInst]);
	}
	m_arypInst.Clear();
}

void CIRBasicBlock::Append(CIRInstruction * pInst)
{
	m_arypInst.Append(pInst);
}

static inline s8 CpValOpcode(IROP irop)
{
	if(irop >= IROP_BinaryOpMin && irop < IROP_BinaryOpMax)
		return 2;
	return 1;
}

s8 CIRInstruction::CpValOpcodeMax()
{
	return CpValOpcode(m_irop);
}



// Builder class Methods
CIRBuilder::CIRBuilder(EWC::CAlloc * pAlloc)
:m_pLbuild(nullptr)
,m_pAlloc(pAlloc)
,m_inspt()
,m_pBlockRoot(nullptr)
,m_pProc(nullptr)
{ 
	llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
	m_pLbuild = EWC_NEW(m_pAlloc, LlvmIRBuilder) LlvmIRBuilder(pLctx);
}
	
CIRBuilder::~CIRBuilder()
{
	if (m_pLbuild)
	{
		m_pAlloc->EWC_DELETE(m_pLbuild);
		m_pLbuild = nullptr;
	}
}

CIRInstruction * CIRBuilder::PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	int cpValOpcode = CpValOpcode(irop);
	size_t cB = sizeof(CIRInstruction) + (cpValOpcode - 1) * sizeof(CIRValue *);

	CIRInstruction * pInst = (CIRInstruction *)m_pAlloc->EWC_ALLOC(cB, EWC_ALIGN_OF(CIRInstruction));
	new (pInst) CIRInstruction(irop);
	pInst->m_apValOperand[0] = pValLhs;
	pInst->m_apValOperand[1] = pValRhs;

	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_NAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFAdd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GAdd, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFAdd(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_NSub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateSub(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GSub, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFSub(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_NMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateMul(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GMul, pValLhs, pValRhs, pChzName);
	pInst->m_pLval = m_pLbuild->CreateFMul(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateNDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	STypeInfoInteger * pTinint = (STypeInfoInteger *)pValLhs->m_pStnod->m_pTin;
	bool fSigned = false;
	if (!EWC_FVERIFY(pTinint->m_tink == TINK_Integer, "bad type in PInstCreateNDiv"))
	{
		fSigned = pTinint->m_fSigned;
	}

	CIRInstruction * pInst = PInstCreate(IROP_NDiv, pValLhs, pValRhs, "nDivTmp");
	if (fSigned)
	{
		pInst->m_pLval = m_pLbuild->CreateSDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}
	else
	{
		pInst->m_pLval = m_pLbuild->CreateUDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	}

	return pInst;
}

CIRInstruction * CIRBuilder::PInstCreateGDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName)
{
	CIRInstruction * pInst = PInstCreate(IROP_GDiv, pValLhs, pValRhs, "gDivTmp");
	pInst->m_pLval = m_pLbuild->CreateFDiv(pValLhs->m_pLval, pValRhs->m_pLval, pChzName);
	return pInst;
}

CIRValue * PValImplicitCast(CIRBuilder * pBuild, CIRValue * pValSrc, STypeInfo * pTinSrc, STypeInfo * pTinDst)
{
	EWC_ASSERT(pTinSrc == pTinDst, "implicit casting is not done yet");	
	return pValSrc;
}

CIRValue * PValGenerate(CIRBuilder * pBuild, CSTNode * pStnod)
{
	switch (pStnod->m_park)
	{
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
			if ((pValLhs == nullptr) | (pValRhs == nullptr))
				return nullptr;

			STypeInfo * pTinOutput = pStnod->m_pTin;
			STypeInfo * pTinLhs = pStnodLhs->m_pTin;
			STypeInfo * pTinRhs = pStnodRhs->m_pTin;
			if (!EWC_FVERIFY((pTinOutput != nullptr) & (pTinLhs != nullptr) & (pTinRhs != nullptr), "bad cast"))
				return nullptr;
	
			CIRValue * pValLhsCast = PValImplicitCast(pBuild, pValLhs, pTinLhs, pTinOutput);
			CIRValue * pValRhsCast = PValImplicitCast(pBuild, pValRhs, pTinRhs, pTinOutput);
			CIRInstruction * pInstOp = nullptr;
			switch (pStnod->m_jtok)
			{
				case '+':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNAdd(pValLhs, pValRhs, "nAddTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGAdd(pValLhs, pValRhs, "gAddTmp"); break;
					}
				}break;
				case '-':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNSub(pValLhs, pValRhs, "nSubTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGSub(pValLhs, pValRhs, "gSubTmp"); break;
					}
				}break;
				case '*':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNMul(pValLhs, pValRhs, "nMulTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGMul(pValLhs, pValRhs, "gMulTmp"); break;
					}
				}break;
				case '/':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreateNDiv(pValLhs, pValRhs, "nDivTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreateGDiv(pValLhs, pValRhs, "gDivTmp"); break;
					}
				}break;
				default: break;
			}

			EWC_ASSERT(pInstOp, "%s operator unsupported in codegen", PChzFromJtok(pStnod->m_jtok));
			pBuild->PBlockEnsure("block")->Append(pInstOp);
			return pInstOp;

		}break;
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

void PerformCodeGen(CAlloc * pAlloc, CSymbolTable * pSymtabTop, CAry<CWorkspace::SEntry> * paryEntry)
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
			CIRProcedure * pProc = EWC_NEW(pAlloc, CIRProcedure) CIRProcedure();
			pEntry->m_pProc = pProc;
			build.m_pProc = pProc;
			pProc->m_pBlockEntry = build.PBlockEnsure("Proc");
		}
		else
		{
			EWC_ASSERT(false, "need to set up named procedure");
		}

		(void) PValGenerate(&build, pStnod);
	}
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
	PerformCodeGen(pWork->m_pAlloc, pWork->m_pSymtab, &pWork->m_aryEntry);

	char aCh[1024];
	char * pCh = aCh;
	char * pChMax = &aCh[EWC_DIM(aCh)];

	(void) CChWriteDebugStringForEntries(pWork, pCh, pChMax, FDBGSTR_Type);


	//EWC_ASSERT(EWC::FAreSame(aCh, pChzOut), "type check debug string doesn't match expected value");

	EndWorkspace(pWork);
}

void TestCodeGen()
{
	u8 aBString[1024 * 100];
	CAlloc allocString(aBString, sizeof(aBString));

	StaticInitStrings(&allocString);

	u8 aB[1024 * 100];
	CAlloc alloc(aB, sizeof(aB));

	SErrorManager errman;
	CWorkspace work(&alloc, &errman);

	const char * pChzIn =	"{ i:=5 + 2 * 3; }";
	//const char * pChzOut = "({} (int @i int) (int @foo int) (float @g float)) (float @g_g float)";
	AssertTestCodeGen(&work, pChzIn);

	StaticShutdownStrings(&allocString);
}
