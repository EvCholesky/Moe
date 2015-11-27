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


/*
..\..\..\Debug\lib\LLVMAnalysis.lib
..\..\..\Debug\lib\LLVMAsmPrinter.lib
..\..\..\Debug\lib\LLVMBitReader.lib
..\..\..\Debug\lib\LLVMCodeGen.lib
..\..\..\Debug\lib\LLVMCore.lib
..\..\..\Debug\lib\LLVMMCDisassembler.lib
..\..\..\Debug\lib\LLVMExecutionEngine.lib
..\..\..\Debug\lib\LLVMInstCombine.lib
..\..\..\Debug\lib\LLVMInstrumentation.lib
..\..\..\Debug\lib\LLVMipa.lib
..\..\..\Debug\lib\LLVMMC.lib
..\..\..\Debug\lib\LLVMMCJIT.lib
..\..\..\Debug\lib\LLVMObject.lib
..\..\..\Debug\lib\LLVMMCParser.lib
..\..\..\Debug\lib\LLVMProfileData.lib
..\..\..\Debug\lib\LLVMRuntimeDyld.lib
..\..\..\Debug\lib\LLVMScalarOpts.lib
..\..\..\Debug\lib\LLVMSelectionDAG.lib
..\..\..\Debug\lib\LLVMSupport.lib
..\..\..\Debug\lib\LLVMTarget.lib
..\..\..\Debug\lib\LLVMTransformUtils.lib
..\..\..\Debug\lib\LLVMX86AsmPrinter.lib
..\..\..\Debug\lib\LLVMX86AsmParser.lib
..\..\..\Debug\lib\LLVMX86CodeGen.lib
..\..\..\Debug\lib\LLVMX86Desc.lib
..\..\..\Debug\lib\LLVMX86Disassembler.lib
..\..\..\Debug\lib\LLVMX86Info.lib
..\..\..\Debug\lib\LLVMX86Utils.lib
*/
//LLVMAnalysis.lib;LLVMAsmPrinter.lib;LLVMBitReader.lib;LLVMCodeGen.lib;LLVMCore.lib;LLVMMCDisassembler.lib;LLVMExecutionEngine.lib;LLVMInstCombine.lib;LLVMInstrumentation.lib;LLVMipa.lib;LLVMMC.lib;LLVMMCJIT.lib;LLVMObject.lib;LLVMMCParser.lib;LLVMProfileData.lib;LLVMRuntimeDyld.lib;LLVMScalarOpts.lib;LLVMSelectionDAG.lib;LLVMSupport.lib;LLVMTarget.lib;LLVMTransformUtils.lib;LLVMX86AsmPrinter.lib;LLVMX86AsmParser.lib;LLVMX86CodeGen.lib;LLVMX86Desc.lib;LLVMX86Disassembler.lib;LLVMX86Info.lib;LLVMX86Utils.lib 


// Builder class Methods
CIRBuilder::CIRBuilder(EWC::CAlloc * pAlloc)
:m_pLbuilder(nullptr)
,m_pAlloc(pAlloc)
,m_inspt()
,m_pBlockRoot(nullptr)
{ 
	llvm::LLVMContext * pLctx = &llvm::getGlobalContext();
	m_pLbuilder = EWC_NEW(m_pAlloc, LlvmIRBuilder) LlvmIRBuilder(pLctx);
}
	
CIRBuilder::~CIRBuilder()
{
	if (m_pLbuilder)
	{

		m_pAlloc->EWC_DELETE(m_pLbuilder);
		m_pLbuilder = nullptr;
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
						case TINK_Integer:	pInstOp = pBuild->PInstCreate(IROP_NAdd, pValLhs, pValRhs, "nAddTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreate(IROP_GAdd, pValLhs, pValRhs, "gAddTmp"); break;
					}
				}break;
				case '-':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreate(IROP_NSub, pValLhs, pValRhs, "nSubTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreate(IROP_GSub, pValLhs, pValRhs, "gSubTmp"); break;
					}
				}break;
				case '*':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreate(IROP_NMul, pValLhs, pValRhs, "nMulTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreate(IROP_GMul, pValLhs, pValRhs, "gMulTmp"); break;
					}
				}break;
				case '/':
				{
					switch (pTinOutput->m_tink)
					{
						case TINK_Integer:	pInstOp = pBuild->PInstCreate(IROP_NDiv, pValLhs, pValRhs, "nDivTmp"); break;
						case TINK_Float:	pInstOp = pBuild->PInstCreate(IROP_GDiv, pValLhs, pValRhs, "gDivTmp"); break;
					}
				}break;
				default: break;
			}

			EWC_ASSERT(pInstOp, "%s operator unsupported in codegen", PChzFromJtok(pStnod->m_jtok));
			pBuild->PBlockEnsure()->Append(pInstOp);
			return pInstOp;

		}break;
	}
	return nullptr;
}

CIRBasicBlock * CIRBuilder::PBlockEnsure()
{
	if (!m_pBlockRoot)
	{
		m_pBlockRoot = EWC_NEW(m_pAlloc, CIRBasicBlock) CIRBasicBlock(m_pAlloc);
	}

	return m_pBlockRoot;
}