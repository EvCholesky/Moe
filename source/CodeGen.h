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

namespace llvm
{
	class Value;
	class BasicBlock;
}

class CIRInstruction;
class CSTNode;
class LlvmIRBuilder; // wrapper class to avoid the forward decl template mess.


class CIRBasicBlock		// tag = block
{
public:
						CIRBasicBlock(EWC::CAlloc * pAlloc);
						~CIRBasicBlock();

	//void				Insert(CIRInstruction * pInst, int iInst);
	void				Append(CIRInstruction * pInst);

	llvm::BasicBlock *				m_pLblock;
	EWC::CDynAry<CIRInstruction *>	m_arypInst;		// BB - eventually this should be replaced with something that has 
													// better random insertion performance.
};



enum VALK	// VALue Kind
{
	VALK_Constant,

	VALK_ProcedureDefinition,

	VALK_Instruction,
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
		OP(Ret), \
		OP_RANGE(TerminalOp, Ret), \
		\
		OP(NAdd), \
		OP(GAdd), \
		OP(NSub), \
		OP(GSub), \
		OP(NMul), \
		OP(GMul), \
		OP(NDiv), \
		OP(GDiv), \
		OP_RANGE(BinaryOp, TerminalOpMax), \
		\
		OP(Shl), \
		OP(Shr), \
		OP(And), \
		OP(Or), \
		OP(Xor), \
		OP_RANGE(LogicOp, BinaryOpMax), \
		\
		OP(Load), \
		OP(Store), \
		OP_RANGE(MemoryOp, LogicOpMax), \
		\
		OP(Trunc), \
		OP(GToS64), \
		OP(GToU64), \
		OP(S64ToG), \
		OP(U64ToG), \
		OP(GTrunc), \
		OP(GExtend), \
		OP(PtrToInt), \
		OP(IntToPtr), \
		OP_RANGE(CastOp, MemoryOpMax), \

#define OP(x) IROP_##x
#define OP_RANGE(range, prev) IROP_##range##Max, IROP_##range##Min = IROP_##prev
	enum IROP
	{
		OPCODE_LIST

		IROP_Max,
		IROP_Min = 0,
		IROP_Nil = -1,
	};
#undef OP
#undef OP_RANGE



class CIRValue		// tag = val
{
public:
						CIRValue(VALK valk);

	llvm::Value *		m_pLval;
	CSTNode *			m_pStnod;
	VALK				m_valk;
};



class CIRConstant : public CIRValue
{
public:
						CIRConstant()
						:CIRValue(VALK_Constant)
							{ ; }
};



class CIRProcedure : public CIRValue
{
public:
						CIRProcedure()
						:CIRValue(VALK_ProcedureDefinition)
							{ ; }
};



class CIRInstruction : public CIRValue	// tag = inst
{
public:
						CIRInstruction(IROP irop)
						:CIRValue(VALK_Instruction)
						,m_pBlock(nullptr)
						,m_cpValOperand(0)
						,m_irop(irop)
							{ ; }

	s8					CpValOpcodeMax();

	CIRBasicBlock *		m_pBlock;
	s8					m_cpValOperand;		// current opcode count
	IROP				m_irop;

	// NOTE - opcode array needs to be at the end of the struct, we allocate room for extra elements if needed.
	CIRValue *			m_apValOperand[1];	
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



class CIRBuilder		// tag = build
{
public:
						CIRBuilder(EWC::CAlloc * pAlloc);
						~CIRBuilder();
	
	CIRInstruction *	PInstCreate(IROP irop, CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRBasicBlock *		PBlockEnsure();

	/*
	CIRInstruction *	PInstCreateNAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGAdd(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateNSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGSub(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateNMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGMul(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateNDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	CIRInstruction *	PInstCreateGDiv(CIRValue * pValLhs, CIRValue * pValRhs, const char * pChzName);
	*/

	LlvmIRBuilder *		m_pLbuilder;
	EWC::CAlloc *		m_pAlloc;
	SInsertPoint		m_inspt;
	CIRBasicBlock *		m_pBlockRoot;
};

CIRValue * PValGenerate(CIRBuilder * pBuild, CSTNode * pStnod);