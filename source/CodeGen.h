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



enum VALK	// VALue Kind
{
	VALK_Constant,

	VALK_PrototypeDefinition,

	VALK_TerminatorInst,
	VALK_BinaryOpInst,
	VALK_UnaryOpInst,
	VALK_CastInst,
	VALK_CompareInst,
	VALK_ReturnInst,
	VALK_InvokeInst,

	VALK_EndInst,
	VALK_BeginInst = VALK_TerminatorInst,

	VALK_Max = VALK_EndInst,
	VALK_Min = 0,
	VALK_Nil = -1,
};
EWC_ENUM_UTILS(VALK);

class CIRValue		// tag = val
{
public:
						CIRValue(VALK valk);

	CSTNode *			m_pStnod;
	VALK				m_valk;
};



class CIRBasicBlock		// tag = bblock
{
public:
						CIRBasicBlock(EWC::CAlloc * pAlloc);
	void				Insert(CIRInstruction * pInst, iInst);

	EWC::CDynAry<CIRInstruction *>	m_arypInst;		// BB - eventually this should be replaced with something that has 
													// better random insertion performance.
};



class CIRConstant : public CIRValue
{
public:
						CIRConstant();
};



class CIRProcedure : public CIRValue
{
public:
						CIRProcedure();
};


class CIRInstruction : public CIRValue	// tag = inst
{
public:
						CIRInstruction(VALK valk);

	CIRBasicBlock *		m_pBblock;
};



struct SInsertPoint		// tag = inspt
{
	CIRBasicBlock *		m_pBblock;
	s32					m_iInst;
};



class CIRBuilder		// tag = build
{
	SInsertPoint		m_inspt;
	CIRBasicBlock *		m_pBblockRoot;
};