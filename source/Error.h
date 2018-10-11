/* Copyright (C) 2017 Evan Christensen
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

enum ERRID
{
	ERRID_UnknownError				= 0,
	ERRID_FailedOpeningFile 		= 1,
	ERRID_MissingEntryPoint			= 2,
	ERRID_FailedLoadingDLL			= 3,

	ERRID_ParserMin					= 1000,
	ERRID_EnumRepeat				= 1001,
	ERRID_ShadowedDefine			= 1002,
	ERRID_InvalidOpOverload			= 1003,
	ERRID_OldCStyle					= 1004,
	ERRID_EmptyCase					= 1005,
	ERRID_DefaultParamOpOverload	= 1006, 
	ERRID_MissingDefaultArgs		= 1007,
	ERRID_MissingName				= 1008,
	ERRID_NoGenericReturn			= 1009,
	ERRID_MultipleAnchorDef			= 1010,
	ERRID_UsingStatementNotAllowed	= 1011,
	ERRID_BadUsingSyntax			= 1012,
	ERRID_ExpectedEndOfLine			= 1013,
	ERRID_UnexpectedToken			= 1014,
	ERRID_ParserMax					= 2000,

	ERRID_TypeCheckMin				= ERRID_ParserMax,
	ERRID_InitTypeMismatch			= 2001,
	ERRID_TooFewArgs				= 2002,
	ERRID_TooManyArgs				= 2003,
	ERRID_NotLvalue					= 2004,
	ERRID_NotRvalue					= 2005,
	ERRID_BadImplicitConversion		= 2006,
	ERRID_CantFindProc				= 2007,
	ERRID_CantFindMain				= 2008,
	ERRID_IncorrectIvalk			= 2009,
	ERRID_BadArrayIndex				= 2010,
	ERRID_NotYetSupported			= 2011,
	ERRID_AmbiguousOverload			= 2012,
	ERRID_BadOverloadSig			= 2013,
	ERRID_ArgumentSuppliedTwice		= 2014,
	ERRID_NamedArgumentNotFound		= 2015,
	ERRID_GenericLookupFail			= 2016,
	ERRID_NoGenericRValue			= 2017,
	ERRID_BakingNonLiteralValue		= 2018,
	ERRID_StructParamsMustBeBaked	= 2019,
	ERRID_BadMemberLookup			= 2020,
	ERRID_CannotTakeReference		= 2021,
	ERRID_InvalidUnaryOp			= 2022,
	ERRID_UsingStatementCycle		= 2023,
	ERRID_UsingStatementCollision	= 2024,
	ERRID_UsingStatementBadType		= 2025,
	ERRID_LiteralMemberNotFound		= 2026,
	ERRID_LiteralUnnamedMember		= 2027,
	ERRID_TypeCheckMax				= 3000,

	ERRID_CodeGenMin				= ERRID_TypeCheckMax,
	ERRID_UnreachableInst			= 3001,
	ERRID_BadStore					= 3002,
	ERRID_BadCastGen				= 3003,
	ERRID_ObjFileFail				= 3004,
	ERRID_ZeroSizeInstance			= 3005,
	ERRID_UndefinedForeignFunction  = 3006,
	ERRID_CodeGenMax				= 4000,
	ERRID_ErrorMax					= 10000,

	ERRID_WarningMin				= 10000,
	ERRID_UnknownWarning			= 10000,
	ERRID_WarningMax				= 20000,


	ERRID_Max,
	ERRID_Min = 0,
	ERRID_Nil = -1
};