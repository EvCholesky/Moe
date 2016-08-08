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

#include "EwcTypes.h"

inline s64		NAbs(s64 n)
					{ return abs(n); }

struct SBigInt // tag = bint
{
			SBigInt()
			:m_nAbs(0)
			, m_fIsNegative(false)
				{ ; }

			s64 S64Coerce() const
			{
				EWC_ASSERT(m_nAbs < INT_MAX, "int too large to be signed value");
				if (m_fIsNegative)
					return -(s64)m_nAbs;
				return (s64)m_nAbs;
			}

			u64 U64Coerce() const
			{
				EWC_ASSERT(!m_fIsNegative, "negative value being coerced into an unsigned value");
				return m_nAbs;
			}

	u64		m_nAbs;
	bool	m_fIsNegative;
};

inline SBigInt BintFromInt(s64 nSigned)
{
	SBigInt bint;
	bint.m_nAbs = NAbs(nSigned);
	bint.m_fIsNegative = nSigned < 0;
	return bint;
}

inline SBigInt BintFromUint(u64 nUnsigned, bool fIsNegative = false)
{
	SBigInt bint;
	bint.m_nAbs = nUnsigned;
	bint.m_fIsNegative = fIsNegative;
	return bint;
}



// NOTE: I'm opting away from operator overloading for Signed65 because I want it to be apparent something
//  unusual is happening here... maybe I'll change my mind later.
inline SBigInt BintAdd(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = bintLhs.m_nAbs;
	u64 nRhs = bintRhs.m_nAbs;
	if (bintLhs.m_fIsNegative == bintRhs.m_fIsNegative)
	{
		return BintFromUint(nLhs + nRhs, bintLhs.m_fIsNegative);
	}

	// one of the operands is negative, if it's the larger one the result is negative
	if (nLhs > nRhs)
	{
		return BintFromUint(nLhs - nRhs, bintLhs.m_fIsNegative);
	}
	return BintFromUint(nRhs - nLhs, bintRhs.m_fIsNegative);
}

inline SBigInt BintSub(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = bintLhs.m_nAbs;
	u64 nRhs = bintRhs.m_nAbs;
	bool fIsRhsNegative = !bintRhs.m_fIsNegative;
	if (bintLhs.m_fIsNegative == fIsRhsNegative)
	{
		return BintFromUint(nLhs + nRhs, bintLhs.m_fIsNegative);
	}

	// one of the operands is negative, if it's the larger one the result is negative
	if (nLhs > nRhs)
	{
		return BintFromUint(nLhs - nRhs, bintLhs.m_fIsNegative);
	}
	return BintFromUint(nRhs - nLhs, fIsRhsNegative);
}

inline SBigInt BintMul(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return BintFromUint(bintLhs.m_nAbs * bintRhs.m_nAbs, bintLhs.m_fIsNegative != bintRhs.m_fIsNegative);
}

inline SBigInt BintDiv(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return BintFromUint(bintLhs.m_nAbs / bintRhs.m_nAbs, bintLhs.m_fIsNegative != bintRhs.m_fIsNegative);
}

inline SBigInt BintRemainder(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	// NOTE: The sign of c++'s % operator is determined by the sign of the numerator:
	//  % is defined as: a == (a/b) * b + a%b, where a/b truncates towards zero.
	//  so with a - (a/b)*b == a%b we know that (a/b)*b is smaller than a, thus a determines the sign.
	return BintFromUint(bintLhs.m_nAbs % bintRhs.m_nAbs, bintLhs.m_fIsNegative);
}

inline u64 NTwosCompliment(u64 n, bool fIsNegative)
{
	return (fIsNegative) ? (~n + 1) : n;
}

inline SBigInt BintBitwiseOr(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	u64 nRhs = NTwosCompliment(bintRhs.m_nAbs, bintRhs.m_fIsNegative);

	bool fIsOutNegative = bintLhs.m_fIsNegative | bintRhs.m_fIsNegative;
	u64 nOut = NTwosCompliment(nLhs | nRhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}

inline SBigInt BintBitwiseAnd(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	u64 nRhs = NTwosCompliment(bintRhs.m_nAbs, bintRhs.m_fIsNegative);

	bool fIsOutNegative = bintLhs.m_fIsNegative & bintRhs.m_fIsNegative;
	u64 nOut = NTwosCompliment(nLhs & nRhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}

inline SBigInt BintBitwiseNot(const SBigInt & bintOp)
{
	u64 nLhs = NTwosCompliment(bintOp.m_nAbs, bintOp.m_fIsNegative);

	bool fIsOutNegative = !bintOp.m_fIsNegative;
	u64 nOut = NTwosCompliment(~nLhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}

inline SBigInt BintBitwiseXor(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	u64 nRhs = NTwosCompliment(bintRhs.m_nAbs, bintRhs.m_fIsNegative);

	bool fIsOutNegative = bintLhs.m_fIsNegative ^ bintRhs.m_fIsNegative;
	u64 nOut = NTwosCompliment(nLhs ^ nRhs, fIsOutNegative);

	return BintFromUint(nOut, fIsOutNegative);
}
inline SBigInt BintShiftRight(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	if (bintRhs.m_fIsNegative)
	{
		return BintFromUint(NTwosCompliment(nLhs << bintRhs.m_nAbs, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
	else
	{
		u64 nSignExtend = (bintLhs.m_fIsNegative) ? (0xFFFFFFFFFFFFFFFF << (64 - bintRhs.m_nAbs)) : 0;
		return BintFromUint(NTwosCompliment((nLhs >> bintRhs.m_nAbs) | nSignExtend, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
}

inline SBigInt BintShiftLeft(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	u64 nLhs = NTwosCompliment(bintLhs.m_nAbs, bintLhs.m_fIsNegative);
	if (bintRhs.m_fIsNegative)
	{
		u64 nSignExtend = (bintLhs.m_fIsNegative) ? 0x8000000000000000 : 0;
		return BintFromUint(NTwosCompliment((nLhs >> bintRhs.m_nAbs) | nSignExtend, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
	else
	{
		return BintFromUint(NTwosCompliment(nLhs << bintRhs.m_nAbs, bintLhs.m_fIsNegative), bintLhs.m_fIsNegative);
	}
}

inline bool operator ==(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return (bintLhs.m_nAbs == bintRhs.m_nAbs) & (bintLhs.m_fIsNegative == bintRhs.m_fIsNegative);
}

inline bool operator !=(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return !(bintLhs == bintRhs);
}

inline bool operator<(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	if (bintLhs.m_fIsNegative != bintRhs.m_fIsNegative)
		return bintLhs.m_fIsNegative;

	return (bintLhs.m_nAbs < bintRhs.m_nAbs) != bintLhs.m_fIsNegative;
}

inline bool operator<=(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return (bintLhs == bintRhs) | (bintLhs < bintRhs);
}

inline bool operator >(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return !(bintLhs <= bintRhs);
}

inline bool operator >=(const SBigInt & bintLhs, const SBigInt & bintRhs)
{
	return !(bintLhs < bintRhs);
}
