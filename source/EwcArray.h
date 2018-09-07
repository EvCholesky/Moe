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

#ifndef EWC_ARRAY_GUARD
#define EWC_ARRAY_GUARD

#include "EwcTypes.h"

namespace EWC
{

// base array template
template <typename T>
class CAry // tag=ary
{
public:
	typedef T Type;

	class CIterator // tag=iter
	{
	public:
				CIterator(CAry<T> * pAry)
				:m_pCurrent(pAry->m_a)
				,m_pEnd(&pAry->m_a[pAry->C()])
					{ ; }
								
		T *		Next() 
					{
						if (m_pCurrent == m_pEnd)
							return nullptr;

						return m_pCurrent++;
					}

		T * m_pCurrent;
		T * m_pEnd;
	};


				CAry(BK bk = BK_Nil)
				:m_a(nullptr)
				,m_c(0)
				,m_cMax(0)
				,m_bk(bk)
					{ ; }

				CAry(T * a, size_t c, size_t cMax, BK bk = BK_Nil)
				:m_a(a)
				,m_c(c)
				,m_cMax(cMax)
				,m_bk(bk)
					{ ; }

				CAry(const CAry&) = delete;
	CAry &		operator=(const CAry&) = delete;

	const T &	operator[](size_t i) const	{ EWC_ASSERT((i>=0) & (i<m_c), "array overflow"); return m_a[i]; }
	T &			operator[](size_t i)		{ EWC_ASSERT((i>=0) & (i<m_c), "array overflow"); return m_a[i]; }

	T *			A()							{ return m_a; }
	const T*	A() const					{ return m_a; }
	size_t		C() const					{ return m_c; }
	size_t		CMax() const				{ return m_cMax; }
	bool		FIsEmpty() const			{ return m_c == 0; }
	T *			PMac()						{ return &m_a[m_c]; }
	const T *	PMac() const				{ return &m_a[m_c]; }
	T *			PLast()						
					{ 
						if (EWC_FVERIFY(m_c > 0, "using PLast on empty CAry"))
							return &m_a[m_c-1];
						return nullptr;
					}
	T 			Last()						
					{ 
						if (EWC_FVERIFY(m_c > 0, "using Last on empty CAry"))
							return m_a[m_c-1];
						return T();
					}
	const T *	PLast() const
					{ 
						if (EWC_FVERIFY(m_c > 0, "using PLast on empty CAry"))
							return &m_a[m_c-1];
						return nullptr;
					}
	size_t		IFromP(const T * pT) const
					{ 
						size_t iT = ((uintptr_t)pT - (uintptr_t)m_a) / sizeof(T); 
						EWC_ASSERT((iT >= 0) & (iT < m_c), "pointer not contained within array bounds");
						return iT; 
					}

	void		Clear()
					{
						if (m_c)
							DestructN(m_a, m_c);
						m_c = 0;
					}

	void		Reverse()
					{
						if (FIsEmpty())
							return;

						size_t iEnd = C() -1;
						for (size_t i = 0; i < iEnd; ++i, --iEnd)
						{
							T t = m_a[i];
							m_a[i] = m_a[iEnd];
							m_a[iEnd] = t;
						}
					}

	// Note - These allocation routines are here so that we can operate on CFixAry without passing the template argument C_MAX
	void		Append(const Type t)
					{
						EWC_ASSERT(m_c + 1 <= m_cMax, "fixed array overflow, %d + 1 > %d", m_c, m_cMax);
						T * retValue = &m_a[m_c++];
						CopyConstruct(retValue, t);
					}
	void		Append(const Type * pTArray, size_t cT)
					{
						if (!cT)
							return;

						EWC_ASSERT(m_c + cT <= m_cMax, "fixed array overflow. %d + %d > %d", m_c, cT, m_cMax);
						T * pTEnd = &m_a[m_c];
						CopyConstructArray(pTEnd, cT, pTArray);
						m_c += cT;
					}

	void		AppendFill(size_t c, const Type t)
					{
						EWC_ASSERT(m_c + c <= m_cMax, "fixed array overflow");
						CopyConstructN(m_a, c, t);
						m_c += c;
					}

	T *			AppendNew()
					{
						EWC_ASSERT(m_c + 1 <= m_cMax, "fixed array overflow");
						T * retValue = &m_a[m_c++];
						Construct(retValue);
						return retValue;
					}
	void		PopLast()
					{
						if (EWC_FVERIFY(m_c > 0, "array underflow"))
						{
							--m_c;
							Destruct(&m_a[m_c]);
						}
					}

	T			TPopLast()
					{
						T last = Last();
						PopLast();
						return last;
					}

	void		Swap(CAry<T> * paryTOther)
					{
						T * m_aTemp    = m_a;
						size_t m_cTemp    = m_c;
						size_t m_cMaxTemp = m_cMax;

						m_a    = paryTOther->m_a;
						m_c    = paryTOther->m_c;
						m_cMax = paryTOther->m_cMax;

						paryTOther->m_a    = m_aTemp;
						paryTOther->m_c    = m_cTemp;
						paryTOther->m_cMax = m_cMaxTemp;

						auto bkTemp			= paryTOther->m_bk;
						paryTOther->m_bk	= m_bk;
						m_bk				= bkTemp;
					}

	T *			m_a;
	size_t		m_c;
	size_t		m_cMax;
	BK			m_bk;
};


// heap allocated array that will NOT resize itself
template <typename T>
class CAllocAry : public CAry<T>
{
public:
	typedef T Type;
	using CAry<T>::m_a;		// workaround for templated base class dependent names 
	using CAry<T>::m_c;	
	using CAry<T>::m_cMax;	
	using CAry<T>::m_bk;

				CAllocAry(CAlloc * pAlloc, BK bk, size_t cMaxStarting = 16)
				:CAry<T>(nullptr, 0, 0, BK_Nil)
					{ SetAlloc(pAlloc, bk, cMaxStarting); }

				CAllocAry(BK bk = BK_Nil)
				:CAry<T>(nullptr, 0, 0, bk)
				,m_pAlloc(nullptr)
					{ ; }

				~CAllocAry()
					{ 
						if (m_pAlloc)
						{
							m_pAlloc->EWC_FREE(m_a);
						}
					}

	void		SetArray(T * a, size_t c, size_t cMax)
				{
					EWC_ASSERT((m_a == nullptr) | (a == nullptr), "overwriting nonzero buffer, leaking memory");
					m_a    = a;
					m_c    = c;
					m_cMax = cMax;
				}

	void		SetAlloc(CAlloc * pAlloc, BK bk, size_t cMax)
				{
					EWC_ASSERT(m_a == nullptr, "overwriting nonzero buffer, leaking memory");
					m_pAlloc = pAlloc;

					size_t cB = sizeof(T) * cMax;
					m_a = (T *)pAlloc->EWC_ALLOC_BK(cB, EWC_ALIGN_OF(T), m_bk);

					m_c    = 0;
					m_cMax = cMax;
					m_bk = bk;
				}

	CAlloc * 	m_pAlloc;
};


template <typename T, int C_PER_BLOCK>
class CBlockList //tag=blist
{
public:
	struct SBlock // tag = block
	{
		SAlignedBytes<sizeof(T) * C_PER_BLOCK, EWC_ALIGN_OF(T)>		m_alby;
		size_t 														m_c;
		SBlock *													m_pBlockNext;

		T * 				A()	
							{ return (T*)m_alby.A(); }
	};

	typedef T Type;

	class CIterator // tag=iter
	{
	public:
				CIterator(CBlockList<T, C_PER_BLOCK> * pBlist)
				:m_pBlock(&pBlist->m_blockRoot)
				,m_pT(nullptr)
					{ ; }
								
		T *		Next() 
					{
						while (m_pBlock)
						{
							if (!m_pT)
							{
								m_pT = m_pBlock->A();
							}
							else
							{
								++m_pT;
							}

							T * pMac = &m_pBlock->A()[m_pBlock->m_c];
							if (m_pT == pMac)
							{
								m_pBlock = m_pBlock->m_pBlockNext;
								m_pT = nullptr;
							}
							else
							{
								return m_pT;
							}
						}

						m_pBlock = nullptr; // don't loop around the last block
						return nullptr;
					}

		SBlock *	m_pBlock;
		T * 		m_pT;
	};

				CBlockList(CAlloc * pAlloc, BK bk)
				:m_blockRoot()
				,m_c()
				,m_bk(bk)
				,m_pAlloc(nullptr)
					{ SetAlloc(pAlloc, bk); }

				CBlockList(BK bk = BK_Nil)
				:m_blockRoot()
				,m_c()
				,m_bk(bk)
				,m_pAlloc(nullptr)
					{ ; }

				~CBlockList()
					{ Clear(); }

	void		SetAlloc(CAlloc * pAlloc, BK bk)
					{
						FreeAll();

						m_pAlloc = pAlloc;
						m_bk = bk;
					}

	void		Clear()
					{ FreeAll(); }

	void		Append(const Type & t)
					{
						auto pT = AllocateUnconstructed();
						CopyConstruct(pT, t);
					}
	T *			AppendNew()
					{
						auto pT = AllocateUnconstructed();
						Construct(pT);
						return pT;
					}

	T * 		AllocateUnconstructed()
					{
						++m_c;
						SBlock * pBlockIt = &m_blockRoot;
						while (1)
						{
							if (pBlockIt->m_c < C_PER_BLOCK)
							{
								return &pBlockIt->A()[pBlockIt->m_c++];		
							}

							if (pBlockIt->m_pBlockNext == nullptr)
								break;

							pBlockIt = pBlockIt->m_pBlockNext;
						}

						EWC_ASSERT(m_pAlloc, "null allocator in Block List");

						auto pBlockNew = (SBlock *)m_pAlloc->EWC_ALLOC_BK(sizeof(SBlock), EWC_ALIGN_OF(SBlock), m_bk);
						pBlockNew->m_c = 1;
						pBlockNew->m_pBlockNext = nullptr;
						pBlockIt->m_pBlockNext = pBlockNew;

						return &pBlockNew->A()[0];
					}

	void			FreeAll()
					{
						SBlock * pBlockIt	= m_blockRoot.m_pBlockNext;
						while (pBlockIt)
						{
							SBlock * pBlockFree = pBlockIt;
							pBlockIt = pBlockIt->m_pBlockNext;

							DestructN(pBlockFree->A(), pBlockFree->m_c);
							m_pAlloc->EWC_FREE(pBlockFree);
						}

						DestructN(m_blockRoot.A(), m_blockRoot.m_c);
						m_blockRoot.m_c = 0;
						m_blockRoot.m_pBlockNext = nullptr;
					}
	size_t			C() const					{ return m_c; }
	bool			FIsEmpty() const			{ return m_c == 0; }


	SBlock 			m_blockRoot;
	size_t			m_c;
	BK 				m_bk;
	CAlloc *		m_pAlloc;
};


// resizable array (aka std::vector)
template <typename T>
class CDynAry : public CAry<T> //tag=ary
{
public:
	typedef T Type;
	using CAry<T>::m_a;		// workaround for templated base class dependent names 
	using CAry<T>::m_c;	
	using CAry<T>::m_cMax;	
	using CAry<T>::m_bk;

				CDynAry(CAlloc * pAlloc, BK bk, size_t cMaxStarting = 16)
				:CAry<T>(nullptr, 0, 0, BK_Nil)
					{ SetAlloc(pAlloc, bk, cMaxStarting); }

				CDynAry(BK bk = BK_Nil)
				:CAry<T>(nullptr, 0, 0, bk)
				,m_pAlloc(nullptr)
					{ ; }

				CDynAry(const CDynAry & rhs)
					{
						m_pAlloc = rhs.m_pAlloc;
						EnsureSize(rhs.C());

						const T * pMac = rhs.PMac();
						for (const T * pT = rhs.A(); pT != pMac; ++pT)
						{
							Append(*pT);
						}
					}

				~CDynAry()
					{ Clear(); }

	CDynAry<T> & operator= (const CDynAry & rhs)
					{
						Clear();
						m_pAlloc = rhs.m_pAlloc;
						EnsureSize(rhs.C());

						const T * pMac = rhs.PMac();
						for (const T * pT = rhs.A(); pT != pMac; ++pT)
						{
							Append(*pT);
						}
						return *this;
					}

	void		SetAlloc(CAlloc * pAlloc, BK bk, size_t cMaxStarting = 32)
					{
						m_pAlloc = pAlloc;
						m_bk = bk;
						Resize(cMaxStarting);
					}

	void		Clear()
					{ Resize(0); }

	void		Append(const Type t)
					{
						EnsureSize(m_c+1);
						T * pTEnd = &m_a[m_c++];
						CopyConstruct(pTEnd, t);
					}

	void		Append(const Type * pTArray, size_t cT)
					{
						if (!cT)
							return;

						EnsureSize(m_c + cT);
						T * pTEnd = &m_a[m_c];
						CopyConstructArray(pTEnd, cT, pTArray);
						m_c += cT;
					}

	void		AppendFill(size_t c, const Type t)
					{
						EnsureSize(m_c + c);
						CopyConstructN(&m_a[m_c], c, t);
						m_c += c;
					}

	T *			AppendNew()
					{
						EnsureSize(m_c+1);
						T * retValue = &m_a[m_c++];
						Construct(retValue);
						return retValue;
					}

	void		EnsureSize(size_t cSize)
					{
						size_t c = cSize;
						if (c > m_cMax) 
						{
							size_t cNew = ewcMax(m_cMax * 2, c); 
							Resize(cNew);
						}
					}
	void		Remove(const Type t)
					{
						T * pEnd = &m_a[m_c];
						for (T * pSrc = m_a, * pDst = m_a; pSrc != pEnd; ++pSrc)
						{
							if(t == *pSrc)
							{
								--m_c;
							}
							else
							{
								*pDst++ = *pSrc;
							}
						}

						// BB - doesn't destruct the object until it resizes!

						size_t cResize = (m_c < 8) ? 0 : m_cMax / 2;
						if(m_c < cResize)
							Resize(cResize);
					}
	void		RemoveFastByI(size_t iT)
					{
						if (EWC_FVERIFY((iT >= 0) & (iT < m_c), "bad element index" ))
						{
							--m_c;
							Destruct(&m_a[iT]);
							if (iT != m_c)
								m_a[iT] = m_a[m_c];

							size_t cResize = (m_c < 8) ? 0 : m_cMax / 2;
							if(m_c <= cResize)
								Resize(cResize);
						}
					}

	void		PopLast()
					{
						if (EWC_FVERIFY(m_c > 0, "array underflow"))
						{
							--m_c;
							Destruct(&m_a[m_c]);

							size_t cResize = (m_c < 8) ? 0 : m_cMax / 2;
							if(m_c <= cResize)
								Resize(cResize);
						}
					}
		
	void		Resize(size_t cMax)
					{
						size_t cNewMax = cMax;
						EWC_ASSERT(cNewMax == static_cast<s32>(cNewMax), "size overflow in CDynAry");
						if (cNewMax == m_cMax)
							return;

						T * aOld = m_a;
						if (cNewMax < m_cMax)
						{
							if (cNewMax < m_c)
							{
								DestructN(&m_a[cNewMax], m_c - cNewMax);
								m_c = cNewMax;
							}
						}

						if (cNewMax > 0)
						{
							size_t cB = sizeof(T) * cNewMax;
							m_a = (T *)m_pAlloc->EWC_ALLOC_BK(cB, EWC_ALIGN_OF(T), m_bk);

							if (aOld)
							{
								auto cMin = (m_cMax < cNewMax) ? m_cMax : cNewMax;
								CopyAB(aOld, m_a, sizeof(T) * cMin);
							}
						}
						else
						{
							m_a = nullptr;
						}

						if (aOld)
							m_pAlloc->EWC_FREE(aOld);
						m_cMax = cNewMax;
					}

	void		Swap(CDynAry<T> * paryTOther)
					{
						T * m_aTemp    = m_a;
						size_t m_cTemp    = m_c;
						size_t m_cMaxTemp = m_cMax;

						m_a    = paryTOther->m_a;
						m_c    = paryTOther->m_c;
						m_cMax = paryTOther->m_cMax;

						paryTOther->m_a    = m_aTemp;
						paryTOther->m_c    = m_cTemp;
						paryTOther->m_cMax = m_cMaxTemp;

						auto bkTemp			= paryTOther->m_bk;
						paryTOther->m_bk	= m_bk;
						m_bk				= bkTemp;

						auto pAllocTemp			= paryTOther->m_pAlloc;
						paryTOther->m_pAlloc	= m_pAlloc;
						m_pAlloc				= pAllocTemp;
					}

	CAlloc *	m_pAlloc;
};

// fixed sized array container template
template <typename T, int C_MAX>
class CFixAry : public CAry<T> // tag=ary
{
public:
	typedef T Type;
	using CAry<T>::m_a;		// workaround for templated base class dependent names 
	using CAry<T>::m_c;	
	using CAry<T>::m_cMax;	
	using CAry<T>::m_bk;

				CFixAry()
				:CAry<T>()
					{ 
						m_a = (T *)m_alby.A(); 
						m_c = 0;
						m_cMax = C_MAX;
					}

				~CFixAry()
					{ Clear(); }

				CFixAry(const CFixAry & rhs)
					{
						const T * pMac = rhs.PMac();
						for (const T * pT = rhs.A(); pT != pMac; ++pT)
						{
							Append(*pT);
						}
					}

	CFixAry& operator=(const CFixAry & rhs)
					{
						Clear();

						const T * pMac = rhs.PMac();
						for (const T * pT = rhs.A(); pT != pMac; ++pT)
						{
							Append(*pT);
						}
						return *this;
					}

	void		Append(const Type t)
					{
						EWC_ASSERT(m_c < m_cMax, "CFixAry overflow");
						T * retValue = &m_a[m_c++];
						CopyConstruct(retValue, t);
					}

	void		AppendFill(size_t c, const Type t)
					{
						EWC_ASSERT(m_c < m_cMax, "CFixAry overflow");
						CopyConstructN(&m_a[m_c], c, t);
						m_c += c;
					}

	T *			AppendNew()
					{
						EWC_ASSERT(m_c < m_cMax, "CFixAry overflow");
						T * retValue = &m_a[m_c++];
						Construct(retValue);
						return retValue;
					}

	void		Remove(const Type t)
					{
						T * pEnd = &m_a[m_c];
						for (T * pSrc = m_a, * pDst = m_a; pSrc != pEnd; ++pSrc)
						{
							if(t == *pSrc)
							{
								Destruct(pSrc);
								--m_c;
							}
							else
							{
								*pDst++ = *pSrc;
							}
						}
					}
		
	void			Clear()
					{
						T * pTMac = &m_a[m_c];
						for (T * pT = m_a; pT != pTMac; ++pT)
						{
							Destruct(pT);
						}
						m_c = 0;
					}
				

	SAlignedBytes<sizeof(T) * C_MAX, EWC_ALIGN_OF(T)>	m_alby;
};

template <typename T>
void ReverseArray(T * aT, size_t cT)
{
	T * pMin = aT;
	T * pMac = &aT[cT-1];
	while ((uintptr_t)pMin < (uintptr_t)pMac)
	{
		T temp = *pMac;
		*pMac = *pMin;
		*pMin = temp;
		++pMin;
		--pMac;
	}
}

template <typename T, class FCMP>
int ILowerBound(T * aT, size_t cT, const T * pTNew, FCMP fcmp)
{
	// return the first position pTNew could be inserted and maintain ordering

	size_t iLb = 0;
	int di = cT;

	while(di > 0)
	{
		int diHalf = di >> 1;
		int iMid = iLb + diHalf;
		int nCmp = fcmp(aT[iMid], *pTNew);

		if(nCmp)
		{
			iLb = iMid + 1;
			di -= diHalf + 1;
		}
		else
		{
			di = diHalf;
		}
	}
	return iLb;
}

template <typename T, class FCMP>
void ReplaceSorted(T * aT, size_t cT, T * pTPrev, const T & tNew, FCMP fcmp)
{
	// replaces a sorted element with another instance (that has a new sort key)
	//  NOTE: tNew cannot be a value in the array (or it will mess up our search for the lower bound)
	
	int iLbNew = ILowerBound(aT, cT, &tNew, fcmp);
	if (!EWC_FVERIFY((iLbNew >= 0) & (iLbNew <= (int)cT), "bad lower bound"))
		return;

	T * pTNew = &aT[iLbNew];
	if ((uintptr_t)pTNew < (uintptr_t)pTPrev)
	{
		for (T * pT = pTPrev; pT != pTNew; --pT)
		{
			*pT = *(pT-1);
		}
	}
	else if (pTNew != pTPrev)
	{
		--pTNew; // we're removing pTPrev but didn't account for it when computing ILowerBound
		for (T * pT = pTPrev; pT != pTNew; ++pT)
		{
			*pT = *(pT + 1);
		}
	}
	*pTNew = tNew;
}

template <typename T, class FCMP>
T * PTLookupSorted(T * aT, size_t cT, const T & tNew, FCMP fcmp)
{
	int iLbNew = ILowerBound(aT, cT, &tNew, fcmp);
	if (iLbNew >= 0)
	{
		if (!fcmp(aT[iLbNew], tNew))
			return &aT[iLbNew];
	}
	return nullptr;
}

template <typename ARY, typename T, class FCMP>
void InsertSorted(ARY * pary, const T & tNew, FCMP fcmp)
{
	int iLbNew = ILowerBound(pary->A(), pary->C(), &tNew, fcmp);
	if (!EWC_FVERIFY((iLbNew >= 0) & (iLbNew <= (int)pary->C()), "bad lower bound"))
		return;

	pary->AppendNew();
	T * pTLast = pary->PMac()-1;
	T * pTNew = &(*pary)[iLbNew];
	for (T * pT = pTLast; pT != pTNew; --pT)
	{
		*pT = *(pT-1);
	}
	(*pary)[iLbNew] = tNew;
}

} // namespace EWC

#endif // EWC_ARRAY_GUARD