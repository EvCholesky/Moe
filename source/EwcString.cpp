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

#include "EwcString.h"
#include "EwcHash.h"
#include <stdio.h>

using namespace EWC;

namespace FastHash
{
	/* By Paul Hsieh (C) 2004, 2005.  Covered under the Paul Hsieh derivative 
	   license. See: 
	   http://www.azillionmonkeys.com/qed/weblicense.html for license details.

	   http://www.azillionmonkeys.com/qed/hash.html */

	/*
	#undef get16bits
	#if (defined(__GNUC__) && defined(__i386__)) || defined(__WATCOMC__) \
	  || defined(_MSC_VER) || defined (__BORLANDC__) || defined (__TURBOC__)
	#define get16bits(d) (*((const uint16_t *) (d)))
	#endif

	#if !defined (get16bits)
	#define get16bits(d) ((((uint32_t)(((const uint8_t *)(d))[1])) << 8)\
						   +(uint32_t)(((const uint8_t *)(d))[0]) )
	#endif
	*/

	EWC_FORCE_INLINE u16 NGet16bits ( const void * p )
	{
	  return *(const u16*)p;
	}

	u32 NSuperFastHash (const char * pB, size_t cB) 
	{
		u32 hash = 0, tmp;
		int cBRemain;

		if (cB <= 0 || pB == nullptr) return 0;

		cBRemain = cB & 3;
		cB >>= 2;

		// Main loop
		for (;cB > 0; --cB)
		{
			hash  += NGet16bits (pB);
			tmp    = (NGet16bits (pB+2) << 11) ^ hash;
			hash   = (hash << 16) ^ tmp;
			pB  += 2*sizeof(u16);
			hash  += hash >> 11;
		}

		// Handle end cases
		switch (cBRemain) 
		{
		case 3:
			hash += NGet16bits (pB);
			hash ^= hash << 16;
			hash ^= pB[sizeof(u16)] << 18;
			hash += hash >> 11;
			break;
		case 2:
			hash += NGet16bits (pB);
			hash ^= hash << 11;
			hash += hash >> 17;
			break;
		case 1:
			hash += *pB;
			hash ^= hash << 10;
			hash += hash >> 1;
		}

		// Force "avalanching" of final 127 bits
		hash ^= hash << 3;
		hash += hash >> 5;
		hash ^= hash << 4;
		hash += hash >> 17;
		hash ^= hash << 25;
		hash += hash >> 6;

		return hash;
	}
} // namespace FastHash	

namespace EWC
{

// string table for ref-counted string class (only used by CString)
class CStringTable // tag=strtab
{
public:
				CStringTable(CAlloc * pAlloc)
				:m_pAlloc(pAlloc)
				,m_mpHvEntry(pAlloc,128)
					{ ; }

	const char* PChzAlloc(const char * pChz, size_t cCh, HV hv)
					{
						Entry * pEntry = nullptr;
						if (m_mpHvEntry.FinsEnsureKey(hv, &pEntry) == FINS_Inserted)
						{
							size_t cB = cCh + 1;
							pEntry->m_cRef = 1;
							pEntry->m_pChz = (char*)m_pAlloc->EWC_ALLOC(sizeof(char) * cB, EWC_ALIGN_OF(char));
							(void) CChCopy(pChz, pEntry->m_pChz, cB);
						}
						else
						{
							++pEntry->m_cRef;
							EWC_ASSERT(FAreSame(pEntry->m_pChz, pChz, cCh), "bad table lookup in CStringTable");
						}

						return pEntry->m_pChz;
					}

	void		FreePChz(const char * pChz, size_t cB,HV hv)
					{
						Entry * pEntry = m_mpHvEntry.Lookup(hv);
						if (!pEntry)
						{
							EWC_ASSERT(false, "failed lookup in CStringTable::FreePchz");
							return;
						}
						--pEntry->m_cRef;
						if (pEntry->m_cRef <= 0)
						{
							m_pAlloc->EWC_FREE(pEntry->m_pChz);
							m_mpHvEntry.Remove(hv);
						}
					}

	struct Entry
	{
		char *	m_pChz;
		u16		m_cRef;
	};

	CAlloc * 			m_pAlloc;
	CHash<HV, Entry>	m_mpHvEntry;
};

CStringTable * CString::s_pStrtab = nullptr;

typedef CHash<OID, CString> OidTable;
OidTable * s_pHashOidStr = nullptr;

OID OID_Nil;


// CString Methods:
void CString::StaticInit(CAlloc * pAlloc)
{
	s_pStrtab = EWC_NEW(pAlloc, CStringTable) CStringTable(pAlloc);
}

void CString::StaticShutdown(CAlloc * pAlloc)
{
	pAlloc->EWC_DELETE(s_pStrtab);
	s_pStrtab = nullptr;
}

void CString::SetPChz(const char * pChzNew)
{
	EWC_ASSERT(s_pStrtab, "String table has not been allocated");
	if (!s_pStrtab)
	{
		m_shash = CStringHash(0);
		m_pChz = nullptr;
		return;
	}

	if(m_pChz)
	{
		size_t cB = EWC::CCh(m_pChz) + 1;
		s_pStrtab->FreePChz(m_pChz, cB, m_shash.HvRaw());
		m_pChz = nullptr;
		m_shash = CStringHash(0);
	}

	if(pChzNew)
	{
		m_shash = CStringHash(pChzNew);
		m_pChz = s_pStrtab->PChzAlloc(pChzNew, EWC::CCh(pChzNew), m_shash.HvRaw());
	}
}

void CString::SetPCh(const char * pChNew, size_t cCh)
{
	EWC_ASSERT(s_pStrtab, "String table has not been allocated");
	if (!s_pStrtab)
	{
		m_shash = CStringHash(0);
		m_pChz = nullptr;
		return;
	}

	if(m_pChz)
	{
		size_t cB = EWC::CCh(m_pChz) + 1;
		s_pStrtab->FreePChz(m_pChz, cB, m_shash.HvRaw());
		m_pChz = nullptr;
		m_shash = CStringHash(0);
	}

	if(pChNew)
	{
		m_shash = CStringHash(pChNew, cCh);
		m_pChz = s_pStrtab->PChzAlloc(pChNew, cCh, m_shash.HvRaw());
	}
}


// OID methods

OID	OidEnsure(const char * pChz)
{
	if (!EWC_FVERIFY(s_pHashOidStr, "did not initialize oid table"))
		return OID_Nil;

	OID oid;
	oid.m_shash = CStringHash(pChz);
	s_pHashOidStr->FinsEnsureKeyAndValue(oid, CString(pChz));
	return oid;
}

const char * PChzFromOid(OID oid)
{
	if (!EWC_FVERIFY(s_pHashOidStr, "did not initialize oid table"))
		return "<uninit>";

	CString * pStr = s_pHashOidStr->Lookup(oid);
	if (!pStr)
		return "<unknown>";
	return pStr->PChz();
}




void StaticInitStrings(CAlloc * pAlloc)
{
	CString::s_pStrtab = EWC_NEW(pAlloc, CStringTable) CStringTable(pAlloc);
						
	s_pHashOidStr = EWC_NEW(pAlloc, OidTable) OidTable(pAlloc);
}

void StaticShutdownStrings(CAlloc * pAlloc)
{
	pAlloc->EWC_DELETE(s_pHashOidStr);
	s_pHashOidStr = nullptr;

	pAlloc->EWC_DELETE(CString::s_pStrtab);
	CString::s_pStrtab = nullptr;
}

} // namespace EWC