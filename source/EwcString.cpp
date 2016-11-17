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

// simple hash function with meh avalanching, works for now. replace with MUM hash or xxHash
u32 HvFromPBFVN(const void * pV, size_t cB)
{
	auto pB = (u8*)pV;
    u32 hv = 2166136261;

	for (size_t iB=0; iB < cB; ++iB)
    {
        hv = (hv * 16777619) ^ pB[iB];
    }

    return hv;
}

u32 HvConcatPBFVN(u32 hv, const void * pV, size_t cB)
{
	auto pB = (u8*)pV;
	for (size_t iB=0; iB < cB; ++iB)
    {
        hv = (hv * 16777619) ^ pB[iB];
    }

    return hv;
}


u32 HvFromPCozLowercaseFVN(const char * pV, size_t cB)
{
	auto pB = (u8*)pV;
    u32 hv = 2166136261;

	for (size_t iB=0; iB < cB; ++iB)
    {
        hv = (hv * 16777619) ^ (u8)tolower(pB[iB]);
    }

    return hv;
}

namespace EWC
{

// string table for ref-counted string class (only used by CString)
class CStringTable // tag=strtab
{
public:
				CStringTable(CAlloc * pAlloc)
				:m_pAlloc(pAlloc)
				,m_mpHvEntry(pAlloc, EWC::BK_StringTable, 128)
					{ ; }

	const char * PCozAlloc(const char * pCoz, size_t cB, HV hv)
					{
						Entry * pEntry = nullptr;
						if (m_mpHvEntry.FinsEnsureKey(hv, &pEntry) == FINS_Inserted)
						{
							pEntry->m_cRef = 1;
							pEntry->m_pCoz = (char*)m_pAlloc->EWC_ALLOC(sizeof(char) * cB, EWC_ALIGN_OF(char));
							(void) CBCopyCoz(pCoz, pEntry->m_pCoz, cB);
						}
						else
						{
							++pEntry->m_cRef;
							EWC_ASSERT(FAreCozEqual(pEntry->m_pCoz, pCoz, cB-1), "bad table lookup in CStringTable");
						}

						return pEntry->m_pCoz;
					}

	void		FreePCoz(const char * pCoz, HV hv)
					{
						Entry * pEntry = m_mpHvEntry.Lookup(hv);
						if (!pEntry)
						{
							EWC_ASSERT(false, "failed lookup in CStringTable::FreePCoz");
							return;
						}
						--pEntry->m_cRef;
						if (pEntry->m_cRef <= 0)
						{
							m_pAlloc->EWC_FREE(pEntry->m_pCoz);
							m_mpHvEntry.Remove(hv);
						}
					}

	struct Entry
	{
		char *	m_pCoz;
		u16		m_cRef;
	};

	CAlloc * 			m_pAlloc;
	CHash<HV, Entry>	m_mpHvEntry;
};

CStringTable * CAsciString::s_pStrtab = nullptr;
CStringTable * CString::s_pStrtab = nullptr;

typedef CHash<OID, CString> OidTable;
OidTable * s_pHashOidStr = nullptr;

OID OID_Nil;


// CAsciString Methods:
void CAsciString::StaticInit(CAlloc * pAlloc)
{
	s_pStrtab = EWC_NEW(pAlloc, CStringTable) CStringTable(pAlloc);
}

void CAsciString::StaticShutdown(CAlloc * pAlloc)
{
	pAlloc->EWC_DELETE(s_pStrtab);
	s_pStrtab = nullptr;
}

void CAsciString::SetPChz(const char * pChzNew)
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
		s_pStrtab->FreePCoz(m_pChz, m_shash.HvRaw());
		m_pChz = nullptr;
		m_shash = CStringHash(0);
	}

	if(pChzNew)
	{
		m_shash = CStringHash(pChzNew);
		m_pChz = s_pStrtab->PCozAlloc(pChzNew, EWC::CCh(pChzNew), m_shash.HvRaw());
	}
}

void CAsciString::SetPCh(const char * pChNew, size_t cCh)
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
		s_pStrtab->FreePCoz(m_pChz, m_shash.HvRaw());
		m_pChz = nullptr;
		m_shash = CStringHash(0);
	}

	if(pChNew)
	{
		m_shash = CStringHash(pChNew, cCh);
		m_pChz = s_pStrtab->PCozAlloc(pChNew, cCh, m_shash.HvRaw());
	}
}



// CWstring Methods:
void CString::StaticInit(CAlloc * pAlloc)
{
	s_pStrtab = EWC_NEW(pAlloc, CStringTable) CStringTable(pAlloc);
}

void CString::StaticShutdown(CAlloc * pAlloc)
{
	pAlloc->EWC_DELETE(s_pStrtab);
	s_pStrtab = nullptr;
}

void CString::SetPCoz(const char * pCozNew)
{
	EWC_ASSERT(s_pStrtab, "String table has not been allocated");
	if (!s_pStrtab)
	{
		m_shash = CStringHash(0);
		m_pCoz = nullptr;
		return;
	}

	if(m_pCoz)
	{
		s_pStrtab->FreePCoz(m_pCoz, m_shash.HvRaw());
		m_pCoz = nullptr;
		m_shash = CStringHash(0);
	}

	if(pCozNew)
	{
		m_shash = CStringHash(pCozNew);
		m_pCoz = s_pStrtab->PCozAlloc(pCozNew, EWC::CBCoz(pCozNew), m_shash.HvRaw());
	}
}

CString StrFromConcat(const char * pCozA, const char * pCozEndA, const char * pCozB, const char * pCozEndB)
{
	size_t cBA = (pCozEndA - pCozA);
	size_t cBB = (pCozEndB - pCozB);
	size_t cBDest = cBA+cBB+1;

	// BB - we could try to do something clever here and construct the string/hash inside the table, I started
	//  on it but it seemed like a bigger mess than it was worth
	char * pCozWork = (char *)CString::s_pStrtab->m_pAlloc->EWC_ALLOC(cBDest, 1);
	(void)CBCopyCoz(pCozA, pCozWork, cBDest);
	(void)CBCopyCoz(pCozB, &pCozWork[cBA], cBDest-cBA);

	CString str(pCozWork); //, cBDest);

	CString::s_pStrtab->m_pAlloc->EWC_DELETE(pCozWork);
	return str;
}

void CString::SetPCo(const char * pCoNew, size_t cCodepoint)
{
	EWC_ASSERT(s_pStrtab, "String table has not been allocated");
	if (!s_pStrtab)
	{
		m_shash = CStringHash(0);
		m_pCoz = nullptr;
		return;
	}

	if(m_pCoz)
	{
		s_pStrtab->FreePCoz(m_pCoz, m_shash.HvRaw());
		m_pCoz = nullptr;
		m_shash = CStringHash(0);
	}

	if(pCoNew)
	{
		size_t cB = CBFromCoz(pCoNew, cCodepoint);
		m_shash = CStringHash(pCoNew, cB-1);
		m_pCoz = s_pStrtab->PCozAlloc(pCoNew, cB, m_shash.HvRaw());
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

const char * PCozFromOid(OID oid)
{
	if (!EWC_FVERIFY(s_pHashOidStr, "did not initialize oid table"))
		return "<uninit>";

	CString * pStr = s_pHashOidStr->Lookup(oid);
	if (!pStr)
		return "<unknown>";
	return pStr->PCoz();
}




void StaticInitStrings(CAlloc * pAlloc)
{
	CString::s_pStrtab = EWC_NEW(pAlloc, CStringTable) CStringTable(pAlloc);
						
	s_pHashOidStr = EWC_NEW(pAlloc, OidTable) OidTable(pAlloc, EWC::BK_StringTable);
}

void StaticShutdownStrings(CAlloc * pAlloc)
{
	pAlloc->EWC_DELETE(s_pHashOidStr);
	s_pHashOidStr = nullptr;

	pAlloc->EWC_DELETE(CString::s_pStrtab);
	CString::s_pStrtab = nullptr;
}

} // namespace EWC