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

#ifndef EWC_STRING_GUARD
#define EWC_STRING_GUARD

#include "EwcTypes.h"

u32 HvFromPBFVN(const void * pV, size_t cB);
u32 HvConcatPBFVN(u32 hv, const void * pV, size_t cB);
u32 HvFromPCozLowercaseFVN(const char * pV, size_t cB);

namespace EWC
{
	class CStringTable;


inline u32		HvFromPCoz(const char * pCoz, size_t cB = 0)
					{
						if (!pCoz)
							return 0;
						if (cB == 0)
							cB = CBCoz(pCoz)-1;

						return HvFromPBFVN(pCoz, cB);
					}

inline u32		HvConcatPCoz(u32 hv, const char * pCoz, size_t cB = 0)
					{
						if (!pCoz)
							return hv;
						if (cB == 0)
							cB = CBCoz(pCoz)-1;

						return HvConcatPBFVN(hv, pCoz, cB);
					}

inline u32		HvFromPCozLowercase(const char * pCoz, size_t cB = 0)
					{
						if (!pCoz)
							return 0;
						if (cB == 0)
							cB = CBCoz(pCoz)-1;

						return HvFromPCozLowercaseFVN(pCoz, cB);
					}
					

// string hash class tracks a (possibly invalid) pointer to the source string
#define EWC_SHASH_DEBUG_POINTER 1

// string hash class stores a full copy of the string for debug purposes
#define EWC_SHASH_DEBUG_COPY	 0

//--------------------------------------------------------------------------
class CStringHash // tag=shash
{
public:
				CStringHash()
				:m_hv(0)
#if EWC_SHASH_DEBUG_POINTER
				,m_pChzDebugSource(nullptr)
#endif
					{
#if EWC_SHASH_DEBUG_COPY
						m_aDebugSourceString[0] = '\0';
#endif
					}

				CStringHash(const char * pChz, size_t cB = 0)
				:m_hv(HvFromPCoz(pChz, cB))
#if EWC_SHASH_DEBUG_POINTER
				,m_pChzDebugSource(pChz)
#endif
					{
#if EWC_SHASH_DEBUG_COPY
						CChCopy(m_aDebugSourceString, pChz, EWC_DIM(m_aDebugSourceString));
#endif
					}
	const char *
				PChzDebug() const
					{
#if EWC_SHASH_DEBUG_POINTER
						return m_pChzDebugSource;
#elif EWC_SHASH_DEBUG_COPY
						return m_aDebugSourceString;
#else
						return "UnknownHash";
#endif
					}

	bool		operator==(const CStringHash& shashOther) const	
					{ return m_hv == shashOther.m_hv; }
	bool		operator!=(const CStringHash& shashOther) const	
					{ return !(*this == shashOther); }
				operator HV() const
					{ return m_hv; }
	HV			HvRaw() const
					{ return m_hv; }
	bool		FIsValid() const
					{ return m_hv != 0; }

protected:
	HV m_hv;

#if EWC_SHASH_DEBUG_POINTER 
	const char* m_pChzDebugSource; // for debug only! may be invalid!
#elif EWC_SHASH_DEBUG_COPY
	char m_aDebugSourceString[256];
#endif
};

// reference-counted heap-allocated string class

class CAsciString // tag=str
{
public:
	static void		StaticInit(CAlloc * pAlloc);
	static void		StaticShutdown(CAlloc * pAlloc);

					CAsciString()
					:m_pChz(nullptr)
					,m_shash(0)
						{ ; }

					CAsciString(const char * pChz)
					:m_pChz(nullptr)
					,m_shash(0)
						{ SetPChz(pChz); }

					CAsciString(const char * pCh, size_t cCh)
					:m_pChz(nullptr)
					,m_shash(0)
						{ SetPCh(pCh, cCh); }

					CAsciString(const CAsciString & strOther)
					:m_pChz(nullptr)
					,m_shash(0)
						{ SetPChz(strOther.m_pChz); }

					~CAsciString()
						{ SetPChz(nullptr); }

	bool			operator==(const CAsciString & strOther) const
						{ return m_shash == strOther.m_shash; }
	bool			operator!=(const CAsciString & strOther) const
						{ return !(*this == strOther); }

	CAsciString &	operator=(const CAsciString & strOther)
						{ return *this = strOther.m_pChz; }
	CAsciString &	operator=(const char * pChz)
						{
							if (m_pChz != pChz)
								SetPChz(pChz);

							return *this;
						}

	CStringHash		Shash() const
						{ return m_shash; }
	HV				Hv() const 
						{ return m_shash.HvRaw(); }
	bool			FIsEmpty() const
						{ return m_pChz == nullptr || *m_pChz == '\0'; }
	const char*		PChz() const
						{ return m_pChz; }

	void			SetPChz(const char * pChzNew);
	void			SetPCh(const char * pChNew, size_t cCh);

	size_t			CB() const
						{ return EWC::CCh(m_pChz); }
	size_t			CCh() const
						{ return EWC::CCh(m_pChz); }

protected:

	const char *		  m_pChz;
	CStringHash			  m_shash;

public:
	static CStringTable * s_pStrtab;
};

// utf8 - wide char strings
class CString // tag=str
{
public:
	static void		StaticInit(CAlloc * pAlloc);
	static void		StaticShutdown(CAlloc * pAlloc);

					CString()
					:m_pCoz(nullptr)
					,m_shash(0)
						{ ; }

					CString(const char * pCoz)
					:m_pCoz(nullptr)
					,m_shash(0)
						{ SetPCoz(pCoz); }

					CString(const char * pCh, size_t cB)
					:m_pCoz(nullptr)
					,m_shash(0)
						{ SetPCo(pCh, cB); }

					CString(const CString & strOther)
					:m_pCoz(nullptr)
					,m_shash(0)
						{ SetPCoz(strOther.m_pCoz); }

					~CString()
						{ SetPCoz(nullptr); }

	bool			operator==(const CString & strOther) const
						{ return m_shash == strOther.m_shash; }
	bool			operator!=(const CString & strOther) const
						{ return !(*this == strOther); }

	CString &		operator=(const CString & strOther)
						{ return *this = strOther.m_pCoz; }
	CString &		operator=(const char * pCoz)
						{
							if (m_pCoz != pCoz)
								SetPCoz(pCoz);

							return *this;
						}

	CStringHash		Shash() const
						{ return m_shash; }
	HV				Hv() const 
						{ return m_shash.HvRaw(); }
	bool			FIsEmpty() const
						{ return m_pCoz == nullptr || *m_pCoz == '\0'; }
	const char*		PCoz() const
						{ return m_pCoz; }

	void			SetPCoz(const char * pCozNew);
	void			SetPCo(const char * pChNew, size_t cCodepoint);

	size_t			CB() const
						{ return EWC::CBCoz(m_pCoz); }
	size_t			CCodepoint() const
						{ return EWC::CCodepoint(m_pCoz); }

protected:

	friend CString	StrFromConcat(const char * , const char * , const char * , const char * );

	const char *		  m_pCoz;
	CStringHash			  m_shash;

public:
	static CStringTable * s_pStrtab;
};

CString StrFromConcat(const char * pCozA, const char * pCozEndA, const char * pCozB, const char * pCozEndB);


inline const void VerifyAscii(const CString & str)
{
	(void) PChzVerifyAscii(str.PCoz());
}



// OID - Is essentially a hashed string, but you can look up the pChz value at runtime

struct OID // tag=oid
{
	bool			operator==(const OID& oidOther) const
						{ return m_shash == oidOther.m_shash; }
	bool			operator!=(const OID& oidOther) const
						{ return !(*this == oidOther.m_shash); }
					operator HV() const
						{ return m_shash; }
					operator CStringHash() const
						{ return m_shash; }

	CStringHash m_shash;
};

extern OID OID_Nil;

void			StaticInitStrings(CAlloc * pAlloc);
void			StaticShutdownStrings(CAlloc * pAlloc);

OID				OidEnsure(const char * pChz);
const char *	PCozFromOid(OID oid);

} // namespace EWC

#endif // EWC_STRING_GUARD