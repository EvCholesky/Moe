#include "Util.h"

#include "EwcTypes.h"
#include <algorithm>



#define TEST_AGAINST_RFC 0

#if TEST_AGAINST_RFC
#include "PunnyRfc.cpp"
#endif

namespace Puny
{
	static const u32 s_nBase = 36;
	static const int s_nTMin = 1;
	static const int s_nTMax = 26;
	static const int s_nSkew = 38;
	static const int s_nDamp = 700;
	static const u32 s_nBiasInitial = 72;
#if TEST_AGAINST_RFC
	static const char s_chDelimiter = '-';
#else
	static const char s_chDelimiter = '_';
#endif
	static const int s_cWrapInitial = 0x80;
	static const u32 s_nU32Max = 0xFFFFFFFF;

	int NAdaptBias(int nDelta, int cPoint, bool fFirstTime)
	{
		if (fFirstTime) nDelta = nDelta / s_nDamp;
		else			nDelta = nDelta / 2;

		nDelta += nDelta / cPoint;
		int nK = 0;

		while (nDelta > ((s_nBase - s_nTMin) * s_nTMax) / 2)
		{
			nDelta = nDelta / (s_nBase - s_nTMin);
			nK += s_nBase;
		}

		return nK + (((s_nBase - s_nTMin + 1) * nDelta) / (nDelta + s_nSkew));
	}

u32 NReadCodepoint(const char ** ppCoz, bool * pFIsValid)
{
	const char * pCoz = *ppCoz;
	u32 ch = (u32)*pCoz;
	u32 ch1, ch2, ch3;
	if ((0xf8 & ch) == 0xf0)		
	{ 
		*pFIsValid = ((pCoz[1] & 0xC0) == 0x80) & ((pCoz[2] & 0xC0) == 0x80) & ((pCoz[3] & 0xC0) == 0x80);
		*ppCoz += 4; 
		ch = (ch & 0x07);		ch1 = u32(pCoz[1]) & 0x3F;			ch2 = u32(pCoz[2]) & 0x3F;		ch3 = u32(pCoz[3]) & 0x3F;
		return (ch << 18) | (ch1 << 12) | (ch2 << 6) | ch3;
	}
	else if ((0xf0 & ch) == 0xe0)
	{ 
		*pFIsValid = ((pCoz[1] & 0xC0) == 0x80) & ((pCoz[2] & 0xC0) == 0x80);
		*ppCoz += 3; 
		ch = (ch & 0x0F);		ch1 = u32(pCoz[1]) & 0x3F;			ch2 = u32(pCoz[2]) & 0x3F;
		return (ch << 12) | (ch1 << 6) | ch2;
	}
	else if ((0xE0 & ch) == 0xC0)
	{ 
		*pFIsValid = (pCoz[1] & 0xC0) == 0x80;
		*ppCoz += 2; 
		ch = (ch & 0x1F);		ch1 = u8(pCoz[1]) & 0x3F;
		return (ch << 6) | ch1;
	}
	else
	{
		*pFIsValid = (pCoz[0] & 0x80) == 0;
		*ppCoz += 1;
		return ch;
	}
}

bool FIsValidUtf8(const char * pCoz)
{
	bool fIsValid;
	while (*pCoz != '\0')
	{
		(void) NReadCodepoint(&pCoz, &fIsValid);
		if (!fIsValid)
			return false;
	}

	return true;
}

bool FTryConvertUtf8ToUcs4(const char * pCozIn, char32_t * pWchzOut, char32_t * pWchOutMax)
{
	// BB - not checking for nullptrs, or zero character destination string

	bool fIsValid;
	const char * pCoz = pCozIn;

	while (*pCoz != '\0' && pWchzOut != pWchOutMax)
	{
		*pWchzOut++ = NReadCodepoint(&pCoz, &fIsValid);
		if (!fIsValid)
			return false;
	}

	if (pWchzOut == pWchOutMax)
	{
		*(pWchOutMax-1) = 0;
		return false;
	}
	*pWchzOut = 0;
	return true;
}

bool FTryConvertUcs4ToUtf8(const u32 * pWchzIn, u8 * pCozOut, u8 * pCozOutMax)
{
	// BB - not checking for nullptrs, or zero character destination string

	for (const u32 * pWchz = pWchzIn; *pWchz != 0; ++pWchz)
	{
		u32 wch = *pWchz;
		auto cBOutLeft = (pCozOutMax - pCozOut);
		if (wch < 0x80)
		{
			if (cBOutLeft < 1) break;
			*pCozOut++ = (u8)wch;
		}
		else if (wch < 0x7FF)
		{
			if (cBOutLeft < 2) break;
			*pCozOut++ = u8((wch >> 6) | 0xC0);
			*pCozOut++ = u8(wch & 0x3F) | 0x80;
		}
		else if (wch < 0xFFFF)
		{
			if (cBOutLeft < 3) break;
			*pCozOut++ = u8(wch >> 12) | 0xE0;
			*pCozOut++ = (u8(wch >> 6) & 0x3F) | 0x80;
			*pCozOut++ = u8(wch & 0x3F) | 0x80;
		}
		else if (wch < 0x10FFFF)
		{
			if (cBOutLeft < 4) break;
			*pCozOut++ = u8((wch >> 18) | 0xF0); 
			*pCozOut++ = (u8(wch >> 12) & 0x3F) | 0x80;
			*pCozOut++ = (u8(wch >> 6) & 0x3F) | 0x80;
			*pCozOut++ = u8(wch & 0x3F) | 0x80;
		}
		else
			return false;

	}

	if (pCozOut == pCozOutMax)
	{
		*(pCozOutMax-1) = '\0';
		return false;
	}
	*pCozOut = '\0';
	return true;
}

enum PUNYRET
{
	PUNYRET_Success,
	PUNYRET_BadInput,			// input is malformed utf8
	PUNYRET_OutputTooLong,
	PUNYRET_Overflow,			// Input needs wider integers to process
};

static inline char ChEncodeDigit(u32 d)
{
  //  0..25 map to ASCII a..z or A..Z,   26..35 map to ASCII 0..9 
  return d + 22 + 75 * (d < 26);
}


PUNYRET PunyretEncode(const char * pCozInput, char * pCozOut, size_t cBMaxOut)
{
	const char * pCoz = pCozInput;
	size_t cBInput = EWC::CBCoz(pCozInput);

	auto aNWork = (int *)alloca(cBInput * sizeof(int));		// UCS4 version of the input string
	auto aNExtSorted = (int*)alloca(cBInput * sizeof(int));	// sorted copy of all extended chars

	// copy and count the basic codepoints
	int * pN = aNWork;
	int * pNExt = aNExtSorted;

	char * pCozDest = pCozOut;
	char * pCozDestMax = &pCozOut[cBMaxOut];

	u32 nCextMin = 0xFFFFFFFF;
	bool fIsValid;
	bool fFoundDelimiter = false;
	while (*pCoz != '\0')
	{
		fFoundDelimiter |= *pCoz == s_chDelimiter;
		u32 nCodepoint = NReadCodepoint(&pCoz, &fIsValid);
		*pN++ = nCodepoint;

		if (nCodepoint > 0x80)
		{
			if (nCextMin > nCodepoint)
				nCextMin = nCodepoint;
			*pNExt++ = nCodepoint;
		}
		else
		{
			*pCozDest++ = (u8)nCodepoint;
			if (pCozDest == pCozDestMax)
			{
				pCozOut[cBMaxOut-1] = '\0';
				return PUNYRET_OutputTooLong;
			}
		}
	}
	int cBasic = pCozDest - pCozOut;
	int cHandled = pCozDest - pCozOut;

	// BB - would like to change this to only add a delimiter when we have extended characters (or another delimiter)
#if TEST_AGAINST_RFC
	if (cBasic != 0)
#else
	if ((pNExt - aNExtSorted) != 0 || fFoundDelimiter)
#endif
//	if (cBasic != 0)
	{
		*pCozDest++ = s_chDelimiter;
	}

	if (pNExt == aNExtSorted)
	{
		*pCozDest++ = '\0';
		return PUNYRET_Success;
	}

	std::sort(aNExtSorted, pNExt);

	int nDelta = 0;
	int cWrap = s_cWrapInitial;
	int nBias = s_nBiasInitial;

	int * pNWorkMax = pN;
	int cCodepoint = pNWorkMax - aNWork;

	int iSorted = 0;
	while (cHandled < cCodepoint)
	{
		int nM = aNExtSorted[iSorted++];
		while (aNExtSorted[iSorted] == nM)
			++iSorted;
		nDelta += (nM - cWrap) * (cHandled + 1);

		cWrap = nM;
		for (int * pN = aNWork; pN != pNWorkMax; ++pN)
		{
			int nC = *pN;
			if (nC < cWrap)
			{
				++nDelta;
				if(nDelta == 0)
					return PUNYRET_Overflow;
			}
			else if (nC == cWrap)
			{
				int nQ = nDelta;
				for (int nK = s_nBase; 1; nK += s_nBase)
				{
					int t = EWC::ewcMax(EWC::ewcMin(nK - nBias, s_nTMax), s_nTMin);
					if (nQ < t)
						break;

					*pCozDest++ = ChEncodeDigit(t + (nQ - t) % (s_nBase - t));
					nQ = (nQ - t) / (s_nBase - t);
				}
				
				*pCozDest++ = ChEncodeDigit(nQ);
		        nBias = NAdaptBias(nDelta, cHandled + 1, cHandled == cBasic);
				nDelta = 0;
				++cHandled;
			}
		}
		++nDelta;
		++cWrap;
	}

	*pCozDest = '\0';
	return PUNYRET_Success;
}



// NDecodeDigit(cp) returns the numeric value of a basic code point (for use in representing integers) 
//  in the range 0 to base-1, or base if cp does not represent a value.

static u32 NDecodeDigit(u32 codepoint)
{
	if (codepoint - 48 < 10)	return codepoint - 22;
	if (codepoint - 65 < 26)	return codepoint - 65;
	if (codepoint - 97 < 26)	return codepoint - 97;
	return s_nBase;
}

PUNYRET PunyretDecode(const char * pCozInput, char * pCozOut, size_t cBOutMax)
{
	const char * pCozDelimiter = nullptr;
	const char * pCoz = pCozInput;

	 // find the last delimiter
	while (*pCoz != '\0')
	{
		if (*pCoz == s_chDelimiter)
			pCozDelimiter = pCoz;
		++pCoz;
	}
	const char * pCozInputMax = pCoz;

	size_t cBInput = (pCoz - pCozInput) + 1;
	u32 * aNOutput = (u32 *)alloca(sizeof(u32) * cBInput);
	u32 * pNOutput = aNOutput;
	//if (cBOutMax <= cBInput)
	//	return PUNYRET_OutputTooLong;

	pCoz = pCozInput;

	bool fAllBasic = pCozDelimiter == nullptr;
	if (fAllBasic)
	{
#if !TEST_AGAINST_RFC
	// BB - would like to change this to only add a delimiter when we have extended characters (or another delimiter)
		pCozDelimiter = pCozInputMax;
#else
		pCozDelimiter = pCozInput;
#endif
	}

	while (pCoz != pCozDelimiter)
	{
		unsigned char ch = *pCoz;
		if (ch >= 0x80)
			return PUNYRET_BadInput;
		*pNOutput++ = *pCoz++;
	}

	if (!fAllBasic)	// skip the delimiter
		++pCoz;

	u32 cWrap = s_cWrapInitial; // aka 'n'
	u32 iPrev;
	u32 iN = 0;
	u32 nBias = s_nBiasInitial;
	u32 nWeight;
	u32 nK;
	u32 t;

	size_t cNOut = pNOutput - aNOutput;
	for ( ; pCoz != pCozInputMax; ++cNOut)
	{
	    // Decode a generalized variable-length integer into delta, which gets added to i.  The overflow checking is 
		//  easier if we increase i as we go, then subtract off its starting value at the end to obtain delta.
		iPrev = iN;
		nWeight = 1;
		for (nK = s_nBase; ; nK += s_nBase)
		{
			if (pCoz == pCozInputMax)
				return PUNYRET_BadInput;
			u32 nDigit = NDecodeDigit(*pCoz++);

			if (nDigit >= s_nBase)
				return PUNYRET_BadInput;
			if (nDigit > (s_nU32Max  -1) / nWeight) 
				return PUNYRET_Overflow;
			iN += nDigit * nWeight;

			if (nK <= nBias)				t = s_nTMin;
			else if (nK >= nBias + s_nTMax) t = s_nTMax;
			else							t = nK - nBias;

			if (nDigit < t)
				break;

			if (nWeight > s_nU32Max  / (s_nBase - t)) 
				return PUNYRET_Overflow;
			nWeight *= (s_nBase - t);
		}

	    nBias = NAdaptBias(iN - iPrev, cNOut + 1, iPrev == 0);

	    // iN was supposed to wrap around from out+1 to 0, incrementing n each time, so we'll fix that now:

		u32 dWrap = iN / (cNOut + 1);
	    if (dWrap > s_nU32Max  - cWrap) 
			return PUNYRET_Overflow;
	    cWrap += dWrap;
	    iN %= (cNOut + 1);

	    // Insert n at position i of the output:

	    if (cNOut >= cBOutMax) 
			return PUNYRET_OutputTooLong;
	    memmove(aNOutput + iN + 1, aNOutput + iN, (cNOut - iN) * sizeof(aNOutput[0]));
		aNOutput[iN++] = cWrap;
	}

	aNOutput[cNOut] = 0;
	if (FTryConvertUcs4ToUtf8(aNOutput, (u8 *)pCozOut, (u8 *)&pCozOut[cBOutMax]))
		return PUNYRET_Success;
	return PUNYRET_BadInput;
}

} // namespace Puny

EWC::CString StrPunyEncode(const char * pCoz)
{
	// BB - there's a lot of stupid copying and counting going on here...

	// I don't have a great way to estimate the punycoded string length from the utf8 length - let's just say
	//  it won't be a lot longer than the original
	size_t cBMaxPuny = EWC::CBCoz(pCoz) * sizeof(char) + 512;
	char * pChzPuny = (char *)alloca(cBMaxPuny);

	auto punyret = Puny::PunyretEncode(pCoz, pChzPuny, cBMaxPuny);
	EWC_ASSERT(punyret == Puny::PUNYRET_Success, "punycoding failed");
	return EWC::CString(pChzPuny);
}

EWC::CString StrPunyDecode(const char * pChz)
{
	// BB - there's a lot of stupid copying and counting going on here...

	// I don't have a great way to estimate the utf8 string length from the punycoded length - let's just say
	//  it won't be a lot longer than the original
	size_t cBMaxPuny = EWC::CBCoz(pChz) * sizeof(char) + 512;
	char * pChzPuny = (char *)alloca(cBMaxPuny);

	auto punyret = Puny::PunyretDecode(pChz, pChzPuny, cBMaxPuny);
	EWC_ASSERT(punyret == Puny::PUNYRET_Success, "punycode decoding failed");
	return EWC::CString(pChzPuny);
}

int NCmpWchz(const char32_t * pWchzA, const char32_t * pWchzB)
{
	if ((pWchzA == nullptr) | (pWchzB == nullptr))
	{
		if (pWchzA == pWchzB)
			return 0;
		
		return (pWchzA == nullptr) ? -1 : 1;
	}

	while ((*pWchzA != '\0') | (*pWchzB != '\0'))
	{
		auto chA = *pWchzA;
		auto chB = *pWchzB;
		if (chA < chB)
			return -1;
		else if (chA > chB)
			return 1;

		++pWchzA;
		++pWchzB;
	}

	return 0;
}

bool FAreWchzEqual(const char32_t * pWchzA, const char32_t * pWchzB)
{
	return NCmpWchz(pWchzA, pWchzB) == 0;
}

size_t CNFromWchz(const u32 * pWchz)
{
	// bytes needed for this string (including the null terminator)
	if (!pWchz)
		return 0;

	auto pWchzIt = pWchz;
	while (*pWchzIt != '\0')
		++pWchzIt;

	++pWchzIt;
	return pWchzIt - pWchz;
}

void TestUtf8()
{
	char aCh[256];

	char * pCoz;
	char * pCozExpected;
	EWC::SStringBuffer strbuf;

	pCoz = u8"いろはに";
	pCozExpected = u8"いろは";
	EWC_ASSERT(EWC::CBCoz(pCoz) == 13, "bad byte count");
	EWC_ASSERT(EWC::CCodepoint(pCoz) == 4, "bad codepoint count");

	strbuf = EWC::SStringBuffer(aCh, 13);
	AppendCoz(&strbuf, pCoz);
	EWC_ASSERT(EWC::FAreCozEqual(strbuf.m_pCozBegin, pCoz), "bad utf8 copy");

	strbuf = EWC::SStringBuffer(aCh, 11);
	AppendCoz(&strbuf, pCoz);
	EWC_ASSERT(EWC::FAreCozEqual(strbuf.m_pCozBegin, pCozExpected), "bad utf8 copy");

	strbuf = EWC::SStringBuffer(aCh, 11);
	FormatCoz(&strbuf, "%s", pCoz);
	EWC_ASSERT(EWC::FAreCozEqual(strbuf.m_pCozBegin, pCozExpected), "bad utf8 copy");

	pCoz = u8"ößöß";
	pCozExpected = u8"ößö";
	EWC_ASSERT(EWC::CBCoz(pCoz) == 9, "bad byte count");
	EWC_ASSERT(EWC::CCodepoint(pCoz) == 4, "bad codepoint count");

	strbuf = EWC::SStringBuffer(aCh, 7);
	AppendCoz(&strbuf, pCoz);
	EWC_ASSERT(EWC::FAreCozEqual(strbuf.m_pCozBegin, pCozExpected), "bad utf8 copy");
}

void TestUnicode()
{
	TestUtf8();

	struct STestStrings // testr
	{
		const char *		m_pCozUtf8;
		const char32_t *	m_pWchzTest;
	};

	#define MAKE_TESTR(str) { u8##str, U##str }
	STestStrings s_aTestr[] = 
	{
		MAKE_TESTR("ascii"),
		MAKE_TESTR("mixedいろはにほ"),
		MAKE_TESTR("いろはにほmixed"),
		MAKE_TESTR("いmろiはxにeほd"),
		MAKE_TESTR("_delimiter"),
		MAKE_TESTR("delimiter_"),
		MAKE_TESTR("d_e_l_i_m_i_t_e_r"),
		MAKE_TESTR("Quizdeltagerne spiste jordbær med fløde, mens cirkusklovnen Wolther spillede på xylofon."), // danis
		MAKE_TESTR("Le cœur déçu mais l'âme plutôt naïve, Louÿs rêva de crapaüter en canoë au delà des îles, près du mälström où brûlent les novæ."), // French
		MAKE_TESTR("D'fhuascail Íosa, Úrmhac na hÓighe Beannaithe, pór Éava agus Ádhaimh"),	// Gaelic
		MAKE_TESTR("Falsches Üben von Xylophonmusik quält jeden größeren Zwerg"),	// German
		MAKE_TESTR("Γαζέες καὶ μυρτιὲς δὲν θὰ βρῶ πιὰ στὸ χρυσαφὶ ξέφωτο"),		// Greek
		MAKE_TESTR("Kæmi ný öxi hér ykist þjófum nú bæði víl og ádrepa"),		// Icelandic
		MAKE_TESTR("いろはにほへとちりぬるをわかよたれそつねならむうゐのおくやまけふこえてあさきゆめみしゑひもせす"),	// Japanese
		MAKE_TESTR("? דג סקרן שט בים מאוכזב ולפתע מצא לו חברה איך הקליטה"), // hebrew
		MAKE_TESTR("Árvíztűrő tükörfúrógép"), // Hungarian
		MAKE_TESTR("В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"),		// Russian
		MAKE_TESTR("El pingüino Wenceslao hizo kilómetros bajo exhaustiva lluvia y frío, añoraba a su querido cachorro."),		// Spanish
		MAKE_TESTR("Pijamalı hasta, yağız şoföre çabucak güvendi."),		// Turkish
		MAKE_TESTR("😁😄😔✂✋🚀⏰⏳"),
		MAKE_TESTR("😁MiXeD-CaSe⏳"),
	};
	#undef MAKE_TESTR

	u32 aNScratch[1024];
	u8 aCozScratch[1024];
	char aCozPuny[1024];

	for (int ipCoz = 0; ipCoz != EWC_DIM(s_aTestr); ++ipCoz)
	{
		EWC_ASSERT(Puny::FIsValidUtf8(s_aTestr[ipCoz].m_pCozUtf8), "bad input string");

		EWC_ASSERT(Puny::FTryConvertUtf8ToUcs4(s_aTestr[ipCoz].m_pCozUtf8, (char32_t *)aNScratch, (char32_t*)EWC_PMAC(aNScratch)), "Failed converting to ucs4");
		EWC_ASSERT(FAreWchzEqual(s_aTestr[ipCoz].m_pWchzTest, (const char32_t *)aNScratch), "conversion error");

		EWC_ASSERT(Puny::FTryConvertUcs4ToUtf8(aNScratch, aCozScratch, EWC_PMAC(aCozScratch)), "Failed converting to utf8");

		EWC_ASSERT(EWC::FAreCozEqual((char*)aCozScratch, s_aTestr[ipCoz].m_pCozUtf8), "conversion error");

#if TEST_AGAINST_RFC
		char aChzPunyRfc[1024];
		u32 cNScratch = CNFromWchz(aNScratch);
		u32 cChPunyRfc = EWC_DIM(aChzPunyRfc);
		punycode_status punys = punycode_encode(cNScratch, aNScratch, nullptr, &cChPunyRfc, aChzPunyRfc);
		EWC_ASSERT(punys == punycode_success, "bad rfc puyncode encoding");

		EWC::ZeroAB(aNScratch, EWC_DIM(aNScratch));

		cNScratch = EWC_DIM(aNScratch);
		punys = punycode_decode(cChPunyRfc, aChzPunyRfc, &cNScratch, aNScratch, nullptr);
		EWC_ASSERT(punys == punycode_success, "bad rfc puyncode encoding");
		EWC_ASSERT(FAreWchzEqual(s_aTestr[ipCoz].m_pWchzTest, (const char32_t *)aNScratch), "rtf punycode error");
#endif

		auto punyret = Puny::PunyretEncode(s_aTestr[ipCoz].m_pCozUtf8, (char *)aCozScratch, EWC_DIM(aCozScratch));
		EWC_ASSERT(punyret == Puny::PUNYRET_Success, "bad punycode encode");

#if TEST_AGAINST_RFC
		EWC_ASSERT(EWC::FAreCozEqual((char*)aCozScratch, aChzPunyRfc), "doesen't match the punycode RFC implementation.");
#endif

		punyret = Puny::PunyretDecode((char *)aCozScratch, aCozPuny, EWC_DIM(aCozPuny));
		EWC_ASSERT(punyret == Puny::PUNYRET_Success, "bad punycode decode");
		EWC_ASSERT(EWC::FAreCozEqual((char*)aCozPuny, s_aTestr[ipCoz].m_pCozUtf8), "punycode decode fail");

	}
}