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

#ifndef EWC_TYPES_GUARD
#define EWC_TYPES_GUARD

#include <xmmintrin.h>
#include <ctype.h>
#include <emmintrin.h>
#include <new>
#include <stdint.h>

#if _WIN64
#define EWC_X64 1
#elif EWC_OSX_X64
#define EWC_X64 1
#else
#define EWC_X64 0
#endif

#if EWC_X64
#define STBM_POINTER_SIZE 64
#define STBM_UINT32 uint32_t
#define STBM_UINTPTR uintptr_t
#endif

//#define STBM_DEBUGCHECK
#include "stb_malloc.h"



typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef wchar_t WChar; // tag = wch

typedef float F32;
typedef double F64;

typedef u32 HV; // Hash Value

enum TFN
{
	TFN_False,
	TFN_True,
	TFN_Nil = -1,
};

// BB - Messy dependency, defined in EwcString.cpp
u32 HvFromPBFVN(const void * pV, size_t cB);

namespace EWC
{

typedef __m128 Simd;
typedef __m128i SimdI;
typedef const Simd & SimdArg;

struct Simd3 { Simd x, y, z; };
struct Simd4 { Simd x, y, z, w; };

// standard macros

#if defined( __GNUC__ )
	#define		EWC_FORCE_INLINE	inline __attribute__((always_inline))
	#define		EWC_ALIGN(CB)		__attribute__((aligned(CB)))
	#define 	EWC_ALIGN_OF(T) 	__alignof__(T)
	#define		EWC_IS_ENUM(T)		__is_enum(T)
	#define		EWC_DEBUG_BREAK()	asm ("int $3")
#elif defined( _MSC_VER )
	#define		EWC_FORCE_INLINE	__forceinline
	#define		EWC_ALIGN(CB)		__declspec(align(CB))
	#define 	EWC_ALIGN_OF(T) 	__alignof(T)
	#define		EWC_IS_ENUM(T)		__is_enum(T)
	#define		EWC_DEBUG_BREAK()	__debugbreak()
#elif defined( __clang__)
	#define		EWC_FORCE_INLINE	inline __attribute__((always_inline))
	#define		EWC_ALIGN(CB)		__attribute__((aligned(CB)))
	#define 	EWC_ALIGN_OF(T) 	__alignof__(T)
	#define		EWC_IS_ENUM(T)		__is_enum(T)
	#define		EWC_DEBUG_BREAK()   asm("int $3")
#endif

#define EWC_OFFSET_OF(STRUCT_NAME, VAR_NAME)	offsetof(STRUCT_NAME, VAR_NAME)
#define EWC_DIM(arr) (sizeof(arr) / sizeof(*arr))
#define EWC_PMAC(arr) &arr[EWC_DIM(arr)]

#define EWC_RELEASE 0
#if EWC_RELEASE
#define EWC_TWEAK static const
#else
#define EWC_TWEAK static
#endif

#define EWC_ENUM_UTILS(ENUM_NAME)  \
	inline bool FIsValid(ENUM_NAME e) { return (e >= ENUM_NAME##_Min) & (e < ENUM_NAME##_Max); } \
	inline ENUM_NAME VerifyValidElement(ENUM_NAME e) { if (EWC_FVERIFY(FIsValid(e), "array access with bad " #ENUM_NAME)) return e; return (ENUM_NAME)0; }

#define EWC_MAX_MIN_NIL(ENUM_NAME) ENUM_NAME##_Max, ENUM_NAME##_Min = 0, ENUM_NAME##_Nil = -1 \
	}; EWC_ENUM_UTILS(ENUM_NAME) \
	enum ENUM_NAME##_Stub {

void AssertHandler( const char* pChzFile, u32 line, const char* pChzCondition, const char* pChzMessage = 0, ...);

#define EWC_VERIFY( PREDICATE, ... ) \
	do { if (!(PREDICATE)) { \
		EWC::AssertHandler(__FILE__, __LINE__, #PREDICATE, __VA_ARGS__); \
		EWC_DEBUG_BREAK(); \
	} } while (0)

#define EWC_ASSERT( PREDICATE, ... ) EWC_VERIFY(PREDICATE, __VA_ARGS__);

#define EWC_CASSERT( PREDICATE, ERROR ) static_assert(PREDICATE, "CASSERT: " ERROR)

#if defined( _MSC_VER )
#define EWC_FVERIFY( PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	(\
	  EWC::AssertHandler( __FILE__, __LINE__, #PREDICATE, __VA_ARGS__ ),\
	  EWC_DEBUG_BREAK(), \
	  false\
	)\
  )\
)
#else
// use a goofy expression statement to play nice with clang
#define EWC_FVERIFY( PREDICATE, ... )\
(\
  ( ( PREDICATE ) ? \
	true :\
	({\
	  EWC::AssertHandler( __FILE__, __LINE__, #PREDICATE, __VA_ARGS__ );\
	  EWC_DEBUG_BREAK(); \
	  false;\
	})\
  )\
)
#endif

#define EWC_FASSERT( PREDICATE, ... ) EWC_FVERIFY(PREDICATE, __VA_ARGS__)

#define EWC_TRACE(PREDICATE, ...) \
do { if (PREDICATE) \
	{ printf(__VA_ARGS__); } \
} while (0)

void DoNothing();
static_assert(sizeof(s64) == 8, "wha");
static_assert(sizeof(s32) == 4, "wha");

// Memory functions

void	FillAB(u8 b, void * pDest, size_t cB);
void	ZeroAB(void * pDest, size_t cB);
void	CopyAB(const void * pSource, void * pDest, size_t cB);
bool	FAreSameAB(const void * aB0, void * aB1, size_t cB);

inline s32 S32Coerce(s64 n)		{ s32 nRet = (s32)n;	EWC_ASSERT((s64)nRet == n, "S32Coerce failure"); return nRet; }
inline s16 S16Coerce(s64 n)		{ s16 nRet = (s16)n;	EWC_ASSERT((s64)nRet == n, "S16Coerce failure"); return nRet; }
inline s8 S8Coerce(s64 n)		{ s8 nRet = (s8)n;		EWC_ASSERT((s64)nRet == n, "S8Coerce failure");  return nRet; }
inline u32 U32Coerce(u64 n)		{ u32 nRet = (u32)n;	EWC_ASSERT((u64)nRet == n, "u32Coerce failure"); return nRet; }
inline u16 U16Coerce(u64 n)		{ u16 nRet = (u16)n;	EWC_ASSERT((u64)nRet == n, "u16Coerce failure"); return nRet; }
inline u8 U8Coerce(u64 n)		{ u8 nRet = (u8)n;		EWC_ASSERT((u64)nRet == n, "u8Coerce failure");  return nRet; }
 
// type traits
template <typename T> struct SStripConst				{ typedef T Type;	enum { F_STRIPPED= false }; };
template <typename T> struct SStripConst<const T>		{ typedef T Type;	enum { F_STRIPPED= true }; };

template <typename T> struct SStripReference			{ typedef T Type; 	enum { F_STRIPPED= false }; };
template <typename T> struct SStripReference<T&>		{ typedef T Type; 	enum { F_STRIPPED= true }; };

template <typename T> struct SStripPointer				{ typedef T Type; 	enum { F_STRIPPED= false }; };
template <typename T> struct SStripPointer<T*>			{ typedef T Type; 	enum { F_STRIPPED= true }; };

template <typename T> struct SIsReference				{ enum { V = false }; };
template <typename T> struct SIsReference<T&>			{ enum { V = true }; };

template <typename T> struct SIsPointer					{ enum { V = false }; };
template <typename T> struct SIsPointer<T&>				{ enum { V = true }; };

template <typename T> struct SIsSignedInt				{ enum { V = false }; };
template <> struct SIsSignedInt<s8>						{ enum { V = true }; };
template <> struct SIsSignedInt<s16>					{ enum { V = true }; };
template <> struct SIsSignedInt<s32>					{ enum { V = true }; };
template <> struct SIsSignedInt<s64>					{ enum { V = true }; };

// templates to allocate a c-style block with proper alignment that doesn't run constructors

struct EWC_ALIGN(16)	SAlignedQword  					{ u8 m_a[16]; };
template <int ALIGNMENT> struct SAlignedBlock			{ SAlignedBlock() {EWC_CASSERT(sizeof(*this), "unknown alignment in SAlignedBlock");}  };
template <> struct SAlignedBlock<0>						{ u8 m_b; };
template <> struct SAlignedBlock<1>						{ u8 m_b1; };
template <> struct SAlignedBlock<2>						{ u16 m_b2; };
template <> struct SAlignedBlock<4>						{ u32 m_b4; };
template <> struct SAlignedBlock<8>						{ u64 m_b8; };
template <> struct SAlignedBlock<16>					{ SAlignedQword m_b16; };

template <int CB, int ALIGNMENT>
struct SAlignedBytes // tag=alby
{
	void *	A()		{ return m_aB; }
	size_t	CBMax()	{ return CB; }

	SAlignedBlock<ALIGNMENT> m_aB[(CB + (ALIGNMENT-1)) / ALIGNMENT];
};

//Alexanderscu's compile time conversion test - Modern C++ Design pg 34-36
template <typename SOURCE_TYPE, typename TARGET_TYPE>
struct SCanConvert
{
	typedef u8 FalseType;
	struct TrueType { FalseType a[2]; };
	static TrueType		TestFunction(TARGET_TYPE);
	static FalseType	TestFunction(...);
	static SOURCE_TYPE 	MakeSource();

	enum { V = sizeof(TestFunction(MakeSource())) == sizeof(TrueType) };
};

template <typename T> struct SIsUnsignedInt						{ enum { V = false }; };
template <> struct SIsUnsignedInt<u8>							{ enum { V = true }; };
template <> struct SIsUnsignedInt<u16>							{ enum { V = true }; };
template <> struct SIsUnsignedInt<u32>							{ enum { V = true }; };
template <> struct SIsUnsignedInt<u64>							{ enum { V = true }; };
 
template <typename T> struct SIsInt								{ enum { V  = SIsSignedInt<T>::V  || SIsUnsignedInt<T>::V  }; };

template <typename T> struct SIsFloat							{ enum { V = false }; };
template <> struct SIsFloat<F32>								{ enum { V = true }; };
template <> struct SIsFloat<F64>								{ enum { V = true }; };

template <typename T> struct SIsBool							{ enum { V = false }; };
template <> struct SIsBool<bool>								{ enum { V = true }; };

template <typename T> struct SIsVoid							{ enum { V = false }; };
template <> struct SIsVoid<void>								{ enum { V = true }; };

template <typename T> struct SVoidSafeSizeof					{ enum { V = sizeof(T) }; };
template <> struct SVoidSafeSizeof<void>						{ enum { V = 0 }; };

// NOTE: can't just check static_cast<T>(-1) because it doesn't work for custom types
template <typename T, bool IS_ENUM> struct SIsSignedSelector	{ enum { V = SIsFloat<T>::V || SIsSignedInt<T>::V }; };
template <typename T> struct SIsSignedSelector<T, true>			{ enum { V = static_cast<T>(-1) < 0 }; };
template <typename T> struct SIsSigned							{ enum { V = SIsSignedSelector<T, EWC_IS_ENUM(T)>::V }; };

template <typename T> struct SIsFundamentalType					{ enum { V  = 
																		SIsPointer<T>::V  || 
																		SIsReference<T>::V  || 
																		SIsInt<T>::V  || 
																		SIsFloat<T>::V  || 
																		SIsBool<T>::V  || 
																		EWC_IS_ENUM(T) 
																	}; 
															};
template <typename T>
struct SArrayTraits
{
	typedef T Element;
	enum { C_ELEMENTS = -1 };
	enum { F_IS_ARRAY = false };
};

template <typename A, int C>
struct SArrayTraits<A[C]>
{
	typedef A Element;
	enum { C_ELEMENTS = C };
	enum { F_IS_ARRAY = true };
};

template <typename T> struct SHasTrivialConstructor		{ enum { V  = SIsFundamentalType<T>::V  }; };
template <typename T> struct SHasTrivialCopy			{ enum { V  = SIsFundamentalType<T>::V  }; };
template <typename T> struct SHasTrivialDestructor		{ enum { V  = SIsFundamentalType<T>::V  }; };

template <typename T, bool TRIVIAL_CONSTRUCT>
struct SConstructSelector
{
	static void Construct(T * p)
	{
		EWC_ASSERT( ((uintptr_t)p & (EWC_ALIGN_OF(T)-1)) == 0, "trying to construct missaligned object" );
		new (p) T;
	}

	static void ConstructN(T * p, size_t c)
	{
		EWC_ASSERT( ((uintptr_t)p & (EWC_ALIGN_OF(T)-1)) == 0, "trying to construct missaligned object" );
		for (size_t i = 0; i < c; ++i)
			new (p + i) T;
	}
};

template <typename T>
struct SConstructSelector<T, true> // trivial constructor
{
	static void Construct(T * p)					{ }
	static void ConstructN(T * p, size_t c)			{ }
};

template <typename T, bool TRIVIAL_COPY>
struct SCopySelector
{
	static void CopyConstruct(T * p, const T & orig)
	{
		EWC_ASSERT( ((uintptr_t)p & (EWC_ALIGN_OF(T)-1)) == 0, "trying to copy construct missaligned object" );
		new (p) T(orig);
	}

	static void CopyConstructN(T * p, size_t c, const T & orig)
	{
		EWC_ASSERT( ((uintptr_t)p & (EWC_ALIGN_OF(T)-1)) == 0, "trying to copy construct missaligned object" );
		for (size_t i = 0; i < c; ++i)
			new (p + i) T(orig);
	}

	static void CopyConstructArray(T * pTDst, size_t cT, const T * pTSrc)
	{
		EWC_ASSERT( ((uintptr_t)pTDst & (EWC_ALIGN_OF(T)-1)) == 0, "trying to copy construct missaligned object" );
		auto pTDstMax = pTDst + cT;
		for (auto pTDstIt = pTDst; pTDstIt != pTDstMax; ++pTDstIt, ++pTSrc)
			new (pTDstIt) T(*pTSrc);
	}
};

template <typename T>
struct SCopySelector<T, true> // trivial copy constructor
{
	static void CopyConstruct(T * p, const T & orig)					{ *p = orig; }
	static void CopyConstructN(T * p, size_t c, const T & orig)		
	{ 
		for (T * pEnd = &p[c]; p != pEnd; ++p)
			*p = orig;
	}

	static void CopyConstructArray(T * pTDst, size_t cT, const T * pTSrc)		
	{ 
		CopyAB(pTSrc, pTDst, sizeof(T) * cT);
	}
};

template <typename T, bool TRIVIAL_DESTRUCT>
struct SDestructSelector
{
	static void Destruct(T * p)
	{
		p->~T();
	}

	static void DestructN(T * p, size_t c)
	{
		for (size_t i = 0; i < c; ++i)
			(p + i)->~T();
	}
};

template <typename T>
struct SDestructSelector<T, true> // trivial destructor 
{
	static void Destruct(T * p)					{ }
	static void DestructN(T * p, size_t c)		{ }
};

template <typename T> void Construct(T * p)									{ SConstructSelector<T, SHasTrivialConstructor<T>::V >::Construct(p); }
template <typename T> void ConstructN(T * p, size_t c)						{ SConstructSelector<T, SHasTrivialConstructor<T>::V >::ConstructN(p,c); }

template <typename T> void CopyConstruct(T * p, const T & orig)				{ SCopySelector<T, SHasTrivialCopy<T>::V >::CopyConstruct(p, orig); }
template <typename T> void CopyConstructN(T * p, size_t c, const T & orig)	{ SCopySelector<T, SHasTrivialCopy<T>::V >::CopyConstructN(p, c, orig); }
template <typename T> void CopyConstructArray(T * pTDst, size_t cT, const T * pTSrc)	
																			{ SCopySelector<T, SHasTrivialCopy<T>::V >::CopyConstructArray(pTDst, cT, pTSrc); }

template <typename T> void Destruct(T * p)									{ SDestructSelector<T, SHasTrivialDestructor<T>::V >::Destruct(p); }
template <typename T> void DestructN(T * p, size_t c)						{ SDestructSelector<T, SHasTrivialDestructor<T>::V >::DestructN(p,c); }


// String functions

struct SStringBuffer // tag=strbuf
{
			SStringBuffer()
			:m_pCozBegin(nullptr)
			,m_pCozAppend(nullptr)
			,m_cBMax(0)
				{ ; }

			SStringBuffer(char * pCoz, size_t cBMax)
			:m_pCozBegin(pCoz)
			,m_pCozAppend(pCoz)
			,m_cBMax(cBMax)
				{ ; }

	char *	m_pCozBegin;
	char *	m_pCozAppend;
	size_t	m_cBMax;
};

class CAlloc;

inline size_t CBCodepoint(const char * pCoz)
{
	if ((*pCoz & 0xF8) == 0xF0)	return 4;
	if ((*pCoz & 0xF0) == 0xE0)	return 3;
	if ((*pCoz & 0xE0) == 0xC0)	return 2;
	return 1;
}

// growing buffer for string edits
struct SStringEditBuffer // tag = seb
{
				SStringEditBuffer(CAlloc * pAlloc)
				:m_pAlloc(pAlloc)
				,m_pCozMin(nullptr)
				,m_pCozBegin(nullptr)
				,m_pCozAppend(nullptr)
				,m_pCozMax(nullptr)
					{ ; }

				~SStringEditBuffer();

	void		PrependCo(const char * pCoz, size_t cB);
	void		PrependCoz(const char * pCoz);
	void		AppendCo(const char * pCoz, size_t cB);
	void		AppendCoz(const char * pCoz);
	size_t		CBAppendCodepoint(const char * pCoz)
					{
						auto cB = CBCodepoint(pCoz);
						AppendCo(pCoz, cB+1);	// +1 because AppendCo argument cB counts the null terminator
						return cB;
					}

	void		Clear();
	void		Resize(size_t cBPrefix, size_t cbUsed, size_t cBPostfix);
	char *		PCozAllocateCopy(CAlloc * pAlloc);

	char *		PCoz()
					{ return m_pCozBegin; }
	size_t		CB()
					{ 
						if (!m_pCozBegin)
							return 0;
						return m_pCozAppend - m_pCozBegin + 1;
					}

	static const int s_cChPrefixPad = 128;

	CAlloc *	m_pAlloc;
	char *		m_pCozMin;
	char *		m_pCozBegin;
	char *		m_pCozAppend;
	char *		m_pCozMax;
};

inline bool FIsValid(const SStringBuffer & strbuf)
{
	return strbuf.m_pCozBegin != nullptr && strbuf.m_pCozAppend != nullptr;
}

size_t  CBFree(const SStringBuffer & strbuf);
size_t	CBCopyCoz(const char * pCozSource, char * aCoDest, size_t cBDest);
void	AppendCoz(SStringBuffer * pStrbuf, const char * pCozSource);
void	FormatCoz(SStringBuffer * pStrbuf, const char * pChzFormat, ...);
void	EnsureTerminated(SStringBuffer * pStrbuf, char ch);
size_t	CBCoz(const char * pCoz);
size_t	CCh(const char * pChz);
size_t  CCodepoint(const char * pCoz);
size_t  CBFromCoz(const char * pCoz, size_t cCodepoint);
void	ConcatPChz(const char* pChzA, const char * pChzB, char * pChOut, size_t cChOutMax);
bool	FAreCozEqual(const char * pChzA, const char * pChzB);
bool	FAreCozEqual(const char * pChzA, const char * pChzB, size_t cCh);
bool	FIsEmptyString(const char * pChzA);
void	ConvertChToWch(const char * pChz, size_t cWchMax, WChar * pWchz);
bool	FPChzContainsChar(const char * pChz, char ch);
void	ReplaceChars(const char * pChSrc, size_t cCh, const char * pChzRemove, char chFill, char * pChDst);

inline const char * PChzVerifyAscii(const char * pChz)
{
	auto pChzIt = pChz;
	while (*pChzIt != '\0')
	{
		EWC_ASSERT(((*pChzIt) & 0x80) == 0, "unexpected upper ascii character");
		++pChzIt;
	}

	return pChz;
}

// Min/Max/Clamp

template <typename T> T ewcMin(T a, T b)						{ return a < b ? a : b; }
template <typename T> T ewcMax(T a, T b)						{ return a > b ? a : b; }
template <typename T> T ewcClamp(T value, T min, T max)			{ return ewcMin(ewcMax(value, min), max); }
template <typename T> T ewcLerp(T a, T b, F32 gLerp)			{ return a + (b - a) * gLerp; }
template <typename T> void ewcSwap(T & a, T & b)				{ T temp = a; a = b; b = temp; }

 // system time

typedef F64 Syst;

inline Syst SystInvalid()										{ return -1.0f; }
inline bool FIsSystValid(Syst syst)								{ return syst >= 0.0f; }
inline Syst SystMax(Syst systA, Syst systB) 					{ return ewcMax(systA, systB); }

// reflect types

class CClassType;
typedef u32 TID;
extern TID TID_Nil;

// Allocator
enum BK // block kind
{
	BK_Nil			= -1,
	BK_Core			= 0,
	BK_Workspace,
	BK_WorkspaceVal,
	BK_WorkspaceFile,
	BK_Parse,
	BK_TypeCheck,
	BK_Dependency,
	BK_CodeGen,
	BK_CodeGenReflect,
	BK_SyntaxTree,
	BK_IR,
	BK_Symbol,
	BK_Stack,	// should be stack array allocated
	BK_StringTable,
	BK_ReflectTable,
	BK_UnitTest,
	BK_Linker,
};

#define EWC_ALLOC(numBytes, alignment) 			AllocImpl(numBytes, alignment, __FILE__, __LINE__)
#define EWC_ALLOC_BK(numBytes, alignment, bk) 	AllocImpl(numBytes, alignment, __FILE__, __LINE__, bk)
#define EWC_ALLOC_TYPE(TYPE_NAME) 				AllocImpl(sizeof(TYPE_NAME), EWC_ALIGN_OF(TYPE_NAME), __FILE__, __LINE__)
#define EWC_ALLOC_TYPE_ARRAY(TYPE_NAME, C_MAX) 	AllocImpl(sizeof(TYPE_NAME) * C_MAX, EWC_ALIGN_OF(TYPE_NAME), __FILE__, __LINE__)
#define EWC_NEW(PALLOC, TYPE_NAME)				new ( (PALLOC)->AllocImpl(sizeof(TYPE_NAME), EWC_ALIGN_OF(TYPE_NAME), __FILE__, __LINE__))
#define EWC_FREE(P) 							FreeImpl(P, __FILE__, __LINE__)
#define EWC_DELETE(P) 							DeleteImpl(P, __FILE__, __LINE__)

extern void * STBM_CALLBACK EwcSystemAlloc(void * pUserContext, size_t cBRequested, size_t * pCbProvided);
extern void STBM_CALLBACK EwcSystemFree(void * pUserContext, void *p);

// BB - This allocation tracking code doesn't work yet, it doesn't clean up properly for deletions (it needs to
// add some kind of HV(file,line) into an allocation prefix

//#define EWC_TRACK_ALLOCATION
#ifdef EWC_TRACK_ALLOCATION
	inline size_t CBAllocationPrefix() { return sizeof(HV); }
#else
	inline size_t CBAllocationPrefix() { return 0; }
#endif

class CAllocTracker;
class CAlloc // tag=alloc
{
public:
						CAlloc()
						:m_pStbheap(nullptr)
						,m_pAltrac(nullptr)
						,m_cBFree(0)
							{ ; }

						CAlloc(void * pBuffer, size_t cB)
						:m_pStbheap(nullptr)
						,m_pAltrac(nullptr)
							{
								Initialize(pBuffer, cB);
							}

						~CAlloc()
							{
								Shutdown();
							}

	void				Initialize(void * pB, size_t cB)
							{
								EWC_ASSERT(cB > STBM_HEAP_SIZEOF, "heap is too small");                                                         

								stbm_heap_config config;
								config.system_alloc = EwcSystemAlloc;
								config.system_free = EwcSystemFree;
								config.user_context = nullptr;

								config.minimum_alignment = 8;
								config.align_all_blocks_to_minimum = false;
								config.allocation_mutex = nullptr;
								config.crossthread_free_mutex = nullptr;
								m_cBFree = cB;

								m_pStbheap = stbm_heap_init(pB, cB, &config);
							}

	void				Shutdown()
							{
								if(s_fProgramIsShutdown)
									return;

								if (m_pStbheap)
								{
									stbm_heap_free(m_pStbheap);
									m_pStbheap = nullptr;
								}

								m_cBFree = 0;
							}

	static void			StaticShutdown()
							{ s_fProgramIsShutdown = true; }

	CAllocTracker *		PAltrac()
							{ return m_pAltrac; }

	void *				AllocImpl(size_t cB, size_t cBAlign, const char* pChzFile, int cLine, BK bk = BK_Nil)
							{
								size_t cBPrefix = CBAllocationPrefix();
								size_t alignOffset = ((cBAlign > cBPrefix) & (cBPrefix > 0)) ? cBAlign - cBPrefix : 0;
								EWC_ASSERT(m_pStbheap, "Trying to allocate from a NULL heap");
								void * pV = stbm_alloc_align_fileline(
											nullptr, 
											m_pStbheap, 
											cB + cBPrefix, 
											0, 
											cBAlign, 
											alignOffset, 
											const_cast<char*>(pChzFile), 
											cLine);

								size_t cBActual = stbm_get_allocation_size(pV);
								
								// BB - We're letting the compiler overflow into system malloc, this is ok - we just
								//  need to change the CAlloc interface from CBFree to CBAllocated.
								//if (!EWC_FVERIFY(cBActual < m_cBFree, "CAlloc out of memory!"))
								//	return nullptr;

								m_cBFree -= cBActual;

								void * pVAdjust = ((char *)pV) + cBPrefix;
								HV * pHv = (HV *)pV;
								*pHv = 0;

								uintptr_t nAlignMask = cBAlign - 1;
								EWC_ASSERT(((uintptr_t)pVAdjust & nAlignMask) == 0, "bad alignment calculation");

#ifdef EWC_TRACK_ALLOCATION
								if (m_pAltrac)
									TrackAlloc(cBActual, pChzFile, cLine, bk, pHv);
#endif

								return pVAdjust;
							}

	void				FreeImpl(void * pV, const char * pChzFile, int cLine)
							{
								EWC_ASSERT(pV, "NULL pointer in CAlloc::FreeImpl");		
								EWC_ASSERT(m_pStbheap, "Trying to free to a NULL heap");

								pV = ((char *)pV) - CBAllocationPrefix();
								size_t cB = stbm_get_allocation_size(pV);

#ifdef EWC_TRACK_ALLOCATION
								HV * pHv = (HV *)pV;
								if (m_pAltrac)
									TrackFree(cB, pHv);
#endif
								
								stbm_free(nullptr, m_pStbheap, pV);

								m_cBFree += cB;
							}

	template <typename T> 
	void				DeleteImpl(T * p, const char * pChzFile, int cLine)
							{
								p->~T();
								FreeImpl(p, pChzFile, cLine);
							}

	void				TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, HV * pHv);
	void				TrackFree(size_t cBt, HV * pHv);
	void				PrintAllocations();

	void				VerifyHeap();

	void				SetAltrac(CAllocTracker * pAltrac)
							{ m_pAltrac = pAltrac; }
	size_t				CB()	
							{ return m_cBFree; }

protected:
	stbm_heap *			m_pStbheap;
	CAllocTracker * 	m_pAltrac;
	size_t				m_cBFree;
	static bool			s_fProgramIsShutdown;
};

CAllocTracker * PAltracCreate(CAlloc * pAllocWork);
void DeleteAltrac(CAlloc * pAllocWork, CAllocTracker * pAltrac);

// GRF definition macro - this used to be a template, but I'm working around clReflects poor template support

#define EWC_DEFINE_GRF(NAME, FENUM, STORAGE)                                                                            \
struct NAME                                                                                                             \
{                                                                                                                       \
	typedef STORAGE StorageType;	                                                                                    \
	NAME():m_raw(FENUM##_None)			{ EWC_CASSERT((STORAGE)FENUM##_All == FENUM##_All, "storage class overflow"); } \
	NAME(FENUM raw):m_raw(raw)			{ ; }                                                                           \
	NAME(u32 raw):m_raw(raw)			{ EWC_ASSERT(m_raw == raw, "GRF overflow"); }                                   \
                                                                                                                        \
	bool		operator==(const NAME & other) const			{ return m_raw == other.m_raw; }                        \
	bool		operator==(FENUM other) const					{ return m_raw == other; }                              \
                                                                                                                        \
	bool		operator!=(const NAME& other) const				{ return !(*this == other); }                           \
	bool		operator!=(STORAGE other) const					{ return !(*this == other); }                           \
	NAME		operator&(const NAME & other) const				{ return this->m_raw & other.m_raw; }                   \
	NAME		operator&(STORAGE other) const					{ return this->m_raw & other; }                         \
	NAME		operator|(const NAME & other) const				{ return this->m_raw | other.m_raw; }                   \
	NAME		operator|(STORAGE other) const					{ return this->m_raw | other; }                         \
                                                                                                                        \
	NAME		operator|=(const NAME & other)					{ m_raw |= other.m_raw; return *this;}                  \
	NAME		operator&=(const NAME & other)					{ m_raw &= other.m_raw; return *this; }                 \
	NAME		operator>>(s32 cBit) const						{ return this->m_raw >> cBit; }                         \
	NAME		operator<<(s32 cBit) const						{ return this->m_raw << cBit; }                         \
                                                                                                                        \
	bool		FIsSet(STORAGE flags) const						{ return (m_raw & flags) == flags; }                    \
	bool		FIsSet(NAME other) const						{ return (m_raw & other.m_raw) == other.m_raw; }        \
	bool		FIsAnySet(STORAGE flags) const					{ return (m_raw & flags) != 0; }                        \
	bool		FIsAnySet(NAME other) const						{ return (m_raw & other.m_raw) != 0; }                  \
	STORAGE		GetSetBits(STORAGE flags) const					{ return (m_raw & flags); }                             \
	void		AddFlags(STORAGE flags)							{ m_raw = STORAGE(m_raw | flags); }                     \
	void		AddFlags(NAME flags)							{ m_raw = STORAGE(m_raw | flags.m_raw); }               \
	void		Clear(STORAGE flags = FENUM##_All)				{ m_raw = m_raw & ~flags; }                             \
	void		Clear(NAME flags)								{ m_raw = m_raw & ~flags.m_raw; }                       \
	void		AssignFlags(STORAGE flags, bool fSet)			{ if (fSet) AddFlags(flags); else Clear(flags); }		\
	void		AssignFlags(NAME flags, bool fSet)				{ if (fSet) AddFlags(flags); else Clear(flags); }		\
	bool		FIsValid(STORAGE flags) const					{ return (flags & FENUM##_All) == flags; }              \
                                                                                                                        \
	STORAGE m_raw;                                                                                                      \
};



struct SSpan // tag = span
{
				SSpan()
				:m_gMin(0.0f)
				,m_gMax(0.0f)
					{
					}

	explicit	SSpan(F32 g)
				:m_gMin(g)
				,m_gMax(g)
					{
					}

				SSpan(F32 gMin, F32 gMax)
				:m_gMin(gMin)
				,m_gMax(gMax)
					{
					}

	void		SetValue(F32 g)
					{
						m_gMin = g;
						m_gMax = g;
					}

	bool		FIsSingleValue() const
					{ return m_gMax == m_gMin; }
	F32			DG() const
					{ return m_gMax - m_gMin; }

	SSpan		SpanExtrude(F32 sExtrude) const
					{
						if (sExtrude < 0.0f)	return SSpan(m_gMin + sExtrude, m_gMin);
						else					return SSpan(m_gMax, m_gMax + sExtrude);
					}
	SSpan		SpanFromPercent(F32 uMin, F32 uMax)
					{
						F32 dG = DG();
						return SSpan(m_gMin + uMin*dG, m_gMin + uMax*dG);
					}

	F32	m_gMin;
	F32	m_gMax;
};



inline void * PVAlign(void * pV, size_t cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	uintptr_t upB = reinterpret_cast<uintptr_t>(pV);
	upB = ( (upB + cBMasked) & ~cBMasked);
	return reinterpret_cast<u8 *>(upB);
}

inline size_t CBAlign(size_t size, size_t cBAlign)
{
	size_t cBMasked = cBAlign - 1;
	size = ( (size + cBMasked) & ~cBMasked);
	return size;
}


// basic ring buffer
class CRingBuffer // tag - ring
{
public:
				CRingBuffer()
				:m_aB(nullptr)
				,m_pBFence(nullptr)
				,m_iB(0)
				,m_cB(0)
				,m_cBMax(0)
					{ ; }

	void		SetArray(void * aB, s32 cBMax)
					{
						EWC_ASSERT(m_cB == 0, "initializing buffer with active allocations");
						m_aB = static_cast<u8 *>(PVAlign(aB, 16));
						m_cBMax = cBMax - (m_aB - static_cast<u8 *>(aB));

						m_iB = 0;
						m_cB = 0;
					}

	void *		PVAlloc(size_t cB, size_t cBAlign)
					{
						size_t cBActual = 0;
						void *pVActual = PVComputeAllocation(cB, cBAlign, &cBActual);
						return PVAllocActual(pVActual, cBActual);
					}

	void *		PVAllocActual(void * pV, size_t cBActual)
					{
						m_cB += cBActual;
						EWC_ASSERT(m_cB <= m_cBMax,"ring buffer overflow");
						return pV;
					}

	void *		PVComputeAllocation(
					size_t cB, 
					size_t cBAlign, 
					size_t * pcBAllocActual, 
					size_t cBPrefix = 0, 
					size_t cBPrefixAlign = 1) const
					{
						// compute the actual allocation including padding and ring wrap around space.

						size_t cBTemp = m_cB;
						size_t iBStart = (m_iB + m_cB ) % m_cBMax;
						u8 * pBStart = &m_aB[iBStart];
						u8 * pBMin = pBStart;

						pBMin = static_cast<u8 *>(PVAlign(pBMin, cBPrefixAlign));
						pBMin += cBPrefix;
						pBMin = static_cast<u8 *>(PVAlign(pBMin, cBAlign));
						u8 * pBMax = pBMin + cB;

						if (pBMax > &m_aB[m_cBMax])
						{
							// wrap around, allocate the last bit of the ring

							if (!EWC_FVERIFY(FCheckFence(pBMin, pBMax), "stepped over ring buffer fence"))
								return nullptr;
							cBTemp += m_cBMax - iBStart;
							pBStart = m_aB;
							pBMin = &m_aB[cBPrefix];
							pBMin = static_cast<u8 *>(PVAlign(pBMin, cBAlign));

							pBMax = &pBMin[cB];
						}

						if (!EWC_FVERIFY(FCheckFence(pBMin, pBMax), "stepped over ring buffer fence"))
							return nullptr;

						EWC_ASSERT(pBMax >= pBStart,"corrupt m_cB");
						cBTemp += pBMax - pBStart;
						*pcBAllocActual = cBTemp - m_cB;
						return pBMin;
					}

	void *		PVAddFence()
					{
						EWC_ASSERT(m_pBFence == nullptr, "cannot set multiple fences");
						size_t iBStart = (m_iB + m_cB ) % m_cBMax;
						m_pBFence = &m_aB[iBStart];
						return m_pBFence;
					}

	void		FreeToFence(void * pVFence)
					{
						if (!EWC_FVERIFY(m_pBFence, "called FreeTpFence with NULL fence"))
							return;

						EWC_ASSERT(m_pBFence == pVFence, "fence mismatch");
						FreeToPB(m_pBFence);
						m_pBFence = nullptr;
					}

	void		FreeToPB(u8 * pB)
					{
						EWC_ASSERT(pB, "bad argument");

						if (pB < &m_aB[m_iB])
						{
							// free to the end of the buffer
							m_cB -= m_cBMax - m_iB;
							m_iB = 0;
						}

						size_t iBPrev = m_iB;
						m_iB = pB - m_aB;
						EWC_ASSERT(m_cB >= (m_iB - iBPrev), "bad calc");
						m_cB -= (m_iB - iBPrev); 

						EWC_ASSERT((m_cB >= 0) & (m_iB < m_cBMax), "bad free operation");
					}
	void		FreeAll()
					{
						m_iB = 0;
						m_cB = 0;
						m_pBFence = nullptr;
					}

	size_t		CB()	
					{ return m_cB; }
	size_t		CbFree()
					{ return m_cBMax - m_cB; }
	bool		FCheckFence(void * pVMin, void * pVMax) const
					{ return (m_pBFence == nullptr) | (m_pBFence <= pVMin) | (m_pBFence >= pVMax); }

	u8 *	m_aB;
	u8 * 	m_pBFence;		
	size_t	m_iB;		// index of the first allocated byte in the ring (least recent)
	size_t	m_cB;
	size_t	m_cBMax;
};


#define DEBUG_BLOCK_RING_BUFFER 1

// ring buffer for allocating (and freeing) in blocks
//  stores block size in a word sized allocation in the word immediately preceeding the returned type.
class CBlockRingBuffer // tag = bring
{
public:
	static const u32 s_nExpectedStub = 0xBEEFBEEF;
	
	struct SHeader // tag = head
	{
#if DEBUG_BLOCK_RING_BUFFER
		u32		m_debugStub;
		void	SetupStub()		
					{ m_debugStub = s_nExpectedStub; }
		bool	FIsValid()
					{ return m_debugStub == s_nExpectedStub; }
#else
		void	SetupStub()		
					{ ; }
		bool	FIsValid()
					{ return true; }
#endif
		SHeader *	m_pHeadNext;
	};

	class CIterator // tag = iter
	{
	public:
				CIterator(CBlockRingBuffer * pBring)
				:m_pHead(pBring->m_pHeadFirst)
					{ ; }

		void *	PVNext()
					{
						if (!m_pHead)
							return nullptr;

						SHeader * pHeadPrev = m_pHead;
						m_pHead = m_pHead->m_pHeadNext;
						return pHeadPrev + 1;
					}

		SHeader * m_pHead;
	};

				CBlockRingBuffer()
				:m_pHeadFirst(nullptr)
				,m_pHeadLast(nullptr)
				,m_ring()
					{ ; }

	void		SetArray(void * aB, s32 cBMax)
					{ m_ring.SetArray(aB, cBMax); }

	CIterator	Iter()
					{ return CIterator(this); }

	void *		PVAlloc(int cB, int cBAlign)
					{
						size_t cBActual = 0;
						void * pVReturn = m_ring.PVComputeAllocation(
													cB, 
													cBAlign, 
													&cBActual, 
													sizeof(SHeader), 
													EWC_ALIGN_OF(SHeader));
						EnsureBytesFree(cBActual);
						if (!EWC_FVERIFY(cBActual <= m_ring.CbFree(), "failed to free enough memory from block ring"))
							return nullptr;

						SHeader * pHead = reinterpret_cast<SHeader *>(pVReturn) -1;
						pHead->SetupStub();
						pHead->m_pHeadNext = nullptr;

						if (m_pHeadLast)
						{
							m_pHeadLast->m_pHeadNext = pHead;
						}
						m_pHeadLast = pHead;
						if (!m_pHeadFirst)
						{
							m_pHeadFirst = pHead;
						}

						return m_ring.PVAllocActual(pVReturn, cBActual);
					}

	void *		PVAllocChildData(void * pVParent, int cB, int cBAlign)
					{
						void * PvLast = (m_pHeadLast + 1);
						if (!EWC_FVERIFY(pVParent == PvLast, "bad parent passed into PvAllocChildData"))
							return nullptr;

						if (!EWC_FVERIFY(m_pHeadLast, "must follow allocation"))
							return nullptr;

						size_t cBActual = 0;
						void * pV = m_ring.PVComputeAllocation(cB, cBAlign, &cBActual);
						EnsureBytesFree(cBActual);
						if (!EWC_FVERIFY(cBActual <= m_ring.CbFree(), "failed to free enough memory from block ring"))
							return nullptr;
						return m_ring.PVAllocActual(pV, cBActual);
					}

	void		EnsureBytesFree(size_t cBRequired)
					{
						if (!EWC_FVERIFY(cBRequired < m_ring.m_cBMax, "trying to allocate a block larger than our ring buffer"))
							return;

						// free blocks until there are cBRequired bytes free
						while (m_ring.CbFree() < cBRequired)
						{
							if (!EWC_FVERIFY(m_pHeadFirst && m_pHeadFirst->FIsValid(), "bad block list"))
								break;

							if (m_pHeadFirst == m_pHeadLast)
							{
								m_ring.FreeAll();
								m_pHeadFirst = nullptr;
								m_pHeadLast = nullptr;
							}
							else
							{
								m_pHeadFirst = m_pHeadFirst->m_pHeadNext;
								m_ring.FreeToPB(reinterpret_cast<u8 *>(m_pHeadFirst));
							}
						}
					}


	SHeader *		m_pHeadFirst;	// pointer to the header before least recent allocation
	SHeader *		m_pHeadLast;	// pointer to the header before most recent allocation

	private:
		// don't let anyone else mess with the ring buffer or our headers could end up out of sync
	CRingBuffer		m_ring;
};



// typed indices - just a wrapper on an int used for type safety
enum INDEXROSTER	{ INDEXROSTER_Nil };	// used with a CAryRoster for tracking ii values
enum INDEXINST		{ INDEXINST_Nil };		// used with a CAryRoster for instance indices
enum INDEXCLASS		{ INDEXCLASS_Nil};		// used for class Id

// templated index wrapper - has a type constant so that the compiler won't try to cross convert
template <typename T, typename INDEXK>
class STypedIndex
{
public:
	typedef T Type;
								STypedIndex()			
								:m_index(INVALID)
									{ ; }
								STypedIndex(T index)
								:m_index(index)
									{ ; }

	T							I() const													{ return m_index; }
	bool						FisValid() const											{ return m_index != INVALID; }

	bool						operator==(const STypedIndex<T, INDEXK> & other) const		{ return m_index == other.m_index; }
	bool						operator!=(const STypedIndex<T, INDEXK> & other) const		{ return !(*this == other); }
	bool						operator<(const STypedIndex<T, INDEXK> & other) const		{ return m_index < other.m_index; }
	bool						operator>(const STypedIndex<T, INDEXK> & other) const		{ return m_index > other.m_index; }
	bool						operator<=(const STypedIndex<T, INDEXK> & other) const		{ return (*this < other) | (*this == other); }
	bool						operator>=(const STypedIndex<T, INDEXK> & other) const		{ return (*this > other) | (*this == other); }
	STypedIndex<T, INDEXK> &	operator++()												{ m_index++; return *this;}

	static const T INVALID = -1;
	T m_index;
};

typedef STypedIndex<s16, INDEXROSTER>		IRoster;	// prefix = ii
typedef STypedIndex<s16, INDEXINST>			IInstance;	// prefix = i
typedef STypedIndex<s16, INDEXCLASS>		IClass;		// prefix = ic



struct SRosterHandle // tag = rohan
{
			SRosterHandle()
			:m_iType(-1)
			,m_iLifetime(0)
				{ ; }

	bool	FIsValid()
				{ return m_iType >= 0; }

	s16		m_iType;
	u16		m_iLifetime;	// lifetime index (to differentiate reuses of a roster instance)
};

inline u32		HvFromAB(const void * aB, size_t cB)
					{
						return HvFromPBFVN(aB, cB);
					}

inline u32		HvFromP(void * pV)
					{
						return HvFromAB(&pV, sizeof(pV));
					}

template <typename T>
HV HvExtract(const T & t)
{
	return static_cast<HV>(t);
}

template <typename T>
HV HvExtract(const T * pT)
{
	return HvFromP((void *)pT);
}

template <typename T>
HV HvExtract(T * pT)
{
	return HvFromP((void *)pT);
}


// Thomas Wang's 32-bit hash mix function
template<typename T>
struct SHash
{
	HV operator()(const T& t) const
	{
		HV hv = HvExtract(t);
		hv = (hv+0x7ed55d16) + (hv<<12);
		hv = (hv^0xc761c23c) ^ (hv>>19);
		hv = (hv+0x165667b1) + (hv<<5);
		hv = (hv+0xd3a2646c) ^ (hv<<9);
		hv = (hv+0xfd7046c5) + (hv<<3);
		hv = (hv^0xb55a4f09) ^ (hv>>16);
		return hv;
	}
};


inline bool FIsPowerOfTwo(size_t value)
{
	return (value & (value-1)) == 0;
}

// container insertion flags
enum FINS
{
	FINS_Nil = -1,
	FINS_Error = FINS_Nil,
	FINS_AlreadyExisted,
	FINS_Inserted,
};



enum EDGES // edge state
{
	EDGES_Off,
	EDGES_Release,
	EDGES_Hold,
	EDGES_Press,

	EWC_MAX_MIN_NIL(EDGES)
};

enum FEDGES // edges state flags
{
	FEDGES_None     = 0x0,
	FEDGES_Off      = 0x1 << EDGES_Off,
	FEDGES_Release  = 0x1 << EDGES_Release,
	FEDGES_Hold     = 0x1 << EDGES_Hold,
	FEDGES_Press    = 0x1 << EDGES_Press,
	FEDGES_All		= (0x1 << EDGES_Max) - 1

};

EWC_DEFINE_GRF(GRFEDGES, FEDGES, u8);                                                                            \

inline bool FIsEdgesDown(EDGES edges)	{ return edges >= EDGES_Hold; }



// BB - this shouldn't really be in ewcTypes!

enum TILAY // tile layer
{
	TILAY_Ground,
	TILAY_Shadow,
	TILAY_Object,
	TILAY_Hud,

	EWC_MAX_MIN_NIL(TILAY)
};

} // namespace EWC

#endif // EWC_TYPES_GUARD

//------------------------------------------------------------------------------------------------------------------------

#ifdef EWC_TYPES_IMPLEMENTATION
#ifndef EWC_TYPES_IMPL_GUARD
#define EWC_TYPES_IMPL_GUARD


#include <cstdarg>
#include <stdio.h>
#include <string.h>

#define STB_MALLOC_IMPLEMENTATION
#include "stb_malloc.h"

#ifdef EWC_TRACK_ALLOCATION
#include "EwcString.h"
#include "EwcArray.h"
#include "EwcHash.h"
#endif

namespace EWC
{

CAlloc	g_allocCManaged;
bool	CAlloc::s_fProgramIsShutdown = false;



class CAllocTracker //tag=altrac
{
public:
#ifdef EWC_TRACK_ALLOCATION
			CAllocTracker()
			:m_aryEntry(BK_Core)
			,m_hashHvIentry()
				{ ; }

			CAllocTracker(CAlloc * pAlloc)
			:m_aryEntry(pAlloc, BK_Core)
			,m_hashHvIentry(pAlloc, BK_Nil)
				{ ; }
#endif 

	void	Clear()
			{
#ifdef EWC_TRACK_ALLOCATION
				m_aryEntry.Clear();
				m_hashHvIentry.Clear();
#endif
			}

	HV		HvFromFileLineBk(const char * pChzFile, int cLine, BK bk)
			{
#ifdef EWC_TRACK_ALLOCATION
				char aCh[2048];
				SStringBuffer strbuf(aCh, EWC_DIM(aCh));
				FormatCoz(&strbuf, "%s:%d%d", pChzFile, cLine, bk);
				return HvFromPCoz(aCh);
#else
				return 0;
#endif
			}

	void	TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, HV * pHv)
			{
#ifdef EWC_TRACK_ALLOCATION

				HV hv = HvFromFileLineBk(pChzFile, cLine, bk);
				*pHv = hv;

				int * piEntry;
				if (m_hashHvIentry.FinsEnsureKey(hv, &piEntry) == FINS_AlreadyExisted)
				{
					SEntry * pEntry = &m_aryEntry[*piEntry];
					pEntry->m_cB += cB;
					pEntry->m_cBHighwater = ewcMax(pEntry->m_cBHighwater, pEntry->m_cB);
					EWC_ASSERT(pEntry->m_cLine == cLine, "line mismatch in TrackAlloc");
					EWC_ASSERT(pEntry->m_bk == bk, "Block kind mismatch in TrackAlloc");
					++pEntry->m_cAllocations;
				}
				else
				{
					*piEntry = (int)m_aryEntry.C();
					SEntry * pEntry = m_aryEntry.AppendNew();
					pEntry->m_cB 	   	   = cB;
					pEntry->m_cBHighwater  = cB;
					pEntry->m_cAllocations = 1;
					pEntry->m_pChzFile 	   = pChzFile;
					pEntry->m_cLine    	   = cLine;
					pEntry->m_bk			= bk;
				}
#endif // EWC_TRACK_ALLOCATION
			}

	void	TrackFree(size_t cB, HV * pHv)
			{
#ifdef EWC_TRACK_ALLOCATION
				if (*pHv)
				{
					int * piEntry = m_hashHvIentry.Lookup(*pHv);

					if (EWC_FASSERT(piEntry, "Failed to find allocation record during free"))
					{
						SEntry * pEntry = &m_aryEntry[*piEntry];
						pEntry->m_cB -= cB;
					}
				}
#endif // EWC_TRACK_ALLOCATION
			}

	void Print()
	{
#ifdef EWC_TRACK_ALLOCATION
		SEntry * pEntryMac = m_aryEntry.PMac();
		size_t cBTotal = 0;
		for (SEntry * pEntry = m_aryEntry.A(); pEntry != pEntryMac; ++pEntry)
		{
			if (pEntry->m_cB == 0)
				continue;
			printf("%zd / %zd\t\t %s BK(%d) : %d\n", pEntry->m_cB, pEntry->m_cBHighwater, pEntry->m_pChzFile, pEntry->m_bk, pEntry->m_cLine);
			cBTotal += pEntry->m_cB;
		}
		printf("%zd tracked\n", cBTotal);
#endif
	}

	struct SEntry // tag=entry
	{
		size_t			m_cB;
		size_t			m_cBHighwater;
		int				m_cAllocations;
		const char *	m_pChzFile;
		int				m_cLine;
		BK				m_bk;
	};

#ifdef EWC_TRACK_ALLOCATION
	// BB - replace with indexed set
	EWC::CDynAry<SEntry>	m_aryEntry;
	EWC::CHash<HV, int>		m_hashHvIentry;
#endif // EWC_TRACK_ALLOCATION
};


CAllocTracker * PAltracCreate(CAlloc * pAllocWork)
{
#ifdef EWC_TRACK_ALLOCATION
	CAllocTracker * pAltrac = EWC_NEW(pAllocWork, CAllocTracker) CAllocTracker(pAllocWork);
	return pAltrac;
#else
	return nullptr;
#endif
}

void DeleteAltrac(CAlloc * pAllocWork, CAllocTracker * pAltrac)
{
#ifdef EWC_TRACK_ALLOCATION
	pAllocWork->EWC_DELETE(pAltrac);
#endif
}

void * STBM_CALLBACK EwcSystemAlloc(void * pUserContext, size_t cBRequested, size_t * pCbProvided)
{
	void * pReturn = malloc(cBRequested);
	*pCbProvided = pReturn ? cBRequested : 0;
	return pReturn;
}

void STBM_CALLBACK EwcSystemFree(void * pUserContext, void *p)
{
	free(p);
}

void CAlloc::VerifyHeap()
{
	// doesn't do anything unless STBM_DEBUGCHECK is set
	STBM__CHECK_LOCKED(m_pStbheap);
}

void CAlloc::TrackAlloc(size_t cB, const char * pChzFile, int cLine, BK bk, HV * pHv)
{
	m_pAltrac->TrackAlloc(cB, pChzFile, cLine, bk, pHv);
}

void CAlloc::TrackFree(size_t cB, HV * pHv)
{
	m_pAltrac->TrackFree(cB, pHv);
}

void CAlloc::PrintAllocations()
{
	if (m_pAltrac)
		m_pAltrac->Print();
}

void AssertHandler(const char* pChzFile, u32 line, const char* pChzCondition, const char* pChzMessage, ... )
{
	printf("Assertion failed: \"%s\" at %s:%u\n", pChzCondition, pChzFile, line);

	if (pChzMessage)
	{
		va_list ap;
		va_start(ap, pChzMessage);
		vprintf(pChzMessage, ap);
		printf("\n");
	}
}

void FillAB(u8 b, void * pDest, size_t cB)
{
	memset(pDest, b, cB);
}

void ZeroAB(void * pDest, size_t cB)
{
	memset(pDest, 0, cB);
}

void CopyAB(const void * pSource, void * pDest, size_t cB)
{
	memcpy(pDest, pSource, cB);
}

bool FAreSameAB(const void * aB0, void * aB1, size_t cB)
{
	return memcmp(aB0, aB1, cB) == 0;
}

void AppendCoz(SStringBuffer * pStrbuf, const char *pCozSource)
{
	if (!EWC_FVERIFY(pCozSource && FIsValid(*pStrbuf), "Null pointer passed to CCoCopy"))
		return;

	char * pCozDest = pStrbuf->m_pCozAppend;
	char * pCozDestEnd = &pStrbuf->m_pCozBegin[pStrbuf->m_cBMax-1];
	for ( ; (*pCozSource != '\0') & (pCozDest != pCozDestEnd); ++pCozSource, ++pCozDest)
	{
		*pCozDest = *pCozSource;
	}

	pStrbuf->m_pCozAppend = pCozDest;
	EnsureTerminated(pStrbuf, '\0');
}

static inline bool FIsBasicChar(char ch)	{ return (ch & 0x80) == 0;}
static inline bool FIsStarterChar(char ch)	{ return (ch & 0xC0) == 0xC0;}

void EnsureTerminated(SStringBuffer * pStrbuf, char ch)
{
	size_t iB = pStrbuf->m_pCozAppend - pStrbuf->m_pCozBegin;

	if (pStrbuf->m_cBMax <= 0)
		return;

	iB = ewcMin(iB, pStrbuf->m_cBMax -1);

	auto pCozEnd = &pStrbuf->m_pCozBegin[iB];
	auto pCozBackup = pCozEnd; // - 1;	// skip the spot our terminator will go

	while (pCozBackup != pStrbuf->m_pCozBegin)
	{
		--pCozBackup;
		if (FIsBasicChar(*pCozBackup) || FIsStarterChar(*pCozBackup))
			break;
	}

	size_t cBBackup;
	if ((*pCozBackup & 0xF8) == 0xF0)		cBBackup = 4;
	else if ((*pCozBackup & 0xF0) == 0xE0)	cBBackup = 3;
	else if ((*pCozBackup & 0xE0) == 0xC0)	cBBackup = 2;
	else									cBBackup = 1;

	if (cBBackup > size_t(pCozEnd - pCozBackup))
	{
		*pCozBackup = ch;
	}
	else
	{
		*pCozEnd = ch;
	}
}

size_t CBFree(const SStringBuffer & strbuf)
{
	return strbuf.m_cBMax - (strbuf.m_pCozAppend - strbuf.m_pCozBegin) - 1; // -1 because pCozAppend points at the null term, need to count it.
}

size_t	CBCopyCoz(const char * pCozSource, char * aCoDest, size_t cBDest)
{
	SStringBuffer strbuf(aCoDest, cBDest);
	AppendCoz(&strbuf, pCozSource);
	return strbuf.m_pCozAppend - strbuf.m_pCozBegin + 1;
}

void FormatCoz(SStringBuffer * pStrbuf, const char * pCozFormat, ...)
{
	ptrdiff_t cBMax = &pStrbuf->m_pCozBegin[pStrbuf->m_cBMax] - pStrbuf->m_pCozAppend;
	if (cBMax > 1)
	{
		va_list ap;
		va_start(ap, pCozFormat);
#ifdef WIN32
		ptrdiff_t cCh = vsnprintf_s(pStrbuf->m_pCozAppend, cBMax, _TRUNCATE, pCozFormat, ap);
#else
		ptrdiff_t cCh = vsnprintf(pStrbuf->m_pCozAppend, cBMax, pCozFormat, ap);
		pStrbuf->m_pCozAppend[cBMax-1] = 0;
#endif
		va_end(ap);

		if (cCh == -1)
		{
			cCh = cBMax-1;
		}
		pStrbuf->m_pCozAppend += cCh;
	}

	// handle truncation within utf8 multibyte char
	EnsureTerminated(pStrbuf, '\0');
}

size_t CBCoz(const char * pCoz)
{
	// bytes needed for this string (including the null terminator)
	if (!pCoz)
		return 0;

	auto pCozIt = pCoz;
	while (*pCozIt != '\0')
		++pCozIt;

	++pCozIt;
	return pCozIt - pCoz;
}

size_t CCh(const char * pChz)
{
	// characters included in this string (excluding the null terminator)

	if (!pChz)
		return 0;
	return strlen(pChz);
}

size_t CCodepoint(const char * pCoz) 
{
	// utf-8 codepoints included in this string (excluding the null terminator)
	size_t cCodepoint = 0;
	while (*pCoz != '\0')
	{
		u8 codepoint = *pCoz;
		if ((0xf8 & codepoint) == 0xf0)			{ pCoz += 4; }
		else if ((0xf0 & codepoint) == 0xe0)	{ pCoz += 3; }
		else if ((0xc0 & codepoint) == 0xc0)	{ pCoz += 2; }
		else									{ pCoz += 1; }

		++cCodepoint;
	}

	return cCodepoint;
}

size_t CBFromCoz(const char * pCoz, size_t cCodepoint) 
{
	// bytes needed for this utf-8 string, up to a given codepoint (including the null terminator)
	size_t iCodepoint = 0;
	auto pCozIt = pCoz;
	while ((*pCozIt != '\0') & (iCodepoint < cCodepoint))
	{
		u8 codepoint = *pCozIt;
		if ((0xf8 & codepoint) == 0xf0)			{ pCozIt += 4; }
		else if ((0xf0 & codepoint) == 0xe0)	{ pCozIt += 3; }
		else if ((0xc0 & codepoint) == 0xc0)	{ pCozIt += 2; }
		else									{ pCozIt += 1; }

		++iCodepoint;
	}
	++pCozIt;

	return pCozIt - pCoz;
}

void ConcatPChz(const char* pChzA, const char * pChzB, char * pChOut, size_t cChOutMax)
{
	char* pChzOutIt = pChOut;
	char* pChzOutEnd = &pChOut[cChOutMax-1];

	const char* pChzInIt = pChzA;
	while((*pChzInIt != '\0') & (pChzOutIt != pChzOutEnd))
		*pChzOutIt++ = *pChzInIt++;

	pChzInIt = pChzB;
	while((*pChzInIt != '\0') & (pChzOutIt != pChzOutEnd))
		*pChzOutIt++ = *pChzInIt++;

	*pChzOutIt = '\0';
}

int NCmpCoz(const char * pCozA, const char * pCozB)
{
	if ((pCozA == nullptr) | (pCozB == nullptr))
	{
		if (pCozA == pCozB)
			return 0;
		
		return (pCozA == nullptr) ? -1 : 1;
	}

	while ((*pCozA != '\0') | (*pCozB != '\0'))
	{
		auto chA = *pCozA;
		auto chB = *pCozB;
		if (chA < chB)
			return -1;
		else if (chA > chB)
			return 1;

		++pCozA;
		++pCozB;
	}

	return 0;
}

bool FIsEmptyString(const char * pChzA)
{
	if (!pChzA)
		return true;
	return pChzA[0] == '\0';
}

bool FAreCozEqual(const char * pCozA, const char * pCozB)
{
	return NCmpCoz(pCozA, pCozB) == 0;
}

int NCmpCoz(const char * pCozA, const char * pCozB, size_t cCodepoint)
{
	if ((pCozA == nullptr) | (pCozB == nullptr))
	{
		if (pCozA == pCozB)
			return 0;
		
		return (pCozA == nullptr) ? -1 : 1;
	}

	size_t iCodepoint = 0;
	while ((iCodepoint < cCodepoint) & ((*pCozA != '\0') | (*pCozB != '\0')))
	{
		auto chA = *pCozA;
		auto chB = *pCozB;
		if (chA < chB)
			return -1;
		else if (chA > chB)
			return 1;

		++pCozA;
		++pCozB;

		if ((chA & 0xC0) != 0x80)	// only count the first bytes of each codepoint
			++iCodepoint;
	}

	return 0;
}

bool FAreCozEqual(const char * pChzA, const char * pChzB, size_t cCodepoint)
{
	return NCmpCoz(pChzA, pChzB, cCodepoint) == 0;
}

void ConvertChToWch(const char* pChz, size_t cWchMax, WChar * pWchz)
{
	WChar * pWchEnd = &pWchz[cWchMax-1];
	for ( ; (pWchz != pWchEnd) & (*pChz != '\0'); ++pWchz, ++pChz)
	{
		*pWchz = *pChz;
	}
	*pWchz = 0;
}

bool FPChzContainsChar(const char * pChz, char ch)
{
	while (*pChz != '\0')
	{
		if (*pChz == ch)
			return true;
		++pChz;
	}
	return false;
}

void ReplaceChars(const char * pChSrc, size_t cCh, const char * pChzRemove, char chFill, char * pChDst)
{
	const char * pChSrcEnd = &pChSrc[cCh];
	for (; pChSrc != pChSrcEnd; ++pChSrc, ++pChDst)
	{
		char chSrc = *pChSrc;
		*pChDst = FPChzContainsChar(pChzRemove, chSrc) ? chFill : chSrc;
	}
}

SStringEditBuffer::~SStringEditBuffer()
{
	Resize(0,0,0);
}

void SStringEditBuffer::PrependCo(const char * pCoz, size_t cB)
{
	auto cCh = cB - 1;
	size_t cBPrefix = m_pCozBegin - m_pCozMin;
	if (cBPrefix < cCh)
	{
		Resize(cCh + s_cChPrefixPad, ewcMax<size_t>(s_cChPrefixPad, CB()), m_pCozMax - m_pCozAppend);
	}

	m_pCozBegin -= cCh;
	CopyAB(pCoz, m_pCozBegin, cCh);
}

void SStringEditBuffer::PrependCoz(const char * pCoz)
{
	size_t cBCoz = CBCoz(pCoz);
	PrependCo(pCoz, cBCoz);
}

void SStringEditBuffer::AppendCo(const char * pCoz, size_t cB)
{
	size_t cBAvail = m_pCozMax - m_pCozAppend;
	if (cBAvail < cB) // +1 for null terminator
	{
		Resize(s_cChPrefixPad, ewcMax<size_t>(s_cChPrefixPad, CB()), cB + s_cChPrefixPad);
	}

	CopyAB(pCoz, m_pCozAppend, cB);

	m_pCozAppend += cB-1;
	EWC_ASSERT((uintptr_t(m_pCozAppend) <= uintptr_t(m_pCozMax)), "seb overflow"); 
}

void SStringEditBuffer::AppendCoz(const char * pCoz)
{
	size_t cBCoz = CBCoz(pCoz);
	AppendCo(pCoz, cBCoz);
}

void SStringEditBuffer::Clear()
{
	if (!m_pCozMin)
		return;

	m_pCozBegin = m_pCozMin + s_cChPrefixPad;
	m_pCozAppend = m_pCozBegin;
	*m_pCozAppend = '\0';
}

void SStringEditBuffer::Resize(size_t cBPrefix, size_t cBUsed, size_t cBPostfix)
{
	if (!EWC_FVERIFY(m_pAlloc, "missing allocator"))
		return;

	auto pCozMinOld = m_pCozMin;
	auto pCozBeginOld = m_pCozBegin;
	auto cBOld = CB();

	size_t cB = cBPrefix + cBUsed + cBPostfix;

	if (!cB)
	{
		m_pCozMin = nullptr;
		m_pCozBegin = nullptr;
		m_pCozAppend = nullptr;
		m_pCozMax = nullptr;

		if (pCozMinOld)
		{
			m_pAlloc->EWC_DELETE(pCozMinOld);
		}
		return;
	}

	m_pCozMin = (char*)m_pAlloc->EWC_ALLOC(sizeof(char) * cB, EWC_ALIGN_OF(char));
	m_pCozBegin = m_pCozMin + cBPrefix;
	m_pCozAppend = m_pCozBegin;
	m_pCozMax = m_pCozMin + cB;
	
	if (pCozBeginOld)
	{
		cBOld = ewcMin<size_t>(cBOld, cBUsed);
		CopyAB(pCozBeginOld, m_pCozBegin, cBOld);

		m_pCozAppend = m_pCozBegin + (cBOld -1); // -1 to point at the null terminator.
		m_pAlloc->EWC_DELETE(pCozMinOld);
	}

	*m_pCozAppend = '\0';
}

char * SStringEditBuffer::PCozAllocateCopy(CAlloc * pAlloc)
{
	auto cBNew = CB();
	const char * pCozSource = PCoz();

	if (!cBNew)
	{
		cBNew = 1;
		pCozSource = "";
	}
	
	char * pCozNew = (char*)pAlloc->EWC_ALLOC(cBNew, sizeof(char));

	CBCopyCoz(pCozSource, pCozNew, cBNew);
	return pCozNew;
}

void DoNothing()
{
}

} // namespace EWC

#endif // EWC_TYPES_IMPL_GUARD
#endif // EWC_TYPES_IMPLEMENTATION
