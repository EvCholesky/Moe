/* Copyright (C) 2019 Evan Christensen
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

// structures and methods dealing with generics

#include "Error.h"
#include "EwcTypes.h"
#include "EwcArray.h"
#include "EwcHash.h"
#include "EwcString.h"
#include "Lexer.h"

class CSTNode;
class CSymbolTable;
class CWorkspace;
struct SSymbol;
struct STypeCheckWorkspace;
struct STypeInfo;

enum GENK	// GENeric Kind
{
	GENK_Value,			// baked value (think integer array size)	($C: int)
	GENK_Type,			// generic type								(t: $T)
	EWC_MAX_MIN_NIL(GENK)
};

struct SAnchor		// tag anc
{
					SAnchor()
					:m_pStnodBaked(nullptr)
					,m_pTin(nullptr)
					,m_genk(GENK_Nil)
						{ ; }

	bool			FIsNull() const
						{ return m_pStnodBaked == nullptr && m_pTin == nullptr; }
	void			AssertIsValid() const
						{
							switch (m_genk)
							{
							case GENK_Nil:		EWC_ASSERT(FIsNull(), "should be null"); break;
							case GENK_Value:	EWC_ASSERT(m_pTin == nullptr, "unexpected type anchor"); break;
							case GENK_Type:		EWC_ASSERT(m_pStnodBaked == nullptr, "unexpected baked value"); break;
							}
						}


	CSTNode *		m_pStnodBaked;
	STypeInfo *		m_pTin;
	GENK			m_genk;
};

// A generic map is instructions for instantiating a generic procedure or struct. For a genmap to be complete it must anchor
//   all of the anchor points from the generic declaration, but they may be anchored to a different generic label

struct SGenericMap // tag = genmap
{
							SGenericMap(EWC::CAlloc * pAlloc, const char * pChzPrefix, STypeInfo * pTinOwner)
							:m_strName()	
							,m_mpStrAnc(pAlloc, EWC::BK_TypeCheckGenerics)
							,m_aryPStnodManaged(pAlloc, EWC::BK_TypeCheckGenerics)
							,m_aryLexlocSrc(pAlloc, EWC::BK_TypeCheckGenerics)
							,m_cPartialType(0)
							,m_cPartialValue(0)
								{
#define BUILD_GENMAP_NAMES 1
#if BUILD_GENMAP_NAMES
									m_strName = pChzPrefix;
									if (pTinOwner)
									{
										char aCh[128];
										EWC::SStringBuffer strbuf(aCh, EWC_DIM(aCh));
										FormatCoz(&strbuf, "%s_%s", pTinOwner->m_strName.PCoz(), pChzPrefix);

										m_strName = EWC::CString(aCh);
									}
#endif 
								}

							~SGenericMap()
								{
									EWC_ASSERT(m_aryPStnodManaged.C() == 0, "Generic map stnod list was not cleaned up");
								}

							void Cleanup(EWC::CAlloc * pAlloc)
							{
								auto ppStnodPMac = m_aryPStnodManaged.PMac();
								for (auto ppStnod = m_aryPStnodManaged.A(); ppStnod != ppStnodPMac; ++ppStnod)
								{
									pAlloc->EWC_DELETE(*ppStnod);
								}
								m_aryPStnodManaged.Clear();
							}

	void 					Swap(SGenericMap * pGenmapOther)
								{ 
									EWC::ewcSwap(m_strName, pGenmapOther->m_strName);
									EWC::ewcSwap(m_cPartialType, pGenmapOther->m_cPartialType);
									EWC::ewcSwap(m_cPartialValue, pGenmapOther->m_cPartialValue);

									m_mpStrAnc.Swap(&pGenmapOther->m_mpStrAnc);
									m_aryPStnodManaged.Swap(&pGenmapOther->m_aryPStnodManaged);
									m_aryLexlocSrc.Swap(&pGenmapOther->m_aryLexlocSrc);
								}

	SAnchor	*				PAncMapType(const EWC::CString & strName, STypeInfo * pTin);
	SAnchor *				PAncMapValue(const EWC::CString & strName, CSTNode * pStnodBaked);
	SAnchor *				PAncLookup(const EWC::CString & str)
								{ return m_mpStrAnc.Lookup(str); }

	SLexerLocation 			LexlocReporting()
								{
									if (EWC_FVERIFY(!m_aryLexlocSrc.FIsEmpty(), "no source location in generic map '%s'.", m_strName.PCoz()))
									{
										return m_aryLexlocSrc[0];
									}
									return SLexerLocation();
								}

	bool					FIsEmpty() const
								{
									return m_mpStrAnc.FIsEmpty();
								}
	bool					FIsPartiallyInstantiated() const
								{
									return m_cPartialType > 0 || m_cPartialValue > 0;
								}

	EWC::CString						m_strName;
	EWC::CHash<EWC::CString, SAnchor>	m_mpStrAnc;				// map from a string name to the anchor value or type mapped to it
	EWC::CDynAry<CSTNode *>				m_aryPStnodManaged;		// stnodes for baked constants
	EWC::CDynAry<SLexerLocation>		m_aryLexlocSrc;			// lexer location where this was instantiated
	int									m_cPartialType;			// how many of the types mapped to will still be generic
	int									m_cPartialValue;		// how many mapped values are still generic
};

void PrintGenmap(CWorkspace * pWork, SGenericMap * pGenmap);
void PrintGenmapAnchors(EWC::SStringBuffer * pStrbuf, SGenericMap * pGenmap);
void PrintGenmapNoLocation(SGenericMap * pGenmap, const char * pChzLineEnd = "\n");

struct SInstantiateRequest // tag = insreq
{
								SInstantiateRequest()
								:m_pStnodGeneric(nullptr)
								,m_pSym(nullptr)
								,m_pGenmap(nullptr)
									{ ; }

	CSTNode * 					m_pStnodGeneric;	// AST for unsubstituted generic 
	SSymbol *					m_pSym;				// instantiated type, tin proc with resolved argument types
	SGenericMap * 				m_pGenmap;
};

class CGenericRegistry // tag = genreg
{
public:
	struct SEntry
	{
								SEntry()
								:m_pGenmapKey(nullptr)
								,m_pTin(nullptr)
								,m_pInsreq(nullptr)
									{ ; }

		SGenericMap *			m_pGenmapKey;	// genmap used as a key to lookup this entry 
		STypeInfo *				m_pTin;
		SInstantiateRequest *	m_pInsreq;
	};

	struct SEntryBlock	// tag = block
	{
								SEntryBlock (EWC::CAlloc * pAlloc)
								:m_aryEntry(pAlloc, EWC::BK_TypeCheckGenerics)
									{ ; }

		EWC::CDynAry<SEntry>	m_aryEntry;
	};

								CGenericRegistry(EWC::CAlloc * pAlloc)
								:m_pAlloc(pAlloc)
								,m_mpStnodInstFromBlock(pAlloc, EWC::BK_TypeCheckGenerics, 512)
								,m_aryInsreq(pAlloc, EWC::BK_TypeCheckGenerics, 128)
									{ ; }

								~CGenericRegistry()
								{
									Cleanup();
								}

	void						Cleanup();

	SEntry *					PEntryLookup(CSTNode * pStnodInstFrom, SGenericMap * pGenmap);
	SEntry *					PEntryEnsure(CSTNode * pStnodInstFrom, SGenericMap * pGenmap);
	SInstantiateRequest *		PInsreqNew(CSTNode * pStnodInstFrom, SGenericMap * pGenmap);

	EWC::CAlloc *							m_pAlloc;
	EWC::CHash<CSTNode *, SEntryBlock *>	m_mpStnodInstFromBlock;
	EWC::CDynAry<SInstantiateRequest>		m_aryInsreq;
};





// Find the name of all the anchors
void FindGenericAnchorNames( EWC::CAlloc * pAlloc, CSTNode * pStnodParamDecl, SGenericMap * pGenmap);

// find the subset of a generic map used by a given generic definition
SGenericMap * PGenmapTrimUnusedAnchors(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodDef,
	SGenericMap * pGenmapSuperset,
	SLexerLocation * pLexloc);

// Compute a generic map (values/types for all anchors)
ERRID ErridComputeDefinedGenerics( 
	STypeCheckWorkspace * pTcwork,
	ERREP errep,
	CSymbolTable * pSymtab,
	STypeInfo * pTinRef,
	CSTNode * pStnodDef,
	SGenericMap * pGenmap);

// Flatten
//   given SFoo($C) and partial where SFoo($C=$Dim) and passed canon where SFoo($C=2) compute SFoo($DIM=2)

// FindCanon
//   given SFoo($DIM=2) instantiated from SFoo($C=$DIM) compute SFoo($C=2)

#define CANONIZE_ON_SUBSTITUTE 1
// PTinSubstituteGenerics should always return a canonical type with a trimmed generic map
//  ie. the struct or proc derived from is the base type, not an instantiated partial type.


// given generic maps: partial($T=%X) and leaf($X=:s16) return flat($T=:s16)
#define USE_FLATTEN_GENMAP 0
#if USE_FLATTEN_GENMAP
ERRID ErridFlattenGenmap(
	STypeCheckWorkspace * pTcwork,
	ERREP errep,
	CSymbolTable * pSymtab,
	SGenericMap * pGenmapPartial,
	SGenericMap * pGenmapLeaf,
	SGenericMap  *pGenmapFlat);
#endif

STypeInfo * PTinFindCanon(STypeCheckWorkspace * pTcwork, STypeInfo * pTin, CSymbolTable * pSymtab, ERREP errep);
void AssertIsCanon(STypeInfo * pTin);

// Create a type by replacing the supplied type anchors, 
//  Note: resulting type can still contain generic anchors if not fully instantiated, or generic types are substituted.
//  Note: doesn't instantiate AST for generic type
STypeInfo * PTinSubstituteGenerics(
	STypeCheckWorkspace * pTcwork,
	CSymbolTable * pSymtab,
	SLexerLocation * pLexloc,
	STypeInfo * pTinUnsub,
	SGenericMap * pGenmap,
	ERREP errep);

// lookup a instantiation request, via a generic map and pStnod that defined the base.
SInstantiateRequest * PInsreqLookup(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodDefinition,
	SGenericMap * pGenmap,
	SLexerLocation * pLexloc);

STypeInfoStruct * PTinstructEnsureUniqueInstance(
	STypeCheckWorkspace * pTcwork,
	SLexerLocation * pLexloc,
	STypeInfoStruct * pTinstruct);

// instantiate symbol & AST for generic procedure
SInstantiateRequest * PInsreqInstantiateGenericProcedure(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGeneric,
	SGenericMap * pGenmap);

// instantiate symbol & AST for generic struct
SInstantiateRequest * PInsreqInstantiateGenericStruct(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGeneric,
	CSTNode * pStnodInstantiation,
	SGenericMap * pGenmap);

// actually do the walking to remap a syntax tree copy
void RemapGenericStnodCopy(
	STypeCheckWorkspace * pTcwork,
	CSTNode * pStnodGen,
	CSTNode * pStnodNew,
	SGenericMap * pGenmap,
	EWC::CHash<SSymbol *, SSymbol *> * pmpPSymGenericPSymRemapped,
	EWC::CHash<SSymbol *, CSTNode *> * pmpPSymSrcPStnodConstant,
	EWC::CHash<CSTNode *, CSTNode *> * pmpPStnodGenPStnodNew,
	CSymbolTable * pSymtabSrc,
	CSymbolTable * pSymtabNew);