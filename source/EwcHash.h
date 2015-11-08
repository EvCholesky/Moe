#ifndef EWC_HASH_GUARD
#define EWC_HASH_GUARD

// copyright (C) 2014 Evan Christensen

#include "EwcTypes.h"

namespace EWC
{


// hash based key -> value mapping
template <	typename K, 
			typename V, 
			int LOAD_FACTOR_PERCENT = 65> // hash grows when cUsed is greater that this percent of cCapacity
class CHash //tag=hash
{
protected:
	struct SEntry;
public:
	static const HV kHvUnused = 0xFFFFFFFF;
	static const HV kHvDeleted = 0xFFFFFFFE;
	typedef K KeyType;
	typedef V ValueType;

	class CIterator
	{
	public:
				CIterator(CHash<K, V, LOAD_FACTOR_PERCENT> * pHash)
				:m_pHash(pHash)
				,m_pEntry(nullptr)
					{ ; }

		V *		Next(K ** ppKey = nullptr) 
					{
						SEntry * pEntryEnd = &m_pHash->m_aEntry[m_pHash->m_cCapacity];
						m_pEntry = (m_pEntry) ? m_pEntry+1 : m_pHash->m_aEntry;

						// skip unused and deleted entries
						while (((m_pEntry->m_hv == kHvUnused)|(m_pEntry->m_hv == kHvDeleted)) & (m_pEntry != pEntryEnd))
							++m_pEntry;

						if (m_pEntry == pEntryEnd)
						{
							if (ppKey)
								*ppKey = nullptr;
							return nullptr;
						}
						
						if (ppKey)
							*ppKey = &m_pEntry->m_key;
						return &m_pEntry->m_value;
					}

		CHash<K, V, LOAD_FACTOR_PERCENT> *	m_pHash;
		SEntry *							m_pEntry;
	};

				CHash(CAlloc * pAlloc, U32 cCapacityStarting = 32)
				:m_pAlloc(pAlloc)
				,m_aEntry(nullptr)
				,m_cUsed(0)
				,m_cCapacity(0)
					{ SetAlloc(pAlloc, cCapacityStarting); }

				CHash()
				:m_pAlloc(nullptr)
				,m_aEntry(nullptr)
				,m_cUsed(0)
				,m_cCapacity(0)
					{ ; }

				~CHash()
					{ Clear(0); }

	void		SetAlloc(CAlloc * pAlloc, U32 cCapacityStarting = 32)
					{
						m_pAlloc = pAlloc;
						Grow(cCapacityStarting);
					}

	V *			Lookup(K key)
					{
						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted

						EWC_ASSERT(FIsPowerOfTwo(m_cCapacity), "cCapacity should be a power of two");
						U32 mask = m_cCapacity - 1;
						U32 iEntry = hv & mask;

						U32 cProbes = 0;
						while (cProbes < m_cCapacity)
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.FIsUnused())
							{
								return nullptr;
							}
							else if (entry.m_hv == hv)
							{
								EWC_ASSERT(key == entry.m_key, "hash collision! buy a lotto ticket!");
								return &entry.m_value;
							}
							iEntry = (iEntry + 1) & mask;
							++cProbes;
						}

						return nullptr;
					}

	void		Clear(U32 cCapacityNew = 32)
					{
						SEntry * pEntryEnd = &m_aEntry[m_cCapacity];
						for (SEntry * pEntry = m_aEntry; pEntry != pEntryEnd; ++pEntry)
						{
							if ((pEntry->m_hv == kHvUnused)|(pEntry->m_hv == kHvDeleted))
								continue;

							Destruct(&pEntry->m_key);
							Destruct(&pEntry->m_value);
						}
						m_pAlloc->EWC_FREE(m_aEntry);
						m_aEntry = nullptr;

						if (cCapacityNew)
							Grow(cCapacityNew);
					}

	void		Remove(K key)
					{
						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted

						EWC_ASSERT(FIsPowerOfTwo(m_cCapacity), "cCapacity should be a power of two");
						U32 mask = m_cCapacity - 1;
						U32 iEntry = hv & mask;

						U32 cProbes = 0;
						while (cProbes < m_cCapacity)
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.FIsUnused())
							{
								EWC_ASSERT(false, "Can't find key in hash (while trying to remove it)");
								return;
							}
							else if (entry.m_hv == hv)
							{
								EWC_ASSERT(key == entry.m_key, "hash collision! buy a lotto ticket!");
								Destruct(&entry.m_key);
								Destruct(&entry.m_value);
								entry.MarkAsDeleted();
								--m_cUsed;
								return;
							}
							iEntry = (iEntry + 1) & mask;
							++cProbes;
						}

					}

	FINS		FinsEnsureKey(K key, V ** ppValue = nullptr)
					{
						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						U32 mask = m_cCapacity - 1;
						U32 iEntry = hv & mask;
						SEntry * pEntryAvailable = nullptr;

						U32 cProbes = 0;	
						for(; cProbes < m_cCapacity; ++cProbes) // prevent infinite looping
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.m_hv == hv)
							{
								EWC_ASSERT(key == entry.m_key, "hash collision! buy a lotto ticket!");
								if (ppValue)
									*ppValue = &entry.m_value;
								return FINS_AlreadyExisted;
							}
							else if (entry.FIsUnusedOrDeleted())
							{
								if (!pEntryAvailable)
									pEntryAvailable = &entry;

								if (entry.FIsUnused())
								{
									break;
								}
							}

							iEntry = (iEntry + 1) & mask;
						}

						if (pEntryAvailable)
						{
							pEntryAvailable->m_key = key;
							Construct<V>(&pEntryAvailable->m_value);
							pEntryAvailable->m_hv = hv;
							++m_cUsed;

							if (m_cUsed * 100 >= m_cCapacity * LOAD_FACTOR_PERCENT)
							{
								I32 cUsedPrev = m_cUsed;
								I32 cCapacityPrev = m_cCapacity;
								Grow(m_cCapacity * 2);

								if (ppValue)
									*ppValue = Lookup(key);
								//EWC_ASSERT(*ppValue, "Hash ensure key failure");
							}
							else if (ppValue)
							{
								*ppValue = &pEntryAvailable->m_value;
								//EWC_ASSERT(*ppValue, "Hash ensure key failure");
							}

							return FINS_Inserted;
						}

						EWC_ASSERT(false, "CHash overflow");
						return FINS_Error;
					}

	FINS		FinsEnsureKeyAndValue(K key, V value)
					{
						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						U32 mask = m_cCapacity - 1;
						U32 iEntry = hv & mask;
						SEntry * pEntryAvailable = nullptr;

						for(U32 cProbes = 0; cProbes < m_cCapacity; ++cProbes) // prevent infinite looping
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.m_hv == hv)
							{
								EWC_ASSERT(key == entry.m_key, "hash collision! buy a lotto ticket!");
								// BB - should we check that entry.m_value == value?
								return FINS_AlreadyExisted;
							}
							else if (entry.FIsUnusedOrDeleted())
							{
								if (!pEntryAvailable)
									pEntryAvailable = &entry;

								if (entry.FIsUnused())
								{
									break;
								}
							}

							iEntry = (iEntry + 1) & (m_cCapacity - 1);
						}

						if (pEntryAvailable)
						{
							pEntryAvailable->m_key = key;
							pEntryAvailable->m_value = value;
							pEntryAvailable->m_hv = hv;
							++m_cUsed;

							if (m_cUsed * 100 >= m_cCapacity * LOAD_FACTOR_PERCENT)
								Grow(m_cCapacity * 2);
							return FINS_Inserted;
						}

						EWC_ASSERT(false, "CHash overflow");
						return FINS_Error;
					}
	
	FINS		FinsDebugEnsureKeyAndValueAfterDeleted(K key, V value, int cDeleted)
					{
						// simulate the effect of inserting multiple colliding entries then deleting all but the last one

						HV hv = HvExtract(key); // BB - need to clamp less than kHvDeleted
						HV hvDebug = hv + 1;
						U32 mask = m_cCapacity - 1;
						U32 iEntry = hv & mask;
						U32 iEntryStart = iEntry;;
								
						if (!EWC_FVERIFY((m_cUsed+1+cDeleted) * 100 < m_cCapacity * LOAD_FACTOR_PERCENT, "Debug insertion will cause rehash"))
							return FINS_Error;

						for (int iDeleted = 0; iDeleted < cDeleted; ++iDeleted)
						{
							for(U32 cProbes = 0; 1; ++cProbes) // prevent infinite looping
							{
								if (cProbes >= m_cCapacity)
								{
									EWC_ASSERT(false, "CHash overflow");
									return FINS_Error;
								}

								SEntry & entry = m_aEntry[iEntry];

								if (entry.FIsUnusedOrDeleted())
								{
									entry.m_hv = hvDebug;
									break;
								}

								iEntry = (iEntry + 1) & (m_cCapacity - 1);
							}
						}

						FINS finsReturn = FinsEnsureKeyAndValue(key, value);

						U32 iEntryEnd = iEntry;
						for (iEntry = iEntryStart; iEntry != iEntryEnd; ++iEntry)
						{
							SEntry & entry = m_aEntry[iEntry];
							if (entry.m_hv == hvDebug)
								entry.m_hv = kHvDeleted;
						}

						return finsReturn;
					}

	void		Grow(U32 cCapacityNew)
					{
						EWC_ASSERT(FIsPowerOfTwo(cCapacityNew), "invalid CHash capacity");

						SEntry * aEntryNew = (SEntry*)m_pAlloc->EWC_ALLOC(sizeof(SEntry) * cCapacityNew, EWC_ALIGN_OF(SEntry));
						ConstructN<SEntry>(aEntryNew, cCapacityNew);

						if (m_aEntry)
						{
							Rehash(m_aEntry, m_cCapacity, aEntryNew, cCapacityNew); 
							m_pAlloc->EWC_FREE(m_aEntry);
						}

						m_aEntry = aEntryNew;
						m_cCapacity = cCapacityNew;
					}

	void		Rehash(SEntry * aEntryOld, U32 cCapacityOld, SEntry * aEntryNew, U32 cCapacityNew)
					{
						U32 mask = cCapacityNew - 1;
						SEntry * pEntryEnd = &aEntryOld[cCapacityOld];
						for (SEntry * pEntryOld = aEntryOld; pEntryOld != pEntryEnd; ++pEntryOld)
						{
							// skip unused and deleted entries
							if ((pEntryOld->m_hv == kHvUnused)|(pEntryOld->m_hv == kHvDeleted))
								continue;

							U32 cProbes = 0;
							U32 iEntryNew = pEntryOld->m_hv & mask;
							for( ; cProbes < cCapacityNew; ++cProbes) // prevent infinite looping
							{
								SEntry & entryNew = aEntryNew[iEntryNew];
								if (entryNew.m_hv == pEntryOld->m_hv)
								{
									EWC_ASSERT(pEntryOld->m_key == entryNew.m_key, "hash collision! buy a lotto ticket!");
									EWC_ASSERT(false, "hash map currently doesn't support one-to-many mapping");
									break;
								}
								else if (entryNew.FIsUnusedOrDeleted())
								{
									entryNew.m_key = pEntryOld->m_key;
									entryNew.m_value = pEntryOld->m_value;
									entryNew.m_hv = pEntryOld->m_hv;
									break;
								}

								iEntryNew = (iEntryNew + 1) & (cCapacityNew - 1);
							}
							EWC_ASSERT(cProbes < cCapacityNew, "Rehash failure");
						}
					}

	CAlloc *	PAlloc()
					{ return m_pAlloc; } 
	U32			C()
					{ return m_cUsed; }
	U32			CCapacity()
					{ return m_cCapacity; }

protected:
	struct SEntry // tag=entry
	{
				SEntry()
				:m_hv(kHvUnused)
					{ ; }
		bool	FIsUnused()
					{ return m_hv == kHvUnused; }
		bool	FIsUnusedOrDeleted()
					{ return (m_hv == kHvUnused) | (m_hv == kHvDeleted); }
		void	MarkAsDeleted()
					{ m_hv = kHvDeleted; }

		HV	m_hv;
		K	m_key;
		V	m_value;
	};

	CAlloc *	m_pAlloc;
	SEntry *	m_aEntry;
	U32			m_cUsed;
	U32			m_cCapacity;
};
} // namespace EWC

#endif // EWC_HASH_GUARD