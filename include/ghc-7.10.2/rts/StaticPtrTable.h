/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Initialization of the Static Pointer Table
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_STATICPTRTABLE_H
#define RTS_STATICPTRTABLE_H

/** Inserts an entry in the Static Pointer Table.
 *
 * The key is a fingerprint computed from the static pointer and the spe_closure
 * is a pointer to the closure defining the table entry.
 *
 * A stable pointer to the closure is made to prevent it from being garbage
 * collected while the entry exists on the table.
 *
 * This function is called from the code generated by
 * compiler/deSugar/StaticPtrTable.sptInitCode
 *
 * */
void hs_spt_insert (StgWord64 key[2],void* spe_closure);

/** Removes an entry from the Static Pointer Table.
 *
 * This function is called from the code generated by
 * compiler/deSugar/StaticPtrTable.sptInitCode
 *
 * */
void hs_spt_remove (StgWord64 key[2]);

#endif /* RTS_STATICPTRTABLE_H */