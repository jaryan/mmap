#include <R.h>
#include <Rinternals.h>
#include "config.h"

#define MMAP_DATA(mmap_object)        R_ExternalPtrAddr(VECTOR_ELT(mmap_object,0))
/*#define MMAP_SIZE(mmap_object)        INTEGER(VECTOR_ELT(mmap_object,1))[0]*/
#define MMAP_SIZE(mmap_object)        (long)REAL(VECTOR_ELT(mmap_object,1))[0]
#define MMAP_FD(mmap_object)          INTEGER(VECTOR_ELT(mmap_object,2))[0]
#define MMAP_MODE(mmap_object)        TYPEOF(VECTOR_ELT(mmap_object,3))
#define MMAP_SMODE(mmap_object)       VECTOR_ELT(mmap_object,3)
#define MMAP_CBYTES(mmap_object)      INTEGER(getAttrib(VECTOR_ELT( \
                                        mmap_object,3),install("bytes")))[0]
#define MMAP_SIGNED(mmap_object)      INTEGER(getAttrib(VECTOR_ELT( \
                                        mmap_object,3),install("signed")))[0]
#define MMAP_OFFSET(mmap_object,i)    INTEGER(getAttrib(VECTOR_ELT( \
                                        mmap_object,3),install("offset")))[i]
#define MMAP_PAGESIZE(mmap_object)    INTEGER(VECTOR_ELT(mmap_object,3))[0]
#define MMAP_SYNC(mmap_object)        INTEGER(VECTOR_ELT(mmap_object,4))[0]

SEXP mmap_unmmap (SEXP mmap_obj);

#ifdef __ICC
/* ICC has no madvise in standard naming/place 
   patch based on one submitted by Daniel Cegielka */

#define MADV_NORMAL 0x0     /* default page-in behavior */
#define MADV_RANDOM 0x1     /* page-in minimum required */
#define MADV_SEQUENTIAL 0x2     /* read ahead aggressively */
#define MADV_WILLNEED 0x3     /* pre-fault pages */
#define MADV_DONTNEED 0x4     /* discard pages */

typedef char * caddr_t;

int madvise(void *start, size_t length, int advice) __THROW;
#endif
