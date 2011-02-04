#include <R.h>
#include <Rinternals.h>
#include "config.h"

/*
  new access macros for environment mmap_obj to facilitate finalizer
*/

#define MMAP_DATA(mmap_object)        R_ExternalPtrAddr(findVar(install("data"),mmap_object))
#define MMAP_SIZE(mmap_object)        (long)REAL(findVar(install("bytes"),mmap_object))[0]
#define MMAP_FD(mmap_object)          INTEGER(findVar(install("filedesc"),mmap_object))[0]
#define MMAP_MODE(mmap_object)        TYPEOF(findVar(install("storage.mode"),mmap_object))
#define MMAP_SMODE(mmap_object)       findVar(install("storage.mode"),mmap_object)
#define MMAP_CBYTES(mmap_object)      INTEGER(getAttrib(findVar(install("storage.mode"), \
                                        mmap_object),install("bytes")))[0]
#define MMAP_SIGNED(mmap_object)      INTEGER(getAttrib(findVar(install("storage.mode"), \
                                        mmap_object),install("signed")))[0]
#define MMAP_OFFSET(mmap_object,i)      INTEGER(getAttrib(findVar(install("storage.mode"), \
                                        mmap_object),install("offset")))[i]
#define MMAP_PAGESIZE(mmap_object)    INTEGER(findVar(install("pagesize"),mmap_object))[0]
#define MMAP_SYNC(mmap_object)        INTEGER(VECTOR_ELT(mmap_object,4))[0]

/*
#define MMAP_DATA(mmap_object)        R_ExternalPtrAddr(VECTOR_ELT(mmap_object,0))
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
*/

#ifdef WIN32
/*#define MMAP_HANDLE(mmap_object)      INTEGER(VECTOR_ELT(mmap_object,5))[0]*/
#define MMAP_HANDLE(mmap_object)      INTEGER(findVar(install("handle"),mmap_object))[0]
/* Definitions from the Linux kernel source 2.6.35.7 */
#define PROT_READ       0x1             /* page can be read */
#define PROT_WRITE      0x2             /* page can be written */
#define PROT_EXEC       0x4             /* page can be executed */
#define PROT_SEM        0x8             /* page may be used for atomic ops */
#define PROT_NONE       0x0             /* page can not be accessed */
#define PROT_GROWSDOWN  0x01000000      /* mprotect flag: extend change to start of growsdown vma */
#define PROT_GROWSUP    0x02000000      /* mprotect flag: extend change to end of growsup vma */

#define MAP_SHARED      0x01            /* Share changes */
#define MAP_PRIVATE     0x02            /* Changes are private */
#define MAP_TYPE        0x0f            /* Mask for type of mapping */
#define MAP_FIXED       0x10            /* Interpret addr exactly */
#define MAP_ANONYMOUS   0x20            /* don't use a file */
#ifdef CONFIG_MMAP_ALLOW_UNINITIALIZED
# define MAP_UNINITIALIZED 0x4000000    /* For anonymous mmap, memory could be uninitialized */
#else
# define MAP_UNINITIALIZED 0x0          /* Don't support this flag */
#endif

#define MS_ASYNC        1               /* sync memory asynchronously */
#define MS_INVALIDATE   2               /* invalidate the caches */
#define MS_SYNC         4               /* synchronous memory sync */

#define MADV_NORMAL     0               /* no further special treatment */
#define MADV_RANDOM     1               /* expect random page references */
#define MADV_SEQUENTIAL 2               /* expect sequential page references */
#define MADV_WILLNEED   3               /* will need these pages */
#define MADV_DONTNEED   4               /* don't need these pages */

/* common parameters: try to keep these consistent across architectures */
#define MADV_REMOVE     9               /* remove these pages & resources */
#define MADV_DONTFORK   10              /* don't inherit across fork */
#define MADV_DOFORK     11              /* do inherit across fork */
#define MADV_HWPOISON   100             /* poison a page for testing */
#define MADV_SOFT_OFFLINE 101           /* soft offline page for testing */

#define MADV_MERGEABLE   12             /* KSM may merge identical pages */
#define MADV_UNMERGEABLE 13             /* KSM may not merge identical pages */
#endif

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
