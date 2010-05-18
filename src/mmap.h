#include <R.h>
#include <Rinternals.h>
#include "config.h"

#define MMAP_DATA(mmap_object)        R_ExternalPtrAddr(VECTOR_ELT(mmap_object,0))
#define MMAP_SIZE(mmap_object)        INTEGER(VECTOR_ELT(mmap_object,1))[0]
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
