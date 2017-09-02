#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>


#include "mmap.h"

#ifndef WIN32
#include "config.h"
#ifdef HAVE_MMAP
#  include <sys/mman.h>
#endif
#else
#include <ctype.h>
#include <stdio.h>
#include <windows.h>
#undef HAVE_MADVISE
#endif

/*
The "mmap" package for R is designed to provide a
low level interface to the POSIX mmap C function
call.  Additional work has been done to make the
R interface friendly enough to the R user, but at
the same time transparent enough to allow for
the underlying system documentation to be used
to manage the memory map functionality.

This package implements all mmap-related calls:

  mmap
  munmap
  msync
  mprotect

The conversion mechnisms to deal with translating
raw bytes as returned by mmap into R level SEXP are
abstracted from the user but handled in the C code.
At present he may read data as R types: "raw", 
"integer", and "double".

This library does not support endianess conversion
yet, or all of the arguments to the underlying
system calls. The latter is due to the fact that
not all are mappable in a usable sense into the
R language, as well as the lack of need for this
level of control at the current package version.

Future work will entail support for more on-disk
types converted into R SEXP upon extraction, as well
as the addition of a smart finalizer.

Comments, criticisms, and concerns should be directed
to the maintainer of the package.
*/

/* initialize bitmask for bits() type {{{*/
int bitmask[32];
int nbitmask[32];

void create_bitmask (void){
  unsigned int i;
  /* little-endian for now */
  for(i=0; i<32; i++) {
     bitmask[i] = 1u << i;
    nbitmask[i] = ~bitmask[i];
  }
}

SEXP make_bitmask () {
  create_bitmask();
  return R_NilValue;
} /*}}}*/

/* mmap_mkFlags {{{ */
SEXP mmap_mkFlags (SEXP _flags) {
  char *cur_string;
  int len_flags = length(_flags);
  int flags_bit = 0x0;
  int i;

  for(i=0; i < len_flags; i++) {
    cur_string = (char *)CHAR(STRING_ELT(_flags,i));
    if(strcmp(cur_string,"PROT_READ")==0) {
      flags_bit = flags_bit | PROT_READ; continue;
    } else
    if(strcmp(cur_string,"PROT_WRITE")==0) {
      flags_bit = flags_bit | PROT_WRITE; continue;
    } else
    if(strcmp(cur_string,"PROT_EXEC")==0) {
      flags_bit = flags_bit | PROT_EXEC; continue;
    } else
    if(strcmp(cur_string,"PROT_NONE")==0) {
      flags_bit = flags_bit | PROT_NONE; continue;
    } else
    if(strcmp(cur_string,"MS_ASYNC")==0) {
      flags_bit = flags_bit | MS_ASYNC; continue;
    } else
    if(strcmp(cur_string,"MS_SYNC")==0) {
      flags_bit = flags_bit | MS_SYNC; continue;
    } else
    if(strcmp(cur_string,"MS_INVALIDATE")==0) {
      flags_bit = flags_bit | MS_INVALIDATE; continue;
    } else
    if(strcmp(cur_string,"MAP_SHARED")==0) {
      flags_bit = flags_bit | MAP_SHARED; continue;
    } else
    if(strcmp(cur_string,"MAP_PRIVATE")==0) {
      flags_bit = flags_bit | MAP_PRIVATE; continue;
    } else
    if(strcmp(cur_string,"MAP_FIXED")==0) {
      flags_bit = flags_bit | MAP_FIXED; continue;

#ifdef HAVE_MADVISE
    } else
    if(strcmp(cur_string,"MADV_NORMAL")==0) {
      flags_bit = flags_bit | MADV_NORMAL; continue;
    } else
    if(strcmp(cur_string,"MADV_RANDOM")==0) {
      flags_bit = flags_bit | MADV_RANDOM; continue;
    } else
    if(strcmp(cur_string,"MADV_SEQUENTIAL")==0) {
      flags_bit = flags_bit | MADV_SEQUENTIAL; continue;
    } else
    if(strcmp(cur_string,"MADV_WILLNEED")==0) {
      flags_bit = flags_bit | MADV_WILLNEED; continue;
    } else
    if(strcmp(cur_string,"MADV_DONTNEED")==0) {
      flags_bit = flags_bit | MADV_DONTNEED; continue;
#endif

    } else {
      warning("unknown constant: skipped");
    }
  }
#ifdef WIN32
  flags_bit = PAGE_READWRITE;
  if(flags_bit & PROT_READ & PROT_WRITE & MAP_SHARED)
    flags_bit = PAGE_READWRITE;
  else if(flags_bit & MAP_PRIVATE)
    flags_bit = PAGE_WRITECOPY;
  else if(flags_bit & PROT_READ)
    flags_bit = PAGE_READONLY;
#endif
  return ScalarInteger(flags_bit);
} /*}}}*/

/* mmap_munmap {{{ */
#ifdef WIN32
SEXP mmap_munmap (SEXP mmap_obj) {
  int ret;
  char *data = MMAP_DATA(mmap_obj);
  HANDLE fd = (HANDLE)(size_t)MMAP_FD(mmap_obj);
  HANDLE mh = (HANDLE)(size_t)MMAP_HANDLE(mmap_obj);

  if(data == NULL)
    error("invalid mmap pointer");

  ret = UnmapViewOfFile(data);
  CloseHandle(mh);
  CloseHandle(fd);
  R_ClearExternalPtr(findVar(mmap_dataSymbol,mmap_obj));
  return(ScalarInteger(ret));
}
#else
SEXP mmap_munmap (SEXP mmap_obj) {
  char *data = MMAP_DATA(mmap_obj);
  int fd = MMAP_FD(mmap_obj);

  if(data == NULL)
    error("invalid mmap pointer");

  int ret = munmap(data, MMAP_SIZE(mmap_obj));
  close(fd); /* should be moved back to R */
  //R_ClearExternalPtr(VECTOR_ELT(mmap_obj,0));
  R_ClearExternalPtr(findVar(mmap_dataSymbol,mmap_obj));
  /*R_ClearExternalPtr(MMAP_DATA(mmap_obj));*/
  return(ScalarInteger(ret)); 
} /*}}}*/

/*
void mmap_finalizer (SEXP mmap_obj) {
  Rprintf("mmap_finalizer called\n");
  mmap_munmap((SEXP)R_ExternalPtrAddr(mmap_obj));
}
*/
#endif

/* mmap_mmap AND mmap_finalizer {{{ */
#ifdef WIN32
SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _prot,
                SEXP _flags, SEXP _len, SEXP _off, SEXP _pageoff) {
  char *data;
  struct stat st;
  SYSTEM_INFO sSysInfo;
  GetSystemInfo(&sSysInfo);

  stat(CHAR(STRING_ELT(_fildesc,0)), &st);

  HANDLE hFile=CreateFile(CHAR(STRING_ELT(_fildesc,0)),
                  GENERIC_READ|GENERIC_WRITE,
                  FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,OPEN_EXISTING,0,NULL);

  HANDLE hMap=CreateFileMapping(hFile,NULL,PAGE_READWRITE,0,0,NULL);
  DWORD dwFileSize=GetFileSize(hFile,NULL);
  data = (char *)MapViewOfFile(hMap,FILE_MAP_WRITE,0,0,dwFileSize);
  /* advance ptr to byte offset from page boundary - shouldn't we do this above?? JR */
  data = data + asInteger(_off) + asInteger(_pageoff); 


  SEXP mmap_obj;
  PROTECT(mmap_obj = allocSExp(ENVSXP));
  SET_FRAME(mmap_obj, R_NilValue);
  SET_ENCLOS(mmap_obj, R_NilValue);
  SET_HASHTAB(mmap_obj, R_NilValue);
  SET_ATTRIB(mmap_obj, R_NilValue);
  defineVar(mmap_dataSymbol, R_MakeExternalPtr(data, R_NilValue, R_NilValue),mmap_obj);
  //defineVar(install("bytes"), ScalarReal(asReal(_len)-asInteger(_off)-asInteger(_pageoff)),mmap_obj);
  defineVar(mmap_bytesSymbol, _len,mmap_obj);
  defineVar(mmap_filedescSymbol, ScalarInteger((size_t)hFile),mmap_obj);
  defineVar(mmap_storageModeSymbol, _type,mmap_obj);
  defineVar(mmap_pagesizeSymbol, ScalarReal((double)sSysInfo.dwPageSize),mmap_obj);
  defineVar(mmap_handleSymbol, ScalarInteger((size_t)hMap),mmap_obj);
  defineVar(mmap_dimSymbol, R_NilValue, mmap_obj);
  defineVar(mmap_protSymbol, _prot, mmap_obj);
  defineVar(mmap_flagsSymbol, _flags, mmap_obj);
  UNPROTECT(1);
  return(mmap_obj);
}
#else
SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _prot,
                SEXP _flags, SEXP _len, SEXP _off, SEXP _pageoff) {
  int fd;
  char *data;
  struct stat st;

  stat(CHAR(STRING_ELT(_fildesc,0)), &st);
  fd = open(CHAR(STRING_ELT(_fildesc,0)), INTEGER(_prot)[0] == PROT_READ ? O_RDONLY : O_RDWR);
  if(fd < 0)
    error("unable to open file: possible permission issue.");
  data = mmap((caddr_t)0, 
              (size_t)REAL(_len)[0], 
              INTEGER(_prot)[0], 
              INTEGER(_flags)[0], 
              fd, 
              INTEGER(_off)[0]);

  if(data == MAP_FAILED)
    error("unable to mmap file");
  data = data + asInteger(_pageoff); /* advance ptr to byte offset from page boundary */
  
  SEXP mmap_obj;
  PROTECT(mmap_obj = allocSExp(ENVSXP));
  SET_FRAME(mmap_obj, R_NilValue);
  SET_ENCLOS(mmap_obj, R_NilValue);
  SET_HASHTAB(mmap_obj, R_NilValue);
  SET_ATTRIB(mmap_obj, R_NilValue);
  defineVar(mmap_dataSymbol, R_MakeExternalPtr(data, R_NilValue, R_NilValue),mmap_obj);
  //defineVar(install("bytes"), ScalarReal(asReal(_len)-asInteger(_off)-asInteger(_pageoff)),mmap_obj);
  defineVar(mmap_bytesSymbol, _len,mmap_obj);
  defineVar(mmap_filedescSymbol, ScalarInteger(fd),mmap_obj);
  defineVar(mmap_storageModeSymbol, _type,mmap_obj);
  defineVar(mmap_pagesizeSymbol, ScalarReal((double)sysconf(_SC_PAGE_SIZE)),mmap_obj);
  defineVar(mmap_dimSymbol, R_NilValue ,mmap_obj);
  defineVar(mmap_protSymbol,  _prot,  mmap_obj);
  defineVar(mmap_flagsSymbol, _flags, mmap_obj);
  UNPROTECT(1);
  return(mmap_obj);
} /*}}}*/
#endif

/* mmap_pagesize {{{ */
#ifdef WIN32
SEXP mmap_pagesize () {
  SYSTEM_INFO sSysInfo;
  GetSystemInfo(&sSysInfo);
  return ScalarInteger((int)sSysInfo.dwPageSize);
}
#else
SEXP mmap_pagesize () {
  return ScalarInteger((int)sysconf(_SC_PAGE_SIZE));
}
#endif
/*}}}*/

/* mmap_is_mmapped {{{ */
SEXP mmap_is_mmapped (SEXP mmap_obj) {
  char *data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    return(ScalarLogical(0));

  return(ScalarLogical(1));
} /*}}}*/

#ifdef WIN32
/* {{{ mmap_msync */
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  char *data;
  data = MMAP_DATA(mmap_obj);
  FlushViewOfFile((void *)data, (size_t)MMAP_SIZE(mmap_obj));
  return 0;
}
#else
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  char *data;
  data = MMAP_DATA(mmap_obj);
  int ret = msync(data, MMAP_SIZE(mmap_obj), INTEGER(_flags)[0]);
  return ScalarInteger(ret);
}/*}}}*/
#endif

/* {{{ mmap_madvise */
SEXP mmap_madvise (SEXP mmap_obj, SEXP _len, SEXP _flags) {
  /* function needs to allow for data to be an offset, else
     we can't control anything of value... */
#ifdef HAVE_MADVISE
  char *data;
  data = MMAP_DATA(mmap_obj);
  int ret = madvise(data, INTEGER(_len)[0], INTEGER(_flags)[0]);
#else
  int ret = -1;
#endif
  return ScalarInteger(ret);
}/*}}}*/

/* {{{ mmap_mprotect */
SEXP mmap_mprotect (SEXP mmap_obj, SEXP index, SEXP prot) {
  int i, LEN, ival;
  size_t upper_bound;
#ifndef WIN32
  char *data;
  char *addr;

  data = MMAP_DATA(mmap_obj);
  int pagesize = MMAP_PAGESIZE(mmap_obj);
#endif
  LEN = length(index);

  SEXP ret; PROTECT(ret = allocVector(INTSXP, LEN));
  
  upper_bound = (MMAP_SIZE(mmap_obj)-sizeof(int));
  for(i=0;i<LEN;i++) {
    ival = (INTEGER(index)[i]-1)*sizeof(int);
    if( ival > upper_bound || ival < 0 )
      error("'i=%i' out of bounds", i);
    
/* Rprintf("offset: %i\n",(ival/pagesize)*pagesize); */
#ifdef WIN32
    INTEGER(ret)[i] = -1;
#else
    addr = &(data[(int)((ival/pagesize)*pagesize)]);
    INTEGER(ret)[i] = mprotect(addr, ((ival/pagesize)*pagesize)*2, INTEGER(prot)[0]);
#endif
  }
  UNPROTECT(1);
  return ret;
}/*}}}*/

/* {{{ mmap_extract */
SEXP mmap_extract (SEXP index, SEXP field, SEXP dim, SEXP mmap_obj) {
/*SEXP mmap_extract (SEXP index, SEXP field, SEXP mmap_obj) {*/
  long v, fi, i, ii, ival;
  int P=0;
  unsigned char *data; /* unsigned int and values */

  /* 24 bit integers require a mask of the depending
     on whether the type is signed or unsigned */
  /*
  char *int24_buf[4],
       *uint24_buf[4];
  memset(int24_buf, 0, 4);
  memset(uint24_buf, 0xFF, 4); // used to convert overflow to negative
  */

  PROTECT(index = coerceVector(index,REALSXP)); P++;
  int LEN = length(index);  
  int mode = MMAP_MODE(mmap_obj);
  int Cbytes = MMAP_CBYTES(mmap_obj);
  int isSigned = MMAP_SIGNED(mmap_obj);

  /* types to hold memcpy of raw bytes to avoid punning */
  short sbuf;
  int intbuf;
  long longbuf;
  float floatbuf;
  double realbuf;
  Rcomplex Rcomplexbuf; 

  /*
  unsigned char *byte_buf;
  SEXP byteBuf;
  */
  int *int_dat;
  int *lgl_dat;
  double *real_dat;
  Rcomplex *complex_dat;
  unsigned char *raw_dat;

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  SEXP dat; /* dat is either a column, or list of columns */
  if(mode==VECSXP) {
    PROTECT(dat = allocVector(VECSXP, length(field)));
  } else PROTECT(dat = allocVector(mode,LEN));
  P++;

  double *index_p = REAL(index);
  long upper_bound;

  /* need R typed storage for structures... 
     ideally we needn't alloc for types
     that are not used --- move alloc to do that */
  int hasnul = 1;
  int fieldCbytes;
  int fieldSigned;
  int offset;

  SEXP vec_dat;  /* need all R types supported: INT/REAL/CPLX/RAW */
  int *int_vec_dat; 
  double *real_vec_dat;
  char *str;  /* temp store for string cp */
  char *str_buf = R_alloc(sizeof(char), Cbytes+1); /* len+\0 */ 
  Rcomplex *complex_vec_dat;
  Rbyte *raw_vec_dat;


  switch(mode) {
  case LGLSXP: /* {{{ */
    /* FIXME Need bound checking */
    if( strcmp(MMAP_CTYPE(mmap_obj), "bits") == 0) { /* bits */
      lgl_dat = LOGICAL(dat);
      for(i=0;  i < LEN; i++) {
        long which_word = (long) ((long)(index_p[i]-1)/32);
        memcpy(&intbuf, 
               &(data[which_word]),
               sizeof(char)*sizeof(int));
        lgl_dat[i] = intbuf;
        if(lgl_dat[i] & bitmask[ ((long)index_p[i]-1 )-(which_word*32) ])
          lgl_dat[i] = 1;
        else
          lgl_dat[i] = 0;
      }
    } else {
      lgl_dat = LOGICAL(dat);
      switch(Cbytes) {
        case sizeof(char): /* logi8 */
          for(i=0;  i < LEN; i++) {
            lgl_dat[i] = (int)(unsigned char)(data[((long)index_p[i]-1)]);
          }
          break;
        case sizeof(int): /* logi32 */
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, 
                   &(data[((long)index_p[i]-1)*sizeof(int)]),
                   sizeof(char)*sizeof(int));
            lgl_dat[i] = intbuf;
          }
          break;
        default:
          error("'logi' types must be either 8 or 32 bit");
          break;
      }
    }
    break; /* }}} */
  case INTSXP: /* {{{ */
    int_dat = INTEGER(dat);
    upper_bound = (MMAP_SIZE(mmap_obj)-Cbytes)/Cbytes;
    switch(Cbytes) {
      case 1: /* 1 byte (signed) char */
        if(isSigned) {
          for(i=0;  i < LEN; i++) {
            ival = ((long)index_p[i]-1);
            if( ival > upper_bound || ival < 0 ) {
              if( ival == 0 ) {
                continue;
              }
              error("'i=%i' out of bounds", index_p[i]);
            }
            int_dat[i] = (int)(char)(data[((long)index_p[i]-1)]);
          }
        } else { /* unsigned */
          for(i=0;  i < LEN; i++) {
            ival = ((long)index_p[i]-1);
            if( ival > upper_bound || ival < 0 ) {
              if( ival == 0 ) {
                continue;
              }
              error("'i=%i' out of bounds", index_p[i]);
             }
            int_dat[i] = (int)(unsigned char)(data[((long)index_p[i]-1)]);
          }
        }
        break;
      case 2: /* 2 byte short */
        if(isSigned) {
        /* 
        mmap_ushort_to_int(data, int_dat, index_p, LEN, upper_bound);
        */
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 ) {
            if( ival == 0 ) {
              continue;
            }
            error("'i=%i' out of bounds", index_p[i]);
          }
          memcpy(&sbuf, 
                 &(data[((long)index_p[i]-1)*sizeof(short)]),
                 sizeof(char)*sizeof(short));
          int_dat[i] = (int)sbuf;
        }
        } else {
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 ) {
            if( ival == 0 ) {
              continue;
            }
            error("'i=%i' out of bounds", index_p[i]);
          }
          memcpy(&sbuf,
                 &(data[((long)index_p[i]-1)*sizeof(short)]),
                 sizeof(char)*sizeof(short));
          int_dat[i] = (int)(unsigned short)sbuf;
        }  
        }
        break;
      case 3: /* 3 byte int */
        if(isSigned) {
        for(i=0;  i < LEN; i++) {
          ival =  ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 ) {
            if( ival == 0 ) {
              continue;
            }
            error("'i=%i' out of bounds", index_p[i]);
          }
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[((long)index_p[i]-1)*3]), /* copy first 3 bytes */
                 3);
          int_dat[i] = intbuf;
          if(int_dat[i] > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[((long)index_p[i]-1)*3]), // copy first 3 bytes
                   3);
            int_dat[i] = intbuf;
          }
        }
        } else { /* 3 byte unsigned */
        for(i=0;  i < LEN; i++) {
          ival =  ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 ) {
            if( ival == 0 ) {
              continue;
            }
            error("'i=%i' out of bounds", index_p[i]);
          }
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[((long)index_p[i]-1)*3]), /* copy first 3 bytes */
                 3);
          int_dat[i] = intbuf;
        }
        }
        break;
      case 4: /* 4 byte int */
        for(i=0;  i < LEN; i++) {
          ival =  ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 ) {
            if( ival == 0 ) {
              continue;
            }
            error("'i=%i' out of bounds", index_p[i]);
          }
          memcpy(&intbuf, 
                 &(data[((long)index_p[i]-1)*sizeof(int)]),
                 sizeof(char)*sizeof(int));
          int_dat[i] = intbuf;
        }
        break;
      default:
        error("unknown data type");
        break;
    }
    break; /* }}} */
  case REALSXP: /* {{{ */
    real_dat = REAL(dat);
    upper_bound = (MMAP_SIZE(mmap_obj)-Cbytes)/Cbytes;
    switch(Cbytes) {
      case 4: /* 4 byte float */
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(&floatbuf, 
                 &(data[((long)index_p[i]-1)*sizeof(float)]), 
                 sizeof(char)*sizeof(float));
          real_dat[i] = (double)floatbuf;
        }
        break;
      case 8: /* 8 byte double or (double)int64 */
        if( strcmp(MMAP_CTYPE(mmap_obj), "int64") == 0) {
          /* casting from int64 to R double to minimize precision loss */
          for(i=0;  i < LEN; i++) {
            ival = ((long)index_p[i]-1);
            if( ival > upper_bound || ival < 0 )
              error("'i=%i' out of bounds", (long)index_p[i]);
            memcpy(&longbuf, 
                   &(data[((long)index_p[i]-1)*sizeof(long)]), 
                   sizeof(char)*sizeof(long));
            real_dat[i] = (double)longbuf;
          }
        } else {
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(&realbuf, 
                 &(data[((long)index_p[i]-1)*sizeof(double)]), 
                 sizeof(char)*sizeof(double));
          real_dat[i] = realbuf;
        }
        }
        break;
      default:
        break;
    }
    break; /* }}} */
  case CPLXSXP: /* {{{ */
    complex_dat = COMPLEX(dat);
    upper_bound = (MMAP_SIZE(mmap_obj)-Cbytes)/Cbytes;
    for(i=0;  i < LEN; i++) {
      ival = ((long)index_p[i]-1);
      if( ival > upper_bound || ival < 0 )
        error("'i=%i' out of bounds", index_p[i]);
      memcpy(&Rcomplexbuf, 
             &(data[((long)index_p[i]-1)*sizeof(Rcomplex)]), 
             sizeof(char)*sizeof(Rcomplex));
      complex_dat[i] = Rcomplexbuf;
    }
    break; /* }}} */
  case STRSXP: /* {{{ */
    /* see https://svn.r-project.org/R/trunk/src/main/raw.c */
    /* fixed width character support */
    if( !isNull(getAttrib(MMAP_SMODE(mmap_obj),nul_Symbol)))
      hasnul = asLogical(getAttrib(MMAP_SMODE(mmap_obj),nul_Symbol));
    if(hasnul) { 
      for(i=0; i < LEN; i++) {
        str = (char *)(&(data[((long)index_p[i]-1)*Cbytes]));
        SET_STRING_ELT(dat, i,
          mkChar( (const char *)str));
          //mkChar( (const char *)&(data[((long)index_p[i]-1)*Cbytes]) ));
          //mkCharLenCE((const char *)&(data[((long)index_p[i]-1)*Cbytes]),
          //            Cbytes-1, CE_NATIVE));
          //mkCharLenCE((const char *)&(data[((long)index_p[i]-1)*Cbytes]),
                      //strlen(&(data[((long)index_p[i]-1)*Cbytes])) /*Cbytes*/, CE_NATIVE));
          //            (strlen(str) > Cbytes ? Cbytes : strlen(str))-1, CE_NATIVE));
      }
    } else {  /* nul-padded char array */
      for(i=0; i < LEN; i++) {
        str = (char *)(&(data[((long)index_p[i]-1)*Cbytes]));
        strncpy(str_buf, str, Cbytes);
        str_buf[Cbytes] = '\0';
        SET_STRING_ELT(dat, i, mkChar( (const char *)str_buf));
          //mkCharLenCE((const char *)&(data[((long)index_p[i]-1)*Cbytes]),
                      //strlen(&(data[((long)index_p[i]-1)*Cbytes])) /*Cbytes*/, CE_NATIVE));
         //             Cbytes, CE_NATIVE));
      }
    }
    break; /* }}} */
  case RAWSXP: /* {{{ */
    raw_dat = RAW(dat);
// differ in signess???
    upper_bound = (MMAP_SIZE(mmap_obj)-1);
    for(i=0;  i < LEN; i++) {
      ival =  ((long)index_p[i]-1);
      if( ival > upper_bound || ival < 0 )
        error("'i=%i' out of bounds", index_p[i]);
      raw_dat[i] = data[((long)index_p[i]-1)];
    }
    break; /* }}} */
  case VECSXP:  /* corresponds to C struct for mmap package {{{ */
    ////PROTECT(byteBuf = allocVector(RAWSXP,LEN*Cbytes)); P++;
    ////byte_buf = RAW(byteBuf);
    /* extract_struct:
      
       - bytes in struct from MMAP_CBYTES
       - loop through all `i` memcpy struct to byte array
       - loop through VECSXP;
           test for TYPEOF
           copy bytes by location into TYPEd array
       - collect arrays into VECSXP dat
    */
    ////for(i=0; i<LEN; i++) {
      /* byte_buf (byteBuf) now has all bytes for the resulting struct
         copied from the current data.

         This means that we'll have two copies of data in memory
         for the duration of the call. It would be better if we
         could release/resize this as we go, but for now this is
         simple and effective.

         Is this even needed?  Seems like our loops below handle the copies... JR
      */
      ////memcpy(&(byte_buf[i*Cbytes]),
      ////       &(data[((long)index_p[i]-1) * Cbytes]),
      ////       Cbytes); 
    ////}  
    for(fi=0; fi<length(field); fi++) {
      v = INTEGER(field)[fi]-1;
      offset = MMAP_OFFSET(mmap_obj,v);
      fieldCbytes = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),
                                      v),mmap_bytesSymbol))[0];
      fieldSigned = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),
                                      v),mmap_signedSymbol))[0];
      switch(TYPEOF(VECTOR_ELT(MMAP_SMODE(mmap_obj), v))) {
        case INTSXP:
          PROTECT(vec_dat = allocVector(INTSXP, LEN)); 
          int_vec_dat = INTEGER(vec_dat);
          switch(fieldCbytes) {
            case sizeof(char):
            if(fieldSigned) {   /* 1 byte char */
            for(ii=0; ii<LEN; ii++) {
              //int_vec_dat[ii] = (int)(char)(byte_buf[ii*Cbytes+offset]);;
              int_vec_dat[ii] = (int)(char)(data[((long)index_p[ii]-1) * Cbytes + offset]);
            }
            } else {            /* 1 byte unsigned char */
            for(ii=0; ii<LEN; ii++) {
              //int_vec_dat[ii] = (int)(unsigned char)(byte_buf[ii*Cbytes+offset]);;
              int_vec_dat[ii] = (int)(unsigned char)(data[((long)index_p[ii]-1) * Cbytes + offset]);
            }
            }
            break;
            case sizeof(short):
            if(fieldSigned) {   /* 2 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(&sbuf, 
                     &(data[((long)index_p[ii]-1) * Cbytes + offset]),
                     sizeof(char)*sizeof(short));
              int_vec_dat[ii] = (int)sbuf;
            }
            } else {            /* 2 byte unsigned short */
            for(ii=0; ii<LEN; ii++) {
              memcpy(&sbuf, 
                     //&(byte_buf[ii*Cbytes+offset]),
                     &(data[((long)index_p[ii]-1) * Cbytes + offset]),
                     sizeof(char)*sizeof(short));
              int_vec_dat[ii] = (int)(unsigned short)sbuf;
            }
            }
            break;
            /*
            case sizeof( three_byte_int )
            */
            case sizeof(int): /* 4 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(&intbuf, 
                     &(data[((long)index_p[ii]-1) * Cbytes+offset]),
                     //&(byte_buf[ii*Cbytes+offset]),
                     sizeof(char)*sizeof(int));
              int_vec_dat[ii] = intbuf;
            }
            break;
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          UNPROTECT(1);
          break;
        case REALSXP:
          PROTECT(vec_dat = allocVector(REALSXP, LEN));
          real_vec_dat = REAL(vec_dat);
          switch(fieldCbytes) {
            case sizeof(float): /* 4 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(&floatbuf, 
                     //&(byte_buf[ii*Cbytes+offset]),
                     &(data[((long)index_p[ii]-1) * Cbytes + offset]),
                     sizeof(char)*sizeof(float));
              real_vec_dat[ii] = (double)floatbuf;
            }
            break;
            case sizeof(double): /* 8 byte */
            if( strcmp(CHAR(STRING_ELT(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj), v),
                                                 R_ClassSymbol),1)),"int64") == 0) { 
              /* casting from int64 to R double to minimize precision loss */
              for(ii=0;  ii < LEN; ii++) {
                memcpy(&longbuf, 
                       //&(byte_buf[ii*Cbytes+offset]),
                       &(data[((long)index_p[ii]-1) * Cbytes + offset]),
                       sizeof(char)*sizeof(long));
                real_vec_dat[ii] = (double)longbuf;
              }
            } else {
            for(ii=0; ii<LEN; ii++) {
              memcpy(&realbuf, 
                     //&(byte_buf[ii*Cbytes+offset]),
                     &(data[((long)index_p[ii]-1) * Cbytes + offset]),
                     sizeof(char)*sizeof(double));
              real_vec_dat[ii] = (double)realbuf;
            }
            }
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          UNPROTECT(1);
          break;
        case CPLXSXP:
          PROTECT(vec_dat = allocVector(CPLXSXP, LEN));
          complex_vec_dat = COMPLEX(vec_dat);
          for(ii=0; ii<LEN; ii++) {
            memcpy(&Rcomplexbuf, 
                   //&(byte_buf[ii*Cbytes+offset]),
                   &(data[((long)index_p[ii]-1) * Cbytes + offset]),
                   sizeof(char)*sizeof(Rcomplex));
            complex_vec_dat[ii] = Rcomplexbuf;
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          UNPROTECT(1);
          break;
        case RAWSXP: 
          PROTECT(vec_dat = allocVector(RAWSXP, LEN));
          raw_vec_dat = RAW(vec_dat);
          for(ii=0; ii<LEN; ii++) {
            //raw_vec_dat[ii] = (Rbyte)(byte_buf[ii*Cbytes+offset]);;
            raw_vec_dat[ii] = (Rbyte)(data[((long)index_p[ii]-1) * Cbytes + offset]);
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          UNPROTECT(1);
          break;
        case STRSXP:
          hasnul = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),
                                      v),nul_Symbol))[0];
          PROTECT(vec_dat = allocVector(STRSXP, LEN));
          if(hasnul) {
            for(ii=0; ii < LEN; ii++) {
              //str = (char *)&(byte_buf[ii*Cbytes+offset]);
              str = (char *)&( data[((long)index_p[ii]-1) * Cbytes + offset]);
              SET_STRING_ELT(vec_dat, ii,
                mkChar( (const char *)str));
                //mkCharLenCE((const char *)&(byte_buf[ii*Cbytes+offset]),
                //          fieldCbytes-1, CE_NATIVE));
            }
          } else {  /* nul-padded char array */
            for(ii=0; ii < LEN; ii++) {
              //str = (char *)&(byte_buf[ii*Cbytes+offset]);
              str = (char *)&(data[((long)index_p[ii]-1) * Cbytes + offset]);
              SET_STRING_ELT(vec_dat, ii,
                mkCharLenCE((const char *)&(data[((long)index_p[ii]-1) * Cbytes + offset]), //byte_buf[ii*Cbytes+offset]),
                      //    fieldCbytes, CE_NATIVE));
                      (strlen(str) > fieldCbytes ? fieldCbytes : strlen(str)), CE_NATIVE));
            }
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          UNPROTECT(1);
          break; 
        default:
          error("unimplemented type");
          break;
      }
    } /* }}} */
    break;
  default:
    error("unsupported type");
    break;
  }
  if( !isNull(dim))
    setAttrib(dat, R_DimSymbol, dim);
  UNPROTECT(P);
  return dat;
}/*}}}*/

/* mmap_replace {{{ */
SEXP mmap_replace (SEXP index, SEXP field, SEXP value, SEXP mmap_obj) {
/*  int i, upper_bound, ival; */
  int i, ival;
  size_t upper_bound; //, ival;
  int v, fi, offset, fieldCbytes, fieldSigned;
  char *data;
  int LEN = length(index);  
  int mode = MMAP_MODE(mmap_obj);
  int Cbytes = MMAP_CBYTES(mmap_obj);
  int prot = asInteger(MMAP_PROT(mmap_obj));
  int flags = asInteger(MMAP_FLAGS(mmap_obj));
/*Rprintf("prot: %i, flags: %i\n", prot, flags);*/
  if(prot == PROT_READ && flags != MAP_PRIVATE)
    error("object was opened with PROT_READ and is not writable");
  //int hasnul = 1;
  /*int isSigned = MMAP_SIGNED(mmap_obj);*/
  int P=0;

  if((data = MMAP_DATA(mmap_obj)) == NULL)
    error("invalid mmap pointer");

  int           *int_value,
                *lgl_value;
  double        *real_value;
  unsigned char *byte_value;
  Rcomplex      *complex_value;
  float         float_value;
  short         short_value;
  long          long_value;
  char          char_value;
  SEXP          string_value;

  if(mode != VECSXP) {
    PROTECT(value = coerceVector(value, mode)); P++;
  }
  PROTECT(index = coerceVector(index, REALSXP) ); P++;
  PROTECT(field = coerceVector(field, INTSXP) ); P++;
  double *index_p = REAL(index);
  int new_word, int_buf;
  long which_word;
  switch(mode) {
  case LGLSXP:
    lgl_value = LOGICAL(value);
    upper_bound = (MMAP_SIZE(mmap_obj)-Cbytes); 
    if( strcmp(MMAP_CTYPE(mmap_obj), "bits") == 0) {  /* bits() */
      for(i=0; i < LEN; i++) {
        which_word = (long) (((long)index_p[i]-1)/32);
        memcpy(&int_buf, &(data[which_word]), sizeof(int));
        if(lgl_value[i])
          new_word = int_buf | bitmask[ ((long)index_p[i]-1)-(which_word*32) ];
        else
          new_word = int_buf & nbitmask[ ((long)index_p[i]-1)-(which_word*32) ];
        memcpy(&(data[which_word]), &(new_word), sizeof(int));
//Rprintf("i: %i\twhich_word: %i\tnew_word%i\n", i, which_word, new_word);
      }
    } else {
    switch(Cbytes) {
      case sizeof(char): /* logi8 */
        for(i=0; i < LEN; i++) {
          ival = ((long)index_p[i]-1)*sizeof(char);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", (long)index_p[i]);
          char_value = (unsigned char)(lgl_value[i]);
          memcpy(&(data[((long)index_p[i]-1)*sizeof(char)]), &(char_value), sizeof(char));
        }
        break;
      case sizeof(int): /* logi32 */
        for(i=0; i < LEN; i++) {
          ival = ((long)index_p[i]-1)*sizeof(int);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          /* endianess issues are here -- FIXME */
          memcpy(&(data[((long)index_p[i]-1)*sizeof(int)]), &(lgl_value[i]), sizeof(int));
        }
        break;
      default:
        error("'logi' types must either 8 or 32 bit");
        break;
    } 
    }
    break;
  case INTSXP: /* {{{ */
    int_value = INTEGER(value);
    upper_bound = (MMAP_SIZE(mmap_obj)-Cbytes); 
    switch(Cbytes) {
      case sizeof(char): /* 1 byte char */
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1)*sizeof(char);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", (long)index_p[i]);
          char_value = (unsigned char)(int_value[i]); 
          memcpy(&(data[((long)index_p[i]-1)*sizeof(char)]), &(char_value), sizeof(char));
        }
      break;
      case sizeof(short): /* 2 byte short */
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1)*sizeof(short);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          short_value = (unsigned short)(int_value[i]); 
          memcpy(&(data[((long)index_p[i]-1)*sizeof(short)]), &(short_value), sizeof(short));
        }
      break;
      case 3: /* case 3 byte */
      for(i=0;  i < LEN; i++) {
        ival = ((long)index_p[i]-1)*3;
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", index_p[i]);
        memcpy(&(data[((long)index_p[i]-1)*3]), &(int_value[i]), 3);
      }
      break;
      case sizeof(int): /* 4 byte int */
        for(i=0;  i < LEN; i++) {
          ival = ((long)index_p[i]-1)*sizeof(int);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(&(data[((long)index_p[i]-1)*sizeof(int)]), &(int_value[i]), sizeof(int));
        }
        break;
    }
    break; /* }}} */
  case REALSXP: /* {{{ */
    real_value = REAL(value);
    upper_bound = (MMAP_SIZE(mmap_obj)-Cbytes);
    switch(Cbytes) {
      case sizeof(float): /* 4 byte float */
      for(i=0;  i < LEN; i++) {
        ival =  ((long)index_p[i]-1)*sizeof(float);
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", i);
        float_value = (float)(real_value[i]);
        memcpy(&(data[((long)index_p[i]-1)*sizeof(float)]), &(float_value), sizeof(float));
      }
      break;
      case sizeof(double): /* 8 byte double */
      if( strcmp(MMAP_CTYPE(mmap_obj), "int64") == 0) { /* stored as long */
      for(i=0;  i < LEN; i++) {
        ival =  ((long)index_p[i]-1)*sizeof(double);
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", i);
        long_value = (long)(real_value[i]);
        memcpy(&(data[((long)index_p[i]-1)*sizeof(long)]), &(long_value), sizeof(long));
      }
      } else {
      for(i=0;  i < LEN; i++) {
        ival =  ((long)index_p[i]-1)*sizeof(double);
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", i);
        memcpy(&(data[((long)index_p[i]-1)*sizeof(double)]), &(real_value[i]), sizeof(double));
      }
      }
      break;
    }
    break; /* }}} */
  case VECSXP: /* aka "struct"{{{ */
    if(length(value) != length(field))
      error("size of struct and size of replacement value do not match");
    for(fi=0; fi<length(field); fi++) {
      v = INTEGER(field)[fi]-1;
      offset = MMAP_OFFSET(mmap_obj, v);  /* byte offset of column */
      fieldCbytes = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),v),
                                      mmap_bytesSymbol))[0];
      fieldSigned = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),v),
                                      mmap_signedSymbol))[0];
      switch(TYPEOF(VECTOR_ELT(MMAP_SMODE(mmap_obj),v))) {
        case INTSXP:
          LEN = length(VECTOR_ELT(value,fi));
          /* make sure we have an int for value */
          if(TYPEOF(VECTOR_ELT(value, fi)) != INTSXP)
            int_value = INTEGER(coerceVector(VECTOR_ELT(value, fi), INTSXP));
          else int_value = INTEGER(VECTOR_ELT(value, fi));

          switch(fieldCbytes) {
            case sizeof(char): /* 1 byte char */
            for(i=0;  i < LEN; i++) {
              /*
              ival = (index_p[i]-1)*sizeof(char);
              if( ival > upper_bound || ival < 0 )
                error("'i=%i' out of bounds", index_p[i]);
              */
              char_value = (unsigned char)(int_value[i]); 
              memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]), 
                     &(char_value), 
                     fieldCbytes);
            }
            break;
            case sizeof(short):
            if(fieldSigned) {
              for(i=0; i < LEN; i++) {
                short_value = (short)(int_value[i]);
                memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]),
                       &short_value,
                       fieldCbytes);
              }
            } else {
              for(i=0; i < LEN; i++) {
                short_value = (unsigned short)(int_value[i]);
                memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]),
                       &short_value,
                       fieldCbytes);
              }
            }
            break;
            case sizeof(int):
              for(i=0; i < LEN; i++) {
                memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]),
                       &(int_value[i]),
                       sizeof(int));
              }
            break;
          }
          break;
        case REALSXP:
          LEN = length(VECTOR_ELT(value,fi));
          if(TYPEOF(VECTOR_ELT(value, fi)) != REALSXP)
            real_value = REAL(coerceVector(VECTOR_ELT(value, fi), REALSXP));
          else real_value = REAL(VECTOR_ELT(value, fi));
          switch(fieldCbytes) {
            case sizeof(float):
            for(i=0; i < LEN; i++) {
              float_value = (float)(real_value[i]);
              memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]),
                     &float_value,
                     sizeof(float));
            }
            break;
          case sizeof(double):
            for(i=0; i < LEN; i++) {
              memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]),
                     /*&(REAL(VECTOR_ELT(value,v))[i]),*/
                     &(real_value[i]),
                     sizeof(double));
            }
            break;
          }
          break;
        case RAWSXP:
          LEN = length(VECTOR_ELT(value,fi));
          /* make sure we have an raw for value */
          if(TYPEOF(VECTOR_ELT(value, fi)) != RAWSXP)
            byte_value = RAW(coerceVector(VECTOR_ELT(value, fi), RAWSXP));
          else byte_value = RAW(VECTOR_ELT(value, fi));

          for(i=0;  i < LEN; i++) {
              /*
              ival = (index_p[i]-1)*sizeof(char);
              if( ival > upper_bound || ival < 0 )
                error("'i=%i' out of bounds", index_p[i]);
              */
            char_value = (unsigned char)(byte_value[i]); 
            memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]), 
                   &(char_value), 
                   fieldCbytes);
          }
          break;
        case STRSXP:
          LEN = length(VECTOR_ELT(value, fi));
          PROTECT(string_value = VECTOR_ELT(value, fi));
          for(i=0; i < LEN; i++) {
            memset(&(data[((long)index_p[i]-1)*Cbytes+offset]), '\0', fieldCbytes);  /* clear fixed width */
            memcpy(&(data[((long)index_p[i]-1)*Cbytes+offset]), 
                   CHAR(STRING_ELT(string_value, i)), 
                   fieldCbytes);
          }
          UNPROTECT(1);
          break;
        default:
          error("unimplemented replacement type");
          break;
      }
    } /* VECSXP }}} */
    break;
  case STRSXP:
    if( !isNull(getAttrib(MMAP_SMODE(mmap_obj),nul_Symbol)) &&
        asLogical(getAttrib(MMAP_SMODE(mmap_obj),nul_Symbol))) {
      for(i=0; i < LEN; i++) {
        memset(&(data[((long)index_p[i]-1)*Cbytes]), '\0', Cbytes);
        memcpy(&(data[((long)index_p[i]-1)*Cbytes]), CHAR(STRING_ELT(value,i)), Cbytes-1);
      }
    } else {
      for(i=0; i < LEN; i++) {
        memset(&(data[((long)index_p[i]-1)*Cbytes]), '\0', Cbytes);
        memcpy(&(data[((long)index_p[i]-1)*Cbytes]), CHAR(STRING_ELT(value,i)), Cbytes);
      }
    }
    /*
    } else {
      for(i=0; i < LEN; i++) {
        memcpy(&(data[(index_p[i]-1)*Cbytes]), CHAR(STRING_ELT(value,i)), Cbytes-1);
      }
    }
    */
    break;
  case RAWSXP:
    byte_value = RAW(value);
    for(i=0; i < LEN; i++) {
      memcpy(&(data[((long)index_p[i]-1)]), &(byte_value[i]), Cbytes);
    }
    break;
  case CPLXSXP:
    complex_value = COMPLEX(value);
    for(i=0; i < LEN; i++) {
      memcpy(&(data[((long)index_p[i]-1)*sizeof(Rcomplex)]), &(complex_value[i]), sizeof(Rcomplex));
    }
    break;
  default:
    error("unsupported type");
    break;
  }
  UNPROTECT(P);
  return R_NilValue;
} /*}}}*/

/* {{{ mmap_compare */
SEXP mmap_compare (SEXP compare_to, SEXP compare_how, SEXP mmap_obj) {
  int i;
  char *data;

  unsigned char charbuf;
  int intbuf;
  short shortbuf;
  float floatbuf;
  double realbuf;

  /* 24 bit integers require a mask of the depending
     on whether the type is signed or unsigned */
  char *int24_buf[4],
       *uint24_buf[4];  
  memset(int24_buf, 0, 4); // 0
  memset(uint24_buf, 0xFF, 4);  // -1

  long LEN;
  int mode = MMAP_MODE(mmap_obj); 
  int Cbytes = MMAP_CBYTES(mmap_obj);
  int isSigned = MMAP_SIGNED(mmap_obj);

  SEXP result;
  LEN = (long)(MMAP_SIZE(mmap_obj)/Cbytes);  /* change to REAL */
  PROTECT(result = allocVector(INTSXP, LEN));
  int *int_result = INTEGER(result);

  /* comp_how
     1  ==
     2  !=
     3  >=
     4  <=
     5  >
     6  < */
  int cmp_how = INTEGER(compare_how)[0];

  //int *int_dat;
  //double *real_dat;
  //char *cmp_str;
  int cmp_len;
  int hits=0;
  int cmp_to_int;
  double cmp_to_real;
  unsigned char * cmp_to_raw;

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  switch(mode) {
  case LGLSXP:
    cmp_to_int = INTEGER(coerceVector(compare_to,INTSXP))[0];
    /* bits, lgl8, lgl32 */
    switch(Cbytes) {
      case sizeof(char):
        if(cmp_how==1) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]),sizeof(char));
            if(cmp_to_int == (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==2) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int != (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==3) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int <= (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==4) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int >= (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==5) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int <  (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==6) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int >  (int)charbuf)
              int_result[hits++] = i+1;
          }
        }
        break;
      case sizeof(int):
        if(cmp_how==1) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int == intbuf) 
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==2) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int != intbuf) 
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==3) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int <= intbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==4) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int >= intbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==5) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int <  intbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==6) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int >  intbuf)
              int_result[hits++] = i+1;
          }
        }
        break;
      default:
        error("unimplemented type in comparison");
        break;
    }
    break;
  case INTSXP:
    cmp_to_int = INTEGER(coerceVector(compare_to,INTSXP))[0];
    /* needs to branch for
        uint8, int8, uint16, int16, uint24, int24, int32 
        FIXME int64 (in REALSXP branch)
    */
    switch(Cbytes) {
    case 1: /* char int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]),sizeof(char));
          if(cmp_to_int == (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int != (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <  (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >  (int)charbuf)
            int_result[hits++] = i+1;
        }
      }
      } else { /* unsigned char */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]),sizeof(char));
          if(cmp_to_int == (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int != (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <  (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >  (int)charbuf)
            int_result[hits++] = i+1;
        }
      } /* end of uchar */
      }
      break; /* }}} */
    case 2: /* short int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        cmp_to_int = (short)cmp_to_int;
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]),sizeof(short));
          //if(cmp_to_int == (int)(short)*(short *)(short_buf)) 
          if(cmp_to_int == shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int != shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <= shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >= shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <  shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >  shortbuf)
            int_result[hits++] = i+1;
        }
      }
      } else { /* unsigned short */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]),sizeof(short));
          if(cmp_to_int == (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int != (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <= (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >= (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <  (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >  (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } /* end of ushort */
      }
      break; /* }}} */
    case 3: /* 3 byte int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(intbuf > 8388607) {
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int == intbuf)
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int == intbuf)
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(intbuf > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int != intbuf)
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int != intbuf)
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(intbuf > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int <= intbuf)
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int <= intbuf)
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(intbuf > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int >= intbuf)
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int >= intbuf)
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(intbuf > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int < intbuf)
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int < intbuf)
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(intbuf > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            intbuf = -1;
            memcpy(&intbuf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int > intbuf)
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int > intbuf)
              int_result[hits++] = i+1;
          }
        }
      }
      } else { /* unsigned 3 byte int */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int == intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int != intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int <= intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int >= intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int <  intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          intbuf = 0;
          memcpy(&intbuf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int >  intbuf)
            int_result[hits++] = i+1;
        }
      } /* end of unsigned 3 byte */
      }
      break; /* }}} */
    case 4: /* int {{{ */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          //if(cmp_to_int == *((int *)(void *)&int_buf)) 
          if(cmp_to_int == intbuf) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int != intbuf) //*((int *)(void *)&int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int <= intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int >= intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int <  intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int >  intbuf)
            int_result[hits++] = i+1;
        }
      }
      break;
    default: /* }}} */
      error("unsupported integer type");
    }
    break;
  case REALSXP:
    /* NA handling is missing .. how is this to behave? 
       Likely should test for compare_to as well as on-disk
       values.
    */
    cmp_to_real = REAL(coerceVector(compare_to,REALSXP))[0];
    switch(Cbytes) {
    case sizeof(float):
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real == (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real != (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real <= (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real >= (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real <  (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real >  (double)floatbuf)
            int_result[hits++] = i+1;
        }
      }
      break;
    case sizeof(double):
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real == realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real != realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real <= realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real >= realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real <  realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real >  realbuf)
            int_result[hits++] = i+1;
        }
      }
      break;
    default:
      error("unknown floating point size");
      break;
    }
/*
    for(i=0;  i < LEN; i++) {
      memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(char) * sizeof(double));
    }
*/
    break;
  case RAWSXP:
    for(i=0;  i < LEN; i++) {
      warning("unimplemented raw comparisons"); 
    }
    break;
  case STRSXP: /* {{{ */
    /* see https://svn.r-project.org/R/trunk/src/main/raw.c */
    /* fixed width character support */
    /*
    if(length(compare_to) > isNull(getAttrib(MMAP_SMODE(mmap_obj),install("nul"))) ? Cbytes-1 : Cbytes) {
      if(isNull(getAttrib(compare_how, install("partial")))) { 
        UNPROTECT(1); return allocVector(INTSXP,0);
      }  
      warning("only first %i characters of string compared", Cbytes-1);
    }
    */
    cmp_len = length(compare_to);
    cmp_to_raw = RAW(compare_to);
    char *str;
    int str_len;
    char *str_buf = R_alloc(sizeof(char), Cbytes);
    int hasnul = 1;
    if( !isNull(getAttrib(MMAP_SMODE(mmap_obj), nul_Symbol)))
      hasnul = asLogical(getAttrib(MMAP_SMODE(mmap_obj),nul_Symbol));

    //if(isNull(getAttrib(MMAP_SMODE(mmap_obj),install("nul")))) {
    if(hasnul) {
      for(i=0; i < LEN; i++) {
        str = &(data[i*Cbytes]);
        //strncpy(str_buf, str, Cbytes);
        //str_len = strlen(str_buf);
        //str_len = (str_len > Cbytes) ? Cbytes : str_len;
        //Rprintf("strnlen(str,6):%i\n",strnlen(str,6));
        //if(str_len != cmp_len)
          //continue;
        if(memcmp(str,cmp_to_raw,cmp_len)==0)
          int_result[hits++] = i+1;
      }
//      for(i=0; i < LEN; i++) {
//          //for(b=0; b < Cbytes-1; b++) {
//          for(b=0; b < cmp_len; b++) {
//            Rprintf("%c == %c,", cmp_to_raw[b], data[i*Cbytes+b]);
//            if(cmp_to_raw[b] != data[i*Cbytes + b])
//              break;
//          }
//          Rprintf("\n");
//          if(b == Cbytes-1)
//            int_result[hits++] = i+1;
//      }
    } else {
      for(i=0; i < LEN; i++) {
        str = &(data[i*Cbytes]);
        strncpy(str_buf, str, Cbytes);
        str_len = strlen(str_buf);
        str_len = (str_len > Cbytes) ? Cbytes : str_len;
        //Rprintf("strnlen(str,6):%i\n",strnlen(str,6));
        if(str_len != cmp_len)
          continue;
        if(memcmp(str,cmp_to_raw,cmp_len)==0)
          int_result[hits++] = i+1;
      }
    }
    break; /* }}} */
  case CPLXSXP:
  default:
    error("unsupported type");
    break;
  }
  result = lengthgets(result, hits);
  UNPROTECT(1);
  return result;
  return ScalarInteger(hits);
}/*}}}*/

SEXP convert_ij_to_i (SEXP rows, SEXP i, SEXP j) {
  /* utility to take i,j subsets for matrix objects and
     convert to subset column-major array in memory */
  long n=0, jj, ii, lenj=length(j), leni=length(i);
  //int _rows = INTEGER(rows)[0];
  long _rows = ((long)REAL(rows)[0]);

  SEXP newi;
  int *_j, *_i, *_newi;

  _j = INTEGER(j);
  _i = INTEGER(i);

  PROTECT( newi = allocVector(INTSXP, leni * lenj));
  _newi = INTEGER(newi); 

  for(jj=0; jj<lenj; jj++) {
    for(ii=0; ii<leni; ii++) {
      _newi[n++] = (_j[jj] - 1) * _rows + _i[ii];
    }
  }

  UNPROTECT(1);
  return newi;
}
