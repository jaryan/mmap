#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>


#include "mmap.h"

#ifdef HAVE_MMAP
#  include <sys/mman.h>
#endif

typedef struct {
  char * data;
  int  size;
  int  fd;
} MM;

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
  return ScalarInteger(flags_bit);
} /*}}}*/

/* mmap_munmap {{{ */
SEXP mmap_munmap (SEXP mmap_obj) {
  char *data = MMAP_DATA(mmap_obj);
  int fd = MMAP_FD(mmap_obj);

  if(data == NULL)
    error("invalid mmap pointer");

  int ret = munmap(data, MMAP_SIZE(mmap_obj));
  close(fd); /* should be moved back to R */
  R_ClearExternalPtr(VECTOR_ELT(mmap_obj,0));
  return(ScalarInteger(ret)); 
} /*}}}*/

/* mmap_mmap AND mmap_finalizer {{{ */
SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _prot,
                SEXP _flags, SEXP _len, SEXP _off) {
  int fd;
  char *data;
  struct stat st;

  stat(CHAR(STRING_ELT(_fildesc,0)), &st);
  fd = open(CHAR(STRING_ELT(_fildesc,0)), O_RDWR);
  if(fd < 0)
    error("unable to open file");
  data = mmap((caddr_t)0, 
              INTEGER(_len)[0], 
              INTEGER(_prot)[0], 
              INTEGER(_flags)[0], 
              fd, 
              INTEGER(_off)[0]);

  if(data == MAP_FAILED)
    error("unable to mmap file");
  
  SEXP mmap_obj;
  PROTECT(mmap_obj = allocVector(VECSXP,5));
  /* data pointer    */
  SET_VECTOR_ELT(mmap_obj, 0, R_MakeExternalPtr(data,R_NilValue,R_NilValue));
  /* size in bytes   */
  SET_VECTOR_ELT(mmap_obj, 1, _len);               
  /* file descriptor */
  SET_VECTOR_ELT(mmap_obj, 2, ScalarInteger(fd));
  /* storage mode    */
  SET_VECTOR_ELT(mmap_obj, 3, _type);
  /* page size       */
  SET_VECTOR_ELT(mmap_obj, 4, ScalarReal((double)sysconf(_SC_PAGE_SIZE)));   

  /*
  need to register a finalizer to munmap in case GC'd
  MM *mm;
  mm = Calloc(1, MM);
  mm[0].data = data;
  mm[0].size = INTEGER(_len)[0];
  mm[0].fd = fd;
  SEXP mm_ptr = R_MakeExternalPtr(mm,R_NilValue,R_NilValue);
  PROTECT(mm_ptr);

  R_RegisterCFinalizerEx(mm_ptr, mmap_finalizer, TRUE);
  */
  UNPROTECT(1);
  return(mmap_obj);
} /*}}}*/

/* mmap_pagesize {{{ */
SEXP mmap_pagesize () {
  return ScalarInteger((int)sysconf(_SC_PAGE_SIZE));
} /*}}}*/

/* mmap_is_mmapped {{{ */
SEXP mmap_is_mmapped (SEXP mmap_obj) {
  char *data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    return(ScalarLogical(0));

  return(ScalarLogical(1));
} /*}}}*/

/* {{{ mmap_msync */
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  char *data;
  data = MMAP_DATA(mmap_obj);
  int ret = msync(data, MMAP_SIZE(mmap_obj), INTEGER(_flags)[0]);
  return ScalarInteger(ret);
}/*}}}*/

/* {{{ mmap_madvise */
SEXP mmap_madvise (SEXP mmap_obj, SEXP _len, SEXP _flags) {
  /* function needs to allow for data to be an offset, else
     we can't control anything of value... */
  char *data;
  data = MMAP_DATA(mmap_obj);
#ifdef HAVE_MADVISE
  int ret = madvise(data, INTEGER(_len)[0], INTEGER(_flags)[0]);
#else
  int ret = -1;
#endif
  return ScalarInteger(ret);
}/*}}}*/

/* {{{ mmap_mprotect */
SEXP mmap_mprotect (SEXP mmap_obj, SEXP index, SEXP prot) {
  int i, ival, upper_bound, LEN;
  char *data, *addr;

  data = MMAP_DATA(mmap_obj);
  LEN = length(index);

  SEXP ret; PROTECT(ret = allocVector(INTSXP, LEN));
  int pagesize = MMAP_PAGESIZE(mmap_obj);
  
  upper_bound = (int)(MMAP_SIZE(mmap_obj)-sizeof(int));
  for(i=0;i<LEN;i++) {
    ival = (INTEGER(index)[i]-1)*sizeof(int);
    if( ival > upper_bound || ival < 0 )
      error("'i=%i' out of bounds", i);
    
Rprintf("offset: %i\n",(ival/pagesize)*pagesize);
    addr = &(data[(int)((ival/pagesize)*pagesize)]);
    INTEGER(ret)[i] = mprotect(addr, ((ival/pagesize)*pagesize)*2, INTEGER(prot)[0]);
  }
  UNPROTECT(1);
  return ret;
}/*}}}*/

/* {{{ mmap_extract */
SEXP mmap_extract (SEXP index, SEXP field, SEXP mmap_obj) {
/*SEXP mmap_extract (SEXP index, SEXP field, SEXP mmap_obj) {*/
  int v, fi, i, ii, ival;
  int P=0;
  unsigned char *data; /* unsigned int and values */

  /* 24 bit integers require a mask of the depending
     on whether the type is signed or unsigned */
  char *int24_buf[4],
       *uint24_buf[4];  
  memset(int24_buf, 0, 4);
  memset(uint24_buf, 0xFF, 4);


  PROTECT(index = coerceVector(index,INTSXP)); P++;
  int LEN = length(index);  
  int mode = MMAP_MODE(mmap_obj);
  int Cbytes = MMAP_CBYTES(mmap_obj);
  int isSigned = MMAP_SIGNED(mmap_obj);

  char *int_buf[sizeof(int)], *real_buf[sizeof(double)];
  char *complex_buf[sizeof(Rcomplex)];
  char *short_buf[sizeof(short)], *float_buf[sizeof(float)];

  unsigned char *byte_buf;
  SEXP byteBuf;
  int *int_dat;
  double *real_dat;
  Rcomplex *complex_dat;
  unsigned char *raw_dat;

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  SEXP dat; /* dat is either a column, or list of columns */
  if(mode==VECSXP) {
    //PROTECT(dat = allocVector(VECSXP, length(MMAP_SMODE(mmap_obj))));
    PROTECT(dat = allocVector(VECSXP, length(field)));
  } else PROTECT(dat = allocVector(mode,LEN));
  P++;

  int *index_p = INTEGER(index);
  int upper_bound;

  /* need R typed storage for structures... 
     ideally we needn't alloc for types
     that are not used --- move alloc to do that */
  int fieldCbytes;
  int fieldSigned;
  int offset;

  SEXP vec_dat;  /* need all R types supported: INT/REAL/CPLX/RAW */
  int *int_vec_dat; 
  double *real_vec_dat;
  Rcomplex *complex_vec_dat;

  switch(mode) {
  case INTSXP: /* {{{ */
    int_dat = INTEGER(dat);
    upper_bound = (int)(MMAP_SIZE(mmap_obj)-Cbytes);
    switch(Cbytes) {
      case 1: /* 1 byte (signed) char */
        if(isSigned) {
          for(i=0;  i < LEN; i++) {
            ival = (index_p[i]-1);
            if( ival > upper_bound || ival < 0 )
              error("'i=%i' out of bounds", index_p[i]);
            int_dat[i] = (int)(char)(data[(index_p[i]-1)]);
          }
        } else { /* unsigned */
          for(i=0;  i < LEN; i++) {
            ival = (index_p[i]-1);
            if( ival > upper_bound || ival < 0 )
              error("'i=%i' out of bounds", index_p[i]);
            int_dat[i] = (int)(unsigned char)(data[(index_p[i]-1)]);
          }
        }
        break;
      case 2: /* 2 byte short */
        if(isSigned) {
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(short);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(short_buf, 
                 &(data[(index_p[i]-1)*sizeof(short)]),
                 sizeof(char)*sizeof(short));
          int_dat[i] = (int)(short)*(short *)(short_buf); 
        }
        } else {
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(short);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(short_buf, 
                 &(data[(index_p[i]-1)*sizeof(short)]),
                 sizeof(char)*sizeof(short));
          int_dat[i] = (int)(unsigned short)*(unsigned short *)(short_buf); 
        }  
        }
        break;
      case 3: /* 3 byte int */
        if(isSigned) {
        for(i=0;  i < LEN; i++) {
          ival =  (index_p[i]-1)*3;
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(int24_buf, 
                 &(data[(index_p[i]-1)*3]), /* copy first 3 bytes */
                 3);
          int_dat[i] = (int)*(int *)(int24_buf); 
          if(int_dat[i] > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[(index_p[i]-1)*3]), /* copy first 3 bytes */
                   3);
            int_dat[i] = (int)*(int *)(uint24_buf); 
          }
        }
        } else { /* 3 byte unsigned */
        for(i=0;  i < LEN; i++) {
          ival =  (index_p[i]-1)*3;
          /* reset int_but to 0 0 0 0 */
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(int24_buf, 
                 &(data[(index_p[i]-1)*3]), /* copy first 3 bytes */
                 3);
          int_dat[i] = (int)*(int *)(int24_buf); 
        }
        }
        break;
      case 4: /* 4 byte int */
        for(i=0;  i < LEN; i++) {
          ival =  (index_p[i]-1)*sizeof(int);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(int_buf, 
                 &(data[(index_p[i]-1)*sizeof(int)]),
                 sizeof(char)*sizeof(int));
          int_dat[i] = (int)*(int *)(int_buf); 
        }
        break;
      default:
        error("unknown data type");
        break;
    }
    break; /* }}} */
  case REALSXP: /* {{{ */
    real_dat = REAL(dat);
    upper_bound = (long)(MMAP_SIZE(mmap_obj)-Cbytes);
    switch(Cbytes) {
      case 4: /* 4 byte float */
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(float_buf, 
                 &(data[(index_p[i]-1)*sizeof(float)]), 
                 sizeof(char)*sizeof(float));
          real_dat[i] = (double)(float)*(float *)(float_buf); 
        }
        break;
      case 8: /* 8 byte double */
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(real_buf, 
                 &(data[(index_p[i]-1)*sizeof(double)]), 
                 sizeof(char)*sizeof(double));
          real_dat[i] = (double)*(double *)(real_buf); 
        }
        break;
      default:
        break;
    }
    break; /* }}} */
  case CPLXSXP: /* {{{ */
    complex_dat = COMPLEX(dat);
    upper_bound = (long)(MMAP_SIZE(mmap_obj)-Cbytes);
    for(i=0;  i < LEN; i++) {
      ival = (index_p[i]-1);
      if( ival > upper_bound || ival < 0 )
        error("'i=%i' out of bounds", index_p[i]);
      memcpy(complex_buf, 
             &(data[(index_p[i]-1)*sizeof(Rcomplex)]), 
             sizeof(char)*sizeof(Rcomplex));
      //complex_dat[i] = (Rcomplex)*(Rcomplex *)(complex_buf); 
      complex_dat[i] = *(Rcomplex *)(complex_buf); 
    }
    break; /* }}} */
  case STRSXP: /* {{{ */
    /* see https://svn.r-project.org/R/trunk/src/main/raw.c */
    /* fixed width character support */
    for(i=0; i < LEN; i++) {
      SET_STRING_ELT(dat, i,
        mkCharLenCE((const char *)&(data[(index_p[i]-1)*Cbytes]),
                    Cbytes-1, CE_NATIVE));
    }
    break; /* }}} */
  case RAWSXP: /* {{{ */
    raw_dat = RAW(dat);
// differ in signess???
    upper_bound = (int)(MMAP_SIZE(mmap_obj)-1);
    for(i=0;  i < LEN; i++) {
      ival =  (index_p[i]-1);
      if( ival > upper_bound || ival < 0 )
        error("'i=%i' out of bounds", index_p[i]);
      raw_dat[i] = data[(index_p[i]-1)];
    }
    break; /* }}} */
  case VECSXP:  /* corresponds to C struct for mmap package {{{ */
    PROTECT(byteBuf = allocVector(RAWSXP,LEN*Cbytes)); P++;
    byte_buf = RAW(byteBuf);
    /* extract_struct:
      
       - bytes in struct from MMAP_CBYTES
       - loop through all `i` memcpy struct to byte array
       - loop through VECSXP;
           test for TYPEOF
           copy bytes by location into TYPEd array
       - collect arrays into VECSXP dat
    */
    for(i=0; i<LEN; i++) {
      /* byte_buf (byteBuf) now has all bytes for all structs */
      memcpy(&(byte_buf[i*Cbytes]),
             &(data[(index_p[i]-1) * Cbytes]),
             Cbytes); 
    }  
    for(fi=0; fi<length(field); fi++) {
      v = INTEGER(field)[fi]-1;
    //for(v=0; v<length(MMAP_SMODE(mmap_obj)); v++) {
      offset = MMAP_OFFSET(mmap_obj,v);
      fieldCbytes = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),
                                      v),install("bytes")))[0];
      fieldSigned = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),
                                      v),install("signed")))[0];
      switch(TYPEOF(VECTOR_ELT(MMAP_SMODE(mmap_obj), v))) {
        case INTSXP:
          PROTECT(vec_dat = allocVector(INTSXP, LEN)); 
          int_vec_dat = INTEGER(vec_dat);
          switch(fieldCbytes) {
            case sizeof(char):
            if(fieldSigned) {   /* 1 byte char */
            for(ii=0; ii<LEN; ii++) {
              int_vec_dat[ii] = (int)(char)(byte_buf[ii*Cbytes+offset]);;
            }
            } else {            /* 1 byte unsigned char */
            for(ii=0; ii<LEN; ii++) {
              int_vec_dat[ii] = (int)(unsigned char)(byte_buf[ii*Cbytes+offset]);;
            }
            }
            break;
            case sizeof(short):
            if(fieldSigned) {   /* 2 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(int_buf, 
                     &(byte_buf[ii*Cbytes+offset]),
                     sizeof(char)*sizeof(short));
              int_vec_dat[ii] = (int)*(short *)(int_buf); 
            }
            } else {            /* 2 byte unsigned short */
            for(ii=0; ii<LEN; ii++) {
              memcpy(int_buf, 
                     &(byte_buf[ii*Cbytes+offset]),
                     sizeof(char)*sizeof(short));
              int_vec_dat[ii] = (int)*(unsigned short *)(int_buf); 
            }
            }
            break;
            case sizeof(int): /* 4 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(int_buf, 
                     &(byte_buf[ii*Cbytes+offset]),
                     sizeof(char)*sizeof(int));
              int_vec_dat[ii] = (int)*(int *)(int_buf); 
            }
            break;
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          //SET_VECTOR_ELT(dat, v, vec_dat);
          UNPROTECT(1);
          break;
        case REALSXP:
          PROTECT(vec_dat = allocVector(REALSXP, LEN));
          real_vec_dat = REAL(vec_dat);
          switch(fieldCbytes) {
            case sizeof(float): /* 4 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(float_buf, 
                     &(byte_buf[ii*Cbytes+offset]),
                     sizeof(char)*sizeof(float));
              real_vec_dat[ii] = (double)(float)*(float *)(float_buf); 
            }
            break;
            case sizeof(double): /* 8 byte */
            for(ii=0; ii<LEN; ii++) {
              memcpy(real_buf, 
                     &(byte_buf[ii*Cbytes+offset]),
                     sizeof(char)*sizeof(double));
              real_vec_dat[ii] = (double)*(double *)(real_buf); 
            }
          }
          SET_VECTOR_ELT(dat, fi, vec_dat);
          UNPROTECT(1);
          break;
        case CPLXSXP:
          PROTECT(vec_dat = allocVector(CPLXSXP, LEN));
          complex_vec_dat = COMPLEX(vec_dat);
          for(ii=0; ii<LEN; ii++) {
            memcpy(complex_buf, 
                   &(byte_buf[ii*Cbytes+offset]),
                   sizeof(char)*sizeof(Rcomplex));
            complex_vec_dat[ii] = *(Rcomplex *)(complex_buf); 
          }
          SET_VECTOR_ELT(dat, v, vec_dat);
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
  UNPROTECT(P);
  return dat;
}/*}}}*/

/* mmap_replace {{{ */
SEXP mmap_replace (SEXP index, SEXP field, SEXP value, SEXP mmap_obj) {
  int i, upper_bound, ival;
  int v, fi, offset, fieldCbytes, fieldSigned;
  char *data;
  int LEN = length(index);  
  int mode = MMAP_MODE(mmap_obj);
  int Cbytes = MMAP_CBYTES(mmap_obj);
  /*int isSigned = MMAP_SIGNED(mmap_obj);*/
  int P=0;

  if((data = MMAP_DATA(mmap_obj)) == NULL)
    error("invalid mmap pointer");

  int    *int_value;
  double *real_value;
  float  float_value;
  short  short_value;
  char   char_value;

  if(mode != VECSXP) {
    PROTECT(value = coerceVector(value, mode)); P++;
  }
  PROTECT(index = coerceVector(index, INTSXP) ); P++;
  PROTECT(field = coerceVector(field, INTSXP) ); P++;
  int *index_p = INTEGER(index);
  switch(mode) {
  case INTSXP: /* {{{ */
    int_value = INTEGER(value);
    upper_bound = (int)(MMAP_SIZE(mmap_obj)-Cbytes);
    switch(Cbytes) {
      case sizeof(char): /* 1 byte char */
      /*
      if(isSigned) {
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(char);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          char_value = (char)(int_value[i]); 
          memcpy(&(data[(index_p[i]-1)*sizeof(char)]), &(char_value), sizeof(char));
        }
      } else {
      */
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(char);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          char_value = (unsigned char)(int_value[i]); 
          memcpy(&(data[(index_p[i]-1)*sizeof(char)]), &(char_value), sizeof(char));
        }
      /*}*/
      break;
      case sizeof(short): /* 2 byte short */
      /*
      if(isSigned) {
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(short);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          short_value = (short)(int_value[i]); 
          memcpy(&(data[(index_p[i]-1)*sizeof(short)]), &(short_value), sizeof(short));
        }
      } else {
      */
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(short);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          short_value = (unsigned short)(int_value[i]); 
          memcpy(&(data[(index_p[i]-1)*sizeof(short)]), &(short_value), sizeof(short));
        }
      /*}*/
      break;
      case 3: /* case 3 byte */
      /*
      if(isSigned) {
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*3;
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(&(data[(index_p[i]-1)*3]), &(int_value[i]), 3);
        }
      } else {
      */
      for(i=0;  i < LEN; i++) {
        ival = (index_p[i]-1)*3;
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", index_p[i]);
        memcpy(&(data[(index_p[i]-1)*3]), &(int_value[i]), 3);
      }
      /*}*/
      break;
      case sizeof(int): /* 4 byte int */
        for(i=0;  i < LEN; i++) {
          ival = (index_p[i]-1)*sizeof(int);
          if( ival > upper_bound || ival < 0 )
            error("'i=%i' out of bounds", index_p[i]);
          memcpy(&(data[(index_p[i]-1)*sizeof(int)]), &(int_value[i]), sizeof(int));
        }
        break;
    }
    break; /* }}} */
  case REALSXP: /* {{{ */
    real_value = REAL(value);
    upper_bound = (int)(MMAP_SIZE(mmap_obj)-Cbytes);
    switch(Cbytes) {
      case sizeof(float): /* 4 byte float */
      for(i=0;  i < LEN; i++) {
        ival =  (index_p[i]-1)*sizeof(float);
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", i);
        float_value = (float)(real_value[i]);
        memcpy(&(data[(index_p[i]-1)*sizeof(float)]), &(float_value), sizeof(float));
      }
      break;
      case sizeof(double): /* 8 byte double */
      for(i=0;  i < LEN; i++) {
        ival =  (index_p[i]-1)*sizeof(double);
        if( ival > upper_bound || ival < 0 )
          error("'i=%i' out of bounds", i);
        memcpy(&(data[(index_p[i]-1)*sizeof(double)]), &(real_value[i]), sizeof(double));
      }
      break;
    }
    break; /* }}} */
  case VECSXP: /* {{{ */
    if(length(value) != length(field))
      error("size of struct and size of replacement value do not match");
    for(fi=0; fi<length(field); fi++) {
      v = INTEGER(field)[fi]-1;
      offset = MMAP_OFFSET(mmap_obj, v);  /* byte offset of column */
      fieldCbytes = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),v),
                                      install("bytes")))[0];
      fieldSigned = INTEGER(getAttrib(VECTOR_ELT(MMAP_SMODE(mmap_obj),v),
                                      install("signed")))[0];
      switch(TYPEOF(VECTOR_ELT(MMAP_SMODE(mmap_obj),v))) {
        case INTSXP:
          LEN = length(VECTOR_ELT(value,fi));
          int_value = INTEGER(VECTOR_ELT(value, fi));
          switch(fieldCbytes) {
            case sizeof(short):
            if(fieldSigned) {
              for(i=0; i < LEN; i++) {
                short_value = (short)(INTEGER(VECTOR_ELT(value,fi))[i]);
                memcpy(&(data[(index_p[i]-1)*Cbytes+offset]),
                       &short_value,
                       fieldCbytes);
              }
            } else {
              for(i=0; i < LEN; i++) {
                short_value = (unsigned short)(INTEGER(VECTOR_ELT(value,fi))[i]);
                memcpy(&(data[(index_p[i]-1)*Cbytes+offset]),
                       &short_value,
                       fieldCbytes);
              }
            }
            break;
            case sizeof(int):
              for(i=0; i < LEN; i++) {
                memcpy(&(data[(index_p[i]-1)*Cbytes+offset]),
                       &(int_value[i]),
                       sizeof(int));
              }
            break;
          }
          break;
        case REALSXP:
          LEN = length(VECTOR_ELT(value,fi));
          switch(fieldCbytes) {
            case sizeof(float):
            for(i=0; i < LEN; i++) {
              float_value = (float)(REAL(VECTOR_ELT(value,fi))[i]);
              memcpy(&(data[(index_p[i]-1)*Cbytes+offset]),
                     &float_value,
                     sizeof(float));
            }
            break;
          case sizeof(double):
            for(i=0; i < LEN; i++) {
              memcpy(&(data[(index_p[i]-1)*Cbytes+offset]),
                     &(REAL(VECTOR_ELT(value,v))[i]),
                     sizeof(double));
            }
            break;
          }
          break;
        default:
          break;
      }
    } /* VECSXP }}} */
    break;
  case STRSXP:
    for(i=0; i < LEN; i++) {
      memcpy(&(data[(index_p[i]-1)*Cbytes]), CHAR(STRING_ELT(value,i)), Cbytes);
    }
    break;
  case CPLXSXP:
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
  char *data, 
       *char_buf[sizeof(char)], 
       *int_buf[sizeof(int)], 
       *short_buf[sizeof(short)],
       *float_buf[sizeof(float)],
       *real_buf[sizeof(double)];

  /* 24 bit integers require a mask of the depending
     on whether the type is signed or unsigned */
  char *int24_buf[4],
       *uint24_buf[4];  
  memset(int24_buf, 0, 4);
  memset(uint24_buf, 0xFF, 4);

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
  int hits=0;
  int cmp_to_int;
  double cmp_to_real;
  unsigned char * cmp_to_raw;

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  switch(mode) {
  case INTSXP:
    cmp_to_int = INTEGER(coerceVector(compare_to,INTSXP))[0];
    /* needs to branch for
        uint8, int8, uint16, int16, uint24, int24, int32 */
    switch(Cbytes) {
    case 1: /* char int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]),sizeof(char));
          if(cmp_to_int == (int)(char)*(char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int != (int)(char)*(char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <= (int)(char)*(char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >= (int)(char)*(char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <  (int)(char)*(char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >  (int)(char)*(char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      }
      } else { /* unsigned char */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]),sizeof(char));
          if(cmp_to_int == (int)(unsigned char)*(unsigned char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int != (int)(unsigned char)*(unsigned char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <= (int)(unsigned char)*(unsigned char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >= (int)(unsigned char)*(unsigned char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <  (int)(unsigned char)*(unsigned char *)(char_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(char_buf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >  (int)(unsigned char)*(unsigned char *)(char_buf)) 
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
          memcpy(short_buf, &(data[i * sizeof(short)]),sizeof(short));
          //if(cmp_to_int == (int)(short)*(short *)(short_buf)) 
          if(cmp_to_int == (short)*(short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int != (int)(short)*(short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <= (int)(short)*(short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >= (int)(short)*(short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <  (int)(short)*(short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >  (int)(short)*(short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      }
      } else { /* unsigned short */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]),sizeof(short));
          if(cmp_to_int == (int)(unsigned short)*(unsigned short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int != (int)(unsigned short)*(unsigned short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <= (int)(unsigned short)*(unsigned short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >= (int)(unsigned short)*(unsigned short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <  (int)(unsigned short)*(unsigned short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(short_buf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >  (int)(unsigned short)*(unsigned short *)(short_buf)) 
            int_result[hits++] = i+1;
        }
      } /* end of ushort */
      }
      break; /* }}} */
    case 3: /* 3 byte int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if((int)*(int *)(int24_buf) > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int == (int)*(int *)(uint24_buf)) 
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int == (int)*(int *)(int24_buf)) 
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if((int)*(int *)(int24_buf) > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int != (int)*(int *)(uint24_buf)) 
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int != (int)*(int *)(int24_buf)) 
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if((int)*(int *)(int24_buf) > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int <= (int)*(int *)(uint24_buf)) 
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int <= (int)*(int *)(int24_buf)) 
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if((int)*(int *)(int24_buf) > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int >= (int)*(int *)(uint24_buf)) 
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int >= (int)*(int *)(int24_buf)) 
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if((int)*(int *)(int24_buf) > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int < (int)*(int *)(uint24_buf)) 
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int < (int)*(int *)(int24_buf)) 
              int_result[hits++] = i+1;
          }
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if((int)*(int *)(int24_buf) > 8388607) {  /* MAX 3 byte unsigned INTEGER */
            memcpy(uint24_buf, 
                   &(data[i*3]), /* copy first 3 bytes */
                   3);
            if(cmp_to_int > (int)*(int *)(uint24_buf)) 
              int_result[hits++] = i+1;
          } else {
            if(cmp_to_int > (int)*(int *)(int24_buf)) 
              int_result[hits++] = i+1;
          }
        }
      }
      } else { /* unsigned 3 byte int */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int == (int)*(int *)(int24_buf))
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int != (int)*(int *)(int24_buf))
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int <= (int)*(int *)(int24_buf))
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int >= (int)*(int *)(int24_buf))
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int <  (int)*(int *)(int24_buf))
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(int24_buf, 
                 &(data[i*3]), /* copy first 3 bytes */
                 3);
          if(cmp_to_int >  (int)*(int *)(int24_buf))
            int_result[hits++] = i+1;
        }
      } /* end of unsigned 3 byte */
      }
      break; /* }}} */
    case 4: /* int {{{ */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(int_buf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int == (int)*(int *)(int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(int_buf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int != (int)*(int *)(int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(int_buf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int <= (int)*(int *)(int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(int_buf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int >= (int)*(int *)(int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(int_buf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int <  (int)*(int *)(int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(int_buf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int >  (int)*(int *)(int_buf)) 
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
          memcpy(float_buf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real == (double)(float)*(float *)(float_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(float_buf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real != (double)(float)*(float *)(float_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(float_buf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real <= (double)(float)*(float *)(float_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(float_buf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real >= (double)(float)*(float *)(float_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(float_buf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real <  (double)(float)*(float *)(float_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(float_buf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real >  (double)(float)*(float *)(float_buf)) 
            int_result[hits++] = i+1;
        }
      }
      break;
    case sizeof(double):
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real == (double)*(double *)(real_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real != (double)*(double *)(real_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real <= (double)*(double *)(real_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real >= (double)*(double *)(real_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real <  (double)*(double *)(real_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real >  (double)*(double *)(real_buf)) 
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
      
    }
    break;
  case STRSXP: /* {{{ */
    /* see https://svn.r-project.org/R/trunk/src/main/raw.c */
    /* fixed width character support */
    cmp_to_raw = RAW(compare_to);
    int b;
    for(i=0; i < LEN; i++) {
        for(b=0; b < Cbytes-1; b++) {
          if(cmp_to_raw[b] != data[i*Cbytes + b])
            break;
        }
        if(b == Cbytes-1)
          int_result[hits++] = i+1;
    }
    break; /* }}} */
  default:
    error("unsupported type");
    break;
  }
  result = lengthgets(result, hits);
  UNPROTECT(1);
  return result;
  return ScalarInteger(hits);
}/*}}}*/

