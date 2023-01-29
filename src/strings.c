#include <R.h>
#include <Rinternals.h>

#include <limits.h>
#include <string.h>
#include "mmap.h"


struct CString_ {
  unsigned short *words;
  int num_words;
  unsigned long *chunks;
  int num_chunks;
  int chunk_size;
} CString_;
typedef struct CString_ *CString;

SEXP mmap_cstring_length (SEXP mmap_obj) {
  long i;
  char *data;

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  long num_words=0;
  long LEN = (long)(MMAP_SIZE(mmap_obj)); // in bytes
  for(i=0;  i < LEN; i++) {
    if(data[i*sizeof(char)] == '\0')
      num_words++;
  }
  return ScalarReal((double)num_words);
}
SEXP mmap_cstring_create (SEXP mmap_obj, SEXP _chunk_size) {
  long i;
  char *data;

  long LEN;
  SEXP CStringData;
  LEN = (long)(MMAP_SIZE(mmap_obj));

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  int word=0;
  int chunk_size=INTEGER(_chunk_size)[0];
  int current_chunk=0, prev_chunk=0;

  // length is added to object during mmap
  long num_words = (long)(MMAP_LENGTH(mmap_obj));

  CString CS ;
  CS = (CString) malloc(sizeof(CString_));
  
  CS->words = (unsigned short *)malloc(sizeof(unsigned short) * num_words);
  int num_chunks = (num_words / chunk_size)+1+1;
  CS->chunks = (unsigned long *)malloc(sizeof(unsigned long) * num_chunks);

  memset(CS->words, 0, sizeof(unsigned short)*num_words);
  memset(CS->chunks, 0, sizeof(unsigned long)*num_chunks);

  for(i=0;  i < LEN; i++) {
    current_chunk = word / chunk_size;
    if (current_chunk != prev_chunk)
      CS->chunks[current_chunk + 1] = CS->chunks[prev_chunk + 1];
    prev_chunk = current_chunk;

    if(data[i*sizeof(char)] == '\0') {
      CS->words[word]++;
      CS->chunks[current_chunk + 1]++;
      word++;
      continue;
    }
    CS->words[word]++;
    CS->chunks[current_chunk + 1]++;
  }

  CS->num_chunks = num_chunks;
  CS->num_words = num_words;
  CS->chunk_size = chunk_size;

  PROTECT(CStringData = R_MakeExternalPtr(CS, R_NilValue, R_NilValue));
  SEXP className;
  PROTECT(className = allocVector(STRSXP,1));
  SET_STRING_ELT(className, 0, mkChar("cstring"));
  setAttrib(CStringData, R_ClassSymbol, className);
  UNPROTECT(2);
  return CStringData;

}

SEXP mmap_cstring_extract(SEXP mmap_obj, SEXP index) {
  long i, ii;
  char *data;

  long LEN;
  SEXP res;
  LEN = (long)LENGTH(index);

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  CString cs = (CString)R_ExternalPtrAddr(MMAP_CSTRING(mmap_obj));
  char *str_buf = R_alloc(sizeof(char), USHRT_MAX); /* len+\0, should be moved to package level */

  PROTECT(res = allocVector(STRSXP,LEN));
  char *str;
  int chunk, offset;
  int word_len;
  long total_offset, cumsum_offset;
  double *index_p = REAL(index);
  for(i=0; i<LEN; i++) {
    chunk = ((long)index_p[i]-1) / cs->chunk_size;
    offset = ((long)index_p[i]-1) % cs->chunk_size;
    total_offset = cs->chunks[chunk];
    cumsum_offset = 0;
    for(ii=chunk * cs->chunk_size; ii < offset + chunk * cs->chunk_size; ii++) {
       cumsum_offset = cumsum_offset + cs->words[ii];
    }
    total_offset = total_offset + cumsum_offset;
    word_len = cs->words[ii];  // account for nul and offset instead of actual word

    str = (char *)(&(data[(long)total_offset]));
    strncpy(str_buf, str, word_len);
    // TODO: allow for na.strings vector of all possible NA values
    if(word_len == 3 && strncmp(str_buf, "NA", 2) == 0) {
      SET_STRING_ELT(res, i, NA_STRING);
    } else {
      SET_STRING_ELT(res, i, mkChar( (const char *)str_buf));
    }
  }

  UNPROTECT(1);
  return res;
}
SEXP mmap_cstring_compare(SEXP compare_to, SEXP compare_how, SEXP mmap_obj, int *hits) {
  long i, ii;
  char *data;

  long LEN;
  SEXP result;
  LEN = MMAP_LENGTH(mmap_obj);

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  CString cs = (CString)R_ExternalPtrAddr(MMAP_CSTRING(mmap_obj));

  PROTECT(result = allocVector(INTSXP, LEN));
  int *int_result = INTEGER(result);
  char *str;
  int cmp_len = length(compare_to);
  int cmp_how = INTEGER(compare_how)[0];
  unsigned char * cmp_to_raw = RAW(compare_to);
  int chunk;
  int word_len;
  long total_offset = 0, cumsum_offset = 0;
  i=0;
  for(chunk=0; chunk < cs->num_chunks; chunk++) {
    cumsum_offset = 0;
    for(ii=0; ii<cs->chunk_size; ii++, i++) {
      if(i >= LEN)
        break;
      word_len = cs->words[i];
      total_offset = cs->chunks[chunk] + cumsum_offset;
      cumsum_offset = cumsum_offset + cs->words[i];
      if(cmp_how==1) { // ==
        if(word_len-1 != cmp_len)
          continue;
        str = (char *)(&(data[(long)total_offset]));
        if(str[0] != cmp_to_raw[0])
          continue;
        if(memcmp(str, cmp_to_raw, word_len-1)==0) {
          int_result[(*hits)++] = i+1;
        }
      } else
      if(cmp_how==2) { // !=
        str = (char *)(&(data[(long)total_offset]));
        if(str[0] == cmp_to_raw[0])
          continue;
        if(memcmp(str, cmp_to_raw, word_len-1)!=0) {
          int_result[(*hits)++] = i+1;
        }
      } else
      if(cmp_how==7) { // is.na  FIXME: this should use compare_to and cmp_len and memcmp
        str = (char *)(&(data[(long)total_offset]));
        if(word_len == 3 && strncmp(str, "NA", 2) == 0) {
          int_result[(*hits)++] = i+1;
        }
      } else {
        error("comparison not valid for characters");
      }
    }
  }

  UNPROTECT(1);
  return result;
}

SEXP mmap_cstring_isna(SEXP mmap_obj, SEXP any) {
  int hits;
  int P = 0;
  SEXP cmp_to;
  PROTECT(cmp_to = allocVector(RAWSXP,1)); P++;
  RAW(cmp_to)[0] = '\0';
  SEXP seven = ScalarInteger(7);
  PROTECT(seven); P++;
  SEXP result = mmap_cstring_compare(cmp_to, seven, mmap_obj, &hits);
  PROTECT(result); P++;
  result = lengthgets(result, hits);
  UNPROTECT(P);
  return result;
}

SEXP mmap_cstring_words(SEXP mmap_cstring) {
  CString cs = (CString)R_ExternalPtrAddr(mmap_cstring);
  SEXP words;

  int num_words = cs->num_words;
  PROTECT(words = allocVector(INTSXP,num_words));
  int *words_p = INTEGER(words);
  long i;
  for(i=0; i<num_words; i++) {
    words_p[i] = (int)cs->words[i];
  }

  UNPROTECT(1);
  return words;
}
SEXP mmap_cstring_chunks(SEXP mmap_cstring) {
  CString cs = (CString)R_ExternalPtrAddr(mmap_cstring);
  SEXP chunks;

  int num_chunks = cs->num_chunks;
  PROTECT(chunks = allocVector(INTSXP,num_chunks));
  int *chunks_p = INTEGER(chunks);
  long i;
  for(i=0; i<num_chunks; i++) {
    chunks_p[i] = (int)cs->chunks[i];
  }

  UNPROTECT(1);
  return chunks;
}
