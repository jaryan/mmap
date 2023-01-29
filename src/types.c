#include <R.h>
#include <Rinternals.h>

SEXP sizeof_Ctypes (void) {
  SEXP Ctypes;
  PROTECT(Ctypes = allocVector(VECSXP,7));

  SET_VECTOR_ELT(Ctypes, 0, ScalarInteger(sizeof(char)));
  SET_VECTOR_ELT(Ctypes, 1, ScalarInteger(sizeof(short)));
  SET_VECTOR_ELT(Ctypes, 2, ScalarInteger(sizeof(int)));
  SET_VECTOR_ELT(Ctypes, 3, ScalarInteger(sizeof(long)));
  SET_VECTOR_ELT(Ctypes, 4, ScalarInteger(sizeof(float)));
  SET_VECTOR_ELT(Ctypes, 5, ScalarInteger(sizeof(double)));
  SET_VECTOR_ELT(Ctypes, 6, ScalarInteger(USHRT_MAX-1));

  SEXP names;
  PROTECT(names = allocVector(STRSXP,7));
  SET_STRING_ELT(names, 0, mkChar("char"));
  SET_STRING_ELT(names, 1, mkChar("short"));
  SET_STRING_ELT(names, 2, mkChar("int"));
  SET_STRING_ELT(names, 3, mkChar("long"));
  SET_STRING_ELT(names, 4, mkChar("float"));
  SET_STRING_ELT(names, 5, mkChar("double"));
  SET_STRING_ELT(names, 6, mkChar("cstring"));
  setAttrib(Ctypes, R_NamesSymbol, names);

  UNPROTECT(2);
  return Ctypes;
}

SEXP mmap_cstring_maxwidth(void) {
    return ScalarInteger(USHRT_MAX - 1);
}
