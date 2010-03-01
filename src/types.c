#include <R.h>
#include <Rinternals.h>

SEXP sizeof_Ctypes () {
  SEXP Ctypes;
  PROTECT(Ctypes = allocVector(VECSXP,6));

  SET_VECTOR_ELT(Ctypes, 0, ScalarInteger(sizeof(char)));
  SET_VECTOR_ELT(Ctypes, 1, ScalarInteger(sizeof(short)));
  SET_VECTOR_ELT(Ctypes, 2, ScalarInteger(sizeof(int)));
  SET_VECTOR_ELT(Ctypes, 3, ScalarInteger(sizeof(long)));
  SET_VECTOR_ELT(Ctypes, 4, ScalarInteger(sizeof(float)));
  SET_VECTOR_ELT(Ctypes, 5, ScalarInteger(sizeof(double)));

  UNPROTECT(1);
  return Ctypes;
}
