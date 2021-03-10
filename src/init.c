/*
#   mmap
#
#   Copyright (C) 2008 - 2013  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich and Dirk Eddelbuettel
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "mmap.h"
#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/libextern.h>

SEXP  mmap_dataSymbol;
SEXP  mmap_bytesSymbol;
SEXP  mmap_endianSymbol;
SEXP  mmap_filedescSymbol;
SEXP  mmap_storageModeSymbol;
SEXP  mmap_signedSymbol;
SEXP  mmap_offsetSymbol;
SEXP  mmap_pagesizeSymbol;
SEXP  mmap_dimSymbol;
SEXP  mmap_lengthSymbol;
SEXP  mmap_cstringSymbol;
SEXP  mmap_protSymbol;
SEXP  mmap_flagsSymbol;
SEXP  mmap_handleSymbol; /* WIN */
SEXP  nul_Symbol;


static void SymbolShortcuts(void) {
  mmap_dataSymbol = install("data");
  mmap_bytesSymbol = install("bytes");
  mmap_endianSymbol = install("endian");
  mmap_filedescSymbol = install("filedesc");
  mmap_storageModeSymbol = install("storage.mode");
  mmap_signedSymbol = install("signed");
  mmap_offsetSymbol = install("offset");
  mmap_pagesizeSymbol = install("pagesize");
  mmap_dimSymbol = install("dim");
  mmap_lengthSymbol = install("length");
  mmap_cstringSymbol = install("cstring");
  mmap_protSymbol = install("prot");
  mmap_flagsSymbol = install("flags");
  mmap_handleSymbol = install("handle");
  nul_Symbol = install("nul");
}

static const
R_CallMethodDef callMethods[] = {
  {"make_bitmask",          (DL_FUNC) &make_bitmask,            0},
  {"mmap_mkFlags",          (DL_FUNC) &mmap_mkFlags,            1},
  {"mmap_munmap",           (DL_FUNC) &mmap_munmap,             1},
  {"mmap_mmap",             (DL_FUNC) &mmap_mmap,               8},
  {"mmap_pagesize",         (DL_FUNC) &mmap_pagesize,           0},
  {"mmap_is_mmapped",       (DL_FUNC) &mmap_is_mmapped,         1},
  {"mmap_msync",            (DL_FUNC) &mmap_msync,              2},
  {"mmap_madvise",          (DL_FUNC) &mmap_madvise,            3},
  {"mmap_mprotect",         (DL_FUNC) &mmap_mprotect,           3},
  {"mmap_extract",          (DL_FUNC) &mmap_extract,            4},
  {"mmap_replace",          (DL_FUNC) &mmap_replace,            4},
  {"mmap_compare",          (DL_FUNC) &mmap_compare,            3},

  // cstring
  {"mmap_cstring_maxwidth", (DL_FUNC) &mmap_cstring_maxwidth,   0},
  {"mmap_cstring_length",   (DL_FUNC) &mmap_cstring_length,     1},
  {"mmap_cstring_extract",  (DL_FUNC) &mmap_cstring_extract,    2},
  {"mmap_cstring_isna",     (DL_FUNC) &mmap_cstring_isna,       2},
  {"mmap_cstring_create",   (DL_FUNC) &mmap_cstring_create,     2},
  {"mmap_cstring_words",    (DL_FUNC) &mmap_cstring_words,      1},
  {"mmap_cstring_chunks",   (DL_FUNC) &mmap_cstring_chunks,     1},

  {"convert_ij_to_i",       (DL_FUNC) &convert_ij_to_i,         3},
  {"sizeof_Ctypes",         (DL_FUNC) &sizeof_Ctypes,           0},
  {NULL,                    NULL,                               0}
};


void R_init_mmap(DllInfo *info)
{
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, TRUE);
  SymbolShortcuts();
}
