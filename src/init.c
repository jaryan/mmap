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

static void SymbolShortcuts(void) {
  mmap_dataSymbol = install("data");
  mmap_bytesSymbol = install("bytes");
  mmap_filedescSymbol = install("filedesc");
  mmap_storageModeSymbol = install("storage.mode");
  mmap_signedSymbol = install("signed");
  mmap_offsetSymbol = install("offset");
  mmap_pagesizeSymbol = install("pagesize");
  mmap_dimSymbol = install("dim");
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
  {"mmap_mmap",             (DL_FUNC) &mmap_mmap,               7},
  {"mmap_pagesize",         (DL_FUNC) &mmap_pagesize,           0},
  {"mmap_is_mmapped",       (DL_FUNC) &mmap_is_mmapped,         1},
  {"mmap_msync",            (DL_FUNC) &mmap_msync,              2},
  {"mmap_madvise",          (DL_FUNC) &mmap_madvise,            3},
  {"mmap_mprotect",         (DL_FUNC) &mmap_mprotect,           3},
  {"mmap_extract",          (DL_FUNC) &mmap_extract,            4},
  {"mmap_replace",          (DL_FUNC) &mmap_replace,            4},
  {"mmap_compare",          (DL_FUNC) &mmap_compare,            3},
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
