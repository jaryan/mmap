AC_INIT([mmap], [0.6-21])

AC_CONFIG_AUX_DIR([tools])


: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
if test -z "${CC}"; then
    AC_MSG_ERROR([No C compiler available])
fi

AC_CONFIG_HEADERS([src/config.h])
AC_FUNC_MMAP
AC_CHECK_ALIGNOF(char)
AC_CHECK_ALIGNOF(int)
AC_CHECK_ALIGNOF(short)
AC_CHECK_ALIGNOF(long)
AC_CHECK_ALIGNOF(double)
AC_CHECK_FUNCS(madvise)
AC_CHECK_FUNCS(strnlen)
AC_OUTPUT
