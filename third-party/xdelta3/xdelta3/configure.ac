AC_INIT([Xdelta3], [3.1.1], [josh.macdonald@gmail.com], 
	[xdelta3], [http://xdelta.org/])
AC_PREREQ([2.68])
AC_CONFIG_MACRO_DIR([m4])
LT_INIT
AM_INIT_AUTOMAKE([1.15 no-define foreign tar-ustar subdir-objects])
AC_CONFIG_MACRO_DIRS([m4])

AX_CHECK_ALIGNED_ACCESS_REQUIRED
AC_PROG_CC
AC_PROG_CXX

AC_CHECK_SIZEOF(size_t)
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(unsigned long long)

AC_ARG_WITH(
    [liblzma],
    [AC_HELP_STRING(
        [--with-liblzma],
        [build with liblzma support @<:@default=autodetect@:>@])],
    [USE_LIBLZMA=$withval],
    [USE_LIBLZMA=auto])

if test "x$USE_LIBLZMA" != xno ; then
    AC_CHECK_HEADERS([lzma.h],,[
        if test "x$with_liblzma" = xyes ; then
            AC_MSG_FAILURE([liblzma includes were not found])
        fi])
    AC_CHECK_LIB([lzma], [lzma_easy_buffer_encode],,[
        if test "x$with_liblzma" = xyes ; then
            AC_MSG_FAILURE([liblzma library were not found])
        fi])
fi

#AM_PATH_PYTHON(,, [:])
#AM_CONDITIONAL([HAVE_PYTHON], [test "$PYTHON" != :])
#AX_PYTHON_DEVEL()
#AX_PKG_SWIG(2.0.0,,)
#AX_SWIG_PYTHON

dnl --enable-debug-symbols : build with debug symbols?
AC_ARG_ENABLE(debug-symbols,
   AS_HELP_STRING(--enable-debug-symbols,[Build with debug symbols (default is NO)]),,enableval=no)
AM_CONDITIONAL([DEBUG_SYMBOLS], [test ${enableval} = "yes"])


AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
