ACLOCAL_AMFLAGS = -I m4
AUTOMAKE_OPTIONS = subdir-objects

bin_PROGRAMS = xdelta3
noinst_PROGRAMS = xdelta3regtest xdelta3decode xdelta3checksum

export AFL_HARDEN

common_SOURCES = \
	  xdelta3-blkcache.h \
	  xdelta3-decode.h \
	  xdelta3-djw.h \
	  xdelta3-fgk.h \
	  xdelta3-hash.h \
	  xdelta3-internal.h \
	  xdelta3-list.h \
	  xdelta3-lzma.h \
	  xdelta3-main.h \
	  xdelta3-merge.h \
	  xdelta3-second.h \
	  xdelta3-test.h \
          xdelta3-cfgs.h \
	  xdelta3.h

xdelta3_SOURCES = $(common_SOURCES) xdelta3.c

xdelta3decode_SOURCES = $(common_SOURCES) xdelta3.c

xdelta3regtest_SOURCES = $(common_SOURCES) \
	testing/cmp.h \
	testing/delta.h \
	testing/file.h \
	testing/modify.h \
	testing/random.h \
	testing/regtest.cc \
	testing/regtest_c.c \
	testing/segment.h \
	testing/sizes.h \
	testing/test.h

xdelta3checksum_SOURCES = $(common_SOURCES) \
	testing/checksum_test.cc \
	testing/checksum_test_c.c

# These sources constitute a regression test written in Go, that is
# not automatically built or run.  Install Go-1.5.x or later, add
# `pwd`/go in $GOPATH, and (cd go/src && go run regtest.go).
# TODO(jmacd): replace hard-coded path names in regtest.go w/ flags.
GOLANG_SRCS = \
	go/src/xdelta/test.go \
	go/src/xdelta/rstream.go \
	go/src/xdelta/tgroup.go \
	go/src/xdelta/run.go \
	go/src/regtest.go

# Note: for extra sanity checks, enable -Wconversion. Note there
# are a lot of false positives.
WFLAGS = -Wall -Wshadow -fno-builtin -Wextra -Wsign-compare \
	 -Wformat=2 -Wno-format-nonliteral \
	 -Wno-unused-parameter -Wno-unused-function

 # -Weverything \
 # -Wc++11-compat-reserved-user-defined-literal \
 # -Wno-padded \
 # -Wno-format-nonliteral \
 # -Wno-cast-align \
 # -Wno-unused-parameter \
 # -Wno-sign-conversion \
 # -Wno-conversion \
 # -Wno-switch-enum \
 # -Wno-covered-switch-default \
 # -Wno-disabled-macro-expansion \
 # -Wno-variadic-macros \
 # -Wno-c++98-compat-pedantic

C_WFLAGS = $(WFLAGS) -std=c99
CXX_WFLAGS = $(WFLAGS) -std=c++11

common_CFLAGS = \
	      -DREGRESSION_TEST=1 \
	      -DSECONDARY_DJW=1 \
	      -DSECONDARY_FGK=1 \
	      -DXD3_MAIN=1

if DEBUG_SYMBOLS
  common_CFLAGS += -g
endif

#common_CFLAGS += -fsanitize=address -fno-omit-frame-pointer
#common_CFLAGS += -O2

# For additional debugging, add -DXD3_DEBUG=1, 2, 3, ...
xdelta3_CFLAGS = $(C_WFLAGS) $(common_CFLAGS) -DXD3_DEBUG=0
xdelta3_LDADD = -lm

xdelta3decode_CFLAGS = \
	$(C_WFLAGS) \
	-DREGRESSION_TEST=0 \
	-DSECONDARY_DJW=0 \
	-DSECONDARY_FGK=0 \
	-DSECONDARY_LZMA=0 \
	-DXD3_MAIN=1 \
	-DXD3_ENCODER=0 \
	-DXD3_STDIO=1 \
	-DEXTERNAL_COMPRESSION=0 \
	-DVCDIFF_TOOLS=0

xdelta3regtest_CXXFLAGS = \
	$(CXX_WFLAGS) $(common_CFLAGS) -DNOT_MAIN=1 -DXD3_DEBUG=1
xdelta3regtest_CFLAGS = \
	$(C_WFLAGS) $(common_CFLAGS) -DNOT_MAIN=1 -DXD3_DEBUG=1
xdelta3regtest_LDADD = -lm

xdelta3checksum_CXXFLAGS = \
	$(CXX_WFLAGS) $(common_CFLAGS) -DNOT_MAIN=1 -DXD3_MAIN=1 -std=c++11
xdelta3checksum_CFLAGS = \
	$(C_WFLAGS) $(common_CFLAGS) -DNOT_MAIN=1 -DXD3_MAIN=1


man1_MANS = xdelta3.1

EXTRA_DIST = \
	README.md \
	run_release.sh \
	draft-korn-vcdiff.txt \
	$(GOLANG_SRCS) \
	examples/Makefile \
	examples/README.md \
	examples/compare_test.c \
	examples/encode_decode_test.c \
	examples/small_page_test.c \
	examples/speed_test.c \
	examples/test.h \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test.xcodeproj/project.pbxproj \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/Xd3iOSAppDelegate.h \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/Xd3iOSAppDelegate.m \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/Xd3iOSViewController.h \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/Xd3iOSViewController.m \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/en.lproj/InfoPlist.strings \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/en.lproj/MainStoryboard_iPad.storyboard \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/en.lproj/MainStoryboard_iPhone.storyboard \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/file_v1.bin \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/file_v1_to_v2.bin \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/file_v2.bin \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/main.m \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/xdelta3-ios-test-Info.plist \
	examples/iOS/xdelta3-ios-test/xdelta3-ios-test/xdelta3-ios-test-Prefix.pch \
	cpp-btree/CMakeLists.txt \
	cpp-btree/COPYING \
	cpp-btree/README \
	cpp-btree/btree.h \
	cpp-btree/btree_bench.cc \
	cpp-btree/btree_container.h \
	cpp-btree/btree_map.h \
	cpp-btree/btree_set.h \
	cpp-btree/btree_test.cc \
	cpp-btree/btree_test.h \
	cpp-btree/btree_test_flags.cc \
	cpp-btree/safe_btree.h \
	cpp-btree/safe_btree_map.h \
	cpp-btree/safe_btree_set.h \
	cpp-btree/safe_btree_test.cc \
	testing/xdelta3-regtest.py \
	testing/xdelta3-test.py \
	xdelta3.1 \
	xdelta3.i \
	xdelta3.vcxproj \
	xdelta3.wxi \
	xdelta3.wxs

# Broken, removed from distribution:
#	xdelta3_pywrap.c
#	xdelta3.py

#PYFILES = xdelta3_pywrap.c xdelta3.py
#XDELTA3PY = xdelta3.py
#XDELTA3PYLIB = xdelta3.la

#BUILT_SOURCES = $(PYFILES)

#xdelta3_pywrap.c xdelta3.py : xdelta3.i
#	$(SWIG) -python -o xdelta3_pywrap.c xdelta3.i

# OS X for some reason requires:
# pythondir = $(PYTHON_SITE_PKG)
# pyexecdir = $(PYTHON_SITE_PKG)

#python_PYTHON = $(XDELTA3PY)
#pyexec_LTLIBRARIES = $(XDELTA3PYLIB)
#_xdelta3_la_SOURCES = $(srcdir)/xdelta3_pywrap.c $(xdelta3_SOURCES)
#_xdelta3_la_CFLAGS = $(common_CFLAGS) -DNOT_MAIN=1 $(PYTHON_CPPFLAGS)
#_xdelta3_la_LDFLAGS = -module
