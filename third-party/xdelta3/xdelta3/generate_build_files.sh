#!/bin/sh

aclocal &&
    autoreconf --install &&
    libtoolize &&
    autoconf &&
    automake --add-missing &&
    automake
