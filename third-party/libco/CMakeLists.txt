set(CMAKE_C_STANDARD 11)

set(LIBCO_SOURCES
  libco.c
  )

add_library(libco STATIC ${LIBCO_SOURCES})
