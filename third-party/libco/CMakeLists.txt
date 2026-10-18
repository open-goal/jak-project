add_library(libco STATIC libco.c)

string(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} LOWERCASE_CMAKE_SYSTEM_PROCESSOR)

if(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "(i[3-6]86|x86)")
  target_sources(libco PRIVATE x86.c)
elseif(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "(x64|x86_64|amd64|e2k)")
  target_sources(libco PRIVATE amd64.c)
elseif(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "^(powerpc|ppc)64(le)?")
  target_sources(libco PRIVATE ppc64v2.c)
elseif(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "(riscv)")
  target_sources(libco PRIVATE riscv.c)
elseif(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "(aarch64|arm64)")
  target_sources(libco PRIVATE aarch64.c)
elseif(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "(arm)")
  target_sources(libco PRIVATE arm.c)
elseif(LOWERCASE_CMAKE_SYSTEM_PROCESSOR MATCHES "(ppc)")
  target_sources(libco PRIVATE ppc.c)
elseif(OS_WINDOWS)
  target_sources(libco PRIVATE fiber.c)
else()
  target_sources(libco PRIVATE sjlj.c)
endif()

target_sources(libco PRIVATE libco.h settings.h)

get_target_property(libco_SOURCES libco SOURCES)

target_include_directories(libco INTERFACE ..)

target_compile_options(libco PRIVATE $<$<CXX_COMPILER_ID:Clang,AppleClang>:-Wno-strict-prototypes>)

set_source_files_properties(libco ${libco_SOURCES} PROPERTIES HEADER_FILE_ONLY TRUE)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${libco_SOURCES})

set_source_files_properties(libco libco.c PROPERTIES HEADER_FILE_ONLY FALSE)
