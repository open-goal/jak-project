if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  set(CMAKE_CXX_FLAGS "-O3")
elseif(MSVC)
  set(CMAKE_CXX_FLAGS "/EHsc")
endif()

include_directories(../)
add_library(fmt format.cc)

if(BUILD_SHARED_LIBS)
  target_compile_definitions(fmt PRIVATE FMT_EXPORT INTERFACE FMT_SHARED PUBLIC FMT_SHARED)
endif()
