if (UNIX)
  set(CMAKE_CXX_FLAGS "-O3")
else ()
  set(CMAKE_CXX_FLAGS "/EHsc")
endif (UNIX)

include_directories(../)
add_library(fmt format.cc)

if(BUILD_SHARED_LIBS)
  target_compile_definitions(fmt PRIVATE FMT_EXPORT INTERFACE FMT_SHARED PUBLIC FMT_SHARED)
endif()
