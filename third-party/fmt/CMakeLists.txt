if (CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_CXX_FLAGS "-O3")
else ()
  set(CMAKE_CXX_FLAGS "/EHsc")
endif (CMAKE_COMPILER_IS_GNUCXX)

include_directories(../)
add_library(fmt SHARED format.cc)