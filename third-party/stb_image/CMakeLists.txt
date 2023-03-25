if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  set(CMAKE_CXX_FLAGS "-O3")
elseif(MSVC)
  set(CMAKE_CXX_FLAGS "/EHsc")
endif()


add_library(stb_image stb_image.cpp)

