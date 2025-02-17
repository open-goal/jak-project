if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  set(CMAKE_CXX_FLAGS "-O3")
elseif(MSVC)
  set(CMAKE_CXX_FLAGS "/EHsc")
endif()

include_directories(../ ../stb_image)
add_library(tiny_gltf tiny_gltf.cpp)
target_link_libraries(tiny_gltf draco::draco stb_image)

