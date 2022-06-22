if (UNIX)
    set(CMAKE_CXX_FLAGS "-O3")
else ()
    set(CMAKE_CXX_FLAGS "/EHsc")
endif (UNIX)


add_library(stb_image stb_image.cpp)

