
IF (WIN32)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2")
ELSE()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
ENDIF()

add_library(goos SHARED Object.cpp TextDB.cpp Reader.cpp Interpreter.cpp PrettyPrinter.cpp)
target_link_libraries(goos common_util fmt)