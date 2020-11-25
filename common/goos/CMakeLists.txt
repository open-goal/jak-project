
IF (WIN32)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2")
ELSE()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
ENDIF()

add_library(goos SHARED Object.cpp ParseHelpers.cpp TextDB.cpp Reader.cpp Interpreter.cpp PrettyPrinter.cpp ParseHelpers.cpp ParseHelpers.h)
target_link_libraries(goos common_util fmt)