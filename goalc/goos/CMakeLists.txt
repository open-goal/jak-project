add_library(goos STATIC Object.cpp TextDB.cpp Reader.cpp Interpreter.cpp InterpreterEval.cpp)
target_link_libraries(goos util)