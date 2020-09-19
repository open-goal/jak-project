add_library(goos SHARED Object.cpp TextDB.cpp Reader.cpp Interpreter.cpp InterpreterEval.cpp)
target_link_libraries(goos common_util)