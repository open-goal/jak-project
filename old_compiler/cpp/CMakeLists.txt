set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "-O0 -g -march=native -ggdb -Wall \
-Wextra -Wcast-align -Wcast-qual -Wdisabled-optimization -Wformat=2 \
-Winit-self -Wmissing-include-dirs -Woverloaded-virtual \
-Wredundant-decls -Wshadow -Wsign-promo ")

include_directories(./)
include_directories(../)

add_subdirectory(reader)
add_subdirectory(goos)
add_subdirectory(goal)
add_subdirectory(listener)
add_subdirectory(codegen)
add_subdirectory(regalloc)
add_subdirectory(logger)


#include_directories(../shared_config)
#include_directories(../third-party/cpp-linenoise)

add_executable(goalc_old main.cpp)
target_link_libraries(goalc_old reader_old goos_old goal listener_old codegen regalloc logger pthread)