cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)
project(json-rpc-cxx VERSION 0.0.1 LANGUAGES CXX)


option(COMPILE_TESTS "Enable tests" ON)
option(COMPILE_EXAMPLES "Enable examples" ON)
option(CODE_COVERAGE "Enable coverage reporting" OFF)

include(GNUInstallDirs)

add_library(json-rpc-cxx INTERFACE)
target_include_directories(json-rpc-cxx INTERFACE include)
install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

add_library(coverage_config INTERFACE)

# Warning options for the compiler
string(
        APPEND _warning_opts
        "$<$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>,$<CXX_COMPILER_ID:GNU>>:-Wall;-Wextra;-Weffc++;-Werror;>"
        "$<$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>>:-Wthread-safety;-Wpedantic;>"
        "$<$<CXX_COMPILER_ID:GNU>:-pedantic;-pedantic-errors;>"
)


if (COMPILE_TESTS)
    if (CODE_COVERAGE)
        message("Enabled coverage flags")
        target_compile_options(coverage_config INTERFACE -O0 -g --coverage)
        target_link_libraries(coverage_config INTERFACE --coverage)
    endif ()
    add_executable(jsonrpccpp-test test/main.cpp test/client.cpp test/typemapper.cpp test/dispatcher.cpp test/server.cpp test/batchclient.cpp test/testclientconnector.hpp examples/warehouse/warehouseapp.cpp test/warehouseapp.cpp test/common.cpp)
    target_compile_options(jsonrpccpp-test PUBLIC "${_warning_opts}")
    target_include_directories(jsonrpccpp-test PRIVATE vendor examples)
    target_link_libraries(jsonrpccpp-test coverage_config json-rpc-cxx)
    enable_testing()
    add_test(NAME test COMMAND jsonrpccpp-test)
endif ()

if (COMPILE_EXAMPLES)
    find_package(Threads)
    add_executable(example-warehouse examples/warehouse/main.cpp examples/warehouse/warehouseapp.cpp examples/warehouse/types.h examples/inmemoryconnector.hpp)
    target_compile_options(example-warehouse PUBLIC "${_warning_opts}")
    target_link_libraries(example-warehouse json-rpc-cxx Threads::Threads)
    target_include_directories(example-warehouse SYSTEM PRIVATE vendor)
    target_include_directories(example-warehouse PRIVATE examples)
    add_test(NAME example COMMAND example-warehouse)
endif ()
