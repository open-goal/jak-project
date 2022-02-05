project(catch)

file(GLOB CATCH_HEADERS include/catch/catch.hpp)

add_library(catch INTERFACE)
add_library(Catch2::Catch2 ALIAS catch)

target_include_directories(catch
    INTERFACE
        $<INSTALL_INTERFACE:include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)