cmake_minimum_required(VERSION 3.10)
include(CMakePackageConfigHelpers)

project(lzokay VERSION 0.1 LANGUAGES CXX)
add_library(lzokay lzokay.hpp lzokay.cpp)
add_executable(lzokaytest test.cpp)
target_include_directories(lzokay PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
target_link_libraries(lzokaytest lzokay)
target_compile_features(lzokay PRIVATE cxx_std_14)
target_compile_features(lzokaytest PRIVATE cxx_std_14)
set_target_properties(lzokay PROPERTIES CXX_STANDARD 14)
set_target_properties(lzokaytest PROPERTIES CXX_STANDARD 14)

# Set installation for headers
install(
    FILES "${PROJECT_SOURCE_DIR}/lzokay.hpp"
    DESTINATION "include/lzokay"
)

set(version_config_file "${PROJECT_BINARY_DIR}/lzokayConfigVersion.cmake")
set(config_file "${PROJECT_BINARY_DIR}/lzokayConfig.cmake")
set(config_install_dir "lib/cmake/lzokay")

# Associate target with export
install(
    TARGETS lzokay
    EXPORT lzokayTargets
    ARCHIVE DESTINATION "lib"
    INCLUDES DESTINATION "include/lzokay"  # This sets the INTERFACE_INCLUDE_DIRECTORIES property of the target.
)

# Install the target config files
install(
    EXPORT lzokayTargets
    NAMESPACE "lzokay::"
    DESTINATION "${config_install_dir}"
)

# Generate version config file
write_basic_package_version_file(
    "${version_config_file}"
    COMPATIBILITY SameMajorVersion
)

# Generate config file
configure_package_config_file(
    "Config.cmake.in"
    "${config_file}"
    INSTALL_DESTINATION "lib/cmake/lzokay"
)

# Install the config files
install(
    FILES "${config_file}" "${version_config_file}"
    DESTINATION ${config_install_dir}
)

add_subdirectory("lzokay-c")
