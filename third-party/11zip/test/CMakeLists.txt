cmake_minimum_required(VERSION 3.12)

include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG        703bd9caab50b139428cea1aaff9974ebee5742e # release-1.10.0
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

add_executable(UnitTests)

target_compile_features(UnitTests
    PUBLIC
        cxx_std_17
)

target_link_libraries(UnitTests
    PRIVATE
        elzip
        gtest_main
)

target_sources(UnitTests
    PRIVATE
        "src/unzipper_test.cpp"
)

add_custom_command(
    TARGET UnitTests POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/assets $<TARGET_FILE_DIR:UnitTests>/assets
)
add_custom_command(
    TARGET UnitTests POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:UnitTests> $<TARGET_FILE_DIR:UnitTests>/assets
)
add_custom_command(
    TARGET UnitTests POST_BUILD
    WORKING_DIRECTORY $<TARGET_FILE_DIR:UnitTests>/assets
    COMMAND ${CMAKE_COMMAND} -E tar cf patch.zip --format=zip --
    $<TARGET_FILE_NAME:UnitTests>
    test.txt
)

target_compile_definitions(UnitTests PRIVATE ASSETS_DIR="$<TARGET_FILE_DIR:UnitTests>/assets")
target_compile_definitions(UnitTests PRIVATE EXE_NAME="$<TARGET_FILE_NAME:UnitTests>")

add_test(NAME UnitTests COMMAND UnitTests)
