# TODO - order matters, unfortunately, the kernel tests write to the filesystem and
# other tests depend on that, solve that someday, maybe

if(CMAKE_APPLE_SILICON_PROCESSOR STREQUAL "arm64")
    set(GOALC_TEST_CASES
        # ${CMAKE_CURRENT_LIST_DIR}/test_goal_kernel.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_goal_kernel2.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_goal_kernel3.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_CodeTester.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_arithmetic.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_collections.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_compiler.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_control_statements.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_debugger.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_game_no_debug.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_jak2_compiler.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_variables.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_with_game.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_type_consistency.cpp
        # ${CMAKE_CURRENT_LIST_DIR}/test_vector_float.cpp
    )
else()
    set(GOALC_TEST_CASES
        ${CMAKE_CURRENT_LIST_DIR}/test_goal_kernel.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_goal_kernel2.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_goal_kernel3.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_CodeTester.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_arithmetic.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_collections.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_compiler.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_control_statements.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_debugger.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_game_no_debug.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_jak2_compiler.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_variables.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_with_game.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_type_consistency.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_vector_float.cpp
    )
endif()

set(GOALC_TEST_FRAMEWORK_SOURCES
    ${CMAKE_CURRENT_LIST_DIR}/framework/test_runner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/framework/test_runner.h
)
