set(GOALC_TEST_CASES
    ${CMAKE_CURRENT_LIST_DIR}/all_goalc_template_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/test_debugger.cpp
    ${CMAKE_CURRENT_LIST_DIR}/test_game_no_debug.cpp
)

set(GOALC_TEST_FRAMEWORK_SOURCES
    ${CMAKE_CURRENT_LIST_DIR}/framework/test_runner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/framework/test_runner.h
)
