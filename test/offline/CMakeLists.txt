
add_executable(offline-test
        ${CMAKE_CURRENT_LIST_DIR}/offline_test_main.cpp)

target_link_libraries(offline-test common gtest decomp compiler)