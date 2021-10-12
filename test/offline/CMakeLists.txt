
add_executable(offline-test
        ${CMAKE_CURRENT_LIST_DIR}/offline_test_main.cpp)

target_link_libraries(offline-test common gtest decomp compiler)

add_executable(offline-test2
        ${CMAKE_CURRENT_LIST_DIR}/offline_test_main2.cpp)

target_link_libraries(offline-test2 common gtest decomp compiler)