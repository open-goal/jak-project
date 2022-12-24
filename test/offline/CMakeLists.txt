
add_executable(offline-test
        ${CMAKE_CURRENT_LIST_DIR}/config/config.cpp
        ${CMAKE_CURRENT_LIST_DIR}/framework/execution.cpp
        ${CMAKE_CURRENT_LIST_DIR}/framework/orchestration.cpp
        ${CMAKE_CURRENT_LIST_DIR}/framework/file_management.cpp
        ${CMAKE_CURRENT_LIST_DIR}/offline_test_main.cpp)

target_link_libraries(offline-test common gtest decomp compiler)

