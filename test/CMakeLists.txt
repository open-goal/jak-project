add_executable(goalc-test
        test_main.cpp
        test_test.cpp
        test_reader.cpp
        test_goos.cpp
        test_listener_deci2.cpp
        test_kernel.cpp
        all_jak1_symbols.cpp
        test_type_system.cpp
        test_CodeTester.cpp
        test_emitter_slow.cpp
        test_emitter_fast.cpp
        )

target_link_libraries(goalc-test goos util listener runtime emitter type_system gtest)