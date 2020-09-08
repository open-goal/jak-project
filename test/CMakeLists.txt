enable_testing()

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
        test_emitter_loads_and_store.cpp
        test_emitter_xmm32.cpp
        test_emitter_integer_math.cpp
        test_compiler_and_runtime.cpp
        )

IF (WIN32)
	set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
	# TODO - split out these declarations for platform specific includes
  target_link_libraries(goalc-test cross_sockets listener mman goos util runtime compiler type_system gtest)
ELSE()
  target_link_libraries(goalc-test cross_sockets goos util listener runtime compiler type_system gtest)
ENDIF()
