enable_testing()

add_executable(goalc-test
        test_main.cpp
        test_test.cpp
        test_reader.cpp
        test_goos.cpp
        test_listener_deci2.cpp
				all_jak1_symbols.cpp
        test_kernel.cpp
        test_CodeTester.cpp
        test_type_system.cpp
)

IF (WIN32)
	set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  # TODO - implement windows listener
  message("Windows Listener Not Implemented!")
  target_link_libraries(goalc-test mman goos util runtime emitter type_system gtest)
ELSE()
  target_link_libraries(goalc-test goos util listener runtime emitter type_system gtest)
ENDIF()
