include("goalc/CMakeLists.txt")

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
        test_common_util.cpp
				${GOALC_TEST_FRAMEWORK_SOURCES}
				${GOALC_TEST_CASES})

enable_testing()

IF (WIN32)
	set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

	# TODO - split out these declarations for platform specific includes
  target_link_libraries(goalc-test cross_sockets listener mman goos common_util runtime compiler type_system gtest)
ELSE()
  target_link_libraries(goalc-test cross_sockets goos common_util listener runtime compiler type_system gtest)
ENDIF()

if(CMAKE_COMPILER_IS_GNUCXX AND CODE_COVERAGE)
  include(CodeCoverage)
  append_coverage_compiler_flags()
  setup_target_for_coverage_lcov(NAME goalc-test_coverage
                                 EXECUTABLE goalc-test --gtest_color=yes
                                 DEPENDENCIES goalc-test
                                 EXCLUDE "third-party/*" "/usr/include/*" "decompiler/*")
endif()
