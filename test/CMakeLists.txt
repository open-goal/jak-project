set(CMAKE_CXX_STANDARD 17)

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
        test_emitter.cpp
        test_common_util.cpp
        test_pretty_print.cpp
        test_zydis.cpp
        ${GOALC_TEST_FRAMEWORK_SOURCES}
        ${GOALC_TEST_CASES})

enable_testing()

IF (WIN32)
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

  # TODO - split out these declarations for platform specific includes
  target_link_libraries(goalc-test cross_sockets goos common_util runtime compiler type_system gtest mman Zydis)
ELSE()
  target_link_libraries(goalc-test cross_sockets goos common_util runtime compiler type_system gtest Zydis)
ENDIF()

if(UNIX AND CODE_COVERAGE)
  include(CodeCoverage)
  append_coverage_compiler_flags()
  setup_target_for_coverage_lcov(NAME goalc-test_coverage
                                 EXECUTABLE goalc-test --gtest_color=yes
                                 DEPENDENCIES goalc-test
                                 EXCLUDE "third-party/*" "/usr/include/*" "decompiler/*")
endif()
