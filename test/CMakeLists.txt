set(CMAKE_CXX_STANDARD 17)

include(${CMAKE_CURRENT_LIST_DIR}/goalc/CMakeLists.txt)
include(${CMAKE_CURRENT_LIST_DIR}/offline/CMakeLists.txt)

add_executable(goalc-test
        ${CMAKE_CURRENT_LIST_DIR}/test_main.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_reader.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_goos.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_listener_deci2.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_kernel_jak1.cpp
        ${CMAKE_CURRENT_LIST_DIR}/all_jak1_symbols.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_type_system.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_CodeTester.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_emitter.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_emitter_avx.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_common_util.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_pretty_print.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_math.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_zstd.cpp
        ${CMAKE_CURRENT_LIST_DIR}/test_zydis.cpp
        ${CMAKE_CURRENT_LIST_DIR}/goalc/test_goal_kernel.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/FormRegressionTest.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_AtomicOpBuilder.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_FormBeforeExpressions.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_FormExpressionBuild.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_FormExpressionBuild2.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_FormExpressionBuild3.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_FormExpressionBuildLong.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_InstructionDecode.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_InstructionParser.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_gkernel_jak1_decomp.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_math_decomp.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_DataParser.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_DisasmVifDecompile.cpp
        ${CMAKE_CURRENT_LIST_DIR}/decompiler/test_VuDisasm.cpp
        ${CMAKE_CURRENT_LIST_DIR}/common/formatter/test_formatter.cpp
        ${GOALC_TEST_FRAMEWORK_SOURCES}
        ${GOALC_TEST_CASES}
        )

target_link_libraries(goalc-test common runtime compiler gtest decomp Zydis libzstd_static tree-sitter)

if(WIN32)
  target_link_libraries(goalc-test mman)
endif()

#gtest_discover_tests(goalc-test WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

if(UNIX AND CMAKE_COMPILER_IS_GNUCXX AND CODE_COVERAGE)
  set(BUILD_ARGS "-w" "dupbuild=warn")
  include(CodeCoverage)
  append_coverage_compiler_flags()
  setup_target_for_coverage_lcov(NAME goalc-test_coverage
                                 EXECUTABLE goalc-test --gtest_color=yes
                                 DEPENDENCIES goalc-test
                                 EXCLUDE "third-party/*" "/usr/include/*")
endif()
