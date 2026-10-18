gtest_dep = dependency('gtest', required: tests)

tests_req = gtest_dep.found() and add_languages('cpp', native: false, required: tests)

if tests_req
  test(
    'string',
    executable(
      'test_string',
      'String.cpp',
      dependencies: [gtest_dep, zycore_dep],
    ),
    protocol: 'gtest',
  )
  test(
    'vector',
    executable(
      'test_vector',
      'Vector.cpp',
      dependencies: [gtest_dep, zycore_dep],
    ),
    protocol: 'gtest',
  )
  test(
    'argparse',
    executable(
      'test_argparse',
      'ArgParse.cpp',
      dependencies: [gtest_dep, zycore_dep],
    ),
    protocol: 'gtest',
  )
endif

summary(
  {'tests': tests_req},
  section: 'Features',
)
