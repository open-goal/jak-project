Goal:

Create a flexible test framework for testing GOAL code:
- pass in templated GOAL files to easily test many/large cases of tests
- probably need macros / helper functions around things like:
  - generating math expressions in post-fix (we need to know the result to assert it)
- if a test fails, need to print out the code / save it to a file so it can be inspected

The real selling point is being able to three-fold:
- Reduce the number of compiler test files, while still maintaining the same test-coverage
- Easily create stressful tests for the compiler, make test combinations
- Have the expected test result in the same place as the test code, no need to cross-reference .gc file
