#ifndef COMPILER_GOOSTEST_H
#define COMPILER_GOOSTEST_H

#include <string>
#include "goos/Goos.h"

class GoosTest {
 public:
  GoosTest(const std::string& filename);
  Goos goos;
  bool run();

  std::string name;
  std::string description;

  Object expected, actual;

 private:
  std::string file;
};

bool run_all_tests();

#endif  // COMPILER_GOOSTEST_H
