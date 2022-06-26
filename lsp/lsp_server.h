#pragma once

#include <map>
#include <string>
#include <vector>

#include "types.h"

class LspServer {
 public:
  LspServer();
  bool TestServer(const std::string& id);
};
