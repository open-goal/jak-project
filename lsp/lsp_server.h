#pragma once

#include <map>
#include <string>
#include <vector>

#include "types.h"
#include <decompiler/util/DecompilerTypeSystem.h>


class LspServer {
 public:
  LspServer();

  decompiler::DecompilerTypeSystem m_dts;

  // Handlers
  bool TestServer(const std::string& id);
};
