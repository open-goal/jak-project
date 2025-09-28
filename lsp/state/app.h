#pragma once

#include "lsp/state/workspace.h"

// TODO - remove this, not really benefiting (never going to have multiple appstates)
struct AppState {
  Workspace workspace;
  bool verbose;
};
