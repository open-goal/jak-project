#pragma once

#include "config.h"

int run_decompilation_process(decompiler::Config config,
                              const fs::path& in_folder,
                              const fs::path& out_folder,
                              const bool minimal_for_extractor);
