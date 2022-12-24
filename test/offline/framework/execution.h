#pragma once

#include "orchestration.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "test/offline/config/config.h"

struct OfflineTestDecompiler {
  std::unique_ptr<decompiler::ObjectFileDB> db;
  std::unique_ptr<decompiler::Config> config;
};

void disassemble(OfflineTestDecompiler& dc);
void decompile(OfflineTestDecompiler& dc,
               const OfflineTestConfig& config,
               const std::shared_ptr<OfflineTestThreadStatus> status);
OfflineTestCompareResult compare(OfflineTestDecompiler& dc,
                                 const OfflineTestWorkGroup& work_group,
                                 const OfflineTestConfig& config);
OfflineTestCompileResult compile(OfflineTestDecompiler& dc,
                                 const OfflineTestWorkGroup& work_group,
                                 const OfflineTestConfig& config);
