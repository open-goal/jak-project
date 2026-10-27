#pragma once

#include <memory>

#include "common/util/FileUtil.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/config.h"

std::unique_ptr<decompiler::ObjectFileDB> load_object_file_db(decompiler::Config& config,
                                                              const fs::path& game_folder);
void run_object_type_analysis(decompiler::ObjectFileDB& db,
                              decompiler::Config& config,
                              decompiler::ObjectFileData& data);
bool type_exists(const TypeSystem& ts, const std::string& name);
std::string closest_label(const decompiler::LinkedObjectFile& file,
                          const decompiler::Function& func,
                          int seg,
                          int idx);
