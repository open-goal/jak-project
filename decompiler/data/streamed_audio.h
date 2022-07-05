#pragma once

#include "common/util/FileUtil.h"
#include <string>
#include <vector>

namespace decompiler {
void process_streamed_audio(const fs::path& output_path,
                            const fs::path& input_dir,
                            const std::vector<std::string>& audio_files);
}
