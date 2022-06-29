#pragma once

#include <filesystem>
#include <string>
#include <vector>

namespace decompiler {
void process_streamed_audio(const std::filesystem::path& output_path,
                            const std::filesystem::path& input_dir,
                            const std::vector<std::string>& audio_files);
}