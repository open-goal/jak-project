#pragma once

#include <string>
#include <vector>

namespace decompiler {
void process_streamed_audio(const std::string& dir, const std::vector<std::string>& audio_files);
}