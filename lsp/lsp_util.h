#pragma once
#include <string>

#include "common/util/FileUtil.h"

#include "protocol/common_types.h"

namespace lsp_util {
std::string url_encode(const std::string& value);
std::string url_decode(const std::string& input);
LSPSpec::DocumentUri uri_from_path(fs::path path);
std::string uri_to_path(const LSPSpec::DocumentUri& uri);
};  // namespace lsp_util
