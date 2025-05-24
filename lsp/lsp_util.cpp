#include "lsp_util.h"

#include <sstream>

#include "common/util/string_util.h"

#include "fmt/core.h"

namespace lsp_util {

std::string url_encode(const std::string& value) {
  std::ostringstream escaped;
  escaped.fill('0');
  escaped << std::hex;

  for (std::string::const_iterator i = value.begin(), n = value.end(); i != n; ++i) {
    std::string::value_type c = (*i);

    // Keep alphanumeric and other accepted characters intact
    if (isalnum(c) || c == '-' || c == '_' || c == '.' || c == '~' || c == '/') {
      escaped << c;
      continue;
    }

    // Any other characters are percent-encoded
    escaped << std::uppercase;
    escaped << '%' << std::setw(2) << int((unsigned char)c);
    escaped << std::nouppercase;
  }

  return escaped.str();
}

std::string url_decode(const std::string& input) {
  std::ostringstream decoded;

  for (std::size_t i = 0; i < input.length(); ++i) {
    if (input[i] == '%') {
      // Check if there are enough characters remaining
      if (i + 2 < input.length()) {
        // Convert the next two characters after '%' into an integer value
        std::istringstream hexStream(input.substr(i + 1, 2));
        int hexValue = 0;
        hexStream >> std::hex >> hexValue;

        // Append the decoded character to the result
        decoded << static_cast<char>(hexValue);

        // Skip the next two characters
        i += 2;
      }
    } else if (input[i] == '+') {
      // Replace '+' with space character ' '
      decoded << ' ';
    } else {
      // Append the character as is
      decoded << input[i];
    }
  }

  return decoded.str();
}

LSPSpec::DocumentUri uri_from_path(fs::path path) {
  auto path_str = file_util::convert_to_unix_path_separators(path.string());
  // vscode works with proper URL encoded URIs for file paths
  // which means we have to roll our own...
  path_str = url_encode(path_str);
  return fmt::format("file:///{}", path_str);
}

std::string uri_to_path(const LSPSpec::DocumentUri& uri) {
  auto decoded_uri = url_decode(uri);
  if (str_util::starts_with(decoded_uri, "file:///")) {
#ifdef _WIN32
    decoded_uri = decoded_uri.substr(8);
#else
    decoded_uri = decoded_uri.substr(7);
#endif
  }
  return decoded_uri;
}
}  // namespace lsp_util
