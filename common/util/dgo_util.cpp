#include "dgo_util.h"

#include <cstring>

#include "common/util/Assert.h"
#include "common/versions/versions.h"

#include "third-party/fmt/core.h"

/*!
 * Assert false if the char[] has non-null data after the null terminated string.
 * Used to sanity check the sizes of strings in DGO/object file headers.
 */
void assert_string_empty_after(const char* str, int size) {
  auto ptr = str;
  while (*ptr)
    ptr++;
  while (ptr - str < size) {
    ASSERT(!*ptr);
    ptr++;
  }
}

std::string get_object_file_name(const std::string& original_name, u8* data, int size) {
  const std::string art_group_text_strings[] = {
      fmt::format("/src/next/data/art-group{}/", versions::jak1::ART_FILE_VERSION),
      fmt::format("/src/jak2/final/art-group{}/", versions::jak2::ART_FILE_VERSION),
      fmt::format("/src/jak3/final/art-group{}/", versions::jak3::ART_FILE_VERSION)};
  const std::string suffix = "-ag.go";

  for (auto& art_group_text : art_group_text_strings) {
    int len = int(art_group_text.length());
    for (int start = 0; start < size; start++) {
      bool failed = false;
      for (int i = 0; i < len; i++) {
        if (start + i >= size || data[start + i] != art_group_text[i]) {
          failed = true;
          break;
        }
      }

      if (!failed) {
        for (int i = 0; i < int(original_name.length()); i++) {
          if (start + len + i >= size || data[start + len + i] != original_name[i]) {
            ASSERT(false);
          }
        }

        ASSERT(int(suffix.length()) + start + len + int(original_name.length()) < size);
        ASSERT(!memcmp(data + start + len + original_name.length(), suffix.data(),
                       suffix.length() + 1));

        return original_name + "-ag";
      }
    }
  }

  return original_name;
}
