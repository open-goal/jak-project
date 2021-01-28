#include <cassert>
#include <cstring>
#include "dgo_util.h"

/*!
 * Assert false if the char[] has non-null data after the null terminated string.
 * Used to sanity check the sizes of strings in DGO/object file headers.
 */
void assert_string_empty_after(const char* str, int size) {
  auto ptr = str;
  while (*ptr)
    ptr++;
  while (ptr - str < size) {
    assert(!*ptr);
    ptr++;
  }
}

std::string get_object_file_name(const std::string& original_name, u8* data, int size) {
  const char art_group_text[] =
      "/src/next/data/art-group6/";  // todo, this may change in other games
  const char suffix[] = "-ag.go";

  int len = int(strlen(art_group_text));
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
          assert(false);
        }
      }

      assert(int(strlen(suffix)) + start + len + int(original_name.length()) < size);
      assert(!memcmp(data + start + len + original_name.length(), suffix, strlen(suffix) + 1));

      return original_name + "-ag";
    }
  }

  return original_name;
}