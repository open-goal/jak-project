#include <algorithm>
#include "game_text.h"
#include "common/goos/Reader.h"
#include "DataObjectGenerator.h"
#include "common/util/FileUtil.h"
#include "third-party/fmt/core.h"

namespace {
template <typename T>
void for_each_in_list(const goos::Object& list, const T& f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if (!iter->is_empty_list()) {
    throw std::runtime_error("Invalid list");
  }
}

int64_t get_int(const goos::Object& obj) {
  if (obj.is_int()) {
    return obj.integer_obj.value;
  }
  throw std::runtime_error(obj.print() + " was supposed to be an integer, but isn't");
}

const goos::Object& car(const goos::Object& x) {
  if (!x.is_pair()) {
    throw std::runtime_error("invalid pair");
  }

  return x.as_pair()->car;
}

const goos::Object& cdr(const goos::Object& x) {
  if (!x.is_pair()) {
    throw std::runtime_error("invalid pair");
  }

  return x.as_pair()->cdr;
}

std::vector<std::unordered_map<int, std::string>> parse(const goos::Object& data) {
  std::vector<std::unordered_map<int, std::string>> text;
  bool languages_set = false;
  for_each_in_list(data.as_pair()->cdr, [&](const goos::Object& obj) {
    if (obj.is_pair()) {
      auto head = obj.as_pair()->car;
      if (head.is_symbol() && head.as_symbol()->name == "language-count") {
        if (languages_set) {
          throw std::runtime_error("Languages has been set multiple times.");
        }

        text.resize(get_int(car(cdr(obj))));
        if (!cdr(cdr(obj)).is_empty_list()) {
          throw std::runtime_error("language-count has too many arguments");
        }
      } else if (head.is_int()) {
        int i = 0;
        int id = head.as_int();
        for_each_in_list(cdr(obj), [&](const goos::Object& entry) {
          if (i >= int(text.size())) {
            throw std::runtime_error(
                "String has too many entries. There should be one per language");
          }

          if (entry.is_string()) {
            auto& map = text.at(i);
            if (map.find(id) != map.end()) {
              throw std::runtime_error("Entry appears more than once");
            }

            map[id] = entry.as_string()->data;
          } else {
            throw std::runtime_error("Each entry must be a string");
          }

          i++;
        });
        if (i != int(text.size())) {
          throw std::runtime_error("String did not have an entry for each language");
        }
      } else {
        throw std::runtime_error("Invalid game text file entry: " + head.print());
      }
    } else {
      throw std::runtime_error("Invalid game text file");
    }
  });
  return text;
}

/*
(deftype game-text (structure)
  ((id   uint32  :offset-assert 0)
   (text basic   :offset-assert 4)
   )
  )

(deftype game-text-info (basic)
  ((length      int32            :offset-assert 4)
   (language-id int32            :offset-assert 8)
   (group-name  basic            :offset-assert 12)
   (data        game-text :dynamic :offset-assert 16)
   )
  )
 */

void compile(const std::vector<std::unordered_map<int, std::string>>& text) {
  std::vector<int> add_order;
  add_order.reserve(text.front().size());

  for (auto& x : text.front()) {
    add_order.push_back(x.first);
  }
  std::sort(add_order.begin(), add_order.end());

  for (int lang = 0; lang < int(text.size()); lang++) {
    DataObjectGenerator gen;
    gen.add_type_tag("game-text-info");  // type
    gen.add_word(text.front().size());   // length
    gen.add_word(lang);                  // language-id
    gen.add_ref_to_string("common");     // group-name

    // now add all the datas:
    for (auto id : add_order) {
      gen.add_word(id);                             // id
      gen.add_ref_to_string(text.at(lang).at(id));  // text
    }
    auto data = gen.generate_v2();
    file_util::write_binary_file(
        file_util::get_file_path({"out", fmt::format("{}COMMON.TXT", lang)}), data.data(),
        data.size());
  }
}
}  // namespace

void compile_game_text(const std::string& filename) {
  goos::Reader reader;
  auto code = reader.read_from_file({filename});
  printf("[Build Game Text] %s\n", filename.c_str());
  auto text_map = parse(code);
  compile(text_map);
}
