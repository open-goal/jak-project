#include "game_text.h"

#include <algorithm>
#include <cstring>
#include <map>
#include <vector>

#include "game_subs.h"

#include "common/goos/Reader.h"
#include "common/util/BitUtils.h"
#include "common/util/FontUtils.h"
#include "common/util/print_float.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

#include "fmt/core.h"

namespace decompiler {
namespace {
template <typename T>
T get_word(const LinkedWord& word) {
  T result;
  ASSERT(word.kind() == LinkedWord::PLAIN_DATA);
  static_assert(sizeof(T) == 4, "bad get_word size");
  memcpy(&result, &word.data, 4);
  return result;
}

DecompilerLabel get_label(ObjectFileData& data, const LinkedWord& word) {
  ASSERT(word.kind() == LinkedWord::PTR);
  return data.linked_data.labels.at(word.label_id());
}

static const std::unordered_map<GameTextVersion, std::pair<int, int>> sTextCreditsIDs = {
    {GameTextVersion::JAK1_V1, {0xb00, 0xf00}},
    {GameTextVersion::JAK1_V2, {0xb00, 0xf00}}};

bool word_is_type(const LinkedWord& word, const std::string& type_name) {
  return word.kind() == LinkedWord::TYPE_PTR && word.symbol_name() == type_name;
}
}  // namespace

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

GameTextResult process_game_text(ObjectFileData& data, GameTextVersion version) {
  GameTextResult result;
  auto& words = data.linked_data.words_by_seg.at(0);
  std::vector<int> read_words(words.size(), false);

  int offset = 0;

  // type tag for game-text-info
  if (words.at(offset).kind() != LinkedWord::TYPE_PTR ||
      words.front().symbol_name() != "game-text-info") {
    ASSERT(false);
  }
  read_words.at(offset)++;
  offset++;

  // length field
  read_words.at(offset)++;
  u32 text_count = get_word<u32>(words.at(offset++));

  // language-id field
  read_words.at(offset)++;
  u32 language = get_word<u32>(words.at(offset++));
  result.language = language;

  // group-name field
  read_words.at(offset)++;
  auto group_label = get_label(data, words.at(offset++));
  auto group_name = data.linked_data.get_goal_string_by_label(group_label);
  ASSERT(group_name == "common");
  // remember that we read these bytes
  auto group_start = (group_label.offset / 4) - 1;
  for (int j = 0; j < align16(8 + 1 + (int)group_name.length()) / 4; j++) {
    read_words.at(group_start + j)++;
  }

  // read each text...
  for (u32 i = 0; i < text_count; i++) {
    // id number
    read_words.at(offset)++;
    auto text_id = get_word<u32>(words.at(offset++));

    // label to string
    read_words.at(offset)++;
    auto text_label = get_label(data, words.at(offset++));

    // actual string
    auto text = data.linked_data.get_goal_string_by_label(text_label);
    result.total_text++;
    result.total_chars += text.length();

    // no duplicate ids
    if (result.text.find(text_id) != result.text.end()) {
      ASSERT(false);
    }

    // escape characters
    if (font_bank_exists(version)) {
      result.text[text_id] = get_font_bank(version)->convert_game_to_utf8(text.c_str());
    } else {
      result.text[text_id] = goos::get_readable_string(text.c_str());  // HACK!
    }

    // remember what we read (-1 for the type tag)
    auto string_start = (text_label.offset / 4) - 1;
    // 8 for type tag and length fields, 1 for null char.
    for (int j = 0, m = align16(8 + 1 + (int)text.length()) / 4;
         j < m && string_start + j < (int)read_words.size(); j++) {
      read_words.at(string_start + j)++;
    }
  }

  // alignment to the string section.
  while (offset & 3) {
    read_words.at(offset)++;
    offset++;
  }

  // make sure we read each thing at least once.
  // reading more than once is ok, some text is duplicated.
  for (int i = 0; i < int(words.size()); i++) {
    if (read_words[i] < 1) {
      std::string debug;
      data.linked_data.append_word_to_string(debug, words.at(i));
      ASSERT_MSG(false, fmt::format("[{}] {} 0x{}", i, int(read_words[i]), debug.c_str()));
    }
  }

  return result;
}

std::string write_game_text(
    GameTextVersion version,
    const std::unordered_map<int, std::unordered_map<int, std::string>>& data) {
  // first sort languages:
  std::vector<int> languages;
  for (const auto& lang : data) {
    languages.push_back(lang.first);
  }
  std::sort(languages.begin(), languages.end());

  // build map
  std::map<int, std::vector<std::string>> text_by_id;
  std::map<int, std::vector<std::string>> text_by_id_credits;
  std::map<int, std::vector<std::string>> text_by_id_post_credits;
  int last_credits_id = 0;
  int credits_begin = 0;
  int credits_end = 0;
  if (auto it = sTextCreditsIDs.find(version); it != sTextCreditsIDs.end()) {
    credits_begin = it->second.first;
    credits_end = it->second.second;
  }
  for (auto lang : languages) {
    for (auto& [id, text] : data.at(lang)) {
      if (id < credits_begin) {
        // comes before credits
        text_by_id[id].push_back(text);
      } else if (id < credits_end) {
        // comes before credits
        text_by_id_credits[id].push_back(text);
        if (id > last_credits_id) {
          last_credits_id = id;
        }
      } else {
        // comes after credits
        text_by_id_post_credits[id].push_back(text);
      }
    }
  }

  // write!
  std::string result;  // = "\xEF\xBB\xBF";  // UTF-8 encode (don't need this anymore)
  result += "(group-name \"common\")\n";
  result += "(language-id";
  for (auto lang : languages) {
    result += fmt::format(" {}", lang);
  }
  result += ")\n";
  result += fmt::format("(text-version {})\n\n", get_text_version_name(version));
  for (auto& x : text_by_id) {
    result += fmt::format("(#x{:04x}\n  ", x.first);
    for (auto& y : x.second) {
      result += fmt::format("\"{}\"\n  ", y);
    }
    result += ")\n\n";
  }
  if (text_by_id_credits.size() > 0) {
    result += fmt::format("(credits :begin #x{:04x}\n  ", sTextCreditsIDs.at(version).first);

    for (int id = sTextCreditsIDs.at(version).first; id <= last_credits_id; ++id) {
      // check if the line exists first
      if (text_by_id_credits.count(id) == 0) {
        result += fmt::format("\"\"\n  ");
        continue;
      }
      // check if all lines are identical first
      bool diff_langs = false;
      bool is_first = true;
      std::string last_lang;
      for (auto& y : text_by_id_credits.at(id)) {
        if (is_first) {
          is_first = false;
        } else if (last_lang != y) {
          diff_langs = true;
          break;
        }
        last_lang = y;
      }
      // now write them
      if (!diff_langs) {
        result += fmt::format("\"{}\"\n  ", last_lang);
      } else {
        result += fmt::format("(");
        for (auto& y : text_by_id_credits.at(id)) {
          result += fmt::format("\"{}\"\n   ", y);
        }
        result += ")\n  ";
      }
    }
    result += ")\n\n";
  }
  for (auto& x : text_by_id_post_credits) {
    result += fmt::format("(#x{:04x}\n  ", x.first);
    for (auto& y : x.second) {
      result += fmt::format("\"{}\"\n  ", y);
    }
    result += ")\n\n";
  }

  return result;
}

/*
(defun unpack-comp-rle ((arg0 (pointer int8)) (arg1 (pointer int8)))
  (local-vars (v1-2 int) (v1-3 uint))
  (nop!)
  (loop
    (loop
      (set! v1-2 (-> arg1 0))
      (set! arg1 (&-> arg1 1))
      (b! (<= v1-2 0) cfg-5 :delay (nop!))
      (let ((a2-0 (-> arg1 0)))
        (set! arg1 (&-> arg1 1))
        (label cfg-3)
        (nop!)
        (nop!)
        (nop!)
        (nop!)
        (set! (-> arg0 0) a2-0)
        )
      (set! arg0 (&-> arg0 1))
      (b! (> v1-2 0) cfg-3 :delay (set! v1-2 (+ v1-2 -1)))
      )
    (label cfg-5)
    (b! (zero? v1-2) cfg-8 :delay (set! v1-3 (the-as uint (- v1-2))))
    (label cfg-6)
    (let ((a2-1 (-> arg1 0)))
      (set! arg1 (&-> arg1 1))
      (nop!)
      (nop!)
      (set! (-> arg0 0) a2-1)
      )
    (+! v1-3 -1)
    (b! (> (the-as int v1-3) 0) cfg-6 :delay (set! arg0 (&-> arg0 1)))
    )
  (label cfg-8)
  (none)
  )
*/
void unpack_comp_rle(s8* dst, s8* src) {
  while (true) {
    int amt;
    while (true) {
      // how many bytes to duplicate?
      amt = *(src++);
      // no more data or copy from src
      if (amt <= 0)
        break;
      // the byte to duplicate
      int dup = *(src++);
      do {
        *(dst++) = dup;
      } while (amt-- > 0);
    }
    // no more data
    if (amt == 0)
      return;
    // how many bytes to simply copy?
    int copy = -amt;
    do {
      *(dst++) = *(src++);
    } while (--copy > 0);
  }
}

/*
(deftype subtitle-range (basic)
  ((start-frame float :offset-assert 4)
   (end-frame   float :offset-assert 8)
   (message    basic 8 :offset-assert 12)
   )
  :method-count-assert 9
  :size-assert         #x2c
  :flag-assert         #x90000002c
  ;; Failed to read fields.
  )
 */

std::vector<SpoolSubtitleRange> process_spool_subtitles(ObjectFileData& data,
                                                        GameTextVersion version) {
  std::vector<SpoolSubtitleRange> result;
  auto& words = data.linked_data.words_by_seg.at(0);

  // type tag for art-group and get art count
  ASSERT(word_is_type(words.at(0), "art-group"));
  auto elt_count = get_word<s32>(words.at(3));

  for (int i = 0; i < elt_count; ++i) {
    auto art_label = get_label(data, words.at(8 + i));
    auto art_word_ofs = art_label.offset / 4;
    if (word_is_type(words.at(art_word_ofs - 1), "art-joint-anim")) {
      auto lump_for_art_word = words.at(art_word_ofs + 3);
      if (lump_for_art_word.kind() == LinkedWord::PTR) {
        // got lump
        auto lump_word_ofs = get_label(data, lump_for_art_word).offset / 4;
        ASSERT(word_is_type(words.at(lump_word_ofs - 1), "res-lump"));
        auto tag_amt = get_word<s32>(words.at(lump_word_ofs));
        auto data_base = get_label(data, words.at(lump_word_ofs + 2)).offset / 4;
        auto tag_ofs = get_label(data, words.at(lump_word_ofs + 6)).offset / 4;

        for (int t = 0; t < tag_amt; ++t) {
          // check for a specific kind of res-tag (name, frame, type and length)
          if (words.at(tag_ofs).symbol_name() == "subtitle-range" &&
              get_word<float>(words.at(tag_ofs + 1)) == -1000000000.0f &&
              word_is_type(words.at(tag_ofs + 2), "array") &&
              (get_word<u32>(words.at(tag_ofs + 3)) >> 16) == 0x1) {
            auto data_ofs = get_word<u32>(words.at(tag_ofs + 3)) & 0xffff;
            data_ofs = data_base + align4(data_ofs) / 4;
            // the res will be a (array subtitle-range)
            auto subtitle_array_ofs = get_label(data, words.at(data_ofs)).offset / 4;
            ASSERT(word_is_type(words.at(subtitle_array_ofs + 2), "subtitle-range"));
            auto subtitle_amount = get_word<s32>(words.at(subtitle_array_ofs));

            for (int s = 0; s < subtitle_amount; ++s) {
              auto subtitle_lbl = get_label(data, words.at(subtitle_array_ofs + 3 + s));
              auto subtitle_ofs = subtitle_lbl.offset / 4;
              // add our new subtitle range
              auto& subtitles = result.emplace_back();
              subtitles.start_frame = get_word<float>(words.at(subtitle_ofs));
              subtitles.end_frame = get_word<float>(words.at(subtitle_ofs + 1));

              for (int m = 0; m < 8; ++m) {
                // process the message for each language
                auto& msg = subtitles.message[m];
                const auto& msg_ptr = words.at(subtitle_ofs + 2 + m);
                if (msg_ptr.kind() == LinkedWord::Kind::SYM_PTR && msg_ptr.symbol_name() == "#f") {
                  msg.kind = SpoolSubtitleMessage::Kind::NIL;
                } else {
                  auto m_lbl = get_label(data, msg_ptr);
                  auto m_ofs = m_lbl.offset / 4;
                  auto m_type_w = words.at(m_ofs - 1);
                  ASSERT(m_type_w.kind() == LinkedWord::TYPE_PTR);
                  if (m_type_w.symbol_name() == "string") {
                    msg.kind = SpoolSubtitleMessage::Kind::STRING;
                    auto text = data.linked_data.get_goal_string_by_label(m_lbl);
                    // escape characters
                    if (font_bank_exists(version)) {
                      msg.text = get_font_bank(version)->convert_game_to_utf8(text.c_str());
                    } else {
                      msg.text = goos::get_readable_string(text.c_str());  // HACK!
                    }
                  } else if (m_type_w.symbol_name() == "subtitle-image") {
                    msg.kind = SpoolSubtitleMessage::Kind::IMAGE;
                    auto wh = get_word<u32>(words.at(m_ofs));
                    msg.w = wh;
                    msg.h = (wh >> 16) + 1;
                    msg.h--;  // correct height
                    for (int p = 0; p < 16; ++p) {
                      msg.palette[p] = get_word<u32>(words.at(m_ofs + 3 + p));
                    }

                    // unpack the image data. we have no idea what the input size is,
                    // so we just copy all of the plain data after this.
                    auto img_data_ofs = m_ofs + 3 + 16;
                    int img_data_top = words.size();
                    for (int check = img_data_ofs; check < img_data_top; ++check) {
                      if (words.at(check).kind() != LinkedWord::Kind::PLAIN_DATA) {
                        img_data_top = check;
                        break;
                      }
                    }
                    // it's a 4-bit image, meaning 2 pixels per byte, so we round up.
                    msg.data.resize(align2(msg.w * msg.h / 2));
                    std::vector<u8> input_data;
                    input_data.resize((img_data_top - img_data_ofs) * 4);
                    for (int copy = 0; copy < (img_data_top - img_data_ofs); ++copy) {
                      *((u32*)(input_data.data() + copy * 4)) =
                          get_word<u32>(words.at(img_data_ofs + copy));
                    }
                    // unpack now! hopefully there was enough input data...
                    unpack_comp_rle((s8*)msg.data.data(), (s8*)input_data.data());
                  } else {
                    ASSERT_MSG(false, fmt::format("unknown subtitle message type {}",
                                                  m_type_w.symbol_name()));
                  }
                }
              }
            }
          }
          tag_ofs += 4;
        }
      }
    }
  }

  return result;
}

std::string write_spool_subtitles(
    GameTextVersion,
    const fs::path& image_out,
    const std::unordered_map<std::string, std::vector<SpoolSubtitleRange>>& data) {
  // write!
  std::string result;  // = "\xEF\xBB\xBF";  // UTF-8 encode (don't need this anymore)

  bool dump_images = !image_out.empty();
  if (dump_images) {
    file_util::create_dir_if_needed(image_out);
  }

  constexpr bool as_json = true;
  if constexpr (as_json) {
    constexpr bool dump_text = false;
    constexpr int lang = 0;
    // no line data
    bool has_spools = false;
    for (auto& [spool_name, subs] : data) {
      result += "    \"" + spool_name + "\": {\n";
      // result += "      \"scene\": true,\n";
      result += "      \"lines\": [\n";
      bool has_subs = false;
      for (auto& sub : subs) {
        const auto& msg = sub.message[lang];
        if (msg.kind != SpoolSubtitleMessage::Kind::STRING) {
          continue;
        }
        result += "        {\n";
        result += "          \"frame_end\": " + float_to_string(sub.end_frame) + ",\n";
        result += "          \"frame_start\": " + float_to_string(sub.start_frame) + ",\n";
        if (dump_text) {
          result += "          \"merge\": false,\n";
        } else {
          result += "          \"merge\": true,\n";
        }
        result += "          \"offscreen\": false,\n";
        if (dump_text) {
          result += "          \"text\": \"" + msg.text + "\",\n";
        } else {
          // result += "          \"text\": \"\",\n";
        }
        result += "          \"speaker\": \"none\"\n";
        result += "        },\n";
        has_subs = true;
      }
      if (has_subs) {
        result.pop_back();
        result.pop_back();
        result.push_back('\n');
      }

      result += "      ]\n";
      result += "    },\n";
      has_spools = true;
    }
    if (has_spools) {
      result.pop_back();
      result.pop_back();
      result.push_back('\n');
    }
  } else {
    for (auto& [spool_name, subs] : data) {
      int image_count = 0;
      result += "(\"" + spool_name + "\"\n";
      for (auto& sub : subs) {
        std::string temp_for_indent = fmt::format("  (({} {}) (", float_to_string(sub.start_frame),
                                                  float_to_string(sub.end_frame));
        auto indent = temp_for_indent.length();
        result += temp_for_indent;
        for (int i = 0; i < 8; ++i) {
          const auto& msg = sub.message[i];
          if (i > 0) {
            result += "\n" + std::string(indent, ' ');
          }
          if (msg.kind == SpoolSubtitleMessage::Kind::NIL) {
            result += "#f";
          } else {
            result += "(";
            if (msg.kind == SpoolSubtitleMessage::Kind::IMAGE) {
              auto img_name = fmt::format("{}-{}-{}.png", spool_name, i, image_count++);
              result += fmt::format("image \"{}\"", img_name);
              if (dump_images) {
                std::vector<u32> rgba_out;
                rgba_out.resize(msg.w * msg.h);
                for (int px = 0; px < (int)rgba_out.size(); ++px) {
                  int idx = px & 1 ? msg.data[px / 2] >> 4 : msg.data[px / 2] & 0xf;
                  rgba_out.at(px) = msg.palette[idx];
                }
                file_util::write_rgba_png(image_out / img_name, rgba_out.data(), msg.w, msg.h);
              }
            } else if (msg.kind == SpoolSubtitleMessage::Kind::STRING) {
              result += "\"" + msg.text + "\"";
            }
            result += ")";
          }
        }
        result += ")\n   )\n";
      }
      result += "  )\n\n";
    }
  }

  return result;
}
}  // namespace decompiler
