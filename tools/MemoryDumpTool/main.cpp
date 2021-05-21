#include <string>
#include <cassert>
#include "third-party/fmt/core.h"
#include "third-party/11zip/include/elzip/elzip.hpp"

#include "common/util/FileUtil.h"
#include "common/goal_constants.h"
#include "common/symbols.h"
#include "common/type_system/TypeSystem.h"

#include "decompiler/util/DecompilerTypeSystem.h"

struct Ram {
  const u8* data = nullptr;
  u32 size = 0;

  Ram(const u8* _data, u32 _size) : data(_data), size(_size) {}

  template <typename T>
  T read(u32 addr) const {
    assert(in_memory<T>(addr));
    T result;
    memcpy(&result, data + addr, sizeof(T));
    return result;
  }

  template <typename T>
  bool in_memory(u32 addr) const {
    return addr > (1 << 19) && addr <= (size - sizeof(T));
  }

  u32 word(u32 addr) const { return read<u32>(addr); }

  u8 byte(int addr) const { return read<u8>(addr); }

  std::string string(u32 addr) const {
    std::string result;
    while (true) {
      assert(in_memory<u8>(addr));
      auto next = read<u8>(addr++);
      if (next) {
        result.push_back(next);
      } else {
        return result;
      }
    }
  }

  std::optional<std::string> try_string(u32 addr, int max_len = 128) const {
    std::string result;
    for (int i = 0; i < max_len; i++) {
      if (!in_memory<u8>(addr)) {
        return {};
      }
      auto next = read<u8>(addr++);
      if (next) {
        result.push_back(next);
      } else {
        return result;
      }
    }
    return {};
  }

  /*!
   * addr, including basic offset.
   */
  std::string goal_string(u32 addr) { return string(addr + 4); }

  bool word_in_memory(u32 addr) const { return in_memory<u32>(addr); }
};

u32 scan_for_symbol_table(const Ram& ram, u32 start_addr, u32 end_addr) {
  fmt::print("scanning for symbol table in 0x{:x} - 0x{:x}\n", start_addr, end_addr);
  std::vector<u32> candidates;

  // look for the false symbol.
  for (u32 addr = (start_addr & 0xfffffff0); addr < end_addr; addr += 8) {
    if (ram.word(addr + 4) == addr + 4) {
      candidates.push_back(addr);
    }
  }

  fmt::print("got {} candidates for #f:\n", candidates.size());

  for (auto addr : candidates) {
    auto str = addr + BASIC_OFFSET + SYM_INFO_OFFSET;
    fmt::print(" trying 0x{:x}:\n", addr);
    if (ram.word_in_memory(str)) {
      auto mem = ram.word(str + 4);         // offset of str in SymInfo
      auto name = ram.try_string(mem + 4);  // offset of data in GOAL string
      if (name) {
        fmt::print("   name: {}\n", *name);
      }
      if (name == "#f") {
        fmt::print("Got #f = 0x{:x}!\n", addr + 4);
        return addr + 4;
      }
    }
  }

  return 0;
}

struct SymbolMap {
  std::unordered_map<std::string, u32> name_to_addr;
  std::unordered_map<u32, std::string> addr_to_name;
};

SymbolMap build_symbol_map(const Ram& ram, u32 s7) {
  fmt::print("finding symbols...\n");
  SymbolMap map;
  /*
   s7 = symbol_table + (GOAL_MAX_SYMBOLS / 2) * 8 + BASIC_OFFSET;
  // pointer to the first symbol (SymbolTable2 is the "lower" symbol table)
  SymbolTable2 = symbol_table + BASIC_OFFSET;
  // the last symbol we will ever access.
  LastSymbol = symbol_table + 0xff00;
   */

  auto symbol_table = s7 - ((GOAL_MAX_SYMBOLS / 2) * 8 + BASIC_OFFSET);
  auto SymbolTable2 = symbol_table + BASIC_OFFSET;
  auto LastSymbol = symbol_table + 0xff00;

  for (u32 sym = SymbolTable2; sym < LastSymbol; sym += 8) {
    auto info = sym + SYM_INFO_OFFSET;  // already has basic offset
    auto str = ram.word(info + 4);
    if (str) {
      auto name = ram.string(str + 4);
      if (name != "asize-of-basic-func") {
        assert(map.name_to_addr.find(name) == map.name_to_addr.end());
        map.name_to_addr[name] = sym;
        map.addr_to_name[sym] = name;
      }
    }
  }

  assert(map.name_to_addr.size() == map.addr_to_name.size());
  fmt::print("found {} symbols.\n", map.name_to_addr.size());
  return map;
}

std::unordered_map<u32, std::string> build_type_map(const Ram& ram,
                                                    const SymbolMap& symbols,
                                                    u32 s7) {
  std::unordered_map<u32, std::string> result;
  fmt::print("finding types...\n");
  u32 type_of_type = ram.word(s7 + FIX_SYM_TYPE_TYPE);
  assert(type_of_type == ram.word(symbols.name_to_addr.at("type")));

  for (const auto& [name, addr] : symbols.name_to_addr) {
    u32 value = ram.word(addr);
    if (ram.word_in_memory(value - 4) && ((value & 0x7) == BASIC_OFFSET)) {
      if (ram.word(value - 4) == type_of_type) {
        result[value] = name;
      }
    }
  }

  fmt::print("found {} types\n", result.size());
  return result;
}

std::unordered_map<std::string, std::vector<u32>> find_basics(
    const Ram& ram,
    const std::unordered_map<u32, std::string>& type_map) {
  fmt::print("Scanning memory for objects. This may take a while...\n");

  std::unordered_map<std::string, std::vector<u32>> result;
  int total_objects = 0;

  for (u32 addr = (1 << 20); addr < ram.size; addr += 16) {
    u32 tag = ram.word(addr);
    auto iter = type_map.find(tag);
    // ignore the stupid types.
    if (iter != type_map.end() && iter->second != "symbol" && iter->second != "string" &&
        iter->second != "function" && iter->second != "object" && iter->second != "integer") {
      result[iter->second].push_back(addr);
      total_objects++;
    }
  }

  fmt::print("Got {} objects of {} unique types\n", total_objects, result.size());
  return result;
}

void inspect_basics(const Ram& ram,
                    const std::unordered_map<std::string, std::vector<u32>>& basics,
                    const std::unordered_map<u32, std::string>& types,
                    const SymbolMap& symbols,
                    const TypeSystem& type_system) {
  std::vector<std::string> sorted_type_names;
  for (auto& x : basics) {
    sorted_type_names.emplace_back(x.first);
  }
  std::sort(sorted_type_names.begin(), sorted_type_names.end(), [&](const auto& a, const auto& b) {
    return basics.at(a).size() < basics.at(b).size();
  });

  for (const auto& name : sorted_type_names) {
    fmt::print("TYPE {} (count {})\n", name, basics.at(name).size());

    // first, try looking up the type.
    if (!type_system.fully_defined_type_exists(name)) {
      fmt::print("-----Type is unknown!\n\n");
      continue;
    }

    auto type = dynamic_cast<BasicType*>(type_system.lookup_type(name));
    if (!type) {
      fmt::print("Could not cast Type! Skipping!!");
      continue;
    }

    for (auto& field : type->fields()) {
      if (!field.is_array() && !field.is_inline() && !field.is_dynamic() &&
          (field.type() == TypeSpec("basic") || field.type() == TypeSpec("object") ||
           field.type() == TypeSpec("uint32"))) {
        fmt::print("  field {}\n", field.name());

        std::unordered_map<std::string, int> type_frequency;

        for (auto base_addr : basics.at(name)) {
          int field_addr = base_addr + field.offset();
          if (ram.word_in_memory(field_addr)) {
            auto field_val = ram.word(field_addr);
            if ((field_val & 0x7) == 4 && ram.word_in_memory(field_val - 4)) {
              auto type_tag = ram.word(field_val - 4);
              auto iter = types.find(type_tag);
              if (iter != types.end()) {
                if (iter->second == "symbol") {
                  auto sym_iter = symbols.addr_to_name.find(field_val);
                  if (sym_iter != symbols.addr_to_name.end()) {
                    type_frequency[fmt::format("(symbol {})", sym_iter->second)]++;
                  } else {
                    type_frequency[iter->second]++;
                  }
                } else {
                  type_frequency[iter->second]++;
                }
              } else {
                type_frequency["_bad-type"]++;
              }
            } else if (field_val == 0) {
              type_frequency["0"]++;
            } else {
              type_frequency["_not-basic-ptr"]++;
            }
          } else {
            type_frequency["_bad-field-memory"]++;
          }
        }

        std::vector<std::string> sorted_field_types;
        for (const auto& x : type_frequency) {
          sorted_field_types.push_back(x.first);
        }
        std::sort(sorted_field_types.begin(), sorted_field_types.end(),
                  [&](const auto& a, const auto& b) {
                    return type_frequency.at(a) > type_frequency.at(b);
                  });

        for (const auto& field_type : sorted_field_types) {
          fmt::print("     [{}] {}\n", type_frequency.at(field_type), field_type);
        }
      }
    }
  }
}

static bool ends_with(const std::string& str, const std::string& suffix) {
  return str.size() >= suffix.size() &&
         0 == str.compare(str.size() - suffix.size(), suffix.size(), suffix);
}

int main(int argc, char** argv) {
  fmt::print("MemoryDumpTool\n");

  if (argc != 2) {
    fmt::print("usage: memory_dump_tool <ee_ram.bin|savestate.p2s>\n");
    return 1;
  }

  fmt::print("Loading type definitions from all-types.gc...\n");
  decompiler::DecompilerTypeSystem dts;
  dts.parse_type_defs({"decompiler", "config", "all-types.gc"});

  std::string file_name = argv[1];

  // If it's a PCSX2 savestate, lets extract the ee memory automatically
  if (ends_with(file_name, "p2s")) {
    fmt::print("Detected PCSX2 Save-state '{}', extracting memory...\n", file_name);
    elz::extractZip(file_name, "./savestate-out");
    // Then, check for and use the eeMemory.bin file
    if (fs::exists("./savestate-out/eeMemory.bin")) {
      file_name = "./savestate-out/eeMemory.bin";
      fmt::print("EE Memory extracted\n");
    } else {
      fmt::print("Couldn't locate EE Memory, aborting!\n");
      return 1;
    }
  }

  fmt::print("Loading memory from '{}'\n", file_name);
  auto data = file_util::read_binary_file(file_name);

  u32 one_mb = (1 << 20);

  if (data.size() == 32 * one_mb) {
    fmt::print("Got 32MB file\n");
  } else if (data.size() == 128 * one_mb) {
    fmt::print("Got 128MB file\n");
  } else if (data.size() == 127 * one_mb) {
    fmt::print("Got a 127MB file. Assuming this is a dump with the first 1 MB missing.\n");
    data.insert(data.begin(), one_mb, 0);
    assert(data.size() == 128 * one_mb);
  } else {
    fmt::print("Invalid size: {} bytes\n", data.size());
  }

  Ram ram(data.data(), data.size());

  u32 s7 = scan_for_symbol_table(ram, one_mb, 2 * one_mb);
  if (!s7) {
    fmt::print("Failed to find symbol table\n");
    return 1;
  }

  auto symbol_map = build_symbol_map(ram, s7);
  auto types = build_type_map(ram, symbol_map, s7);
  auto basics = find_basics(ram, types);

  inspect_basics(ram, basics, types, symbol_map, dts.ts);

  return 0;
}
