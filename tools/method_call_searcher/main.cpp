#include <algorithm>
#include <cctype>
#include <optional>
#include <thread>

#include "common/goal_constants.h"
#include "common/link_types.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/unicode_util.h"

#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/IR2/AtomicOp.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/analysis/atomic_op_builder.h"
#include "decompiler/config.h"
#include "tools/shared/decompiler_setup.h"
#include "tools/shared/isolated_map.h"

#include "fmt/format.h"
#include "fmt/ranges.h"
#include "third-party/CLI11.hpp"
#include "third-party/json.hpp"

using namespace decompiler;

namespace {

// how far back to look for the instructions that load the method and the type for the pattern
// fallback. the loads are normally right before the argument setup.
constexpr int PATTERN_WINDOW = 24;

struct Query {
  std::optional<std::string> type;
  std::optional<int> method_id;
  bool subtypes = false;
  bool supertypes = false;
  bool unresolved = false;
};

struct MethodCall {
  std::string object;
  std::string function;
  int instruction = -1;
  int op = -1;
  std::string label;  // closest preceding label
  std::optional<std::string> type;
  int method_id = -1;
  std::string method_name;
  bool is_virtual = false;
  bool from_types = false;  // found by type analysis, otherwise by instruction pattern
};

void to_json(nlohmann::json& j, const MethodCall& call) {
  j = nlohmann::json{{"object", call.object},
                     {"function", call.function},
                     {"instruction", call.instruction},
                     {"op", call.op},
                     {"label", call.label},
                     {"type", call.type ? nlohmann::json(*call.type) : nlohmann::json(nullptr)},
                     {"method_id", call.method_id},
                     {"method_name", call.method_name},
                     {"virtual", call.is_virtual},
                     {"source", call.from_types ? "types" : "pattern"}};
}

void from_json(const nlohmann::json& j, MethodCall& call) {
  call.object = j.at("object").get<std::string>();
  call.function = j.at("function").get<std::string>();
  call.instruction = j.at("instruction").get<int>();
  call.op = j.at("op").get<int>();
  call.label = j.at("label").get<std::string>();
  if (!j.at("type").is_null()) {
    call.type = j.at("type").get<std::string>();
  }
  call.method_id = j.at("method_id").get<int>();
  call.method_name = j.at("method_name").get<std::string>();
  call.is_virtual = j.at("virtual").get<bool>();
  call.from_types = j.at("source").get<std::string>() == "types";
}

bool is_method_call(const Instruction& instr) {
  return instr.kind == InstructionKind::JALR && instr.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
         instr.get_src(0).is_reg(make_gpr(Reg::T9));
}

// what the type pass knows about the function in t9 at the call.
bool resolve_from_types(const Function& func, int idx, MethodCall* call) {
  if (!func.ir2.atomic_ops_succeeded || !func.ir2.env.has_type_analysis()) {
    return false;
  }
  const auto& ops = *func.ir2.atomic_ops;
  auto op_it = ops.instruction_to_atomic_op.find(idx);
  if (op_it == ops.instruction_to_atomic_op.end() ||
      !dynamic_cast<const CallOp*>(ops.ops.at(op_it->second).get())) {
    return false;
  }
  call->op = op_it->second;

  const auto& t9 = func.ir2.env.get_types_before_op(op_it->second).get(make_gpr(Reg::T9));
  switch (t9.kind) {
    case TP_Type::Kind::VIRTUAL_METHOD:
    case TP_Type::Kind::NON_VIRTUAL_METHOD:
    case TP_Type::Kind::GET_ART_BY_NAME_METHOD:
      call->type = t9.method_from_type().base_type();
      call->method_id = t9.method_id();
      call->is_virtual = t9.kind != TP_Type::Kind::NON_VIRTUAL_METHOD;
      break;
    case TP_Type::Kind::NON_OBJECT_NEW_METHOD:
      call->type = t9.method_from_type().base_type();
      call->method_id = GOAL_NEW_METHOD;
      break;
    case TP_Type::Kind::OBJECT_NEW_METHOD:
      call->type = "object";
      call->method_id = GOAL_NEW_METHOD;
      break;
    default:
      return false;
  }
  call->from_types = true;
  return true;
}

std::optional<int> last_write_to(const Function& func, int idx, Register reg) {
  for (int i = idx - 1; i >= std::max(0, idx - PATTERN_WINDOW); i--) {
    const auto& instr = func.instructions.at(i);
    for (int d = 0; d < instr.n_dst; d++) {
      if (instr.get_dst(d).is_reg(reg)) {
        return i;
      }
    }
  }
  return std::nullopt;
}

// fallback for code the type pass couldn't handle. recognizes
//   lw <type>, some-type(s7)   or   lwu <type>, -4(<obj>)
//   lwu t9, <16 + 4 * method-id>(<type>)
// before the jalr. the second form is a virtual call on an unknown type.
bool resolve_from_pattern(const Function& func, int idx, const TypeSystem& ts, MethodCall* call) {
  auto method_load_idx = last_write_to(func, idx, make_gpr(Reg::T9));
  if (!method_load_idx) {
    return false;
  }
  const auto& method_load = func.instructions.at(*method_load_idx);
  if (method_load.kind != InstructionKind::LWU || !method_load.get_src(0).is_imm() ||
      !method_load.get_src(1).is_reg()) {
    return false;
  }
  int offset = method_load.get_src(0).get_imm();
  if (offset < 16 || (offset & 3) != 0) {
    return false;
  }

  auto type_load_idx = last_write_to(func, *method_load_idx, method_load.get_src(1).get_reg());
  if (!type_load_idx) {
    return false;
  }
  const auto& type_load = func.instructions.at(*type_load_idx);
  if (type_load.kind == InstructionKind::LW && type_load.get_src(0).is_sym() &&
      type_load.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto name = type_load.get_src(0).get_sym();
    if (!type_exists(ts, name)) {
      return false;  // a function stored in some other global
    }
    call->type = name;
    call->is_virtual = false;
  } else if (type_load.kind == InstructionKind::LWU && type_load.get_src(0).is_imm(-4) &&
             type_load.get_src(1).is_reg()) {
    call->type = std::nullopt;
    call->is_virtual = true;
  } else {
    return false;
  }
  call->method_id = (offset - 16) / 4;
  call->from_types = false;
  return true;
}

bool matches(const Query& query, const TypeSystem& ts, const MethodCall& call) {
  if (query.method_id && call.method_id != *query.method_id) {
    return false;
  }
  if (!call.type) {
    return query.unresolved;
  }
  if (!query.type || *call.type == *query.type) {
    return true;
  }
  try {
    MethodInfo info;
    if (query.subtypes && ts.tc(TypeSpec(*query.type), TypeSpec(*call.type)) &&
        ts.try_lookup_method(*query.type, call.method_id, &info)) {
      return true;
    }
    if (query.supertypes && ts.tc(TypeSpec(*call.type), TypeSpec(*query.type))) {
      return true;
    }
  } catch (const std::exception&) {
  }
  return false;
}

struct ObjectResult {
  std::vector<MethodCall> calls;
  int functions = 0;
  std::vector<std::string> untyped_functions;
};

ObjectResult search_object(ObjectFileDB& db,
                           Config& config,
                           ObjectFileData& data,
                           const Query& query) {
  run_object_type_analysis(db, config, data);

  ObjectResult result;
  const auto& ts = db.dts.ts;
  for (int seg = 0; seg < (int)data.linked_data.functions_by_seg.size(); seg++) {
    for (auto& func : data.linked_data.functions_by_seg.at(seg)) {
      result.functions++;
      if (!func.ir2.env.has_type_analysis()) {
        result.untyped_functions.push_back(func.name());
      }
      for (int i = 0; i < (int)func.instructions.size(); i++) {
        if (!is_method_call(func.instructions[i])) {
          continue;
        }
        MethodCall call;
        if (!resolve_from_types(func, i, &call) && !resolve_from_pattern(func, i, ts, &call)) {
          continue;
        }
        if (!matches(query, ts, call)) {
          continue;
        }
        call.object = data.to_unique_name();
        call.function = func.name();
        call.instruction = i;
        call.label = closest_label(data.linked_data, func, seg, i);
        MethodInfo info;
        if (call.type && ts.try_lookup_method(*call.type, call.method_id, &info)) {
          call.method_name = info.name;
        }
        result.calls.push_back(std::move(call));
      }
      func.ir2 = {};
    }
  }
  return result;
}

std::string describe_call(const MethodCall& call) {
  std::string method = call.type ? fmt::format("(method {} {})", call.method_id, *call.type)
                                 : fmt::format("(method {} <unknown type>)", call.method_id);
  if (!call.method_name.empty()) {
    method += " " + call.method_name;
  }
  std::vector<std::string> notes;
  notes.push_back(call.is_virtual ? "virtual" : "non-virtual");
  if (!call.from_types) {
    notes.push_back("pattern match, no type info");
  }
  return fmt::format("{} [{}]", method, fmt::join(notes, ", "));
}

}  // namespace

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path config_path;
  fs::path in_folder;
  std::string config_game_version;
  std::string config_override = "{}";
  std::string type_name;
  std::string method_str;
  std::vector<std::string> object_names;
  fs::path output_path;
  Query query;
  bool verbose = false;
  int jobs = std::max(1u, std::thread::hardware_concurrency());

  CLI::App app{"OpenGOAL Method Call Searcher"};
  app.add_option("config-path", config_path, "Path to the decompiler config .jsonc file")
      ->required();
  app.add_option("in-folder", in_folder,
                 "The path containing the iso_data folders. ie. ./iso_data/. Assumes the "
                 "'gameName' from the config as a sub-directory")
      ->required();
  app.add_option("--version", config_game_version, "e.g. ntsc_v1")->required();
  app.add_option("-t,--type", type_name, "The type the method is called on");
  app.add_option("-m,--method", method_str,
                 "The method id, or its name (needs --type). Leave out to find every method call "
                 "on --type");
  app.add_flag("-s,--subtypes", query.subtypes,
               "Also match calls on child types of --type, which use its method unless they "
               "override it");
  app.add_flag("-S,--supertypes", query.supertypes, "Also match calls on parent types of --type");
  app.add_flag("-u,--unresolved", query.unresolved,
               "Also list calls of the method id where the type couldn't be figured out (needs a "
               "numeric --method)");
  app.add_option("--obj", object_names,
                 "Only search these object files (can be repeated). Defaults to the config's "
                 "allowed_objects or everything");
  app.add_option("--config-override", config_override,
                 "JSON provided will be merged with the specified config");
  app.add_option("-o,--output-path", output_path, "Also write the results to this json file");
  app.add_option("-j,--jobs", jobs, "Number of worker processes");
  app.add_flag("-v,--verbose", verbose,
               "Show the decompiler's own log output and list functions without type info");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  if (type_name.empty() && method_str.empty()) {
    fmt::print("Give a --type, a --method, or both.\n");
    return 1;
  }

  if (!file_util::setup_project_path(std::nullopt)) {
    fmt::print("Unable to setup project path\n");
    return 1;
  }
  lg::initialize();
  lg::set_stdout_level(verbose ? lg::level::info : lg::level::off_unless_die);

  Config config;
  try {
    config = read_config_file(config_path, config_game_version, config_override);
  } catch (const std::exception& e) {
    fmt::print("Failed to parse config: {}\n", e.what());
    return 1;
  }
  if (!object_names.empty()) {
    config.allowed_objects =
        std::unordered_set<std::string>(object_names.begin(), object_names.end());
  }
  config.generate_all_types = false;
  in_folder = in_folder / config.game_name;
  if (!fs::exists(in_folder)) {
    fmt::print("in-folder '{}' does not exist\n", in_folder.string());
    return 1;
  }

  Timer timer;
  fmt::print("Loading object files...\n");
  auto db = load_object_file_db(config, in_folder);
  const auto& ts = db->dts.ts;

  if (!type_name.empty()) {
    if (!type_exists(ts, type_name)) {
      fmt::print("Unknown type '{}'\n", type_name);
      return 1;
    }
    query.type = type_name;
  }
  if (!method_str.empty()) {
    bool numeric = std::all_of(method_str.begin(), method_str.end(), ::isdigit);
    MethodInfo info;
    if (numeric) {
      query.method_id = std::stoi(method_str);
      if (query.type && !ts.try_lookup_method(*query.type, *query.method_id, &info)) {
        fmt::print("Warning: {} has no method {}\n", *query.type, *query.method_id);
      }
    } else if (!query.type) {
      fmt::print("Looking up a method by name needs --type\n");
      return 1;
    } else if (ts.try_lookup_method(*query.type, method_str, &info)) {
      query.method_id = info.id;
    } else {
      fmt::print("{} has no method named '{}'\n", *query.type, method_str);
      return 1;
    }
  }
  if (query.unresolved && !query.method_id) {
    fmt::print("--unresolved needs a --method\n");
    return 1;
  }

  std::vector<ObjectFileData*> objects;
  db->for_each_obj([&](ObjectFileData& data) { objects.push_back(&data); });

  fmt::print("Searching {} object(s) with {} worker process(es)...\n", objects.size(), jobs);
  size_t done = 0;
  size_t chunk = std::clamp<size_t>(objects.size() / (jobs * 4), 1, 16);
  auto results = isolated_map(
      objects.size(), jobs, chunk,
      [&](size_t i) {
        auto result = search_object(*db, config, *objects[i], query);
        nlohmann::json j = {{"calls", result.calls},
                            {"functions", result.functions},
                            {"untyped", result.untyped_functions}};
        return j.dump();
      },
      [&]() {
        done++;
        if (done * 100 / objects.size() != (done - 1) * 100 / objects.size()) {
          fmt::print("\r  {}%", done * 100 / objects.size());
          fflush(stdout);
        }
      });
  fmt::print("\n\n");

  std::vector<MethodCall> calls;
  std::vector<std::string> crashed_objects;
  std::vector<std::string> untyped;
  int total_functions = 0;
  for (size_t i = 0; i < objects.size(); i++) {
    if (!results[i]) {
      crashed_objects.push_back(objects[i]->to_unique_name());
      continue;
    }
    auto j = nlohmann::json::parse(*results[i]);
    for (auto& call : j.at("calls")) {
      calls.push_back(call.get<MethodCall>());
    }
    total_functions += j.at("functions").get<int>();
    for (auto& name : j.at("untyped")) {
      untyped.push_back(
          fmt::format("{} in {}", name.get<std::string>(), objects[i]->to_unique_name()));
    }
  }

  std::string last_object;
  for (const auto& call : calls) {
    if (call.object != last_object) {
      fmt::print("{}\n", call.object);
      last_object = call.object;
    }
    std::string where =
        call.op >= 0 ? fmt::format("op {}", call.op) : fmt::format("instr {}", call.instruction);
    fmt::print("  {:<50} {:<9} {:<10} {}\n", call.function, where, call.label, describe_call(call));
  }
  fmt::print("{}Found {} call(s) in {:.2f}s\n", calls.empty() ? "" : "\n", calls.size(),
             timer.getSeconds());

  if (!untyped.empty()) {
    fmt::print(
        "{} of {} function(s) had no type information, calls in those were only found by "
        "instruction pattern{}\n",
        untyped.size(), total_functions, verbose ? ":" : " (use -v to list them)");
    if (verbose) {
      for (auto& name : untyped) {
        fmt::print("  {}\n", name);
      }
    }
  }
  if (!crashed_objects.empty()) {
    fmt::print("The decompiler crashed on {} object(s), they were not searched:\n",
               crashed_objects.size());
    for (auto& name : crashed_objects) {
      fmt::print("  {}\n", name);
    }
  }

  if (!output_path.empty()) {
    nlohmann::json j = calls;
    file_util::write_text_file(output_path, j.dump(2));
    fmt::print("Wrote results to {}\n", output_path.string());
  }
  return 0;
}
