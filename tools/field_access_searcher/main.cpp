#include <algorithm>
#include <optional>
#include <thread>

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/unicode_util.h"

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

struct Query {
  std::string type;
  std::optional<std::string> field;
  bool exact = false;
  bool supertypes = false;
  bool alternatives = false;
};

enum class AccessKind { READ, WRITE, ADDR, REF };

std::string kind_name(AccessKind kind) {
  switch (kind) {
    case AccessKind::READ:
      return "read";
    case AccessKind::WRITE:
      return "write";
    case AccessKind::ADDR:
      return "addr";
    case AccessKind::REF:
      return "ref";
  }
  return "?";
}

struct FieldAccess {
  std::string object;
  std::string function;
  int op = -1;
  int instruction = -1;
  std::string label;
  std::string kind;
  std::string type;
  std::string field;
  std::string path;
  std::string base_type;
  std::string op_text;
  bool alternative = false;  // decompiler picked a different field at the same offset
};

void to_json(nlohmann::json& j, const FieldAccess& a) {
  j = nlohmann::json{{"object", a.object},   {"function", a.function},
                     {"op", a.op},           {"instruction", a.instruction},
                     {"label", a.label},     {"kind", a.kind},
                     {"type", a.type},       {"field", a.field},
                     {"path", a.path},       {"base_type", a.base_type},
                     {"op_text", a.op_text}, {"alternative", a.alternative}};
}

void from_json(const nlohmann::json& j, FieldAccess& a) {
  a.object = j.at("object").get<std::string>();
  a.function = j.at("function").get<std::string>();
  a.op = j.at("op").get<int>();
  a.instruction = j.at("instruction").get<int>();
  a.label = j.at("label").get<std::string>();
  a.kind = j.at("kind").get<std::string>();
  a.type = j.at("type").get<std::string>();
  a.field = j.at("field").get<std::string>();
  a.path = j.at("path").get<std::string>();
  a.base_type = j.at("base_type").get<std::string>();
  a.op_text = j.at("op_text").get<std::string>();
  a.alternative = j.at("alternative").get<bool>();
}

bool type_has_field(const TypeSystem& ts, const std::string& type, const std::string& field) {
  auto* as_struct = dynamic_cast<StructureType*>(ts.lookup_type_allow_partial_def(type));
  return as_struct && as_struct->lookup_field(field, nullptr);
}

bool field_matches(const Query& query,
                   const TypeSystem& ts,
                   const TypeSpec& on,
                   const std::string& field) {
  if (query.field && field != *query.field) {
    return false;
  }
  if (on.base_type() == query.type) {
    return true;
  }
  try {
    // child types inherit the field, so it's the same one. without a --field, only count the
    // fields the queried type has, not the ones the child added.
    if (!query.exact && ts.tc(TypeSpec(query.type), on) &&
        (query.field || type_has_field(ts, query.type, field))) {
      return true;
    }
    if (query.supertypes && ts.tc(on, TypeSpec(query.type))) {
      return true;
    }
  } catch (const std::exception&) {
  }
  return false;
}

struct Hit {
  TypeSpec on;
  std::string field;
};

// walks the field path of a reverse lookup result, tracking the type each field is looked up on,
// so fields of inline structures (ie. the trans in (-> a0 camera trans)) are matched as well.
std::vector<Hit> match_path(const Query& query,
                            const TypeSystem& ts,
                            const TypeSpec& base,
                            const std::vector<FieldReverseLookupOutput::Token>& tokens) {
  std::vector<Hit> hits;
  TypeSpec cur = base;
  for (const auto& tok : tokens) {
    if (tok.kind == FieldReverseLookupOutput::Token::Kind::FIELD) {
      if (field_matches(query, ts, cur, tok.name)) {
        hits.push_back({cur, tok.name});
      }
      try {
        cur = ts.lookup_field_info(cur.base_type(), tok.name).type;
      } catch (const std::exception&) {
        break;
      }
    } else if (cur.arg_count() > 0) {
      TypeSpec elt = cur.get_single_arg();
      cur = elt;
    } else {
      break;
    }
  }
  return hits;
}

std::string format_path(const std::string& reg,
                        bool addr_of,
                        const std::vector<FieldReverseLookupOutput::Token>& tokens) {
  std::vector<std::string> parts;
  for (const auto& tok : tokens) {
    parts.push_back(tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX ? "<idx>"
                                                                               : tok.print());
  }
  return fmt::format("({} {} {})", addr_of ? "&->" : "->", reg, fmt::join(parts, " "));
}

RegClass reg_class(const Register& r) {
  return r.get_kind() == Reg::FPR ? RegClass::FLOAT : RegClass::GPR_64;
}

// one way of reading an access: the type to look the offset up in, and with what stride.
struct LookupBase {
  TypeSpec type;
  int offset = 0;
  int stride = 0;
};

// the same cases (in the same order) that LoadVarOp::get_src_type and the store/load form
// conversion use to turn a register + offset into a field.
std::vector<LookupBase> lookup_bases(const TP_Type& t, int offset) {
  std::vector<LookupBase> result;
  if (t.kind == TP_Type::Kind::INVALID) {
    return result;
  }
  if (t.kind == TP_Type::Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT) {
    LookupBase b{t.get_obj_plus_const_mult_typespec(), offset, (int)t.get_multiplier()};
    if (b.type == TypeSpec("pointer", {TypeSpec("uint8")}) && b.stride != 0) {
      b.stride = 1;
    }
    result.push_back(b);
  }
  if (t.typespec() != TypeSpec("pointer") && t.typespec() != TypeSpec("pair")) {
    result.push_back({t.typespec(), offset, 0});
  }
  if (t.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
    if (t.get_integer_constant() == 0) {
      result.push_back({t.get_objects_typespec(), offset, 0});
    }
    if (offset == 0) {
      result.push_back({t.get_objects_typespec(), (int)t.get_integer_constant(), 0});
    }
  }
  return result;
}

struct ObjectResult {
  std::vector<FieldAccess> accesses;
  int functions = 0;
  std::vector<std::string> untyped_functions;
  std::vector<std::string> failed_ops;
};

class Searcher {
 public:
  Searcher(const Query& query, const TypeSystem& ts, ObjectFileData& data)
      : m_query(query), m_ts(ts), m_data(data) {}

  ObjectResult& result() { return m_result; }

  void search_function(Function& func, int seg) {
    m_result.functions++;
    if (!func.ir2.atomic_ops_succeeded || !func.ir2.env.has_type_analysis()) {
      m_result.untyped_functions.push_back(func.name());
      return;
    }
    const auto& ops = *func.ir2.atomic_ops;
    for (int i = 0; i < (int)ops.ops.size(); i++) {
      try {
        search_op(func, seg, i);
      } catch (const std::exception& e) {
        m_result.failed_ops.push_back(fmt::format("{} op {}: {}", func.name(), i, e.what()));
      }
    }
  }

 private:
  void search_op(const Function& func, int seg, int op_idx) {
    const auto* op = func.ir2.atomic_ops->ops.at(op_idx).get();
    const auto& types = func.ir2.env.get_types_before_op(op_idx);
    IR2_RegOffset ro;

    if (auto* load = dynamic_cast<const LoadVarOp*>(op)) {
      if (get_as_reg_offset(load->src(), &ro)) {
        DerefKind dk;
        dk.is_store = false;
        dk.size = load->size();
        dk.sign_extend = load->kind() == LoadVarOp::Kind::SIGNED;
        dk.reg_kind = reg_class(ro.reg);
        check_access(func, seg, op_idx, ro, types.get(ro.reg), dk, AccessKind::READ,
                     load->kind() == LoadVarOp::Kind::VECTOR_FLOAT);
      }
    } else if (auto* store = dynamic_cast<const StoreOp*>(op)) {
      if (get_as_reg_offset(store->addr(), &ro)) {
        DerefKind dk;
        dk.is_store = true;
        dk.size = store->store_size();
        dk.reg_kind = reg_class(ro.reg);
        check_access(func, seg, op_idx, ro, types.get(ro.reg), dk, AccessKind::WRITE,
                     store->kind() == StoreOp::Kind::VECTOR_FLOAT);
      }
    } else if (auto* set = dynamic_cast<const SetVarOp*>(op)) {
      check_address(func, seg, op_idx, set->src(), types);
    }
  }

  void check_access(const Function& func,
                    int seg,
                    int op_idx,
                    const IR2_RegOffset& ro,
                    const TP_Type& type,
                    const DerefKind& dk,
                    AccessKind kind,
                    bool vector_float) {
    for (const auto& base : lookup_bases(type, ro.offset)) {
      FieldReverseLookupInput in;
      in.deref = dk;
      in.base_type = base.type;
      in.offset = base.offset;
      in.stride = base.stride;
      auto rd = m_ts.reverse_field_multi_lookup(in);
      if (!rd.success && vector_float) {
        // lqc2/sqc2 are often on the address of a field, ie. (.lvf vf1 (&-> a0 trans))
        in.deref = std::nullopt;
        rd = m_ts.reverse_field_multi_lookup(in);
      }
      if (rd.success) {
        report(func, seg, op_idx, ro.reg.to_string(), base.type, rd, kind);
        return;
      }
    }
  }

  // an address calculation, ie. (+ a0 16) for (&-> a0 trans) or an inline field, and the
  // (+ obj (* idx stride)) forms of an inline array element.
  void check_address(const Function& func,
                     int seg,
                     int op_idx,
                     const SimpleExpression& expr,
                     const TypeState& types) {
    if (expr.kind() != SimpleExpression::Kind::ADD || expr.args() != 2 ||
        !expr.get_arg(0).is_var()) {
      return;
    }
    auto reg0 = expr.get_arg(0).var().reg();
    if (expr.get_arg(1).is_int()) {
      if (reg0 == Register(Reg::GPR, Reg::SP) || types.get(reg0).kind != TP_Type::Kind::TYPESPEC) {
        return;
      }
      check_lookup(func, seg, op_idx, reg0, types.get(reg0).typespec(), expr.get_arg(1).get_int(),
                   0);
      return;
    }
    if (!expr.get_arg(1).is_var()) {
      return;
    }
    auto reg1 = expr.get_arg(1).var().reg();
    const auto& t0 = types.get(reg0);
    const auto& t1 = types.get(reg1);
    if (t0.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT) {
      check_lookup(func, seg, op_idx, reg1, t1.typespec(), t0.get_add_int_constant(),
                   t0.get_mult_int_constant());
    } else if (t1.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT) {
      check_lookup(func, seg, op_idx, reg0, t0.typespec(), t1.get_add_int_constant(),
                   t1.get_mult_int_constant());
    } else if (t0.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
      check_lookup(func, seg, op_idx, reg1, t1.typespec(), t0.get_integer_constant(), 1);
    }
  }

  void check_lookup(const Function& func,
                    int seg,
                    int op_idx,
                    const Register& reg,
                    const TypeSpec& base,
                    int offset,
                    int stride) {
    FieldReverseLookupInput in;
    in.base_type = base;
    in.offset = offset;
    in.stride = stride;
    auto rd = m_ts.reverse_field_multi_lookup(in);
    if (rd.success) {
      report(func, seg, op_idx, reg.to_string(), base, rd, AccessKind::ADDR);
    }
  }

  void report(const Function& func,
              int seg,
              int op_idx,
              const std::string& reg,
              const TypeSpec& base,
              const FieldReverseMultiLookupOutput& rd,
              AccessKind kind) {
    size_t count = m_query.alternatives ? rd.results.size() : 1;
    for (size_t r = 0; r < count; r++) {
      const auto& result = rd.results.at(r);
      auto hits = match_path(m_query, m_ts, base, result.tokens);
      for (const auto& hit : hits) {
        FieldAccess access;
        access.object = m_data.to_unique_name();
        access.function = func.name();
        access.op = op_idx;
        auto instr = func.ir2.atomic_ops->atomic_op_to_instruction.find(op_idx);
        if (instr != func.ir2.atomic_ops->atomic_op_to_instruction.end()) {
          access.instruction = instr->second;
          access.label = closest_label(m_data.linked_data, func, seg, instr->second);
        }
        // an address calculation that isn't an &-> is a reference to an inline field.
        auto k = kind == AccessKind::ADDR && !result.addr_of ? AccessKind::REF : kind;
        access.kind = kind_name(k);
        access.type = hit.on.print();
        access.field = hit.field;
        access.path = format_path(reg, kind == AccessKind::ADDR && result.addr_of, result.tokens);
        access.base_type = base.print();
        access.op_text = func.ir2.atomic_ops->ops.at(op_idx)->to_string(func.ir2.env);
        access.alternative = r > 0;
        m_result.accesses.push_back(std::move(access));
      }
      if (!hits.empty()) {
        // alternatives only matter when the decompiler's pick isn't the field.
        break;
      }
    }
  }

  const Query& m_query;
  const TypeSystem& m_ts;
  ObjectFileData& m_data;
  ObjectResult m_result;
};

ObjectResult search_object(ObjectFileDB& db,
                           Config& config,
                           ObjectFileData& data,
                           const Query& query) {
  run_object_type_analysis(db, config, data);
  Searcher searcher(query, db.dts.ts, data);
  for (int seg = 0; seg < (int)data.linked_data.functions_by_seg.size(); seg++) {
    for (auto& func : data.linked_data.functions_by_seg.at(seg)) {
      searcher.search_function(func, seg);
      func.ir2 = {};
    }
  }
  return std::move(searcher.result());
}

}  // namespace

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path config_path;
  fs::path in_folder;
  std::string config_game_version;
  std::string config_override = "{}";
  std::string field_name;
  std::vector<std::string> object_names;
  fs::path output_path;
  Query query;
  bool verbose = false;
  int jobs = std::max(1u, std::thread::hardware_concurrency());

  CLI::App app{"OpenGOAL Field Access Searcher"};
  app.add_option("config-path", config_path, "Path to the decompiler config file")->required();
  app.add_option("in-folder", in_folder,
                 "The path containing the iso_data folders. ie. ./iso_data/. Assumes the "
                 "'gameName' from the config as a sub-directory")
      ->required();
  app.add_option("--version", config_game_version, "e.g. ntsc_v1")->required();
  app.add_option("-t,--type", query.type, "The type the field belongs to")->required();
  app.add_option("-f,--field", field_name,
                 "The field to search for. Leave out to find every access of --type's fields");
  app.add_flag("-e,--exact", query.exact,
               "Only match accesses on --type itself, not on child types that inherit the field");
  app.add_flag("-S,--supertypes", query.supertypes,
               "Also match accesses on parent types of --type that have the field");
  app.add_flag("-a,--alternatives", query.alternatives, "Also match overlay fields");
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

  if (!type_exists(ts, query.type)) {
    fmt::print("Unknown type '{}'\n", query.type);
    return 1;
  }
  if (!field_name.empty()) {
    if (!type_has_field(ts, query.type, field_name)) {
      fmt::print("{} has no field named '{}'\n", query.type, field_name);
      return 1;
    }
    query.field = field_name;
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
        nlohmann::json j = {{"accesses", result.accesses},
                            {"functions", result.functions},
                            {"untyped", result.untyped_functions},
                            {"failed", result.failed_ops}};
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

  std::vector<FieldAccess> accesses;
  std::vector<std::string> crashed_objects;
  std::vector<std::string> untyped;
  std::vector<std::string> failed_ops;
  int total_functions = 0;
  for (size_t i = 0; i < objects.size(); i++) {
    if (!results[i]) {
      crashed_objects.push_back(objects[i]->to_unique_name());
      continue;
    }
    auto j = nlohmann::json::parse(*results[i]);
    for (auto& access : j.at("accesses")) {
      accesses.push_back(access.get<FieldAccess>());
    }
    total_functions += j.at("functions").get<int>();
    for (auto& name : j.at("untyped")) {
      untyped.push_back(
          fmt::format("{} in {}", name.get<std::string>(), objects[i]->to_unique_name()));
    }
    for (auto& op : j.at("failed")) {
      failed_ops.push_back(
          fmt::format("{} in {}", op.get<std::string>(), objects[i]->to_unique_name()));
    }
  }

  std::string last_object;
  int reads = 0, writes = 0, addrs = 0;
  for (const auto& a : accesses) {
    if (a.object != last_object) {
      fmt::print("{}\n", a.object);
      last_object = a.object;
    }
    std::string on = a.type == query.type ? "" : fmt::format(" [on {}]", a.type);
    if (!query.field) {
      on = fmt::format(" [{}{}]", a.field, a.type == query.type ? "" : " on " + a.type);
    }
    fmt::print("  {:<50} op {:<5} {:<10} {:<5} {}{}{}\n", a.function, a.op, a.label, a.kind, a.path,
               on, a.alternative ? " (alternative)" : "");
    reads += a.kind == "read";
    writes += a.kind == "write";
    addrs += a.kind == "addr" || a.kind == "ref";
  }
  fmt::print("{}Found {} access(es) ({} read, {} write, {} address) in {:.2f}s\n",
             accesses.empty() ? "" : "\n", accesses.size(), reads, writes, addrs,
             timer.getSeconds());

  if (!untyped.empty()) {
    fmt::print("{} of {} function(s) had no type information and could not be searched{}\n",
               untyped.size(), total_functions, verbose ? ":" : " (use -v to list them)");
    if (verbose) {
      for (auto& name : untyped) {
        fmt::print("  {}\n", name);
      }
    }
  }
  if (!failed_ops.empty()) {
    fmt::print("{} op(s) could not be checked because the type system failed on them{}\n",
               failed_ops.size(), verbose ? ":" : " (use -v to list them)");
    if (verbose) {
      for (auto& op : failed_ops) {
        fmt::print("  {}\n", op);
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
    nlohmann::json j = accesses;
    file_util::write_text_file(output_path, j.dump(2));
    fmt::print("Wrote results to {}\n", output_path.string());
  }
  return 0;
}
