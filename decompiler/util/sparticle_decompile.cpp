#include "sparticle_decompile.h"

#include "common/goos/PrettyPrinter.h"
#include "common/util/Assert.h"
#include "common/util/print_float.h"

#include "decompiler/util/data_decompile.h"

namespace decompiler {
// sparticle fields.
// should match the enum in the game.
enum class FieldId {
  MISC_FIELDS_START = 0,
  SPT_TEXTURE = 1,
  SPT_ANIM = 2,
  SPT_ANIM_SPEED = 3,
  SPT_BIRTH_FUNC = 4,
  SPT_JOINT_REFPOINT = 5,
  SPT_NUM = 6,
  SPT_SOUND = 7,
  MISC_FIELDS_END = 8,
  SPRITE_FIELDS_START = 9,
  SPT_X = 10,
  SPT_Y = 11,
  SPT_Z = 12,
  SPT_SCALE_X = 13,
  SPT_ROT_X = 14,
  SPT_ROT_Y = 15,
  SPT_ROT_Z = 16,
  SPT_SCALE_Y = 17,
  SPT_R = 18,
  SPT_G = 19,
  SPT_B = 20,
  SPT_A = 21,
  SPRITE_FIELDS_END = 22,
  CPU_FIELDS_START = 23,
  SPT_OMEGA = 24,
  SPT_VEL_X = 25,
  SPT_VEL_Y = 26,
  SPT_VEL_Z = 27,
  SPT_SCALEVEL_X = 28,
  SPT_ROTVEL_X = 29,
  SPT_ROTVEL_Y = 30,
  SPT_ROTVEL_Z = 31,
  SPT_SCALEVEL_Y = 32,
  SPT_FADE_R = 33,
  SPT_FADE_G = 34,
  SPT_FADE_B = 35,
  SPT_FADE_A = 36,
  SPT_ACCEL_X = 37,
  SPT_ACCEL_Y = 38,
  SPT_ACCEL_Z = 39,
  SPT_DUMMY = 40,
  SPT_QUAT_X = 41,
  SPT_QUAT_Y = 42,
  SPT_QUAT_Z = 43,
  SPT_QUAD_W = 44,
  SPT_FRICTION = 45,
  SPT_TIMER = 46,
  SPT_FLAGS = 47,
  SPT_USERDATA = 48,
  SPT_FUNC = 49,
  SPT_NEXT_TIME = 50,
  SPT_NEXT_LAUNCHER = 51,
  CPU_FIELDS_END = 52,
  LAUNCH_FIELDS_START = 53,
  SPT_LAUNCHROT_X = 54,
  SPT_LAUNCHROT_Y = 55,
  SPT_LAUNCHROT_Z = 56,
  SPT_LAUNCHROT_W = 57,
  SPT_CONEROT_X = 58,
  SPT_CONEROT_Y = 59,
  SPT_CONEROT_Z = 60,
  SPT_CONEROT_W = 61,
  SPT_CONEROT_RADIUS = 62,
  SPT_ROTATE_Y = 63,
  LAUNCH_FIELDS_END = 64,
  SPT_SCALE = 65,
  SPT_SCALEVEL = 66,
  SPT_END = 67,
};

// flag vals:
// 0: timer, flags, end
// 1: texture, float, random-rangef
// 3: integer
// 6: next launcher

// flag bits
// 2: number is an integer
// 4: launcher index

enum class FieldKind {
  FLOAT,
  TEXTURE_ID,
  FLOAT_WITH_RAND,
  METER_WITH_RAND,
  DEGREES_WITH_RAND,
  //  INT_WITH_RAND,
  PLAIN_INT,
  PLAIN_INT_WITH_RANDS,
  CPUINFO_FLAGS,
  END_FLAG,
  LAUNCHER_BY_ID,
  NO_FANCY_DECOMP,
  FUNCTION,
  USERDATA,
  ROT_X,
  SOUND_SPEC,
  INVALID
};

struct SparticleFieldDecomp {
  bool known = false;  // error if we try to decomp one that isn't known
  FieldKind kind = FieldKind::INVALID;
};

const SparticleFieldDecomp field_kinds[68] = {
    {false},                                  // MISC_FIELDS_START = 0
    {true, FieldKind::TEXTURE_ID},            // SPT_TEXTURE = 1
    {false},                                  // SPT_ANIM = 2
    {false},                                  // SPT_ANIM_SPEED = 3
    {true, FieldKind::FUNCTION},              // SPT_BIRTH_FUNC = 4
    {false},                                  // SPT_JOINT/REFPOINT = 5
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_NUM = 6
    {true, FieldKind::SOUND_SPEC},            // SPT_SOUND = 7
    {false},                                  // MISC_FIELDS_END = 8
    {false},                                  // SPRITE_FIELDS_START = 9
    {true, FieldKind::METER_WITH_RAND},       // SPT_X = 10
    {true, FieldKind::METER_WITH_RAND},       // SPT_Y = 11
    {true, FieldKind::METER_WITH_RAND},       // SPT_Z = 12
    {true, FieldKind::METER_WITH_RAND},       // SPT_SCALE_X = 13
    {true, FieldKind::ROT_X},                 // SPT_ROT_X = 14
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_ROT_Y = 15
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_ROT_Z = 16
    {true, FieldKind::METER_WITH_RAND},       // SPT_SCALE_Y = 17
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_R = 18
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_G = 19
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_B = 20
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_A = 21
    {false},                                  // SPRITE_FIELDS_END = 22
    {false},                                  // CPU_FIELDS_START = 23
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_OMEGA = 24
    {true, FieldKind::METER_WITH_RAND},       // SPT_VEL_X = 25  (likely m/s)
    {true, FieldKind::METER_WITH_RAND},       // SPT_VEL_Y = 26
    {true, FieldKind::METER_WITH_RAND},       // SPT_VEL_Z = 27
    {true, FieldKind::METER_WITH_RAND},       // SPT_SCALEVEL_X = 28
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_ROTVEL_X = 29
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_ROTVEL_Y = 30
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_ROTVEL_Z = 31
    {true, FieldKind::METER_WITH_RAND},       // SPT_SCALEVEL_Y = 32
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_FADE_R = 33
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_FADE_G = 34
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_FADE_B = 35
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_FADE_A = 36
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_ACCEL_X = 37
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_ACCEL_Y = 38
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_ACCEL_Z = 39
    {false},                                  // SPT_DUMMY = 40
    {false},                                  // SPT_QUAT_X = 41
    {false},                                  // SPT_QUAT_Y = 42
    {false},                                  // SPT_QUAT_Z = 43
    {false},                                  // SPT_QUAD_W = 44
    {true, FieldKind::FLOAT_WITH_RAND},       // SPT_FRICTION = 45
    {true, FieldKind::PLAIN_INT_WITH_RANDS},  // SPT_TIMER = 46
    {true, FieldKind::CPUINFO_FLAGS},         // SPT_FLAGS = 47
    {true, FieldKind::USERDATA},              // SPT_USERDATA = 48
    {true, FieldKind::FUNCTION},              // SPT_FUNC = 49
    {true, FieldKind::PLAIN_INT_WITH_RANDS},  // SPT_NEXT_TIME = 50
    {true, FieldKind::LAUNCHER_BY_ID},        // SPT_NEXT_LAUNCHER = 51
    {false},                                  // CPU_FIELDS_END = 52
    {false},                                  // LAUNCH_FIELDS_START = 53
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_LAUNCHROT_X = 54
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_LAUNCHROT_Y = 55
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_LAUNCHROT_Z = 56
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_LAUNCHROT_W = 57
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_CONEROT_X = 58
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_CONEROT_Y = 59
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_CONEROT_Z = 60
    {false},                                  // SPT_CONEROT_W = 61
    {true, FieldKind::METER_WITH_RAND},       // SPT_CONEROT_RADIUS = 62
    {true, FieldKind::DEGREES_WITH_RAND},     // SPT_ROTATE_Y = 63
    {false},                                  // LAUNCH_FIELDS_END = 64
    {false},                                  // SPT_SCALE = 65
    {false},                                  // SPT_SCALEVEL = 66
    {true, FieldKind::END_FLAG},              // SPT_END = 67

};

std::string make_flags_str(const std::vector<std::string>& flags) {
  if (flags.empty()) {
    return "";
  }
  std::string result = " :flags (";
  for (auto& x : flags) {
    result += x;
    result += ' ';
  }
  result.pop_back();
  result += ')';
  return result;
}

goos::Object decompile_sparticle_tex_field_init(const std::vector<LinkedWord>& words,
                                                const TypeSystem& ts,
                                                const std::string& field_name,
                                                const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "plain-v1");

  auto tex_id_type = TypeSpec("texture-id");
  auto tex_id_str = bitfield_defs_print(
      tex_id_type, decompile_bitfield_from_int(tex_id_type, ts, words.at(1).data));

  return pretty_print::to_symbol(fmt::format("(sp-tex {} {})", field_name, tex_id_str.print()));
}

float word_as_float(const LinkedWord& w) {
  ASSERT(w.kind() == LinkedWord::PLAIN_DATA);
  float v;
  memcpy(&v, &w.data, 4);
  return v;
}

s32 word_as_s32(const LinkedWord& w) {
  ASSERT(w.kind() == LinkedWord::PLAIN_DATA);
  return w.data;
}

goos::Object decompile_sparticle_func(const std::vector<LinkedWord>& words,
                                      const std::string& field_name,
                                      const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::SYM_PTR);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "from-pointer");
  return pretty_print::to_symbol(
      fmt::format("(sp-func {} '{})", field_name, words.at(1).symbol_name()));
}

goos::Object decompile_sparticle_end(const std::vector<LinkedWord>& words,
                                     const std::string& field_name,
                                     const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(1).data == 0);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "plain-v1");
  ASSERT(field_name == "spt-end");
  return pretty_print::to_symbol("(sp-end)");
}

goos::Object decompile_sparticle_int_with_rand_to_float(const std::vector<LinkedWord>& words,
                                                        const std::string& field_name,
                                                        const std::string& flag_name) {
  ASSERT(flag_name == "int-with-rand");

  return pretty_print::to_symbol(fmt::format("(sp-rnd-int {} {} {} {})", field_name,
                                             word_as_s32(words.at(1)), word_as_s32(words.at(2)),
                                             float_to_string(word_as_float(words.at(3)))));
}

goos::Object decompile_sparticle_float_with_rand_init(const std::vector<LinkedWord>& words,
                                                      const std::string& field_name,
                                                      const std::string& flag_name) {
  if (flag_name == "int-with-rand") {
    return decompile_sparticle_int_with_rand_to_float(words, field_name, flag_name);
  }
  ASSERT(flag_name == "float-with-rand");

  float range = word_as_float(words.at(2));
  float mult = word_as_float(words.at(3));

  if (range == 0.f && mult == 1.f) {
    return pretty_print::to_symbol(
        fmt::format("(sp-flt {} {})", field_name, float_to_string(word_as_float(words.at(1)))));
  } else {
    return pretty_print::to_symbol(fmt::format("(sp-rnd-flt {} {} {} {})", field_name,
                                               float_to_string(word_as_float(words.at(1))),
                                               float_to_string(range), float_to_string(mult)));
  }
}

goos::Object decompile_sparticle_userdata(const std::vector<LinkedWord>& words,
                                          const std::string& field_name,
                                          const std::string& flag_name,
                                          const goos::Object& original) {
  if (flag_name == "int-with-rand" || flag_name == "float-with-rand") {
    return decompile_sparticle_float_with_rand_init(words, field_name, flag_name);
  } else {
    return original;
  }
}

goos::Object decompile_sparticle_int_init(const std::vector<LinkedWord>& words,
                                          const std::string& field_name,
                                          const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 1);
  ASSERT(flag_name == "plain-v1");
  return pretty_print::to_symbol(
      fmt::format("(sp-int {} {})", field_name, word_as_s32(words.at(1))));
}

goos::Object decompile_sparticle_rot_x(const std::vector<LinkedWord>& words,
                                       const std::string& field_name,
                                       const std::string& flag_name) {
  if (flag_name == "int-with-rand" || flag_name == "float-with-rand") {
    return decompile_sparticle_float_with_rand_init(words, field_name, flag_name);
  } else {
    return decompile_sparticle_int_init(words, field_name, flag_name);
  }
}

goos::Object decompile_sparticle_int_with_rand_init(const std::vector<LinkedWord>& words,
                                                    const std::string& field_name,
                                                    const std::string& flag_name) {
  if (flag_name != "plain-v1") {
    fmt::print("Bad {} {}\n", field_name, flag_name);
  }
  ASSERT(flag_name == "plain-v1");
  if (word_as_s32(words.at(2)) == 0 && word_as_s32(words.at(3)) == 1) {
    return decompile_sparticle_int_init(words, field_name, flag_name);
  }
  return pretty_print::to_symbol(fmt::format("(sp-int-plain-rnd {} {} {} {})", field_name,
                                             word_as_s32(words.at(1)), word_as_s32(words.at(2)),
                                             word_as_s32(words.at(3))));
}

goos::Object decompile_sparticle_launcher_by_id(const std::vector<LinkedWord>& words,
                                                const std::string& field_name,
                                                const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "part-by-id");
  return pretty_print::to_symbol(
      fmt::format("(sp-launcher-by-id {} {})", field_name, word_as_s32(words.at(1))));
}

goos::Object decompile_sparticle_flags(const std::vector<LinkedWord>& words,
                                       const TypeSystem& ts,
                                       const std::string& field_name,
                                       const std::string& flag_name) {
  ASSERT(flag_name == "plain-v1");
  ASSERT(field_name == "spt-flags");
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 1);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  auto flag_def =
      decompile_bitfield_enum_from_int(TypeSpec("sp-cpuinfo-flag"), ts, word_as_s32(words.at(1)));
  std::string result = "(sp-cpuinfo-flags";
  for (const auto& def : flag_def) {
    result += ' ';
    result += def;
  }
  result += ')';
  return pretty_print::to_symbol(result);
}

goos::Object decompile_sparticle_from_other(const std::vector<LinkedWord>& words,
                                            const std::string& field_name,
                                            const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 1);
  ASSERT(flag_name == "copy-from-other-field");
  return pretty_print::to_symbol(
      fmt::format("(sp-copy-from-other {} {})", field_name, word_as_s32(words.at(1))));
}

goos::Object decompile_sparticle_float_meters_with_rand_init(const std::vector<LinkedWord>& words,
                                                             const std::string& field_name,
                                                             const std::string& flag_name) {
  if (flag_name == "int-with-rand") {
    return pretty_print::to_symbol(fmt::format("(sp-rnd-int-flt {} (meters {}) {} {})", field_name,
                                               meters_to_string(word_as_float(words.at(1))),
                                               word_as_s32(words.at(2)),
                                               float_to_string(word_as_float(words.at(3)))));
  }
  ASSERT(flag_name == "float-with-rand");

  float range = word_as_float(words.at(2));
  float mult = word_as_float(words.at(3));

  if (range == 0.f && mult == 1.f) {
    return pretty_print::to_symbol(fmt::format("(sp-flt {} (meters {}))", field_name,
                                               meters_to_string(word_as_float(words.at(1)))));
  } else {
    return pretty_print::to_symbol(fmt::format(
        "(sp-rnd-flt {} (meters {}) (meters {}) {})", field_name,
        meters_to_string(word_as_float(words.at(1))), meters_to_string(word_as_float(words.at(2))),
        float_to_string(word_as_float(words.at(3)))));
  }
}

goos::Object decompile_sparticle_float_degrees_with_rand_init(const std::vector<LinkedWord>& words,
                                                              const std::string& field_name,
                                                              const std::string& flag_name) {
  if (flag_name == "int-with-rand") {
    return pretty_print::to_symbol(
        fmt::format("(sp-rnd-int-flt {} (degrees {}) {} {})", field_name,
                    float_to_string(word_as_float(words.at(1)) / DEGREES_LENGTH),
                    word_as_s32(words.at(2)), float_to_string(word_as_float(words.at(3)))));
  }
  ASSERT(flag_name == "float-with-rand");
  float range = word_as_float(words.at(2));
  float mult = word_as_float(words.at(3));
  if (range == 0.f && mult == 1.f) {
    return pretty_print::to_symbol(
        fmt::format("(sp-flt {} (degrees {}))", field_name,
                    float_to_string(word_as_float(words.at(1)) / DEGREES_LENGTH)));
  } else {
    return pretty_print::to_symbol(
        fmt::format("(sp-rnd-flt {} (degrees {}) (degrees {}) {})", field_name,
                    float_to_string(word_as_float(words.at(1)) / DEGREES_LENGTH),
                    float_to_string(word_as_float(words.at(2)) / DEGREES_LENGTH),
                    float_to_string(word_as_float(words.at(3)))));
  }
}

goos::Object decompile_sparticle_sound_spec(const std::vector<LinkedWord>& /*words*/,
                                            const std::string& field_name,
                                            const std::string& flag_name,
                                            const goos::Object& original) {
  ASSERT(field_name == "spt-sound");
  ASSERT(flag_name == "plain-v2");
  return pretty_print::build_list("sp-sound", original);
}

goos::Object decompile_sparticle_group_item(const TypeSpec& type,
                                            const DecompilerLabel& label,
                                            const std::vector<DecompilerLabel>& /*labels*/,
                                            const std::vector<std::vector<LinkedWord>>& words,
                                            const TypeSystem& ts,
                                            const LinkedObjectFile* /*file*/) {
  // auto normal = decompile_structure(type, label, labels, words, ts, file, false);
  // fmt::print("Doing: {}\n", normal.print());
  auto uncast_type_info = ts.lookup_type(type);
  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
  if (!type_info) {
    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", type.print()));
  }
  ASSERT(type_info->get_size_in_memory() == 0x1c);

  // get words for real
  auto offset_location = label.offset - type_info->get_offset();
  int word_count = (type_info->get_size_in_memory() + 3) / 4;
  std::vector<LinkedWord> obj_words;
  obj_words.insert(obj_words.begin(),
                   words.at(label.target_segment).begin() + (offset_location / 4),
                   words.at(label.target_segment).begin() + (offset_location / 4) + word_count);

  // 0 launcher
  // 4 fade-after (meters)
  // 8 falloff-to (meters)
  // flags, period
  // length, offset
  // hour-mask
  // binding

  s32 launcher = word_as_s32(obj_words.at(0));
  float fade_after = word_as_float(obj_words.at(1));
  float falloff_to = word_as_float(obj_words.at(2));
  u32 fp = word_as_s32(obj_words.at(3));
  u16 flags = fp & 0xffff;
  u16 period = fp >> 16;
  u32 lo = word_as_s32(obj_words.at(4));
  u16 length = lo & 0xffff;
  u16 offset = lo >> 16;
  u32 hour_mask = word_as_s32(obj_words.at(5));
  u32 binding = word_as_s32(obj_words.at(6));

  std::string result =
      fmt::format("(sp-item {}", launcher);  // use decimal, so it matches array idx

  if (fade_after != 0.0) {
    result += fmt::format(" :fade-after (meters {})", meters_to_string(fade_after));
  }

  if (falloff_to != 0.0) {
    result += fmt::format(" :falloff-to (meters {})", meters_to_string(falloff_to));
  }

  if (flags) {
    auto things = decompile_bitfield_enum_from_int(TypeSpec("sp-group-item-flag"), ts, flags);
    result += " :flags (";
    for (auto& thing : things) {
      result += thing;
      result += ' ';
    }
    result.pop_back();
    result += ')';
  }

  if (period) {
    result += fmt::format(" :period {}", period);
  }

  if (length) {
    result += fmt::format(" :length {}", length);
  }

  if (offset) {
    result += fmt::format(" :offset {}", offset);
  }

  if (hour_mask) {
    result += fmt::format(" :hour-mask #b{:b}", hour_mask);
  }

  if (binding) {
    result += fmt::format(" :binding {}", binding);
  }

  result += ')';
  // fmt::print("Result: {}\n", result);
  return pretty_print::to_symbol(result);
}

goos::Object decompile_sparticle_field_init(const TypeSpec& type,
                                            const DecompilerLabel& label,
                                            const std::vector<DecompilerLabel>& labels,
                                            const std::vector<std::vector<LinkedWord>>& words,
                                            const TypeSystem& ts,
                                            const LinkedObjectFile* file) {
  auto normal = decompile_structure(type, label, labels, words, ts, file, false);
  // fmt::print("Doing: {}\n", normal.print());
  auto uncast_type_info = ts.lookup_type(type);
  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
  if (!type_info) {
    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", type.print()));
  }
  ASSERT(type_info->get_size_in_memory() == 16);

  // get words for real
  auto offset_location = label.offset - type_info->get_offset();
  int word_count = (type_info->get_size_in_memory() + 3) / 4;
  std::vector<LinkedWord> obj_words;
  obj_words.insert(obj_words.begin(),
                   words.at(label.target_segment).begin() + (offset_location / 4),
                   words.at(label.target_segment).begin() + (offset_location / 4) + word_count);

  ASSERT(obj_words.at(0).kind() == LinkedWord::PLAIN_DATA);
  u16 field_id = obj_words.at(0).data & 0xffff;
  u16 flags = obj_words.at(0).data >> 16;

  ASSERT(field_id <= (u32)FieldId::SPT_END);
  auto field_name = decompile_int_enum_from_int(TypeSpec("sp-field-id"), ts, field_id);
  const auto& field_info = field_kinds[field_id];
  if (!field_info.known) {
    throw std::runtime_error("Unknown sparticle field: " + field_name);
  }

  auto flag_name = decompile_int_enum_from_int(TypeSpec("sp-flag"), ts, flags);
  goos::Object result;

  if (flag_name == "copy-from-other-field") {
    result = decompile_sparticle_from_other(obj_words, field_name, flag_name);
  } else {
    switch (field_info.kind) {
      case FieldKind::TEXTURE_ID:
        result = decompile_sparticle_tex_field_init(obj_words, ts, field_name, flag_name);
        break;
      case FieldKind::FLOAT_WITH_RAND:
        result = decompile_sparticle_float_with_rand_init(obj_words, field_name, flag_name);
        break;
      case FieldKind::METER_WITH_RAND:
        result = decompile_sparticle_float_meters_with_rand_init(obj_words, field_name, flag_name);
        break;
      case FieldKind::DEGREES_WITH_RAND:
        result = decompile_sparticle_float_degrees_with_rand_init(obj_words, field_name, flag_name);
        break;
        //      case FieldKind::INT_WITH_RAND:
        //        result = decompile_sparticle_int_with_rand_init(obj_words, field_name, flag_name);
        //        break;
      case FieldKind::PLAIN_INT_WITH_RANDS:
        result = decompile_sparticle_int_with_rand_init(obj_words, field_name, flag_name);
        break;
      case FieldKind::PLAIN_INT:
        result = decompile_sparticle_int_init(obj_words, field_name, flag_name);
        break;
      case FieldKind::CPUINFO_FLAGS:
        result = decompile_sparticle_flags(obj_words, ts, field_name, flag_name);
        break;
      case FieldKind::END_FLAG:
        result = decompile_sparticle_end(obj_words, field_name, flag_name);
        break;
      case FieldKind::LAUNCHER_BY_ID:
        result = decompile_sparticle_launcher_by_id(obj_words, field_name, flag_name);
        break;
      case FieldKind::SOUND_SPEC:
      case FieldKind::NO_FANCY_DECOMP:
        result = normal;
        break;
      case FieldKind::FUNCTION:
        result = decompile_sparticle_func(obj_words, field_name, flag_name);
        break;
      case FieldKind::USERDATA:
        result = decompile_sparticle_userdata(obj_words, field_name, flag_name, normal);
        break;
      case FieldKind::ROT_X:
        result = decompile_sparticle_rot_x(obj_words, field_name, flag_name);
        break;
      default:
        ASSERT(false);
    }
  }

  // fmt::print("Result: {}\n\n", result.print());
  return result;
}

goos::Object decompile_sparticle_userdata_ASSERT(const std::vector<LinkedWord>& words,
                                                 const std::string& field_name,
                                                 const std::string& flag_name) {
  if (flag_name == "int-with-rand" || flag_name == "float-with-rand") {
    return decompile_sparticle_float_with_rand_init(words, field_name, flag_name);
  } else {
    ASSERT(false);
  }
}

goos::Object decompile_sparticle_field_init(const DefpartElement::StaticInfo::PartField& field,
                                            const TypeSystem& ts) {
  auto field_id = field.field_id;
  auto flags = field.flags;

  ASSERT(field_id <= (u32)FieldId::SPT_END);
  auto field_name = decompile_int_enum_from_int(TypeSpec("sp-field-id"), ts, field_id);
  const auto& field_info = field_kinds[field_id];
  if (!field_info.known) {
    throw std::runtime_error("Unknown sparticle field: " + field_name);
  }

  auto flag_name = decompile_int_enum_from_int(TypeSpec("sp-flag"), ts, flags);
  goos::Object result;

  if (flag_name == "copy-from-other-field") {
    result = decompile_sparticle_from_other(field.data, field_name, flag_name);
  } else {
    switch (field_info.kind) {
      case FieldKind::TEXTURE_ID:
        result = decompile_sparticle_tex_field_init(field.data, ts, field_name, flag_name);
        break;
      case FieldKind::FLOAT_WITH_RAND:
        result = decompile_sparticle_float_with_rand_init(field.data, field_name, flag_name);
        break;
      case FieldKind::METER_WITH_RAND:
        result = decompile_sparticle_float_meters_with_rand_init(field.data, field_name, flag_name);
        break;
      case FieldKind::DEGREES_WITH_RAND:
        result =
            decompile_sparticle_float_degrees_with_rand_init(field.data, field_name, flag_name);
        break;
        //      case FieldKind::INT_WITH_RAND:
        //        result = decompile_sparticle_int_with_rand_init(field.data, field_name,
        //        flag_name); break;
      case FieldKind::PLAIN_INT_WITH_RANDS:
        result = decompile_sparticle_int_with_rand_init(field.data, field_name, flag_name);
        break;
      case FieldKind::PLAIN_INT:
        result = decompile_sparticle_int_init(field.data, field_name, flag_name);
        break;
      case FieldKind::CPUINFO_FLAGS:
        result = decompile_sparticle_flags(field.data, ts, field_name, flag_name);
        break;
      case FieldKind::END_FLAG:
        result = decompile_sparticle_end(field.data, field_name, flag_name);
        break;
      case FieldKind::LAUNCHER_BY_ID:
        result = decompile_sparticle_launcher_by_id(field.data, field_name, flag_name);
        break;
      case FieldKind::NO_FANCY_DECOMP:
        ASSERT(false);
        break;
      case FieldKind::FUNCTION:
        result = decompile_sparticle_func(field.data, field_name, flag_name);
        break;
      case FieldKind::USERDATA:
        result = decompile_sparticle_userdata_ASSERT(field.data, field_name, flag_name);
        break;
      case FieldKind::ROT_X:
        result = decompile_sparticle_rot_x(field.data, field_name, flag_name);
        break;
      case FieldKind::SOUND_SPEC:
        result =
            decompile_sparticle_sound_spec(field.data, field_name, flag_name, field.sound_spec);
        break;
      default:
        ASSERT(false);
    }
  }

  // fmt::print("Result: {}\n\n", result.print());
  return result;
}
}  // namespace decompiler

/*
(deftype sp-field-init-spec (structure)
  ((field          sp-field-id  :offset-assert 0)
   (flags          sp-flag  :offset-assert 2)
   (initial-valuef float   :offset-assert 4)
   (random-rangef  float   :offset-assert 8)
   (random-multf   float   :offset-assert 12)
   (initial-value  int32   :offset 4)
   (random-range   int32   :offset 8)
   (random-mult    int32   :offset 12)
   (sym            symbol  :offset 4) ;; moved
   (func           function  :offset 4)
   (tex            uint32  :offset 4)
   (pntr           pointer :offset 4)
   ;; gap
   (sound          basic   :offset 4)
   )
  :method-count-assert 9
  :size-assert         #x10
  :flag-assert         #x900000010
  )
 */
