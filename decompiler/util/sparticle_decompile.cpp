#include "sparticle_decompile.h"

#include "common/goos/PrettyPrinter.h"
#include "common/util/Assert.h"
#include "common/util/print_float.h"

#include "decompiler/util/data_decompile.h"

#include "third-party/fmt/format.h"

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

// jak2 version
enum class FieldId2 {
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
  SPT_ROTATE_X = 62,
  SPT_ROTATE_Y = 63,
  SPT_ROTATE_Z = 64,
  SPT_CONEROT_RADIUS = 65,
  SPT_MAT_SCALE_X = 66,
  SPT_MAT_SCALE_Y = 67,
  SPT_MAT_SCALE_Z = 68,
  LAUNCH_FIELDS_END = 69,
  SPT_SCALE = 70,
  SPT_SCALEVEL = 71,
  SPT_END = 72,
};

// NOTE : "per second" here means it's in 1/60th instead of the usual 1/300ths
constexpr bool allow_per_sec = false;

enum class FieldKind {
  TEXTURE_ID,
  INT,
  SECONDS,
  FLOAT,
  FLOAT_PER_SEC,
  METERS,
  METERS_PER_SEC,
  DEGREES,
  DEGREES_PER_SEC,
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

const SparticleFieldDecomp field_kind_jak1[68] = {
    {false},                             // MISC_FIELDS_START = 0
    {true, FieldKind::TEXTURE_ID},       // SPT_TEXTURE = 1
    {false},                             // SPT_ANIM = 2
    {false},                             // SPT_ANIM_SPEED = 3
    {true, FieldKind::FUNCTION},         // SPT_BIRTH_FUNC = 4
    {false},                             // SPT_JOINT/REFPOINT = 5
    {true, FieldKind::FLOAT},            // SPT_NUM = 6
    {true, FieldKind::SOUND_SPEC},       // SPT_SOUND = 7
    {false},                             // MISC_FIELDS_END = 8
    {false},                             // SPRITE_FIELDS_START = 9
    {true, FieldKind::METERS},           // SPT_X = 10
    {true, FieldKind::METERS},           // SPT_Y = 11
    {true, FieldKind::METERS},           // SPT_Z = 12
    {true, FieldKind::METERS},           // SPT_SCALE_X = 13
    {true, FieldKind::ROT_X},            // SPT_ROT_X = 14
    {true, FieldKind::DEGREES},          // SPT_ROT_Y = 15
    {true, FieldKind::DEGREES},          // SPT_ROT_Z = 16
    {true, FieldKind::METERS},           // SPT_SCALE_Y = 17
    {true, FieldKind::FLOAT},            // SPT_R = 18
    {true, FieldKind::FLOAT},            // SPT_G = 19
    {true, FieldKind::FLOAT},            // SPT_B = 20
    {true, FieldKind::FLOAT},            // SPT_A = 21
    {false},                             // SPRITE_FIELDS_END = 22
    {false},                             // CPU_FIELDS_START = 23
    {true, FieldKind::FLOAT},            // SPT_OMEGA = 24
    {true, FieldKind::METERS_PER_SEC},   // SPT_VEL_X = 25  (likely m/s)
    {true, FieldKind::METERS_PER_SEC},   // SPT_VEL_Y = 26
    {true, FieldKind::METERS_PER_SEC},   // SPT_VEL_Z = 27
    {true, FieldKind::METERS_PER_SEC},   // SPT_SCALEVEL_X = 28
    {true, FieldKind::DEGREES_PER_SEC},  // SPT_ROTVEL_X = 29
    {true, FieldKind::DEGREES_PER_SEC},  // SPT_ROTVEL_Y = 30
    {true, FieldKind::DEGREES_PER_SEC},  // SPT_ROTVEL_Z = 31
    {true, FieldKind::METERS_PER_SEC},   // SPT_SCALEVEL_Y = 32
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_R = 33
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_G = 34
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_B = 35
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_A = 36
    {true, FieldKind::METERS_PER_SEC},   // SPT_ACCEL_X = 37
    {true, FieldKind::METERS_PER_SEC},   // SPT_ACCEL_Y = 38
    {true, FieldKind::METERS_PER_SEC},   // SPT_ACCEL_Z = 39
    {false},                             // SPT_DUMMY = 40
    {false},                             // SPT_QUAT_X = 41
    {false},                             // SPT_QUAT_Y = 42
    {false},                             // SPT_QUAT_Z = 43
    {false},                             // SPT_QUAD_W = 44
    {true, FieldKind::FLOAT},            // SPT_FRICTION = 45
    {true, FieldKind::SECONDS},          // SPT_TIMER = 46
    {true, FieldKind::CPUINFO_FLAGS},    // SPT_FLAGS = 47
    {true, FieldKind::USERDATA},         // SPT_USERDATA = 48
    {true, FieldKind::FUNCTION},         // SPT_FUNC = 49
    {true, FieldKind::SECONDS},          // SPT_NEXT_TIME = 50
    {true, FieldKind::LAUNCHER_BY_ID},   // SPT_NEXT_LAUNCHER = 51
    {false},                             // CPU_FIELDS_END = 52
    {false},                             // LAUNCH_FIELDS_START = 53
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_X = 54
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_Y = 55
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_Z = 56
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_W = 57
    {true, FieldKind::DEGREES},          // SPT_CONEROT_X = 58
    {true, FieldKind::DEGREES},          // SPT_CONEROT_Y = 59
    {true, FieldKind::DEGREES},          // SPT_CONEROT_Z = 60
    {false},                             // SPT_CONEROT_W = 61
    {true, FieldKind::METERS},           // SPT_CONEROT_RADIUS = 62
    {true, FieldKind::DEGREES},          // SPT_ROTATE_Y = 63
    {false},                             // LAUNCH_FIELDS_END = 64
    {false},                             // SPT_SCALE = 65
    {false},                             // SPT_SCALEVEL = 66
    {true, FieldKind::END_FLAG}          // SPT_END = 67
};

const SparticleFieldDecomp field_kind_jak2[73] = {
    {false},                             // MISC_FIELDS_START = 0
    {true, FieldKind::TEXTURE_ID},       // SPT_TEXTURE = 1
    {false},                             // SPT_ANIM = 2
    {false},                             // SPT_ANIM_SPEED = 3
    {true, FieldKind::FUNCTION},         // SPT_BIRTH_FUNC = 4
    {false},                             // SPT_JOINT/REFPOINT = 5
    {true, FieldKind::FLOAT},            // SPT_NUM = 6
    {true, FieldKind::SOUND_SPEC},       // SPT_SOUND = 7
    {false},                             // MISC_FIELDS_END = 8
    {false},                             // SPRITE_FIELDS_START = 9
    {true, FieldKind::METERS},           // SPT_X = 10
    {true, FieldKind::METERS},           // SPT_Y = 11
    {true, FieldKind::METERS},           // SPT_Z = 12
    {true, FieldKind::METERS},           // SPT_SCALE_X = 13
    {true, FieldKind::ROT_X},            // SPT_ROT_X = 14
    {true, FieldKind::DEGREES},          // SPT_ROT_Y = 15
    {true, FieldKind::DEGREES},          // SPT_ROT_Z = 16
    {true, FieldKind::METERS},           // SPT_SCALE_Y = 17
    {true, FieldKind::FLOAT},            // SPT_R = 18
    {true, FieldKind::FLOAT},            // SPT_G = 19
    {true, FieldKind::FLOAT},            // SPT_B = 20
    {true, FieldKind::FLOAT},            // SPT_A = 21
    {false},                             // SPRITE_FIELDS_END = 22
    {false},                             // CPU_FIELDS_START = 23
    {true, FieldKind::DEGREES},          // SPT_OMEGA = 24
    {true, FieldKind::METERS_PER_SEC},   // SPT_VEL_X = 25  (likely m/s)
    {true, FieldKind::METERS_PER_SEC},   // SPT_VEL_Y = 26
    {true, FieldKind::METERS_PER_SEC},   // SPT_VEL_Z = 27
    {true, FieldKind::METERS_PER_SEC},   // SPT_SCALEVEL_X = 28
    {true, FieldKind::DEGREES_PER_SEC},  // SPT_ROTVEL_X = 29
    {true, FieldKind::DEGREES_PER_SEC},  // SPT_ROTVEL_Y = 30
    {true, FieldKind::DEGREES_PER_SEC},  // SPT_ROTVEL_Z = 31
    {true, FieldKind::METERS_PER_SEC},   // SPT_SCALEVEL_Y = 32
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_R = 33
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_G = 34
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_B = 35
    {true, FieldKind::FLOAT_PER_SEC},    // SPT_FADE_A = 36
    {true, FieldKind::METERS_PER_SEC},   // SPT_ACCEL_X = 37
    {true, FieldKind::METERS_PER_SEC},   // SPT_ACCEL_Y = 38
    {true, FieldKind::METERS_PER_SEC},   // SPT_ACCEL_Z = 39
    {false},                             // SPT_DUMMY = 40
    {false},                             // SPT_QUAT_X = 41
    {false},                             // SPT_QUAT_Y = 42
    {false},                             // SPT_QUAT_Z = 43
    {false},                             // SPT_QUAD_W = 44
    {true, FieldKind::FLOAT},            // SPT_FRICTION = 45
    {true, FieldKind::SECONDS},          // SPT_TIMER = 46
    {true, FieldKind::CPUINFO_FLAGS},    // SPT_FLAGS = 47
    {true, FieldKind::USERDATA},         // SPT_USERDATA = 48
    {true, FieldKind::FUNCTION},         // SPT_FUNC = 49
    {true, FieldKind::SECONDS},          // SPT_NEXT_TIME = 50
    {true, FieldKind::LAUNCHER_BY_ID},   // SPT_NEXT_LAUNCHER = 51
    {false},                             // CPU_FIELDS_END = 52
    {false},                             // LAUNCH_FIELDS_START = 53
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_X = 54
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_Y = 55
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_Z = 56
    {true, FieldKind::DEGREES},          // SPT_LAUNCHROT_W = 57
    {true, FieldKind::DEGREES},          // SPT_CONEROT_X = 58
    {true, FieldKind::DEGREES},          // SPT_CONEROT_Y = 59
    {true, FieldKind::DEGREES},          // SPT_CONEROT_Z = 60
    {false},                             // SPT_CONEROT_W = 61
    {true, FieldKind::DEGREES},          // SPT_ROTATE_X = 62
    {true, FieldKind::DEGREES},          // SPT_ROTATE_Y = 63
    {true, FieldKind::DEGREES},          // SPT_ROTATE_Z = 64
    {true, FieldKind::METERS},           // SPT_CONEROT_RADIUS = 65
    {true, FieldKind::METERS},           // SPT_MAT_SCALE_X = 66
    {true, FieldKind::METERS},           // SPT_MAT_SCALE_X = 67
    {true, FieldKind::METERS},           // SPT_MAT_SCALE_X = 68
    {false},                             // LAUNCH_FIELDS_END = 69
    {false},                             // SPT_SCALE = 70
    {false},                             // SPT_SCALEVEL = 71
    {true, FieldKind::END_FLAG}          // SPT_END = 72
};

const std::unordered_map<GameVersion, const SparticleFieldDecomp*> field_kinds = {
    {GameVersion::Jak1, field_kind_jak1},
    {GameVersion::Jak2, field_kind_jak2}};

float word_as_float(const LinkedWord& w) {
  ASSERT(w.kind() == LinkedWord::PLAIN_DATA);
  float v;
  memcpy(&v, &w.data, 4);
  return v;
}

void assert_spec_flag_int_single_field(const std::vector<LinkedWord>& words,
                                       const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "int");
}

void assert_spec_flag_int(const std::vector<LinkedWord>& words, const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(flag_name == "int");
}

void assert_spec_flag_int_no_rand(const std::vector<LinkedWord>& words,
                                  const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 1);
  ASSERT(flag_name == "int");
}

void assert_spec_flag_launcher(const std::vector<LinkedWord>& words, const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "launcher");
}

void assert_spec_flag_copy_from_other(const std::vector<LinkedWord>& words,
                                      const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 1);
  ASSERT(flag_name == "copy-from-other");
}

void assert_spec_flag_float(const std::vector<LinkedWord>& words, const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(word_as_float(words.at(2)) != 0.f || word_as_float(words.at(3)) == 1.f);
  ASSERT(flag_name == "float" || flag_name == "float-int-rand" || flag_name == "float-store");
}

std::string decompile_sparticle_texture(const std::vector<LinkedWord>& words,
                                        const TypeSystem& ts,
                                        const std::string& flag_name) {
  assert_spec_flag_int_single_field(words, flag_name);

  const auto tex_id_type = TypeSpec("texture-id");
  auto tex_id_str = bitfield_defs_print(
      tex_id_type, decompile_bitfield_from_int(tex_id_type, ts, words.at(1).data));

  return tex_id_str.print();
}

s32 word_as_s32(const LinkedWord& w) {
  ASSERT(w.kind() == LinkedWord::PLAIN_DATA);
  return w.data;
}

std::string decompile_sparticle_func(const std::vector<LinkedWord>& words,
                                     const std::string& flag_name) {
  ASSERT(words.at(1).kind() == LinkedWord::SYM_PTR);
  ASSERT(words.at(2).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(2).data == 0);
  ASSERT(words.at(3).kind() == LinkedWord::PLAIN_DATA);
  ASSERT(words.at(3).data == 0);
  ASSERT(flag_name == "symbol");
  return fmt::format("'{}", words.at(1).symbol_name());
}

std::string decompile_sparticle_end(const std::vector<LinkedWord>& words,
                                    const std::string& field_name,
                                    const std::string& flag_name) {
  assert_spec_flag_int_single_field(words, flag_name);
  ASSERT(words.at(1).data == 0);
  ASSERT(field_name == "spt-end");
  return "";
}

std::string decompile_sparticle_float(const std::vector<LinkedWord>& words,
                                      const std::string& flag_name,
                                      bool per_sec) {
  assert_spec_flag_float(words, flag_name);

  per_sec = per_sec && allow_per_sec;
  float base = word_as_float(words.at(1));
  float range = word_as_float(words.at(2));
  float mult = word_as_float(words.at(3));
  if (per_sec) {
    base *= 60;
  }

  if (flag_name == "float-int-rand") {
    if (per_sec) {
      mult *= 60;
    }
    return fmt::format("{} {} {}", float_to_string(base), word_as_s32(words.at(2)),
                       float_to_string(mult));
  }

  if (range == 0.f && mult == 1.f) {
    return fmt::format("{}", float_to_string(base));
  } else if (mult != 1.f) {
    if (per_sec) {
      mult *= 60;
    }
    return fmt::format("{} {} {}", float_to_string(base), float_to_string(range),
                       float_to_string(mult));
  } else {
    if (per_sec) {
      range *= 60;
    }
    return fmt::format("{} {}", float_to_string(base), float_to_string(range));
  }
}

std::string decompile_sparticle_userdata(const std::vector<LinkedWord>& words,
                                         const std::string& flag_name,
                                         const goos::Object& original) {
  if (flag_name == "float-int-rand" || flag_name == "float") {
    return decompile_sparticle_float(words, flag_name, false);
  } else {
    return original.print();
  }
}

std::string decompile_sparticle_int_init(const std::vector<LinkedWord>& words,
                                         const std::string& flag_name) {
  assert_spec_flag_int_no_rand(words, flag_name);
  return fmt::format("{}", word_as_s32(words.at(1)));
}

std::string decompile_sparticle_int(const std::vector<LinkedWord>& words,
                                    const std::string& flag_name) {
  assert_spec_flag_int(words, flag_name);

  int base = word_as_s32(words.at(1));
  int range = word_as_s32(words.at(2));
  int mult = word_as_s32(words.at(3));

  if (range == 0 && mult == 1) {
    return fmt::format("{}", base);
  } else if (mult != 1) {
    return fmt::format("{} {} {}", base, range, mult);
  } else {
    return fmt::format("{} {}", base, range);
  }
}

std::string decompile_sparticle_seconds(const std::vector<LinkedWord>& words,
                                        const std::string& flag_name) {
  assert_spec_flag_int(words, flag_name);

  int base = word_as_s32(words.at(1));
  int range = word_as_s32(words.at(2));
  int mult = word_as_s32(words.at(3));

  if (range == 0 && mult == 1) {
    return fmt::format("(seconds {})", seconds_to_string(base));
  } else if (mult != 1) {
    return fmt::format("(seconds {}) {} (seconds {})", seconds_to_string(base), range,
                       seconds_to_string(mult));
  } else {
    return fmt::format("(seconds {}) (seconds {})", seconds_to_string(base),
                       seconds_to_string(range));
  }
}

std::string decompile_sparticle_launcher_by_id(const std::vector<LinkedWord>& words,
                                               const std::string& flag_name) {
  assert_spec_flag_launcher(words, flag_name);
  return fmt::format("{}", word_as_s32(words.at(1)));
}

std::string decompile_sparticle_flags(const std::vector<LinkedWord>& words,
                                      const TypeSystem& ts,
                                      const std::string& /*field_name*/,
                                      const std::string& flag_name) {
  assert_spec_flag_int_no_rand(words, flag_name);

  auto flag_def =
      decompile_bitfield_enum_from_int(TypeSpec("sp-cpuinfo-flag"), ts, word_as_s32(words.at(1)));
  std::string result = "(";
  for (const auto& def : flag_def) {
    if (result.length() > 1) {
      result += ' ';
    }
    result += def;
  }
  result += ')';
  return result;
}

std::string decompile_sparticle_from_other(const std::vector<LinkedWord>& words,
                                           const std::string& flag_name,
                                           u16 field_id,
                                           const TypeSystem& ts) {
  assert_spec_flag_copy_from_other(words, flag_name);
  int diff = word_as_s32(words.at(1));
  // naughty dog messed up sometimes
  // ASSERT(diff < 0);
  return fmt::format(
      ":copy {}",
      decompile_int_enum_from_int(TypeSpec("sp-field-id"), ts, field_id + diff).substr(4));
}

std::string decompile_sparticle_meters(const std::vector<LinkedWord>& words,
                                       const std::string& flag_name,
                                       bool per_sec) {
  assert_spec_flag_float(words, flag_name);

  per_sec = per_sec && allow_per_sec;
  float base = word_as_float(words.at(1));
  float range = word_as_float(words.at(2));
  float mult = word_as_float(words.at(3));
  if (per_sec) {
    base *= 60;
  }

  if (flag_name == "float-int-rand") {
    if (per_sec) {
      mult *= 60;
    }
    return fmt::format("(meters {}) {} (meters {})", meters_to_string(base),
                       word_as_s32(words.at(2)), meters_to_string(mult));
  }

  if (range == 0.f && mult == 1.f) {
    return fmt::format("(meters {})", meters_to_string(base));
  } else if (mult != 1.f) {
    if (per_sec) {
      mult *= 60;
    }
    return fmt::format("(meters {}) {} (meters {})", meters_to_string(base), float_to_string(range),
                       meters_to_string(mult));
  } else {
    if (per_sec) {
      range *= 60;
    }
    return fmt::format("(meters {}) (meters {})", meters_to_string(base), meters_to_string(range));
  }
}

std::string decompile_sparticle_degrees(const std::vector<LinkedWord>& words,
                                        const std::string& flag_name,
                                        bool per_sec) {
  assert_spec_flag_float(words, flag_name);

  per_sec = per_sec && allow_per_sec;
  float base = word_as_float(words.at(1));
  float range = word_as_float(words.at(2));
  float mult = word_as_float(words.at(3));
  if (per_sec) {
    base *= 60;
  }

  if (flag_name == "float-int-rand") {
    if (per_sec) {
      mult *= 60;
    }
    return fmt::format("(degrees {}) {} (degrees {})", degrees_to_string(base),
                       word_as_s32(words.at(2)), degrees_to_string(mult));
  }

  if (range == 0.f && mult == 1.f) {
    return fmt::format("(degrees {})", degrees_to_string(base));
  } else if (mult != 1.f) {
    if (per_sec) {
      mult *= 60;
    }
    return fmt::format("(degrees {}) {} (degrees {})", degrees_to_string(base),
                       float_to_string(range), degrees_to_string(mult));
  } else {
    if (per_sec) {
      range *= 60;
    }
    return fmt::format("(degrees {}) (degrees {})", degrees_to_string(base),
                       degrees_to_string(range));
  }
}

std::string decompile_sparticle_rot_x(const std::vector<LinkedWord>& words,
                                      const std::string& flag_name) {
  if (flag_name == "float-int-rand" || flag_name == "float" || flag_name == "float-store") {
    return decompile_sparticle_degrees(words, flag_name, false);
  } else {
    return decompile_sparticle_int_init(words, flag_name);
  }
}

goos::Object decompile_sparticle_sound_spec(const std::vector<LinkedWord>& /*words*/,
                                            const std::string& flag_name,
                                            const goos::Object& original) {
  ASSERT(flag_name == "object");
  return original;
}

goos::Object decompile_sparticle_field_init(const std::vector<decompiler::LinkedWord>& words,
                                            u16 field_id,
                                            u16 flags,
                                            goos::Object sound_spec,
                                            goos::Object userdata,
                                            const TypeSystem& ts,
                                            GameVersion version) {
  auto field_name = decompile_int_enum_from_int(TypeSpec("sp-field-id"), ts, field_id);
  const auto& field_info = field_kinds.at(version)[field_id];
  if (!field_info.known) {
    throw std::runtime_error("Unknown sparticle field: " + field_name);
  }

  auto flag_name = decompile_int_enum_from_int(TypeSpec("sp-flag"), ts, flags);
  std::string result;

  // first, handle some specific flags
  if (flag_name == "copy-from-other") {
    result = decompile_sparticle_from_other(words, flag_name, field_id, ts);
  } else if (flag_name == "symbol") {
    // a symbol
    result = decompile_sparticle_func(words, flag_name);
  } else if (flag_name == "object") {
    // static data
    if (field_info.kind == FieldKind::SOUND_SPEC) {
      return pretty_print::build_list(
          pretty_print::to_symbol(fmt::format(":{}", field_name.substr(4))), sound_spec);
    } else {
      return pretty_print::build_list(
          pretty_print::to_symbol(fmt::format(":{}", field_name.substr(4))),
          pretty_print::to_symbol(":data"), sound_spec);
    }
  } else {
    // let's handle things on a more specific level now
    switch (field_info.kind) {
      case FieldKind::TEXTURE_ID:
        result = decompile_sparticle_texture(words, ts, flag_name);
        break;
      case FieldKind::FLOAT:
        result = decompile_sparticle_float(words, flag_name, false);
        break;
      case FieldKind::FLOAT_PER_SEC:
        result = decompile_sparticle_float(words, flag_name, true);
        break;
      case FieldKind::METERS:
        result = decompile_sparticle_meters(words, flag_name, false);
        break;
      case FieldKind::METERS_PER_SEC:
        result = decompile_sparticle_meters(words, flag_name, true);
        break;
      case FieldKind::DEGREES:
        result = decompile_sparticle_degrees(words, flag_name, false);
        break;
      case FieldKind::DEGREES_PER_SEC:
        result = decompile_sparticle_degrees(words, flag_name, true);
        break;
      case FieldKind::INT:
        result = decompile_sparticle_int(words, flag_name);
        break;
      case FieldKind::SECONDS:
        result = decompile_sparticle_seconds(words, flag_name);
        break;
      case FieldKind::CPUINFO_FLAGS:
        result = decompile_sparticle_flags(words, ts, field_name, flag_name);
        break;
      case FieldKind::END_FLAG:
        result = decompile_sparticle_end(words, field_name, flag_name);
        break;
      case FieldKind::LAUNCHER_BY_ID:
        result = decompile_sparticle_launcher_by_id(words, flag_name);
        break;
      case FieldKind::NO_FANCY_DECOMP:
        ASSERT(false);
        break;
      case FieldKind::FUNCTION:
        ASSERT(false);
        break;
      case FieldKind::USERDATA:
        result = decompile_sparticle_userdata(words, flag_name, userdata);
        break;
      case FieldKind::ROT_X:
        result = decompile_sparticle_rot_x(words, flag_name);
        break;
      default:
        ASSERT(false);
    }
  }

  // lg::print("Result: {}\n\n", result.print());
  if (flag_name == "float-store") {
    result = fmt::format("(:{} {} :store)", field_name.substr(4), result);
  } else {
    result = fmt::format("(:{} {})", field_name.substr(4), result);
  }
  return pretty_print::to_symbol(result);
}

std::string debug_print(const LinkedWord& word) {
  switch (word.kind()) {
    case LinkedWord::PLAIN_DATA:
      return fmt::format("0x{:08x}", word.data);
    case LinkedWord::TYPE_PTR:
      return fmt::format("type: {}\n", word.symbol_name());
    case LinkedWord::EMPTY_PTR:
      return fmt::format("'()");
    case LinkedWord::HI_PTR:
      return fmt::format("hi ptr");
    case LinkedWord::LO_PTR:
      return fmt::format("lo ptr");
    case LinkedWord::PTR:
      return fmt::format("ptr");
    case LinkedWord::SYM_OFFSET:
      return fmt::format("offset '{}", word.symbol_name());
    case LinkedWord::SYM_PTR:
      return fmt::format("ptr '{}", word.symbol_name());
    case LinkedWord::SYM_VAL_OFFSET:
      return fmt::format("val-ptr '{}", word.symbol_name());
    default:
      ASSERT(false);
  }
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
