#include "sparticle_decompile.h"

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




enum class FieldKind { FLOAT, TEXTURE_ID, INVALID };

struct SparticleFieldDecomp {
  bool known = false;  // error if we try to decomp one that isn't known
  FieldKind kind = FieldKind::INVALID;
};

const SparticleFieldDecomp field_kinds[68] = {
    {false},                        // MISC_FIELDS_START = 0
    {true, FieldKind::TEXTURE_ID},  // SPT_TEXTURE = 1
    {false},                        // SPT_ANIM = 2
    {false},                        // SPT_ANIM_SPEED = 3
    {false},                        // SPT_BIRTH_FUNC = 4
    {false},                        // SPT_JOINT/REFPOINT = 5
    {false},                        // SPT_NUM = 6
    {false},                        // SPT_SOUND = 7
    {false},                        // MISC_FIELDS_END = 8
    {false},                        // SPRITE_FIELDS_START = 9
    {false},                        // SPT_X = 10
    {false},                        // SPT_Y = 11
    {false},                        // SPT_Z = 12
    {false},                        // SPT_SCALE_X = 13
    {false},                        // SPT_ROT_X = 14
    {false},                        // SPT_ROT_Y = 15
    {false},                        // SPT_ROT_Z = 16
    {false},                        // SPT_SCALE_Y = 17
    {false},                        // SPT_R = 18
    {false},                        // SPT_G = 19
    {false},                        // SPT_B = 20
    {false},                        // SPT_A = 21
    {false},                        // SPRITE_FIELDS_END = 22
    {false},                        // CPU_FIELDS_START = 23
    {false},                        // SPT_OMEGA = 24
    {false},                        // SPT_VEL_X = 25
    {false},                        // SPT_VEL_Y = 26
    {false},                        // SPT_VEL_Z = 27
    {false},                        // SPT_SCALEVEL_X = 28
    {false},                        // SPT_ROTVEL_X = 29
    {false},                        // SPT_ROTVEL_Y = 30
    {false},                        // SPT_ROTVEL_Z = 31
    {false},                        // SPT_SCALEVEL_Y = 32
    {false},                        // SPT_FADE_R = 33
    {false},                        // SPT_FADE_G = 34
    {false},                        // SPT_FADE_B = 35
    {false},                        // SPT_FADE_A = 36
    {false},                        // SPT_ACCEL_X = 37
    {false},                        // SPT_ACCEL_Y = 38
    {false},                        // SPT_ACCEL_Z = 39
    {false},                        // SPT_DUMMY = 40
    {false},                        // SPT_QUAT_X = 41
    {false},                        // SPT_QUAT_Y = 42
    {false},                        // SPT_QUAT_Z = 43
    {false},                        // SPT_QUAD_W = 44
    {false},                        // SPT_FRICTION = 45
    {false},                        // SPT_TIMER = 46
    {false},                        // SPT_FLAGS = 47
    {false},                        // SPT_USERDATA = 48
    {false},                        // SPT_FUNC = 49
    {false},                        // SPT_NEXT_TIME = 50
    {false},                        // SPT_NEXT_LAUNCHER = 51
    {false},                        // CPU_FIELDS_END = 52
    {false},                        // LAUNCH_FIELDS_START = 53
    {false},                        // SPT_LAUNCHROT_X = 54
    {false},                        // SPT_LAUNCHROT_Y = 55
    {false},                        // SPT_LAUNCHROT_Z = 56
    {false},                        // SPT_LAUNCHROT_W = 57
    {false},                        // SPT_CONEROT_X = 58
    {false},                        // SPT_CONEROT_Y = 59
    {false},                        // SPT_CONEROT_Z = 60
    {false},                        // SPT_CONEROT_W = 61
    {false},                        // SPT_CONEROT_RADIUS = 62
    {false},                        // SPT_ROTATE_Y = 63
    {false},                        // LAUNCH_FIELDS_END = 64
    {false},                        // SPT_SCALE = 65
    {false},                        // SPT_SCALEVEL = 66
    {false},                        // SPT_END = 67

};
