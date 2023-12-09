#pragma once

#include "common/versions/versions.h"

/*!
 * @file symbols.h
 * The location of fixed symbols in the GOAL symbol table.
 */

namespace jak1_symbols {
constexpr int FIX_SYM_EMPTY_CAR = -0xc;
constexpr int FIX_SYM_EMPTY_PAIR = -0xa;
constexpr int FIX_SYM_EMPTY_CDR = -0x8;
constexpr int FIX_SYM_FALSE = 0x0;  // GOAL boolean #f (note that this is equal to the $s7 register)
constexpr int FIX_SYM_TRUE = 0x8;   // GOAL boolean #t

// types
constexpr int FIX_SYM_FUNCTION_TYPE = 0x10;  // GOAL type of function
constexpr int FIX_SYM_BASIC_TYPE = 0x18;     // GOAL structure type with type tag
constexpr int FIX_SYM_STRING_TYPE = 0x20;    // GOAL string type (gstring)
constexpr int FIX_SYM_SYMBOL_TYPE = 0x28;    // GOAL symbol type
constexpr int FIX_SYM_TYPE_TYPE = 0x30;      // GOAL type of type
constexpr int FIX_SYM_OBJECT_TYPE = 0x38;    // GOAL parent type of all types
constexpr int FIX_SYM_LINK_BLOCK =
    0x40;  // GOAL type of link-block (used by linker, but seems to be unused by GOAL)
constexpr int FIX_SYM_INTEGER_TYPE = 0x48;   // GOAL integer parent type, assumes unboxed
constexpr int FIX_SYM_SINTEGER_TYPE = 0x50;  // GOAL signed integer parent type, assumes unboxed
constexpr int FIX_SYM_UINTEGER_TYPE = 0x58;  // GOAL unsinged integer parent type, assumes unboxed
constexpr int FIX_SYM_BINTEGER_TYPE = 0x60;  // GOAL "boxed integer" type
constexpr int FIX_SYM_INT8_TYPE = 0x68;      // GOAL 8-bit signed integer
constexpr int FIX_SYM_INT16_TYPE = 0x70;     // ...
constexpr int FIX_SYM_INT32_TYPE = 0x78;     // ...
constexpr int FIX_SYM_INT64_TYPE = 0x80;     // ...
constexpr int FIX_SYM_INT128_TYPE = 0x88;    // GOAL 128-bit integer type, behaves strangely
constexpr int FIX_SYM_UINT8_TYPE = 0x90;     // GOAL 8-bit unsigned integer
constexpr int FIX_SYM_UINT16_TYPE = 0x98;    // ...
constexpr int FIX_SYM_UINT32_TYPE = 0xA0;    // ...
constexpr int FIX_SYM_UINT64_TYPE = 0xA8;    // ...
constexpr int FIX_SYM_UINT128_TYPE = 0xB0;   // ...
constexpr int FIX_SYM_FLOAT_TYPE = 0xB8;     // GOAL 32-bit floating point type
constexpr int FIX_SYM_PROCESS_TREE_TYPE = 0xC0;  // GOAL process-tree type.  Used in the gkernel
constexpr int FIX_SYM_PROCESS_TYPE = 0xC8;       // GOAL process type
constexpr int FIX_SYM_THREAD_TYPE = 0xD0;        // GOAL thread type
constexpr int FIX_SYM_STRUCTURE_TYPE = 0xD8;     // GOAL structure type.  Any type with fields
constexpr int FIX_SYM_PAIR_TYPE = 0xE0;          // GOAL pair type
constexpr int FIX_SYM_POINTER_TYPE = 0xE8;       // GOAL pointer type (32-bit)
constexpr int FIX_SYM_NUMBER_TYPE = 0xF0;        // GOAL number type (parent of integer/float types)
constexpr int FIX_SYM_ARRAY_TYPE = 0xF8;         // GOAL array type
constexpr int FIX_SYM_VU_FUNCTION_TYPE = 0x100;  // GOAL vu-function type
constexpr int FIX_SYM_CONNECTABLE_TYPE = 0x108;  // GOAL connectable
constexpr int FIX_SYM_STACK_FRAME_TYPE = 0x110;  // GOAL stack-frame
constexpr int FIX_SYM_FILE_STREAM_TYPE = 0x118;  // GOAL file-stream
constexpr int FIX_SYM_KHEAP = 0x120;             // GOAL kheap

// GOAL functions
constexpr int FIX_SYM_NOTHING_FUNC = 0x128;    // GOAL nothing-func (does nothing)
constexpr int FIX_SYM_DEL_BASIC_FUNC = 0x130;  // GOAL delete-basic function

// GOAL allocation symbols (?)
constexpr int FIX_SYM_STATIC = 0x138;              // GOAL 'static
constexpr int FIX_SYM_GLOBAL_HEAP = 0x140;         // GOAL 'global
constexpr int FIX_SYM_DEBUG_HEAP = 0x148;          // GOAL 'debug
constexpr int FIX_SYM_LOADING_LEVEL = 0x150;       // ??
constexpr int FIX_SYM_LOADING_PACKAGE = 0x158;     // ??
constexpr int FIX_SYM_PROCESS_LEVEL_HEAP = 0x160;  // ??
constexpr int FIX_SYM_STACK = 0x168;               // GOAL 'stack
constexpr int FIX_SYM_SCRATCH = 0x170;             // GOAL 'scratch

// GOAL random stuff
constexpr int FIX_SYM_SCRATCH_TOP = 0x178;          // GOAL *scratch-top*
constexpr int FIX_SYM_ZERO_FUNC = 0x180;            // GOAL zero-func (returns 0x0 in $v0 register)
constexpr int FIX_SYM_ASIZE_OF_BASIC_FUNC = 0x188;  // GOAL asize-of-basic function
constexpr int FIX_SYM_COPY_BASIC_FUNC = 0x190;      // GOAL copy-basic function
constexpr int FIX_SYM_LEVEL = 0x198;                // ??
constexpr int FIX_SYM_ART_GROUP = 0x1a0;            // ??
constexpr int FIX_SYM_TX_PAGE_DIR = 0x1a8;          // ??
constexpr int FIX_SYM_TX_PAGE = 0x1b0;              // ??
constexpr int FIX_SYM_SOUND = 0x1b8;                // ??
constexpr int FIX_SYM_DGO = 0x1c0;                  // ??
constexpr int FIX_SYM_TOP_LEVEL = 0x1c8;            // ??
constexpr int FIX_FIXED_SYM_END_OFFSET = 0x1d0;
}  // namespace jak1_symbols

namespace jak2_symbols {
constexpr int FIX_SYM_EMPTY_CAR = -0x8;
constexpr int S7_OFF_FIX_SYM_EMPTY_PAIR = -0x6 - 1;
constexpr int FIX_SYM_EMPTY_CDR = -0x4;
constexpr int FIX_SYM_FALSE = 0x0;  // GOAL boolean #f (note that this is equal to the $s7 register)
constexpr int FIX_SYM_TRUE = 0x4;   // GOAL boolean #t
constexpr int FIX_SYM_FUNCTION_TYPE = 0x8;
constexpr int FIX_SYM_BASIC = 0xc;
constexpr int FIX_SYM_STRING_TYPE = 0x10;
constexpr int FIX_SYM_SYMBOL_TYPE = 0x14;
constexpr int FIX_SYM_TYPE_TYPE = 0x18;
constexpr int FIX_SYM_OBJECT_TYPE = 0x1c;
constexpr int FIX_SYM_LINK_BLOCK = 0x20;
constexpr int FIX_SYM_INTEGER = 0x24;
constexpr int FIX_SYM_SINTEGER = 0x28;
constexpr int FIX_SYM_UINTEGER = 0x2c;
constexpr int FIX_SYM_BINTEGER = 0x30;
constexpr int FIX_SYM_INT8 = 0x34;
constexpr int FIX_SYM_INT16 = 0x38;
constexpr int FIX_SYM_INT32 = 0x3c;
constexpr int FIX_SYM_INT64 = 0x40;
constexpr int FIX_SYM_INT128 = 0x44;
constexpr int FIX_SYM_UINT8 = 0x48;
constexpr int FIX_SYM_UINT16 = 0x4c;
constexpr int FIX_SYM_UINT32 = 0x50;
constexpr int FIX_SYM_UINT64 = 0x54;
constexpr int FIX_SYM_UINT128 = 0x58;
constexpr int FIX_SYM_FLOAT = 0x5c;
constexpr int FIX_SYM_PROCESS_TREE = 0x60;
constexpr int FIX_SYM_PROCESS_TYPE = 0x64;
constexpr int FIX_SYM_THREAD = 0x68;
constexpr int FIX_SYM_STRUCTURE = 0x6c;
constexpr int FIX_SYM_PAIR_TYPE = 0x70;
constexpr int FIX_SYM_POINTER = 0x74;
constexpr int FIX_SYM_NUMBER = 0x78;
constexpr int FIX_SYM_ARRAY = 0x7c;
constexpr int FIX_SYM_VU_FUNCTION = 0x80;
constexpr int FIX_SYM_CONNECTABLE = 0x84;
constexpr int FIX_SYM_STACK_FRAME = 0x88;
constexpr int FIX_SYM_FILE_STREAM = 0x8c;

constexpr int FIX_SYM_HEAP = 0x90;
constexpr int FIX_SYM_NOTHING_FUNC = 0x94;
constexpr int FIX_SYM_DELETE_BASIC = 0x98;
constexpr int FIX_SYM_STATIC = 0x9c;
constexpr int FIX_SYM_GLOBAL_HEAP = 0xa0;
constexpr int FIX_SYM_DEBUG = 0xa4;
constexpr int FIX_SYM_LOADING_LEVEL = 0xa8;
constexpr int FIX_SYM_LOADING_PACKAGE = 0xac;
constexpr int FIX_SYM_PROCESS_LEVEL_HEAP = 0xb0;
constexpr int FIX_SYM_STACK = 0xb4;
constexpr int FIX_SYM_SCRATCH = 0xb8;
constexpr int FIX_SYM_SCRATCH_TOP = 0xbc;

constexpr int FIX_SYM_ZERO_FUNC = 0xc0;
constexpr int FIX_SYM_ASIZE_OF_BASIC_FUNC = 0xc4;
constexpr int FIX_SYM_COPY_BASIC_FUNC = 0xc8;  // bugged name
constexpr int FIX_SYM_LEVEL = 0xcc;
constexpr int FIX_SYM_ART_GROUP = 0xd0;
constexpr int FIX_SYM_TEXTURE_PAGE_DIR = 0xd4;
constexpr int FIX_SYM_TEXTURE_PAGE = 0xd8;
constexpr int FIX_SYM_SOUND = 0xdc;
constexpr int FIX_SYM_DGO = 0xe0;
constexpr int FIX_SYM_TOP_LEVEL = 0xe4;
constexpr int FIX_SYM_QUOTE = 0xe8;
constexpr int FIX_FIXED_SYM_END_OFFSET = 0xec;

}  // namespace jak2_symbols

namespace jak3_symbols {

constexpr int FIX_SYM_EMPTY_CAR = -0x8;
constexpr int S7_OFF_FIX_SYM_EMPTY_PAIR = -0x6 - 1;
constexpr int FIX_SYM_EMPTY_CDR = -0x4;
constexpr int FIX_SYM_FALSE = 0x0;  // GOAL boolean #f (note that this is equal to the $s7 register)
constexpr int FIX_SYM_TRUE = 0x4;   // GOAL boolean #t
constexpr int FIX_SYM_FUNCTION_TYPE = 0x8;
constexpr int FIX_SYM_BASIC = 0xc;
constexpr int FIX_SYM_STRING_TYPE = 0x10;
constexpr int FIX_SYM_SYMBOL_TYPE = 0x14;
constexpr int FIX_SYM_TYPE_TYPE = 0x18;
constexpr int FIX_SYM_OBJECT_TYPE = 0x1c;
constexpr int FIX_SYM_LINK_BLOCK = 0x20;
constexpr int FIX_SYM_INTEGER = 0x24;
constexpr int FIX_SYM_SINTEGER = 0x28;
constexpr int FIX_SYM_UINTEGER = 0x2c;
constexpr int FIX_SYM_BINTEGER = 0x30;
constexpr int FIX_SYM_INT8 = 0x34;
constexpr int FIX_SYM_INT16 = 0x38;
constexpr int FIX_SYM_INT32 = 0x3c;
constexpr int FIX_SYM_INT64 = 0x40;
constexpr int FIX_SYM_INT128 = 0x44;
constexpr int FIX_SYM_UINT8 = 0x48;
constexpr int FIX_SYM_UINT16 = 0x4c;
constexpr int FIX_SYM_UINT32 = 0x50;
constexpr int FIX_SYM_UINT64 = 0x54;
constexpr int FIX_SYM_UINT128 = 0x58;
constexpr int FIX_SYM_FLOAT = 0x5c;
constexpr int FIX_SYM_PROCESS_TREE = 0x60;
constexpr int FIX_SYM_PROCESS_TYPE = 0x64;
constexpr int FIX_SYM_THREAD = 0x68;
constexpr int FIX_SYM_STRUCTURE = 0x6c;
constexpr int FIX_SYM_PAIR_TYPE = 0x70;
constexpr int FIX_SYM_POINTER = 0x74;
constexpr int FIX_SYM_NUMBER = 0x78;
constexpr int FIX_SYM_ARRAY = 0x7c;
constexpr int FIX_SYM_VU_FUNCTION = 0x80;
constexpr int FIX_SYM_CONNECTABLE = 0x84;
constexpr int FIX_SYM_STACK_FRAME = 0x88;
constexpr int FIX_SYM_FILE_STREAM = 0x8c;
constexpr int FIX_SYM_HEAP = 0x90;
constexpr int FIX_SYM_NOTHING_FUNC = 0x94;
constexpr int FIX_SYM_DELETE_BASIC = 0x98;
constexpr int FIX_SYM_STATIC = 0x9c;
constexpr int FIX_SYM_GLOBAL_HEAP = 0xa0;
constexpr int FIX_SYM_DEBUG = 0xa4;
constexpr int FIX_SYM_LOADING_LEVEL = 0xa8;
constexpr int FIX_SYM_LOADING_PACKAGE = 0xac;
constexpr int FIX_SYM_PROCESS_LEVEL_HEAP = 0xb0;
constexpr int FIX_SYM_STACK = 0xb4;
constexpr int FIX_SYM_SCRATCH = 0xb8;
constexpr int FIX_SYM_SCRATCH_TOP = 0xbc;
constexpr int FIX_SYM_ZERO_FUNC = 0xc0;
constexpr int FIX_SYM_ASIZE_OF_BASIC_FUNC = 0xc4;
constexpr int FIX_SYM_COPY_BASIC_FUNC = 0xc8;  // bugged name
constexpr int FIX_SYM_LEVEL = 0xcc;
constexpr int FIX_SYM_ART_GROUP = 0xd0;
constexpr int FIX_SYM_TEXTURE_PAGE_DIR = 0xd4;
constexpr int FIX_SYM_TEXTURE_PAGE = 0xd8;
constexpr int FIX_SYM_SOUND = 0xdc;
constexpr int FIX_SYM_DGO = 0xe0;
constexpr int FIX_SYM_TOP_LEVEL = 0xe4;
constexpr int FIX_SYM_QUOTE = 0xe8;
constexpr int FIX_SYM_LISTENER_LINK_BLOCK = 0xec;
constexpr int FIX_SYM_LISTENER_FUNCTION = 0xf0;
constexpr int FIX_SYM_STACK_TOP = 0xf4;
constexpr int FIX_SYM_STACK_BASE = 0xf8;
constexpr int FIX_SYM_STACK_SIZE = 0xfc;
constexpr int FIX_SYM_KERNEL_FUNCTION = 0x100;
constexpr int FIX_SYM_KERNEL_PACKAGES = 0x104;
constexpr int FIX_SYM_KERNEL_BOOT_MESSAGE = 0x108;
constexpr int FIX_SYM_KERNEL_BOOT_MODE = 0x10c;
constexpr int FIX_SYM_KERNEL_BOOT_LEVEL = 0x110;
constexpr int FIX_SYM_KERNEL_BOOT_ART_GROUP = 0x114;
constexpr int FIX_SYM_KERNEL_DEBUG = 0x118;
constexpr int FIX_SYM_KERNEL_VERSION = 0x11c;
constexpr int FIX_SYM_KERNEL_DISPATCHER = 0x120;
constexpr int FIX_SYM_SYNC_DISPATCHER = 0x124;
constexpr int FIX_SYM_PRINT_COLLUMN = 0x128;
constexpr int FIX_SYM_DEBUG_SEGMENT = 300;
constexpr int FIX_SYM_ENABLE_METHOD_SET = 0x130;
constexpr int FIX_SYM_SQL_RESULT = 0x134;
constexpr int FIX_SYM_COLLAPSE_QUOTE = 0x138;
constexpr int FIX_SYM_LEVEL_TYPE_LIST = 0x13C;
constexpr int FIX_SYM_DECI_COUNT = 0x140;
constexpr int FIX_SYM_USER = 0x144;
constexpr int FIX_SYM_VIDEO_MODE = 0x148;
constexpr int FIX_SYM_BOOT_VIDEO_MODE = 0x14C;
constexpr int FIX_SYM_BOOT = 0x150;
constexpr int FIX_SYM_DEMO = 0x154;
constexpr int FIX_SYM_DEMO_SHARED = 0x158;
constexpr int FIX_SYM_PREVIEW = 0x15C;
constexpr int FIX_SYM_KIOSK = 0x160;
constexpr int FIX_SYM_PLAY_BOOT = 0x164;
constexpr int FIX_SYM_SIN = 0x168;
constexpr int FIX_SYM_COS = 0x16C;
constexpr int FIX_SYM_PUT_DISPLAY_ENV = 0x170;
constexpr int FIX_SYM_SYNCV = 0x174;
constexpr int FIX_SYM_SYNC_PATH = 0x178;
constexpr int FIX_SYM_RESET_PATH = 0x17C;
constexpr int FIX_SYM_RESET_GRAPH = 0x180;
constexpr int FIX_SYM_DMA_SYNC = 0x184;
constexpr int FIX_SYM_GS_PUT_IMR = 0x188;
constexpr int FIX_SYM_GS_GET_IMR = 0x18C;
constexpr int FIX_SYM_GS_STORE_IMAGE = 400;
constexpr int FIX_SYM_FLUSH_CACHE = 0x194;
constexpr int FIX_SYM_CPAD_OPEN = 0x198;
constexpr int FIX_SYM_CPAD_GET_DATA = 0x19C;
constexpr int FIX_SYM_MOUSE_GET_DATA = 0x1A0;
constexpr int FIX_SYM_KEYBD_GET_DATA = 0x1A4;
constexpr int FIX_SYM_INSTALL_HANDLER = 0x1A8;
constexpr int FIX_SYM_INSTALL_DEBUG_HANDLER = 0x1AC;
constexpr int FIX_SYM_FILE_STREAM_OPEN = 0x1B0;
constexpr int FIX_SYM_FILE_STREAM_CLOSE = 0x1B4;
constexpr int FIX_SYM_FILE_STREAM_LENGTH = 0x1B8;
constexpr int FIX_SYM_FILE_STREAM_SEEK = 0x1BC;
constexpr int FIX_SYM_FILE_STREAM_READ = 0x1C0;
constexpr int FIX_SYM_FILE_STREAM_WRITE = 0x1C4;
constexpr int FIX_SYM_SCF_GET_LANGUAGE = 0x1C8;
constexpr int FIX_SYM_SCF_GET_TIME = 0x1CC;
constexpr int FIX_SYM_SCF_GET_ASPECT = 0x1D0;
constexpr int FIX_SYM_SCF_GET_VOLUME = 0x1D4;
constexpr int FIX_SYM_SCF_GET_TERRITORY = 0x1D8;
constexpr int FIX_SYM_SCF_GET_TIMEOUT = 0x1DC;
constexpr int FIX_SYM_SCF_GET_INACTIVE_TIMEOUT = 0x1E0;
constexpr int FIX_SYM_DMA_TO_IOP = 0x1E4;
constexpr int FIX_SYM_KERNEL_SHUTDOWN = 0x1E8;
constexpr int FIX_SYM_AYBABTU = 0x1EC;
constexpr int FIX_SYM_STRING_TO_SYMBOL = 0x1F0;
constexpr int FIX_SYM_SYMBOL_TO_STRING = 500;
constexpr int FIX_SYM_PRINT = 0x1F8;
constexpr int FIX_SYM_INSPECT = 0x1FC;
constexpr int FIX_SYM_LOAD = 0x200;
constexpr int FIX_SYM_LOADB = 0x204;
constexpr int FIX_SYM_LOADO = 0x208;
constexpr int FIX_SYM_UNLOAD = 0x20C;
constexpr int FIX_SYM_FORMAT = 0x210;
constexpr int FIX_SYM_MALLOC = 0x214;
constexpr int FIX_SYM_KMALLOC = 0x218;
constexpr int FIX_SYM_KMEMOPEN = 0x21C;
constexpr int FIX_SYM_KMEMCLOSE = 0x220;
constexpr int FIX_SYM_NEW_DYNAMIC_STRUCTURE = 0x224;
constexpr int FIX_SYM_METHOD_SET = 0x228;
constexpr int FIX_SYM_LINK = 0x22C;
constexpr int FIX_SYM_LINK_BUSY = 0x230;
constexpr int FIX_SYM_LINK_RESET = 0x234;
constexpr int FIX_SYM_LINK_BEGIN = 0x238;
constexpr int FIX_SYM_LINK_RESUME = 0x23C;
constexpr int FIX_SYM_DGO_LOAD = 0x240;
constexpr int FIX_SYM_SQL_QUERY = 0x244;
constexpr int FIX_SYM_MC_RUN = 0x248;
constexpr int FIX_SYM_MC_FORMAT = 0x24C;
constexpr int FIX_SYM_MC_UNFORMAT = 0x250;
constexpr int FIX_SYM_MC_CREATE_FILE = 0x254;
constexpr int FIX_SYM_MC_SAVE = 600;
constexpr int FIX_SYM_MC_LOAD = 0x25C;
constexpr int FIX_SYM_MC_CHECK_RESULT = 0x260;
constexpr int FIX_SYM_MC_GET_SLOT_INFO = 0x264;
constexpr int FIX_SYM_MC_MAKEFILE = 0x268;
constexpr int FIX_SYM_KSET_LANGUAGE = 0x26C;
constexpr int FIX_SYM_RPC_CALL = 0x270;
constexpr int FIX_SYM_RPC_BUSY = 0x274;
constexpr int FIX_SYM_TEST_LOAD_DGO_C = 0x278;
constexpr int FIX_SYM_SYMLINK2 = 0x27c;
constexpr int FIX_SYM_SYMLINK3 = 0x280;
constexpr int FIX_SYM_ULTIMATE_MEMCPY = 0x284;
constexpr int FIX_SYM_PLAY = 0x288;
constexpr int FIX_SYM_SYMBOL_STRING = 0x28c;
constexpr int FIX_SYM_KERNEL_SYMBOL_WARNINGS = 0x290;
constexpr int FIX_SYM_NETWORK_BOOTSTRAP = 0x294;

}  // namespace jak3_symbols

constexpr int false_symbol_offset() {
  return jak1_symbols::FIX_SYM_FALSE;
}

constexpr int true_symbol_offset(GameVersion version) {
  switch (version) {
    case GameVersion::Jak1:
      return jak1_symbols::FIX_SYM_TRUE;
    case GameVersion::Jak2:
    case GameVersion::Jak3:
      return jak2_symbols::FIX_SYM_TRUE;
  }
}

constexpr int empty_pair_offset_from_s7(GameVersion version) {
  switch (version) {
    case GameVersion::Jak1:
      return jak1_symbols::FIX_SYM_EMPTY_PAIR;
    case GameVersion::Jak2:
    case GameVersion::Jak3:
      // minus 1 for the symbol table pointer's offset.
      return jak2_symbols::S7_OFF_FIX_SYM_EMPTY_PAIR;
  }
}
