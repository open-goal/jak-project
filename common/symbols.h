/*!
 * @file symbols.h
 * The location of fixed symbols in the GOAL symbol table.
 */

#ifndef JAK1_SYMBOLS_H
#define JAK1_SYMBOLS_H

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

#endif  // JAK1_SYMBOLS_H
