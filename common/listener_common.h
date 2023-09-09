#pragma once

/*!
 * @file listener_common.h
 * Common types shared between the compiler and the runtime for the listener connection.
 */

#include "common/common_types.h"

/*!
 * Header of a DECI2 protocol message
 * NOTE: we've changed this to use 32-bit integers for len/rsvd
 */
struct Deci2Header {
  u32 len;    //! size of data following header
  u32 rsvd;   //! zero, used internally by runtime.
  u16 proto;  //! protocol identification number
  u8 src;     //! identification code of sender
  u8 dst;     //! identification code of recipient
};

/*!
 * Type of message sent to compiler
 */
enum class ListenerMessageKind : u16 {
  MSG_ACK = 0,     //! Acknowledge a compiler message
  MSG_OUTPUT = 1,  //! Send output buffer data
  MSG_PRINT = 2,   //! Send print buffer data
  MSG_INVALID = 24
};

/*!
 * Type of message sent from compiler
 */
enum ListenerToTargetMsgKind : u16 {
  LTT_MSG_POKE = 1,           //! "Poke" the game and have it flush buffers
  LTT_MSG_INSPECT = 5,        //! Inspect an object
  LTT_MSG_PRINT = 6,          //! Print an object
  LTT_MSG_PRINT_SYMBOLS = 7,  //! Print all symbols
  LTT_MSG_RESET = 8,          //! Reset the game
  LTT_MSG_CODE = 9,           //! Send code to patch into the game
  // below here are added
  LTT_MSG_SHUTDOWN = 10  //! Shut down the runtime.
};

/*!
 * The full header of a listener message, including the Deci2Header
 */
struct ListenerMessageHeader {
  Deci2Header deci2_header;  //! The header used for DECI2 communication
  union {
    ListenerMessageKind msg_kind;  //! GOAL Listener message kind
    ListenerToTargetMsgKind ltt_msg_kind;
  };

  u16 u6;        //! Unknown
  u32 msg_size;  //! Size of data after this header
  u64 msg_id;    //! Message ID number, target echoes this back.
};

constexpr int DECI2_PORT = 8112;  // TODO - is this a good choice?

constexpr u16 DECI2_PROTOCOL = 0xe042;
