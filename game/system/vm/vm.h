#pragma once

/*!
 * @file vm.h
 * Base "PS2 virtual machine" code.
 * Simulates the existence of select PS2 components, for inspection & debugging.
 * Not an emulator!
 */

#include "common/common_types.h"

namespace VM {

extern bool use;

enum class Status { Disabled, Uninited, Inited, Kill, Dead };

void wait_vm_init();
void wait_vm_dead();

bool vm_want_exit();

void vm_prepare();
void vm_init();
void vm_kill();

void subscribe_component();
void unsubscribe_component();

u64 get_vm_ptr(u32 ptr);

}  // namespace VM
