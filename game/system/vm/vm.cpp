/*!
 * @file vm.cpp
 * Base "PS2 virtual machine" code.
 * Simulates the existence of select PS2 components, for inspection & debugging.
 * Not an emulator!
 */

#include "vm.h"
#include "dmac.h"
#include "common/log/log.h"
#include "game/kernel/kscheme.h"
#include <condition_variable>
#include <mutex>

namespace VM {

bool use = true;  // enable VM by default, since we're debugging right now

namespace {
Status status;
std::condition_variable vm_init_cv;
std::condition_variable vm_dead_cv;
std::mutex status_mutex;

int components = 0;
}  // namespace

void wait_vm_init() {
  std::unique_lock<std::mutex> lk(status_mutex);

  vm_init_cv.wait(lk, [&] { return status == Status::Inited; });
}

void wait_vm_dead() {
  std::unique_lock<std::mutex> lk(status_mutex);

  vm_dead_cv.wait(lk, [&] { return status == Status::Dead; });
}

bool vm_want_exit() {
  return status == Status::Kill || status == Status::Dead;
}

void vm_prepare() {
  lg::debug("[VM] Preparing...");
  {
    std::unique_lock<std::mutex> lk(status_mutex);
    status = Status::Uninited;
  }
  lg::debug("[VM] Prepared");
}

void vm_init() {
  if (status != Status::Uninited) {
    lg::warn("[VM] unexpected status {}", status);
  }

  lg::debug("[VM] Inited");
  {
    std::unique_lock<std::mutex> lk(status_mutex);
    status = Status::Inited;
  }
  vm_init_cv.notify_all();
}

void vm_kill() {
  lg::debug("[VM] Killing");

  {
    std::unique_lock<std::mutex> lk(status_mutex);
    status = Status::Kill;
  }

  // stall caller until VM is done dying
  wait_vm_dead();
}

void subscribe_component() {
  ++components;

  // stall component until VM is ready
  if (status == Status::Uninited) {
    wait_vm_init();
  }
}

void unsubscribe_component() {
  --components;

  // the VM is "killed" when there's no more components running
  if (status == Status::Kill && components == 0) {
    {
      std::unique_lock<std::mutex> lk(status_mutex);
      status = Status::Dead;
    }
    vm_dead_cv.notify_all();
  }
}

/*!
 * Return the GOAL pointer to a specified PS2 VM component based on the EE address.
 */
u64 get_vm_ptr(u32 ptr) {
  // currently, only DMAC and DMA channel banks are implemented. add more as necessary.
  if (ptr == 0x10008000) {
    return VM::dmac_ch[0].offset;
  } else if (ptr == 0x10009000) {
    return VM::dmac_ch[1].offset;
  } else if (ptr == 0x1000a000) {
    return VM::dmac_ch[2].offset;
  } else if (ptr == 0x1000b000) {
    return VM::dmac_ch[3].offset;
  } else if (ptr == 0x1000b400) {
    return VM::dmac_ch[4].offset;
  } else if (ptr == 0x1000c000) {
    return VM::dmac_ch[5].offset;
  } else if (ptr == 0x1000c400) {
    return VM::dmac_ch[6].offset;
  } else if (ptr == 0x1000c800) {
    return VM::dmac_ch[7].offset;
  } else if (ptr == 0x1000d000) {
    return VM::dmac_ch[8].offset;
  } else if (ptr == 0x1000d400) {
    return VM::dmac_ch[9].offset;
  } else if (ptr == 0x1000e000) {
    return VM::dmac.offset;
  } else {
    // return zero, using this result will segfault GOAL!
    // we could die immediately, but it might be worth it to keep going just on the off chance more
    // errors are reported, and not just only this one.
    lg::error("unknown EE register for VM at #x{:08x}", ptr);
    return 0;
  }
}

}  // namespace VM
