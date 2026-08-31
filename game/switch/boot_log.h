#pragma once

/*!
 * @file boot_log.h
 * Shared SD-card trace sink for the Switch port.
 *
 * Each translation unit used to open/write/close sdmc:/gk_boot_log.txt per message. newlib's
 * fsdev layer isn't thread-safe, and the EE/IOP/DECI/main threads all trace concurrently, so
 * that raced its internal state; it also cost three emulated fsp-srv round-trips per line.
 * One fd, opened once, guarded by a mutex.
 *
 * Tracing is boot-only. Several of these call sites live in the renderer and would otherwise
 * run every frame, doing guest file I/O from the render thread -- which is where the printf
 * path was faulting once the game loop started. switch_boot_log_finish() latches it off.
 */

#include <atomic>
#include <fcntl.h>
#include <mutex>
#include <string.h>
#include <unistd.h>

inline std::atomic<bool>& switch_boot_log_active() {
  static std::atomic<bool> active{true};
  return active;
}

inline void switch_boot_log(const char* msg) {
  if (!switch_boot_log_active().load(std::memory_order_relaxed)) {
    return;
  }
  static std::mutex mtx;
  std::lock_guard<std::mutex> lock(mtx);
  static int fd = open("sdmc:/gk_boot_log.txt", O_WRONLY | O_CREAT | O_APPEND, 0644);
  if (fd >= 0) {
    write(fd, msg, strlen(msg));
    fsync(fd);
  }
}

inline void switch_boot_log_finish() {
  switch_boot_log("[boot_log] boot complete, tracing disabled\n");
  switch_boot_log_active().store(false, std::memory_order_relaxed);
}
