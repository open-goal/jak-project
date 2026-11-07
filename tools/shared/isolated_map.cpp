#include "tools/shared/isolated_map.h"

#include <algorithm>

#include "common/util/Assert.h"

#ifndef _WIN32
#include <cerrno>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <deque>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>

#include <sys/resource.h>
#include <sys/wait.h>
#endif

#ifdef _WIN32

std::vector<std::optional<std::string>> isolated_map(size_t count,
                                                     int /*jobs*/,
                                                     size_t /*chunk_size*/,
                                                     const std::function<std::string(size_t)>& task,
                                                     const std::function<void()>& on_done) {
  std::vector<std::optional<std::string>> results(count);
  for (size_t i = 0; i < count; i++) {
    try {
      results[i] = task(i);
    } catch (...) {
    }
    if (on_done) {
      on_done();
    }
  }
  return results;
}

#else

namespace {

struct Chunk {
  size_t begin;
  size_t end;
};

struct Child {
  pid_t pid;
  int fd;
  Chunk chunk;
  size_t next;  // first task in the chunk whose result hasn't been received yet
  std::string buffer;
};

bool write_all(int fd, const void* data, size_t size) {
  auto* ptr = static_cast<const char*>(data);
  while (size > 0) {
    ssize_t n = write(fd, ptr, size);
    if (n < 0) {
      if (errno == EINTR) {
        continue;
      }
      return false;
    }
    ptr += n;
    size -= n;
  }
  return true;
}

void exit_on_signal(int sig) {
  _exit(128 + sig);
}

[[noreturn]] void run_child(int fd, Chunk chunk, const std::function<std::string(size_t)>& task) {
  // the decompiler is chatty and assertion failures are expected here, keep the terminal clean.
  int null_fd = open("/dev/null", O_WRONLY);
  if (null_fd >= 0) {
    dup2(null_fd, STDOUT_FILENO);
    dup2(null_fd, STDERR_FILENO);
  }

  rlimit no_core = {0, 0};
  setrlimit(RLIMIT_CORE, &no_core);
  for (int sig : {SIGABRT, SIGSEGV, SIGBUS, SIGFPE, SIGILL}) {
    signal(sig, exit_on_signal);
  }

  for (size_t i = chunk.begin; i < chunk.end; i++) {
    std::string result;
    try {
      result = task(i);
    } catch (...) {
      _exit(1);
    }
    uint32_t size = result.size();
    if (!write_all(fd, &size, sizeof(size)) || !write_all(fd, result.data(), size)) {
      _exit(1);
    }
  }
  _exit(0);
}

void consume_records(Child& child,
                     std::vector<std::optional<std::string>>& results,
                     const std::function<void()>& on_done) {
  size_t pos = 0;
  while (child.buffer.size() - pos >= sizeof(uint32_t)) {
    uint32_t size;
    memcpy(&size, child.buffer.data() + pos, sizeof(size));
    if (child.buffer.size() - pos - sizeof(size) < size) {
      break;
    }
    ASSERT(child.next < child.chunk.end);
    results[child.next++] = child.buffer.substr(pos + sizeof(size), size);
    pos += sizeof(size) + size;
    if (on_done) {
      on_done();
    }
  }
  child.buffer.erase(0, pos);
}

}  // namespace

std::vector<std::optional<std::string>> isolated_map(size_t count,
                                                     int jobs,
                                                     size_t chunk_size,
                                                     const std::function<std::string(size_t)>& task,
                                                     const std::function<void()>& on_done) {
  std::vector<std::optional<std::string>> results(count);
  jobs = std::max(jobs, 1);
  chunk_size = std::max<size_t>(chunk_size, 1);

  std::deque<Chunk> pending;
  for (size_t begin = 0; begin < count; begin += chunk_size) {
    pending.push_back({begin, std::min(count, begin + chunk_size)});
  }

  std::vector<Child> children;
  while (!pending.empty() || !children.empty()) {
    while ((int)children.size() < jobs && !pending.empty()) {
      auto chunk = pending.front();
      pending.pop_front();

      int fds[2];
      ASSERT_MSG(pipe(fds) == 0, "failed to create pipe");
      fflush(stdout);
      fflush(stderr);
      pid_t pid = fork();
      ASSERT_MSG(pid >= 0, "failed to fork");
      if (pid == 0) {
        close(fds[0]);
        run_child(fds[1], chunk, task);
      }
      close(fds[1]);
      children.push_back({pid, fds[0], chunk, chunk.begin, {}});
    }

    std::vector<pollfd> pfds;
    for (auto& child : children) {
      pfds.push_back({child.fd, POLLIN, 0});
    }
    if (poll(pfds.data(), pfds.size(), -1) < 0) {
      ASSERT_MSG(errno == EINTR, "poll failed");
      continue;
    }

    for (size_t i = children.size(); i-- > 0;) {
      if (!pfds[i].revents) {
        continue;
      }
      auto& child = children[i];
      char buf[64 * 1024];
      ssize_t n = read(child.fd, buf, sizeof(buf));
      if (n > 0) {
        child.buffer.append(buf, n);
        consume_records(child, results, on_done);
        continue;
      }
      if (n < 0 && (errno == EINTR || errno == EAGAIN)) {
        continue;
      }

      close(child.fd);
      waitpid(child.pid, nullptr, 0);
      if (child.next < child.chunk.end) {
        if (on_done) {
          on_done();
        }
        if (child.next + 1 < child.chunk.end) {
          pending.push_front({child.next + 1, child.chunk.end});
        }
      }
      children.erase(children.begin() + i);
    }
  }

  return results;
}

#endif
