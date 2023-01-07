#include "term_util.h"

#if defined _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <condition_variable>
#include <mutex>
#elif defined(__LINUX__) || defined(__gnu_linux__) || defined(__linux__)
#include <stdio.h>
#include <unistd.h>

#include <sys/ioctl.h>
#elif defined(__APPLE__)
#endif

namespace term_util {
void clear() {
#if defined _WIN32
  system("cls");
#elif defined(__LINUX__) || defined(__gnu_linux__) || defined(__linux__)
  system("clear");
#elif defined(__APPLE__)
  system("clear)";
#endif
}

int row_count() {
#if defined _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
  return csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
#elif defined(__LINUX__) || defined(__gnu_linux__) || defined(__linux__)
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  return w.ws_row;
#elif defined(__APPLE__)
  ASSERT(false);
  return 0;
#endif
}

int col_count() {
#if defined _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
  return csbi.srWindow.Right - csbi.srWindow.Left + 1;
#elif defined(__LINUX__) || defined(__gnu_linux__) || defined(__linux__)
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  return w.ws_col;
#elif defined(__APPLE__)
  ASSERT(false);
  return 0;
#endif
}
}  // namespace term_util
