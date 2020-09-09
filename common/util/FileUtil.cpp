#include "FileUtil.h"
#include <iostream>
#include <stdio.h> /* defines FILENAME_MAX */

#ifdef _WIN32
#include <direct.h>
#define GetCurrentDir _getcwd
#else
#include <unistd.h>
#define GetCurrentDir getcwd
#endif

std::string FileUtil::GetCurrentWorkingDir() {
  char buff[FILENAME_MAX];
  GetCurrentDir(buff, FILENAME_MAX);
  std::string current_working_dir(buff);
  return current_working_dir;
}

std::string FileUtil::get_file_path(const std::vector<std::string>& input) {
  std::string currentPath = FileUtil::GetCurrentWorkingDir();  // std::filesystem::current_path();
  char dirSeparator;

#ifdef _WIN32
  dirSeparator = '\\';
#else
  dirSeparator = '/';
#endif

  std::string filePath = currentPath;
  for (int i = 0; i < input.size(); i++) {
    filePath = filePath + dirSeparator + input[i];
  }

  return filePath;
}
