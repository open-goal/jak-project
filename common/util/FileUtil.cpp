#include "FileUtil.h"
#include <iostream>
#include <stdio.h> /* defines FILENAME_MAX */

#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#endif

std::string FileUtil::GetExecutablePath() {
#ifdef _WIN32
  char buffer[FILENAME_MAX];
  GetModuleFileNameA(NULL, buffer, FILENAME_MAX);
  std::string::size_type pos =
      std::string(buffer).find_last_of("/\\");  // Strip executable from file path
  return std::string(buffer).substr(0, pos);
#else  // do Linux stuff
  char buffer[FILENAME_MAX];
  readlink("/proc/self/exe", buffer,
           FILENAME_MAX);  // /proc/self acts like a "virtual folder" containing information about
                           // the current process
  std::string::size_type pos =
      std::string(buffer).find_last_of("/\\");  // Strip executable from file path
  return std::string(buffer).substr(0, pos);
#endif
}

std::string FileUtil::get_file_path(const std::vector<std::string>& input) {
  std::string currentPath = FileUtil::GetExecutablePath();
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
