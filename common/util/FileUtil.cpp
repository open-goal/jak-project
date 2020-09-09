#include "FileUtil.h"
#include <iostream>
#include <filesystem>

std::string FileUtil::get_file_path(std::string input[]) {
    int arrSize = std::sizeof(input);
    std::string currentPath = std::filesystem::current_path();
    char dirSeparator;

    #ifdef _WIN32
        dirSeparator = '\';
    #else
        dirSeparator = '/';
    #endif

    std::string filePath = currentPath;
    for (int i = 0; i < arrSize; i++) {
        if (arrSize = i+1) {
            filePath = filePath << input[i];
        } else {
            filePath = filePath << input[i] << dirSeparator;
        }
    }

    return filePath;
}