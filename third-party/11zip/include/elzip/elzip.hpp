#pragma once

#include <string>

namespace elz
{
    void extractZip(std::string zipname, std::string target = ".");
    void extractFile(std::string zipname, std::string filename, std::string target = ".");
}
