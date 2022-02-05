#pragma once

#include <fswrapper.hpp>
#include <string>

namespace elz
{
    class zip_exception : public std::runtime_error
    {
    public:
        zip_exception(const std::string& error);
        ~zip_exception() override = default;
    };

    inline zip_exception::zip_exception(const std::string& error) : std::runtime_error(error)
    {
    }

    using path = std::filesystem::path;

    void extractZip(const path& archive, const path& target = ".");
    void extractFile(const path& archive, const path& fileInArchive, const path& target = ".", std::string outFilename = "");
    void zipFolder(const path& folder, path archivePath = "");
}
