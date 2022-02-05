#include <array>
#include <fstream>
#include <iostream>

#include <elzip.hpp>
#include <unzipper.hpp>
#include <zipper.hpp>

#include "minizip/mz_os.h"

namespace elz
{
    std::string _resolvePath(const std::string& entry)
    {
        std::array<char, 512> buf = {};
        const int32_t err = mz_path_resolve(entry.c_str(), buf.data(), buf.size());
        if (err != MZ_OK)
        {
            throw zip_exception("error on resolving path of entry : '" + entry + "'");
        }
        return std::string(buf.data());
    }

    void _extractFile(ziputils::unzipper& zipFile, const path& filename, const path& target)
    {
        zipFile.openEntry(filename.string().c_str());
        std::ofstream wFile;
        wFile.open(target.string(), std::ios_base::binary | std::ios_base::out);
        try
        {
            const std::string dumped = zipFile.dump();
            wFile.write(dumped.c_str(), static_cast<std::streamsize>(dumped.size()));
        }
        catch (const ziputils::dump_error& e)
        {
            throw zip_exception("exception occurred when extracting file '" + filename.string() + "' : " + std::string(e.what()));
        }
        wFile.close();
    }

    void extractZip(const path& archive, const path& target)
    {
        ziputils::unzipper zipFile;
        zipFile.open(archive.string().c_str());

        for (const std::string& filename : zipFile.getFilenames())
        {
            std::string real_path = _resolvePath(filename);
            std::filesystem::path currentDir = target / std::filesystem::path(real_path).parent_path();

            std::filesystem::create_directories(currentDir);
            std::filesystem::path currentFile = target / real_path;

            _extractFile(zipFile, filename, currentFile.string());
        }
    }

    void extractFile(const path& archive, const path& file_in_archive, const path& target, std::string out_filename)
    {
        ziputils::unzipper zipFile;
        zipFile.open(archive.string().c_str());
        out_filename = (out_filename.empty() ? file_in_archive.string() : out_filename);
        std::filesystem::create_directories(target);
        const std::string real_path = _resolvePath(file_in_archive.string());
        _extractFile(zipFile, file_in_archive.string(), target / real_path);
    }

    void zipFolder(const path& directory, path archive_path)
    {
        if (!std::filesystem::is_directory(std::filesystem::path(directory)))
        {
            throw std::runtime_error("'" + directory.string() + "' is not a valid directory");
        }
        if (archive_path.empty())
        {
            archive_path = std::filesystem::path(directory).filename().string() + ".zip";
        }
        ziputils::zipper zipper;
        zipper.open(archive_path.string().c_str());
        for (const auto& path : std::filesystem::recursive_directory_iterator(directory))
        {
            const auto& absolute_path = path.path();
            auto relativePath = std::filesystem::relative(absolute_path, directory);
            zipper.addEntry(relativePath.string().c_str());

            std::ifstream fileContent;
            fileContent.open(absolute_path, std::ifstream::in | std::ifstream::binary);

            zipper << fileContent;

            zipper.closeEntry();
        }
        zipper.close();
    }
}  // namespace elz