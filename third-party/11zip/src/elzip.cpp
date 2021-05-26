#include <fstream>
#include <iostream>

#include <elzip.hpp>
#include <fswrapper.hpp>
#include <unzipper.hpp>

namespace elz
{
    void extractZip(std::string zipname, std::string target)
    {
        ziputils::unzipper zipFile;
        zipFile.open(zipname.c_str());
        for (std::string filename : zipFile.getFilenames())
        {
            std::filesystem::path cDir(target + ((std::filesystem::path(filename).parent_path().string() == "") ? "" : "/") + std::filesystem::path(filename).parent_path().string());
            std::filesystem::path cFile(target + "/" + filename);
            std::filesystem::path fillPath;
            for (std::filesystem::path pathPart : cDir)
            {
                fillPath /= pathPart;
                if (!exists(fillPath))
                {
                    create_directory(fillPath);
                }
            }
            std::cout << "Opening file : " << filename << std::endl;
            zipFile.openEntry(filename.c_str());
            std::ofstream wFile;
            wFile.open(cFile.string(), std::ios_base::binary | std::ios_base::out);
            std::string dumped = zipFile.dump();
            wFile.write(dumped.c_str(), dumped.size());
            wFile.close();
        }
    }

    void extractFile(std::string zipname, std::string filename, std::string target)
    {
        ziputils::unzipper zipFile;
        zipFile.open(zipname.c_str());
        zipFile.openEntry(filename.c_str());
        std::ofstream wFile;
        wFile.open(target, std::ios_base::binary | std::ios_base::out);
        std::string dumped = zipFile.dump();
        wFile.write(dumped.c_str(), dumped.size());
        wFile.close();
    }
}
