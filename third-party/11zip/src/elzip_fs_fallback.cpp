#if defined _WIN32
#include <fileapi.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include <iostream>
#include <fstream>
#include <string>


#include <tinydir/tinydir.h>

#include <elzip.hpp>
#include <unzipper.hpp>

template <typename V>
inline bool isInList(V term, const std::vector<V>& list1)
{
    for (size_t k = 0; k < list1.size(); k++)
    {
        if (term == list1[k])
            return true;
    }
    return false;
}

bool createDir(const std::string& dir)
{
#ifdef _WIN32
    return bool(CreateDirectory(dir.c_str(), LPSECURITY_ATTRIBUTES(NULL)));
#else
    return bool(mkdir(dir.c_str(), S_IRUSR | S_IWUSR | S_IXUSR));
#endif
}

std::vector<std::string> split(const std::string &str, const std::string &delimiters) {
    std::vector<std::string> tokens;
    std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    std::string::size_type pos = str.find_first_of(delimiters, lastPos);
    while (std::string::npos != pos || std::string::npos != lastPos) {
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        lastPos = str.find_first_not_of(delimiters, pos);
        pos = str.find_first_of(delimiters, lastPos);
    }
    return tokens;
}

std::vector<std::string> listDirInDir(std::string path) {
    tinydir_dir dir;
    tinydir_open(&dir, path.c_str());

    std::vector<std::string> fileList;
    while (dir.has_next)
    {
        tinydir_file file;
        tinydir_readfile(&dir, &file);
        if (file.is_dir)
        {
            if (std::string(file.name) != "." && std::string(file.name) != "..") { fileList.push_back(std::string(file.name)); }
        }
        tinydir_next(&dir);
    }
    tinydir_close(&dir);
    return fileList;
}

std::string join(std::vector<std::string>& vector, std::string sep, int start, int end)
{
    std::string result = "";
    if (end >= vector.size())
        end = vector.size();
    if (start >= vector.size() - 1)
        start = vector.size() - 1;
    for (int i = start; i < vector.size() - end; i++)
    {
        if (i != vector.size() - 1)
            result += vector[i] + sep;
        else
            result += vector[i];
    }
    return result;
}


namespace elz
{
    void extractZip(std::string zipname, std::string target)
    {
        ziputils::unzipper zipFile;
        zipFile.open(zipname.c_str());
        for (std::string filename : zipFile.getFilenames())
        {
            std::vector<std::string> splittedPath = split(filename, "/");
            std::string parentPath = join(splittedPath, "/", 0, 1);
            std::string cDir(target + ((parentPath == "") ? "" : "/") + parentPath);
            std::string cFile(target + "/" + filename);
            std::string fillPath;
            std::string buffTest = ".";
            for (std::string pathPart : split(cDir, "/")) {
                
                fillPath += ((fillPath != "") ? "/" : "") + pathPart;
                if (!isInList(fillPath, listDirInDir(buffTest))) {
                    createDir(fillPath.c_str());
                }
                buffTest = fillPath;
            }
            std::cout << "fb] Opening file : " << filename << std::endl;
            zipFile.openEntry(filename.c_str());
            std::ofstream wFile;
            wFile.open(cFile, std::ios_base::binary | std::ios_base::out);
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