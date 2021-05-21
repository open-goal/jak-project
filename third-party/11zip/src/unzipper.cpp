#include <algorithm>
#include <exception>
#include <sstream>

#include <minizip/zlib.h>

#include <unzipper.hpp>

namespace ziputils
{
    // Default constructor
    unzipper::unzipper() :
        zipFile_(nullptr),
        entryOpen_(false)
    {
    }

    // Default destructor
    unzipper::~unzipper(void)
    {
        close();
    }

    // open a zip file.
    // param:
    //         filename    path and the filename of the zip file to open
    //
    // return:
    //         true if open, false otherwise
    bool unzipper::open(const char* filename)
    {
        close();
        zipFile_ = unzOpen64(filename);
        if (zipFile_)
        {
            readEntries();
        }

        return isOpen();
    }

    // Close the zip file
    void unzipper::close()
    {
        if (zipFile_)
        {
            files_.clear();
            folders_.clear();

            closeEntry();
            unzClose(zipFile_);
            zipFile_ = nullptr;
        }
    }

    // Check if a zipfile is open.
    // return:
    //        true if open, false otherwise
    bool unzipper::isOpen()
    {
        return zipFile_ != nullptr;
    }

    // Get the list of file zip entires contained in the zip file.
    const std::vector<std::string>& unzipper::getFilenames()
    {
        return files_;
    }

    // Get the list of folders zip entires contained in the zip file.
    const std::vector<std::string>& unzipper::getFolders()
    {
        return folders_;
    }

    // open an existing zip entry.
    // return:
    //        true if open, false otherwise
    bool unzipper::openEntry(const char* filename)
    {
        if (isOpen())
        {
            closeEntry();
            int err = unzLocateFile(zipFile_, filename, 0);
            if (err == UNZ_OK)
            {
                err = unzOpenCurrentFile(zipFile_);
                entryOpen_ = (err == UNZ_OK);
            }
        }
        return entryOpen_;
    }

    // Close the currently open zip entry.
    void unzipper::closeEntry()
    {
        if (entryOpen_)
        {
            unzCloseCurrentFile(zipFile_);
            entryOpen_ = false;
        }
    }

    // Check if there is a currently open zip entry.
    // return:
    //        true if open, false otherwise
    bool unzipper::isOpenEntry()
    {
        return entryOpen_;
    }

    // Get the zip entry uncompressed size.
    // return:
    //        zip entry uncompressed size
    unsigned int unzipper::getEntrySize()
    {
        if (entryOpen_)
        {
            unz_file_info64 oFileInfo;

            int err = unzGetCurrentFileInfo64(zipFile_, &oFileInfo, nullptr, 0, nullptr, 0, nullptr, 0);

            if (err == UNZ_OK)
            {
                return (unsigned int)oFileInfo.uncompressed_size;
            }
        }
        return 0;
    }

    // Private method used to build a list of files and folders.
    void unzipper::readEntries()
    {
        files_.clear();
        folders_.clear();

        if (isOpen())
        {
            unz_global_info64 oGlobalInfo;
            int err = unzGetGlobalInfo64(zipFile_, &oGlobalInfo);
            for (unsigned long i = 0;
                 i < oGlobalInfo.number_entry && err == UNZ_OK; i++)
            {
                char filename[FILENAME_MAX];
                unz_file_info64 oFileInfo;

                err = unzGetCurrentFileInfo64(zipFile_, &oFileInfo, filename,
                                              sizeof(filename), nullptr, 0, nullptr, 0);
                if (err == UNZ_OK)
                {
                    char nLast = filename[oFileInfo.size_filename - 1];
                    if (nLast == '/' || nLast == '\\')
                    {
                        folders_.push_back(filename);
                    }
                    else
                    {
                        files_.push_back(filename);
                    }

                    err = unzGoToNextFile(zipFile_);
                }
            }
        }
    }

    // Dump the currently open entry to the uotput stream
    unzipper& unzipper::operator>>(std::ostream& os)
    {
        if (isOpenEntry())
        {
            unsigned int size = getEntrySize();
            char* buf = new char[size];
            size = unzReadCurrentFile(zipFile_, buf, size);
            if (size > 0)
            {
                os.write(buf, size);
                os.flush();
            }
            delete [] buf;
        }
        return *this;
    }

    std::string unzipper::dump()
    {
        if (isOpenEntry())
        {
            unsigned int size = getEntrySize();
            char* buf = new char[size];
            size = unzReadCurrentFile(zipFile_, buf, size);
            std::string ret = "";
            if (size > 0)
                ret = std::string(buf, size);
            delete[] buf;
            return ret;
        }
        throw std::runtime_error("Entry is not opened");
    }
};
