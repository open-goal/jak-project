
#include <algorithm>
#include <ctime>

#include <minizip/mz_compat.h>

#include <zipper.hpp>

namespace ziputils
{
    constexpr unsigned int BUFSIZE = 2048;

    // Default constructor
    zipper::zipper() : zipFile_(nullptr), entryOpen_(false)
    {
    }

    // Default destructor
    zipper::~zipper(void)
    {
        close();
    }

    // Create a new zip file.
    // param:
    //        filename    path and the filename of the zip file to open
    //        append        set true to append the zip file
    // return:
    //        true if open, false otherwise
    bool zipper::open(const char* filename, bool append)
    {
        close();
        zipFile_ = zipOpen64(filename, append ? APPEND_STATUS_ADDINZIP : 0);

        return isOpen();
    }

    // Close the zip file
    void zipper::close()
    {
        if (zipFile_)
        {
            closeEntry();
            zipClose(zipFile_, nullptr);
            zipFile_ = nullptr;
        }
    }

    // Check if a zipfile is open.
    // return:
    //        true if open, false otherwise
    bool zipper::isOpen() const
    {
        return zipFile_ != nullptr;
    }

    // Create a zip entry; either file or folder. Folder has to
    // end with a slash or backslash.
    // return:
    //        true if open, false otherwise
    bool zipper::addEntry(const char* filename)
    {
        if (isOpen())
        {
            closeEntry();

            while (filename[0] == '\\' || filename[0] == '/')
            {
                filename++;
            }

            //?? we dont need the stinking time
            zip_fileinfo zi = {0};
            getTime(zi.tmz_date);

            int err = zipOpenNewFileInZip(zipFile_, filename, &zi, nullptr, 0, nullptr, 0, nullptr, Z_DEFLATED, MZ_COMPRESS_LEVEL_DEFAULT);

            entryOpen_ = (err == ZIP_OK);
        }
        return entryOpen_;
    }

    // Close the currently open zip entry.
    void zipper::closeEntry()
    {
        if (entryOpen_)
        {
            zipCloseFileInZip(zipFile_);
            entryOpen_ = false;
        }
    }

    // Check if there is a currently open file zip entry.
    // return:
    //        true if open, false otherwise
    bool zipper::isOpenEntry() const
    {
        return entryOpen_;
    }

    // Stream operator for dumping data from an input stream to the
    // currently open zip entry.
    zipper& zipper::operator<<(std::istream& is)
    {
        int err = ZIP_OK;

        if (isOpenEntry())
        {
            while (err == ZIP_OK && is.good())
            {
                char buf[BUFSIZE];
                is.read(buf, BUFSIZE);

                if (const unsigned long nRead = static_cast<unsigned>(is.gcount()))
                {
                    err = zipWriteInFileInZip(zipFile_, buf, nRead);
                }
                else
                {
                    break;
                }
            }
        }
        return *this;
    }

    // Fill the zip time structure
    // param:
    //        tmZip    time structure to be filled
    void zipper::getTime(tm_zip& tmZip)
    {
        time_t rawtime;
        time(&rawtime);
        const auto timeinfo = localtime(&rawtime);
        tmZip.tm_sec = timeinfo->tm_sec;
        tmZip.tm_min = timeinfo->tm_min;
        tmZip.tm_hour = timeinfo->tm_hour;
        tmZip.tm_mday = timeinfo->tm_mday;
        tmZip.tm_mon = timeinfo->tm_mon;
        tmZip.tm_year = timeinfo->tm_year;
    }
};  // namespace ziputils
