#pragma once

#include <iostream>
#include <string>
#include <vector>

#include <minizip/mz_compat.h>

namespace ziputils
{
    class dump_error : public std::runtime_error
    {
    public:
        dump_error();
        ~dump_error() override = default;
    };

    class unzipper
    {
    public:
        unzipper();
        ~unzipper(void);

        bool open( const char* filename );
        void close();
        bool isOpen();

        bool openEntry( const char* filename );
        void closeEntry();
        bool isOpenEntry() const;
        unsigned int getEntrySize() const;

        const std::vector<std::string>& getFilenames();
        const std::vector<std::string>& getFolders();

        unzipper& operator>>( std::ostream& os );
        std::string dump() const;

    private:
        void readEntries();

    private:
        unzFile            zipFile_;
        bool            entryOpen_;

        std::vector<std::string> files_;
        std::vector<std::string> folders_;
    };
};
