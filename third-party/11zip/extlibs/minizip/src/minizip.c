/*
   minizip.c
   Version 1.1, February 14h, 2010
   sample part of the MiniZip project - ( http://www.winimage.com/zLibDll/minizip.html )

         Copyright (C) 1998-2010 Gilles Vollant (minizip) ( http://www.winimage.com/zLibDll/minizip.html )

         Modifications of Unzip for Zip64
         Copyright (C) 2007-2008 Even Rouault

         Modifications for Zip64 support on both zip and unzip
         Copyright (C) 2009-2010 Mathias Svensson ( http://result42.com )
*/


#if (!defined(_WIN32)) && (!defined(WIN32)) && (!defined(__APPLE__))
#ifndef __USE_FILE_OFFSET64
                #define __USE_FILE_OFFSET64
#endif
#ifndef __USE_LARGEFILE64
                #define __USE_LARGEFILE64
#endif
#ifndef _LARGEFILE64_SOURCE
                #define _LARGEFILE64_SOURCE
#endif
#ifndef _FILE_OFFSET_BIT
                #define _FILE_OFFSET_BIT 64
#endif
#endif

#ifdef __APPLE__
// In darwin and perhaps other BSD variants off_t is a 64 bit value, hence no need for specific 64 bit functions
#define FOPEN_FUNC(filename, mode) fopen(filename, mode)
#define FTELLO_FUNC(stream) ftello(stream)
#define FSEEKO_FUNC(stream, offset, origin) fseeko(stream, offset, origin)
#else
#define FOPEN_FUNC(filename, mode) fopen64(filename, mode)
#define FTELLO_FUNC(stream) ftello64(stream)
#define FSEEKO_FUNC(stream, offset, origin) fseeko64(stream, offset, origin)
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>

#ifdef _WIN32
# include <direct.h>
# include <io.h>
#else
# include <unistd.h>
# include <utime.h>
# include <sys/types.h>
# include <sys/stat.h>
#endif

#include "zip.h"

#ifdef _WIN32
#define USEWIN32IOAPI
#include "iowin32.h"
#endif


#define WRITEBUFFERSIZE (16384)
#define MAXFILENAME (256)

#ifdef _WIN32
uLong filetime(f, tmzip, dt)
char* f; /* name of file to get info on */
tm_zip* tmzip; /* return value: access, modific. and creation times */
uLong* dt; /* dostime */
{
    int ret = 0;
    {
        FILETIME ftLocal;
        HANDLE hFind;
        WIN32_FIND_DATAA ff32;

        hFind = FindFirstFileA(f, &ff32);
        if (hFind != INVALID_HANDLE_VALUE)
        {
            FileTimeToLocalFileTime(&(ff32.ftLastWriteTime), &ftLocal);
            FileTimeToDosDateTime(&ftLocal, ((LPWORD)dt) + 1, ((LPWORD)dt) + 0);
            FindClose(hFind);
            ret = 1;
        }
    }
    return ret;
}
#else
#if defined unix || defined __APPLE__
uLong filetime(f, tmzip, dt)
    char *f;               /* name of file to get info on */
    tm_zip *tmzip;         /* return value: access, modific. and creation times */
    uLong *dt;             /* dostime */
{
  int ret=0;
  struct stat s;        /* results of stat() */
  struct tm* filedate;
  time_t tm_t=0;

  if (strcmp(f,"-")!=0)
  {
    char name[MAXFILENAME+1];
    int len = strlen(f);
    if (len > MAXFILENAME)
      len = MAXFILENAME;

    strncpy(name, f,MAXFILENAME-1);
/* strncpy doesnt append the trailing NULL, of the string is too long. */
    name[ MAXFILENAME ] = '\0';

    if (name[len - 1] == '/')
      name[len - 1] = '\0';
/* not all systems allow stat'ing a file with / appended */
    if (stat(name,&s)==0)
    {
      tm_t = s.st_mtime;
      ret = 1;
    }
  }
  filedate = localtime(&tm_t);

  tmzip->tm_sec  = filedate->tm_sec;
  tmzip->tm_min  = filedate->tm_min;
  tmzip->tm_hour = filedate->tm_hour;
  tmzip->tm_mday = filedate->tm_mday;
  tmzip->tm_mon  = filedate->tm_mon ;
  tmzip->tm_year = filedate->tm_year;

  return ret;
}
#else
uLong filetime(f, tmzip, dt)
    char *f;                /* name of file to get info on */
    tm_zip *tmzip;             /* return value: access, modific. and creation times */
    uLong *dt;             /* dostime */
{
    return 0;
}
#endif
#endif


int check_exist_file(filename)
    const

char* filename;
{
    FILE* ftestexist;
    int ret = 1;
    ftestexist = FOPEN_FUNC(filename,"rb");
    if (ftestexist == NULL)
        ret = 0;
    else
        fclose(ftestexist);
    return ret;
}

/* calculate the CRC32 of a file,
   because to encrypt a file, we need known the CRC32 of the file before */
int getFileCrc(const char* filenameinzip, void* buf, unsigned long size_buf, unsigned long* result_crc)
{
    unsigned long calculate_crc = 0;
    int err = ZIP_OK;
    FILE* fin = FOPEN_FUNC(filenameinzip,"rb");

    unsigned long size_read = 0;
    unsigned long total_read = 0;
    if (fin == NULL)
    {
        err = ZIP_ERRNO;
    }

    if (err == ZIP_OK)
        do
        {
            err = ZIP_OK;
            size_read = (int)fread(buf, 1, size_buf, fin);
            if (size_read < size_buf)
                if (feof(fin) == 0)
                {
                    printf("error in reading %s\n", filenameinzip);
                    err = ZIP_ERRNO;
                }

            if (size_read > 0)
                calculate_crc = crc32(calculate_crc, buf, size_read);
            total_read += size_read;
        }
        while ((err == ZIP_OK) && (size_read > 0));

    if (fin)
        fclose(fin);

    *result_crc = calculate_crc;
    printf("file %s crc %lx\n", filenameinzip, calculate_crc);
    return err;
}

int isLargeFile(const char* filename)
{
    int largeFile = 0;
    ZPOS64_T pos = 0;
    FILE* pFile = FOPEN_FUNC(filename, "rb");

    if (pFile != NULL)
    {
        int n = FSEEKO_FUNC(pFile, 0, SEEK_END);
        pos = FTELLO_FUNC(pFile);

        printf("File : %s is %lld bytes\n", filename, pos);

        if (pos >= 0xffffffff)
            largeFile = 1;

        fclose(pFile);
    }

    return largeFile;
}
