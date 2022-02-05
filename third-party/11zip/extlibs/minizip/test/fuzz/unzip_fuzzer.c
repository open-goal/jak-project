/* unzip_fuzzer.c - Unzip fuzzer for libFuzzer
   part of the minizip-ng project

   Copyright (C) 2018 The Chromium Authors
   Copyright (C) 2018 Anand K. Mistry
   Copyright (C) 2018-2020 Nathan Moinvaziri
     https://github.com/zlib-ng/minizip-ng

   This program is distributed under the terms of the same license as zlib.
   See the accompanying LICENSE file for the full text of the license.
*/


#include "mz.h"
#include "mz_strm.h"
#include "mz_strm_mem.h"
#include "mz_zip.h"

#ifdef __cplusplus
extern "C" {
#endif

/***************************************************************************/

#define MZ_FUZZ_TEST_PWD        "test123"
#define MZ_FUZZ_TEST_FILENAME   "foo"
#define MZ_FUZZ_TEST_FILENAMEUC "FOO"

/***************************************************************************/

int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    mz_zip_file* file_info = NULL;
    void *stream = NULL;
    void *handle = NULL;
    const char* archive_comment = NULL;
    char buffer[1024];
    uint16_t version_madeby = 0;
    uint64_t num_entries = 0;
    int64_t entry_pos = 0;
    int32_t err = MZ_OK;
    uint8_t encrypted = 0;


    mz_stream_mem_create(&stream);
    mz_stream_mem_set_buffer(stream, (void *)data, (int32_t)size);

    mz_zip_create(&handle);

    err = mz_zip_open(handle, stream, MZ_OPEN_MODE_READ);

    if (err == MZ_OK)
    {
        /* Some archive properties that are non-fatal for reading the archive. */
        mz_zip_get_comment(handle, &archive_comment);
        mz_zip_get_version_madeby(handle, &version_madeby);
        mz_zip_get_number_entry(handle, &num_entries);

        err = mz_zip_goto_first_entry(handle);
        while (err == MZ_OK)
        {
            err = mz_zip_entry_get_info(handle, &file_info);
            if (err != MZ_OK)
                break;

            encrypted = (file_info->flag & MZ_ZIP_FLAG_ENCRYPTED);

            err = mz_zip_entry_read_open(handle, 0,
                        encrypted ? MZ_FUZZ_TEST_PWD : NULL);
            if (err != MZ_OK)
                break;

            err = mz_zip_entry_is_open(handle);
            if (err != MZ_OK)
                break;

            /* Return value isn't checked here because we can't predict
               what the value will be. */

            mz_zip_entry_is_dir(handle);
            entry_pos = mz_zip_get_entry(handle);
            if (entry_pos < 0)
                break;

            err = mz_zip_entry_read(handle, buffer, sizeof(buffer));
            if (err < 0)
                break;

            err = mz_zip_entry_close(handle);
            if (err != MZ_OK)
                break;

            err = mz_zip_goto_next_entry(handle);
        }

        mz_zip_entry_close(handle);

        /* Return value isn't checked here because we can't predict what the value
           will be. */

        mz_zip_locate_entry(handle, MZ_FUZZ_TEST_FILENAME, 0);
        mz_zip_locate_entry(handle, MZ_FUZZ_TEST_FILENAMEUC, 0);
        mz_zip_locate_entry(handle, MZ_FUZZ_TEST_FILENAME, 1);
        mz_zip_locate_entry(handle, MZ_FUZZ_TEST_FILENAMEUC, 1);

        mz_zip_close(handle);
    }

    mz_zip_delete(&handle);
    mz_stream_mem_delete(&stream);

    return 0;
}

/***************************************************************************/

#ifdef __cplusplus
}
#endif
