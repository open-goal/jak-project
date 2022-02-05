/* zip_fuzzer.c - Zip fuzzer for libFuzzer
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

#define MZ_FUZZ_TEST_FILENAME   "foo"

/***************************************************************************/

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    mz_zip_file file_info;
    void *fuzz_stream = NULL;
    void *stream = NULL;
    void *handle = NULL;
    int32_t err = MZ_OK;
    uint16_t value16 = 0;
    uint8_t value8 = 0;
    int16_t compress_level = 0;
    int64_t fuzz_pos = 0;
    int32_t fuzz_length = 0;
    uint8_t *fuzz_buf = NULL;

    mz_stream_mem_create(&fuzz_stream);
    mz_stream_mem_set_buffer(fuzz_stream, (void *)data, (int32_t)size);

    memset(&file_info, 0, sizeof(file_info));

    file_info.flag = MZ_ZIP_FLAG_UTF8;
    if ((mz_stream_read_uint8(fuzz_stream, &value8) == MZ_OK) && (value8 < 0x08))
    {
        if (mz_stream_read_uint16(fuzz_stream, &value16) == MZ_OK)
            file_info.flag = value16;
    }
    file_info.compression_method = MZ_COMPRESS_METHOD_DEFLATE;
    if ((mz_stream_read_uint8(fuzz_stream, &value8) == MZ_OK) && (value8 < 0x08))
    {
        file_info.compression_method = MZ_COMPRESS_METHOD_STORE;
    }
    else if ((mz_stream_read_uint8(fuzz_stream, &value8) == MZ_OK) && (value8 < 0x08))
    {
        if (mz_stream_read_uint16(fuzz_stream, &value16) == MZ_OK)
            file_info.compression_method = value16;
    }

    if ((mz_stream_read_uint8(fuzz_stream, &value8) == MZ_OK) && (value8 < 0x08))
    {
        if (mz_stream_read_uint16(fuzz_stream, &value16) == MZ_OK)
            file_info.zip64 = value16;
    }

    file_info.filename = MZ_FUZZ_TEST_FILENAME;
    file_info.filename_size = (uint16_t)strlen(MZ_FUZZ_TEST_FILENAME);

    compress_level = MZ_COMPRESS_LEVEL_DEFAULT;
    if ((mz_stream_read_uint8(fuzz_stream, &value8) == MZ_OK) && (value8 < 0x08))
    {
        if (mz_stream_read_uint16(fuzz_stream, &value16) == MZ_OK)
            compress_level = value16;
    }

    mz_stream_mem_create(&stream);
    mz_zip_create(&handle);

    err = mz_zip_open(handle, stream, MZ_OPEN_MODE_CREATE | MZ_OPEN_MODE_WRITE);
    if (err == MZ_OK)
    {
        err = mz_zip_entry_write_open(handle, &file_info, compress_level, 0, NULL);
        if (err == MZ_OK)
        {
            mz_stream_mem_get_buffer_at_current(fuzz_stream, (const void **)&fuzz_buf);
            fuzz_pos = mz_stream_tell(fuzz_stream);
            mz_stream_mem_get_buffer_length(fuzz_stream, &fuzz_length);

            err = mz_zip_entry_write(handle, fuzz_buf, (fuzz_length - (int32_t)fuzz_pos));

            mz_zip_entry_close(handle);
        }

        mz_zip_close(handle);
    }

    mz_zip_delete(&handle);
    mz_stream_mem_delete(&stream);

    mz_stream_mem_delete(&fuzz_stream);

    return 0;
}

/***************************************************************************/

#ifdef __cplusplus
}
#endif
