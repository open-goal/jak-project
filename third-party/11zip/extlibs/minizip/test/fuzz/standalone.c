/* standalone.c - Standalone fuzzer tester
   part of the minizip-ng project

   Copyright (C) 2018 sebpop
     https://github.com/sebpop
   Copyright (C) 2018-2020 Nathan Moinvaziri
     https://github.com/zlib-ng/minizip-ng

   This program is distributed under the terms of the same license as zlib.
   See the accompanying LICENSE file for the full text of the license.
*/


#include "mz.h"
#include "mz_strm.h"
#include "mz_strm_os.h"

#include <stdio.h> /* printf */

#ifdef __cplusplus
extern "C" {
#endif

/***************************************************************************/

extern int LLVMFuzzerTestOneInput(const unsigned char *data, size_t size);

/***************************************************************************/

int main(int argc, char **argv)
{
    void *stream = NULL;
    int64_t file_size = 0;
    uint8_t *buf = NULL;
    int32_t buf_length = 0;
    int32_t err = MZ_OK;
    int32_t read = 0;
    int32_t i = 0;


    if (argc < 1)
    {
        printf("Must specify an input file\n");
        return 1;
    }

    printf("Running %"PRId32" inputs\n", argc - 1);

    for (i = 1; (i < argc) && (err == MZ_OK); i++)
    {
        read = 0;

        mz_stream_os_create(&stream);
        err = mz_stream_os_open(stream, argv[i], MZ_OPEN_MODE_READ);

        if (err != MZ_OK)
        {
            printf("Skipping %s (%"PRId32")\n", argv[i], err);
        }
        else
        {
            mz_stream_os_seek(stream, 0, MZ_SEEK_END);
            file_size = mz_stream_os_tell(stream);
            if (file_size > INT32_MAX)
                printf("File size is too large (%"PRId64")\n", file_size);
            else
                buf_length = (int32_t)file_size;
            mz_stream_os_seek(stream, 0, MZ_SEEK_SET);

            buf = NULL;
            if (buf_length > 0)
                buf = MZ_ALLOC(buf_length);

            if (buf != NULL)
            {
                printf("Running %s %"PRId32"\n", argv[i], buf_length);
                read = mz_stream_os_read(stream, buf, buf_length);
                if (read == buf_length)
                    LLVMFuzzerTestOneInput(buf, buf_length);
                else
                    err = MZ_BUF_ERROR;

                MZ_FREE(buf);
            }

            mz_stream_os_close(stream);
        }

        mz_stream_os_delete(&stream);
        printf("Done %s (%"PRId32")\n", argv[i], err);
    }

    return 0;
}

/***************************************************************************/

#ifdef __cplusplus
}
#endif
