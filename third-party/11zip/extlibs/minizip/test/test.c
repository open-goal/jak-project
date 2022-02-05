/* test.c - Test bed area
   part of the minizip-ng project

   Copyright (C) 2018-2020 Nathan Moinvaziri
     https://github.com/zlib-ng/minizip-ng

   This program is distributed under the terms of the same license as zlib.
   See the accompanying LICENSE file for the full text of the license.
*/

#include "mz.h"
#ifdef HAVE_COMPAT
#include "mz_compat.h"
#endif
#include "mz_crypt.h"
#include "mz_os.h"
#include "mz_strm.h"
#ifdef HAVE_BZIP2
#include "mz_strm_bzip.h"
#endif
#ifdef HAVE_PKCRYPT
#include "mz_strm_pkcrypt.h"
#endif
#include "mz_strm_mem.h"
#include "mz_strm_os.h"
#ifdef HAVE_WZAES
#include "mz_strm_wzaes.h"
#endif
#ifdef HAVE_ZLIB
#include "mz_strm_zlib.h"
#endif
#include "mz_zip.h"

#include <stdio.h> /* printf, snprintf */

#if defined(_MSC_VER) && (_MSC_VER < 1900)
#  define snprintf _snprintf
#endif

/***************************************************************************/

int32_t test_path_resolve_int(char *path, char *expected_path)
{
    char output[256];
    int32_t ok = 0;

    memset(output, 'z', sizeof(output));
    mz_path_resolve(path, output, sizeof(output));
    ok = (strcmp(output, expected_path) == 0);
    printf("path resolve - %s -> %s = %s (%" PRId32 ")\n", path, expected_path, output, ok);
    return !ok;
}

int32_t test_path_resolve(void)
{
    int32_t err = MZ_OK;

    err |= test_path_resolve_int("c:\\test\\.", "c:\\test\\");
    err |= test_path_resolve_int("c:\\test\\.\\", "c:\\test\\");
    err |= test_path_resolve_int("c:\\test\\.\\.", "c:\\test\\");
    err |= test_path_resolve_int("c:\\test\\..", "c:\\");
    err |= test_path_resolve_int("c:\\test\\..\\", "c:\\");
    err |= test_path_resolve_int("c:\\test\\.\\..", "c:\\");
    err |= test_path_resolve_int("c:\\test\\.\\\\..", "c:\\");
    err |= test_path_resolve_int(".", ".");
    err |= test_path_resolve_int(".\\", "");
    err |= test_path_resolve_int("..", "");
    err |= test_path_resolve_int("..\\", "");
    err |= test_path_resolve_int(".\\test\\123", "test\\123");
    err |= test_path_resolve_int(".\\..\\test\\123", "test\\123");
    err |= test_path_resolve_int("..\\..\\test\\123", "test\\123");
    err |= test_path_resolve_int("test\\.abc.txt", "test\\.abc.txt");
    err |= test_path_resolve_int("c:\\test\\123\\.\\abc.txt", "c:\\test\\123\\abc.txt");
    err |= test_path_resolve_int("c:\\test\\123\\..\\abc.txt", "c:\\test\\abc.txt");
    err |= test_path_resolve_int("c:\\test\\123\\..\\..\\abc.txt", "c:\\abc.txt");
    err |= test_path_resolve_int("c:\\test\\123\\..\\..\\..\\abc.txt", "abc.txt");
    err |= test_path_resolve_int("c:\\test\\123\\..\\.\\..\\abc.txt", "c:\\abc.txt");

    return err;
}

int32_t test_utf8(void)
{
    const char *test_string = "Heiz�lr�cksto�abd�mpfung";
    uint8_t *utf8_string = mz_os_utf8_string_create(test_string, MZ_ENCODING_CODEPAGE_950);
    if (utf8_string == NULL)
        return MZ_BUF_ERROR;
#if defined(_WIN32)
    wchar_t *unicode_string = mz_os_unicode_string_create((const char *)utf8_string, MZ_ENCODING_UTF8);
    if (unicode_string == NULL)
        return MZ_BUF_ERROR;
    mz_os_unicode_string_delete(&unicode_string);
#endif
    mz_os_utf8_string_delete(&utf8_string);
    return MZ_OK;
}

int32_t test_encrypt(char *method, mz_stream_create_cb crypt_create, char *password)
{
    char buf[UINT16_MAX];
    int32_t read = 0;
    int32_t written = 0;
    int64_t total_written = 0;
    void *out_stream = NULL;
    void *in_stream = NULL;
    void *crypt_out_stream = NULL;
    char encrypt_path[120];
    char decrypt_path[120];

    snprintf(encrypt_path, sizeof(encrypt_path), "LICENSE.encrypt.%s", method);
    snprintf(decrypt_path, sizeof(decrypt_path), "LICENSE.decrypt.%s", method);

    mz_stream_os_create(&in_stream);

    if (mz_stream_os_open(in_stream, "LICENSE", MZ_OPEN_MODE_READ) == MZ_OK)
    {
        read = mz_stream_os_read(in_stream, buf, UINT16_MAX);
        mz_stream_os_close(in_stream);
    }

    mz_stream_os_delete(&in_stream);
    mz_stream_os_create(&out_stream);

    if (mz_stream_os_open(out_stream, encrypt_path, MZ_OPEN_MODE_CREATE | MZ_OPEN_MODE_WRITE) == MZ_OK)
    {
        crypt_create(&crypt_out_stream);

        mz_stream_set_base(crypt_out_stream, out_stream);

        if (mz_stream_open(crypt_out_stream, password, MZ_OPEN_MODE_WRITE) == MZ_OK)
        {
            written = mz_stream_write(crypt_out_stream, buf, read);
            mz_stream_close(crypt_out_stream);
            mz_stream_get_prop_int64(crypt_out_stream, MZ_STREAM_PROP_TOTAL_OUT, &total_written);
        }

        mz_stream_delete(&crypt_out_stream);

        mz_stream_os_close(out_stream);

        printf("%s encrypted %" PRId32 "\n", encrypt_path, written);
    }

    mz_stream_os_delete(&out_stream);
    mz_stream_os_create(&in_stream);

    if (mz_stream_os_open(in_stream, encrypt_path, MZ_OPEN_MODE_READ) == MZ_OK)
    {
        crypt_create(&crypt_out_stream);

        mz_stream_set_base(crypt_out_stream, in_stream);
        mz_stream_set_prop_int64(crypt_out_stream, MZ_STREAM_PROP_TOTAL_IN_MAX, total_written);

        if (mz_stream_open(crypt_out_stream, password, MZ_OPEN_MODE_READ) == MZ_OK)
        {
            read = mz_stream_read(crypt_out_stream, buf, read);
            mz_stream_close(crypt_out_stream);
        }

        mz_stream_delete(&crypt_out_stream);

        mz_stream_os_close(in_stream);

        printf("%s decrypted %" PRId32 "\n", decrypt_path, read);
    }

    mz_stream_os_delete(&in_stream);
    mz_stream_os_create(&out_stream);

    if (mz_stream_os_open(out_stream, decrypt_path, MZ_OPEN_MODE_CREATE | MZ_OPEN_MODE_WRITE) == MZ_OK)
    {
        mz_stream_os_write(out_stream, buf, read);
        mz_stream_os_close(out_stream);
    }

    mz_stream_os_delete(&out_stream);
    return 0;
}

int32_t test_compress(char *method, mz_stream_create_cb create_compress)
{
    uint8_t buf[UINT16_MAX];
    int32_t read = 0;
    int64_t total_in = 0;
    int64_t total_out = 0;
    void *in_stream = NULL;
    void *out_stream = NULL;
    void *deflate_stream = NULL;
    void *inflate_stream = NULL;
    uint32_t crc32 = 0;
    char filename[120];

    printf("Testing compress %s\n", method);

    mz_stream_os_create(&in_stream);

    if (mz_stream_os_open(in_stream, "LICENSE", MZ_OPEN_MODE_READ) == MZ_OK)
    {
        read = mz_stream_os_read(in_stream, buf, UINT16_MAX);
        if (read > 0)
            crc32 = mz_crypt_crc32_update(crc32, (const uint8_t *)buf, read);

        mz_stream_os_close(in_stream);
    }

    mz_stream_os_delete(&in_stream);

    if (read < 0)
    {
        printf("Failed to read LICENSE\n");
        return MZ_OPEN_ERROR;
    }

    printf("LICENSE crc 0x%08x\n", crc32);

    mz_stream_os_create(&out_stream);

    snprintf(filename, sizeof(filename), "LICENSE.deflate.%s", method);
    if (mz_stream_os_open(out_stream, filename, MZ_OPEN_MODE_CREATE | MZ_OPEN_MODE_WRITE) == MZ_OK)
    {
        create_compress(&deflate_stream);
        mz_stream_set_base(deflate_stream, out_stream);

        mz_stream_open(deflate_stream, NULL, MZ_OPEN_MODE_WRITE);
        mz_stream_write(deflate_stream, buf, read);
        mz_stream_close(deflate_stream);

        mz_stream_get_prop_int64(deflate_stream, MZ_STREAM_PROP_TOTAL_IN, &total_in);
        mz_stream_get_prop_int64(deflate_stream, MZ_STREAM_PROP_TOTAL_OUT, &total_out);

        mz_stream_delete(&deflate_stream);

        printf("%s compressed from %u to %u\n", filename, (uint32_t)total_in, (uint32_t)total_out);

        mz_stream_os_close(out_stream);
    }

    mz_stream_os_delete(&out_stream);
    mz_stream_os_create(&in_stream);

    if (mz_stream_os_open(in_stream, filename, MZ_OPEN_MODE_READ) == MZ_OK)
    {
        create_compress(&inflate_stream);
        mz_stream_set_base(inflate_stream, in_stream);

        mz_stream_open(inflate_stream, NULL, MZ_OPEN_MODE_READ);
        read = mz_stream_read(inflate_stream, buf, UINT16_MAX);
        mz_stream_close(inflate_stream);

        mz_stream_get_prop_int64(inflate_stream, MZ_STREAM_PROP_TOTAL_IN, &total_in);
        mz_stream_get_prop_int64(inflate_stream, MZ_STREAM_PROP_TOTAL_OUT, &total_out);

        mz_stream_delete(&inflate_stream);

        mz_stream_os_close(in_stream);

        printf("%s uncompressed from %u to %u\n", filename, (uint32_t)total_in, (uint32_t)total_out);
    }

    mz_stream_os_delete(&in_stream);
    mz_stream_os_create(&out_stream);

    crc32 = 0;

    snprintf(filename, sizeof(filename), "LICENSE.inflate.%s", method);
    if (mz_stream_os_open(out_stream, filename, MZ_OPEN_MODE_CREATE | MZ_OPEN_MODE_WRITE) == MZ_OK)
    {
        crc32 = mz_crypt_crc32_update(crc32, (const uint8_t *)buf, read);

        mz_stream_os_close(out_stream);

        printf("%s crc 0x%08x\n", filename, crc32);
    }

    mz_stream_os_delete(&out_stream);

    return MZ_OK;
}

/***************************************************************************/

#ifdef HAVE_BZIP2
int test_stream_bzip(void)
{
    return test_compress("bzip", mz_stream_bzip_create);
}
#endif
#ifdef HAVE_PKCRYPT
int test_stream_pkcrypt(void)
{
    return test_encrypt("pkcrypt", mz_stream_pkcrypt_create, "hello");
}
#endif
#ifdef HAVE_WZAES
int test_stream_wzaes(void)
{
    int32_t iteration_count = 1000;
    int32_t err = MZ_OK;
    int32_t i = 0;
    uint8_t key[MZ_HASH_SHA1_SIZE];
    const char *password = "passwordpasswordpasswordpassword";
    const char *salt = "8F3472E4EA57F56E36F30246DC22C173";


    printf("Pbkdf2 password - %s\n", password);
    printf("Pbkdf2 salt - %s\n", salt);

    err = mz_crypt_pbkdf2((uint8_t *)password, (int32_t)strlen(password),
        (uint8_t *)salt, (int32_t)strlen(salt), iteration_count, key, sizeof(key));

    if (err == MZ_OK)
    {
        printf("Pbkdf2 key hex\n");
        for (i = 0; i < (int32_t)sizeof(key); i += 1)
            printf("%02x", key[i]);
        printf("\n");
    }
    else
    {
        printf("Pbkdf2 failed - %" PRId32 "", err);
        return MZ_CRYPT_ERROR;
    }

    return test_encrypt("aes", mz_stream_wzaes_create, "hello");
}
#endif
#ifdef HAVE_ZLIB
int32_t test_stream_zlib(void)
{
    return test_compress("zlib", mz_stream_zlib_create);
}

int32_t test_stream_zlib_mem(void)
{
    mz_zip_file file_info;
    void *read_mem_stream = NULL;
    void *write_mem_stream = NULL;
    void *os_stream = NULL;
    void *zip_handle = NULL;
    int32_t written = 0;
    int32_t read = 0;
    int32_t text_size = 0;
    int32_t buffer_size = 0;
    int32_t err = MZ_OK;
    const uint8_t *buffer_ptr = NULL;
    char *password = NULL;
    char *text_name = "test";
    char *text_ptr = "test string";
    char temp[120];


    memset(&file_info, 0, sizeof(file_info));

    text_size = (int32_t)strlen(text_ptr);

    /* Write zip to memory stream */
    mz_stream_mem_create(&write_mem_stream);
    mz_stream_mem_set_grow_size(write_mem_stream, 128 * 1024);
    mz_stream_open(write_mem_stream, NULL, MZ_OPEN_MODE_CREATE);

    mz_zip_create(&zip_handle);
    err = mz_zip_open(zip_handle, write_mem_stream, MZ_OPEN_MODE_WRITE);

    if (err == MZ_OK)
    {
        file_info.version_madeby = MZ_VERSION_MADEBY;
        file_info.compression_method = MZ_COMPRESS_METHOD_DEFLATE;
        file_info.filename = text_name;
        file_info.uncompressed_size = text_size;
#ifdef HAVE_WZAES
        file_info.aes_version = MZ_AES_VERSION;
        password = "1234";
#endif

        err = mz_zip_entry_write_open(zip_handle, &file_info, MZ_COMPRESS_LEVEL_DEFAULT, 0, password);
        if (err == MZ_OK)
        {
            written = mz_zip_entry_write(zip_handle, text_ptr, text_size);
            if (written < MZ_OK)
                err = written;
            mz_zip_entry_close(zip_handle);
        }

        mz_zip_close(zip_handle);
    }
    else
    {
        err = MZ_INTERNAL_ERROR;
    }

    mz_zip_delete(&zip_handle);

    mz_stream_mem_get_buffer(write_mem_stream, (const void **)&buffer_ptr);
    mz_stream_mem_seek(write_mem_stream, 0, MZ_SEEK_END);
    buffer_size = (int32_t)mz_stream_mem_tell(write_mem_stream);

    if (err == MZ_OK)
    {
        /* Create a zip file on disk for inspection */
        mz_stream_os_create(&os_stream);
        mz_stream_os_open(os_stream, "mytest.zip", MZ_OPEN_MODE_WRITE | MZ_OPEN_MODE_CREATE);
        mz_stream_os_write(os_stream, buffer_ptr, buffer_size);
        mz_stream_os_close(os_stream);
        mz_stream_os_delete(&os_stream);
    }

    if (err == MZ_OK)
    {
        /* Read from a memory stream */
        mz_stream_mem_create(&read_mem_stream);
        mz_stream_mem_set_buffer(read_mem_stream, (void *)buffer_ptr, buffer_size);
        mz_stream_open(read_mem_stream, NULL, MZ_OPEN_MODE_READ);

        mz_zip_create(&zip_handle);
        err = mz_zip_open(zip_handle, read_mem_stream, MZ_OPEN_MODE_READ);

        if (err == MZ_OK)
        {
            err = mz_zip_goto_first_entry(zip_handle);
            if (err == MZ_OK)
                err = mz_zip_entry_read_open(zip_handle, 0, password);
            if (err == MZ_OK)
                read = mz_zip_entry_read(zip_handle, temp, sizeof(temp));

            MZ_UNUSED(read);

            mz_zip_entry_close(zip_handle);
            mz_zip_close(zip_handle);
        }

        mz_zip_delete(&zip_handle);

        mz_stream_mem_close(&read_mem_stream);
        mz_stream_mem_delete(&read_mem_stream);
        read_mem_stream = NULL;
    }

    mz_stream_mem_close(write_mem_stream);
    mz_stream_mem_delete(&write_mem_stream);
    write_mem_stream = NULL;

    return err;
}
#endif

/***************************************************************************/

int32_t test_stream_find_run(char *name, int32_t count, const uint8_t *find, int32_t find_size, mz_stream_find_cb find_cb)
{
    void *mem_stream = NULL;
    int32_t i = 0;
    int32_t x = 0;
    int32_t err = MZ_OK;
    int64_t last_pos = 0;
    int64_t position = 0;

    MZ_UNUSED(name);

    if (find == NULL || find_size == 0 || find_cb == NULL)
        return MZ_PARAM_ERROR;

    for (i = 0; i < count; i += 1)
    {
#if 1
        mz_stream_mem_create(&mem_stream);
        mz_stream_mem_open(mem_stream, NULL, MZ_OPEN_MODE_CREATE);

        for (x = 0; x < find_size; x += 1)
            mz_stream_write_uint8(mem_stream, find[x]);
        for (x = 0; x < i; x += 1)
            mz_stream_write_uint8(mem_stream, 0);

        if (find_cb == mz_stream_find)
            mz_stream_seek(mem_stream, 0, MZ_SEEK_SET);

        err = find_cb(mem_stream, (const void *)find, find_size, (int64_t)i + find_size, &position);
        last_pos = mz_stream_tell(mem_stream);
        mz_stream_mem_delete(&mem_stream);

#ifdef TEST_VERBOSE
        printf("Find postzero - %s (len %" PRId32 " pos %" PRId64 " ok %" PRId32 ")\n",
            name, find_size, position, (position == 0));
#endif

        if (position != 0 || last_pos != position)
            break;
#endif
        mz_stream_mem_create(&mem_stream);
        mz_stream_mem_open(mem_stream, NULL, MZ_OPEN_MODE_CREATE);

        for (x = 0; x < i; x += 1)
            mz_stream_write_uint8(mem_stream, 0);
        for (x = 0; x < find_size; x += 1)
            mz_stream_write_uint8(mem_stream, find[x]);

        if (find_cb == mz_stream_find)
            mz_stream_seek(mem_stream, 0, MZ_SEEK_SET);

        err = find_cb(mem_stream, (const void *)find, find_size, (int64_t)i + find_size, &position);
        last_pos = mz_stream_tell(mem_stream);
        mz_stream_mem_delete(&mem_stream);

#ifdef TEST_VERBOSE
        printf("Find prezero - %s (len %" PRId32 " pos %" PRId64 " ok %" PRId32 ")\n",
            name, find_size, position, (position == i));
#endif

        if (position != i || last_pos != position)
            break;

        mz_stream_mem_create(&mem_stream);
        mz_stream_mem_open(mem_stream, NULL, MZ_OPEN_MODE_CREATE);

        for (x = 0; x < i; x += 1)
            mz_stream_write_uint8(mem_stream, 0);
        for (x = 0; x < find_size; x += 1)
            mz_stream_write_uint8(mem_stream, find[x]);
        for (x = 0; x < i; x += 1)
            mz_stream_write_uint8(mem_stream, 0);

        if (find_cb == mz_stream_find)
            mz_stream_seek(mem_stream, 0, MZ_SEEK_SET);

        err = find_cb(mem_stream, (const void *)find, find_size, (int64_t)i + find_size + i, &position);
        last_pos = mz_stream_tell(mem_stream);
        mz_stream_mem_delete(&mem_stream);

#ifdef TEST_VERBOSE
        printf("Find equalzero - %s (len %" PRId32 " pos %" PRId64 " ok %" PRId32 ")\n",
            name, find_size, position, (position == i));
#endif

        if (position != i || last_pos != position)
            break;

        mz_stream_mem_create(&mem_stream);
        mz_stream_mem_open(mem_stream, NULL, MZ_OPEN_MODE_CREATE);

        for (x = 0; x < i; x += 1)
            mz_stream_write_uint8(mem_stream, 0);
        for (x = 0; x < find_size; x += 1)
            mz_stream_write_uint8(mem_stream, find[x]);
        for (x = 0; x < i; x += 1)
            mz_stream_write_uint8(mem_stream, 0);
        mz_stream_write_uint8(mem_stream, 0);

        if (find_cb == mz_stream_find)
            mz_stream_seek(mem_stream, 0, MZ_SEEK_SET);

        err = find_cb(mem_stream, (const void *)find, find_size, (int64_t)i + find_size + i + 1, &position);
        last_pos = mz_stream_tell(mem_stream);
        mz_stream_mem_delete(&mem_stream);

#ifdef TEST_VERBOSE
        printf("Find unequalzero - %s (len %" PRId32 " pos %" PRId64 " ok %" PRId32 ")\n",
            name, find_size, position, (position == i));
#endif

        if (position != i || last_pos != position)
            break;
    }

    return err;
}

int32_t test_stream_find(void)
{
    int32_t c = 1;
    int32_t err = MZ_OK;
    char *find = "0123456789";

    printf("Find stream.. ");
    for (c = 1; c < (int32_t)strlen(find); c += 1)
    {
        err = test_stream_find_run("forward", 2096, (uint8_t *)find, c, mz_stream_find);
        if (err != MZ_OK)
            return err;
    }

    printf("OK\n");
    return MZ_OK;
}

int32_t test_stream_find_reverse(void)
{
    int32_t c = 1;
    int32_t err = MZ_OK;
    char *find = "0123456789";

    printf("Find reverse stream.. ");
    for (c = 1; c < (int32_t)strlen(find); c += 1)
    {
        err = test_stream_find_run("backward", 2096, (uint8_t *)find, c, mz_stream_find_reverse);
        if (err != MZ_OK)
            return err;
    }

    printf("OK\n");
    return MZ_OK;
}

/***************************************************************************/

int32_t convert_buffer_to_hex_string(uint8_t *buf, int32_t buf_size, char *hex_string, int32_t max_hex_string)
{
    int32_t p = 0;
    int32_t i = 0;

    if (max_hex_string > 0)
        hex_string[0] = 0;
    for (i = 0, p = 0; i < (int32_t)buf_size && p < max_hex_string; i += 1, p += 2)
        snprintf(hex_string + p, max_hex_string - p, "%02x", buf[i]);
    if (p < max_hex_string)
        hex_string[p] = 0;
    return MZ_OK;
}

#ifndef MZ_ZIP_NO_CRYPTO
int32_t test_crypt_sha(void)
{
    void *sha1 = NULL;
    void *sha256 = NULL;
    char *test = "the quick and lazy fox did his thang";
    char computed_hash[320];
    uint8_t hash[MZ_HASH_SHA1_SIZE];
    uint8_t hash256[MZ_HASH_SHA256_SIZE];

    printf("Sha hash input - %s\n", test);

    memset(hash, 0, sizeof(hash));

    mz_crypt_sha_create(&sha1);
    mz_crypt_sha_set_algorithm(sha1, MZ_HASH_SHA1);
    mz_crypt_sha_begin(sha1);
    mz_crypt_sha_update(sha1, test, (int32_t)strlen(test));
    mz_crypt_sha_end(sha1, hash, sizeof(hash));
    mz_crypt_sha_delete(&sha1);

    convert_buffer_to_hex_string(hash, sizeof(hash), computed_hash, sizeof(computed_hash));

    printf("Sha1 hash computed - %s\n", computed_hash);
    printf("Sha1 hash expected - 3efb8392b6cd8e14bd76bd08081521dc73df418c\n");

    if (strcmp(computed_hash, "3efb8392b6cd8e14bd76bd08081521dc73df418c") != 0)
        return MZ_HASH_ERROR;

    memset(hash256, 0, sizeof(hash256));

    mz_crypt_sha_create(&sha256);
    mz_crypt_sha_set_algorithm(sha256, MZ_HASH_SHA256);
    mz_crypt_sha_begin(sha256);
    mz_crypt_sha_update(sha256, test, (int32_t)strlen(test));
    mz_crypt_sha_end(sha256, hash256, sizeof(hash256));
    mz_crypt_sha_delete(&sha256);

    convert_buffer_to_hex_string(hash256, sizeof(hash256), computed_hash, sizeof(computed_hash));

    printf("Sha256 hash computed - %s\n", computed_hash);
    printf("Sha256 hash expected - 7a31ea0848525f7ebfeec9ee532bcc5d6d26772427e097b86cf440a56546541c\n");

    if (strcmp(computed_hash, "7a31ea0848525f7ebfeec9ee532bcc5d6d26772427e097b86cf440a56546541c") != 0)
        return MZ_HASH_ERROR;

    printf("Sha.. OK\n");
    return MZ_OK;
}

int test_crypt_aes(void)
{
    void *aes = NULL;
    char *key = "awesomekeythisis";
    char *test = "youknowitsogrowi";
    char computed_hash[320];
    int32_t key_length = 0;
    int32_t test_length = 0;
    uint8_t buf[120];
    uint8_t hash[MZ_HASH_SHA256_SIZE];

    printf("Aes key - %s\n", key);
    printf("Aes input - %s\n", test);

    memset(hash, 0, sizeof(hash));

    key_length = (int32_t)strlen(key);
    test_length = (int32_t)strlen(test);

    strncpy((char *)buf, test, sizeof(buf));

    printf("Aes input hex\n");
    convert_buffer_to_hex_string(buf, test_length, computed_hash, sizeof(computed_hash));
    printf("%s\n", computed_hash);

    mz_crypt_aes_create(&aes);
    mz_crypt_aes_set_mode(aes, MZ_AES_ENCRYPTION_MODE_256);
    mz_crypt_aes_set_encrypt_key(aes, key, key_length);
    mz_crypt_aes_encrypt(aes, buf, test_length);
    mz_crypt_aes_delete(&aes);

    printf("Aes encrypted\n");
    convert_buffer_to_hex_string(buf, test_length, computed_hash, sizeof(computed_hash));
    printf("%s\n", computed_hash);

    mz_crypt_aes_create(&aes);
    mz_crypt_aes_set_mode(aes, MZ_AES_ENCRYPTION_MODE_256);
    mz_crypt_aes_set_decrypt_key(aes, key, key_length);
    mz_crypt_aes_decrypt(aes, buf, test_length);
    mz_crypt_aes_delete(&aes);

    printf("Aes decrypted\n");
    convert_buffer_to_hex_string(buf, test_length, computed_hash, sizeof(computed_hash));
    printf("%s\n", computed_hash);

    if (strcmp((char *)buf, test) != 0)
        return MZ_CRYPT_ERROR;

    printf("Aes.. OK\n");
    return MZ_OK;
}

int32_t test_crypt_hmac(void)
{
    void *hmac;
    char *key = "hm123";
    char *test = "12345678";
    char computed_hash[320];
    int32_t key_length = 0;
    int32_t test_length = 0;
    uint8_t hash[MZ_HASH_SHA1_SIZE];
    uint8_t hash256[MZ_HASH_SHA256_SIZE];

    key_length = (int32_t)strlen(key);
    test_length = (int32_t)strlen(test);

    printf("Hmac sha1 key - %s\n", key);
    printf("Hmac sha1 input - %s\n", test);

    mz_crypt_hmac_create(&hmac);
    mz_crypt_hmac_set_algorithm(hmac, MZ_HASH_SHA1);
    mz_crypt_hmac_init(hmac, key, key_length);
    mz_crypt_hmac_update(hmac, test, test_length);
    mz_crypt_hmac_end(hmac, hash, sizeof(hash));
    mz_crypt_hmac_delete(&hmac);

    printf("Hmac sha1 output hash hex\n");
    convert_buffer_to_hex_string(hash, sizeof(hash), computed_hash, sizeof(computed_hash));
    printf("%s\n", computed_hash);

    printf("Hmac sha1 expected\n");
    printf("c785a02ff303c886c304d9a4c06073dfe4c24aa9\n");

    if (strcmp(computed_hash, "c785a02ff303c886c304d9a4c06073dfe4c24aa9") != 0)
        return MZ_CRYPT_ERROR;

    printf("Hmac sha256 key - %s\n", key);
    printf("Hmac sha256 input - %s\n", test);

    mz_crypt_hmac_create(&hmac);
    mz_crypt_hmac_set_algorithm(hmac, MZ_HASH_SHA256);
    mz_crypt_hmac_init(hmac, key, key_length);
    mz_crypt_hmac_update(hmac, test, test_length);
    mz_crypt_hmac_end(hmac, hash256, sizeof(hash256));
    mz_crypt_hmac_delete(&hmac);

    printf("Hmac sha256 output hash hex\n");
    convert_buffer_to_hex_string(hash256, sizeof(hash256), computed_hash, sizeof(computed_hash));
    printf("%s\n", computed_hash);

    printf("Hmac sha256 expected\n");
    printf("fb22a9c715a47a06bad4f6cee9badc31c921562f5d6b24adf2be009f73181f7a\n");

    if (strcmp(computed_hash, "fb22a9c715a47a06bad4f6cee9badc31c921562f5d6b24adf2be009f73181f7a") != 0)
        return MZ_CRYPT_ERROR;

    printf("Hmac.. OK\n");
    return MZ_OK;
}
#endif

#if defined(HAVE_COMPAT) && defined(HAVE_ZLIB)
int32_t test_zip_compat_int(zipFile zip, char *filename)
{
    int32_t err = ZIP_OK;
    zip_fileinfo file_info;
    char *buffer = "test data";

    memset(&file_info, 0, sizeof(file_info));
    file_info.dosDate = mz_zip_time_t_to_dos_date(1588561637);

    err = zipOpenNewFileInZip(zip, filename, &file_info, NULL, 0, NULL, 0, "test local comment",
        Z_DEFLATED, 1);
    if (err != ZIP_OK)
    {
        printf("Failed to create new file in zip (%" PRId32 ")\n", err);
        return err;
    }
    err = zipWriteInFileInZip(zip, buffer, (uint32_t)strlen(buffer));
    if (err != ZIP_OK)
    {
        printf("Failed to write file in zip (%" PRId32 ")\n", err);
        return err;
    }
    err = zipCloseFileInZip(zip);
    if (err != ZIP_OK)
    {
        printf("Failed to close file in zip (%" PRId32 ")\n", err);
        return err;
    }

    return ZIP_OK;
}

int32_t test_zip_compat(void)
{
    int32_t err = ZIP_OK;
    zipFile zip;


    zip = zipOpen64("compat.zip", APPEND_STATUS_CREATE);

    if (zip == NULL)
    {
        printf("Failed to create test zip file\n");
        return ZIP_PARAMERROR;
    }
    err = test_zip_compat_int(zip, "test.txt");
    if (err != ZIP_OK)
        return err;
    err = test_zip_compat_int(zip, "test2.txt");
    if (err != ZIP_OK)
        return err;

    zipClose(zip, "test global comment");

    if (err != ZIP_OK)
        return err;

    printf("Compat zip.. OK\n");

    return ZIP_OK;
}

static int32_t test_unzip_compat_int(unzFile unzip)
{
    unz_global_info64 global_info64;
    unz_global_info global_info;
    unz_file_info64 file_info64;
    unz_file_info file_info;
    unz_file_pos file_pos;
    int32_t err = UNZ_OK;
    int32_t bytes_read = 0;
    char comment[120];
    char filename[120];
    char buffer[120];
    char *test_data = "test data";

    memset(&file_info, 0, sizeof(file_info));
    memset(&file_info64, 0, sizeof(file_info64));
    memset(&global_info, 0, sizeof(global_info));
    memset(&global_info64, 0, sizeof(global_info64));

    comment[0] = 0;
    err = unzGetGlobalComment(unzip, comment, sizeof(comment));
    if (err != UNZ_OK)
    {
        printf("Failed to get global comment (%" PRId32 ")\n", err);
        return err;
    }
    if (strcmp(comment, "test global comment") != 0)
    {
        printf("Unexpected global comment value (%s)\n", comment);
        return err;
    }
    err = unzGetGlobalInfo(unzip, &global_info);
    if (err != UNZ_OK)
    {
        printf("Failed to get global info  (%" PRId32 ")\n", err);
        return err;
    }
    err = unzGetGlobalInfo64(unzip, &global_info64);
    if (err != UNZ_OK)
    {
        printf("Failed to get global info 64-bit (%" PRId32 ")\n", err);
        return err;
    }
    if (global_info.number_entry != 2 || global_info64.number_entry != 2)
    {
        printf("Invalid number of entries in zip (%" PRId32 ")\n", global_info.number_entry);
        return err;
    }
    if (global_info.number_disk_with_CD != 0 || global_info64.number_disk_with_CD != 0)
    {
        printf("Invalid disk with cd (%" PRIu32 ")\n", global_info.number_disk_with_CD);
        return err;
    }

    err = unzLocateFile(unzip, "test.txt", (void *)1);
    if (err != UNZ_OK)
    {
        printf("Failed to locate test file (%" PRId32 ")\n", err);
        return err;
    }

    err = unzGoToFirstFile(unzip);
    if (err == UNZ_OK)
    {
        filename[0] = 0;
        err = unzGetCurrentFileInfo64(unzip, &file_info64, filename, sizeof(filename), NULL, 0, NULL, 0);
        if (err != UNZ_OK)
        {
            printf("Failed to get current file info 64-bit (%" PRId32 ")\n", err);
            return err;
        }

        err = unzOpenCurrentFile(unzip);
        if (err != UNZ_OK)
        {
            printf("Failed to open current file (%" PRId32 ")\n", err);
            return err;
        }
        bytes_read = unzReadCurrentFile(unzip, buffer, sizeof(buffer));
        if (bytes_read != (int32_t)strlen(test_data))
        {
            printf("Failed to read zip entry data (%" PRId32 ")\n", err);
            unzCloseCurrentFile(unzip);
            return err;
        }
        if (unzEndOfFile(unzip) != 1)
        {
            printf("End of unzip not reported correctly\n");
            return UNZ_INTERNALERROR;
        }
        err = unzCloseCurrentFile(unzip);
        if (err != UNZ_OK)
        {
            printf("Failed to close current file (%" PRId32 ")\n", err);
            return err;
        }

        if (unztell(unzip) != bytes_read)
        {
            printf("Unzip position not reported correctly\n");
            return UNZ_INTERNALERROR;
        }

        err = unzGoToNextFile(unzip);
        if (err != UNZ_OK)
        {
            printf("Failed to get next file info (%" PRId32 ")\n", err);
            return err;
        }

        comment[0] = 0;
        err = unzGetCurrentFileInfo(unzip, &file_info, filename, sizeof(filename), NULL, 0, comment, sizeof(comment));
        if (err != UNZ_OK)
        {
            printf("Failed to get current file info (%" PRId32 ")\n", err);
            return err;
        }
        if (strcmp(comment, "test local comment") != 0)
        {
            printf("Unexpected local comment value (%s)\n", comment);
            return err;
        }

        err = unzGetFilePos(unzip, &file_pos);
        if (err != UNZ_OK)
        {
            printf("Failed to get file position (%" PRId32 ")\n", err);
            return err;
        }
        if (file_pos.num_of_file != 1)
        {
            printf("Unzip file position not reported correctly\n");
            return UNZ_INTERNALERROR;
        }

        err = unzGetOffset(unzip);
        if (err <= 0)
        {
            printf("Unzip invalid offset reported\n");
            return UNZ_INTERNALERROR;
        }

        err = unzGoToNextFile(unzip);

        if (err != UNZ_END_OF_LIST_OF_FILE)
        {
            printf("Failed to reach end of zip entries (%" PRId32 ")\n", err);
            unzCloseCurrentFile(unzip);
            return err;
        }
        err = unzSeek64(unzip, 0, SEEK_SET);
    }

    return UNZ_OK;
}

#ifndef MZ_FILE32_API
#  ifndef NO_FSEEKO
#    define ftello64 ftello
#    define fseeko64 fseeko
#  elif defined(_MSC_VER) && (_MSC_VER >= 1400)
#    define ftello64 _ftelli64
#    define fseeko64 _fseeki64
#  endif
#endif
#ifndef ftello64
#  define ftello64 ftell
#endif
#ifndef fseeko64
#  define fseeko64 fseek
#endif

static void *ZCALLBACK fopen_file_func(void *opaque, const char *filename, int mode)
{
    FILE* file = NULL;
    const char* mode_fopen = NULL;

    if ((mode & ZLIB_FILEFUNC_MODE_READWRITEFILTER)==ZLIB_FILEFUNC_MODE_READ)
        mode_fopen = "rb";
    else if (mode & ZLIB_FILEFUNC_MODE_EXISTING)
        mode_fopen = "r+b";
    else if (mode & ZLIB_FILEFUNC_MODE_CREATE)
        mode_fopen = "wb";

    if ((filename != NULL) && (mode_fopen != NULL))
        file = fopen(filename, mode_fopen);

    return file;
}

static unsigned long ZCALLBACK fread_file_func(void *opaque, void *stream, void *buf, unsigned long size)
{
    return (unsigned long)fread(buf, 1, (size_t)size, (FILE *)stream);
}

static unsigned long ZCALLBACK fwrite_file_func(void *opaque, void *stream, const void *buf, unsigned long size)
{
    return (unsigned long)fwrite(buf, 1, (size_t)size, (FILE *)stream);
}

static long ZCALLBACK ftell_file_func(void *opaque, void *stream)
{
    return ftell((FILE *)stream);
}

static ZPOS64_T ZCALLBACK ftell64_file_func(void *opaque, void *stream)
{
    return ftello64((FILE *)stream);
}

static long ZCALLBACK fseek_file_func(void *opaque, void *stream, unsigned long offset, int origin)
{
    int fseek_origin = 0;
    long ret = 0;
    switch (origin)
    {
    case ZLIB_FILEFUNC_SEEK_CUR:
        fseek_origin = SEEK_CUR;
        break;
    case ZLIB_FILEFUNC_SEEK_END:
        fseek_origin = SEEK_END;
        break;
    case ZLIB_FILEFUNC_SEEK_SET:
        fseek_origin = SEEK_SET;
        break;
    default:
        return -1;
    }
    if (fseek((FILE *)stream, offset, fseek_origin) != 0)
        ret = -1;
    return ret;
}

static long ZCALLBACK fseek64_file_func(void *opaque, void *stream, ZPOS64_T offset, int origin)
{
    int fseek_origin = 0;
    long ret = 0;
    switch (origin)
    {
    case ZLIB_FILEFUNC_SEEK_CUR:
        fseek_origin = SEEK_CUR;
        break;
    case ZLIB_FILEFUNC_SEEK_END:
        fseek_origin = SEEK_END;
        break;
    case ZLIB_FILEFUNC_SEEK_SET:
        fseek_origin = SEEK_SET;
        break;
    default:
        return -1;
    }
    if (fseeko64((FILE *)stream, offset, fseek_origin) != 0)
        ret = -1;
    return ret;
}

static int ZCALLBACK fclose_file_func(void *opaque, void *stream)
{
    return fclose((FILE *)stream);
}

static int ZCALLBACK ferror_file_func(void *opaque, void *stream)
{
    return ferror((FILE *)stream);
}

void fill_ioapi32_filefunc(zlib_filefunc_def *pzlib_filefunc_def)
{
    pzlib_filefunc_def->zopen_file = fopen_file_func;
    pzlib_filefunc_def->zread_file = fread_file_func;
    pzlib_filefunc_def->zwrite_file = fwrite_file_func;
    pzlib_filefunc_def->ztell_file = ftell_file_func;
    pzlib_filefunc_def->zseek_file = fseek_file_func;
    pzlib_filefunc_def->zclose_file = fclose_file_func;
    pzlib_filefunc_def->zerror_file = ferror_file_func;
    pzlib_filefunc_def->opaque = NULL;
}

void fill_ioapi64_filefunc(zlib_filefunc64_def *pzlib_filefunc_def)
{
    pzlib_filefunc_def->zopen64_file = (open64_file_func)fopen_file_func;
    pzlib_filefunc_def->zread_file = fread_file_func;
    pzlib_filefunc_def->zwrite_file = fwrite_file_func;
    pzlib_filefunc_def->ztell64_file = ftell64_file_func;
    pzlib_filefunc_def->zseek64_file = fseek64_file_func;
    pzlib_filefunc_def->zclose_file = fclose_file_func;
    pzlib_filefunc_def->zerror_file = ferror_file_func;
    pzlib_filefunc_def->opaque = NULL;
}

int32_t test_unzip_compat(void)
{
    unzFile unzip;
    int32_t err = UNZ_OK;

    unzip = unzOpen("compat.zip");
    if (unzip == NULL)
    {
        printf("Failed to open test zip file\n");
        return UNZ_PARAMERROR;
    }
    err = test_unzip_compat_int(unzip);
    unzClose(unzip);

    if (err != UNZ_OK)
        return err;

    printf("Compat unzip.. OK\n");

    return UNZ_OK;
}

int32_t test_unzip_compat32(void)
{
    unzFile unzip;
    int32_t err = UNZ_OK;
    zlib_filefunc_def zlib_filefunc_def;

    fill_ioapi32_filefunc(&zlib_filefunc_def);
    unzip = unzOpen2("compat.zip", &zlib_filefunc_def);
    if (unzip == NULL)
    {
        printf("Failed to open test zip file\n");
        return UNZ_PARAMERROR;
    }
    err = test_unzip_compat_int(unzip);
    unzClose(unzip);

    if (err != UNZ_OK)
        return err;

    printf("Compat unzip with 32-bit ioapi.. OK\n");

    return UNZ_OK;
}

int32_t test_unzip_compat64(void)
{
    unzFile unzip;
    int32_t err = UNZ_OK;
    zlib_filefunc64_def zlib_filefunc_def;

    fill_ioapi64_filefunc(&zlib_filefunc_def);
    unzip = unzOpen2_64("compat.zip", &zlib_filefunc_def);
    if (unzip == NULL)
    {
        printf("Failed to open test zip file\n");
        return UNZ_PARAMERROR;
    }
    err = test_unzip_compat_int(unzip);
    unzClose(unzip);

    if (err != UNZ_OK)
        return err;

    printf("Compat unzip with 64-bit ioapi.. OK\n");

    return UNZ_OK;
}
#endif

/***************************************************************************/

int main(int argc, const char *argv[])
{
    int32_t err = MZ_OK;

    MZ_UNUSED(argc);
    MZ_UNUSED(argv);

    err |= test_path_resolve();
    err |= test_utf8();
    err |= test_stream_find();
    err |= test_stream_find_reverse();

#if !defined(MZ_ZIP_NO_COMPRESSION) && !defined(MZ_ZIP_NO_DECOMPRESSION)
#ifdef HAVE_BZIP2
    err |= test_stream_bzip();
#endif
#ifdef HAVE_ZLIB
    err |= test_stream_zlib();
    err |= test_stream_zlib_mem();
#ifdef HAVE_COMPAT
    err |= test_zip_compat();
    err |= test_unzip_compat();
    err |= test_unzip_compat32();
    err |= test_unzip_compat64();
#endif
#endif
#endif

#ifdef HAVE_PKCRYPT
    err |= test_stream_pkcrypt();
#endif
#if !defined(MZ_ZIP_NO_CRYPTO)
#ifdef HAVE_WZAES
    err |= test_stream_wzaes();
#endif
    err |= test_crypt_sha();
    err |= test_crypt_aes();
    err |= test_crypt_hmac();
#endif

    return err;
}

/***************************************************************************/
