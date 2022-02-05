#ifndef _MZ_TEST_H
#define _MZ_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

/***************************************************************************/

int32_t test_stream_bzip(void);
int32_t test_stream_pkcrypt(void);
int32_t test_stream_wzaes(void);
int32_t test_stream_zlib(void);
int32_t test_stream_zlib_mem(void);
int32_t test_stream_find(void);
int32_t test_stream_find_reverse(void);

int32_t test_crypt_sha(void);
int32_t test_crypt_aes(void);
int32_t test_crypt_hmac(void);

/***************************************************************************/

#ifdef __cplusplus
}
#endif

#endif
