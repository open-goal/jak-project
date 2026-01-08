/***************************************************************************************************

  Zyan Core Library (Zycore-C)

  Original Author : Florian Bernd

 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.

***************************************************************************************************/

/**
 * @file
 * Demonstrates the `String` implementation.
 */

#include <stdio.h>
#include <Zycore/Allocator.h>
#include <Zycore/Defines.h>
#include <Zycore/LibC.h>
#include <Zycore/String.h>
#include <Zycore/Types.h>

/* ============================================================================================== */
/* Enums and types                                                                                */
/* ============================================================================================== */


/* ============================================================================================== */
/* Helper functions                                                                               */
/* ============================================================================================== */

static ZyanStatus PrintString(ZyanString* string) {
    const char* cstring;
    ZYAN_CHECK(ZyanStringGetData(string, &cstring));

    printf("(*ZyanString)%p = %s\n", (void*)string, cstring);

    return ZYAN_STATUS_SUCCESS;
}

/* ============================================================================================== */
/* Tests                                                                                          */
/* ============================================================================================== */

/* ---------------------------------------------------------------------------------------------- */
/* Basic tests                                                                                    */
/* ---------------------------------------------------------------------------------------------- */

/**
 * Performs some basic test on the given `ZyanString` instance.
 *
 * @param   string  A pointer to the `ZyanString` instance.
 *
 * @return  A zyan status code.
 */
static ZyanStatus PerformBasicTests(ZyanString* string) {
    ZYAN_ASSERT(string);

    ZyanUSize size;
    ZYAN_CHECK(ZyanStringGetSize(string, &size));

    ZyanStringView view1 = ZYAN_DEFINE_STRING_VIEW("The quick brown fox jumps over the lazy dog");
    ZyanStringView view2 = ZYAN_DEFINE_STRING_VIEW("big ");

    ZYAN_CHECK(ZyanStringAppend(string, &view1));
    PrintString(string);

    ZYAN_CHECK(ZyanStringInsert(string, 4, &view2));
    PrintString(string);

    ZYAN_CHECK(ZyanStringSetChar(string, 7, ','));
    PrintString(string);

    return ZYAN_STATUS_SUCCESS;
}

/**
 * Performs basic tests on a string that dynamically manages memory.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestDynamic(void) {
    ZyanString string;

    ZYAN_CHECK(ZyanStringInit(&string, 10));

    ZYAN_CHECK(PerformBasicTests(&string));

    return ZyanStringDestroy(&string);
}

/**
 * Performs basic tests on a string that uses a static buffer.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestStatic(void) {
    static char buffer[50];
    ZyanString string;

    ZYAN_CHECK(ZyanStringInitCustomBuffer(&string, buffer, sizeof(buffer)));

    ZYAN_CHECK(PerformBasicTests(&string));

    return ZyanStringDestroy(&string);
}

/* ---------------------------------------------------------------------------------------------- */
/* Custom allocator                                                                               */
/* ---------------------------------------------------------------------------------------------- */

static ZyanStatus AllocatorAllocate(
    ZyanAllocator* allocator, void** p, ZyanUSize element_size, ZyanUSize n) {
    ZYAN_ASSERT(allocator);
    ZYAN_ASSERT(p);
    ZYAN_ASSERT(element_size);
    ZYAN_ASSERT(n);

    ZYAN_UNUSED(allocator);

    *p = ZYAN_MALLOC(element_size * n);
    if (!*p) {
        return ZYAN_STATUS_NOT_ENOUGH_MEMORY;
    }

    return ZYAN_STATUS_SUCCESS;
}

static ZyanStatus AllocatorReallocate(
    ZyanAllocator* allocator, void** p, ZyanUSize element_size, ZyanUSize n) {
    ZYAN_ASSERT(allocator);
    ZYAN_ASSERT(p);
    ZYAN_ASSERT(element_size);
    ZYAN_ASSERT(n);

    ZYAN_UNUSED(allocator);

    void* const x = ZYAN_REALLOC(*p, element_size * n);
    if (!x) {
        return ZYAN_STATUS_NOT_ENOUGH_MEMORY;
    }
    *p = x;

    return ZYAN_STATUS_SUCCESS;
}

static ZyanStatus AllocatorDeallocate(
    ZyanAllocator* allocator, void* p, ZyanUSize element_size, ZyanUSize n) {
    ZYAN_ASSERT(allocator);
    ZYAN_ASSERT(p);
    ZYAN_ASSERT(element_size);
    ZYAN_ASSERT(n);

    ZYAN_UNUSED(allocator);
    ZYAN_UNUSED(element_size);
    ZYAN_UNUSED(n);

    ZYAN_FREE(p);

    return ZYAN_STATUS_SUCCESS;
}

/* ---------------------------------------------------------------------------------------------- */

/**
 * Performs basic tests on a vector that dynamically manages memory using a custom
 * allocator and modified growth-factor/shrink-threshold.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestAllocator(void) {
    ZyanAllocator allocator;
    ZYAN_CHECK(
        ZyanAllocatorInit(&allocator, AllocatorAllocate, AllocatorReallocate, AllocatorDeallocate));

    ZyanString string;
    ZYAN_CHECK(ZyanStringInitEx(&string, 20, &allocator, 10, 0));

    ZYAN_CHECK(PerformBasicTests(&string));

    return ZyanStringDestroy(&string);
}

/* ---------------------------------------------------------------------------------------------- */

/* ============================================================================================== */
/* Entry point                                                                                    */
/* ============================================================================================== */

int main(void) {
    if (!ZYAN_SUCCESS(TestDynamic())) {
        return EXIT_FAILURE;
    }
    if (!ZYAN_SUCCESS(TestStatic())) {
        return EXIT_FAILURE;
    }
    if (!ZYAN_SUCCESS(TestAllocator())) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

/* ============================================================================================== */
