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
 * Demonstrates the `ZyanVector` implementation.
 */

#include <inttypes.h>
#include <stdio.h>
#include <time.h>
#include <Zycore/Allocator.h>
#include <Zycore/Defines.h>
#include <Zycore/LibC.h>
#include <Zycore/Types.h>
#include <Zycore/Vector.h>

/* ============================================================================================== */
/* Enums and types                                                                                */
/* ============================================================================================== */

/**
 * Defines the `TestStruct` struct that represents a single element in the vector.
 */
typedef struct TestStruct_
{
    ZyanU32 u32;
    ZyanU64 u64;
    float f;
} TestStruct;

/* ============================================================================================== */
/* Helper functions                                                                               */
/* ============================================================================================== */

/**
 * Initializes the given `TestStruct` struct.
 *
 * @param   data    A pointer to the `TestStruct` struct.
 * @param   n       The number to initialize the struct with.
 */
static void InitTestdata(TestStruct* data, ZyanU32 n)
{
    ZYAN_ASSERT(data);

    data->u32 = n;
    data->u64 = n;
    data->f   = (float)n;
}

/* ============================================================================================== */
/* Tests                                                                                          */
/* ============================================================================================== */

/* ---------------------------------------------------------------------------------------------- */
/* Basic tests                                                                                    */
/* ---------------------------------------------------------------------------------------------- */

/**
 * Performs some basic test on the given `ZyanVector` instance.
 *
 * @param   vector  A pointer to the `ZyanVector` instance.
 *
 * @return  A zyan status code.
 */
static ZyanStatus PerformBasicTests(ZyanVector* vector)
{
    ZYAN_ASSERT(vector);

    static       TestStruct  e_v;
    static const TestStruct* e_p;

    // Insert `20` elements. The vector automatically manages its size
    for (ZyanU32 i = 0; i < 20; ++i)
    {
        InitTestdata(&e_v, i);
        ZYAN_CHECK(ZyanVectorPushBack(vector, &e_v));
    }

    // Remove elements `#05..#09`
    ZYAN_CHECK(ZyanVectorDeleteRange(vector, 5, 5));

    // Insert a new element at index `#05`
    InitTestdata(&e_v, 12345678);
    ZYAN_CHECK(ZyanVectorInsert(vector, 5, &e_v));

    // Change value of element `#15`
    InitTestdata(&e_v, 87654321);
    ZYAN_CHECK(ZyanVectorSet(vector, 10, &e_v));

    // Print `u64` of all vector elements
    ZyanUSize value;
    ZYAN_CHECK(ZyanVectorGetSize(vector, &value));
    puts("ELEMENTS");
    for (ZyanUSize i = 0;  i < value; ++i)
    {
        ZYAN_CHECK(ZyanVectorGetPointer(vector, i, (const void**)&e_p));
        printf("  Element #%02" PRIuPTR ": %08" PRIu64 "\n", i, e_p->u64);
    }

    // Print infos
    puts("INFO");
    printf("  Size       : %08" PRIuPTR "\n", value);
    ZYAN_CHECK(ZyanVectorGetCapacity(vector, &value));
    printf("  Capacity   : %08" PRIuPTR "\n\n", value);

    return ZYAN_STATUS_SUCCESS;
}

/**
 * A dummy comparison function for the `TestStruct` that uses the `u32` field as key
 * value.
 *
 * @param   left    A pointer to the first element.
 * @param   right   A pointer to the second element.
 *
 * @return  Returns values in the following range:
 *          `left == right -> result == 0`
 *          `left <  right -> result  < 0`
 *          `left >  right -> result  > 0`
 */
static ZyanI32 TestDataComparison(const TestStruct* left, const TestStruct* right)
{
    ZYAN_ASSERT(left);
    ZYAN_ASSERT(right);

    if (left->u32 < right->u32)
    {
        return -1;
    }
    if (left->u32 > right->u32)
    {
        return  1;
    }
    return 0;
}

/**
 * Tests the binary-search functionality of the given `ZyanVector` instance.
 *
 * @param   vector  A pointer to the `ZyanVector` instance.
 *
 * @return  A zyan status code.
 */
static ZyanStatus PerformBinarySearchTest(ZyanVector* vector)
{
    ZYAN_ASSERT(vector);

    static       TestStruct  e_v;
    static const TestStruct* e_p;

    ZyanUSize value;
    ZYAN_CHECK(ZyanVectorGetCapacity(vector, &value));

    // Create a sorted test vector
    for (ZyanUSize i = 0; i < value; ++i)
    {
        const ZyanU32 n = rand() % 100;
        InitTestdata(&e_v, n);

        ZyanUSize found_index;
        ZYAN_CHECK(ZyanVectorBinarySearch(vector, &e_v, &found_index,
            (ZyanComparison)&TestDataComparison));
        ZYAN_CHECK(ZyanVectorInsert(vector, found_index, &e_v));
    }

    // Print `u32` of all vector elements
    ZYAN_CHECK(ZyanVectorGetSize(vector, &value));
    puts("ELEMENTS");
    for (ZyanUSize i = 0;  i < value; ++i)
    {
        ZYAN_CHECK(ZyanVectorGetPointer(vector, i, (const void**)&e_p));
        printf("  Element #%02" PRIuPTR ": %08" PRIu32 "\n", i, e_p->u32);
    }

    return ZYAN_STATUS_SUCCESS;
}

/**
 * Performs basic tests on a vector that dynamically manages memory.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestDynamic(void)
{
    // Initialize vector with a base capacity of `10` elements
    ZyanVector vector;
    ZYAN_CHECK(ZyanVectorInit(&vector, sizeof(TestStruct), 10, ZYAN_NULL));

    ZYAN_CHECK(PerformBasicTests(&vector));
    ZYAN_CHECK(ZyanVectorClear(&vector));
    ZYAN_CHECK(ZyanVectorReserve(&vector, 20));
    ZYAN_CHECK(PerformBinarySearchTest(&vector));

    // Cleanup
    return ZyanVectorDestroy(&vector);
}

/**
 * Performs basic tests on a vector that uses a static buffer.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestStatic(void)
{
    static TestStruct buffer[20];

    // Initialize vector to use a static buffer with a total capacity of `20` elements.
    ZyanVector vector;
    ZYAN_CHECK(ZyanVectorInitCustomBuffer(&vector, sizeof(TestStruct), buffer,
        ZYAN_ARRAY_LENGTH(buffer), ZYAN_NULL));

    // Compare elements
    ZyanUSize size;
    ZYAN_CHECK(ZyanVectorGetSize(&vector, &size));
    for (ZyanUSize i = 0;  i < size; ++i)
    {
        static TestStruct* element;
        ZYAN_CHECK(ZyanVectorGetPointer(&vector, i, (const void**)&element));
        if (element->u64 != buffer[i].u64)
        {
            return ZYAN_STATUS_INVALID_OPERATION;
        }
    }

    ZYAN_CHECK(PerformBasicTests(&vector));
    ZYAN_CHECK(ZyanVectorClear(&vector));
    ZYAN_CHECK(PerformBinarySearchTest(&vector));

    // Cleanup
    return ZyanVectorDestroy(&vector);
}

/* ---------------------------------------------------------------------------------------------- */
/* Custom allocator                                                                               */
/* ---------------------------------------------------------------------------------------------- */

static ZyanStatus AllocatorAllocate(ZyanAllocator* allocator, void** p, ZyanUSize element_size,
    ZyanUSize n)
{
    ZYAN_ASSERT(allocator);
    ZYAN_ASSERT(p);
    ZYAN_ASSERT(element_size);
    ZYAN_ASSERT(n);

    ZYAN_UNUSED(allocator);

    *p = ZYAN_MALLOC(element_size * n);
    if (!*p)
    {
        return ZYAN_STATUS_NOT_ENOUGH_MEMORY;
    }

    return ZYAN_STATUS_SUCCESS;
}

static ZyanStatus AllocatorReallocate(ZyanAllocator* allocator, void** p, ZyanUSize element_size,
    ZyanUSize n)
{
    ZYAN_ASSERT(allocator);
    ZYAN_ASSERT(p);
    ZYAN_ASSERT(element_size);
    ZYAN_ASSERT(n);

    ZYAN_UNUSED(allocator);

    void* const x = ZYAN_REALLOC(*p, element_size * n);
    if (!x)
    {
        return ZYAN_STATUS_NOT_ENOUGH_MEMORY;
    }
    *p = x;

    return ZYAN_STATUS_SUCCESS;
}

static ZyanStatus AllocatorDeallocate(ZyanAllocator* allocator, void* p, ZyanUSize element_size,
    ZyanUSize n)
{
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
static ZyanStatus TestAllocator(void)
{
    ZyanAllocator allocator;
    ZYAN_CHECK(ZyanAllocatorInit(&allocator, &AllocatorAllocate, &AllocatorReallocate,
        &AllocatorDeallocate));

    // Initialize vector with a base capacity of `10` elements. Growth-factor is set to 10 and
    // dynamic shrinking is disabled
    ZyanVector vector;
    ZYAN_CHECK(ZyanVectorInitEx(&vector, sizeof(TestStruct), 5, ZYAN_NULL, &allocator,
        10, 0));

    static TestStruct  e_v;

    // Insert `10` elements. The vector automatically manages its size
    for (ZyanU32 i = 0; i < 10; ++i)
    {
        InitTestdata(&e_v, i);
        ZYAN_CHECK(ZyanVectorPushBack(&vector, &e_v));
    }

    // Check capacity
    ZyanUSize value;
    ZYAN_CHECK(ZyanVectorGetCapacity(&vector, &value));
    if (value != 60) // (5 + 1) * 10.0f
    {
        return ZYAN_STATUS_INVALID_OPERATION;
    }

    // Remove all elements
    ZYAN_CHECK(ZyanVectorClear(&vector));

    // Print infos
    puts("INFO");
    ZYAN_CHECK(ZyanVectorGetSize(&vector, &value));
    printf("  Size       : %08" PRIuPTR "\n", value);
    ZYAN_CHECK(ZyanVectorGetCapacity(&vector, &value));
    printf("  Capacity   : %08" PRIuPTR "\n\n", value);

    // Cleanup
    return ZyanVectorDestroy(&vector);
}

/* ---------------------------------------------------------------------------------------------- */

/* ============================================================================================== */
/* Entry point                                                                                    */
/* ============================================================================================== */

int main()
{
    time_t t;
    srand((unsigned)time(&t));

    if (!ZYAN_SUCCESS(TestDynamic()))
    {
        return EXIT_FAILURE;
    }
    if (!ZYAN_SUCCESS(TestStatic()))
    {
        return EXIT_FAILURE;
    }
    if (!ZYAN_SUCCESS(TestAllocator()))
    {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

/* ============================================================================================== */
