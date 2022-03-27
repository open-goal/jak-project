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
static ZyanStatus PerformBasicTests(ZyanString* string)
{
    ZYAN_ASSERT(string);
    ZYAN_UNUSED(string);



    return ZYAN_STATUS_SUCCESS;
}

/**
 * Performs basic tests on a string that dynamically manages memory.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestDynamic(void)
{
    PerformBasicTests(ZYAN_NULL);
    return ZYAN_STATUS_SUCCESS;
}

/**
 * Performs basic tests on a string that uses a static buffer.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestStatic(void)
{
    PerformBasicTests(ZYAN_NULL);
    return ZYAN_STATUS_SUCCESS;
}

/* ---------------------------------------------------------------------------------------------- */
/* Custom allocator                                                                               */
/* ---------------------------------------------------------------------------------------------- */

//static ZyanStatus AllocatorAllocate(ZyanAllocator* allocator, void** p, ZyanUSize element_size,
//    ZyanUSize n)
//{
//    ZYAN_ASSERT(allocator);
//    ZYAN_ASSERT(p);
//    ZYAN_ASSERT(element_size);
//    ZYAN_ASSERT(n);
//
//    ZYAN_UNUSED(allocator);
//
//    *p = ZYAN_MALLOC(element_size * n);
//    if (!*p)
//    {
//        return ZYAN_STATUS_NOT_ENOUGH_MEMORY;
//    }
//
//    return ZYAN_STATUS_SUCCESS;
//}
//
//static ZyanStatus AllocatorReallocate(ZyanAllocator* allocator, void** p, ZyanUSize element_size,
//    ZyanUSize n)
//{
//    ZYAN_ASSERT(allocator);
//    ZYAN_ASSERT(p);
//    ZYAN_ASSERT(element_size);
//    ZYAN_ASSERT(n);
//
//    ZYAN_UNUSED(allocator);
//
//    void* const x = ZYAN_REALLOC(*p, element_size * n);
//    if (!x)
//    {
//        return ZYAN_STATUS_NOT_ENOUGH_MEMORY;
//    }
//    *p = x;
//
//    return ZYAN_STATUS_SUCCESS;
//}
//
//static ZyanStatus AllocatorDeallocate(ZyanAllocator* allocator, void* p, ZyanUSize element_size,
//    ZyanUSize n)
//{
//    ZYAN_ASSERT(allocator);
//    ZYAN_ASSERT(p);
//    ZYAN_ASSERT(element_size);
//    ZYAN_ASSERT(n);
//
//    ZYAN_UNUSED(allocator);
//    ZYAN_UNUSED(element_size);
//    ZYAN_UNUSED(n);
//
//    ZYAN_FREE(p);
//
//    return ZYAN_STATUS_SUCCESS;
//}

/* ---------------------------------------------------------------------------------------------- */

/**
 * Performs basic tests on a vector that dynamically manages memory using a custom
 * allocator and modified growth-factor/shrink-threshold.
 *
 * @return  A zyan status code.
 */
static ZyanStatus TestAllocator(void)
{
    return ZYAN_STATUS_SUCCESS;
}

/* ---------------------------------------------------------------------------------------------- */

/* ============================================================================================== */
/* Entry point                                                                                    */
/* ============================================================================================== */

int main()
{
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
