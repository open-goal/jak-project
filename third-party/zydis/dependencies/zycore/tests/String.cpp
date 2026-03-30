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
 * @brief   Tests the `ZyanString` implementation.
 */

#include <gtest/gtest.h>
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

/* ============================================================================================== */
/* Tests                                                                                          */
/* ============================================================================================== */

TEST(StringTest, InitDynamic)
{
    ZyanString string;

    ASSERT_EQ(ZyanStringInit(&string, 0), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(string.vector.allocator, ZyanAllocatorDefault());
    EXPECT_EQ(string.vector.growth_factor, ZYAN_STRING_DEFAULT_GROWTH_FACTOR);
    EXPECT_EQ(string.vector.shrink_threshold, ZYAN_STRING_DEFAULT_SHRINK_THRESHOLD);
    EXPECT_EQ(string.vector.size, static_cast<ZyanUSize>(1));
    EXPECT_EQ(string.vector.capacity, static_cast<ZyanUSize>(ZYAN_STRING_MIN_CAPACITY + 1));
    EXPECT_EQ(string.vector.element_size, sizeof(char));
    EXPECT_NE(string.vector.data, ZYAN_NULL);
    EXPECT_EQ(ZyanStringDestroy(&string), ZYAN_STATUS_SUCCESS);
}

TEST(StringTest, InitStatic)
{
    ZyanString string;

    static char buffer[32];
    EXPECT_EQ(ZyanStringInitCustomBuffer(&string, buffer, 0), ZYAN_STATUS_INVALID_ARGUMENT);
    ASSERT_EQ(ZyanStringInitCustomBuffer(&string, buffer, ZYAN_ARRAY_LENGTH(buffer)),
        ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(string.vector.allocator, ZYAN_NULL);
    EXPECT_EQ(string.vector.growth_factor, 1);
    EXPECT_EQ(string.vector.shrink_threshold, 0);
    EXPECT_EQ(string.vector.size, static_cast<ZyanUSize>(1));
    EXPECT_EQ(string.vector.capacity, ZYAN_ARRAY_LENGTH(buffer));
    EXPECT_EQ(string.vector.element_size, sizeof(char));
    EXPECT_EQ(string.vector.data, &buffer);
    EXPECT_EQ(ZyanStringDestroy(&string), ZYAN_STATUS_SUCCESS);
}

TEST(StringTest, InitAdvanced)
{
    ZyanString string;
    ZyanAllocator allocator;

    ASSERT_EQ(
        ZyanAllocatorInit(&allocator, AllocatorAllocate, AllocatorReallocate, AllocatorDeallocate),
        ZYAN_STATUS_SUCCESS);
    ASSERT_EQ(ZyanStringInitEx(&string, 0, &allocator, 1, 0), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(string.vector.allocator, &allocator);
    EXPECT_EQ(string.vector.growth_factor, 1);
    EXPECT_EQ(string.vector.shrink_threshold, 0);
    EXPECT_EQ(string.vector.size, static_cast<ZyanUSize>(1));
    EXPECT_EQ(string.vector.capacity, static_cast<ZyanUSize>(ZYAN_STRING_MIN_CAPACITY + 1));
    EXPECT_EQ(string.vector.element_size, sizeof(char));
    EXPECT_NE(string.vector.data, ZYAN_NULL);
    EXPECT_EQ(ZyanStringDestroy(&string), ZYAN_STATUS_SUCCESS);
}

/* ---------------------------------------------------------------------------------------------- */

/* ============================================================================================== */
/* Entry point                                                                                    */
/* ============================================================================================== */

int main(int argc, char **argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

/* ============================================================================================== */
