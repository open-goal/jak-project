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
 * @brief   Tests the `ZyanVector` implementation.
 */

#include <time.h>
#include <gtest/gtest.h>
#include <Zycore/Comparison.h>
#include <Zycore/Vector.h>

/* ============================================================================================== */
/* Fixtures                                                                                       */
/* ============================================================================================== */

/* ---------------------------------------------------------------------------------------------- */
/* VectorTestBase                                                                                 */
/* ---------------------------------------------------------------------------------------------- */

/**
 * @brief   Implements a fixture-class that provides an initialized `ZyanVector` instance for
 *          `ZyanU64` values.
 */
class VectorTestBase : public ::testing::TestWithParam<bool>
{
protected:
    static const ZyanUSize m_test_size = 100;
    ZyanBool               m_has_fixed_capacity;
    ZyanVector             m_vector;
    std::vector<ZyanU64>   m_buffer;
protected:
    void SetUp() override
    {
        m_has_fixed_capacity = GetParam();

        if (!m_has_fixed_capacity)
        {
            ASSERT_EQ(ZyanVectorInit(&m_vector, sizeof(ZyanU64), m_test_size, 
                reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), ZYAN_STATUS_SUCCESS);
        } else
        {
            m_buffer.reserve(m_test_size);
            ASSERT_EQ(ZyanVectorInitCustomBuffer(&m_vector, sizeof(ZyanU64), m_buffer.data(),
                m_test_size, reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), 
                ZYAN_STATUS_SUCCESS);
        }
    }

    void TearDown() override
    {
        EXPECT_EQ(ZyanVectorDestroy(&m_vector), ZYAN_STATUS_SUCCESS);
    }
};

/* ---------------------------------------------------------------------------------------------- */
/* VectorTestFilled                                                                               */
/* ---------------------------------------------------------------------------------------------- */

/**
 * @brief   Implements a fixture-class that provides an initialized `ZyanVector` instance which
 *          is filled with `ZyanU64` values from 0..100.
 */
class VectorTestFilled : public VectorTestBase
{
protected:
    void SetUp() override
    {
        VectorTestBase::SetUp();

        if (m_has_fixed_capacity)
        {
            m_buffer.resize(m_test_size);
        }
        for (ZyanU64 i = 0; i < m_test_size; ++i)
        {
            ASSERT_EQ(ZyanVectorPushBack(&m_vector, &i), ZYAN_STATUS_SUCCESS);
        }
    }
};

/* ---------------------------------------------------------------------------------------------- */

/* ============================================================================================== */
/* Helper functions                                                                               */
/* ============================================================================================== */

/**
 * @brief   A dummy constructor for `ZyanU64` objects.
 *
 * @param   object  A pointer to the object.
 *
 * @return  A zyan status code.
 */
static ZyanStatus InitZyanU64(ZyanU64* object)
{
    *object = 1337;
    return ZYAN_STATUS_SUCCESS;
}

/**
 * @brief   A dummy destructor for `ZyanU16` objects.
 *
 * @param   object  A pointer to the object.
 *
 * @return  A zyan status code.
 */
static ZyanStatus FreeZyanU16(ZyanU16* object)
{
    *object = 0;
    return ZYAN_STATUS_SUCCESS;
}

/* ============================================================================================== */
/* Tests                                                                                          */
/* ============================================================================================== */

TEST(VectorTest, InitBasic)
{
    ZyanVector vector;

    ASSERT_EQ(ZyanVectorInit(&vector, sizeof(ZyanU64), 0, 
        reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(vector.allocator, ZyanAllocatorDefault());
    EXPECT_EQ(vector.growth_factor, ZYAN_VECTOR_DEFAULT_GROWTH_FACTOR);
    EXPECT_EQ(vector.shrink_threshold, ZYAN_VECTOR_DEFAULT_SHRINK_THRESHOLD);
    EXPECT_EQ(vector.size, static_cast<ZyanUSize>(0));
    EXPECT_EQ(vector.capacity, static_cast<ZyanUSize>(ZYAN_VECTOR_MIN_CAPACITY));
    EXPECT_EQ(vector.element_size, sizeof(ZyanU64));
    EXPECT_NE(vector.data, ZYAN_NULL);
    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);

    // Custom capacity
    EXPECT_EQ(ZyanVectorInit(&vector, sizeof(ZyanU16), 10, 
        reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(vector.capacity, static_cast<ZyanUSize>(ZYAN_MAX(ZYAN_VECTOR_MIN_CAPACITY, 10)));
    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);
}

TEST(VectorTest, InitAdvanced)
{
    ZyanVector vector;

    ASSERT_EQ(ZyanVectorInitEx(&vector, sizeof(ZyanU16), 0, 
        reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL), ZyanAllocatorDefault(), 1, 0),
        ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(vector.allocator, ZyanAllocatorDefault());
    EXPECT_EQ(vector.growth_factor, 1);
    EXPECT_EQ(vector.shrink_threshold, 0);
    EXPECT_EQ(vector.size, static_cast<ZyanUSize>(0));
    EXPECT_EQ(vector.capacity, static_cast<ZyanUSize>(ZYAN_VECTOR_MIN_CAPACITY));
    EXPECT_EQ(vector.element_size, sizeof(ZyanU16));
    EXPECT_NE(vector.data, ZYAN_NULL);
    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);

    // Custom capacity
    EXPECT_EQ(ZyanVectorInitEx(&vector, sizeof(ZyanU16), 10, 
        reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL), ZyanAllocatorDefault(), 1, 0),
        ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(vector.capacity, static_cast<ZyanUSize>(ZYAN_MAX(ZYAN_VECTOR_MIN_CAPACITY, 10)));
    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);
}

TEST(VectorTest, InitCustomBuffer)
{
    ZyanVector vector;

    ZyanU16 buffer[32];
    EXPECT_EQ(ZyanVectorInitCustomBuffer(&vector, sizeof(ZyanU16), &buffer, 0, 
        reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), ZYAN_STATUS_INVALID_ARGUMENT);
    ASSERT_EQ(ZyanVectorInitCustomBuffer(&vector, sizeof(ZyanU16), &buffer,
        ZYAN_ARRAY_LENGTH(buffer), reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), 
        ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(vector.allocator, ZYAN_NULL);
    EXPECT_EQ(vector.growth_factor, 1);
    EXPECT_EQ(vector.shrink_threshold, 0);
    EXPECT_EQ(vector.size, static_cast<ZyanUSize>(0));
    EXPECT_EQ(vector.capacity, ZYAN_ARRAY_LENGTH(buffer));
    EXPECT_EQ(vector.element_size, sizeof(ZyanU16));
    EXPECT_EQ(vector.data, &buffer);
    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);
}

TEST(VectorTest, Destructor)
{
    ZyanVector vector;

    ZyanU16 buffer[16];
    ASSERT_EQ(ZyanVectorInitCustomBuffer(&vector, sizeof(ZyanU16), &buffer,
        ZYAN_ARRAY_LENGTH(buffer), reinterpret_cast<ZyanMemberProcedure>(&FreeZyanU16)), 
        ZYAN_STATUS_SUCCESS);
    
    for (ZyanUSize i = 0; i < ZYAN_ARRAY_LENGTH(buffer); ++i)
    {
        const auto element = static_cast<ZyanU16>(i) + 0;
        ASSERT_EQ(ZyanVectorPushBack(&vector, &element), ZYAN_STATUS_SUCCESS);
        ASSERT_EQ(buffer[i], element);
    }

    ASSERT_EQ(ZyanVectorPopBack(&vector), ZYAN_STATUS_SUCCESS);
    ASSERT_EQ(buffer[15], 0);

    ASSERT_EQ(ZyanVectorDeleteRange(&vector, 12, 3), ZYAN_STATUS_SUCCESS);
    ASSERT_EQ(buffer[12], 0);
    ASSERT_EQ(buffer[13], 0);
    ASSERT_EQ(buffer[14], 0);

    ASSERT_EQ(ZyanVectorClear(&vector), ZYAN_STATUS_SUCCESS);
    for (ZyanUSize i : buffer)
    {
        ASSERT_EQ(i, 0);
    }

    for (ZyanUSize i = 0; i < ZYAN_ARRAY_LENGTH(buffer); ++i)
    {
        const auto element = static_cast<ZyanU16>(i) + 1;
        ASSERT_EQ(ZyanVectorPushBack(&vector, &element), ZYAN_STATUS_SUCCESS);
        ASSERT_EQ(buffer[i], element);
    }

    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);
    for (ZyanUSize i : buffer)
    {
        ASSERT_EQ(i, 0);
    }
}

TEST(VectorTest, TestGrowingAndShrinking)
{
    ZyanVector vector;

    ASSERT_EQ(ZyanVectorInit(&vector, sizeof(ZyanU64), 0,
        reinterpret_cast<ZyanMemberProcedure>(ZYAN_NULL)), ZYAN_STATUS_SUCCESS);

    for (ZyanU64 i = 0; i < 100; ++i)
    {
        ZyanUSize expected_capacity = vector.capacity;
        if (expected_capacity < (i + 1))
        {
            expected_capacity = (expected_capacity + 1) * vector.growth_factor;
        }
        ASSERT_EQ(ZyanVectorPushBack(&vector, &i), ZYAN_STATUS_SUCCESS);
        ASSERT_EQ(vector.capacity, expected_capacity);
    }

    for (ZyanU64 i = 100; i > 0; --i)
    {
        const auto index = static_cast<ZyanUSize>(i - 1);

        ZyanUSize expected_capacity = vector.capacity;
        if ((vector.shrink_threshold != 0) && (index * vector.shrink_threshold < vector.capacity))
        {
            expected_capacity = ZYAN_MAX(1, (ZyanUSize)(index * vector.growth_factor));
        }
        ASSERT_EQ(ZyanVectorDelete(&vector, index), ZYAN_STATUS_SUCCESS);
        ASSERT_EQ(vector.capacity, expected_capacity);
    }

    EXPECT_EQ(ZyanVectorDestroy(&vector), ZYAN_STATUS_SUCCESS);
}

TEST_P(VectorTestFilled, ElementAccess)
{
    static const ZyanU64 element_in = 1337;
    const ZyanU64* element_dummy;
    ZyanU64* element_out_mut;

    EXPECT_EQ(ZyanVectorSet(&m_vector, m_vector.size, &element_in),
        ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorSet(&m_vector, m_vector.size - 1, &element_in),
        ZYAN_STATUS_SUCCESS);

    EXPECT_EQ(ZyanVectorGetPointer(&m_vector, m_vector.size,
        reinterpret_cast<const void**>(&element_dummy)), ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, m_vector.size - 1), element_in);

    EXPECT_EQ(ZyanVectorGetPointerMutable(&m_vector, m_vector.size,
        reinterpret_cast<void**>(&element_out_mut)), ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorGetPointerMutable(&m_vector, m_vector.size - 1,
        reinterpret_cast<void**>(&element_out_mut)), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(*element_out_mut, element_in);
    *element_out_mut = 42;
    EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, m_vector.size - 1), 42);

    if (m_has_fixed_capacity)
    {
        EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, m_vector.size - 1),
            m_buffer[m_vector.size - 1]);
    }
}

TEST_P(VectorTestFilled, PushPop)
{
    static const ZyanU64 element_in = 1337;
    const ZyanUSize size = m_vector.size;

    if (!m_has_fixed_capacity)
    {
        EXPECT_EQ(ZyanVectorPushBack(&m_vector, &element_in), ZYAN_STATUS_SUCCESS);
        EXPECT_EQ(m_vector.size, size + 1);
        EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, size), element_in);
        EXPECT_EQ(ZyanVectorPopBack(&m_vector), ZYAN_STATUS_SUCCESS);
        EXPECT_EQ(m_vector.size, size);
    } else
    {
        EXPECT_EQ(ZyanVectorPushBack(&m_vector, &element_in), ZYAN_STATUS_INSUFFICIENT_BUFFER_SIZE);
        EXPECT_EQ(m_vector.size, size);
        EXPECT_EQ(ZyanVectorPopBack(&m_vector), ZYAN_STATUS_SUCCESS);
        EXPECT_EQ(m_vector.size, size - 1);
        EXPECT_EQ(ZyanVectorPushBack(&m_vector, &element_in), ZYAN_STATUS_SUCCESS);
        EXPECT_EQ(m_vector.size, size);
        EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, size - 1), element_in);
    }
}

TEST_P(VectorTestFilled, Insert)
{
    static const ZyanU64 elements[4] =
    {
        1337, 1338, 1339, 1340
    };
    const ZyanUSize count = ZYAN_ARRAY_LENGTH(elements);

    if (m_has_fixed_capacity)
    {
        const ZyanUSize size_temp = m_vector.size;
        EXPECT_EQ(ZyanVectorInsertRange(&m_vector, size_temp / 2, &elements, count),
            ZYAN_STATUS_INSUFFICIENT_BUFFER_SIZE);
        EXPECT_EQ(ZyanVectorResize(&m_vector, size_temp - count), ZYAN_STATUS_SUCCESS);
        EXPECT_EQ(m_vector.size, size_temp - count);
    }

    const ZyanUSize size = m_vector.size;
    const ZyanUSize half = (size / 2);

    EXPECT_EQ(ZyanVectorInsertRange(&m_vector, half, &elements, ZYAN_ARRAY_LENGTH(elements)),
        ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(m_vector.size, size + count);
    for (ZyanUSize i = 0; i < m_vector.size; ++i)
    {
        const ZyanU64 element_out = ZYAN_VECTOR_GET(ZyanU64, &m_vector, i);

        if ((i >= half) && (i < half + count))
        {
            EXPECT_EQ(element_out, elements[i - half]);
        } else
        if (i < half)
        {
            EXPECT_EQ(element_out, i);
        } else
        {
            EXPECT_EQ(element_out, i - count);
        }
    }
}

TEST_P(VectorTestFilled, Delete)
{
    EXPECT_EQ(ZyanVectorDeleteRange(&m_vector, m_vector.size, 1), ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorDeleteRange(&m_vector, 1, m_vector.size), ZYAN_STATUS_OUT_OF_RANGE);

    const ZyanUSize size = m_vector.size;
    const ZyanUSize half = (size / 2);
    const ZyanUSize count = (half / 2);

    EXPECT_EQ(ZyanVectorDeleteRange(&m_vector, half, count), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(m_vector.size, size - count);
    for (ZyanUSize i = 0; i < m_vector.size; ++i)
    {
        const ZyanU64 element_out = ZYAN_VECTOR_GET(ZyanU64, &m_vector, i);

        if ((i >= half) && (i < half + count))
        {
            EXPECT_EQ(element_out, i + count);
        } else
        if (i < half)
        {
            EXPECT_EQ(element_out, i);
        } else
        {
            EXPECT_EQ(element_out, i - count);
        }
    }
}

TEST_P(VectorTestFilled, Find)
{
    ZyanISize index;
    ZyanU64 element_in = m_vector.size / 2;
    EXPECT_EQ(ZyanVectorFind(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanEqualityComparison>(&ZyanEqualsNumeric64)), ZYAN_STATUS_TRUE);
    EXPECT_EQ(static_cast<ZyanU64>(index), element_in);

    element_in = 1337;
    EXPECT_EQ(ZyanVectorFind(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanEqualityComparison>(&ZyanEqualsNumeric64)), ZYAN_STATUS_FALSE);
    EXPECT_EQ(index, -1);

    // Edge cases
    EXPECT_EQ(ZyanVectorFindEx(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanEqualityComparison>(&ZyanEqualsNumeric64), 0, 0),
        ZYAN_STATUS_FALSE);
    EXPECT_EQ(ZyanVectorFindEx(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanEqualityComparison>(&ZyanEqualsNumeric64), 0, m_vector.size + 1),
        ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorFindEx(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanEqualityComparison>(&ZyanEqualsNumeric64), 1, m_vector.size),
        ZYAN_STATUS_OUT_OF_RANGE);
}

TEST_P(VectorTestBase, BinarySearch)
{
    EXPECT_EQ(ZyanVectorReserve(&m_vector, 100), ZYAN_STATUS_SUCCESS);
    for (ZyanUSize i = 0; i < 100; ++i)
    {
        const ZyanU64 element = rand() % 100;

        ZyanUSize index;
        const ZyanStatus status = ZyanVectorBinarySearch(&m_vector, &element, &index,
            reinterpret_cast<ZyanComparison>(&ZyanCompareNumeric64));
        EXPECT_EQ(ZYAN_SUCCESS(status), ZYAN_TRUE);
        EXPECT_EQ(ZyanVectorInsert(&m_vector, index, &element), ZYAN_STATUS_SUCCESS);
    }
    EXPECT_EQ(m_vector.size, static_cast<ZyanUSize>(100));

    ZyanU64 element_out = ZYAN_VECTOR_GET(ZyanU64, &m_vector, 0);
    for (ZyanUSize i = 1; i < m_vector.size; ++i)
    {
        const ZyanU64 value = element_out;
        element_out = ZYAN_VECTOR_GET(ZyanU64, &m_vector, i);
        EXPECT_GE(element_out, value);
    }

    // Edge cases
    const ZyanU64 element_in = 1337;
    ZyanUSize index;
    EXPECT_EQ(ZyanVectorBinarySearchEx(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanComparison>(&ZyanCompareNumeric64), 0, 101),
        ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorBinarySearchEx(&m_vector, &element_in, &index,
        reinterpret_cast<ZyanComparison>(&ZyanCompareNumeric64), 1, 100),
        ZYAN_STATUS_OUT_OF_RANGE);
}

TEST_P(VectorTestBase, Emplace)
{
    ZyanU64* element_new;

    for (ZyanUSize i = 0; i < 10; ++i)
    {
        EXPECT_EQ(ZyanVectorEmplace(&m_vector, reinterpret_cast<void**>(&element_new), 
            reinterpret_cast<ZyanMemberFunction>(ZYAN_NULL)), ZYAN_STATUS_SUCCESS);
        *element_new = i;
    }
    EXPECT_EQ(m_vector.size, static_cast<ZyanUSize>(10));

    for (ZyanUSize i = 0; i < m_vector.size; ++i)
    {
        EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, i), i);
    }

    EXPECT_EQ(ZyanVectorEmplaceEx(&m_vector, 5, reinterpret_cast<void**>(&element_new),
        reinterpret_cast<ZyanMemberFunction>(&InitZyanU64)), ZYAN_STATUS_SUCCESS);
    EXPECT_EQ(*element_new, 1337);
    EXPECT_EQ(ZYAN_VECTOR_GET(ZyanU64, &m_vector, 5), 1337);
}

TEST_P(VectorTestFilled, SwapElements)
{
    EXPECT_EQ(m_vector.capacity, m_vector.size);

    // Edge cases
    EXPECT_EQ(ZyanVectorSwapElements(&m_vector, 0, m_vector.size), ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorSwapElements(&m_vector, m_vector.size, 0), ZYAN_STATUS_OUT_OF_RANGE);
    EXPECT_EQ(ZyanVectorSwapElements(&m_vector, 0, m_vector.size - 1),
        ZYAN_STATUS_INSUFFICIENT_BUFFER_SIZE);

    // Free space for the temporary element
    EXPECT_EQ(ZyanVectorPopBack(&m_vector), ZYAN_STATUS_SUCCESS);

    // Retrieve element pointers
    const ZyanU64* element_first;
    EXPECT_EQ(ZyanVectorGetPointer(&m_vector, 0, reinterpret_cast<const void**>(&element_first)),
        ZYAN_STATUS_SUCCESS);
    const ZyanU64* element_second;
    EXPECT_EQ(ZyanVectorGetPointer(&m_vector, m_vector.size - 1, 
        reinterpret_cast<const void**>(&element_second)), ZYAN_STATUS_SUCCESS);

    const ZyanU64 values_before[2] = { *element_first, *element_second };
    EXPECT_EQ(ZyanVectorSwapElements(&m_vector, 0, m_vector.size - 1), ZYAN_STATUS_SUCCESS);
    const ZyanU64 values_after [2] = { *element_first, *element_second };

    EXPECT_EQ(values_before[0], values_after[1]);
    EXPECT_EQ(values_before[1], values_after[0]);
}

INSTANTIATE_TEST_SUITE_P(Param, VectorTestBase, ::testing::Values(false, true));
INSTANTIATE_TEST_SUITE_P(Param, VectorTestFilled, ::testing::Values(false, true));

/* ---------------------------------------------------------------------------------------------- */

/* ============================================================================================== */
/* Entry point                                                                                    */
/* ============================================================================================== */

int main(int argc, char **argv)
{
    time_t t;
    srand(static_cast<unsigned>(time(&t)));

    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

/* ============================================================================================== */
