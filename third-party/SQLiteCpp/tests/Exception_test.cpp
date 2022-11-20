/**
 * @file    Transaction_test.cpp
 * @ingroup tests
 * @brief   Test of a SQLite Transaction.
 *
 * Copyright (c) 2012-2020 Sebastien Rombauts (sebastien.rombauts@gmail.com)
 *
 * Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
 * or copy at http://opensource.org/licenses/MIT)
 */

#include <SQLiteCpp/Exception.h>

#include <gtest/gtest.h>

#include <string>

TEST(Exception, copy)
{
    const SQLite::Exception ex1("some error", 2);
    const SQLite::Exception ex2 = ex1;
    EXPECT_STREQ(ex1.what(), ex2.what());
    EXPECT_EQ(ex1.getErrorCode(), ex2.getErrorCode());
    EXPECT_EQ(ex1.getExtendedErrorCode(), ex2.getExtendedErrorCode());
}

// see http://eel.is/c++draft/exception#2 or http://www.cplusplus.com/reference/exception/exception/operator=/
// an assignment operator is expected to be avaiable
TEST(Exception, assignment)
{
    const char message[] = "some error";
    const SQLite::Exception ex1(message, 1);
    SQLite::Exception ex2("another error", 2);

    ex2 = ex1;

    EXPECT_STREQ(ex2.what(), message);
    EXPECT_EQ(ex2.getErrorCode(), 1);
    EXPECT_EQ(ex2.getExtendedErrorCode(), -1);
    EXPECT_STREQ(ex2.getErrorStr(), "SQL logic error");
}

TEST(Exception, throw_catch)
{
    const char message[] = "some error";
    try
    {
        throw SQLite::Exception(message);
    }
    catch (const std::runtime_error& ex)
    {
        EXPECT_STREQ(ex.what(), message);
    }
}


TEST(Exception, constructor)
{
    const char msg1[] = "some error";
    std::string msg2 = "another error";
    {
        const SQLite::Exception ex(msg1);
        EXPECT_STREQ(ex.what(), msg1);
        EXPECT_EQ(ex.getErrorCode(), -1);
        EXPECT_EQ(ex.getExtendedErrorCode(), -1);
        EXPECT_STREQ("unknown error", ex.getErrorStr());
    }
    {
        const SQLite::Exception ex(msg2);
        EXPECT_STREQ(ex.what(), msg2.c_str());
        EXPECT_EQ(ex.getErrorCode(), -1);
        EXPECT_EQ(ex.getExtendedErrorCode(), -1);
        EXPECT_STREQ("unknown error", ex.getErrorStr());
    }
    {
        const SQLite::Exception ex(msg1, 1);
        EXPECT_STREQ(ex.what(), msg1);
        EXPECT_EQ(ex.getErrorCode(), 1);
        EXPECT_EQ(ex.getExtendedErrorCode(), -1);
        EXPECT_STREQ(ex.getErrorStr(), "SQL logic error");
    }
    {
        const SQLite::Exception ex(msg2, 2);
        EXPECT_STREQ(ex.what(), msg2.c_str());
        EXPECT_EQ(ex.getErrorCode(), 2);
        EXPECT_EQ(ex.getExtendedErrorCode(), -1);
        EXPECT_STREQ(ex.getErrorStr(), "unknown error");
    }
}
