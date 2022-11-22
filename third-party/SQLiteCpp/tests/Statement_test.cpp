/**
 * @file    Statement_test.cpp
 * @ingroup tests
 * @brief   Test of a SQLiteCpp Statement.
 *
 * Copyright (c) 2012-2020 Sebastien Rombauts (sebastien.rombauts@gmail.com)
 *
 * Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
 * or copy at http://opensource.org/licenses/MIT)
 */

#include <SQLiteCpp/Database.h>
#include <SQLiteCpp/Statement.h>

#include <cstdint>   // for int64_t
#include <sqlite3.h> // for SQLITE_DONE

#include <gtest/gtest.h>

#include <cstdio>
#include <stdint.h>

#include <climits> // For INT_MAX

TEST(Statement, invalid)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());
    EXPECT_EQ(SQLite::OK, db.getExtendedErrorCode());

    // Compile a SQL query, but without any table in the database
    EXPECT_THROW(SQLite::Statement query(db, "SELECT * FROM test"), SQLite::Exception);
    EXPECT_EQ(SQLITE_ERROR, db.getErrorCode());
    EXPECT_EQ(SQLITE_ERROR, db.getExtendedErrorCode());

    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());
    EXPECT_EQ(SQLite::OK, db.getExtendedErrorCode());

    // Compile a SQL query with no parameter
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(2, query.getColumnCount ());
    EXPECT_FALSE(query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_EQ(SQLite::OK, query.getErrorCode());
    EXPECT_EQ(SQLite::OK, query.getExtendedErrorCode());
    EXPECT_THROW(query.isColumnNull(-1), SQLite::Exception);
    EXPECT_THROW(query.isColumnNull(0), SQLite::Exception);
    EXPECT_THROW(query.isColumnNull(1), SQLite::Exception);
    EXPECT_THROW(query.isColumnNull(2), SQLite::Exception);
    EXPECT_THROW(query.getColumn(-1), SQLite::Exception);
    EXPECT_THROW(query.getColumn(0), SQLite::Exception);
    EXPECT_THROW(query.getColumn(1), SQLite::Exception);
    EXPECT_THROW(query.getColumn(2), SQLite::Exception);

    query.reset();
    EXPECT_FALSE(query.hasRow());
    EXPECT_FALSE(query.isDone());

    query.executeStep();
    EXPECT_FALSE(query.hasRow());
    EXPECT_TRUE( query.isDone());
    query.reset();
    EXPECT_FALSE(query.hasRow());
    EXPECT_FALSE(query.isDone());

    query.reset();
    EXPECT_THROW(query.bind(-1, 123), SQLite::Exception);
    EXPECT_THROW(query.bind(0, 123), SQLite::Exception);
    EXPECT_THROW(query.bind(1, 123), SQLite::Exception);
    EXPECT_THROW(query.bind(2, 123), SQLite::Exception);
    EXPECT_THROW(query.bind(0, "abc"), SQLite::Exception);
    EXPECT_THROW(query.bind(0), SQLite::Exception);
    EXPECT_EQ(SQLITE_RANGE, db.getErrorCode());
    EXPECT_EQ(SQLITE_RANGE, db.getExtendedErrorCode());
    EXPECT_STREQ("column index out of range", db.getErrorMsg());
    EXPECT_EQ(SQLITE_RANGE, query.getErrorCode());
    EXPECT_EQ(SQLITE_RANGE, query.getExtendedErrorCode());
    EXPECT_STREQ("column index out of range", query.getErrorMsg());

    query.exec(); // exec() instead of executeStep() as there is no result
    EXPECT_THROW(query.isColumnNull(0), SQLite::Exception);
    EXPECT_THROW(query.getColumn(0), SQLite::Exception);

    EXPECT_THROW(query.exec(), SQLite::Exception); // exec() shall throw as it needs to be reseted

    // Add a first row
    EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, \"first\")"));
    EXPECT_EQ(1, db.getLastInsertRowid());
    EXPECT_EQ(1, db.getTotalChanges());

    query.reset();
    EXPECT_FALSE(query.hasRow());
    EXPECT_FALSE(query.isDone());

    EXPECT_THROW(query.exec(), SQLite::Exception); // exec() shall throw as it does not expect a result
}

#if __cplusplus >= 201103L || (defined(_MSC_VER) && _MSC_VER >= 1600)

SQLite::Statement StatementBuilder(SQLite::Database& aDb, const char* apQuery)
{
    return SQLite::Statement(aDb, apQuery);
}

TEST(Statement, moveConstructor)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE | SQLite::OPEN_CREATE);
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)"));
    EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, \"first\")"));
    EXPECT_EQ(1, db.getLastInsertRowid());

    SQLite::Statement query = StatementBuilder(db, "SELECT * FROM test");
    EXPECT_FALSE(query.getQuery().empty());
    EXPECT_FALSE(query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_EQ(2, query.getColumnCount());
    SQLite::Statement moved = std::move(query);
    EXPECT_TRUE(query.getQuery().empty());
    EXPECT_FALSE(moved.getQuery().empty());
    EXPECT_EQ(2, moved.getColumnCount());
    // Execute
    moved.executeStep();
    EXPECT_TRUE(moved.hasRow());
    EXPECT_FALSE(moved.isDone());
    EXPECT_FALSE(query.hasRow());
    EXPECT_FALSE(query.isDone());

    // Const statement lookup
    const auto const_query = std::move(moved);
    auto index = const_query.getColumnIndex("value");
    EXPECT_EQ(1, index);
    EXPECT_NO_THROW(const_query.getColumn(index));

    // Moved statements should throw
    EXPECT_THROW(query.getColumnIndex("value"), SQLite::Exception);
    EXPECT_THROW(query.getColumn(index), SQLite::Exception);
}

#endif

TEST(Statement, executeStep)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, double REAL)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a first row
    EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, \"first\", 123, 0.123)"));
    EXPECT_EQ(1, db.getLastInsertRowid());

    // Compile a SQL query
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());

    // Get the first row
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    const int64_t       id      = query.getColumn(0);
    const std::string   msg     = query.getColumn(1);
    const int           integer = query.getColumn(2);
    const int64_t       integer2= query.getColumn(2);
    const double        real    = query.getColumn(3);
    EXPECT_EQ(1,        id);
    EXPECT_EQ("first",  msg);
    EXPECT_EQ(123,      integer);
    EXPECT_EQ(123,      integer2);
    EXPECT_EQ(0.123,    real);

    // Step one more time to discover there is nothing more
    query.executeStep();
    EXPECT_FALSE(query.hasRow());
    EXPECT_TRUE (query.isDone()); // "done" is "the end"

    // Step after "the end" throw an exception
    EXPECT_THROW(query.executeStep(), SQLite::Exception);

    // Try to insert a new row with the same PRIMARY KEY: "UNIQUE constraint failed: test.id"
    SQLite::Statement insert(db, "INSERT INTO test VALUES (1, \"impossible\", 456, 0.456)");
    EXPECT_THROW(insert.executeStep(), SQLite::Exception);
    // in this case, reset() do throw again the same error
    EXPECT_THROW(insert.reset(), SQLite::Exception);

    // Try again to insert a new row with the same PRIMARY KEY (with an alternative method): "UNIQUE constraint failed: test.id"
    SQLite::Statement insert2(db, "INSERT INTO test VALUES (1, \"impossible\", 456, 0.456)");
    EXPECT_THROW(insert2.exec(), SQLite::Exception);
}

TEST(Statement, tryExecuteStep)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, double REAL)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a first row
    EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, \"first\", 123, 0.123)"));
    EXPECT_EQ(1, db.getLastInsertRowid());

    // Compile a SQL query
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());

    // Get the first row
    EXPECT_EQ(query.tryExecuteStep(), SQLITE_ROW);
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    const int64_t       id      = query.getColumn(0);
    const std::string   msg     = query.getColumn(1);
    const int           integer = query.getColumn(2);
    const int64_t       integer2= query.getColumn(2);
    const double        real    = query.getColumn(3);
    EXPECT_EQ(1,        id);
    EXPECT_EQ("first",  msg);
    EXPECT_EQ(123,      integer);
    EXPECT_EQ(123,      integer2);
    EXPECT_EQ(0.123,    real);

    // Step one more time to discover there is nothing more
    EXPECT_EQ(query.tryExecuteStep(), SQLITE_DONE);
    EXPECT_FALSE(query.hasRow());
    EXPECT_TRUE (query.isDone()); // "done" is "the end"

    // Try to insert a new row with the same PRIMARY KEY: "UNIQUE constraint failed: test.id"
    SQLite::Statement insert(db, "INSERT INTO test VALUES (1, \"impossible\", 456, 0.456)");
    EXPECT_EQ(insert.tryExecuteStep(), SQLITE_CONSTRAINT);
    // in this case, reset() do throw again the same error
    EXPECT_EQ(insert.tryReset(), SQLITE_CONSTRAINT);
}

TEST(Statement, bindings)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, double REAL)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Insertion with bindable parameters
    SQLite::Statement insert(db, "INSERT INTO test VALUES (NULL, ?, ?, ?)");

    // Compile a SQL query to check the results
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());

    // First row with text/int/double
    {
        const char* text = "first";
        const int integer = -123;
        const double dbl = 0.123;
        insert.bind(1, text);
        insert.bind(2, integer);
        insert.bind(3, dbl);
        EXPECT_EQ(insert.getExpandedSQL(), "INSERT INTO test VALUES (NULL, 'first', -123, 0.123)");
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ   (1,         query.getColumn(0).getInt64());
        EXPECT_STREQ("first",   query.getColumn(1).getText());
        EXPECT_EQ   (-123,      query.getColumn(2).getInt());
        EXPECT_EQ   (0.123,     query.getColumn(3).getDouble());
    }

    // reset() without clearbindings()
    insert.reset();

    // Second row with the same exact values because clearbindings() was not called
    {
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ   (2,         query.getColumn(0).getInt64());
        EXPECT_STREQ("first",   query.getColumn(1).getText());
        EXPECT_EQ   (-123,      query.getColumn(2).getInt());
        EXPECT_EQ   (0.123,     query.getColumn(3).getDouble());
    }

    // reset() with clearbindings() and no more bindings
    insert.reset();
    insert.clearBindings();

    // Third row with the all null values because clearbindings() was called
    {
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the resultw
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ   (3,     query.getColumn(0).getInt64());
        EXPECT_TRUE (query.isColumnNull(1));
        EXPECT_STREQ("",    query.getColumn(1).getText());
        EXPECT_TRUE (query.isColumnNull(2));
        EXPECT_EQ   (0,     query.getColumn(2).getInt());
        EXPECT_TRUE (query.isColumnNull(3));
        EXPECT_EQ   (0.0,   query.getColumn(3).getDouble());
    }

    // reset() with clearbindings() and new bindings
    insert.reset();
    insert.clearBindings();

    // Fourth row with string/int64/float
    {
        const std::string   fourth("fourth");
        const int64_t       int64 = 12345678900000LL;
        const float         float32 = 0.234f;
        insert.bind(1, fourth);
        insert.bind(2, int64);
        insert.bind(3, float32);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(4,                query.getColumn(0).getInt64());
        EXPECT_EQ(fourth,           query.getColumn(1).getText());
        EXPECT_EQ(12345678900000LL, query.getColumn(2).getInt64());
        EXPECT_EQ(0.234f,           query.getColumn(3).getDouble());
    }

    // reset() without clearbindings()
    insert.reset();

    // Fifth row with binary buffer and a null parameter
    {
        const char buffer[] = "binary";
        insert.bind(1, buffer, sizeof(buffer));
        insert.bind(2); // bind a NULL value
        EXPECT_EQ(1, insert.exec());

        // Check the result
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(5,                query.getColumn(0).getInt64());
        EXPECT_STREQ(buffer,        query.getColumn(1).getText());
        EXPECT_TRUE (query.isColumnNull(2));
        EXPECT_EQ(0,                query.getColumn(2).getInt());
        EXPECT_EQ(0.234f,           query.getColumn(3).getDouble());
    }


    // reset() without clearbindings()
    insert.reset();

    // Sixth row with uint32_t unsigned value and a long value (which is either a 32b int or a 64b int64_t)
    {
        const uint32_t  uint32 = 4294967295U;
        const int64_t   integer = -123;
        insert.bind(2, uint32);
        insert.bind(3, integer);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(6, query.getColumn(0).getInt64());
        EXPECT_EQ(4294967295U, query.getColumn(2).getUInt());
        EXPECT_EQ(-123, query.getColumn(3).getInt());
    }


    // reset() without clearbindings()
    insert.reset();

    // Seventh row using another variant of int64 type
    {
        const int64_t   int64 = 12345678900000LL;
        insert.bind(2, int64);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(7, query.getColumn(0).getInt64());
        EXPECT_EQ(12345678900000LL, query.getColumn(2).getInt64());
    }
}

TEST(Statement, bindNoCopy)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, txt1 TEXT, txt2 TEXT, binary BLOB)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Insertion with bindable parameters
    SQLite::Statement insert(db, "INSERT INTO test VALUES (NULL, ?, ?, ?)");

    // Compile a SQL query to check the results
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());

    // Insert one row with all variants of bindNoCopy()
    {
        const char*         txt1   = "first";
        const std::string   txt2   = "sec\0nd";
        const char          blob[] = {'b','l','\0','b'};
        insert.bindNoCopy(1, txt1);
        insert.bindNoCopy(2, txt2);
        insert.bindNoCopy(3, blob, sizeof(blob));
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(1, query.getColumn(0).getInt64());
        EXPECT_STREQ(txt1, query.getColumn(1).getText());
        EXPECT_EQ(0, memcmp(&txt2[0], &query.getColumn(2).getString()[0], txt2.size()));
        EXPECT_EQ(0, memcmp(blob, &query.getColumn(3).getString()[0], sizeof(blob)));
    }
}

TEST(Statement, bindByName)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, long INTEGER, double REAL)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Insertion with bindable parameters
    SQLite::Statement insert(db, "INSERT INTO test VALUES (NULL, @msg, @int, @long, @double)");

    // First row with text/int/double
    insert.bind("@msg",      "first");
    insert.bind("@int",      123);
    insert.bind("@long",      -123);
    insert.bind("@double",   0.123);
    EXPECT_EQ(1, insert.exec());
    EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

    // Compile a SQL query to check the result
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(5, query.getColumnCount());

    // Check the result
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_EQ   (1,         query.getColumn(0).getInt64());
    EXPECT_STREQ("first",   query.getColumn(1).getText());
    EXPECT_EQ   (123,       query.getColumn(2).getInt());
    EXPECT_EQ   (-123,      query.getColumn(3).getInt());
    EXPECT_EQ   (0.123,     query.getColumn(4).getDouble());

    // reset() with clearbindings() and new bindings
    insert.reset();
    insert.clearBindings();

    // Second row with string/int64/float
    {
        const std::string   second("second");
        const int32_t       int32 = -123;
        const int64_t       int64 = 12345678900000LL;
        const float         float32 = 0.234f;
        insert.bind("@msg",      second);
        insert.bind("@int",      int32);
        insert.bind("@long",     int64);
        insert.bind("@double",   float32);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(2,                query.getColumn(0).getInt64());
        EXPECT_EQ(second,           query.getColumn(1).getText());
        EXPECT_EQ(-123,             query.getColumn(2).getInt());
        EXPECT_EQ(12345678900000LL, query.getColumn(3).getInt64());
        EXPECT_EQ(0.234f,           query.getColumn(4).getDouble());
    }

    // reset() without clearbindings()
    insert.reset();

    // Third row with binary buffer and a null parameter
    {
        const char buffer[] = "binary";
        insert.bind("@msg", buffer, sizeof(buffer));
        insert.bind("@int"); // bind a NULL value
        EXPECT_EQ(1, insert.exec());

        // Check the result
        query.executeStep();
        EXPECT_TRUE (query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(3,                query.getColumn(0).getInt64());
        EXPECT_STREQ(buffer,        query.getColumn(1).getText());
        EXPECT_TRUE (query.isColumnNull(2));
        EXPECT_EQ(0,                query.getColumn(2).getInt());
        EXPECT_EQ(0.234f,           query.getColumn(4).getDouble());
    }

    // reset() without clearbindings()
    insert.reset();

    // Fourth row with uint32_t unsigned value and int64_t 64bits value
    {
        const uint32_t  uint32 = 4294967295U;
        const int64_t   int64 = 12345678900000LL;
        insert.bind("@int", uint32);
        insert.bind("@long", int64);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(4, query.getColumn(0).getInt64());
        EXPECT_EQ(4294967295U, query.getColumn(2).getUInt());
        EXPECT_EQ(12345678900000LL, query.getColumn(3).getInt64());
    }
}


TEST(Statement, bindByNameString)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE | SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, double REAL, long INTEGER)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Insertion with bindable parameters
    SQLite::Statement insert(db, "INSERT INTO test VALUES (NULL, @msg, @int, @double, @long)");

    const std::string amsg = "@msg";
    const std::string aint = "@int";
    const std::string along = "@long";
    const std::string adouble = "@double";

    // First row with text/int/double
    insert.bind(amsg, "first");
    insert.bind(aint, 123);
    insert.bind(along, -123);
    insert.bind(adouble, 0.123);
    EXPECT_EQ(1, insert.exec());
    EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

    // Compile a SQL query to check the result
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(5, query.getColumnCount());

    // Check the result
    query.executeStep();
    EXPECT_TRUE(query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_EQ(1, query.getColumn(0).getInt64());
    EXPECT_STREQ("first", query.getColumn(1).getText());
    EXPECT_EQ(123, query.getColumn(2).getInt());
    EXPECT_EQ(0.123, query.getColumn(3).getDouble());
    EXPECT_EQ(-123, query.getColumn(4).getInt());

    // reset() with clearbindings() and new bindings
    insert.reset();
    insert.clearBindings();

    // Second row with string/int64/float
    {
        const std::string   second("second");
        const int64_t       int64 = 12345678900000LL;
        const int64_t       integer = -123;
        const float         float32 = 0.234f;
        insert.bind(amsg, second);
        insert.bind(aint, int64);
        insert.bind(adouble, float32);
        insert.bind(along, integer);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(2, query.getColumn(0).getInt64());
        EXPECT_EQ(second, query.getColumn(1).getText());
        EXPECT_EQ(12345678900000LL, query.getColumn(2).getInt64());
        EXPECT_EQ(0.234f, query.getColumn(3).getDouble());
        EXPECT_EQ(-123, query.getColumn(4).getInt());
    }

    // reset() without clearbindings()
    insert.reset();

    // Third row with binary buffer and a null parameter
    {
        const char buffer[] = "binary";
        insert.bind(amsg, buffer, sizeof(buffer));
        insert.bind(aint); // bind a NULL value
        EXPECT_EQ(1, insert.exec());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(3, query.getColumn(0).getInt64());
        EXPECT_STREQ(buffer, query.getColumn(1).getText());
        EXPECT_TRUE(query.isColumnNull(2));
        EXPECT_EQ(0, query.getColumn(2).getInt());
        EXPECT_EQ(0.234f, query.getColumn(3).getDouble());
    }

    // reset() without clearbindings()
    insert.reset();

    // Fourth row with uint32_t unsigned value and int64_t 64bits value
    {
        const uint32_t  uint32 = 4294967295U;
        const int64_t   int64 = 12345678900000LL;
        insert.bind(aint, uint32);
        insert.bind(along, int64);
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(4, query.getColumn(0).getInt64());
        EXPECT_EQ(4294967295U, query.getColumn(2).getUInt());
        EXPECT_EQ(12345678900000LL, query.getColumn(4).getInt64());
    }
}

TEST(Statement, bindNoCopyByName)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, txt1 TEXT, txt2 TEXT, binary BLOB)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    // Insertion with bindable parameters
    SQLite::Statement insert(db, "INSERT INTO test VALUES (NULL, @txt1, @txt2, @blob)");

    // Compile a SQL query to check the results
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());

    // Insert one row with all variants of bindNoCopy()
    {
        const char*         txt1 = "first";
        const std::string   txt2 = "sec\0nd";
        const char          blob[] = { 'b','l','\0','b' };
        insert.bindNoCopy("@txt1", txt1);
        insert.bindNoCopy("@txt2", txt2);
        insert.bindNoCopy("@blob", blob, sizeof(blob));
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(1, db.getLastInsertRowid());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(1, query.getColumn(0).getInt64());
        EXPECT_STREQ(txt1, query.getColumn(1).getText());
        EXPECT_EQ(0, memcmp(&txt2[0], &query.getColumn(2).getString()[0], txt2.size()));
        EXPECT_EQ(0, memcmp(blob, &query.getColumn(3).getString()[0], sizeof(blob)));
    }

    insert.reset();
    query.reset();

    // Insert a second row with all variants of bindNoCopy() using std::string names
    {
        const std::string   atxt1 = "@txt1";
        const std::string   atxt2 = "@txt2";
        const std::string   ablob = "@blob";
        const char*         txt1 = "first2";
        const std::string   txt2 = "sec\0nd2";
        const char          blob[] = { 'b','l','\0','b','2' };
        insert.bindNoCopy(atxt1, txt1);
        insert.bindNoCopy(atxt2, txt2);
        insert.bindNoCopy(ablob, blob, sizeof(blob));
        EXPECT_EQ(1, insert.exec());
        EXPECT_EQ(2, db.getLastInsertRowid());
        EXPECT_EQ(SQLITE_DONE, db.getErrorCode());

        // Check the result
        query.executeStep(); // pass on the first row
        query.executeStep();
        EXPECT_TRUE(query.hasRow());
        EXPECT_FALSE(query.isDone());
        EXPECT_EQ(2, query.getColumn(0).getInt64());
        EXPECT_STREQ(txt1, query.getColumn(1).getText());
        EXPECT_EQ(0, memcmp(&txt2[0], &query.getColumn(2).getString()[0], txt2.size()));
        EXPECT_EQ(0, memcmp(blob, &query.getColumn(3).getString()[0], sizeof(blob)));
    }
}

TEST(Statement, isColumnNull)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    ASSERT_EQ(SQLite::OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (msg TEXT, int INTEGER, double REAL)"));
    ASSERT_EQ(SQLite::OK, db.getErrorCode());

    // Create a first row with no null values, then other rows with each time a NULL value
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (\"first\", 123,  0.123)"));
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (NULL,      123,  0.123)"));
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (\"first\", NULL, 0.123)"));
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (\"first\", 123,  NULL)"));

    // Compile a SQL query
    const std::string select("SELECT * FROM test");
    SQLite::Statement query(db, select);
    EXPECT_EQ(select, query.getQuery());
    EXPECT_EQ(3, query.getColumnCount());

    // Get the first non-null row
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(-1), SQLite::Exception);
    EXPECT_EQ(false, query.isColumnNull(0));
    EXPECT_EQ(false, query.isColumnNull(1));
    EXPECT_EQ(false, query.isColumnNull(2));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);

    // Get the second row with null text
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(-1), SQLite::Exception);
    EXPECT_EQ(true, query.isColumnNull(0));
    EXPECT_EQ(false, query.isColumnNull(1));
    EXPECT_EQ(false, query.isColumnNull(2));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);

    // Get the second row with null integer
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(-1), SQLite::Exception);
    EXPECT_EQ(false, query.isColumnNull(0));
    EXPECT_EQ(true, query.isColumnNull(1));
    EXPECT_EQ(false, query.isColumnNull(2));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);

    // Get the third row with null float
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(-1), SQLite::Exception);
    EXPECT_EQ(false, query.isColumnNull(0));
    EXPECT_EQ(false, query.isColumnNull(1));
    EXPECT_EQ(true, query.isColumnNull(2));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);
}

TEST(Statement, isColumnNullByName)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE);
    ASSERT_EQ(SQLITE_OK, db.getErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (msg TEXT, int INTEGER, double REAL)"));
    ASSERT_EQ(SQLITE_OK, db.getErrorCode());

    // Create a first row with no null values, then other rows with each time a NULL value
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (\"first\", 123,  0.123)"));
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (NULL,      123,  0.123)"));
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (\"first\", NULL, 0.123)"));
    ASSERT_EQ(1, db.exec("INSERT INTO test VALUES (\"first\", 123,  NULL)"));

    // Compile a SQL query
    const std::string select("SELECT * FROM test");
    SQLite::Statement query(db, select);
    EXPECT_EQ(select, query.getQuery());
    EXPECT_EQ(3, query.getColumnCount());

    // Get the first non-null row
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(""), SQLite::Exception);
    EXPECT_EQ(false, query.isColumnNull("msg"));
    EXPECT_EQ(false, query.isColumnNull("int"));
    EXPECT_EQ(false, query.isColumnNull("double"));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);

    // Get the second row with null text
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(""), SQLite::Exception);
    EXPECT_EQ(true, query.isColumnNull("msg"));
    EXPECT_EQ(false, query.isColumnNull(1));
    EXPECT_EQ(false, query.isColumnNull("double"));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);

    // Get the second row with null integer
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(""), SQLite::Exception);
    EXPECT_EQ(false, query.isColumnNull("msg"));
    EXPECT_EQ(true, query.isColumnNull("int"));
    EXPECT_EQ(false, query.isColumnNull("double"));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);

    // Get the third row with null float
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());
    EXPECT_THROW(query.isColumnNull(""), SQLite::Exception);
    EXPECT_EQ(false, query.isColumnNull("msg"));
    EXPECT_EQ(false, query.isColumnNull("int"));
    EXPECT_EQ(true, query.isColumnNull("double"));
    EXPECT_THROW(query.isColumnNull(3), SQLite::Exception);
}

TEST(Statement, getColumnByName)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());
    EXPECT_EQ(SQLite::OK, db.getExtendedErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, double REAL)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());
    EXPECT_EQ(SQLite::OK, db.getExtendedErrorCode());

    // Create a first row
    EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, \"first\", 123, 0.123)"));
    EXPECT_EQ(1, db.getLastInsertRowid());
    EXPECT_EQ(1, db.getTotalChanges());

    // Compile a SQL query
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());
    query.executeStep();
    EXPECT_TRUE (query.hasRow());
    EXPECT_FALSE(query.isDone());

    // Look for non-existing columns
    EXPECT_THROW(query.getColumn("unknown"), SQLite::Exception);
    EXPECT_THROW(query.getColumn(""), SQLite::Exception);

    const std::string   msg     = query.getColumn("msg");
    const int           integer = query.getColumn("int");
    const double        real    = query.getColumn("double");
    EXPECT_EQ("first",  msg);
    EXPECT_EQ(123,      integer);
    EXPECT_EQ(0.123,    real);
}

TEST(Statement, getName)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE|SQLite::OPEN_CREATE);
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT)"));

    // Compile a SQL query, using the "id" column name as-is, but aliasing the "msg" column with new name "value"
    SQLite::Statement query(db, "SELECT id, msg as value FROM test");
    query.executeStep();

    const std::string name0 = query.getColumnName(0);
    const std::string name1 = query.getColumnName(1);
    EXPECT_EQ("id", name0);
    EXPECT_EQ("value", name1);

#ifdef SQLITE_ENABLE_COLUMN_METADATA
    // Show how to get origin names of the table columns from which theses result columns come from.
    // Requires the SQLITE_ENABLE_COLUMN_METADATA preprocessor macro to be
    // also defined at compile times of the SQLite library itself.
    const std::string oname0 = query.getColumnOriginName(0);
    const std::string oname1 = query.getColumnOriginName(1);
    EXPECT_EQ("id", oname0);
    EXPECT_EQ("msg", oname1);
#endif
}

TEST(Statement, getColumnDeclaredType)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE | SQLite::OPEN_CREATE);
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, value DOUBLE)"));

    SQLite::Statement query(db, "SELECT *, 1 FROM test");

    const std::string decltype0 = query.getColumnDeclaredType(0);
    const std::string decltype1 = query.getColumnDeclaredType(1);
    const std::string decltype2 = query.getColumnDeclaredType(2);
    EXPECT_EQ("INTEGER", decltype0);
    EXPECT_EQ("TEXT", decltype1);
    EXPECT_EQ("DOUBLE", decltype2);

    // The column at index 3 is not a table column.
    EXPECT_THROW(query.getColumnDeclaredType(3), SQLite::Exception);

    // Index out of bounds.
    EXPECT_THROW(query.getColumnDeclaredType(4), SQLite::Exception);

    // Not a select statement.
    SQLite::Statement pragma(db,"PRAGMA compile_options");
    EXPECT_THROW(pragma.getColumnDeclaredType(0), SQLite::Exception);
}

#if __cplusplus >= 201402L || (defined(_MSC_VER) && _MSC_VER >= 1900)
TEST(Statement, getColumns)
{
    struct GetRowTestStruct
    {
        int id;
        std::string msg;
        int integer;
        double real;
        GetRowTestStruct(int _id, std::string _msg, int _integer, double _real)
        : id(_id), msg(_msg), integer(_integer), real(_real)
        {}

        GetRowTestStruct(int _id, const std::string& _msg)
        : id(_id), msg(_msg), integer(-1), real(0.0)
        {}
    };

    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE | SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());
    EXPECT_EQ(SQLite::OK, db.getExtendedErrorCode());

    // Create a new table
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT, int INTEGER, double REAL)"));
    EXPECT_EQ(SQLite::OK, db.getErrorCode());
    EXPECT_EQ(SQLite::OK, db.getExtendedErrorCode());

    // Create a first row
    EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, \"first\", 123, 0.123)"));
    EXPECT_EQ(1, db.getLastInsertRowid());
    EXPECT_EQ(1, db.getTotalChanges());

    // Compile a SQL query
    SQLite::Statement query(db, "SELECT * FROM test");
    EXPECT_STREQ("SELECT * FROM test", query.getQuery().c_str());
    EXPECT_EQ(4, query.getColumnCount());
    query.executeStep();
    EXPECT_TRUE(query.hasRow());
    EXPECT_FALSE(query.isDone());

    // Get all columns
    auto testStruct = query.getColumns<GetRowTestStruct, 4>();
    EXPECT_EQ(1, testStruct.id);
    EXPECT_EQ("first", testStruct.msg);
    EXPECT_EQ(123, testStruct.integer);
    EXPECT_EQ(0.123, testStruct.real);

    // Get only the first 2 columns
    auto testStruct2 = query.getColumns<GetRowTestStruct, 2>();
    EXPECT_EQ(1, testStruct2.id);
    EXPECT_EQ("first", testStruct2.msg);
    EXPECT_EQ(-1, testStruct2.integer);
    EXPECT_EQ(0.0, testStruct2.real);
}
#endif

TEST(Statement, getBindParameterCount)
{
    // Create a new database
    SQLite::Database db(":memory:", SQLite::OPEN_READWRITE | SQLite::OPEN_CREATE);
    EXPECT_EQ(0, db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, msg TEXT)"));

    SQLite::Statement query(db, "SELECT id, msg FROM test where id = ?");
    EXPECT_EQ(1, query.getBindParameterCount());

    SQLite::Statement query2(db, "SELECT id, msg FROM test where id = ? and msg = ?");
    EXPECT_EQ(2, query2.getBindParameterCount());

    SQLite::Statement query3(db, "SELECT id, msg FROM test");
    EXPECT_EQ(0, query3.getBindParameterCount());
}
