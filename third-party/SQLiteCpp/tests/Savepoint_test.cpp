/**
 * @file    Savepoint_test.cpp
 * @ingroup tests
 * @brief   Test of a SQLite Savepoint.
 *
 * Copyright (c) 2020 Kelvin Hammond (hammond.kelvin@gmail.com)
 *
 * Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt or
 * copy at http://opensource.org/licenses/MIT)
 */

#include <SQLiteCpp/Database.h>
#include <SQLiteCpp/Exception.h>
#include <SQLiteCpp/Savepoint.h>
#include <SQLiteCpp/Statement.h>
#include <SQLiteCpp/Transaction.h>
#include <gtest/gtest.h>

#include <cstdio>

TEST(Savepoint, commitRollback) {
    // Create a new database
    SQLite::Database db(":memory:",
                        SQLite::OPEN_READWRITE | SQLite::OPEN_CREATE);
    EXPECT_EQ(SQLite::OK, db.getErrorCode());

    {
        // Begin savepoint
        SQLite::Savepoint savepoint(db, "sp1");

        EXPECT_EQ(
            0,
            db.exec("CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)"));
        EXPECT_EQ(SQLite::OK, db.getErrorCode());

        // Insert a first valu
        EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, 'first')"));
        EXPECT_EQ(1, db.getLastInsertRowid());

        // release savepoint
        savepoint.release();

        // Commit again throw an exception
        EXPECT_THROW(savepoint.release(), SQLite::Exception);
        EXPECT_THROW(savepoint.rollback(), SQLite::Exception);
    }

    // Auto rollback if no release() before the end of scope
    {
        // Begin savepoint
        SQLite::Savepoint savepoint(db, "sp2");

        // Insert a second value (that will be rollbacked)
        EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, 'third')"));
        EXPECT_EQ(2, db.getLastInsertRowid());

        // end of scope: automatic rollback
    }

    // Auto rollback of a transaction on error / exception
    try {
        // Begin savepoint
        SQLite::Savepoint savepoint(db, "sp3");

        // Insert a second value (that will be rollbacked)
        EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, 'second')"));
        EXPECT_EQ(2, db.getLastInsertRowid());

        // Execute with an error => exception with auto-rollback
        db.exec(
            "DesiredSyntaxError to raise an exception to rollback the "
            "transaction");

        GTEST_FATAL_FAILURE_("we should never get there");
        savepoint.release();  // We should never get there
    } catch (std::exception& e) {
        std::cout << "SQLite exception: " << e.what() << std::endl;
        // expected error, see above
    }

    // Double rollback with a manual command before the end of scope
    {
        // Begin savepoint
        SQLite::Savepoint savepoint(db, "sp4");

        // Insert a second value (that will be rollbacked)
        EXPECT_EQ(1, db.exec("INSERT INTO test VALUES (NULL, 'third')"));
        EXPECT_EQ(2, db.getLastInsertRowid());

        // Execute a manual rollback (no real use case I can think of, so no
        // rollback() method)
        db.exec("ROLLBACK");

        // end of scope: the automatic rollback should not raise an error
        // because it is harmless
    }

    // Check the results (expect only one row of result, as all other one have
    // been rollbacked)
    SQLite::Statement query(db, "SELECT * FROM test");
    int nbRows = 0;
    while (query.executeStep()) {
        nbRows++;
        EXPECT_EQ(1, query.getColumn(0).getInt());
        EXPECT_STREQ("first", query.getColumn(1).getText());
    }
    EXPECT_EQ(1, nbRows);
}
