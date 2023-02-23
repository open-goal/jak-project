/**
 * @file    Savepoint.cpp
 * @ingroup SQLiteCpp
 * @brief   A Savepoint is a way to group multiple SQL statements into an atomic
 * secured operation. Similar to a transaction while allowing child savepoints.
 *
 * Copyright (c) 2020 Kelvin Hammond (hammond.kelvin@gmail.com)
 * Copyright (c) 2020-2022 Sebastien Rombauts (sebastien.rombauts@gmail.com)
 *
 * Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt or
 * copy at http://opensource.org/licenses/MIT)
 */

#include <SQLiteCpp/Assertion.h>
#include <SQLiteCpp/Database.h>
#include <SQLiteCpp/Savepoint.h>
#include <SQLiteCpp/Statement.h>

namespace SQLite {

// Begins the SQLite savepoint
Savepoint::Savepoint(Database& aDatabase, const std::string& aName)
    : mDatabase(aDatabase), msName(aName) {
    // workaround because you cannot bind to SAVEPOINT
    // escape name for use in query
    Statement stmt(mDatabase, "SELECT quote(?)");
    stmt.bind(1, msName);
    stmt.executeStep();
    msName = stmt.getColumn(0).getText();

    mDatabase.exec(std::string("SAVEPOINT ") + msName);
}

// Safely rollback the savepoint if it has not been committed.
Savepoint::~Savepoint() {
    if (!mbReleased) {
        try {
            rollback();
        } catch (SQLite::Exception&) {
            // Never throw an exception in a destructor: error if already rolled
            // back or released, but no harm is caused by this.
        }
    }
}

// Release the savepoint and commit
void Savepoint::release() {
    if (!mbReleased) {
        mDatabase.exec(std::string("RELEASE SAVEPOINT ") + msName);
        mbReleased = true;
    } else {
        throw SQLite::Exception("Savepoint already released or rolled back.");
    }
}

// Rollback the savepoint
void Savepoint::rollback() {
    if (!mbReleased) {
        mDatabase.exec(std::string("ROLLBACK TO SAVEPOINT ") + msName);
        mbReleased = true;
    } else {
        throw SQLite::Exception("Savepoint already released or rolled back.");
    }
}

}  // namespace SQLite
