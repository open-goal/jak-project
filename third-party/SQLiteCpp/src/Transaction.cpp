/**
 * @file    Transaction.cpp
 * @ingroup SQLiteCpp
 * @brief   A Transaction is way to group multiple SQL statements into an atomic secured operation.
 *
 * Copyright (c) 2012-2022 Sebastien Rombauts (sebastien.rombauts@gmail.com)
 *
 * Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
 * or copy at http://opensource.org/licenses/MIT)
 */
#include <SQLiteCpp/Transaction.h>

#include <SQLiteCpp/Database.h>
#include <SQLiteCpp/Assertion.h>

#include <sqlite3.h>

namespace SQLite
{


// Begins the SQLite transaction
Transaction::Transaction(Database& aDatabase, TransactionBehavior behavior) :
    mDatabase(aDatabase)
{
    const char *stmt;
    switch (behavior) {
        case TransactionBehavior::DEFERRED:
            stmt = "BEGIN DEFERRED";
            break;
        case TransactionBehavior::IMMEDIATE:
            stmt = "BEGIN IMMEDIATE";
            break;
        case TransactionBehavior::EXCLUSIVE:
            stmt = "BEGIN EXCLUSIVE";
            break;
        default:
            throw SQLite::Exception("invalid/unknown transaction behavior", SQLITE_ERROR);
    }
    mDatabase.exec(stmt);
}

// Begins the SQLite transaction
Transaction::Transaction(Database &aDatabase) :
    mDatabase(aDatabase)
{
    mDatabase.exec("BEGIN");
}

// Safely rollback the transaction if it has not been committed.
Transaction::~Transaction()
{
    if (false == mbCommited)
    {
        try
        {
            mDatabase.exec("ROLLBACK");
        }
        catch (SQLite::Exception&)
        {
            // Never throw an exception in a destructor: error if already rollbacked, but no harm is caused by this.
        }
    }
}

// Commit the transaction.
void Transaction::commit()
{
    if (false == mbCommited)
    {
        mDatabase.exec("COMMIT");
        mbCommited = true;
    }
    else
    {
        throw SQLite::Exception("Transaction already committed.");
    }
}


}  // namespace SQLite
