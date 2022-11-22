/**
 * @file    Savepoint.h
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
#pragma once

#include <SQLiteCpp/Exception.h>

namespace SQLite {

// Foward declaration
class Database;

/**
 * @brief RAII encapsulation of a SQLite Savepoint.
 *
 * A Savepoint is a way to group multiple SQL statements into an atomic
 * secureced operation; either it succeeds, with all the changes commited to the
 * database file, or if it fails, all the changes are rolled back to the initial
 * state at the start of the savepoint.
 *
 * This method also offers big performances improvements compared to
 * individually executed statements.
 *
 * Caveats:
 *
 * 1) Calling COMMIT or commiting a parent transaction or RELEASE on a parent
 * savepoint will cause this savepoint to be released.
 *
 * 2) Calling ROLLBACK or rolling back a parent savepoint will cause this
 * savepoint to be rolled back.
 *
 * 3) This savepoint is not saved to the database until this and all savepoints
 * or transaction in the savepoint stack have been released or commited.
 *
 * See also: https://sqlite.org/lang_savepoint.html
 *
 * Thread-safety: a Transaction object shall not be shared by multiple threads,
 * because:
 *
 * 1) in the SQLite "Thread Safe" mode, "SQLite can be safely used by multiple
 * threads provided that no single database connection is used simultaneously in
 * two or more threads."
 *
 * 2) the SQLite "Serialized" mode is not supported by SQLiteC++, because of the
 * way it shares the underling SQLite precompiled statement in a custom shared
 * pointer (See the inner class "Statement::Ptr").
 */

class Savepoint {
   public:
    /**
     * @brief Begins the SQLite savepoint
     *
     * @param[in] aDatabase the SQLite Database Connection
     * @param[in] aName the name of the Savepoint
     *
     * Exception is thrown in case of error, then the Savepoint is NOT
     * initiated.
     */
    Savepoint(Database& aDatabase, const std::string& name);

    // Savepoint is non-copyable
    Savepoint(const Savepoint&) = delete;
    Savepoint& operator=(const Savepoint&) = delete;

    /**
     * @brief Safely rollback the savepoint if it has not been commited.
     */
    ~Savepoint();

    /**
     * @brief Commit and release the savepoint.
     */
    void release();

    /**
     * @brief Rollback the savepoint
     */
    void rollback();

   private:
    Database& mDatabase;        ///< Reference to the SQLite Database Connection
    std::string msName;         ///< Name of the Savepoint
    bool mbReleased = false;    ///< True when release has been called
};
}  // namespace SQLite
