#include "sqlite.h"

#include "common/log/log.h"

bool sqlite::SQLiteDatabase::open_db(const std::string& path) {
  if (is_open()) {
    return true;
  }
  sqlite3* db;

  // NOTE - this opens with SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE
  const auto status_code = sqlite3_open(path.data(), &db);
  if (status_code) {
    lg::error("Unable to open SQLite database: {}", sqlite3_errmsg(db));
    sqlite3_close(db);
    return false;
  }

  // Otherwise, track it
  m_db = std::shared_ptr<sqlite3>(db, SQLite3DatabaseDeleter());
  return true;
}

// TODO - allow passing in a `std::function<int(void*, int, char**, char**)>` to format the results
// for now we do something that works in general (converts to vectors)

sqlite::GenericResponse sqlite::SQLiteDatabase::run_query(const std::string& sql) {
  GenericResponse resp;
  if (!is_open()) {
    return resp;
  }

  char* errMsg = 0;

  const auto rc = sqlite3_exec(
      m_db.value().get(), sql.data(),
      [](void* data, int argc, char** argv, char** /*azColName*/) {
        GenericResponse* resp = static_cast<GenericResponse*>(data);
        std::vector<std::string> row = {};
        for (int i = 0; i < argc; i++) {
          row.push_back(argv[i] ? argv[i] : "NULL");
        }
        resp->rows.push_back(row);
        return 0;
      },
      &resp, &errMsg);

  if (rc != SQLITE_OK) {
    fprintf(stderr, "SQL error: %s\n", errMsg);
    sqlite3_free(errMsg);
    // TODO - store error on response
    return resp;
  }
  return resp;
}
