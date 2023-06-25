#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "third-party/sqlite3/sqlite3.h"

// Just a simple wrapper around the raw C types from sqlite3
// takes care of all the relevant memory freeing and such for you (hopefully)
namespace sqlite {
struct SQLite3DatabaseDeleter {
  void operator()(sqlite3* db) const {
    if (db) {
      sqlite3_close(db);
    }
  }
};

struct GenericResponse {
  std::vector<std::vector<std::string>> rows;
};

class SQLiteDatabase {
 public:
  SQLiteDatabase() = default;
  bool is_open() const { return m_db.has_value(); }
  bool open_db(const std::string& path);
  GenericResponse run_query(const std::string& sql);

 private:
  std::optional<std::shared_ptr<sqlite3>> m_db;
};

}  // namespace sqlite
