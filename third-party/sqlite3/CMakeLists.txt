add_library(sqlite3
 sqlite3.c
 sqlite3.h
)

add_library(SQLite::SQLite3 ALIAS sqlite3)
