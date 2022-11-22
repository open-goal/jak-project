2012 Mar 30
- Start of a new thin C++ SQLite wrapper

2012 Apr 2
- The wrapper is functional
- Added documentation and examples
- Publication on GitHub

Version 0.1.0 - 2012 Apr 4
- Added a Database::exec() method to execute simple SQL statement
- Added a version number like in sqlite3.h, starting with 0.1.0

Version 0.2.0 - 2012 Apr 11
- Added getLastInsertId() and setBusyTimout()
- Added bind() by name methods

Version 0.3.0 - 2012 Apr 16
- Added an easy wrapper Database::execAngGet()

Version 0.4.0 - 2012 Apr 23
- Added a Database::tableExists() easy to use function

Dec 10 2012
- Added a Statement::exec() method to execute a one-step query with no expected result

Version 0.5.0 - 2013 March 9
- Added assert() on errors on destructors
- Added getBytes()
- Added getBlob(), getType() and isInteger/isFloat/isText/isBlob/isNull
- Added bind() for binary blob data

Version 0.5.1 - 2013 April 7
- Added Column::getName()

Version 0.6.0 - 2013 November 22
- Renamed Column::getName() to Column::getOriginName()
- Added Column::getName()

Version 0.7.0 - 2014 January 9
- Added Database::createFunction()
- Added std::string version of existing APIs
- Improved CMake with more build options and Doxygen auto-detection

Version 0.8.0 - 2014 February 26
- Database constructor support opening a database with a custom VFS (default to NULL)
- Changed Column::getText() to return empty string "" by default instead of NULL pointer (to handle std::string conversion)

Version 1.0.0 - 2015 May 3
- Public headers file moved to include/ dir
- Added support to biicode in CMakeLists.txt
- Added Unit Tests
- Added aBusyTimeoutMs parameter to Database() constructors
- Added Database::getTotalChanges()
- Added Database::getErrorCode()
- Added Statement::clearBindings()
- Added Statement::getColumn(aName)
- Added Statement::getErrorCode()
- Added Statement::getColumnName(aIndex)
- Added Statement::getColumnOriginName(aIndex)

Version 1.1.0 - 2015 May 18
- Fixed valgrind error on Database destructor
- Added Database::loadExtension

Version 1.2.0 - 2015 September 9
- Fixed build with GCC 5.1.0
- Fixed MSVC release build warning
- Fixed CppDepends warnings
- Updated documentation on installation
- Added Database::getHandle()

Version 1.3.0 - 2015 November 1
- Fixed build with Visual Studio 2015
- Further improvements to README
- Added Backup class

Version 1.3.1 - 2016 February 10
- Switch Linux/Mac build to the provided SQLite3 C library
- Update SQLite3 from 3.8.8.3 to latest 3.10.2 (2016-01-20)
- Remove warnings
- Remove biicode support (defunct service, servers will shutdown the 16th of February 2016)

Version 2.0.0 - 2016 July 25
- Update SQLite3 from 3.10.2 to latest 3.13 (2016-05-18)
- Move #include <sqlite3.h> from headers to .cpp files only using forward declarations
- Add Database::VERSION to reach SQLITE_VERSION without including sqlite3.h in application code
- Add getLibVersion() and getLibVersionNumber() to get runtime version of the library
- Better exception messages when Statements fail PR #84
- Variadic templates for bind() (C++14) PR #85
- Add Statement::bindNoCopy() methods for strings, using SQLITE_STATIC to avoid internal copy by SQLite3 PR #86
- Add Statement::bind() overload for uint32_t, and Column::getUint() and cast operator to uint32_t PR #86
- Use the new SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION from SQLite 3.13 for security reason
- Rename Backup::remainingPageCount()/totalPageCount() to Backup::getRemainingPageCount()/getTotalPageCount()
- Remove Column::errmsg() method : use Database or Statement equivalents
- More unit tests, with code coverage status on the GitHub page
- Do not force MSVC to use static runtime if unit-tests are not build

Version 2.1.0 - 2017 July 18
- Update SQLite3 from 3.13 to latest 3.19.3 (2017-06-08)
- Fixed Incompatibility in 3.19.0 (to use older SQLite version set the CMake variable SQLITE_USE_LEGACY_STRUCT) #125
- Fixed link error (inline in cpp) and compiler warnings (unused variable...) #96
- Added ability to open encrypted databases (using SQLCipher, eg. libsqlcipher-dev) #107
- Added convenience functions for constructing objects from a row #114
- Added CMake install step #118
- Fix warnings #119
- Make cpplint.py Python-3 compatible #120
- Link libssp when targeted #100
- Removed redundant const #102

Version 2.2.0 - 2017 Sept 19
- Update SQLite3 from 3.19.3 to latest 3.20.1 (2017-08-24) #143
- Added tryExecuteStep and tryReset #142
- Removed virtual keywords from destructors #140
- Removed misplaced noexcept keyword #139
- Improved Exception class C++ conformance #138
- Fix warnings #134
- Deprecated Statement::isOk() to Statement::hasRow()

Version 2.3.0 - 2019 March 3
- Update SQLite3 from 3.20.1 to latest 3.27.2 (2019-02-25) #183 #187
- Add Statement binding for long int values #147
- Allows long int for bind when used with name #148
- More cmake instructions for Linux #151
- Add comparison with sqlite_orm #141
- Fix Statement::bind truncates long integer to 32 bits on x86_64 Linux #155
- Add a move constructor to Database #157
- Added tests for all MSVC compilers available on AppVeyor (2013, 2015, 2017) #169
- Update VariadicBind.h #172
- Better CMake compatibility #170
- Add implicit cast operator to char and short types #179 #180

Version 2.4.0 - 2019 August 25
- Update SQLite3 from 3.27.2 to 3.29.0 (2019-07-10) #217
- #191 CMake Warning line 299
- #190 Implement move constructors
- #192 Add wrapper for bind parameter count
- #197 Add tuple_bind and execute_many (requested by #24)
- #199 Fix #156 misleading error message in exception from Statement::exec
- #201 Add Statement::getExpandedSQL() to get the SQL text of prepared statement with bound parameters expanded
- #211 Implement Database::backup()
- #215 Disable implicit fallthrough warning when building internal sqlite3
- #216 Set PROJECT_VERSION to fix CMP0048 Policy warnings

Version 2.5.0 - 2019 December 31
- Update SQLite3 from 3.29.0 to 3.30.1 (2019-10-10)
- 100% Unit Test coverage
- #212 fix sqlite3 compile properties (jzt)
- #219 Disable cast-function-type warning when building internal sqlite (zxey)
- #230 Fixed installation on other than Ubuntu GNU/Linux distributions (xvitaly)
- #228 use transitive compile definitions via cmake (BioDataAnalysis/emmenlau)
- #232 Added support of packaged GTest for running unit tests (xvitaly)
- #231 Added SOVERSION field for shared library (xvitaly)
- #229 Explicitly find and link against system sqlite library (xvitaly)
- #235 Added support for cmake dependencies and version information (BioDataAnalysis/emmenlau)
- #249 Added SQLite header parsing functionality and associated tests (patrick--)

- #251 Added example for getHeaderInfo()

Version 3.0.0 - 2020 January 31
- C++11 is now required
- CMake 3.1 minimum
- Visual Studio 2015 minimum
- Update Googletest to latest release 1.10
- Add Github Actions continuous integration solution
- Add Valgrind memcheck tool to Travis CI
- Remove Statement::isOk() deprecated in 2.2.0 when renamed to Statement::hasRow()
- Replace Database::backup() "C" implementation by calling the Backup class
- #252 Run Valgrind memcheck on Travis CI
- #253 Keep inline functions for GCov code coverage
- #254 Re-enable Coverity static analysis
- #256 Fix linking with system library (libsqlite3)
- #242 Added a `getIndex` method and used it (KOLANICH)
- #257 Improve Statement unit tests coverage (bind by name with a std::string)
- #234 support for external sqlite3 (BioDataAnalysis/emmenlau)
- #243 adding a pure attribute to getIndex() (KOLANICH)

Version 3.1.0 - 2020 August 11
- Update SQLite3 from 3.30.1 to 3.32.3 (2020-06-18)
- #274 Install both cmake files into same lib directory from tcraigtyler
- #275 Add a method on Statement to get the declared type of a column. from daniel-schmidt
- #284 Add SQLITE_OPEN_FULLMUTEX flag from rwrx
- #286 Add CMake option to toggle stack protection from chrisdalke
- #287 Fixed installation on other than Ubuntu distributions from xvitaly
- #288 Allow building of sqlite JSON1 extension when building internal sqlite library from zxey

Version 3.1.1 - 2020 August 19
- #292 Fix compilation if using SQLITE_HAS_CODEC from sum01
- #293 Remove FindSQLiteCpp.cmake from sum01

Version 3.2.0 - 2022 Septembre 18
- #300 #316 #362 #368 Updated SQLite3 from 3.32.3 to 3.39.3 (2022-09-05)
- #236 Disable explicit setting of MSVC runtime from BioDataAnalysis/emmenlau
- #308 Fix build warning due to string truncation from stauffer-garmin
- #311 Add Database::tryExec() from kcowolf
- #313 [CMake] Add SQLITECPP_INCLUDE_SCRIPT option from past-due
- #314 Add Database constructor for filesystem::path (#296) from ptrks
- #295 Compile internal SQLite library with -ffunction-sections from smichaku
- #299 Added Savepoint support from catalogm
- #333 Added Database and Statement getChanges()
- #305 Add other constants that work with sqlite3_open_v2 from LuAPi/more-flags
- #333 Added Database and Statement method getChanges() from SRombauts/get-changes
- #334 fix link for HAS_CODEC from linux-fan-dave/master
- #338 fix load extension from paulo-coutinho/fix-load-extension
- #335 from jagerman/older-macos-avoid-std-filesystem
- #337 Add catkin configuration from ardabbour/master
- #339 Allow specifying transaction behaviors DEFERRED, IMMEDIATE, and EXCLUSIVE from jjenkins278/transaction_behavior
- #340 add HTML keywords and properly link up the links in docs/README.md from phoebe-leong/patch-1
- #341 Install the package.xml file from ardabbour/patch-1
- #352 add basic meson support from ninjaoflight/meson-support
- #349 Refactoring of Statement and Column classes from Kacperos155/refactoring-Statement&Column
- #359 Fix compilation issues earlier than iOS 13
- #354 Windows improved support (meson) from ninjaoflight/windows-migration
- #361 Fix Statement unit test using long from SRombauts/fix-statement-unit-tests-long-long-type
- #346 Add compatible definition for std::experimental::filesystem from guoh27/master
- #364 Removal of remaining long APIs from SRombauts/convert-remaining-long-types
- #366 Add vcpkg installation instructions from FrankXie05/vcpkg-instructions
- #360 Small improvements and code cleaning from Kacperos155/small_improvements
