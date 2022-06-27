#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "cubeb::cubeb" for configuration "Release"
set_property(TARGET cubeb::cubeb APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(cubeb::cubeb PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C;CXX"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libcubeb.a"
  )

list(APPEND _IMPORT_CHECK_TARGETS cubeb::cubeb )
list(APPEND _IMPORT_CHECK_FILES_FOR_cubeb::cubeb "${_IMPORT_PREFIX}/lib/libcubeb.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
