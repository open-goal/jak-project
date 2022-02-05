# 11Zip

[![build](https://github.com/Sygmei/11Zip/actions/workflows/build.yml/badge.svg)](https://github.com/Sygmei/11Zip/actions/workflows/build.yml)

11Zip (or **EL**<sub>even</sub>**Zip**) is a dead simple zipping / unzipping C++ Lib

I didn't really wanted to make my own zipping / unzipping library but as I found nothing else, I made my own.

That's why I called this lib 11Zip, 11 is the atomic number of the sodium as it describes pretty well my salty feeling when searching for a zipping / unzipping C++ Lib.

By the same time it's also a reference to 7zip.

The lib is based on zlib & minizip : https://github.com/madler/zlib

It does also include some wrapper code from here : www.vilipetek.com/2013/11/22/zippingunzipping-files-in-c/

## Installation

Clone this repository using the following command :

```bash
git clone --recurse-submodules https://github.com/Sygmei/11Zip.git
```

(don't forget about the `--recurse-submodules` part or dependencies are not going to be downloaded).

Once downloaded, put the content into your `extlibs/` (or equivalent) directory and put the following lines in your `CMakeLists.txt` :

```cmake
add_subdirectory(extlibs/elzip) # Path to the 11zip

target_link_libraries(YourTargetName elzip)
```

## Reference

### `elz::extractZip`

```cpp
void elz::extractZip(std::string zipname, std::string target = ".");
```

Unzips the content of a zip in "target" folder (which is current folder by default).

- **zipname** : Path to the .zip archive to unzip
- **target** : Path to the directory where you want to unzip the archive, defaults to the current working directory

### `elz::extractFile`

```cpp
void elz::extractFile(std::string zipname, std::string filename, std::string target = ".");
```

Extracts a single file from a zip archive

- **zipname** : Path to the .zip archive to unzip the file from
- **filename** : Path of the file to extract within the zip archive, note that this name will be used by the unzipped file as well
- **target** : Path where to extract the file to, defaults to the current working directory

### `elz::zipFolder`

```cpp
void elz::zipFolder(std::string folderName, std::string zipTarget = "");
```

Zips the folder "folderName" to "zipTarget" (Which is "folderName".zip by default).

- **folderName** : Name of the folder to zip (will zip all of its content, root folder excluded)
- **zipTarget** : Path to .zip archive that will be created, defaults to `folderName`.zip
