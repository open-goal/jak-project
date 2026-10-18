# Linux Environment Setup

- [Linux Environment Setup](#linux-environment-setup)
  - [Packages](#packages)
    - [Ubuntu (20.04)](#ubuntu-2004)
  - [Arch](#arch)
  - [Fedora](#fedora)
  - [Compiling and Building](#compiling-and-building)


## Packages

### Ubuntu (20.04)

```sh
sudo apt install gcc make cmake ninja-build build-essential g++ nasm clang-format libxrandr-dev libxinerama-dev libxcursor-dev libpulse-dev libxi-dev python libgl1-mesa-dev libssl-dev
sudo sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin
```

## Arch

```sh
sudo pacman -S cmake libpulse base-devel nasm python libx11 libxrandr libxinerama libxcursor libxi
yay -S go-task
```

> Note: Any later documentation that shows a `task ...` command, would instead be a `go-task ...` command.

## Fedora

```sh
sudo dnf install cmake python lld clang nasm libX11-devel libXrandr-devel libXinerama-devel libXcursor-devel libXi-devel pulseaudio-libs-devel mesa-libGL-devel
sudo sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin
```

## Compiling and Building

Compile:

```sh
cmake -B build && cmake --build build -j 8
```

Run tests:

```sh
./test.sh
```

Note: we have found that `clang` and `lld` are significantly faster to compile and link than `gcc`, generate faster code, and have better warning messages. To install these:

```sh
sudo apt install lld clang
```

and run `cmake` (in a fresh build directory) with:

```sh
cmake -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld" -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ ..
```
