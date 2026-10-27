# Linux Environment Setup

- [Linux Environment Setup](#linux-environment-setup)
  - [Packages](#packages)
    - [Ubuntu](#ubuntu)
    - [Arch](#arch)
    - [Fedora](#fedora)


## Packages

### Ubuntu

```sh
sudo apt install gcc make cmake ninja-build build-essential g++ nasm clang-format libxrandr-dev libxinerama-dev libxcursor-dev libpulse-dev libxi-dev python libgl1-mesa-dev libssl-dev
sudo sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin
```

### Arch

```sh
sudo pacman -S cmake libpulse base-devel nasm python libx11 libxrandr libxinerama libxcursor libxi
yay -S go-task
```

> Note: Any later documentation that shows a `task ...` command, would instead be a `go-task ...` command.

### Fedora

```sh
sudo dnf install cmake python nasm libX11-devel libXrandr-devel libXinerama-devel libXcursor-devel libXi-devel pulseaudio-libs-devel mesa-libGL-devel
sudo sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin
```

Note: we have found that `clang` and `lld` are significantly faster to compile and link than `gcc`, generate faster code, and have better warning messages. 
To install these:
```sh
sudo apt install lld clang  # Substitute 'apt install' for 'pacman -S' on Arch or 'dnf install' on Fedora
```
If you have previously compiled with a different configuration and now would like to compile with clang, delete 
CMakeCache.txt from your build folder and then build using clang.
Clang is needed to compile with `task gen-cmake-debug` or `task gen-cmake-release`. 
If you want to compile with gcc instead of clang, you will either need to compile using cmake directly or use one of the gcc presets from CMakePresets.json. 