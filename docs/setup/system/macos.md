# MacOS Environment Setup

Running the game requires an Apple Silicon Mac running macOS Sequoia, or an Intel Mac.  Additionally, we currently build on macOS 15 (Sequoia) at this time.

Ensure that you have Xcode command line tools installed (this installs things like Apple Clang).  If you don't, you can run the following command:

```bash
xcode-select --install
```

On Apple Silicon, Rosetta 2 also must be installed:

```bash
softwareupdate --install-rosetta
```

## Building for x86_64

```bash
brew install cmake nasm ninja go-task clang-format
cmake -B build --preset=Release-macos-x86_64-clang
cmake --build build --parallel $((`sysctl -n hw.logicalcpu`))
```

## Building for ARM64 (experimental, unsupported)

```bash
brew install cmake ninja go-task clang-format
cmake -B build --preset=Release-macos-arm64-clang
cmake --build build --parallel $((`sysctl -n hw.logicalcpu`))
```

You may have to add the MacOS SDK to your `LIBRARY_PATH`:
- `export LIBRARY_PATH="$LIBRARY_PATH:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"`
