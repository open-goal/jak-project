# Windows Environment Setup

Even if you do not intend to use Visual Studio as your IDE of choice, it is the only officially supported way of downloading the various Windows SDKs to build the C++ project.

Download the latest community edition from [here](https://visualstudio.microsoft.com/vs/).  At the time of writing this is Visual Studio 2022.

You will require the `Desktop development with C++` workload.  This can be selected during the installation, or after via the `Visual Studio Installer` program and modifying the Visual Studio Installation.

We recommend getting the rest of the project's dependencies via a package manager, and for that we use Scoop. Follow the steps on the bottom of the homepage [here](https://scoop.sh/) to get it installed.

```sh
scoop install git llvm nasm python task ninja cmake
```
