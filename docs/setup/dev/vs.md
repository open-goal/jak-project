# Visual Studio

## Known Issues

- Later versions of 2022 have issues with intellisense when using clang
- Recent versions of 2026 have issues with CMake where the project will endlessly build:
  - https://developercommunity.visualstudio.com/t/Switching-git-branches-seemingly-causes-/11025316?viewtype=all

## Steps

This will create a `jak-project` folder, open the project as a CMake project via Visual Studio.

![](/docs/img/windows/open-project.png)

Then build the entire project as `Windows Release (clang)`. You can also press Ctrl+Shift+B as a hotkey for Build All.  We currently prefer `clang` on Windows as opposed to `msvc`, though it should work as well!

![](/docs/img/windows/release-build.png)
![](/docs/img/windows/build-all.png)
