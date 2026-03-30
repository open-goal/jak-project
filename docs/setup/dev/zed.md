# Zed

Zed comes with out of the box C++ support via `clangd`.  The project comes with a bunch of already setup task and debug configurations.  Note that many of them depend on the usage of `task` to make them cross-OS compatible (Zed at this time doesn't allow for OS-conditional logic within the task definitions).

## Recommended Extensions:

- NeoCMake

## Building the Project

We are going to build a debug version of the project because that is most useful for development.

Run the `task: spawn` command (default shortcut is `Alt-Shift-T`) and run `Generate CMake: Debug`

![](/docs/img/editors/zed/zed-gen-cmake.png)

Then do the same thing to run `Build Project: Debug`

![](/docs/img/editors/zed/zed-build-proj.png)

The project is now fully built, you can now for example -- launch the REPL (`goalc`), or the game (`gk`) and attach breakpoints.

![](/docs/img/editors/zed/zed-run-example.png)

![](/docs/img/editors/zed/zed-debugger.png)
