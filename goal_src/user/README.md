This directory holds the user profiles.

To make your own profile, create a new directory here with your username.
e.g. for username `mark` make a directory called `mark`
Inside that directory, create `user.gs` and `user.gc` files.
These are your own user scripts, loaded after the GOOS library and GOAL library respectively.

> Alternatively, if you only have a single folder in `goal_src/user/` it will be assumed to be your user folder

The rest of the directory can be used however you please!

To automatically log in as a specific user, create a `user.txt` file in this directory
which contains just the username you want to log in as. That way you don't have to
modify multiple scripts when you want to change users.

If you want to make your profile public, edit the .gitignore in this directory.

Additionally, you can provide a `repl-config.json` to set various REPL settings, an example configuration:
```json
{
  "numConnectToTargetAttempts": 1,
  "jak1": {
    "gameVersionFolder": "jak1_pal", // corresponds with your "gameName" in the decomp config, "jak1" by default
    "asmFileSearchDirs": [
      "goal_src/jak1"
    ]
  },
  "jak2": {
    "gameVersionFolder": "jak2_pal", // corresponds with your "gameName" in the decomp config, "jak2" by default
    "asmFileSearchDirs": [
      "goal_src/jak2"
    ]
  },
  "appendKeybinds": true,
  "keybinds": [
    {
      "modifier": "ctrl",
      "key": "S",
      "description": "Test Bind",
      "command": "(format 0 \"hello world\")"
    }
  ]
}
```

And a `startup.gc` where each line will be executed upon startup

## Re-running certain commands upon listening to the target

A common workflow that you might want in your `startup.gc` is something like the following:

```clj
(mi)
(lt)
(dbgc)
(test-play)
```

This builds the game, connects to the game, attaches the debugger, and runs it.

However, when you crash you ideally want to just be able to:
- stop the game via `(:stop)` or the respective keybind
- fix the code, rebuild just that file
- re-launch the game and re-connect

Upon which you'd probably want to run all or some of the above startup again.  But how can you accomplish this without re-launching the REPL completely?  Like so:

```clj
(mi)
(lt)
;; og:run-below-on-listen
(dbgc)
(test-play)
```

As the comment suggests, upon a succesful `(lt)` it will run any lines below it again.
