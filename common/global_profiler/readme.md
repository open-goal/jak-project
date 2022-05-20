# Event Profiler
The event profiler is a tool to analyze timing of multiple frames.  Unlike sampling-based profilers, this profile captures an exact timeline of what happens of what happens when.

## Capturing a profile
In the OpenGOAL window, click "Profiler" and check "Record" to start recording. The buffer has a fixed maximum size and it will automatically overwrite old data once it is full.

When something interesting happens, click the "dump to file" button to save the buffer (currently a few seconds) to `prof.json` in `jak-project`.

The idea is that you can leave this running as you play, and then when the game stutters or does something interesting, you can click the dump button and get the result.

## Viewing a profile
Open Google Chrome and go to `chrome://tracing`. Then click load and open the json file.  Or, just drag and drop the file into chrome.

Press `1` for a box drawing tool. This lets you select a region of the flame chart and get a list of events inside the box.

Press `2` for panning.

Press `3` for zooming.

## Adding an event
The GOAL kernel automatically adds events for each process. If you want to add another event, you can use `(with-profiler "name-of-event" <body>)`. Do not call `suspend` or do a `return` inside of this. If you need more control over stopping/starting, there are functions in `gcommon.gc` to explicitly start/stop events. But you must match them up correctly yourself!

In C++, the graphics profiler automatically adds a profiler nodes as events. To add an event, you can use 

```auto p = scoped_prof("name-of-event");``` 

The event is active from this call until the destruction of `p`.

## Multiple threads
The event profiler currently works on both the graphics and EE threads. Adding the events can safely be done from any thread, but enable/disable/dump should be done from a single thread at a time.

Each thread should periodically insert a `ROOT` instant event when there are no active range events. This is required to make the retroactive dump feature work properly as the event buffer does not capture the tree structure fully, and it must be able to find a point in time when no events are active.