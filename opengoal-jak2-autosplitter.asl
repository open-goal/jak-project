state("gk") {
  // nothing to do here; we need to grab the pointers ourselves instead of hardcoding them
}

// Runs once, the only place you can add custom settings, before the process is connected to!
startup {
  // NOTE: Enable Log Output
  Action<string, bool> DebugOutput = (text, setting) => {
    if (setting) {
      print("[OpenGOAL-jak2] " + text);
    }
  };
  vars.DebugOutput = DebugOutput;

  Action<List<Dictionary<String, dynamic>>, string, int, Type, dynamic, bool, string, bool> AddOption = (list, id, offset, type, splitVal, defaultEnabled, name, debug) => {
    var d = new Dictionary<String, dynamic>();
    d.Add("id", id);
    d.Add("offset", offset);
    d.Add("type", type);
    d.Add("splitVal", splitVal);
    d.Add("defaultEnabled", defaultEnabled);
    d.Add("name", name);
    d.Add("debug", debug);
    list.Add(d);
  };

  Action<dynamic, string> AddToSettings = (options, parent) => {
    foreach (Dictionary<String, dynamic> option in options) {
      settings.Add(option["id"], option["defaultEnabled"], option["name"], parent);
    }
  };

  settings.Add("asl_settings", true, "Autosplitter Settings");
  settings.Add("asl_settings_debug", false, "Enable Debug Logs", "asl_settings");

  // Need Resolution Splits - offset is relative from the need resolution block of the struct
  settings.Add("jak2_need_res", true, "Splits");
  var structByteIdx = 0;

  vars.optionLists = new List<List<Dictionary<String, dynamic>>>();

  // Training
  vars.missionEndResolutions = new List<Dictionary<String, dynamic>>();

  AddOption(vars.missionEndResolutions, "unknown", 0, typeof(byte), 1, false, "unknown", false);
AddOption(vars.missionEndResolutions, "none", 1, typeof(byte), 1, false, "none", false);
AddOption(vars.missionEndResolutions, "complete", 2, typeof(byte), 1, false, "complete", false);
AddOption(vars.missionEndResolutions, "dummy0", 3, typeof(byte), 1, false, "dummy0", false);
AddOption(vars.missionEndResolutions, "eco-blue-button", 4, typeof(byte), 1, false, "eco-blue-button", false);
AddOption(vars.missionEndResolutions, "eco-yellow-button", 5, typeof(byte), 1, false, "eco-yellow-button", false);
AddOption(vars.missionEndResolutions, "eco-red-button", 6, typeof(byte), 1, false, "eco-red-button", false);
AddOption(vars.missionEndResolutions, "fortress-escape", 7, typeof(byte), 1, false, "fortress-escape", false);
AddOption(vars.missionEndResolutions, "city-help-kid", 8, typeof(byte), 1, false, "city-help-kid", false);
AddOption(vars.missionEndResolutions, "city-vehicle-training", 9, typeof(byte), 1, false, "city-vehicle-training", false);
AddOption(vars.missionEndResolutions, "ruins-tower", 10, typeof(byte), 1, false, "ruins-tower", false);
AddOption(vars.missionEndResolutions, "atoll-water", 11, typeof(byte), 1, false, "atoll-water", false);
AddOption(vars.missionEndResolutions, "fortress-dump", 12, typeof(byte), 1, false, "fortress-dump", false);
AddOption(vars.missionEndResolutions, "city-krew-delivery", 13, typeof(byte), 1, false, "city-krew-delivery", false);
AddOption(vars.missionEndResolutions, "city-red-gun-training", 14, typeof(byte), 1, false, "city-red-gun-training", false);
AddOption(vars.missionEndResolutions, "atoll-sig", 15, typeof(byte), 1, false, "atoll-sig", false);
AddOption(vars.missionEndResolutions, "sewer-enemy", 16, typeof(byte), 1, false, "sewer-enemy", false);
AddOption(vars.missionEndResolutions, "strip-rescue", 17, typeof(byte), 1, false, "strip-rescue", false);
AddOption(vars.missionEndResolutions, "atoll-battle", 18, typeof(byte), 1, false, "atoll-battle", false);
AddOption(vars.missionEndResolutions, "mountain-lens", 19, typeof(byte), 1, false, "mountain-lens", false);
AddOption(vars.missionEndResolutions, "mountain-gear", 20, typeof(byte), 1, false, "mountain-gear", false);
AddOption(vars.missionEndResolutions, "mountain-shard", 21, typeof(byte), 1, false, "mountain-shard", false);
AddOption(vars.missionEndResolutions, "mountain-collection", 22, typeof(byte), 1, false, "mountain-collection", false);
AddOption(vars.missionEndResolutions, "city-keira-delivery", 23, typeof(byte), 1, false, "city-keira-delivery", false);
AddOption(vars.missionEndResolutions, "stadium-board1", 24, typeof(byte), 1, false, "stadium-board1", false);
AddOption(vars.missionEndResolutions, "city-krew-collection", 25, typeof(byte), 1, false, "city-krew-collection", false);
AddOption(vars.missionEndResolutions, "city-yellow-gun-training", 26, typeof(byte), 1, false, "city-yellow-gun-training", false);
AddOption(vars.missionEndResolutions, "drill-eggs", 27, typeof(byte), 1, false, "drill-eggs", false);
AddOption(vars.missionEndResolutions, "city-power", 28, typeof(byte), 1, false, "city-power", false);
AddOption(vars.missionEndResolutions, "palace-cable", 29, typeof(byte), 1, false, "palace-cable", false);
AddOption(vars.missionEndResolutions, "palace-boss", 30, typeof(byte), 1, false, "palace-boss", false);
AddOption(vars.missionEndResolutions, "city-shuttle", 31, typeof(byte), 1, false, "city-shuttle", false);
AddOption(vars.missionEndResolutions, "ruins-enemy", 32, typeof(byte), 1, false, "ruins-enemy", false);
AddOption(vars.missionEndResolutions, "city-blue-gun-training", 33, typeof(byte), 1, false, "city-blue-gun-training", false);
AddOption(vars.missionEndResolutions, "forest-scouts", 34, typeof(byte), 1, false, "forest-scouts", false);
AddOption(vars.missionEndResolutions, "city-escort-kid", 35, typeof(byte), 1, false, "city-escort-kid", false);
AddOption(vars.missionEndResolutions, "dig-knock-down", 36, typeof(byte), 1, false, "dig-knock-down", false);
AddOption(vars.missionEndResolutions, "strip-grenade", 37, typeof(byte), 1, false, "strip-grenade", false);
AddOption(vars.missionEndResolutions, "drill-ship", 38, typeof(byte), 1, false, "drill-ship", false);
AddOption(vars.missionEndResolutions, "city-port-run", 39, typeof(byte), 1, false, "city-port-run", false);
AddOption(vars.missionEndResolutions, "city-meet-brutter", 40, typeof(byte), 1, false, "city-meet-brutter", false);
AddOption(vars.missionEndResolutions, "sewer-board", 41, typeof(byte), 1, false, "sewer-board", false);
AddOption(vars.missionEndResolutions, "forest-hunt", 42, typeof(byte), 1, false, "forest-hunt", false);
AddOption(vars.missionEndResolutions, "city-intercept-tanker", 43, typeof(byte), 1, false, "city-intercept-tanker", false);
AddOption(vars.missionEndResolutions, "stadium-race-class3", 44, typeof(byte), 1, false, "stadium-race-class3", false);
AddOption(vars.missionEndResolutions, "city-protect-water-slums", 45, typeof(byte), 1, false, "city-protect-water-slums", false);
AddOption(vars.missionEndResolutions, "dig-find-totem", 46, typeof(byte), 1, false, "dig-find-totem", false);
AddOption(vars.missionEndResolutions, "city-destroy-guard-vehicles", 47, typeof(byte), 1, false, "city-destroy-guard-vehicles", false);
AddOption(vars.missionEndResolutions, "city-play-onin-game", 48, typeof(byte), 1, false, "city-play-onin-game", false);
AddOption(vars.missionEndResolutions, "canyon-insert-items", 49, typeof(byte), 1, false, "canyon-insert-items", false);
AddOption(vars.missionEndResolutions, "tomb-poles", 50, typeof(byte), 1, false, "tomb-poles", false);
AddOption(vars.missionEndResolutions, "tomb-water", 51, typeof(byte), 1, false, "tomb-water", false);
AddOption(vars.missionEndResolutions, "tomb-boss", 52, typeof(byte), 1, false, "tomb-boss", false);
AddOption(vars.missionEndResolutions, "fortress-save-friends", 53, typeof(byte), 1, false, "fortress-save-friends", false);
AddOption(vars.missionEndResolutions, "sewer-escort", 54, typeof(byte), 1, false, "sewer-escort", false);
AddOption(vars.missionEndResolutions, "city-dark-gun-training", 55, typeof(byte), 1, false, "city-dark-gun-training", false);
AddOption(vars.missionEndResolutions, "stadium-race-class2", 56, typeof(byte), 1, false, "stadium-race-class2", false);
AddOption(vars.missionEndResolutions, "city-stop-bomb-bots", 57, typeof(byte), 1, false, "city-stop-bomb-bots", false);
AddOption(vars.missionEndResolutions, "city-errol-challenge", 58, typeof(byte), 1, false, "city-errol-challenge", false);
AddOption(vars.missionEndResolutions, "strip-drop", 59, typeof(byte), 1, false, "strip-drop", false);
AddOption(vars.missionEndResolutions, "ruins-mech", 60, typeof(byte), 1, false, "ruins-mech", false);
AddOption(vars.missionEndResolutions, "forest-protect", 61, typeof(byte), 1, false, "forest-protect", false);
AddOption(vars.missionEndResolutions, "drill-mech", 62, typeof(byte), 1, false, "drill-mech", false);
AddOption(vars.missionEndResolutions, "city-save-lurkers", 63, typeof(byte), 1, false, "city-save-lurkers", false);
AddOption(vars.missionEndResolutions, "stadium-race-class", 64, typeof(byte), 1, false, "stadium-race-class", false);
AddOption(vars.missionEndResolutions, "palace-sneak-in", 65, typeof(byte), 1, false, "palace-sneak-in", false);
AddOption(vars.missionEndResolutions, "castle-break-in", 66, typeof(byte), 1, false, "castle-break-in", false);
AddOption(vars.missionEndResolutions, "castle-boss", 67, typeof(byte), 1, false, "castle-boss", false);
AddOption(vars.missionEndResolutions, "city-whack", 68, typeof(byte), 1, false, "city-whack", false);
AddOption(vars.missionEndResolutions, "under-mech", 69, typeof(byte), 1, false, "under-mech", false);
AddOption(vars.missionEndResolutions, "under-sig", 70, typeof(byte), 1, false, "under-sig", false);
AddOption(vars.missionEndResolutions, "city-defend-stadium", 71, typeof(byte), 1, false, "city-defend-stadium", false);
AddOption(vars.missionEndResolutions, "consite-find-baron", 72, typeof(byte), 1, false, "consite-find-baron", false);
AddOption(vars.missionEndResolutions, "nest-get-to-gun", 73, typeof(byte), 1, false, "nest-get-to-gun", false);
AddOption(vars.missionEndResolutions, "nest-enter", 74, typeof(byte), 1, false, "nest-enter", false);
AddOption(vars.missionEndResolutions, "nest-boss", 75, typeof(byte), 1, false, "nest-boss", false);
AddOption(vars.missionEndResolutions, "city-win", 76, typeof(byte), 1, false, "city-win", false);
AddOption(vars.missionEndResolutions, "city-oracle", 77, typeof(byte), 1, false, "city-oracle", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-ring-1", 78, typeof(byte), 1, false, "city-burning-bush-ring-1", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-1", 79, typeof(byte), 1, false, "city-burning-bush-get-to-1", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-2", 80, typeof(byte), 1, false, "city-burning-bush-get-to-2", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-3", 81, typeof(byte), 1, false, "city-burning-bush-get-to-3", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-4", 82, typeof(byte), 1, false, "city-burning-bush-get-to-4", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-collection-1", 83, typeof(byte), 1, false, "city-burning-bush-collection-1", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-racepoint-1", 84, typeof(byte), 1, false, "city-burning-bush-racepoint-1", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-ring-2", 85, typeof(byte), 1, false, "city-burning-bush-ring-2", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-5", 86, typeof(byte), 1, false, "city-burning-bush-get-to-5", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-6", 87, typeof(byte), 1, false, "city-burning-bush-get-to-6", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-shuttle-1", 88, typeof(byte), 1, false, "city-burning-bush-shuttle-1", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-7", 89, typeof(byte), 1, false, "city-burning-bush-get-to-7", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-8", 90, typeof(byte), 1, false, "city-burning-bush-get-to-8", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-9", 91, typeof(byte), 1, false, "city-burning-bush-get-to-9", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-collection-2", 92, typeof(byte), 1, false, "city-burning-bush-collection-2", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-10", 93, typeof(byte), 1, false, "city-burning-bush-get-to-10", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-11", 94, typeof(byte), 1, false, "city-burning-bush-get-to-11", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-ring-3", 95, typeof(byte), 1, false, "city-burning-bush-ring-3", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-12", 96, typeof(byte), 1, false, "city-burning-bush-get-to-12", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-bombbot-1", 97, typeof(byte), 1, false, "city-burning-bush-bombbot-1", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-13", 98, typeof(byte), 1, false, "city-burning-bush-get-to-13", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-14", 99, typeof(byte), 1, false, "city-burning-bush-get-to-14", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-get-to-15", 100, typeof(byte), 1, false, "city-burning-bush-get-to-15", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-collection-3", 101, typeof(byte), 1, false, "city-burning-bush-collection-3", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-race-errol", 102, typeof(byte), 1, false, "city-burning-bush-race-errol", false);
AddOption(vars.missionEndResolutions, "city-burning-bush-race-port", 103, typeof(byte), 1, false, "city-burning-bush-race-port", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-board", 104, typeof(byte), 1, false, "stadium-burning-bush-race-board", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-class3", 105, typeof(byte), 1, false, "stadium-burning-bush-race-class3", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-class2", 106, typeof(byte), 1, false, "stadium-burning-bush-race-class2", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-class1", 107, typeof(byte), 1, false, "stadium-burning-bush-race-class1", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-class3-r", 108, typeof(byte), 1, false, "stadium-burning-bush-race-class3-r", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-class2-r", 109, typeof(byte), 1, false, "stadium-burning-bush-race-class2-r", false);
AddOption(vars.missionEndResolutions, "stadium-burning-bush-race-class1-r", 110, typeof(byte), 1, false, "stadium-burning-bush-race-class1-r", false);
AddOption(vars.missionEndResolutions, "max", 111, typeof(byte), 1, false, "max", false);
  
  settings.Add("jak2_need_res_training", true, "Mission End Triggers", "jak2_need_res");
  AddToSettings(vars.missionEndResolutions, "jak2_need_res_training");
  vars.optionLists.Add(vars.missionEndResolutions);
  


  

  // Misc Tasks
  // - other tasks other than `need_resolution` ones, the ones deemed useful enough to be added
  settings.Add("jak2_misc_tasks", true, "Final Task");
  vars.miscallenousTasks = new List<Dictionary<String, dynamic>>();
  AddOption(vars.miscallenousTasks, "int_finalboss_movies", 105, typeof(byte), 1, true, "Collect Light Eco", false);
  AddToSettings(vars.miscallenousTasks, "jak2_misc_tasks");
  vars.optionLists.Add(vars.miscallenousTasks);

  // Treat this one as special, so we can ensure the timer ends no matter what!
  vars.finalSplitTask = vars.missionEndResolutions[32];

  vars.DebugOutput("Finished {startup}", true);
}

init {
  vars.DebugOutput("Running {init} looking for `gk.exe`", true);
  var sw = new Stopwatch();
  sw.Start();
  var exported_ptr = IntPtr.Zero;
  vars.foundPointers = false;
  byte[] marker = Encoding.ASCII.GetBytes("UnLiStEdStRaTs_JaK1" + Char.MinValue);
  vars.debugTick = 0;
  vars.DebugOutput(String.Format("Base Addr - {0}", modules.First().BaseAddress.ToString("x8")), true);
  exported_ptr = new SignatureScanner(game, modules.First().BaseAddress, modules.First().ModuleMemorySize).Scan(
    new SigScanTarget(marker.Length, marker)
  );

  if (exported_ptr == IntPtr.Zero) {
    vars.DebugOutput("Could not find the AutoSplittingInfo struct, old version of gk.exe? Failing!", true);
    sw.Reset();
    return false;
  }
  vars.DebugOutput(String.Format("Found AutoSplittingInfo struct - {0}", exported_ptr.ToString("x8")), true);

  // The offset to the GOAL struct is stored in a u64 next to the marker!
  var goal_struct_ptr = new IntPtr(memory.ReadValue<long>(exported_ptr + 4));
  while (goal_struct_ptr == IntPtr.Zero) {
    vars.DebugOutput("Could not find pointer to GOAL struct, game still loading? Retrying in 1000ms...!", true);
    Thread.Sleep(1000);
    sw.Reset();
    throw new Exception("Could not find pointer to GOAL struct, game still loading? Retrying...");
  }
  Action<MemoryWatcherList, IntPtr, List<Dictionary<String, dynamic>>> AddMemoryWatchers = (memList, bPtr, options) => {
    foreach (Dictionary<String, dynamic> option in options) {
      var finalOffset = bPtr + (option["offset"]);
      // TODO - use the type on the object to make this value properly.  Right now everything is a u8
      memList.Add(new MemoryWatcher<byte>(finalOffset) { Name = option["id"] });
      if (option["debug"] == true) {
        memList[option["id"]].Update(game);
        vars.DebugOutput(String.Format("Debug ({0}) -> ptr [{1}]; val [{2}]", option["id"], finalOffset.ToString("x8"), memList[option["id"]].Current), true);
      }
    }
  };

  var watchers = new MemoryWatcherList{
    new MemoryWatcher<uint>(goal_struct_ptr + 212) { Name = "currentGameHash" }
  };

  // Init current game has in case script is loaded while game is already started
  watchers["currentGameHash"].Update(game);

  var jak2_need_res_bptr = goal_struct_ptr + 424; // bytes
  foreach (List<Dictionary<String, dynamic>> optionList in vars.optionLists) {
    AddMemoryWatchers(watchers, jak2_need_res_bptr, optionList);
  }
  vars.foundPointers = true;
  vars.watchers = watchers;
  sw.Stop();
  vars.DebugOutput("Script Initialized, Game Compatible.", true);
  vars.DebugOutput(String.Format("Found the exported struct at {0}", goal_struct_ptr.ToString("x8")), true);
  vars.DebugOutput(String.Format("It took {0} ms", sw.ElapsedMilliseconds), true);
}

update {
  if (!vars.foundPointers) {
    return false;
  }

  vars.watchers.UpdateAll(game);
}

reset {
  if (vars.watchers["currentGameHash"].Current != 0 && vars.watchers["currentGameHash"].Current != vars.watchers["currentGameHash"].Old) {
    vars.DebugOutput("Resetting!", settings["asl_settings_debug"]);
    vars.DebugOutput(String.Format("Reset -> Old: {0}, Curr: {1}", vars.watchers["currentGameHash"].Old, vars.watchers["currentGameHash"].Current), settings["asl_settings_debug"]);
    return true;
  }
  return false;
}

start {
  if (vars.watchers["currentGameHash"].Current != 0 && vars.watchers["currentGameHash"].Current != vars.watchers["currentGameHash"].Old) {
    vars.DebugOutput("Starting!", settings["asl_settings_debug"]);
    vars.DebugOutput(String.Format("Start -> Old: {0}, Curr: {1}", vars.watchers["currentGameHash"].Old, vars.watchers["currentGameHash"].Current), settings["asl_settings_debug"]);
    return true;
  }
  return false;
}

isLoading {
  // todo
  return false;
}

split {
  Func<List<Dictionary<String, dynamic>>, bool> InspectValues = (list) => {
    var debugThisIter = false;
    if (vars.debugTick++ % 60 == 0) {
      debugThisIter = true;
    }
    foreach (Dictionary<String, dynamic> option in list) {
      var watcher = vars.watchers[option["id"]];
      if (option["debug"] && debugThisIter) {
        vars.DebugOutput(String.Format("Debug ({0}) -> old [{1}]; current [{2}]", option["id"], watcher.Old, watcher.Current), settings["asl_settings_debug"]);
      }
      if (settings[option["id"]]) {
        // if we don't care about the amount, split on any change
        if (option["splitVal"] == null && watcher.Current != watcher.Old) {
          return true;
        }
        // Else, make sure we've hit that goal amount
        else if (option["splitVal"] != null && watcher.Current != watcher.Old && watcher.Current == option["splitVal"]) {
          return true;
        }
      }
    }
    return false;
  };
  foreach (List<Dictionary<String, dynamic>> optionList in vars.optionLists) {
    if (InspectValues(optionList)) {
      return true;
    }
  }

  // ALWAYS split if the final split condition is true, so no matter what we exhaust all splits until the end
  if (vars.watchers[vars.finalSplitTask["id"]].Current == vars.finalSplitTask["splitVal"]) {
    return true;
  }
}
