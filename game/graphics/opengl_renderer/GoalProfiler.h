#pragma once

namespace GoalProfiler {

// Called by the EE thread once it is ready to execute GOAL code.
void register_goal_thread();
void unregister_goal_thread();

// Called from the graphics thread.
void update();
void draw_window(bool* open);

}  // namespace GoalProfiler
