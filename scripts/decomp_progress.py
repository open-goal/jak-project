import os
# change working dir to script location
os.chdir(os.path.dirname(__file__))
import glob
import argparse


### Script to track decompilation progress.
### Example usage: python3 scripts/decomp_progress.py jak2

def get_goal_files(root_dir, ext = "*.gc"):
    """Get all GOAL source files under root_dir."""
    return [goal_file for file in os.walk(root_dir) for goal_file in glob.glob(os.path.join(file[0], ext))]


def lines_in_file(file_path):
    with open(file_path) as f:
        lines = 0
        for _ in f:
            lines += 1
        return lines


def print_table(game, stats, total_gc_files):
    total_lines = 0
    print("| {: <24} | {: <6} |".format("file name", "lines"))
    print("-------------------------------------")
    for x in stats:
        print("  {: <24} | {: >6} |".format(x[0], x[1]))
        total_lines += x[1]
    print("-------------------------------------")
    print("| {: <24} | {: >6} |".format("TOTAL", total_lines))
    print("-------------------------------------")
    estimated_lines = 500000
    if game == "jak2":
      estimated_lines = 1000000
    elif game == "jak3":
      estimated_lines = 1200000
    print("Progress: {}/{} lines ({:.2f}%)".format(total_lines, estimated_lines, 100. * total_lines / estimated_lines))
    print("{}/{} files modified from template ({:.2f}%)".format(len(stats), total_gc_files,
                                                                100. * len(stats) / total_gc_files))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(dest='game', help='game name')
    args = parser.parse_args()
    gsrc_path = "../goal_src/" + args.game
    all_files = get_goal_files(gsrc_path)

    ref_files = get_goal_files("../test/decompiler/reference/" + args.game, "*_REF.gc")
    ref_files_no_ext = [os.path.basename(fn)[:-7] for fn in ref_files]

    file_stats = []
    total_gc_files = 0
    excluded_files = {"game_dgos.gc", "all_files.gc", "goal-lib.gc", "ocean-trans-tables.gc", "ocean-frames.gc",
                      "ocean-tables.gc", "art-elts.gc", "joint-nodes.gc", "compiler-setup.gc", "kernel-defs.gc"}
    excluded_paths = {"pc", "examples", "old"}
    for path in excluded_paths:
        files = get_goal_files(gsrc_path + "/" + path)
        for fn in files:
            excluded_files.add(os.path.basename(fn))
    modified = set()

    for fn in all_files:
        short_name = os.path.basename(fn)
        line_count = lines_in_file(fn)

        if short_name in excluded_files:
            continue

        total_gc_files += 1

        if line_count < 10 or short_name in excluded_files:
            # the template has 7 lines, just skip it.
            continue

        file_stats.append((short_name, line_count))
        modified.add(short_name[:-3])

    file_stats.sort(key=lambda x: x[1])


    missing_ref_files = modified - set(ref_files_no_ext)

    print("Missing ref files:")
    for fn in missing_ref_files:
        print(" {}".format(fn))
    print_table(args.game, file_stats, total_gc_files)

if __name__ == "__main__":
    main()
