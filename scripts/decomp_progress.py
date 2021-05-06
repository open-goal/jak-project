import os
import glob
import argparse

### Script to track decompilation progress.
### Example usage: python3 scripts/decomp_progress.py ~/jak-project/goal_src

def get_goal_files(root_dir):
	"""Get all GOAL source files under root_dir."""
	return [goal_file for file in os.walk(root_dir) for goal_file in glob.glob(os.path.join(file[0], '*.gc'))]

def lines_in_file(file_path):
	with open(file_path) as f:
		lines = 0
		for _ in f:
			lines += 1
		return lines


def print_table(stats, total_gc_files):
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
	print("Progress: {}/{} lines ({:.2f}%)".format(total_lines, estimated_lines, 100. * total_lines / estimated_lines))
	print("{}/{} files modified from template ({:.2f}%)".format(len(stats), total_gc_files, 100. * len(stats)/total_gc_files))



def main():
	parser = argparse.ArgumentParser()
	parser.add_argument(dest='goal_src', help='the goal_src folder')
	args = parser.parse_args()
	all_files = get_goal_files(args.goal_src)

	file_stats = []
	total_gc_files = 0
	excluded_files = {"all_files.gc", "goal-lib.gc", "ocean-trans-tables.gc", "ocean-frames.gc", "ocean-tables.gc"}


	for fn in all_files:
		short_name = os.path.basename(fn)
		line_count = lines_in_file(fn)

		if short_name in excluded_files:
			continue

		total_gc_files += 1

		if line_count == 7 or short_name in excluded_files:
			# the template has 7 lines, just skip it.
			continue

		file_stats.append((short_name, line_count))
	file_stats.sort(key=lambda x: x[1])

	print_table(file_stats, total_gc_files)


if __name__ == "__main__":
	main()
