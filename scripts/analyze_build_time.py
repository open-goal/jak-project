# this script is used to analyze the output of the build when running
# export CXX="/usr/bin/time -p g++"; cmake ..
# you must run this on a fresh build.

# This only works on linux and relies on the output of cmake looking a certain way

import argparse
import re

def get_time(line):
	return float(line.split()[-1])

def parse_file(lines):
	scanning_pattern = re.compile("Scanning dependencies of target \\w+\\n")
	building_cxx_pattern = re.compile("\\[....\\] Building CXX object .+\\n")
	current_target = "UNKNOWN"
	all_builds = []
	time_by_target = dict()
	count_by_target = dict()
	total_real_time = 0.0
	for i in range(len(lines)):
		if(scanning_pattern.match(lines[i])):
			current_target = lines[i][:-1].split()[-1]
			print("current_target is {}".format(current_target))
			if current_target not in time_by_target:
				time_by_target[current_target] = 0.0
				count_by_target[current_target] = 0
		elif(building_cxx_pattern.match(lines[i])):
			obj = lines[i][:-3].split('/')[-1]
			real_time = get_time(lines[i+1])
			user_time = get_time(lines[i+2])
			sys_time = get_time(lines[i+3])
			i += 3
			print("  building cxx is {}: {}, {}, {}".format(obj, real_time, user_time, sys_time))
			all_builds.append((obj, real_time, user_time, sys_time))
			total_real_time += real_time
			time_by_target[current_target] += real_time
			count_by_target[current_target] += 1

	print("Total build time: {}".format(total_real_time))
	for k, v in count_by_target.items():
		print("{}, {}".format(k, v))

	print("-----------------------------------")
	for k, v in time_by_target.items():
		print("{}, {}".format(k, v))
 


def main():
	parser = argparse.ArgumentParser()
	parser.add_argument(dest = 'input', help = 'Input text file.')
	args = parser.parse_args()

	with open(args.input, "r") as f:
		parse_file(f.readlines())

if __name__ == "__main__":
	main()
