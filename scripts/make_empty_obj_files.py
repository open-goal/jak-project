#!/usr/bin/env python3

import argparse
import glob
import os

from jak1_file_list import file_list

def dgo_names_string(names):
	result = ""
	for x in names:
		result += x + ", "
	result = result[:-2]
	return result

def make_file(root, path, name, name_in_dgo, dgos, version):
	os.makedirs(os.path.join(root, path), exist_ok=True)
	if version == 3:
		filename = name + ".gc"
		text = """;;-*-Lisp-*-
(in-package goal)

;; name: {}
;; name in dgo: {}
;; dgos: {}

""".format(filename, name_in_dgo, dgo_names_string(dgos))
	elif version == 4:
		filename = name + ".gd"
		text = """;;-*-Lisp-*-
;; GOAL Data Description File

;; name: {}
;; name in dgo: {}
;; dgos: {}
""".format(filename, name_in_dgo, dgo_names_string(dgos))

	with open(os.path.join(root, path, filename), "w") as f:
		f.write(text)	



def main():
	parser = argparse.ArgumentParser()
	parser.add_argument(dest='root', help='root directory to create output in')
	args = parser.parse_args()
	for x in file_list:
		make_file(args.root, x[4], x[0], x[1], x[3], x[2])



if __name__ == "__main__":
	main()
