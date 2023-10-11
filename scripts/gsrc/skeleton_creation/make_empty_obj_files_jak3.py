#!/usr/bin/env python3

import argparse
import os
from pathlib import Path
import json

file_list = None

with open('../../../goal_src/jak3/build/all_objs.json', 'r') as f:
  file_list = json.load(f)

def dgo_names_string(names):
	result = ""
	for x in names:
		result += x + ", "
	result = result[:-2]
	return result

def is_code_file(name, dgos):
	return not name.endswith("-ag") and not name.endswith("-vis") and not name.startswith("tpage-") and not name.endswith("COMMON") and not dgos[0].lower() == name and not name.endswith("-tx")

def make_file(root, path, name, name_in_dgo, dgos, version):
	if is_code_file(name, dgos):
		filename = name + ".gc"
		text = """;;-*-Lisp-*-
(in-package goal)

;; name: {}
;; name in dgo: {}
;; dgos: {}

;; DECOMP BEGINS

""".format(filename, name_in_dgo, dgo_names_string(dgos))
		Path(os.path.join(root, path)).mkdir(parents=True, exist_ok=True)
		with open(os.path.join(root, path, filename), "w") as f:
			f.write(text)
		# print("make_file {}/{}/{} {} {}".format(root, path, filename, name_in_dgo, dgos))

for x in file_list:
	make_file("../../../goal_src/jak3", x[4], x[0], x[1], x[3], x[2])

