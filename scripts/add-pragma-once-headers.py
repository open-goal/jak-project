#!/usr/bin/python

import sys
import os
from glob import glob


folders = sys.argv
cwd = os.getcwd()

for folder in folders:
  directory = os.path.join(cwd, folder)
  files = [y for x in os.walk(directory) for y in glob(os.path.join(x[0], '*.h'))]
  for fPath in files:
    print("Processing %s" % fPath)
    with open(fPath, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        if (content.startswith("#pragma once") == False):
          f.write("#pragma once\n\n" + content)

