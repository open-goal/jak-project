import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--game")
args = parser.parse_args()
import os
import glob

def delete_extension(ext):
  fileList = glob.glob('./decompiler_out/{}/*.{}'.format(args.game, ext))
  for filePath in fileList:
    os.remove(filePath)

delete_extension("gc")
delete_extension("asm")
