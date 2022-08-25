import argparse
import zipfile

parser = argparse.ArgumentParser()
parser.add_argument("--file")
parser.add_argument("--out")
args = parser.parse_args()

with zipfile.ZipFile(args.file, 'r') as p2s:
  p2s.extractall(args.out)
