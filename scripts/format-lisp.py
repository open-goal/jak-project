from pathlib import Path
import glob
from ctypes import c_int
from subprocess import call
import multiprocessing.dummy as mp
from multiprocessing import Value, Lock, Process

# Assumed to be ran from the root directory
# this formats all *.gs and *.gc files in the repository!
# TODO:
# - dry-run (CI check for formatting)
# - fix mode (actually apply formatting)

lisp_files = []
for path in glob.glob("**/*.gs", recursive=True):
    lisp_files.append(path)
for path in glob.glob("**/*.gc", recursive=True):
    lisp_files.append(path)

print("Found {} files to format!".format(len(lisp_files)))

counter = Value(c_int)  # defaults to 0
counter_lock = Lock()


def status_update(path):
    with counter_lock:
        counter.value += 1
        print("[{}/{}] Formatted {}".format(counter.value, len(lisp_files), path))


# TODO - call isn't actually formatting the file, it seems to be opening emacs fine but...doesn't actually do anything!
def format_file(path):
    call(
        [
            "emacs",
            "-batch",
            "-l",
            "./scripts/emacs/load-srefactor.el",
            path,
            "--eval",
            "'(srefactor-lisp-format-buffer)'",
            "-f",
            "save-buffer",
        ]
    )
    status_update(path)


# TODO - multi-threading opening emacs jobs
# in the future, it would be probably WAY faster to just get emacs to open/save the files (pass it a list)
# p = mp.Pool(8)
# p.map(format_file, lisp_files)
# p.close()
# p.join()
