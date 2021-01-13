#!/usr/bin/env python
# encoding: utf-8
"""
File:        make-dist-el.py
Created:     Friday,  9 March 2012
Author:      Sergey Vinokurov
Description:
"""

from __future__ import print_function, division

import tarfile
import sys, os
import re
import argparse
import datetime

# from utils_common import *

#### constants

EXT = "bz2"
assert EXT in ["gz", "bz2"], "invalid EXT"

birthday = datetime.date(1991, 07, 03)
today = datetime.date.today()
days = (today - birthday).days
print("days = {}".format(days))
default_archive_name = datetime.datetime.today().strftime(
    "emacs-el-" + "{:09d}".format(days) + "--%Hh-%d-%b-%Y.tar." + EXT)
#### arguments

parser = argparse.ArgumentParser(description="Create distribution of *.el files")

parser.add_argument("-d",
                    "--dest",
                    action  = "store",
                    type    = str,
                    default = os.getcwd(),
                    help    = "Directory to store resulting archive")
# optional positional argument
parser.add_argument("name",
                    action  = "store",
                    type    = str,
                    default = default_archive_name,
                    nargs   = "?",
                    help    = "Archive name, {0} by default".format(default_archive_name))
parser.add_argument("-v", "--verbose",
                    action  = "store_true",
                    default = False,
                    help    = "Be verbose and don't create archive")
parser.add_argument("-f",
                    "--file",
                    nargs   = "*",
                    dest    = "files",
                    default = [],
                    help    = "Additional files to add to archive")
parser.add_argument("--include-doc",
                    dest    = "include_documentation",
                    action  = "store_true",
                    default = False,
                    help    = "Include documentation into archive")
parser.add_argument("--include-git",
                    dest    = "include_git",
                    action  = "store_true",
                    default = False,
                    help    = "Include git repositories into archive")


args = parser.parse_args()

####


if not args.name[len(args.name) - (len(".tar.") + len(EXT)):] == ".tar." + EXT:
    print("""name "{}" must end in .tar.{}""".format(args.name, EXT))
    sys.exit(1)

def split_path(p):
    p = os.path.normpath(p)
    result = []
    while os.path.basename(p):
        result.append(os.path.basename(p))
        p = os.path.dirname(p)
    result.reverse()
    return result

def is_excluded_dir(abs_path):
    EXCLUDED_DIRS = [ "misc/build"
                    , "misc/emacs-dev"
                    , ".cask"
                    , "local"
                    , "local-24.5"
                    , "local-25.0.94"
                    , "packages"
                    , "prog-data/elpa"
                    , "prog-data/packages"
                    , "prog-data/elisp-packages"
                    , "prog-data/backup"
                    , "prog-data/imaxima-tmp"
                    , "prog-data/auto-save-alist"
                    # These extras contain nothing useful.
                    , "third-party/yasnippet/extras"
                    , "tmp/emacs"
                    , "experimental"
                    ]
    if not args.include_documentation:
        EXCLUDED_DIRS.append("doc")

    directory = os.path.relpath(abs_path, EMACS_DIR)
    return os.path.isdir(abs_path) and \
      any(map(lambda p: directory.startswith(p), EXCLUDED_DIRS))

def is_subseq(subseq, seq):
    """Testh whether subseq is subsequence fo seq, both of whith shoulb be some
    sort of seuences."""
    if len(subseq) > len(seq):
        return False
    for i in range(len(seq) - len(subseq) + 1):
        if all(map(lambda j: subseq[j] == seq[i + j], range(len(subseq)))):
            return True
    return False

def is_included_file(abs_path):
    INCLUDED_EXTENSIONS = \
    map(lambda x: "." + x,
        ["el", "cmd", "sh", "snip", "txt",
         "clj", "org", "py", "hs", "ttf", "rnc"
         # , "elc"
        ])
    INCLUDED_FILENAMES = [".emacs",
                          ".gitignore",
                          ".gitattributes",
                          "persistent-store",
                          "saveplace",
                          "recentf",
                          "pylookup.db"]
    # EXCLUDED_FILENAMES = []
    EXCLUDED_ABS_FILENAME_RES = [r"^.*diary\.org$",
                                 r"^.*\.emacs\.elc$",
                                 r"^.*emacs-dev\.tar\.gz$",
                                 r"^.*backup.*\.tar\.(gz|bz2|7z)$",
                                 r"^(.*/)?prog-data/tmp/.*$",
                                 r"^(.*/)?prog-data/image-dired/.*$"]

    included_dirs_subseqs = set([# (".git",),
                                 ("drills",),
                                 ("execs",),
                                 ("experimental",),
                                 ("prog-data", "auto-insert"),
                                 ("prog-data", "eshell"),
                                 ("prog-data", "snippets"),
                                 ("scripts",),
                                 ("tests",),
                                 ("tmp",)])

    excluded_dirs_subseqs = set()
    if not args.include_git:
        excluded_dirs_subseqs.add((".git",))


    abs_dir, filename = os.path.split(abs_path)
    abs_path_parts = abs_dir.split("/")
    extension = os.path.splitext(filename)[1]

    return ((filename in INCLUDED_FILENAMES)  or
            (extension in INCLUDED_EXTENSIONS) or
            any(map(lambda path_subseq: is_subseq(path_subseq, abs_path_parts),
                    included_dirs_subseqs))) and \
        all(map(lambda path_subseq: not is_subseq(path_subseq, abs_path_parts),
                excluded_dirs_subseqs)) and \
        not (any(map(lambda r: re.match(r, abs_path), EXCLUDED_ABS_FILENAME_RES)))

def collect_files(root, file_pred=lambda p: True, dir_visit_pred=lambda p: True):
    result = []
    if dir_visit_pred(root):
        for p in os.listdir(root):
            path = os.path.join(root, p)
            if os.path.isfile(path) and file_pred(path):
                result.append(path)
            elif os.path.isdir(path) and dir_visit_pred(path):
                files = collect_files(path,
                                      file_pred=file_pred,
                                      dir_visit_pred=dir_visit_pred)
                result = result + files
    return result

os.chdir(EMACS_DIR)

# note: these directories will be simply created, their content will not make
# its way into archive
INCLUDED_DIRS = ["doc",
                 "prog-data/auto-insert",
                 "prog-data/image-dired",
                 "prog-data/themes",
                 "prog-data/tmp",
                 "prog-data/tmp/render-formula",
                 "windows"]

names = set()

for name in args.files:
    if not os.path.exists(name) or not os.path.isfile(name):
        raise RuntimeError, "Invalid file name {}".format(name)

for name in map(lambda p: os.path.relpath(p, EMACS_DIR),
                collect_files(EMACS_DIR,
                              file_pred=is_included_file,
                              dir_visit_pred=lambda p: not is_excluded_dir(p))):
    names.add((name, None))

for name in args.files:
    n = os.path.relpath(name, EMACS_DIR)
    names.add((n, None))

for name in INCLUDED_DIRS:
    full_name = os.path.join(EMACS_DIR, name)
    if not (os.path.exists(full_name) and os.path.isdir(full_name)):
        print("nonexistent directory {} in INCLUDED_DIRS".format(name))
        continue
    n = os.path.relpath(name, EMACS_DIR)
    t = tarfile.TarInfo(n)
    t.type = tarfile.DIRTYPE
    t.mode = 0777
    names.add((n, t))


if args.verbose:
    print("archive: " + os.path.join(args.dest, args.name))
    for name, tinfo in sorted(names, key = lambda x: x[0]):
        print(name)
else:
    archive = tarfile.open(os.path.join(args.dest, args.name), "w:" + EXT)
    for name, tinfo in sorted(names, key = lambda x: x[0]):
        if tinfo is None:
            archive.add(name)
        else:
            archive.addfile(tinfo)
    archive.close()


