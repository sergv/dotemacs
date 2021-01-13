#! /usr/bin/env bash
#
# File: recompile-native.sh
#
# Created: 19 January 2020
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

emacs_dir=${1:-"${EMACS_ROOT}"}

# echo "Removing old *.eln files"
# find -O3 . -name '*.eln' -delete

export PATH="/home/sergey/projects/emacs/gcc-9.2-jit/bin:$PATH"
export CC=/home/sergey/projects/emacs/gcc-9.2-jit/bin/gcc
export LD_LIBRARY_PATH=/home/sergey/projects/emacs/gcc-9.2-jit/lib:/home/sergey/projects/emacs/gcc-9.2-jit/lib64:/home/sergey/projects/emacs/gcc-9.2-jit/lib/gcc/x86_64-pc-linux-gnu/9.2.1/
export LDFLAGS="-L/home/sergey/projects/emacs/gcc-9.2-jit/lib -L/home/sergey/projects/emacs/gcc-9.2-jit/lib64 -L/home/sergey/projects/emacs/gcc-9.2-jit/lib/gcc/x86_64-pc-linux-gnu/9.2.1/"

emacs --batch --load src/recompile.el --eval "(recompile-native \"$emacs_dir\")"

exit 0

