#! /usr/bin/env bash
#
# File: dump.sh
#
# Created:  7 September 2020
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

# ~/projects/emacs/installed/local-27.1-debug/bin/emacs \
# /home/sergey/projects/emacs/emacs/src/emacs \
emacs-pristine \
    --batch --quick --no-window-system --load src/dump.el --eval '(progn (dump-main "/home/sergey/.emacs.d"))'
# emacs

exit 0

