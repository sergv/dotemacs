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

source "$(dirname "$(readlink -f "$0")")/utils.sh"

native_comp="$(native-comp-available)"

if [[ "$native_comp" = "nil" ]]; then
    emacs-pristine \
        --batch --quick --no-window-system --load src/dump.el --eval '(progn (dump-main "/home/sergey/.emacs.d"))'
fi

exit 0

