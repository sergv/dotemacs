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
set -e

export EMACS_FORCE_PRISTINE=1
emacs="${EMACS:-emacs}"

source "$(dirname "$(readlink -f "$0")")/utils.sh"

native_comp="$(native-comp-available)"

"$emacs" \
    --batch --quick --no-window-system -L src --load src/dump.el --eval "(progn (dump-main \"~/.emacs.d\" \"~/.emacs.d/compiled/$(basename "$emacs").dmp\"))"

exit 0

