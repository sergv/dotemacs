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
export EMACS_DEBUG=0
emacs="${EMACS:-emacs}"

emacs_dir=${1:-"${EMACS_ROOT:-.}"}

"$emacs" \
    --batch --quick --no-window-system \
    -L "${emacs_dir}/src" \
    --load "${emacs_dir}/src/dump.el" \
    --eval "(dump-main \"${emacs_dir}\" \"${emacs_dir}/compiled/$(basename "$emacs").dmp\")"

exit 0

