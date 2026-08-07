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

if [[ -z "${2:-}" ]]; then
    export EMACS_FORCE_PRISTINE=1
    export EMACS_DEBUG=0
    emacs="${EMACS:-emacs}"
    snapshot_name="$(basename "$emacs").dmp"
else
    emacs="${2}"
    snapshot_name="${3}"
fi

emacs_dir=${1:-"${EMACS_ROOT:-.}"}

dump_src=""
for tmp in "${emacs_dir}/src/dump.el" "${emacs_dir}/src/dump.el.gz"; do
    if [[ -f "$tmp" ]]; then
        dump_src="$tmp"
        break
    fi
done

if [[ -z "$dump_src" || ! -f "$dump_src" ]]; then
    echo "Unable to locate dump.el" >&2
    exit 1
fi

"$emacs" \
    --batch --quick --no-window-system \
    -L "${emacs_dir}/src" \
    --load "$dump_src" \
    --eval "(dump-main \"${emacs_dir}\" \"${emacs_dir}/compiled/${snapshot_name}\")"

exit 0

