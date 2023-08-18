#! /usr/bin/env bash

dir="$(dirname "$0")"

if [[ ! -z "${EMACS_ROOT+x}" ]]; then
    dump_file="$EMACS_ROOT/compiled/emacs.dmp"
else
    dump_file="$HOME/.emacs.d/compiled/emacs.dmp"
fi

if [[ ! -f "$dump_file" || ! -z "${EMACS_FORCE_PRISTINE+x}" ]]; then
    "$dir/emacs-pristine" "${@}"
else
    "$dir/emacs-pristine" --dump-file "$dump_file" "${@}"
fi
