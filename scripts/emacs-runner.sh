#!/bin/bash

if [[ ! -z "${EMACS_ROOT+x}" ]]; then
    dump_file="$EMACS_ROOT/emacs.dmp"
else
    dump_file="$HOME/.emacs.d/emacs.dmp"
fi

if [[ ! -f "$dump_file" || ! -z "${EMACS_FORCE_PRISTINE+x}" ]]; then
    echo "Starting pristine Emacs"
    emacs-pristine "${@}"
else
    emacs-pristine --dump-file "$dump_file" "${@}"
fi
