#! /usr/bin/env bash
#
# File: regenerate.sh
#
# Created:  7 November 2024
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

# [[ -z "${IN_NIX_SHELL-}" ]]

if ! which tree-sitter >/dev/null 2>/dev/null && which nix >/dev/null 2>/dev/null; then
    echo "Building via nix"
    echo nix shell nixpkgs#nodejs nixpkgs#tree-sitter --command "$0"
    exec nix shell nixpkgs#nodejs nixpkgs#tree-sitter --command "$0"
fi

# nix shell nixpkgs#nodejs nixpkgs#tree-sitter --command
tree-sitter generate "${@}"
tree-sitter generate --build -o hsc/src hsc/grammar.js "${@}"

#tree-sitter generate --build

exit 0

