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

# nix shell nixpkgs#nodejs nixpkgs#tree-sitter --command
tree-sitter generate "${@}"
tree-sitter generate --build -o hsc/src hsc/grammar.js "${@}"

#tree-sitter generate --build

exit 0

