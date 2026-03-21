#! /usr/bin/env bash
#
# File: test-with-asserts.sh
#
# Created: 11 January 2026
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

# Don’t load elc files because they have asserts disabled
export EMACS_SKIP_ELC=1
export EMACS_FORCE_PRISTINE=1
# export EMACS=emacs-bytecode
"$(dirname "$(readlink -f "$0")")/tests/run-tests.sh" "${@}"

exit 0

