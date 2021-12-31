#! /usr/bin/env bash
#
# File: rebuild.sh
#
# Created: 13 January 2021
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

#./scripts/dump.sh &&
./scripts/recompile.sh && ./tests/run-tests.sh

exit 0

