#! /usr/bin/env bash
#
# File: merge-changes.sh
#
# Created: 23 July 2017
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

echo "[INFO] Read the script to understand what to do, it's currently in semi-runnable state"
echo "[INFO] Don't forget to ensure that grafts are in place at .git/info/grafts"
echo "--------------------------------------------------------------------------------"
# exit 1

if [[ "$#" != 3 ]]; then
    echo "usage: $0 <url> <subtree-path> <commit>"
    exit 1
fi

source subtree-utils.sh

url="$1"
subtree_path="$2"
commit="$3"

# # Automatically infers subtree name
# git merge --strategy subtree --allow-unrelated-histories --squash --no-commit "$commit"

# This version causes conflicts because histories are unrelated. This requires adding grafts in
# .git/info/grafts so that merge will be successful.

path="$subtree_path"

if [[ ! -d "$path" ]]; then
    echo "Subtree does not exist: '$path'" >&2
    exit
fi


hash_before_merge=$(get-ref-hash HEAD)

# Comment this out to skip merging.
git merge --strategy recursive --strategy-option "subtree=$path" --allow-unrelated-histories --squash --no-commit "$commit"

subtree_hash_to_merge_pretty=$(get-ref-hash-pretty "$commit")
subtree_hash_to_merge=$(get-ref-hash "$commit")
git commit -m "Merge commit $subtree_hash_to_merge_pretty from '$url' into '$path'"

hash_after_merge=$(get-ref-hash HEAD)

add_grafts_file_entry "${path}" "${hash_after_merge}" "${hash_before_merge}" "${subtree_hash_to_merge}"

exit 0

