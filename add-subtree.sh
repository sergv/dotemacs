#! /usr/bin/env bash
#
# File: add-subtree.sh
#
# Created: 22 July 2017
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

if [[ "$#" != 3 ]]; then
    echo "usage: $0 <subtree-path> <repo-url> <commit>" >&2
    exit 1
fi

source subtree-utils.sh

subtree_path="$1"
repo_url="$2"
commit="$3"

path="${subtree_path}"

hash_before_commit=$(get-ref-hash HEAD)

git remote add -f "$(basename "${subtree_path}")-repo" "$repo_url"
git read-tree --prefix=$path -u "$commit"
git commit -m "Merge commit '$(get-ref-hash "$commit")' from '$repo_url' into '$path'"

hash_after_commit=$(get-ref-hash HEAD)
subtree_hash_to_merge=$(get-ref-hash "$commit")

add_grafts_file_entry "${path}" "${hash_after_commit}" "${hash_before_commit}" "${subtree_hash_to_merge}"

exit 0

