#! /usr/bin/env bash
#
# File: record-merge-commit-in-grafts.sh
#
# Created: 22 July 2017
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

if [[ "$#" != 1 ]]; then
    echo "usage: $0 <commit>" >&2
    exit 1
fi

source subtree-utils.sh

commit="$1"

merge_hash=$(get-ref-hash "${commit}")
merge_parent_hash=$(get-ref-hash "${commit}^")

merged_commit_path=$(get-commit-message "$commit" | sed -r -e "s/^Merge commit '([0-9a-fA-F]+)[^']*' from '([^']+)' into '([^']+)'$/\3/")

repo_url=$(get-commit-message "$commit" | sed -r -e "s/^Merge commit '([0-9a-fA-F]+)[^']*' from '([^']+)' into '([^']+)'$/\2/")
repo_name="${merged_commit_path#third-party/}"

repo_name_in_repository="${repo_name}-repo"
if ! git remote | grep -q "$repo_name_in_repository"; then
    echo "Remote not added: $repo_name_in_repository. Adding"
    git remote add -f "$repo_name_in_repository" "$repo_url"
fi

merged_commit_hash_abbrev=$(get-commit-message "$commit" | sed -r -e "s/^Merge commit '([0-9a-fA-F]+)[^']*' from '([^']+)' into '([^']+)'$/\1/")
merged_commit_hash=$(get-ref-hash "${merged_commit_hash_abbrev}")

echo "merge_hash = ${merge_hash}"
echo "merge_parent_hash = ${merge_parent_hash}"
echo "merged_commit_hash = ${merged_commit_hash}"
echo "merged_commit_path = ${merged_commit_path}"

add_grafts_file_entry "${merged_commit_path}" "${merge_hash}" "${merge_parent_hash}" "${merged_commit_hash}"

# hash_before_commit=$(get-ref-hash HEAD)
#
# git remote add -f "${subtree_name}-repo" "$repo_url"
# git read-tree --prefix=third-party/$subtree_name -u "$commit"
# git commit -m "Merge commit '$commit' from '$repo_url' into '$path'"
#
# hash_after_commit=$(get-ref-hash HEAD)
# subtree_hash_to_merge=$(get-ref-hash "$commit")
#
# add_grafts_file_entry "${path}" "${hash_after_commit}" "${hash_before_commit}" "${subtree_hash_to_merge}"

exit 0

