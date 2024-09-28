#! /usr/bin/env bash
#
# File: update-third-party.sh
#
# Created:  3 October 2017
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

if [[ "$#" != 1 && "$#" != 2 && "$#" != 3 ]]; then
    echo "usage: $0 <subtree-path> [<repository> [<revision>]]"
    exit 1
fi

subtree_path="$1"

. subtree-utils.sh

if [[ ! -d "$subtree_path" ]]; then
    echo "Subree does exist: $subtree_path" >&2
    exit 1
fi

repository=${2:-$(basename "${subtree_path}")-repo}

if ! git remote | grep -q -F "$repository"; then
    echo "No such repository: '$repository'" >&2
    exit 1
fi

url=$(git remote show "$repository" | awk 'NR > 1' | sed -r 's,.*URL: ((https?://|git@).*\.git)$,\1,' | head -n 1)
if [[ $? != 0 ]]; then
    echo "Failed to get repository url" >&2
fi

if [[ "$#" == 3 ]]; then
    hash="$3"
elif [[ "${repository}" == "magit-repo" || "${repository}" == "with-editor-repo" || "${repository}" == "transient-repo" || "${repository}" == "el-patch-repo" || "${repository}" == "git-modes-repo" || "${repository}" == "ghub-repo"  || "${repository}" == "kotlin-ts-mode-repo" || "${repository}" == "tree-sitter-kotlin-repo" ]]; then
    hash="$(get-ref-hash "${repository}/main")"
else
    hash="$(get-ref-hash "${repository}/master")"
fi

echo "url = ${url}"
echo "hash = ${hash}"
bash merge-changes.sh "$url" "$subtree_path" "$hash"

exit 0

