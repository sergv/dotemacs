#
# File: subtree-utils.sh
#
# Created: 17 September 2017
#

grafts_file=".git/info/grafts"

if [[ ! -f "$grafts_file" ]]; then
    echo "Grafts file does not exist: $grafts_file, creating" >&2
    touch "$grafts_file"
fi


function get-commit-message () {
    local hash="$1"
    git show --no-patch --format='%s' "$hash"
}

function get-ref-hash () {
    local ref="$1"
    git show --no-patch --format='%H' "$ref" | tail -n1
}

function get-ref-hash-pretty () {
    local ref="$1"
    if git describe --tags --exact-match "$ref" >/dev/null 2>&1; then
        git show --no-patch --format="'%H' (%D)" "$ref" | tail -n1
    else
        git show --no-patch --format="'%H'" "$ref" | tail -n1
    fi
}

function add_grafts_file_entry () {
    local path="$1"
    local hash_after_merge="$2"
    local hash_before_merge="$3"
    local subtree_hash_to_merge="$4"

    cat >>"${grafts_file}" <<EOF

#-------------------------------------------------------------------------------
# ${path}
# <commit-hash> - <explanation>
# ${hash_after_merge} - $(get-commit-message "$hash_after_merge")
# ${hash_before_merge} = original parent
# ${subtree_hash_to_merge} - merged commit
${hash_after_merge} ${hash_before_merge} ${subtree_hash_to_merge}

EOF
    echo "Updated grafts file at ${grafts_file}"
}
