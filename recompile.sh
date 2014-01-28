#!/bin/bash
#
# File: recompile.sh
#
# Created: Tuesday, 11 September 2012
#

DIR=$(dirname $(readlink -f "$0"))

function update-dir-autoloads {
    local dir="$1"
    local name="$2"
    emacs --batch --eval "(progn (setq generated-autoload-file \"$DIR/$dir/$name\") (update-directory-autoloads \"$DIR/$dir\"))"
}

update-dir-autoloads "standalone/clojure-mode" "clojure-mode-autoload.el"
update-dir-autoloads "standalone/nrepl.el" "nrepl-autoload.el"
update-dir-autoloads "third-party/haskell-mode" "haskell-mode-autoloads.el"

emacs --batch --load recompile.el -f recompile-main


exit 0

