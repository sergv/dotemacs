#! /usr/bin/env bash
#
# File: utils.sh
#
# Created:  6 July 2022
#

function native-comp-available() {
    "$emacs" -Q --batch \
          --eval "(message \"%s\" (and (fboundp #'native-comp-available-p) (native-comp-available-p)))" 2>&1
}

