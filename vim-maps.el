;;; vim-maps.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description:

;; This file contains all standard keymaps.  Key mappings are defined
;; using one of the following vim-like macros:

;;   - vim:map ... general mapping in an arbitrary mode
;;   - vim:nmap ... mapping in the normal-mode keymap
;;   - vim:omap ... mapping in the operator-pending keymap
;;   - vim:imap ... mapping in the insert-mode keymap
;;   - vim:vmap ... mapping in the visual-mode keymap
;;
;; Commands should usually be placed in the normal-mode keymap.
;; Motions should be placed in the operator-pending keymap. All
;; commands in the operator-pending-keymap are available as
;; operator-pending in normal-mode and visual-mode (but may be
;; overwritten by the corresponding keymaps) and as motion-arguments
;; for complex commands in normal-mode.
;;
;; A mapping has one of the following two forms:
;;
;;   (vim:map KEYEVENTS 'my-command)
;;
;;   (vim:map KEYEVENTS MAPEVENTS)
;;
;; The first form maps the events in KEYEVENTS to the command
;; my-command.  The second form defines a vim-like mapping of
;; KEYEVENTS to MAPEVENTS, i.e. the activation of KEYEVENTS invokes
;; the (key-)events in MAPEVENTS.
;;
;; Events is a usual Emacs-sequence of events as it would be used by
;; define-key.


;; TODO:
;;   - better mapping to support stuff like
;;     (vim:def-map "c" "d{motion}a")
;;
;;   - because of this, mapping "cc" to "0C" does not work with a
;;     count since the count is eaten by the '0'
;;
;;   - similarily 'o' and 'O' won't work
;;
;;   - mapping to "ESC" to leave insert-mode doesn't work because ESC
;;     is seen as meta-prefix, I think
;;
;;   - should we have a 'deep-mapping' function: for example, "x" is
;;     mapped to "dl" in the default keymap.  If someone decides to
;;     redefine "l" to some other command, "x" will change its
;;     behaviour, too.  A 'deep-mapping' should save the mapping on
;;     definition of "x", therefor let "x" behave as usual even after
;;     redefining "l"
;;
;;   - do we need a special insert-mode-mapping to simulate ESC?
(provide 'vim-maps)

(vim:nmap "\\" 'vim:cmd-emacs)

(vim:omap "0" 'vim:motion-beginning-of-line-or-digit-argument)
(vim:omap "1" 'digit-argument)
(vim:omap "2" 'digit-argument)
(vim:omap "3" 'digit-argument)
(vim:omap "4" 'digit-argument)
(vim:omap "5" 'digit-argument)
(vim:omap "6" 'digit-argument)
(vim:omap "7" 'digit-argument)
(vim:omap "8" 'digit-argument)
(vim:omap "9" 'digit-argument)
;(vim:def-special "\"" 'vim:do-something-funny-with-registers)

(vim:omap "h" 'vim:motion-left)
(vim:omap "l" 'vim:motion-right)
(vim:omap "^" 'vim:motion-first-non-blank)
(vim:omap "$" 'vim:motion-end-of-line)
(vim:omap "g_" 'vim:motion-last-non-blank)

(vim:omap "k" 'vim:motion-up)
(vim:omap "j" 'vim:motion-down)
(vim:omap "-" "k^")
(vim:omap "+" "j^")
(vim:omap "G" 'vim:motion-go-to-first-non-blank-end) 
(vim:omap "gg" 'vim:motion-go-to-first-non-blank-beg) 

(vim:omap "w" 'vim:motion-fwd-word)
(vim:omap "W" 'vim:motion-fwd-WORD)
(vim:omap "e" 'vim:motion-fwd-word-end)
(vim:omap "E" 'vim:motion-fwd-WORD-end)
(vim:omap "b" 'vim:motion-bwd-word)
(vim:omap "B" 'vim:motion-bwd-WORD)
(vim:omap "ge" 'vim:motion-bwd-word-end)
(vim:omap "gE" 'vim:motion-bwd-WORD-end)

(vim:omap "(" 'vim:motion-bwd-sentence)
(vim:omap ")" 'vim:motion-fwd-sentence)
(vim:omap "{" 'vim:motion-bwd-paragraph)
(vim:omap "}" 'vim:motion-fwd-paragraph)
(vim:omap "]]" 'vim:motion-fwd-section)
(vim:omap "][" 'vim:motion-fwd-section)
(vim:omap "[[" 'vim:motion-bwd-section)
(vim:omap "[]" 'vim:motion-bwd-section)

(vim:omap "f" 'vim:motion-find)
(vim:omap "F" 'vim:motion-find-back)
(vim:omap "t" 'vim:motion-find-to)
(vim:omap "T" 'vim:motion-find-back-to)
(vim:omap ";" 'vim:motion-repeat-last-find)
(vim:omap "," 'vim:motion-repeat-last-find-opposite)

(vim:omap "%" 'vim:motion-jump-item)

(vim:omap "iw" 'vim:motion-inner-word)

;(vim:nmap "x" "dl")
(vim:nmap "x" 'vim:cmd-delete-char)
(vim:nmap "D" "d$")
(vim:nmap "d" 'vim:cmd-delete)

(vim:nmap "C" 'vim:cmd-change-rest-of-line)
(vim:nmap "c" 'vim:cmd-change)
(vim:nmap "s" 'vim:cmd-change-char)

(vim:nmap "r" 'vim:cmd-replace-char)
(vim:nmap "R" 'vim:cmd-replace)

(vim:nmap "y" 'vim:cmd-yank)
(vim:nmap "Y" "yy")
(vim:nmap "p" 'vim:cmd-paste-behind)
(vim:nmap "P" 'vim:cmd-paste-before)

(vim:nmap "J" 'vim:cmd-join-lines)

(vim:nmap "/" 'vim:search-start)
(vim:nmap "?" 'vim:search-start-backward)
(vim:nmap "*" 'vim:search-word)
(vim:nmap "#" 'vim:search-word-backward)
(vim:omap "g*" 'vim:search-unbounded-word)
(vim:omap "g#" 'vim:search-unbounded-word-backward)
(vim:nmap "n" 'vim:search-repeat)
(vim:nmap "N" 'vim:search-repeat-opposite)
;; The next two maps are very special for an active search.
(vim:map "n" 'vim:search-repeat :keymap vim:search-mode-keymap)
(vim:map "N" 'vim:search-repeat-opposite :keymap vim:search-mode-keymap)

(vim:nmap "i" 'vim:cmd-insert)
(vim:nmap "a" 'vim:cmd-append)
(vim:nmap "I" 'vim:cmd-Insert)
(vim:nmap "A" 'vim:cmd-Append)
(vim:nmap "o" 'vim:cmd-insert-line-below)
(vim:nmap "O" 'vim:cmd-insert-line-above)

(vim:nmap "u" 'vim:cmd-undo)
(vim:nmap (kbd "C-r") 'vim:cmd-redo)

(vim:nmap "." 'vim:cmd-repeat)

(vim:nmap "=" 'vim:cmd-indent)
(vim:nmap "<" 'vim:cmd-shift-left)
(vim:nmap ">" 'vim:cmd-shift-right)

(vim:nmap "~" "g~l")
(vim:omap "g~" 'vim:cmd-toggle-case)
(vim:omap "gU" 'vim:cmd-make-upcase)
(vim:omap "gu" 'vim:cmd-make-downcase)

(vim:omap (kbd "C-e") 'vim:scroll-line-down)
(vim:omap (kbd "C-d") 'vim:scroll-down)
(vim:omap (kbd "C-f") 'vim:scroll-page-down)
(vim:omap "z+" 'vim:scroll-bottom-line-to-top)

(vim:omap (kbd "C-y") 'vim:scroll-line-up)
(vim:omap (kbd "C-u") 'vim:scroll-up)
(vim:omap (kbd "C-b") 'vim:scroll-page-up)
(vim:omap "z^" 'vim:scroll-top-line-to-bottom)

(vim:omap "zt" 'vim:scroll-line-to-top)
(vim:omap (vconcat "z" [return]) "zt^")
(vim:omap "zz" 'vim:scroll-line-to-center)
(vim:omap "z." "z.^")
(vim:omap "zb" 'vim:scroll-line-to-bottom)
(vim:omap "z-" "zb^")


(vim:nmap (kbd "C-w +") 'vim:window-increase-height)
(vim:nmap (kbd "C-w -") 'vim:window-decrease-height)
(vim:nmap (kbd "C-w =") 'vim:window-balance)
(vim:nmap (kbd "C-w >") 'vim:window-increase-width)
(vim:nmap (kbd "C-w <") 'vim:window-decrease-width)
(vim:nmap (kbd "C-w H") 'vim:window-move-far-left)
(vim:nmap (kbd "C-w J") 'vim:window-move-very-bottom)
(vim:nmap (kbd "C-w K") 'vim:window-move-very-top)
(vim:nmap (kbd "C-w L") 'vim:window-move-far-right)
(vim:nmap (kbd "C-w R") 'vim:window-rotate-upwards)
(vim:nmap (kbd "C-w C-R") (kbd "C-w R"))
(vim:nmap (kbd "C-w r") 'vim:window-rotate-downwards)
(vim:nmap (kbd "C-w C-r") (kbd "C-w r"))
(vim:nmap (kbd "C-w _") 'vim:window-set-height)
(vim:nmap (kbd "C-w C-_") (kbd "C-w _"))
(vim:nmap (kbd "C-w |") 'vim:window-set-width)
(vim:nmap (kbd "C-w b") 'vim:window-bottom-right)
(vim:nmap (kbd "C-w C-b") (kbd "C-w b"))
(vim:nmap (kbd "C-w t") 'vim:window-top-left)
(vim:nmap (kbd "C-w C-t") (kbd "C-w t"))
(vim:nmap (kbd "C-w c") 'vim:window-close)
(vim:nmap (kbd "C-w h") 'vim:window-left)
(vim:nmap (kbd "C-w C-h") (kbd "C-w h"))
(vim:nmap (kbd "C-w j") 'vim:window-down)
(vim:nmap (kbd "C-w C-j") (kbd "C-w j"))
(vim:nmap (kbd "C-w k") 'vim:window-up)
(vim:nmap (kbd "C-w C-k") (kbd "C-w k"))
(vim:nmap (kbd "C-w l") 'vim:window-right)
(vim:nmap (kbd "C-w C-l") (kbd "C-w l"))
(vim:nmap (kbd "C-w p") 'vim:window-previous)
(vim:nmap (kbd "C-w C-p") (kbd "C-w p"))
(vim:nmap (kbd "C-w n") 'vim:window-new)
(vim:nmap (kbd "C-w C-n") (kbd "C-w n"))
(vim:nmap (kbd "C-w o") 'vim:window-only)
(vim:nmap (kbd "C-w C-o") (kbd "C-w o"))
(vim:nmap (kbd "C-w s") 'vim:window-split)
(vim:nmap (kbd "C-w C-s") (kbd "C-w s"))
(vim:nmap (kbd "C-w S") (kbd "C-w s"))
(vim:nmap (kbd "C-w v") 'vim:window-vsplit)
(vim:nmap (kbd "C-w C-v") (kbd "C-w v"))

(vim:nmap "v" 'vim:visual-toggle-normal)
(vim:nmap "V" 'vim:visual-toggle-linewise)
(vim:nmap (kbd "C-v") 'vim:visual-toggle-block)
(vim:nmap "gv" 'vim:visual-mode-reactivate)


(vim:map [escape] 'vim:insert-mode-exit :keymap vim:insert-mode-ESC-keymap)
(vim:map (kbd "ESC") 'vim:insert-mode-exit :keymap vim:insert-mode-ESC-keymap)
(vim:imap [insert] 'vim:insert-mode-toggle-replace)
(vim:imap [kp-insert] 'vim:insert-mode-toggle-replace)

(vim:vmap [escape] 'vim:visual-mode-exit)
(vim:vmap (kbd "ESC") 'vim:visual-mode-exit)
(vim:vmap "v" 'vim:visual-toggle-normal)
(vim:vmap "V" 'vim:visual-toggle-linewise)
(vim:vmap (kbd "C-v") 'vim:visual-toggle-block)

(vim:vmap "d" 'vim:cmd-delete)
(vim:vmap "D" 'vim:cmd-delete)
(vim:vmap "x" 'vim:cmd-delete)

(vim:vmap "c" 'vim:cmd-change)
(vim:vmap "C" "Vc")
(vim:vmap "r" 'vim:cmd-replace-region)
(vim:vmap "R" 'vim:cmd-change)
(vim:vmap "s" 'vim:cmd-change)
(vim:vmap "S" 'vim:cmd-change)

(vim:vmap "y" 'vim:cmd-yank)
(vim:vmap "Y" 'vim:cmd-yank)

(vim:vmap "J" 'vim:cmd-join)

(vim:vmap "=" 'vim:cmd-indent)
(vim:vmap "<" 'vim:cmd-shift-left)
(vim:vmap ">" 'vim:cmd-shift-right)

(vim:vmap "~" 'vim:cmd-toggle-case)
(vim:vmap "U" 'vim:cmd-make-upcase)
(vim:vmap "u" 'vim:cmd-make-downcase)

(vim:vmap "I" 'vim:visual-insert)
(vim:vmap "A" 'vim:visual-append)

(vim:vmap "o" 'vim:visual-exchange-point-and-mark)
(vim:vmap "O" 'vim:visual-jump-point)
