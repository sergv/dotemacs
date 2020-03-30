;; vim-maps.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains all standard keymaps.  Key mappings are defined
;; using one of the following vim-like macros:

;;   - vim:map ... general mapping in an arbitrary mode
;;   - vim:nmap ... mapping in the normal-mode keymap
;;   - vim:omap ... mapping in the operator-pending keymap
;;   - vim:imap ... mapping in the insert-mode keymap
;;   - vim:vmap ... mapping in the visual-mode keymap
;;   - vim:emap ... mapping in the ex-mode keymap
;;
;; Furthermore, for each of these map-function there's a buffer-local
;; variant
;;
;;   - vim:local-nmap ... mapping in the normal-mode local keymap
;;   - vim:local-omap ... mapping in the operator-pending local keymap
;;   - vim:local-imap ... mapping in the insert-mode local keymap
;;   - vim:local-vmap ... mapping in the visual-mode local keymap
;;   - vim:local-emap ... mapping in the ex-mode local keymap
;;
;; The local versions should be used to define mode specific bindings
;; as local-set-key would do.
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
;; KEYEVENTS is a usual Emacs-sequence of events as it would be used by
;; define-key.

;;; TODO:
;;   - better mapping to support stuff like
;;     (vim:def-map "c" "d{motion}a")
;;
;;   - because of this, mapping "cc" to "0C" does not work with a
;;     count since the count is eaten by the '0'
;;
;;   - similarily 'o' and 'O' won't work
;;
;;   - should we have a 'deep-mapping' function: for example, "x" is
;;     mapped to "dl" in the default keymap.  If someone decides to
;;     redefine "l" to some other command, "x" will change its
;;     behaviour, too.  A 'deep-mapping' should save the mapping on
;;     definition of "x", therefor let "x" behave as usual even after
;;     redefining "l"

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'vim-keymap)
(require 'vim-motions)
(require 'vim-commands)
(require 'vim-ex-commands)
(require 'vim-scroll)
;; (require 'vim-window)
(require 'vim-search)
(require 'vim-normal-mode)
(require 'vim-insert-mode)
(require 'vim-visual-mode)
(require 'vim-ex)

;; (vim:nmap "\\" #'vim:cmd-emacs)

;; (vim:nmap (kbd "C-z") #'vim:activate-emacs-mode)
;; (vim:map (kbd "C-z") #'vim:activate-normal-mode :keymap vim:emacs-keymap)

(vim:omap [escape] #'vim:operator-pending-mode-exit)
;; (vim:omap "0" #'vim:motion-beginning-of-line-or-digit-argument)
;; (vim:omap "1" #'digit-argument)
;; (vim:omap "2" #'digit-argument)
;; (vim:omap "3" #'digit-argument)
;; (vim:omap "4" #'digit-argument)
;; (vim:omap "5" #'digit-argument)
;; (vim:omap "6" #'digit-argument)
;; (vim:omap "7" #'digit-argument)
;; (vim:omap "8" #'digit-argument)
;; (vim:omap "9" #'digit-argument)

;; (vim:nmap "ZZ" #'vim:cmd-write-and-close)

(vim:mmap "h" #'vim:motion-left)
(vim:mmap "l" #'vim:motion-right)
(vim:mmap "^" #'vim:motion-first-non-blank)
(vim:mmap "$" #'vim:motion-end-of-line)
;; (vim:mmap "g_" #'vim:motion-last-non-blank)
;; (vim:mmap "g0" #'vim:motion-beginning-of-screen-line)
;; (vim:mmap "g^" #'vim:motion-first-non-blank-of-screen-line)
;; (vim:mmap "gm" "g0")
;; (vim:mmap "g$" #'vim:motion-end-of-screen-line)

(vim:mmap "k" #'vim:motion-up)
(vim:mmap "j" #'vim:motion-down)

;; (vim:mmap "H" #'vim:motion-window-first-line)
;; (vim:mmap "M" #'vim:motion-window-middle-line)
;; (vim:mmap "L" #'vim:motion-window-last-line)

(vim:mmap "w" #'vim:motion-fwd-word)
(vim:mmap "W" #'vim:motion-fwd-WORD)
(vim:mmap "e" #'vim:motion-fwd-word-end)
(vim:mmap "E" #'vim:motion-fwd-WORD-end)
(vim:mmap "b" #'vim:motion-bwd-word)
(vim:mmap "B" #'vim:motion-bwd-WORD)
(vim:mmap "ge" #'vim:motion-bwd-word-end)
(vim:mmap "gE" #'vim:motion-bwd-WORD-end)

;; (vim:mmap "(" #'vim:motion-bwd-sentence)
;; (vim:mmap ")" #'vim:motion-fwd-sentence)
;; (vim:mmap "{" #'vim:motion-bwd-paragraph)
;; (vim:mmap "}" #'vim:motion-fwd-paragraph)
;; (vim:mmap "]]" #'vim:motion-fwd-section)
;; (vim:mmap "][" #'vim:motion-fwd-section)
;; (vim:mmap "[[" #'vim:motion-bwd-section)
;; (vim:mmap "[]" #'vim:motion-bwd-section)
;; (vim:mmap "[(" #'vim:motion-backward-opening-parenthesis)
;; (vim:mmap "])" #'vim:motion-forward-closing-parenthesis)
;; (vim:mmap "[{" #'vim:motion-backward-opening-brace)
;; (vim:mmap "]}" #'vim:motion-forward-closing-brace)
;; (vim:mmap "[#" #'vim:motion-backward-preprocessor-if)
;; (vim:mmap "]#" #'vim:motion-forward-preprocessor-endif)
;; (vim:mmap "[/" #'vim:motion-backward-opening-comment)
;; (vim:mmap "[*" "[/")
;; (vim:mmap "]/" #'vim:motion-forward-closing-comment)
;; (vim:mmap "]*" "]/")

(vim:omap "f" #'vim:motion-find)
(vim:omap "F" #'vim:motion-find-back)
(vim:omap "t" #'vim:motion-find-to)
(vim:omap "T" #'vim:motion-find-back-to)
;; (vim:omap ":" #'vim:motion-repeat-last-find)
;; (vim:omap "," #'vim:motion-repeat-last-find-opposite)

(vim:omap "%" #'vim:motion-jump-item)

;; (vim:omap [tab] #'indent-for-tab-command)

(vim:omap "iw" #'vim:motion-inner-word)
(vim:omap "aw" #'vim:motion-outer-word)
(vim:omap "iW" #'vim:motion-inner-WORD)
(vim:omap "aW" #'vim:motion-outer-WORD)
(vim:omap "is" #'vim:motion-inner-sentence)
(vim:omap "as" #'vim:motion-outer-sentence)
(vim:omap "ip" #'vim:motion-inner-paragraph)
(vim:omap "ap" #'vim:motion-outer-paragraph)

(vim:omap "i[" #'vim:motion-inner-brackets)
(vim:omap "i]" #'vim:motion-inner-brackets)
(vim:omap "a[" #'vim:motion-outer-brackets)
(vim:omap "a]" #'vim:motion-outer-brackets)

(vim:omap "i(" #'vim:motion-inner-parentheses)
(vim:omap "i)" #'vim:motion-inner-parentheses)
(vim:omap "ib" #'vim:motion-inner-parentheses)
(vim:omap "a(" #'vim:motion-outer-parentheses)
(vim:omap "a)" #'vim:motion-outer-parentheses)
(vim:omap "ab" #'vim:motion-outer-parentheses)

(vim:omap "i<" #'vim:motion-inner-angles)
(vim:omap "i>" #'vim:motion-inner-angles)
(vim:omap "a<" #'vim:motion-outer-angles)
(vim:omap "a>" #'vim:motion-outer-angles)

(vim:omap "i{" #'vim:motion-inner-braces)
(vim:omap "i}" #'vim:motion-inner-braces)
(vim:omap "iB" #'vim:motion-inner-braces)
(vim:omap "a{" #'vim:motion-outer-braces)
(vim:omap "a}" #'vim:motion-outer-braces)
(vim:omap "aB" #'vim:motion-outer-braces)

(vim:omap "it" #'vim:motion-inner-xml-tags)
(vim:omap "at" #'vim:motion-outer-xml-tags)

(vim:omap "i#'" #'vim:motion-inner-single-quote)
(vim:omap "a#'" #'vim:motion-outer-single-quote)
(vim:omap "i\"" #'vim:motion-inner-double-quote)
(vim:omap "a\"" #'vim:motion-outer-double-quote)
(vim:omap "i`" #'vim:motion-inner-back-quote)
(vim:omap "a`" #'vim:motion-outer-back-quote)

(vim:omap "v" #'vim:cmd-force-charwise)
(vim:omap "V" #'vim:cmd-force-linewise)
(vim:omap (kbd "C-v") #'vim:cmd-force-blockwise)

;(vim:nmap "x" "dl")
;; (vim:nmap "m" #'vim:cmd-set-mark)
(vim:nmap "x" #'vim:cmd-delete-char)
(vim:nmap "D" "d$")
(vim:nmap "d" #'vim:cmd-delete)

(vim:nmap "C" #'vim:cmd-change-rest-of-line)
(vim:nmap "c" #'vim:cmd-change)
(vim:nmap "s" #'vim:cmd-change-char)

(vim:nmap "r" #'vim:cmd-replace-char)
;; (vim:nmap "R" #'vim:cmd-replace)

(vim:nmap "y" #'vim:cmd-yank)
(vim:nmap "Y" "y$")
(vim:nmap "p" #'vim:cmd-paste-behind)
(vim:nmap "P" #'vim:cmd-paste-before)
;; (vim:nmap (kbd "C-p") #'vim:cmd-paste-pop)

(vim:nmap "J" #'vim:cmd-join-lines)

;; (vim:mmap "/" #'vim:motion-search-fwd)
;; (vim:mmap "?" #'vim:motion-search-bwd)
;; (vim:mmap "*" #'vim:search-word)
;; (vim:mmap "#" #'vim:search-word-backward)
;; (vim:mmap (kbd "C-n") #'vim:motion-search-next)
;; (vim:mmap (kbd "C-S-N") #'vim:motion-search-next-reverse)
;; (vim:nmap "n" #'vim:motion-search-next)
;; (vim:nmap "N" #'vim:motion-search-next-reverse)

(vim:nmap "i" #'vim:cmd-insert)
(vim:nmap "a" #'vim:cmd-append)
(vim:nmap "I" #'vim:cmd-Insert)
(vim:nmap "A" #'vim:cmd-Append)
(vim:nmap "o" #'vim:cmd-insert-line-below)
(vim:nmap "O" #'vim:cmd-insert-line-above)

(vim:nmap "u" #'vim:cmd-undo)
;; (vim:nmap (kbd "C-r") #'vim:cmd-redo)

(vim:nmap "." #'vim:cmd-repeat)

(vim:nmap "<" #'vim:cmd-shift-left)
(vim:nmap ">" #'vim:cmd-shift-right)

(vim:omap "zt" #'vim:scroll-line-to-top)
;; (vim:omap (vconcat "z" [return]) "zt^")
;; (vim:omap (kbd "z RET") (vconcat "z" [return]))
(vim:omap "zz" #'vim:scroll-line-to-center)
;; (vim:omap "z." "zz^")
(vim:omap "zb" #'vim:scroll-line-to-bottom)
;; (vim:omap "z-" "zb^")

(vim:nmap "v" #'vim:visual-toggle-normal)
(vim:nmap "V" #'vim:visual-toggle-linewise)
(vim:nmap (kbd "C-v") #'vim:visual-toggle-block)

(vim:nmap ":" #'vim:ex-read-command)
(vim:mmap ":" #'vim:ex-read-command)
;; (vim:omap ":" #'undefined)
;; (vim:wmap (kbd "C-:") #'vim:ex-read-command)
(vim:nmap "q" #'vim:cmd-toggle-macro-recording)
(vim:nmap "@" #'vim:cmd-execute-macro)


(vim:imap (vector vim:ESC-event) #'vim:insert-mode-exit)
(vim:imap [insert] #'vim:insert-mode-toggle-replace)
(vim:imap [kp-insert] [insert])
(vim:imap [insertchar] [insert])

(vim:vmap (vector vim:ESC-event) #'vim:visual-mode-exit)
(vim:vmap "v" #'vim:visual-toggle-normal)
(vim:vmap "V" #'vim:visual-toggle-linewise)
(vim:vmap (kbd "C-v") #'vim:visual-toggle-block)

(vim:vmap "d" #'vim:cmd-delete)
(vim:vmap "D" #'vim:cmd-delete)
(vim:vmap "x" #'vim:cmd-delete)

(vim:vmap "c" #'vim:cmd-change)
(vim:vmap "C" "Vc")
(vim:vmap "r" #'vim:cmd-replace-region)
(vim:vmap "R" #'vim:cmd-change)
(vim:vmap "s" #'vim:cmd-change)
(vim:vmap "S" #'vim:cmd-change)

(vim:vmap "y" #'vim:cmd-yank)
(vim:vmap "Y" #'vim:cmd-yank)

;; (vim:vmap "J" #'vim:cmd-join)

(vim:vmap "<" #'vim:cmd-shift-left)
(vim:vmap ">" #'vim:cmd-shift-right)

(vim:vmap "~" #'vim:cmd-toggle-case)

(vim:vmap "I" #'vim:visual-insert)
(vim:vmap "A" #'vim:visual-append)

;; (vim:vmap "o" #'vim:visual-exchange-point-and-mark)
(vim:vmap "O" #'vim:visual-jump-point)

(vim:vmap ":" #'vim:visual-ex-read-command)

;; (vim:emap "split" #'vim:window-split)
;; (vim:emap "sp" "split")
;; (vim:emap "vsplit" #'vim:window-vsplit)
;; (vim:emap "vs" "vsplit")
;; (vim:emap "new" #'vim:window-new)
;; (vim:emap "vnew" #'vim:window-vnew)
;; (vim:emap "vne" "vnew")
;; (vim:emap "close" #'vim:window-close)
;; (vim:emap "clo" "close")
;; (vim:emap "only" #'vim:window-only)
;; (vim:emap "on" "only")
(vim:emap "quit" #'vim:cmd-quit)
(vim:emap "q" "quit")
(vim:emap "wq" #'vim:cmd-save-and-close)
;; (vim:emap "quitall" #'vim:cmd-quit-all)
;; (vim:emap "quita" "quitall")
;; (vim:emap "qall" "quitall")
;; (vim:emap "qa" "qall")
;; (vim:emap "wqall" #'vim:cmd-save-and-quit)
;; (vim:emap "wqa" "wqall")
;; (vim:emap "xall" "wqall")
;; (vim:emap "xa" "xall")
;; (vim:emap "bdelete" #'vim:cmd-delete-buffer)
;; (vim:emap "bd" "bdelete")
(vim:emap "substitute" #'vim:cmd-substitute)
(vim:emap "s" "substitute")
;; (vim:emap "ju" "jumps")
;; (vim:emap "nohlsearch" #'vim:cmd-nohighlight)
;; (vim:emap "noh" "nohlsearch")
;; (vim:emap "nh" "nohlsearch")
(vim:emap "nhg" #'vim:cmd-nohighlight-everywhere)
;; (vim:emap "setmode" #'vim:cmd-setmode)

(provide #'vim-maps)

;; Local Variables:
;; End:

;; vim-maps.el ends here
