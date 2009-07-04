;;; vim-maps.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

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

;(vim:def-motion "0" 'vim:motion-beginning-of-line :type 'exclusive)
(vim:omap "0" 'vim:feed-numeric-prefix-or-bol)
(vim:omap "1" 'vim:feed-numeric-prefix)
(vim:omap "2" 'vim:feed-numeric-prefix)
(vim:omap "3" 'vim:feed-numeric-prefix)
(vim:omap "4" 'vim:feed-numeric-prefix)
(vim:omap "5" 'vim:feed-numeric-prefix)
(vim:omap "6" 'vim:feed-numeric-prefix)
(vim:omap "7" 'vim:feed-numeric-prefix)
(vim:omap "8" 'vim:feed-numeric-prefix)
(vim:omap "9" 'vim:feed-numeric-prefix)
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

(vim:nmap "x" "dl")
(vim:nmap "D" "d$")
(vim:nmap "dd" 'vim:cmd-delete-line)
(vim:nmap "d" 'vim:cmd-delete)

(vim:nmap "C" "d$a")
(vim:nmap "cc" 'vim:cmd-change-line)
(vim:nmap "c" 'vim:cmd-change)
(vim:nmap "s" "xi")

(vim:nmap "r" 'vim:cmd-replace-char)
(vim:nmap "R" 'vim:cmd-replace)

(vim:nmap "yy" 'vim:cmd-yank-line)
(vim:nmap "y" 'vim:cmd-yank)
(vim:nmap "Y" "yy")
(vim:nmap "p" 'vim:cmd-paste-behind)
(vim:nmap "P" 'vim:cmd-paste-before)

(vim:nmap "i" 'vim:cmd-insert)
(vim:nmap "a" 'vim:cmd-append)
(vim:nmap "I" 'vim:cmd-Insert)
(vim:nmap "A" 'vim:cmd-Append)
(vim:nmap "o" (kbd "A RET TAB"))
(vim:nmap "O" (kbd "0 i RET <up> TAB"))

(vim:nmap "u" 'vim:cmd-undo)
(vim:nmap (kbd "C-r") 'vim:cmd-redo)

(vim:nmap "." 'vim:cmd-repeat)

(vim:map [escape] 'vim:insert-mode-exit :mode vim:insert-mode)
