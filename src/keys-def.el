;; keys-def.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (almost since the august inception)
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'hydra-setup)
(require 'vim-mock)

(declaim (special +vi-essential-keys+
                  +vi-keys+))

(defconst +vi-essential-keys+
  '(("d" backward-char)
    ("h" next-line)
    ("t" previous-line)
    ("n" forward-char)

    ("0" vim-mock:motion-beginning-of-line-or-digit-argument)
    ("^" vim-mock:motion-first-non-blank)
    ("$" vim-mock:motion-end-of-line)

    ("[" forward-paragraph)
    ("]" backward-paragraph))
  "Essential vi movement keys")

(defconst +vi-keys+
  `(,@+vi-essential-keys+
    ("/"   search-start-forward)
    ("C-/" search-start-forward-new-color)
    ("?"   search-start-backward)
    ("C-?" search-start-backward-new-color))
  "Essential vi movement keys plus search keys")

(defconst +vim-search-keys+
  '(("/"   search-start-forward)
    ("C-/" search-start-forward-new-color)
    ("?"   search-start-backward)
    ("C-?" search-start-backward-new-color)
    ("u"   search-next)
    ("U"   search-prev))
  "Vi key for searching from search.el, probably should not be used without
`+vim-special-keys+'.")

(defconst +vim-search-extended-keys+
  '(("*"   search-for-symbol-at-point-forward)
    ("C-*" search-for-symbol-at-point-forward-new-color)
    ("#"   search-for-symbol-at-point-backward)
    ("C-#" search-for-symbol-at-point-backward-new-color)))

(defconst +vim-mock:word-motion-keys+
  '(("w" vim-mock:motion-fwd-word)
    ("b" vim-mock:motion-bwd-word)
    ("W" vim-mock:motion-fwd-WORD)
    ("B" vim-mock:motion-bwd-WORD)
    ("e" vim-mock:motion-fwd-word-end)
    ("E" vim-mock:motion-fwd-WORD-end)))

(defconst +vim-interbuffer-navigation-keys+
  '(("<home>"     tab-next)
    ("<end>"      tab-previous)

    ("C-<home>"   next-f)
    ("C-<end>"    prev-f)
    ("C-S-<home>" swap-buffers-forward-through-frames)
    ("C-S-<end>"  swap-buffers-backward-through-frames)
    ("<left>"     prev-w)
    ("<right>"    next-w)
    ("S-<left>"   swap-buffers-backward)
    ("S-<right>"  swap-buffers-forward)))

(defconst +vim-character-navigation-keys+
  '(("C-<down>"  forward-line)
    ("C-<up>"    backward-line)
    ("C-<right>" forward-char)
    ("C-<left>"  backward-char)))

(defhydra-ext hydra-vim-mock-z-ext (:exit t :foreign-keys nil :hint nil)
  "
scroll to _b_ottom
scroll to _t_op
_z_: scroll to center
"
  ;; we do not always have vim mode enabled
  ;; and these scrolling commands are completely independent of vim mode
  ("b" vim-mock:scroll-line-to-bottom)
  ("t" vim-mock:scroll-line-to-top)
  ("z" vim-mock:scroll-line-to-center))

(defconst +vim-special-keys+
  `(("s"     vim:ex-read-command)
    ("C-M-k" remove-buffer)

    ("g"     hydra-vim-normal-g-ext/body)
    ("j"     hydra-vim-normal-j-ext/body)
    ("M-x"   ivy-smex)

    ("G"     vim-mock:motion-go-to-first-non-blank-end)

    ("z"     hydra-vim-mock-z-ext/body)

    ("<insert>" vim-mock:scroll-line-up)
    ("<delete>" vim-mock:scroll-line-down)

    ,@+vim-interbuffer-navigation-keys+
    ,@+vim-character-navigation-keys+

    ;; ("v"   set-mark-command)
    ;; ("y"   copy-region-as-kill)
    ))

(provide 'keys-def)

;; Local Variables:
;; End:

;; keys-def.el ends here
