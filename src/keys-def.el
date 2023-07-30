;; keys-def.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (almost since the august inception)
;; Description:

(eval-when-compile
  (require 'cl))

(require 'hydra-setup)
(require 'pseudovim)

(defconst +vi-essential-keys+
  '(("d" backward-char)
    ("h" next-line)
    ("t" previous-line)
    ("n" forward-char)

    ("0" pseudovim-motion-beginning-of-line-or-digit-argument)
    ("^" pseudovim-motion-first-non-blank)
    ("$" pseudovim-motion-end-of-line)

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

(defconst +vim-word-motion-keys+
  '(("w" pseudovim-motion-fwd-word)
    ("b" pseudovim-motion-bwd-word)
    ("W" pseudovim-motion-fwd-WORD)
    ("B" pseudovim-motion-bwd-WORD)
    ("e" pseudovim-motion-fwd-word-end)
    ("E" pseudovim-motion-fwd-WORD-end)))

(defconst +vim-interbuffer-navigation-keys+
  '(("<home>"     next-tab-or-frame)
    ("<end>"      prev-tab-or-frame)

    ("C-<home>"   next-f)
    ("C-<end>"    prev-f)
    ("C-S-<home>" swap-buffers-forward-through-frames)
    ("C-S-<end>"  swap-buffers-backward-through-frames)
    ("<left>"     prev-w)
    ("<right>"    next-w)
    ("S-<left>"   swap-buffers-backward)
    ("S-<right>"  swap-buffers-forward)))

(defconst +vim-character-navigation-keys+
  '(("C-<down>"   forward-line)
    ("C-<up>"     backward-line)
    ("C-<right>"  forward-char)
    ("C-<left>"   backward-char)))

(defconst +vim-parens-keys+
  '(("C-("        vim:backward-slurp-sexp:interactive)
    ("C-)"        vim:forward-slurp-sexp:interactive)
    ("M-("        vim:splice-sexp-killing-backward:interactive)
    ("M-)"        vim:splice-sexp-killing-forward:interactive)
    ("M-<up>"     vim:raise-sexp:interactive)
    ("M-<down>"   vim:wrap-sexp:interactive)
    ("M-?"        vim:convolute-sexp:interactive)))

(defhydra-ext hydra-pseudovim-z-ext (:exit t :foreign-keys nil :hint nil)
  "
scroll to _b_ottom
scroll to _t_op
_z_: scroll to center
"
  ;; we do not always have vim mode enabled
  ;; and these scrolling commands are completely independent of vim mode
  ("b" pseudovim-scroll-line-to-bottom)
  ("t" pseudovim-scroll-line-to-top)
  ("z" pseudovim-scroll-line-to-center))

(defconst +vim-special-keys+
  `(("s"     vim-ex-read-command)
    ("C-M-k" remove-buffer)

    ("g"     hydra-vim-normal-g-ext/body)
    ("j"     hydra-vim-normal-j-ext/body)
    ("M-x"   counsel-M-x)

    ("G"     pseudovim-motion-go-to-first-non-blank-end)

    ("z"     hydra-pseudovim-z-ext/body)

    ("<insert>" pseudovim-scroll-line-up)
    ("<delete>" pseudovim-scroll-line-down)

    ,@+vim-interbuffer-navigation-keys+
    ,@+vim-character-navigation-keys+

    ;; ("v"   set-mark-command)
    ;; ("y"   copy-region-as-kill)
    ))

(provide 'keys-def)

;; Local Variables:
;; End:

;; keys-def.el ends here
