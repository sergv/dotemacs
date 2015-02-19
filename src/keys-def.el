;; keys-def.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (almost since the august inception)
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'vim-mock)

(declaim (special +vi-essential-keys+
                  +vi-keys+))

(defconst +vi-essential-keys+
  '(("d" backward-char)
    ("h" next-line)
    ("t" previous-line)
    ("n" forward-char)

    ("[" forward-paragraph)
    ("]" backward-paragraph))
  "Essential vi movement keys")

(defconst +vi-keys+
  (append +vi-essential-keys+
          '(("/"   search-start-forward)
            ("C-/" search-start-forward-new-color)
            ("?"   search-start-backward)
            ("C-?" search-start-backward-new-color)))
  "Essential vi movement keys plus search keys")

(defconst +vi-search-keys+
  '(("/"   search-start-forward)
    ("C-/" search-start-forward-new-color)
    ("?"   search-start-backward)
    ("C-?" search-start-backward-new-color)
    ("u"   search-next)
    ("U"   search-prev)
    ("C-h" search-toggle-highlighting)
    ;; rebind "C-h" for terminals that refuse to send "C-h" and
    ;; send "C-<backspace>" instead
    ("C-<backspace>" search-toggle-highlighting)
    ("<C-backspace>" search-toggle-highlighting))
  "Vi key for searching from search.el, probably should not be used without
`+vim-special-keys+'.")

(defconst +vim-word-motion-keys+
  '(("w" vim-mock:motion-fwd-word)
    ("b" vim-mock:motion-bwd-word)
    ("W" vim-mock:motion-fwd-WORD)
    ("B" vim-mock:motion-bwd-WORD)
    ("e" vim-mock:motion-fwd-word-end)
    ("E" vim-mock:motion-fwd-WORD-end)))

(defconst +vim-special-keys+
  '(("s"     vim:ex-read-command)
    ("C-b"   icicle-buffer)

    ("g"     nil)
    ("g x"   smex)
    ("M-x"   smex)
    ("g e"   vim-mock:motion-bwd-word-end)
    ("g E"   vim-mock:motion-bwd-WORD-end)
    ("g f"   icicle-file)
    ("g r"   rgrep-wrapper)
    ("g k"   remove-buffer)
    ("g K"   remove-buffer-and-window)
    ("g u"   undo-tree-visualize)

    ("g g"   vim-mock:motion-go-to-first-non-blank-beg)
    ("G"     vim-mock:motion-go-to-first-non-blank-end)

    ("z"     nil)
    ;; we do not always have vim mode enabled
    ;; and these scrolling commands are completely independent of vim mode
    ("z t"   vim-mock:scroll-line-to-top)
    ("z z"   vim-mock:scroll-line-to-center)
    ("z b"   vim-mock:scroll-line-to-bottom)

    ("<insert>" vim-mock:scroll-line-up)
    ("<delete>" vim-mock:scroll-line-down)

    ("<home>"    prev-f)
    ("<end>"     next-f)
    ("<left>"    prev-w)
    ("<right>"   next-w)
    ("S-<left>"  swap-buffers-backward)
    ("S-<right>" swap-buffers-forward)
    ;; ("v"   set-mark-command)
    ;; ("y"   copy-region-as-kill)
    ))

(provide 'keys-def)

;; Local Variables:
;; End:

;; keys-def.el ends here
