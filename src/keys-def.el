
(require 'vim-mock)

(declaim (special +vi-essential-keys+
                  +vi-keys+
                  +control-x-prefix+))

(defconst +vi-essential-keys+
  '(("t" next-line)
    ("n" previous-line)
    ("h" backward-char)
    ("s" forward-char)

    ("[" forward-paragraph)
    ("]" backward-paragraph))
  "Esential vi movement keys")

(defconst +vi-keys+
  (append +vi-essential-keys+
          '(("/" isearch-forward-regexp)
            ("?" isearch-backward-regexp)))
  "Essential vi movement keys plus search keys")

(defconst +control-x-prefix+
  '((","   nil)
    ("g"   nil)
    ("g h" help-command)
    ("g u" Control-X-prefix))
  "Prefix to C-x")

(defconst +vim-word-motion-keys+
  '(("w"   vim-mock:motion-fwd-word)
    ("b"   vim-mock:motion-bwd-word)
    ("W"   vim-mock:motion-fwd-WORD)
    ("B"   vim-mock:motion-bwd-WORD)))


(defconst +vim-special-keys+
  '((", b" ibuffer)
    (";"   vim:ex-read-command)

    ("g C" remember-win-config-store-configuration)
    ("g x" smex)
    ("g X" smex-major-mode-commands)
    ("g f" find-file)
    ("g <" vim-mock:motion-go-to-first-non-blank-beg)
    ("g >" vim-mock:motion-go-to-first-non-blank-end)

    ("z"   nil)
    ;; we're not always have vim mode enabled
    ;; and these scrolling commands are completely independent of vim mode
    ("z t" vim-mock:scroll-line-to-top)
    ("z z" vim-mock:scroll-line-to-center)
    ("z b" vim-mock:scroll-line-to-bottom)

    ("<insert>" vim-mock:scroll-line-up)
    ("<delete>" vim-mock:scroll-line-down)

    ;; ("v"   set-mark-command)
    ;; ("y"   copy-region-as-kill)
    ))


(provide 'keys-def)

;; Local Variables:
;; lexical-binding: t
;; End:

