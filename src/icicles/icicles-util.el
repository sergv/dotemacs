;; icicles-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  8 June 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(defun* icicles-util/bind-minibuffer-keys (map &key (sexp-keys nil))
  "Utility function that binds my custom keys and is used in several places."
  (def-keys-for-map map
    ("<escape>"          abort-recursive-edit)
    ("?"                 self-insert-command)

    ("C-w"               backward-delete-word)
    ("C-S-w"             backward-delete-word*)
    ("C-p"               vim:cmd-paste-before)
    ("M-p"               browse-kill-ring)
    ("C-M-p"             browse-kill-ring)

    ("C-v"               set-mark-command)
    ("C-y"               copy-region-as-kill)
    ("C-d"               kill-region)
    ("C-f"               read-and-insert-filename)

    ("M-<tab>"           icicle-narrow-candidates)
    ("C-<tab>"           icicle-apropos-complete-and-narrow)

    ("<delete>"          delete-char)
    ("<home>"            beginning-of-line)
    ("<end>"             end-of-line)
    ("<next>"            icicle-next-prefix-candidate)
    ("<prior>"           icicle-previous-prefix-candidate)
    ("S-<next>"          icicle-next-apropos-candidate)
    ("S-<prior>"         icicle-previous-apropos-candidate)

    ("C-n"               icicle-next-TAB-completion-method)
    ;; ("C-p"               icicle-narrow-candidates-with-predicate)

    ("S-M-<tab>"         icicle-candidate-set-complement)
    ("<M-S-iso-lefttab>" icicle-candidate-set-complement)

    ("M-SPC"             toggle-icicle-hiding-common-match)
    ("C-M-SPC"           icicle-retrieve-previous-input)
    ("C-SPC"             icicle-erase-minibuffer)

    ("S-<delete>"        delete-whitespace-forward)
    ("S-<backspace>"     delete-whitespace-backward)
    ("<backspace>"       delete-backward-char))

  (when sexp-keys
    (def-keys-for-map map
      ("\\"        icicle-self-insert)
      ("M-<up>"    sp-splice-sexp-killing-backward)
      ("M-<down>"  sp-splice-sexp-killing-forward)
      ("C-)"       sp-forward-slurp-sexp)
      ("C-<right>" sp-forward-slurp-sexp)
      ("C-<left>"  sp-forward-barf-sexp)
      ("C-("       sp-backward-slurp-sexp)
      ("M-<left>"  sp-backward-slurp-sexp)
      ("M-<right>" sp-backward-barf-sexp))))

(provide 'icicles-util)

;; Local Variables:
;; End:

;; icicles-util.el ends here
