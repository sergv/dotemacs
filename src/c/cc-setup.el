;; cc-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom)

(require 'more-clojure)
(require 'c-eldoc)
(require 'ctypes)
(require 'find-file)
(require 'etags)

(require 'dtrt-indent) ;; indent offset guesser

(setf dtrt-indent-verbosity 2
      dtrt-indent-max-relevant-lines 10000)

(make-align-function c-align-on-equals
                     "\\([+*|&/!%]\\|-\\|\\^\\)?=[^=]"
                     :require-one-or-more-spaces t)

(defun c-forward-preprocessor-conditional (count)
  "Clone of `c-scan-conditionals' with interface of `forward-sexp' for matching
#if with #else and #else with #endif."
  (save-match-data
    (let* ((forward (> count 0))
           (increment (if forward -1 1))
           (search-function (if forward 're-search-forward 're-search-backward))
           (target-depth 0)
           new
           (case-fold-search nil))
      (save-excursion
        (while (/= count 0)
          (let ((depth 0)
                (found nil))
            (save-excursion
              ;; Find the "next" significant line in the proper direction.
              (while (and (not found)
                          ;; Rather than searching for a # sign that
                          ;; comes at the beginning of a line aside from
                          ;; whitespace, search first for a string
                          ;; starting with # sign.  Then verify what
                          ;; precedes it.  This is faster on account of
                          ;; the fastmap feature of the regexp matcher.
                          (funcall search-function
                                   "#[ \t]*\\(if\\|elif\\|endif\\|else\\)"
                                   nil t))
                (beginning-of-line)
                ;; Now verify it is really a preproc line.
                (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\|else\\)")
                  (let ((dchange nil)
                        (directive (match-string 1)))
                    (cond ((or (string= directive "if")
                               (string= directive "ifdef")
                               (string= directive "ifndef"))
                           (setq dchange (- increment)))
                          ((string= directive "endif")
                           (setq dchange increment))
                          ((= depth 0)
                           (setq dchange +1))
                          ((= depth +1)
                           (setq dchange -1)))
                    (when dchange
                      (setq depth (+ depth dchange))
                      ;; If we are trying to move across, and we find an
                      ;; end before we find a beginning, get an error.
                      (if (and (< depth target-depth) (< dchange 0))
                        (error (concat (if forward
                                         "No following conditional at this level"
                                         "No previous conditional at this level")
                                       ", depth = %s, target-depth = %s")
                               depth
                               target-depth)))
                    ;; When searching forward, start from next line so
                    ;; that we don't find the same line again.
                    (if forward (forward-line 1))
                    ;; We found something if we've arrived at the
                    ;; target depth.
                    (if (and dchange (= depth target-depth))
                      (setq found (point))))
                  ;; else
                  (if forward (forward-line 1)))))
            (or found
                (error "No containing preprocessor conditional"))
            (goto-char (setq new found)))
          (setq count (+ count increment))))
      (c-keep-region-active)
      (goto-char new))))

(defconst +c-preprocessor-directives-re+
  (rx (seq bol
           "#"
           (* (syntax whitespace))
           symbol-start
           (or "ifdef"
               "ifndef"
               "if"
               "elif"
               "endif"
               "else")
           symbol-end)))

(defun c-hideshow-forward-sexp (&optional arg)
  "Special version of `forward-sexp' for hideshow in c-mode."
  (if (char=? (char-after) ?\{)
    (forward-sexp arg)
    (let ((start (point)))
      (save-match-data
        (c-forward-preprocessor-conditional (or arg 1))
        (re-search-backward +c-preprocessor-directives-re+ start t)))))

(let ((hs-spec (list
                ;; This is like `+c-preprocessor-directives-re+' but does not
                ;; include ending directives like endif.
                (rx (or (seq bol
                             "#"
                             (* (syntax whitespace))
                             symbol-start
                             (or "ifdef"
                                 "ifndef"
                                 "if"
                                 "elif"
                                 "else")
                             symbol-end)
                        "{"))
                nil
                "/[*/]"
                #'c-hideshow-forward-sexp
                nil)))
  (setf hs-special-modes-alist
        (cons `(c-mode ,hs-spec)
              (cons `(c++-mode ,hs-spec)
                    (remove-if (comp (partial-first #'memq '(c-mode c++-mode))
                                     #'first)
                               hs-special-modes-alist)))))

(defun* cc-setup/set-up-c-basic-offset (&key (use-work-code-style nil))
  "Try to guess offset (`c-basic-offset') for current buffer or use value
dictated by code standard at work if use-work-code-style is non-nil.
Also propagate new offset to `vim:shift-width'."
  (let ((dtrt-indent-verbosity 0)
        (work-code-style? (and use-work-code-style
                               (platform-use? 'work))))
    (unless (dtrt-indent-try-set-offset)
      (when work-code-style?
        (setq-local c-basic-offset 4)))
    (setq-local vim:shift-width
                (cond ((integer? c-basic-offset) c-basic-offset)
                      (work-code-style?          4)
                      (t                         8)))))


(defun* cc-setup (&key (define-special-keys t)
                       (use-c-eldoc (not (platform-use? 'work))))
  (init-common :use-render-formula t)
  (autopair-mode 1)
  (hs-minor-mode 1)
  (dtrt-indent-mode 1)
  (which-function-mode -1)
  (when use-c-eldoc
    (c-turn-on-eldoc-mode)
    (set (make-variable-buffer-local 'eldoc-idle-delay) 0.2))

  (modify-syntax-entry ?_ "_")

  (setf whitespace-line-column 80
        whitespace-style '(face tabs lines-tail)
        ;; affects only tab display
        tab-width 4)

  (setf c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (setq vim:normal-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g TAB" c-indent-defun)

    ("g t"   c-end-of-defun)
    ("g n"   c-beginning-of-defun)
    ("g <up>"   c-beginning-of-defun)
    ("g <down>" c-end-of-defun)

    ("z o"   hs-show-block)
    ("z v"   hs-show-block)
    ("z c"   hs-hide-block)
    ("z C"   hs-hide-all)
    ("z O"   hs-show-all)

    ("M-."   find-tag)
    ("M-,"   pop-tag-mark))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("z c"   hs-hide-c-sexps-in-region)
    ("z o"   hs-show-c-sexps-in-region)
    ("g a a" align)
    ("g a =" c-align-on-equals)
    ("g t"   c-end-of-defun)
    ("g n"   c-beginning-of-defun))

  (when define-special-keys
    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" ff-find-related-file)))
  (when use-c-eldoc
    (def-keys-for-map vim:normal-mode-local-keymap
      (", ?"     c-eldoc-show-current-symbol-declaration))))

(provide 'cc-setup)

;; Local Variables:
;; End:

;; cc-setup.el ends here


