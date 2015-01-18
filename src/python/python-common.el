;; python-common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  2 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'comint-setup)
(require 'common)
(require 'outline-headers)
(require 'python-abbrev+)

;;; helper functions, inside-string?, inside-comment?, aligns etc

(defun python-point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (let ((state (parse-partial-sexp begin end)))
          (or (elt state 3)
              (elt state 4)))))))

(defun python-point-inside-string? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (elt (parse-partial-sexp begin end)
             3)))))

(defun python-point-inside-string-and-not-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (save-match-data
      (let* ((end (point))
             (begin (line-beginning-position)))
        (when begin
          (let ((state (parse-partial-sexp begin
                                           end)))
            (and (elt state 3)
                 (null (elt state 4)))))))))

(make-align-function python-align-on-equals
                     (rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                             ">>=" "<<=" "&=" "^=" "|=")
                         (regexp "[^=]"))
                     :require-one-or-more-spaces t)

(defun python-backward-sexp (&optional count)
  (interactive "p")
  (python-nav-forward-sexp (- (or count 1))))

(defun python-forward-sexp (&optional count)
  (interactive "p")
  (python-nav-forward-sexp (or count 1)))

(defun python-forward-indentation-level ()
  "Move forward to the end of indentation block that has the same or
greater indenation as current line."
  (interactive)
  (beginning-of-line)
  (let ((start-column
         (lambda ()
           (save-excursion
             (beginning-of-line)
             (skip-syntax-forward "-")
             (current-column)))))
    (let ((c (funcall start-column)))
      (forward-line)
      (while (and (not (eob?))
                  (< c (funcall start-column)))
        (forward-line))
      (backward-line)
      (end-of-line))))

(defun python-hide-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx bol
                                  (* whitespace)
                                  "def"
                                  (+ whitespace)
                                  (* not-newline)
                                  eol)
                              nil
                              t)
      (goto-char (match-end 0))
      (hs-hide-block)
      (forward-line 1))))

(defun python-convolute-lines ()
  "Python translation of `paredit-convolute-sexp'."
  (interactive)
  (save-excursion
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    vim:shift-width)
    (forward-line -1)
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    (- vim:shift-width))))

;;; common setup parts

(defun register-python-hideshow (mode)
  (setf hs-special-modes-alist
        (cons `(,mode ,(rx line-start
                           (* (syntax whitespace))
                           symbol-start
                           (or "def"
                               "class"
                               "for"
                               "if"
                               "elif"
                               "else"
                               "while"
                               "try"
                               "except"
                               "finally")
                           symbol-end)
                      nil
                      "#"
                      ,(lambda (arg)
                         (python-forward-indentation-level))
                      nil)
              (remove* 'python
                       hs-special-modes-alist
                       :key #'car
                       :test #'eq?))))

(defun python-common-setup ()
  (init-common :use-yasnippet t
               :use-render-formula t
               :sp-slurp-sexp-insert-space nil)

  (setq-local tab-width 4)
  (setq-local vim:shift-width 4)

  ;; ;; make ' a string delimiter
  ;; (modify-syntax-entry ?' "\"")

  ;; make _ a symbol constituent, mostly for me
  (modify-syntax-entry ?_ "_")
  ;; make . a symbol constituent, mostly for me too
  (modify-syntax-entry ?. ".")

  (hs-minor-mode 1)
  (setf hs-block-end-regexp nil)

  ;; By default this is set to `python-nav-forward-sexp' which is too
  ;; heavyweight alternative to `forward-sexp' for general-purpose use
  ;; (causes noticeable delay on inserting (, " or """)
  (setq-local forward-sexp-function nil)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f6>"    python-shell-send-buffer)
    ("<f9>"    python-run-script)
    ("S-<f9>"  python-check)

    ("j"       python-shell-send-defun)
    ("M-?"     python-convolute-lines)

    ("SPC SPC" switch-to-python)
    (", s s"   vim:replace-symbol-at-point)

    ("z o"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     python-hide-all)
    ("z O"     hs-show-all))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("<f6>"  python-shell-send-region)
    ("j"     python-shell-send-region)
    ("g a"   nil)
    ("g a =" python-align-on-equals))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("g t"      end-of-defun)
    ("g n"      beginning-of-defun)
    ("g <up>"   beginning-of-defun)
    ("g <down>" end-of-defun)

    ("<up>"     python-nav-backward-block)
    ("<down>"   python-nav-forward-block)

    ("="        python-nav-backward-up-list)
    ("q"        python-nav-up-list)

    ("*"        search-for-symbol-at-point-forward)
    ("M-*"      search-for-symbol-at-point-forward-new-color)
    ("#"        search-for-symbol-at-point-backward)
    ("M-#"      search-for-symbol-at-point-backward-new-color))

  (python-abbrev+-setup)

  ;; pabbrev isn't powerful enough
  ;; (pabbrev-mode 1)
  ;; (def-keys-for-map (vim:normal-mode-local-keymap
  ;;                     vim:insert-mode-local-keymap)
  ;;   ("M-/"     pabbrev-show-menu ;; pabbrev-expand-maybe
  ;;              ))
  ;; (when pabbrev-mode
  ;;   (pabbrev-scavenge-buffer))

  (setup-outline-headers :header-start "^[ \t]*"
                         :header-symbol "#"
                         :length-min 3))

(provide 'python-common)

;; Local Variables:
;; End:

;; python-common.el ends here
