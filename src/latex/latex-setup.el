;; latex-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 27 August 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'custom)
(require 'preview)
(require 'preview-latex)
(require 'latex-abbrev+)

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-PDF-mode t
      TeX-debug-warnings t)

(defun TeX-fold-item-at-point ()
  (interactive)
  (cond
    ((TeX-fold-item 'macro))
    ((TeX-fold-item 'math))
    ((TeX-fold-item 'env))
    ((TeX-fold-comment))))


(defvar-local latex:preview nil
  "Becomes t when latex buffer is being previewed and nil
  otherwise.")

(defun latex-toggle-preview ()
  "Toggle preview in current buffer."
  (if latex:preview
    (preview-clearout-buffer)
    (preview-buffer))
  (setq latex:preview (not latex:preview)))


(defvar-local latex:russian-spellcheck nil
  "Becomes t when latex buffer has russian spellcheck enabled.")

(defun latex-toggle-russian-spell-check ()
  "Toggle russian spell checking in current buffer."
  (if latex:russian-spellcheck
    (progn
      (flyspell-mode-off)
      (def-keys-for-map vim:normal-mode-local-keymap
        ("'"  nil)
        ("\"" nil)))
    (progn
      (def-keys-for-map vim:normal-mode-local-keymap
        ("'"  ispell-word)
        ("\"" flyspell-goto-next-error))
      (flyspell-russian)
      (message "REMINDER: ' - ispell-word, \" - flyspell-goto-next-error")))
  (setf latex:russian-spellcheck (not latex:russian-spellcheck)))


(defun latex-toggle-preview-or-russian-spellcheck (&optional arg)
  (interactive (list current-prefix-arg))
  (if arg
    (latex-toggle-russian-spell-check)
    (latex-toggle-preview)))



(load-library "latex-outline")



(defun latex-setup ()
  (init-common :use-whitespace 'tabs-only)
  (tex-fold-mode 1)

  ;; (menu-bar-mode 1)
  (setq latex:preview nil)

  (setq-local yas-key-syntaxes '("^ >"))
  (modify-syntax-entry ?$ "\"")

  ;; compilation setup
  (if-buffer-has-file
    (setq-local compile-command
                (let* ((input-file buffer-file-name)
                       (result-name (concat
                                     (file-name-sans-extension
                                      (file-name-nondirectory input-file))
                                     ".pdf"))
                       (tmp-file (concat +tmp-path+ "/" result-name))
                       (result-file (concat (strip-trailing-slash
                                             (file-name-directory input-file))
                                            "/"
                                            result-name)))
                  (format
                   (concat "cd '%s' && pdflatex -halt-on-error -shell-escape --file-line-error "
                           "-output-directory '%s' '%s' && cp '%s' '%s'")
                   +tmp-path+
                   +tmp-path+
                   input-file
                   tmp-file
                   result-file)))

    ;; don't ask - just compile
    (setq-local compilation-read-command nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; don't skip any messages
    (setq-local compilation-skip-threshold 0))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("S-<f9>"  open-buffer-as-pdf)
    ("<f9>"    latex-compile)
    ("<f6>"    latex-toggle-preview-or-russian-spellcheck)

    ("<left>"  prev-w)
    ("<right>" next-w))

  ;; (define-key vim:insert-mode-local-keymap (kbd "<f3>") LaTeX-math-keymap)

  (def-keys-for-map vim:visual-mode-local-keymap
    ("z c" TeX-fold-region)
    ("z C" TeX-fold-region)
    ("z o" TeX-fold-clearout-region)
    ("z O" TeX-fold-clearout-region))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:motion-mode-local-keymap
                     vim:operator-pending-mode-local-keymap)
    ("%" vim:motion-end-of-line))

  (latex-set-up-document-start-marker)
  (latex-setup-folding)
  (latex-setup-abbrev+))


(provide 'latex-setup)

;; Local Variables:
;; End:

;; latex-setup.el ends here
