;; latex-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 27 August 2011
;; Keywords:
;; Requirements:
;; Status:

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

(defun latex-preview-in-okular ()
  "Preview current buffer's pdf file, if any, in okular program."
  (interactive)
  (let ((doc-name (replace-in-string (buffer-file-name) ".tex" ".pdf")))
    (if (file-existp doc-name)
      (start-process-shell-command "okular - tex preview"
                                   nil
                                   (concat "okular "
                                           (shell-quote-argument doc-name)))
      (error "No pdf file found - maybe compilation errors?"))))



(defun latex-setup ()
  (init-common)
  (tex-fold-mode 1)

  ;; (menu-bar-mode 1)
  (setq latex:preview nil)

  (setq-local yas-key-syntaxes '("^ >"))
  (modify-syntax-entry ?$ "\"")

  ;; compilation setup
  (if-buffer-has-file
    (setq-local compile-command
                (concat "pdflatex -halt-on-error -shell-escape --file-line-error "
                        (shell-quote-argument (buffer-file-name))))

    ;; don't ask - just compile
    (setq-local compilation-read-command nil)
    ;; don't ask - just save
    (setq-local compilation-ask-about-save nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; don't skip any messages
    (setq-local compilation-skip-threshold 0))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("S-<f9>" latex-preview-in-okular)
    ("<f9>"   latex-compile)
    ("<f6>"   latex-toggle-preview-or-russian-spellcheck)

    ("<home>" vim:motion-bwd-paragraph)
    ("<end>"  vim:motion-fwd-paragraph))

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
