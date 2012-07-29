;;; latex-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 27 August 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'preview)
(require 'preview-latex)
;; (require 'vim-rus)
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


(defvar-loc latex:preview nil
    "Becomes t when latex buffer is being previewed and nil
  otherwise.")

(defun latex-toggle-preview ()
  "Toggle preview in current buffer."
  (if latex:preview
      (progn
        (setq latex:preview nil)
        (preview-clearout-buffer))
      (progn
        (setq latex:preview t)
        (preview-buffer))))


(defvar-loc latex:russian-spellcheck nil
  "Becomes t when latex buffer has russian spellcheck enabled.")

(defun latex-toggle-russian-spell-check ()
  "Toggle russian spell checking in current buffer."
  (if latex:russian-spellcheck
    (progn
      (flyspell-mode-off)
      (def-keys-for-map2 vim:normal-mode-local-keymap
        ("'"  nil)
        ("\"" nil)))
    (progn
      (def-keys-for-map2 vim:normal-mode-local-keymap
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
  (autopair-mode)
  (tex-fold-mode 1)

  ;; (menu-bar-mode 1)
  (setq latex:preview nil)

  (set (make-local-variable 'yas/key-syntaxes) '("^ >"))
  (modify-syntax-entry ?$ "\"")

  ;; compilation setup
  (if-buffer-has-file
   (set (make-local-variable 'compile-command)
        (concat "pdflatex -halt-on-error -shell-escape --file-line-error "
                (shell-quote-argument (buffer-file-name))))

   ;; don't ask - just compile
   (set (make-local-variable 'compilation-read-command) nil)
   ;; don't ask - just save
   (set (make-local-variable 'compilation-ask-about-save) nil)
   (set (make-local-variable 'compilation-auto-jump-to-first-error) nil)
   ;; don't skip any messages
   (set (make-local-variable 'compilation-skip-threshold) 0))

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("S-<f9>" latex-preview-in-okular)
    ("<f9>"   latex-compile)
    ("<f1>"   latex-toggle-preview-or-russian-spellcheck)

    ("<home>" vim:motion-bwd-paragraph)
    ("<end>"  vim:motion-fwd-paragraph))

  ;; (define-key vim:insert-mode-local-keymap (kbd "<f3>") LaTeX-math-keymap)

  (def-keys-for-map2 vim:visual-mode-local-keymap
    ("z c" TeX-fold-region)
    ("z C" TeX-fold-region)
    ("z o" TeX-fold-clearout-region)
    ("z O" TeX-fold-clearout-region))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:visual-mode-local-keymap
                      vim:motion-mode-local-keymap
                      vim:operator-pending-mode-local-keymap)
    ("%" vim:motion-end-of-line))

  (latex-set-up-document-start-marker)
  (latex-setup-folding)
  (latex-setup-abbrev+))


(provide 'latex-setup)

;;; latex-setup.el ends here
