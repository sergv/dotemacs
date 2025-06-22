;; latex-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 27 August 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'macro-util))

(declare-function TeX-fold-clearout-region "tex-fold")
(declare-function TeX-fold-region "tex-fold")
(declare-function TeX-fold-comment "tex-fold")
(declare-function TeX-fold-item "tex-fold")

(require 'custom)
(require 'preview)
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

;;;###autoload
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
        (def-keys-for-map vim-normal-mode-local-keymap
          ("'"  nil)
          ("\"" nil)))
    (progn
      (def-keys-for-map vim-normal-mode-local-keymap
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

(defhydra-derive hydra-latex-vim-visual-z-ext hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: fold region
_o_: unfold region
"
  ("c" TeX-fold-region)
  ("o" TeX-fold-clearout-region))

(load-library "latex-outline")

;;;###autoload
(defun latex-setup ()
  (init-common :use-whitespace 'tabs-only)
  (setq-local typopunct-typographical-dashes-enabled? nil)
  (typography-setup)
  (tex-fold-mode 1)

  (setup-indent-size 2)

  ;; (menu-bar-mode 1)
  (setq latex:preview nil)

  (setq-local yas-key-syntaxes '("^ >"))
  (modify-syntax-entry ?$ "\"")

  ;; compilation setup
  (when-buffer-has-file
    (let* ((input-file buffer-file-name)
           ;; (result-name (concat
           ;;               (file-name-sans-extension
           ;;                (file-name-nondirectory input-file))
           ;;               ".pdf"))
           ;; (tmp-file (concat +tmp-path+ "/" result-name))
           ;; (result-file (concat (strip-trailing-slash
           ;;                       (file-name-directory input-file))
           ;;                      "/"
           ;;                      result-name))
           )

      (setq-local compile-command
                  (format "pdflatex -halt-on-error -shell-escape --file-line-error '%s'" input-file)

                  ;; (format
                  ;;  (concat "cd '%s' && pdflatex -halt-on-error -shell-escape --file-line-error "
                  ;;          "-output-directory '%s' '%s' && cp '%s' '%s'")
                  ;;  +tmp-path+
                  ;;  +tmp-path+
                  ;;  input-file
                  ;;  tmp-file
                  ;;  result-file)
                  ))

    (setq-local ;; don't ask - just compile
     compilation-read-command nil
     compilation-auto-jump-to-first-error nil
     ;; don't skip any messages
     compilation-skip-threshold 0))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("S-<f9>"       open-buffer-as-pdf)
    (("C-m" "<f9>") latex-compile)
    ("<return>"     newline-and-indent)
    ("<f6>"         latex-toggle-preview-or-russian-spellcheck)

    ("<left>"       prev-w)
    ("<right>"      next-w))

  ;; (define-key vim-insert-mode-local-keymap (kbd "<f3>") LaTeX-math-keymap)

  (def-keys-for-map vim-visual-mode-local-keymap
    ("z" hydra-latex-vim-visual-z-ext/body))

  (latex-set-up-document-start-marker)
  (latex-setup-folding)
  (latex-setup-abbrev+))

(provide 'latex-setup)

;; Local Variables:
;; End:

;; latex-setup.el ends here
