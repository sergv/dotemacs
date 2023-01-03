;; elm-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 April 2017
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'align-util)
(require 'company-eproj)
(require 'company-mode-setup)
(require 'flycheck-setup)
(require 'indentation)
(require 'haskell-smart-operators-mode)

;;;###autoload
(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))

(elm-oracle-setup-completion)

(vim-defcmd vim:elm-load-file (nonrepeatable)
  (elm-repl-load))

(puthash 'elm-mode
         #'elm-format-buffer
         *mode-indent-functions-table*)

(setf elm-format-elm-version "0.18")

;;;###autoload (autoload 'elm-align-on-equals "elm-setup.el" nil t)
(defalign elm-align-on-equals
  "=[^=]"
  :require-one-or-more-spaces t)
;;;###autoload (autoload 'elm-align-on-arrows "elm-setup.el" nil t)
(defalign elm-align-on-arrows
  "\\(?:->\\|→\\)\\(?: \\|$\\)")
;;;###autoload (autoload 'elm-align-on-left-arrows "elm-setup.el" nil t)
(defalign elm-align-on-left-arrows
  "\\(?:<-\\|←\\)\\(?: \\|$\\)")
;;;###autoload (autoload 'elm-align-on-pipes "elm-setup.el" nil t)
(defalign elm-align-on-pipes
  "|\\(?:[^|]\\|$\\)"
  :require-one-or-more-spaces t)
;;;###autoload (autoload 'elm-align-on-commas "elm-setup.el" nil t)
(defalign elm-align-on-commas
  ",\\(?:[^,\)]\\|$\\)")
;;;###autoload (autoload 'elm-align-on-comments "elm-setup.el" nil t)
(defalign elm-align-on-comments
  "--+\\(?: \\|$\\)"
  :require-one-or-more-spaces t)
;;;###autoload (autoload 'elm-align-on-colons "elm-setup.el" nil t)
(defalign elm-align-on-colons
  "\\(?::[^:]\\)")

(vim-defcmd vim:elm-compile (nonrepeatable)
  (elm-compile-buffer))

(defun elm-oracle-extended-doc-at-point ()
  "Show the extended documentation of the value at point."
  (interactive)
  (let ((completion (elm-oracle--completion-at-point)))
    (if completion
        (elm-documentation--show-extended completion)
      (message "Unknown symbol"))))

(defun elm-documentation--show-extended (documentation)
  "Show DOCUMENTATION in a help buffer."
  (let-alist-static documentation (fullName signature args cases type comment)
    (help-setup-xref (list #'elm-documentation--show documentation) nil)
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (point-min)
          (let ((full-name (propertize fullName 'face 'font-lock-function-name-face)))
            (insert full-name)
            (when signature
              (insert (concat " : " signature))
              (when args
                (insert "\n")))
            (when args
              (insert (concat " " (s-join " " args))))
            (when cases
              (let ((first t))
                (mapc
                 (lambda (case)
                   (if first
                       (insert "\n  = ")
                     (insert "\n  | "))
                   (insert (propertize (elt case 0) 'face 'font-lock-function-name-face))
                   (insert (concat " " (s-join " " (elt case 1))))
                   (setq first nil))
                 cases)))
            (when type
              (insert " : ")
              (insert (propertize type 'face 'font-lock-type-face)))
            (insert (concat "\n\n" (trim-whitespace-left comment))))))))  )

(defun elm-enhancements--compilation-filter-remove-ansi-color ()
  (save-match-data
    (let ((end (point)))
      (goto-char compilation-filter-start)
      (beginning-of-line)
      (while (re-search-forward "\\[[0-9]+m" end t)
        (replace-match "" nil nil)))))

(add-hook 'compilation-filter-hook
          'elm-enhancements--compilation-filter-remove-ansi-color)
(remove-hook 'compilation-filter-hook
             #'elm-compile--colorize-compilation-buffer)

(defhydra-ext hydra-elm-dash (:exit t :foreign-keys nil :hint nil)
  "
_d_oc
_t_ype"
  ("t" elm-oracle-type-at-point)
  ("d" elm-oracle-extended-doc-at-point))

(defhydra-ext hydra-elm-align (:exit t :foreign-keys nil :hint nil :base-map (make-sparse-keymap))
  "
_=_:  on equals
_->_: on arrows
_<-_: on left-arrows
_|_:  on pipes
_,_:  on commas
_--_: on comments
_:_:  on colons"
  ("="  elm-align-on-equals)
  ("->" elm-align-on-arrows)
  ("<-" elm-align-on-left-arrows)
  ("|"  elm-align-on-pipes)
  (","  elm-align-on-commas)
  ("--" elm-align-on-comments)
  (":"  elm-align-on-colons))

(defhydra-derive hydra-elm-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign"
  ("a" hydra-elm-align/body))

;;;###autoload
(defun elm-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t
               :use-whitespace 'tabs-only)
  (flycheck-mode +1)
  (haskell-smart-operators-mode +1)
  (company-mode +1)
  (setq-local company-backends '(company-elm))
  (setup-indent-size 4)

  (pretty-ligatures-install-special-haskell-ligatures!)
  (pretty-ligatures-install!)

  (setq-local elm-sort-imports-on-save t

              mode-line-format
              (apply #'default-mode-line-format
                     (when flycheck-mode
                       (list
                        " "
                        '(:eval (flycheck-pretty-mode-line))))))

  (flycheck-install-ex-commands!
   :install-flycheck t
   :load-func #'vim:elm-load-file:interactive)
  (dolist (cmd '("c" "compile"))
    (vim-local-emap cmd #'vim:elm-compile))
  (haskell-setup-folding :enable-hideshow t
                         :enable-cpp nil)

  (def-keys-for-map vim-normal-mode-local-keymap
    (("<f6>" "SPC SPC") elm-repl-load)
    (("C-m" "<f9>")     elm-compile-buffer)
    ("<return>"         newline-and-indent)
    ("-"                hydra-elm-dash/body))

 (def-keys-for-map vim-visual-mode-local-keymap
    ("g" hydra-elm-vim-visual-g-ext/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-t"              flycheck-enhancements-previous-error-with-wraparound)
    ("C-h"              flycheck-enhancements-next-error-with-wraparound)
    ("C-SPC"            company-complete)))

;;;###autoload
(add-hook 'elm-mode-hook #'elm-setup)

;;;###autoload
(defun elm-interactive-setup ()
  (init-common :use-comment nil
               :use-yasnippet nil
               :use-whitespace nil
               :use-fci nil
               :smerge nil)
  (init-repl :create-keymaps t
             :bind-return nil
             :bind-vim:motion-current-line nil)
  (setq-local company-backends '(company-elm))
  (haskell-smart-operators-mode +1)
  (setq-local indent-region-function #'ignore)

  (vim-local-emap "clear" 'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap
                     elm-interactive-mode-map)
    ("C-SPC"            vim:comint-clear-buffer-above-prompt:interactive)
    ("C-w"              backward-delete-word)
    ("C-S-w"            backward-delete-word*)
    ("M-p"              browse-comint-input-history)

    (("C-t" "S-<up>")   comint-previous-prompt)
    (("C-h" "S-<down>") comint-next-prompt)

    ("<up>"             comint-previous-input)
    ("<down>"           comint-next-input)))

;;;###autoload
(add-hook 'elm-interactive-mode-hook #'elm-interactive-setup)

(provide 'elm-setup)

;; Local Variables:
;; End:

;; elm-setup.el ends here
