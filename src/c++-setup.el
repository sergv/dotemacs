;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'macro-util))

(defvar c-basic-offset)

(require 'c++-abbrev+)
(require 'cc-setup)
(require 'clang-format)
(require 'common)
(require 'compilation-setup)
(require 'configurable-compilation)
(require 'flycheck)
(require 'indentation)
(require 'lsp-setup)
(require 'select-mode)

(defvar *c++-related-file-cache* (make-hash-table :test 'equal))

(defun c++-find-related-file (&optional new-window)
  (interactive)
  (if lsp-mode
      (call-interactively #'lsp-clangd-find-other-file new-window)
    (let* ((filename buffer-file-name))
      (aif (gethash filename *c++-related-file-cache* nil)
          (find-file it)
        (let* ((path       (split-string filename "/"))
               (ext        (file-name-extension filename))
               (file-dir   (file-name-directory filename))
               (file-nodir (file-name-nondirectory (car (last path))))


               (header-exts '("h" "hh" "hpp" "hxx"))
               (inline-exts '("inc" "inl" "incl"))
               (source-exts '("cc" "cc" "cpp" "cxx"))
               (alt-exts (cond ((member ext header-exts)
                                source-exts)
                               ((or (member ext inline-exts)
                                    (member ext source-exts))
                                header-exts)
                               (t
                                nil)))
               (alternative-names
                (--map (concat (file-name-sans-extension file-nodir)
                               "."
                               it)
                       alt-exts))
               (alt-names-in-same-dir
                (--map (concat file-dir "/" it)
                       alternative-names)))
          (letrec ((path-join (lambda (path) (join-lines path "/")))
                   (find-subroot
                    (lambda (path needle)
                      (let ((dir (funcall path-join
                                          (append path
                                                  (list needle)))))
                        (cond ((null path)
                               (error "No %s subdirectory found while moving upward starting from %s"
                                      needle
                                      file-dir))
                              ((file-exists? dir)
                               path)
                              (t
                               (funcall find-subroot
                                        (butlast path)
                                        needle)))))))
            (aif (-find #'file-exists? alt-names-in-same-dir)
                (progn
                  (puthash filename it *c++-related-file-cache*)
                  (puthash it filename *c++-related-file-cache*)
                  (find-file it))
              ;; note: subroot - root of some git submodule
              (let ((subroot (funcall find-subroot
                                      path
                                      (cond ((member ext header-exts)
                                             "src")
                                            ((member ext inline-exts)
                                             (file-name-directory (car (last path))))
                                            ((member ext source-exts)
                                             "include")))))
                (aif (find-rec (funcall path-join subroot)
                               :filep (lambda (p)
                                        (member (file-name-nondirectory p) alternative-names)))
                    (pcase it
                      (`(,related)
                       (puthash filename related *c++-related-file-cache*)
                       (puthash related filename *c++-related-file-cache*)
                       (find-file related))
                      (choices
                       (select-mode-start-selection
                        choices
                        :buffer-name "select file"
                        :after-init #'select-mode-setup
                        :on-selection
                        (lambda (_idx alt-file selection-type)
                          (select-mode-exit)
                          (puthash filename alt-file *c++-related-file-cache*)
                          (puthash alt-file filename *c++-related-file-cache*)
                          (funcall
                           (pcase selection-type
                             (`same-window  #'find-file)
                             (`other-window #'find-file-other-window))
                           alt-file))
                        :item-show-function
                        (lambda (x) (concat x "\n"))
                        :preamble
                        (lambda () (concat "Select desired alternative file\n")))))
                  (error "No %s file found for %s"
                         (--map (concat "*." it) alt-exts)
                         filename))))))))))

(defun c++-format-buffer ()
  (interactive)
  (clang-format-buffer (format "{ IndentWidth: %s }" c-basic-offset)))

(defun c++-indent-region (start end)
  (clang-format-region start end (format "{ IndentWidth: %s }" c-basic-offset)))

(defun c++-format-region ()
  (interactive)
  (with-region-bounds start end
    (c++-indent-region start end)))

(puthash 'c++-mode
         #'c++-format-buffer
         *mode-indent-functions-table*)

(defvar c++-compile--build-presets-history nil)

(defconst +c++-compilation-presets+
  `((make . ,(lambda (proj-dir) (make-cc-command '("make") nil proj-dir "make")))))

(defhydra hydra-c++-toggle (:exit nil :foreign-keys nil :hint nil)
  "
Toggle:
_f_ormatting on typing             %`lsp-enable-on-type-formatting
_h_ighlight of symbol at point     %`lsp-enable-symbol-highlighting
"
  ("f" lsp-toggle-on-type-formatting)
  ("h" lsp-toggle-symbol-highlight))

(defhydra-ext hydra-c++-dash (:exit t :foreign-keys nil :hint nil)
  "
_a_ctions                 toggle some _o_ptions
_d_ocumentation
_e_xplain error at point
_i_mplementations
_r_ename
_u_sages"
  ("a" lsp-execute-code-action)
  ("d" lsp-doc-other-window)
  ("e" flycheck-explain-error-at-point)
  ("i" lsp-symbnav/find-implementations)
  ("r" lsp-rename)
  ("u" lsp-symbnav/find-references)

  ("o" hydra-c++-toggle/body))

(defhydra-ext hydra-c++-visual-dash (:exit t :foreign-keys nil :hint nil)
  "
_a_ctions"
  ("a" lsp-execute-code-action))

(vim-defcmd vim:c++-flycheck-reset (nonrepeatable)
  (vim:flycheck-clear:wrapper)
  (when (and (boundp 'flycheck-checker)
             (eq flycheck-checker 'lsp))
    (lsp-workspace-restart (lsp--read-workspace))))

(defhydra-derive hydra-c++-vim-visual-g-ext hydra-c-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_<tab>_: format region
"
  ("<tab>" c++-format-region))

;;;###autoload
(defun c++-setup ()
  (cc-setup :define-special-keys t)
  (cc-setup/set-up-c-basic-offset)
  (setq-local company-backends
              '(company-clang
                company-files
                (company-eproj company-dabbrev-code company-keywords)
                company-dabbrev)
              indent-tabs-mode nil)
  (setup-folding 'enable-cpp '(:header-symbol "/" :length-min 3))

  (configurable-compilation-install-command-presets!
   +c++-compilation-presets+
   'c++-compile--build-presets-history
   'c++-cc-mode)

  (let (;; NB may be nil.
        (proj (eproj-get-project-for-buf-lax (current-buffer))))

    (setq-local company-backends
                `(company-files
                  ,(if proj
                       '(company-eproj company-dabbrev-code company-keywords)
                     '(company-dabbrev-code company-keywords))
                  company-dabbrev))

    (eproj-setup-local-variables proj)
    (flycheck-setup-from-eproj proj 'lsp))

  (flycheck-install-ex-commands!
   :install-flycheck flycheck-mode
   :reset-func #'vim:c++-flycheck-reset:interactive)

  (vim-local-emap "compile"  'vim:c++-compile)
  (vim-local-emap "c"        'vim:c++-compile)
  (vim-local-emap "ccompile" 'vim:c++-compile-choosing-command)
  (vim-local-emap "cc"       'vim:c++-compile-choosing-command)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC" c++-find-related-file)
    ("-"       hydra-c++-dash/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("-"       hydra-c++-visual-dash/body)
    ("g"       hydra-c++-vim-visual-g-ext/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    (("C-m" "<f9>") vim:c++-compile:interactive)
    ("<return>"     newline-and-indent)
    ("C-h"          flycheck-enhancements-next-error-with-wraparound)
    ("C-t"          flycheck-enhancements-previous-error-with-wraparound)
    ("M-h"          compilation-navigation-next-error-other-window)
    ("M-t"          compilation-navigation-prev-error-other-window)
    ;; ("C-SPC" company-complete)
    )

  (c++-abbrev+-setup)
  ;; (setup-eproj-symbnav)
  (setup-lsp-symbnav)

  (setq-local company-idle-delay 0.0
              company-minimum-prefix-length 1
              lsp-idle-delay 0.1
              lsp-clients-clangd-args
              (list "--header-insertion-decorators=0"
                    (format "--fallback-style={ IndentWidth: %s }" c-basic-offset)
                    "--clang-tidy"
                    "--suggest-missing-includes"
                    "--cross-file-rename")
              lsp-enable-indentation nil)
  (lsp)
  (setq-local indent-region-function #'c++-indent-region)
  (when lsp-mode
    (def-keys-for-map vim-normal-mode-local-keymap
      ("C-r" lsp-rename))))

(vim-defcmd vim:c++-compile (nonrepeatable)
  (configurable-compilation-start nil))
(vim-defcmd vim:c++-compile-choosing-command (nonrepeatable)
  (configurable-compilation-start t))

(provide 'c++-setup)

;; c++-setup.el ends here
