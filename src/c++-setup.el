;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(defun c++-setup ()
  (init-common :use-render-formula t)
  (autopair-mode 1)

  (hs-minor-mode 1)
  (c-turn-on-eldoc-mode)

  (modify-syntax-entry ?_ "_")

  (setf whitespace-line-column 80
        whitespace-style '(tabs lines-tail)
        tab-width 4)

  (if-buffer-has-file
   (set (make-local-variable 'compile-command)
        (let* ((fname  (file-name-nondirectory buffer-file-name))
               (target (file-name-sans-extension fname)))
          (mapconcat #'identity
                     (list "g++"
                           ;; "-std=c++0x"
                           "-W"
                           "-Wall"
                           "-Wextra"
                           "-Weffc++"
                           "-Wold-style-cast"
                           "-Woverloaded-virtual"
                           "-Wconversion"
                           "-Wuninitialized"
                           "-Wshadow"
                           "-pedantic"
                           "-O2"
                           "-I."
                           "-o"
                           target
                           fname)
                     " "))))

  (if-has-makefile-command
   (set (make-local-variable 'compile-command)
        (concat "make " (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))))

  (which-function-mode -1)

  (setq c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  ;; (subword-mode t)

  (setq vim:normal-mode-local-keymap (make-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC" ff-find-related-file)
    ("g TAB"   c-indent-defun)

    ("g t"     c-end-of-defun)
    ("g n"     c-beginning-of-defun)

    ("z o"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     hs-hide-all)
    ("z O"     hs-show-all)))


(provide 'c++-setup)

;; c++-setup.el ends here
