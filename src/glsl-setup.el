;; glsl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 14 October 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'cc-setup)

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "."
                       (or (seq (or "vs"
                                    "fs"
                                    "gs")
                                (? ".glsl"))
                           "vert"
                           "frag"
                           "geom"
                           "vsh"
                           "fsh"
                           "gsh")
                       eot)
                   'glsl-mode))

(defvar glsl-other-file-alist)

(eval-after-load "glsl-mode"
  '(progn
     (add-to-list 'glsl-other-file-alist
                  '(("\\.fs\\'" (".vs"))
                    ("\\.vs\\'" (".fs"))))))

;;;###autoload
(defun glsl-setup ()
  (cc-setup :define-special-keys nil)
  (setup-folding 'enable-cpp nil)
  (setup-indent-size 4)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("- ?" glsl-find-man-page)))

;;;###autoload
(add-hook 'glsl-mode-hook #'glsl-setup)

(provide 'glsl-setup)

;; Local Variables:
;; End:

;; glsl-setup.el ends here
