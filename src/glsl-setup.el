;; glsl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 14 October 2012
;; Description:

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

(eval-after-load "glsl-mode"
  '(progn
     (add-to-list 'glsl-other-file-alist
                  '(("\\.fs\\'" (".vs"))
                    ("\\.vs\\'" (".fs"))))))

;;;###autoload
(defun glsl-setup ()
  (cc-setup :define-special-keys nil)
  (setup-folding t nil)
  (setup-indent-size 4)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("- ?" glsl-find-man-page)))

;;;###autoload
(add-hook 'glsl-mode-hook #'glsl-setup)

;;;###autoload
(defun glsl-file-magic-function ()
  (when-buffer-has-file
    (let ((ext (and buffer-file-name
                    (file-name-extension buffer-file-name))))
      ;; check for null since .emacs doesn't have extension
      (and ext
           (or (and (string-match-p (rx bot
                                        (or "vs"
                                            "fs"
                                            "gs")
                                        eot)
                                    ext)
                    (looking-at-p (rx-let ((wh (or whitespace (char ?\n))))
                                    (rx bot
                                        (* anything)
                                        "#"
                                        (* wh)
                                        "version"
                                        (+ wh)
                                        (+ (or digit ".")))) )))))))

;;;###autoload
(add-to-list 'magic-mode-alist (cons #'glsl-file-magic-function #'glsl-mode))

(provide 'glsl-setup)

;; Local Variables:
;; End:

;; glsl-setup.el ends here
