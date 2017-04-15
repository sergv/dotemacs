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

;;;###autoload
(defun glsl-setup ()
  (cc-setup :define-special-keys nil)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("- ?" glsl-find-man-page)))

;;;###autoload
(add-hook 'glsl-mode-hook #'glsl-setup)

;;;###autoload
(defun glsl-file-magic-function ()
  (if-buffer-has-file
    (let ((ext (and buffer-file-name
                    (file-name-extension buffer-file-name))))
      ;; check for null since .emacs doesn't have extension
      (and ext
           (or (and (string-match-pure? (rx bot
                                            (or "vs"
                                                "fs"
                                                "gs")
                                            eot)
                                        ext)
                    (looking-at-pure? (rxx ((wh (or whitespace (char ?\n))))
                                        bot
                                        (* anything)
                                        "#"
                                        (* wh)
                                        "version"
                                        (+ wh)
                                        (+ (or digit "."))))))))))

;;;###autoload
(push (cons #'glsl-file-magic-function #'glsl-mode) magic-mode-alist)

(provide 'glsl-setup)

;; Local Variables:
;; End:

;; glsl-setup.el ends here
