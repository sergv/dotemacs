;; glsl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 14 October 2012
;; Description:

(require 'cc-setup)

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist (cons (rx "."
                                        (or "glsl"
                                            "vert"
                                            "frag"
                                            "geom")
                                        eot)
                                    'glsl-mode))

(defun glsl-setup ()
  (cc-setup :define-special-keys nil :use-c-eldoc nil)

  (def-keys-for-map vim:normal-mode-local-keymap
    (", ?" glsl-find-man-page)))

(add-hook 'glsl-mode-hook #'glsl-setup)

(provide 'glsl-setup)

;; Local Variables:
;; End:

;; glsl-setup.el ends here
