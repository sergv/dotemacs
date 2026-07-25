;; xref-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  7 January 2021
;; Description:

(eval-when-compile
  (require 'macro-util))

(autoload 'xref-next-line "xref" nil t)
(autoload 'xref-prev-line "xref" nil t)

(defvar xref--xref-buffer-mode-map)

(with-eval-after-load 'xref
  (def-keys-for-map xref--xref-buffer-mode-map
    ("n" :remove)
    ("p" :remove)
    ("H" revert-buffer)
    ("h" xref-next-line)
    ("t" xref-prev-line)

    (("q" "<escape>") quit-window)))

;;;###autoload
(defun xref-setup ())

;;;###autoload
(add-hook 'xref--xref-buffer-mode-hook #'xref-setup)

(provide 'xref-setup)

;; Local Variables:
;; End:

;; xref-setup.el ends here
