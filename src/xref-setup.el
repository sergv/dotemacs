;; xref-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  7 January 2021
;; Description:

;;;###autoload
(defun xref-setup ()
  (def-keys-for-map xref--xref-buffer-mode-map
    ("n" nil)
    ("p" nil)
    ("H" xref-revert-buffer)
    ("h" xref-next-line)
    ("t" xref-prev-line)

    (("q" "<escape>") quit-window)))

;;;###autoload
(add-hook 'xref--xref-buffer-mode-hook #'xref-setup)

(provide 'xref-setup)

;; Local Variables:
;; End:

;; xref-setup.el ends here
