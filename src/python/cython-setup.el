;; cython-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 12 June 2012
;; Description:

(eval-when-compile
  (require 'cl))
(require 'common)
(require 'python-common)
;; (require 'python-setup)

(autoload 'cython-mode "cython-mode" "Cythom mode." t)

(add-to-list 'auto-mode-alist (cons (rx "."
                                        (or "pyx"
                                            "pxd"
                                            "pxi")
                                        string-end)
                                    'cython-mode))

(register-python-hideshow 'cython-mode)

(defun cython-setup ()
  (python-common-setup)
  (def-keys-for-map vim:normal-mode-local-keymap
    (", d"     pylookup-lookup)
    ("<f9>"    cython-compile))

  (pabbrev-mode 1)
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("M-/"     pabbrev-show-menu ;; pabbrev-expand-maybe
               ))

  (when pabbrev-mode
    (pabbrev-scavenge-buffer)))

(add-hook 'cython-mode-hook #'cython-setup)


(provide 'cython-setup)

;; Local Variables:
;; End:

;; cython-setup.el ends here
