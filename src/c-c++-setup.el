;; c-c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 December 2011
;; Keywords:
;; Requirements:
;; Status:

(autoload 'c-turn-on-eldoc-mode "c-eldoc" nil t)
;; (autoload 'c-c++-switch-header-and-source "c-setup" nil t)
(autoload 'c-setup "c-setup")
(add-hook 'c-mode-hook #'c-setup)

(autoload 'c++-setup "c++-setup")
(add-hook 'c++-mode-hook #'c++-setup)


(defun c++-file-magic-function ()
  (interactive)
  (let ((ext (file-name-extension (buffer-file-name))))
    ;; check for null since .emacs doesn't have extension
    (when (and ext
               (string-match-p (rx (* anything) ".h" eol)
                               ext))
      (save-excursion
       (save-match-data
        (re-search-forward (rx
                            (or "class"
                                "namespace"
                                "::"
                                ;; it's quite rare to see other template
                                ;; open brace styles so lets accomodate
                                ;; only for frequently used ones
                                "template<"
                                "template <"
                                "public:"
                                "protected:"
                                "private:"))
                           nil
                           t))))))

;; this will make sure that *.h c++ header will be correctly handled
(push (cons #'c++-file-magic-function #'c++-mode) magic-mode-alist)



;; Local Variables:
;; End:

;; c-c++-setup.el ends here
