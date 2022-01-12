;; c++-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 January 2022
;; Description:

(defconst +c-header-exts+ '("h"))
(defconst +c-source-exts+ '("c"))

(defconst +cpp-header-exts+
  (append +c-header-exts+
          '("hh" "hxx" "hpp" "h++" "inl" "inc" "incl" "ino")))
(defconst +cpp-source-exts+
  (append +c-source-exts+
          '("cc" "cxx" "cpp" "c++")))

(defconst +c-extensions+
  (append +c-header-exts+ +c-source-exts+))
(defconst +cpp-extensions+
  (append +cpp-header-exts+ +cpp-source-exts+))

(defun c++-file-magic-function ()
  (when-buffer-has-file
    (let ((ext (file-name-extension buffer-file-name)))
      ;; check for null since .emacs doesn't have extension
      (when (and ext
                 (member ext +cpp-header-exts+))
        (save-excursion
          (save-match-data
            (re-search-forward (rx
                                (or "class"
                                    "namespace"
                                    "::"
                                    ;; it's quite rare to see other template
                                    ;; open brace styles so lets accomodate
                                    ;; only for frequently used ones
                                    (regex "template[[:space:]]*<")
                                    (regex "\\(?:public\\|protected\\|private\\)[[:space:]]*:")))
                               nil
                               t)))))))

;; this will make sure that *.h c++ header will be correctly handled
(add-to-list 'magic-mode-alist (cons #'c++-file-magic-function #'c++-mode))

(add-hook 'c++-mode-hook #'c++-setup)

(add-to-list 'auto-mode-alist '("\\.in\\(?:l\\|c\\|cl\\)\\'" . c++-mode))

(provide 'c++-autoload)

;; Local Variables:
;; End:

;; c++-autoload.el ends here
