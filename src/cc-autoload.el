;; cc-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 January 2022
;; Description:

(eval-when-compile
  (require 'macro-util))

(defconst +c-like-modes+ '(c-mode c++-mode cuda-mode glsl-mode idl-mode java-mode))

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

(add-to-list 'magic-mode-alist (cons #'glsl-file-magic-function #'glsl-mode))

(provide 'cc-autoload)

;; Local Variables:
;; End:

;; cc-autoload.el ends here
