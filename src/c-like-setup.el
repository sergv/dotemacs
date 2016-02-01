;; c-like-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'custom-predicates)

(eval-after-load "cc-styles" ;; (require 'cc-styles)
  '(progn
     (unless (assoc* "my-c-style" c-style-alist :test #'string=?)
       ;; inherited from linux style
       (push '("my-c-style"
               (c-basic-offset  . 8)
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((brace-list-open)
                                          (brace-entry-open)
                                          (substatement-open after)
                                          (block-close . c-snug-do-while)
                                          (arglist-cont-nonempty)))
               (c-cleanup-list . (brace-else-brace))
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro     . 0)
                                   (substatement-open     . 0)
                                   (substatement-label    . 0)
                                   (label                 . 0)
                                   (statement-cont        . +)
                                   (innamespace           . 0))))
             c-style-alist))

     (when (platform-use? 'work)
       (unless (assoc* "work-c++-sophia-style" c-style-alist :test #'string=?)
         ;; inherited from linux style
         (push '("work-c++-sophia-style"
                 (c-basic-offset  . 4)
                 (indent-tabs-mode . nil)
                 (c-comment-only-line-offset . 0)
                 (c-hanging-braces-alist . ((brace-list-open)
                                            (brace-entry-open)
                                            (substatement-open after)
                                            (block-close . c-snug-do-while)
                                            (arglist-cont-nonempty)))
                 (c-cleanup-list . (brace-else-brace))
                 (c-offsets-alist . ((statement-block-intro . +)
                                     (knr-argdecl-intro     . 0)
                                     (substatement-open     . 0)
                                     (substatement-label    . 0)
                                     (label                 . 0)
                                     (statement-cont        . +)
                                     (innamespace           . +))))
               c-style-alist)))

     ;; (setf c-style-alist
     ;;       (remove* "my-java-style" c-style-alist :test #'string=? :key #'first))
     (unless (assoc* "my-java-style" c-style-alist :test #'string=?)
       ;; inherited from linux style
       (push `("my-java-style"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset 0 . 0)
               (c-offsets-alist
                ;; just like in clojure sources
                (topmost-intro . 0)
                (inclass . ,(if (platform-use? 'work)
                              '+
                              0))
                (arglist-intro . +)
                (arglist-close . +)
                (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
                (arglist-cont c-lineup-gcc-asm-reg 0)

                (inline-open . 0)
                (topmost-intro-cont . +)
                (statement-block-intro . +)
                (knr-argdecl-intro . 5)
                (substatement-open . +)
                (substatement-label . +)
                (label . +)
                (statement-case-open . +)
                (statement-cont . +)
                ;; (arglist-intro . c-lineup-arglist-intro-after-paren)
                ;; (arglist-close . c-lineup-arglist)
                (access-label . 0)
                (inher-cont . c-lineup-java-inher)
                (func-decl-cont . c-lineup-java-throws)))
             c-style-alist))

     (setf c-default-style
           `((java-mode . "my-java-style")
             (awk-mode . "awk")
             (c++-mode . ,(if (platform-use? 'work)
                            "work-c++-sophia-style"
                            "my-c-style"))
             (other . ,(cond ((assoc* "my-c-style" c-style-alist :test #'string=?)
                              "my-c-style")
                             (t
                              "linux")))))
     (setq-default c-basic-offset 8)))

(autoload 'c-indentation-indent-buffer "c-indentation" nil t)

;; (autoload 'c-c++-switch-header-and-source "c-setup" nil t)
(push (cons 'c-mode #'c-indentation-indent-buffer)
      *mode-buffer-indent-function-alist*)

(autoload 'c-setup "c-setup")
(add-hook 'c-mode-hook #'c-setup)


(autoload 'c++-setup "c++-setup")
(add-hook 'c++-mode-hook #'c++-setup)

(when (platform-use? 'work)
  (autoload 'c++-indent-buffer "c++-setup" "" t)
  (autoload 'c++-find-related-file "c++-setup" "" t))
(push (cons 'c++-mode #'c-indentation-indent-buffer)
      *mode-buffer-indent-function-alist*)

(when (platform-use? 'work)
  (add-to-list 'auto-mode-alist '("\\.in\\(?:l\\|c\\|cl\\)\\'" . c++-mode)))


(push (cons 'java-mode #'c-indentation-indent-buffer)
      *mode-buffer-indent-function-alist*)

(autoload 'java-setup "java-setup" "" t)
(add-hook 'java-mode-hook #'java-setup)


(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist (cons (rx "."
                                        (or (seq (or "vs."
                                                     "fs."
                                                     "gs.")
                                                 "glsl")
                                            "vert"
                                            "frag"
                                            "geom"
                                            "vsh"
                                            "fsh"
                                            "gsh")
                                        eot)
                                    'glsl-mode))

(autoload 'glsl-setup "glsl-setup")
(add-hook 'glsl-mode-hook #'glsl-setup)


(autoload 'cuda-mode "cuda-mode" nil t)
(autoload 'cuda-setup "cuda-setup" nil nil)
(add-to-list 'auto-mode-alist '("\\.c[ul]h?\\'" . cuda-mode))
(add-hook 'cuda-mode-hook #'cuda-setup)

(autoload 'idl-setup "idl-setup")
(add-hook 'idl-mode-hook #'idl-setup)

(defun c++-file-magic-function ()
  (if-buffer-has-file
    (let ((ext (file-name-extension buffer-file-name)))
      ;; check for null since .emacs doesn't have extension
      (when (and ext
                 (member* ext '("h" "inl" "inc" "incl")
                          :test #'string=))
        (save-excursion
          (save-match-data
            (let ((search-result
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
                                      t)))
              search-result)))))))

;; this will make sure that *.h c++ header will be correctly handled
(push (cons #'c++-file-magic-function #'c++-mode) magic-mode-alist)


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

(push (cons #'glsl-file-magic-function #'glsl-mode) magic-mode-alist)


(provide 'c-like-setup)

;; Local Variables:
;; End:

;; c-like-setup.el ends here
