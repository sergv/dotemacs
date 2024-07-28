;; c-like-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl))

(require 'custom-predicates)

(eval-after-load "cc-styles"
  '(progn
     (unless (assoc "my-c-style" c-style-alist)
       ;; inherited from linux style
       (push '("my-c-style"
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
                                   (innamespace           . 0))))
             c-style-alist))

     ;; (setf c-style-alist
     ;;       (remove* "my-java-style" c-style-alist :test #'string=? :key #'first))
     (unless (assoc "my-java-style" c-style-alist)
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

     (unless (assoc "android-java-style" c-style-alist)
       (push `("android-java-style"
               (c-basic-offset . 8)
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset 0 . 0)
               (c-offsets-alist
                (topmost-intro . 0)
                (inclass . +)
                (inexpr-class . 0)

                (arglist-intro . +)
                (arglist-close . 0)
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
                (access-label . 0)
                (inher-cont . c-lineup-java-inher)
                (func-decl-cont . c-lineup-java-throws)))
             c-style-alist))

     (setf c-default-style
           `((java-mode . "my-java-style")
             (awk-mode . "awk")
             (c++-mode . "my-c-style")
             (other . ,(cond ((assoc "my-c-style" c-style-alist)
                              "my-c-style")
                             (t
                              "linux")))))

     (setq-default c-basic-offset 4)))

(provide 'c-like-setup)

;; Local Variables:
;; End:

;; c-like-setup.el ends here
