;; html-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(add-to-list 'load-path (concat +emacs-standalone-path+ "/nxhtml"))

;; (require 'nxhtml-autostart)

(add-hook 'rnc-mode-hook #'init-common)

(autoload 'nxhtml-hs-forward-sexp-func "nxhtml-mode")

(autoload 'hl-tags-mode "hl-tags-mode" nil t)
(autoload 'sgml-skip-tag-backward "sgml-mode" nil t)
(autoload 'sgml-skip-tag-forward "sgml-mode" nil t)

(add-to-list 'magic-mode-alist '("<!DOCTYPE html .+DTD XHTML .+>" . nxhtml-mode))
(add-to-list 'magic-mode-alist '("<[ ?]*xml " . nxml-mode))

;; (eval-after-load
;;  "rng-loc"
;;  '(progn
;;    (setf rng-schema-locating-files
;;          (cons (concat +emacs-standalone-path+
;;                        "/xhtml-transitional-in-nxml/schemas.xml")
;;                rng-schema-locating-files-default))))

(eval-after-load "nxhtml-autostart"
  '(progn
     (setf load-path
           (remove-if (lambda (path)
                        (string-match-pure? "/nxhtml/tests/?$"
                                            path))
                      load-path))
     (setf auto-mode-alist
           (cons (cons "\\.html\\'" 'nxhtml-mode)
                 (remove* "\\.html\\'"
                          auto-mode-alist
                          :test #'string=
                          :key #'car)))

     (setq hs-special-modes-alist
           (assq-delete-all 'nxml-mode
                            (assq-delete-all 'nxhtml-mode
                                             hs-special-modes-alist)))
     (dolist (mode '(nxml-mode nxhtml-mode))
       (add-to-list 'hs-special-modes-alist
                    `(,mode
                      "<[^/>]>\\|<[^>]*"
                      "</"
                      "<!--" ;; won't work on its own; uses syntax table
                      nxhtml-hs-forward-sexp-func
                      nil)))
     ))



;; hideshow special mode

(defparameter *hexcolour-keywords*
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))


(autoload 'vim:motion-jump-tag "html-setup" "" t)
(autoload 'vim:nxml-backward-up-element "html-setup" "" t)
(autoload 'markup-forward-up-element "html-setup" "" t)
(autoload 'vim:markup-forward-up-element "html-setup" "" t)

(autoload 'markup-setup "html-setup")
(autoload 'html-setup "html-setup")
(autoload 'nxhtml-reindent-enclosing-tag "html-setup" "" t)
(autoload 'nxhtml-setup "html-setup")
(autoload 'nxml-setup "html-setup")

(add-hook 'html-mode-hook #'html-setup)
(add-hook 'sgml-mode-hook #'html-setup)
(add-hook 'nxhtml-mode-hook #'nxhtml-setup)
(add-hook 'nxml-mode-hook #'nxml-setup)

(provide 'html-autoload)

;; Local Variables:
;; End:

;; html-autoload.el ends here
