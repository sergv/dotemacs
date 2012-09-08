;;; html-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 26 August 2011
;; Keywords:
;; Requirements:
;; Status:

(add-to-list 'load-path (concat +emacs-standalone-path+ "/nxhtml"))

(require 'nxhtml-autostart)

(add-hook 'rnc-mode-hook #'init-common)

(autoload 'hl-tags-mode "hl-tags-mode" nil t)

(add-to-list 'magic-mode-alist '("<!DOCTYPE html .+DTD XHTML .+>" . nxhtml-mode))

;; (eval-after-load
;;  "rng-loc"
;;  '(progn
;;    (setf rng-schema-locating-files
;;          (cons (concat +emacs-standalone-path+
;;                        "/xhtml-transitional-in-nxml/schemas.xml")
;;                rng-schema-locating-files-default))))

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

;; hideshow special mode
(setq hs-special-modes-alist (assq-delete-all 'nxhtml-mode hs-special-modes-alist))
(add-to-list 'hs-special-modes-alist
             '(nxhtml-mode
               "<[^/>]>\\|<[^>]*"
               "</"
               "<!--" ;; won't work on its own; uses syntax table
               nxhtml-hs-forward-sexp-func
               nil))


(defvar *hexcolour-keywords*
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
         (match-end 0)
         'face (list :background
                (match-string-no-properties 0)))))))



(defun markup-setup ()
  (init-common :use-nxhtml-menu t)
  (hl-tags-mode 1)
  (foldit-mode -1)

  (hs-minor-mode 1)
  (set (make-local-variable 'hs-set-up-overlay) 'nxhtml-hs-set-up-overlay)
  (put 'hs-set-up-overlay 'permanent-local t)

  (modify-syntax-entry ?\" "\"")
  (autopair-mode 1)

  (font-lock-add-keywords nil *hexcolour-keywords*)

  (setf vim:normal-mode-local-keymap (make-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("="      nxml-backward-up-element)

    ("<up>"   sgml-skip-tag-backward)
    ("<down>" sgml-skip-tag-forward)
    ("<up>"   sgml-skip-tag-backward)
    ("<down>" sgml-skip-tag-forward)
    ("<home>" sgml-skip-tag-backward)
    ("<end>"  sgml-skip-tag-forward)

    ("z o"    hs-show-block)
    ("z c"    hs-hide-block)
    ("z C"    hs-hide-all)
    ("z O"    hs-show-all)))


(defun html-setup ()
  (markup-setup)
  (setf *hl-tags-context-func* #'hl-tags-context-sgml-mode)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f1>"   browse-url-of-buffer)
    ("<f9>"   browse-url-of-buffer)))


(defun nxhtml-reindent-enclosing-tag ()
  (interactive)
  (let ((p (point-marker))
        (context (hl-tags-context-nxml-mode))
        (pnt (point)))
    (destructuring-bind ((start1 . end1) start2 . end2)
        context
      ;; if we're inside tag
      (when (or (and (<= start1 pnt)
                     (<= pnt end1))
                (and (<= start2 pnt)
                     (<= pnt end2)))
        (nxml-backward-up-element 2)
        (setf context (hl-tags-context-nxml-mode)))
      (destructuring-bind ((start1 . end1) start2 . end2)
          context
        (reindent-region start1 end2)))
    (goto-char p)
    (move-marker p nil)))

(defun nxhtml-setup ()
  (html-setup)
  (setf *hl-tags-context-func* #'hl-tags-context-nxml-mode)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g <tab>" nxhtml-reindent-enclosing-tag)))

(defun nxml-setup ()
  (markup-setup)
  (setf *hl-tags-context-func* #'hl-tags-context-nxml-mode))

(add-hook 'html-mode-hook #'html-setup)
(add-hook 'sgml-mode-hook #'html-setup)
(add-hook 'nxhtml-mode-hook #'nxhtml-setup)
(add-hook 'nxml-mode-hook #'nxml-setup)

(provide 'html-setup)

;;; html-setup.el ends here
