;; html-setup.el --- -*- lexical-binding: t; -*-

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


(defvar *hexcolour-keywords*
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))


(defvar-local *markup-tags-context-func*
  (lambda ()
    (error "no `*markup-tags-context-func*' function specified for %s mode"
           major-mode))
  "Function that should return a pair ((start1 . end1) . (start2 . end2))
containing the boundaries of the current start and end tag, or nil. Note that
end1 and end2 should be exclusive ends of tags.")


(defmacro with-html-tags-context (bb be eb ee on-found &optional on-not-found)
  "Execute BODY with BB, BE, EB and EE bound to enclonig tags' boundaries
if such tag can be found."
  (let ((test-var (gensym)))
    `(let ((,test-var (funcall *markup-tags-context-func*)))
       (if ,test-var
         (destructuring-bind ((,bb . ,be) . (,eb . ,ee))
             ,test-var
           ,on-found)
         ,on-not-found))))

(vim:defmotion vim:motion-jump-tag (inclusive)
  "If point is positioned inside tag then jump to the beginning
of the matching tag, else fallback to `vim:motion-jump-item'."
  (macrolet ((inside? (x low high)
                      `(and (<= ,low ,x) (< ,x ,high))))
    (if (let ((synt (char-syntax (char-after))))
          (or (char=? synt ?\()
              (char=? synt ?\))))
      (vim:motion-jump-item)
      (let ((tag-start (point))
            (type nil)
            ;; note: be and ee are exclusive ends
            (bb nil)   ;; beginning of beginning tag
            (be nil)   ;; end       of beginning tag
            (eb nil)   ;; beginning of end       tag
            (ee nil)   ;; end       of end       tag
            )
        (save-excursion
          ;; handle case when we're inside the tag (_|_ being the point):
          ;; <foo_|_> or </foo_|_>
          (with-html-tags-context pre-bb pre-be pre-eb pre-ee
                                  (let ((p (point)))
                                    (cond ((inside? p pre-bb pre-be)
                                           (setf type 'start-tag
                                                 tag-start pre-bb))
                                          ((inside? p pre-eb pre-ee)
                                           (setf type 'end-tag
                                                 tag-start pre-eb)))
                                    (when type
                                      (setf bb pre-bb
                                            be pre-be
                                            eb pre-eb
                                            ee pre-ee))))
          ;; if prev step yielded nothing
          (unless type
            ;; scan one token at a time
            (while (and (setf type (nxml-tokenize-forward))
                        (not (memq type '(start-tag end-tag partial-end-tag))))
              (setf tag-start (point)))
            ;; if interesting tag was found
            (when (memq type '(start-tag end-tag partial-end-tag))
              (goto-char tag-start)
              (with-html-tags-context pre-bb pre-be pre-eb pre-ee
                                      (setf bb pre-bb
                                            be pre-be
                                            eb pre-eb
                                            ee pre-ee)))))
        (if (not (null? bb)) ;; if bb is non-nil then interesting type was found
          (progn
            ;; combined case of tag and open/closing paren
            (vim:add-jump)
            (let ((next-open (condition-case nil
                                 (1- (scan-lists (point) 1 -1))
                               (error (point-max))))
                  (next-close (condition-case nil
                                  (1- (scan-lists (point) 1 +1))
                                (error (point-max)))))
              (let ((pos (min next-open next-close)))
                (if (>= pos (line-end-position))
                  ;; original error "No matching item found on the current line"
                  (setf pos tag-start)
                  ;; else use closest position
                  (setf pos (min pos tag-start)))
                (cond ((= pos next-open)
                       (goto-char pos)
                       (forward-list)
                       (backward-char))
                      ((= pos next-close)
                       (goto-char (1+ pos))
                       (backward-list))
                      ((inside? pos bb be)
                       ;; ee is an exclusive end
                       (goto-char (- ee 1)))
                      ((inside? pos eb ee)
                       (goto-char bb))
                      (else
                       (error "No matching item found"))))))
          (vim:motion-jump-item))))))


(vimmize-motion nxml-backward-up-element
                :name vim:nxml-backward-up-element
                :exclusive nil)

(defun markup-forward-up-element ()
  "Similar to `vim:lisp-up-list' - jump to the end of enclosing tag exclusively."
  (interactive)
  (with-html-tags-context bb be eb ee
                          (goto-char ee)))

(vimmize-motion markup-forward-up-element
                :name vim:markup-forward-up-element
                :exclusive t
                :do-not-adjust-point t)


(defun markup-setup ()
  (init-common :use-nxhtml-menu t)
  (hl-tags-mode t)
  (foldit-mode -1)

  (hs-minor-mode t)
  (setq-local hs-set-up-overlay 'nxhtml-hs-set-up-overlay)
  (put 'hs-set-up-overlay 'permanent-local t)

  (modify-syntax-entry ?\" "\"")
  (autopair-mode t)

  (font-lock-add-keywords nil *hexcolour-keywords*)

  (setf vim:normal-mode-local-keymap           (make-sparse-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<up>"   sgml-skip-tag-backward)
    ("<down>" sgml-skip-tag-forward)
    ("<up>"   sgml-skip-tag-backward)
    ("<down>" sgml-skip-tag-forward)
    ("<home>" sgml-skip-tag-backward)
    ("<end>"  sgml-skip-tag-forward)

    ("z o"    hs-show-block)
    ("z c"    hs-hide-block)
    ("z C"    hs-hide-all)
    ("z O"    hs-show-all)

    ;; ("q"      markup-forward-up-element)
    )

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:motion-mode-local-keymap
                     vim:operator-pending-mode-local-keymap)
    ("m" vim:motion-jump-tag)

    ("=" vim:nxml-backward-up-element)
    ("Q" vim:nxml-backward-up-element)
    ("q" vim:markup-forward-up-element)))

(defun html-setup ()
  (markup-setup)
  (setf *markup-tags-context-func* #'hl-tags-context-sgml-mode)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f9>" browse-url-of-buffer)))


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
  (setf *markup-tags-context-func* #'hl-tags-context-nxml-mode)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g <tab>" nxhtml-reindent-enclosing-tag)))


(defun nxml-setup ()
  (markup-setup)
  (setf *markup-tags-context-func* #'hl-tags-context-nxml-mode))

(add-hook 'html-mode-hook #'html-setup)
(add-hook 'sgml-mode-hook #'html-setup)
(add-hook 'nxhtml-mode-hook #'nxhtml-setup)
(add-hook 'nxml-mode-hook #'nxml-setup)

(provide 'html-setup)

;; Local Variables:
;; End:

;; html-setup.el ends here
