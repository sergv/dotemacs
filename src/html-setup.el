;; html-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 26 August 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'el-patch)
(require 'indentation)
(require 'nxml-mode)

;;;###autoload
(el-patch-feature rng-valid)

;;;###autoload
(autoload 'web-mode "web-mode" nil t)
;;;###autoload
(autoload 'nxml-tokenize-forward "nxml-mode" nil nil)
;;;###autoload
(autoload 'sgml-skip-tag-backward "sgml-mode" nil t)
;;;###autoload
(autoload 'sgml-skip-tag-forward "sgml-mode" nil t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.x?html?\\'" . web-mode))

;;;###autoload
(add-to-list 'magic-mode-alist '("<[ ?]*xml " . nxml-mode))

(dolist (mode '(nxml-mode web-mode))
  (setf hs-special-modes-alist
        (cons `(,mode
                "<!--\\|<[^/>]*[^/]\\|[\(\[\{]" ;; start
                "-->\\|</[^/>]*[^/]\\|[\)\]\}]" ;; end
                "<!--"                          ;; commend-start, won't work on its own; uses syntax table
                my-nxml-forward-element
                nil)
              (assq-delete-all mode hs-special-modes-alist))))

(defvar-local *markup-tags-context-func*
  (lambda ()
    (error "no `*markup-tags-context-func*' function specified for %s mode"
           major-mode))
  "Function that should return a pair ((start1 . end1) . (start2 . end2))
containing the boundaries of the current start and end tag, or nil. Note that
end1 and end2 should be exclusive ends of tags.")

;;;###autoload
(defun my-nxml-forward-element (n)
  (let ((c (char-after)))
    (if (memq c '(?\( ?\[ ?\{))
        (let ((forward-sexp-function nil))
          (forward-sexp n)
          (forward-char -1))
      (let ((nxml-sexp-element-flag (not (looking-at-p "<!--"))))
        (condition-case nil
            (progn
              (nxml-forward-balanced-item n)
              (skip-to-indentation))
          (error nil))))))

(eval-when-compile
  (defmacro with-html-tags-context (bb be eb ee on-found &optional on-not-found)
    "Execute BODY with BB, BE, EB and EE bound to enclonig tags' boundaries
if such tag can be found."
    (declare (indent 5))
    (let ((test-var '#:test))
      `(let ((,test-var (funcall *markup-tags-context-func*)))
         (if ,test-var
             (destructuring-bind ((,bb . ,be) . (,eb . ,ee))
                 ,test-var
               ,on-found)
           ,on-not-found)))))

;;;###autoload (autoload 'vim:motion-jump-tag "html-setup" "" t)
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
            (bb nil) ;; beginning of beginning tag
            (be nil) ;; end       of beginning tag
            (eb nil) ;; beginning of end       tag
            (ee nil) ;; end       of end       tag
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
            (let (
                  ;; (nxml-sexp-element-flag (not (looking-at-p "<!--")))
                  )
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
                                              ee pre-ee))))))
        (if bb ;; if bb is non-nil then interesting type was found
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
                      (t
                       (error "No matching item found")))))
          (vim:motion-jump-item))))))

(eval-after-load "rng-valid"
  '(progn
     ;; propertize Invalid message with distinctive face
     (el-patch-defun rng-compute-mode-line-string ()
       (cond (rng-validate-timer
              (format " Validated:%d%%"
                      (if (= 0 (buffer-size))
                          0
                        (floor (- rng-validate-up-to-date-end (point-min))
                               (- (point-max) (point-min))))))
             ((> rng-error-count 0)
              (concat " "
                      (propertize "Invalid"
                                  (el-patch-add 'face 'error)
                                  'help-echo "mouse-1: go to first error"
                                  'local-map (make-mode-line-mouse-map
                                              'mouse-1
                                              'rng-mouse-first-error))))
             (t " Valid")))))

;;;###autoload (autoload 'vim:nxml-backward-up-element "html-setup" "" t)
(vimmize-motion nxml-backward-up-element
                :name vim:nxml-backward-up-element
                :exclusive nil)

;;;###autoload
(defun markup-forward-up-element ()
  "Similar to `vim:lisp-up-list' - jump to the end of enclosing tag exclusively."
  (interactive)
  (with-html-tags-context _bb _be _eb ee
                          (goto-char ee)))

;;;###autoload (autoload 'vim:markup-forward-up-element "html-setup" "" t)
(vimmize-motion markup-forward-up-element
                :name vim:markup-forward-up-element
                :exclusive t
                :do-not-adjust-point t)

(defvar *hexcolour-keywords*
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))

;;;###autoload
(defun markup-setup (tags-context-func)
  (init-common :use-whitespace 'tabs-only)
  (setup-folding t nil)
  (hl-tags-mode t)

  (setq-local yas-fallback-behavior 'call-other-command
              *markup-tags-context-func* tags-context-func)

  (put 'hs-set-up-overlay 'permanent-local t)

  (modify-syntax-entry ?\" "\"")

  (font-lock-add-keywords nil *hexcolour-keywords*)


  (def-keys-for-map vim:normal-mode-local-keymap
    ("<up>"   sgml-skip-tag-backward)
    ("<down>" sgml-skip-tag-forward)
    ;; ("q"      markup-forward-up-element)
    )

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:motion-mode-local-keymap
                     vim:operator-pending-mode-local-keymap)
    ("m" vim:motion-jump-tag)

    ("'" vim:nxml-backward-up-element)
    ("q" vim:markup-forward-up-element)))

;;;###autoload
(defun html-setup ()
  (markup-setup #'hl-tags-context-sgml-mode)
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    (("C-m" "<f9>")  browse-url-of-buffer)))

;;;###autoload
(add-hook 'html-mode-hook #'html-setup)
;;;###autoload
(add-hook 'sgml-mode-hook #'html-setup)

(setf nxml-slash-auto-complete-flag t)

;;;###autoload
(defun nxml-reindent-enclosing-tag ()
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
      (destructuring-bind ((start1 . _) _ . end2)
          context
        (reindent-region start1 end2)))
    (goto-char p)
    (move-marker p nil)))

(defun nxml-format-buffer ()
  (interactive)
  (indent-whole-buffer))

(puthash 'nxml-mode
         #'nxml-format-buffer
         *mode-indent-functions-table*)

;;;###autoload
(defun nxml-setup ()
  (markup-setup #'hl-tags-context-nxml-mode)
  (rng-set-vacuous-schema))

;;;###autoload
(add-hook 'nxml-mode-hook #'nxml-setup)

;;;###autoload
(defun web-mode-setup ()
  (init-common :use-whitespace 'tabs-only)
  (setup-folding t nil)

  (put 'hs-set-up-overlay 'permanent-local t)

  ;; (markup-setup)
  (setf *markup-tags-context-func* #'hl-tags-context-nxml-mode)
  (setf web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-quoting t
        web-mode-auto-close-style 2
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:motion-mode-local-keymap
                     vim:operator-pending-mode-local-keymap)
    ("m" vim:motion-jump-tag)

    ("'" vim:nxml-backward-up-element)
    ("q" vim:markup-forward-up-element)))

;;;###autoload
(add-hook 'web-mode-hook #'web-mode-setup)


(provide 'html-setup)

;; Local Variables:
;; End:

;; html-setup.el ends here
