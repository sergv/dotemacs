;; ctags-symbols.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 29 December 2012
;; Description:

(eval-when-compile
  (require 'cl))

(require 'select-mode)
(require 'ctags-mode)



(defvar ctags-symbols-go-to-symbol-home-stack nil
  "Stack of locations from which `ctags-symbols-go-to-symbol-home' was invoked.")

(defun ctags-symbols-identifier-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds))
      (error "No identifier at point found"))))

(defvar *ctags-symbols-name-delimiter-alist*
  '((c-mode ".")
    (c++-mode "::")
    (python-mode ".")))


(defun ctags-symbols-go-to-symbol-home ()
  (interactive)
  (let* ((sym (ctags-symbols-identifier-at-point))
         (proj-root (proj-get-project-root))
         (symbols (proj-get-project-ctags-symbols
                   (lambda ()
                     (proj-load-ctags-project (proj-get-project-root)
                                              major-mode))))
         (entries (gethash sym symbols nil))
         (stack-entry (list (current-buffer) (point)))
         (jump-to-home
           (lambda (entry)
             (aif (ctags-tag-file entry)
               (begin
                 (push stack-entry ctags-symbols-go-to-symbol-home-stack)
                 (find-file (concat proj-root "/" it))
                 (goto-line (ctags-tag-line entry)))
               (error "Identifier %s is not defined in file" sym)))))
    (cond ((null? entries)
           (error "No entries for identifier %s" sym))
          ((null? (cdr entries))
           (funcall jump-to-home (car entries)))
          (else
           (select-start-selection
            entries
            :buffer-name "Symbol homes"
            :after-init #'ignore
            :on-selection
            (lambda (idx)
              (funcall jump-to-home (elt entries idx)))
            :predisplay-function
            (lambda (entry)
              (format "%s\n%s:%s\n%s%s%s\n"
                      (ctags-tag-kind entry)
                      (ctags-tag-file entry)
                      (ctags-tag-line entry)
                      (aif (or (assoc 'class (ctags-tag-aux-fields entry))
                               (assoc 'struct (ctags-tag-aux-fields entry))
                               (assoc 'union (ctags-tag-aux-fields entry))
                               (assoc 'enum (ctags-tag-aux-fields entry)))
                        (concat (cdr it)
                                (cdr (assq major-mode
                                           *ctags-symbols-name-delimiter-alist*)))
                        "")
                      (ctags-tag-symbol entry)
                      (aif (assoc 'signature (ctags-tag-aux-fields entry))
                        (cdr it)
                        "")))
            :preamble-function
            (lambda ()
              "Choose symbol\n\n"))))))

(defun ctags-symbols-go-back ()
  (interactive)
  (if ctags-symbols-go-to-symbol-home-stack
    (let ((top (pop ctags-symbols-go-to-symbol-home-stack)))
      (switch-to-buffer (car top))
      (goto-char (cadr top)))
    (error "no more previous go-to-definition entries")))

(defun setup-ctags-symbols ()
  (def-keys-for-map ((current-local-map)
                     vim:normal-mode-local-keymap)
    ("M-." ctags-symbols-go-to-symbol-home)
    ("M-," ctags-symbols-go-back)))



(provide 'ctags-symbols)

;; Local Variables:
;; End:

;; ctags-symbols.el ends here
