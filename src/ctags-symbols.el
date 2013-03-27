;; ctags-symbols.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 29 December 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (setf lexical-binding t))

(require 'eproj)
(require 'select-mode)
(require 'ctags-mode)



(defvar ctags-symbols-homes-zipper (list nil nil)
  "Two stacks of locations (previous next) from which
`ctags-symbols-go-to-symbol-home' was invoked.")

(defun ctags-symbols/previous-homes (zipper)
  (car zipper))
(defsetf ctags-symbols/previous-homes (zipper) (value)
  `(setf (car ,zipper) ,value))

(defun ctags-symbols/next-homes (zipper)
  (cadr zipper))
(defsetf ctags-symbols/next-homes (zipper) (value)
  `(setf (cadr ,zipper) ,value))

(defun ctags-symbols/next-home (zipper)
  (car-safe (ctags-symbols/next-homes zipper)))



(defun ctags-symbols-identifier-at-point (&optional noerror)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (cond ((not (null? bounds))
           (buffer-substring-no-properties (car bounds) (cdr bounds)))
          ((null? noerror)
           (error "No identifier at point found"))
          (else
           nil))))

(defvar *ctags-symbols-name-delimiter-alist*
  '((c-mode ".")
    (c++-mode "::")
    (python-mode ".")
    (java-mode ".")))


(defstruct ctags-home-entry
  buffer
  point
  symbol)

(defun ctags-switch-to-home-entry (home-entry)
  (switch-to-buffer (ctags-home-entry-buffer home-entry))
  (goto-char (ctags-home-entry-point home-entry)))

(defun ctags-symbols-go-to-symbol-home ()
  (interactive)
  (let* ((sym (ctags-symbols-identifier-at-point nil))
         (orig-major-mode major-mode)
         (proj (eproj-get-project-for-buf (current-buffer)))
         (current-home-entry (make-ctags-home-entry :buffer (current-buffer)
                                                    :point (point)
                                                    :symbol sym))
         (jump-to-home
           (lambda (entry)
             (let ((file (ctags-tag-file entry)))
               (push current-home-entry
                     (ctags-symbols/previous-homes ctags-symbols-homes-zipper))
               (setf (ctags-symbols/next-homes ctags-symbols-homes-zipper) nil)
               (unless (file-exists? file)
                 (error "file %s does not exist" file))
               (find-file file)
               (goto-line (ctags-tag-line entry))
               (save-match-data
                (when (re-search-forward (regexp-quote (ctags-tag-symbol entry))
                                         (line-end-position)
                                         t)
                  (goto-char (match-beginning 0)))))))
         (next-home-entry (ctags-symbols/next-home ctags-symbols-homes-zipper)))
    (unless (or (eproj-project-names proj)
                (assq major-mode (eproj-project-names proj)))
      (eproj-load-ctags-project proj)
      (unless (eproj-project-names proj)
        (error "Project %s loaded no names\nProject: %s"
               (eproj-project-root proj)
               proj))
      (unless (assq major-mode (eproj-project-names proj))
        (error "No names in project %s for language %s"
               (eproj-project-root proj)
               major-mode)))
    (if (and next-home-entry
             (string=? sym
                       (ctags-home-entry-symbol next-home-entry)))
      (begin
        (ctags-switch-to-home-entry next-home-entry)
        (pop (ctags-symbols/next-homes ctags-symbols-homes-zipper))
        (push current-home-entry
              (ctags-symbols/previous-homes ctags-symbols-homes-zipper)))
      (let* ((entry->string
               (lambda (entry)
                 (let ((delim (cadr (assq orig-major-mode
                                          *ctags-symbols-name-delimiter-alist*))))
                   (format "%s %s%s%s\n%s:%s\n%s:%s\n"
                           (ctags-tag-kind entry)
                           (aif (find-if (lambda (entry)
                                           (memq (car entry)
                                                 '(class
                                                   struct
                                                   union
                                                   enum)))
                                         (ctags-tag-aux-fields entry))
                             (concat (cdr it) delim)
                             "")
                           (ctags-tag-symbol entry)
                           (aif (assoc 'signature (ctags-tag-aux-fields entry))
                             (cdr it)
                             "")
                           (file-name-nondirectory (ctags-tag-file entry))
                           (ctags-tag-line entry)
                           (ctags-tag-file entry)
                           (ctags-tag-line entry)))))
             (entries
               (sort (reduce #'append
                             (map (lambda (root)
                                    (aif (assq major-mode
                                               (eproj-project-names
                                                (eproj-get-project root)))
                                      (gethash sym (cdr it) nil)
                                      nil))
                                  (cons (eproj-project-root proj)
                                        (eproj-get-all-related-projects
                                         (eproj-project-root proj)))))
                     (lambda (a b)
                       (string< (funcall entry->string a)
                                (funcall entry->string b))))))
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
                  (select-exit)
                  (funcall jump-to-home (elt entries idx)))
                :predisplay-function
                entry->string
                :preamble-function
                (lambda ()
                  "Choose symbol\n\n"))))))))

(defun ctags-symbols-go-back ()
  (interactive)
  (if (null? (ctags-symbols/previous-homes ctags-symbols-homes-zipper))
    (error "no more previous go-to-definition entries")
    (let ((sym (ctags-symbols-identifier-at-point t)))
      (when sym
        (push (make-ctags-home-entry :buffer (current-buffer)
                                     :point (point)
                                     :symbol sym)
              (ctags-symbols/next-homes ctags-symbols-homes-zipper)))
      (ctags-switch-to-home-entry
       (pop (ctags-symbols/previous-homes ctags-symbols-homes-zipper))))))

(defun setup-ctags-symbols ()
  (aif (current-local-map)
    (def-keys-for-map it
      ("M-." ctags-symbols-go-to-symbol-home)
      ("M-," ctags-symbols-go-back)))
  (def-keys-for-map vim:normal-mode-local-keymap
    ("M-." ctags-symbols-go-to-symbol-home)
    ("M-," ctags-symbols-go-back)))



(provide 'ctags-symbols)

;; Local Variables:
;; End:

;; ctags-symbols.el ends here
