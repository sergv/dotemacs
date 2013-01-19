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
  (let ((sym (ctags-symbols-identifier-at-point))
        (orig-major-mode major-mode)
        (proj (eproj-get-project-for-buf (current-buffer))))
    (unless (or (eproj-project-names proj)
                (assq major-mode (eproj-project-names proj)))
      (eproj-load-ctags-project (current-buffer))
      (unless (eproj-project-names proj)
        (error "Project %s is not loaded" (eproj-project-root proj)))
      (unless (assq major-mode (eproj-project-names proj))
        (error "No names in project %s for language %s"
               (eproj-project-root proj)
               major-mode)))
    (let* ((entries (reduce #'append
                            (mapcar (lambda (root)
                                      (aif (assq major-mode
                                                 (eproj-project-names
                                                  (eproj-get-project root)))
                                        (gethash sym (cdr it) nil)
                                        nil))
                                    (cons (eproj-project-root proj)
                                          (eproj-get-all-related-projects
                                           (eproj-project-root proj))))))
           (stack-entry (list (current-buffer) (point)))
           (jump-to-home
             (lambda (entry)
               (let ((file (ctags-tag-file entry)))
                   (push stack-entry ctags-symbols-go-to-symbol-home-stack)
                 (unless (file-exists? file)
                   (error "file %s does not exist" file))
                 (find-file file)
                 (goto-line (ctags-tag-line entry))))))
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
              (lambda (entry)
                (format "%s %s%s%s\n%s:%s\n%s:%s\n"
                        (ctags-tag-kind entry)
                        (aif (find-if (lambda (entry)
                                        (memq (car entry)
                                              '(class
                                                struct
                                                union
                                                enum)))
                                      (ctags-tag-aux-fields entry))
                          (concat (cdr it)
                                  (cadr (assq orig-major-mode
                                              *ctags-symbols-name-delimiter-alist*)))
                          "")
                        (ctags-tag-symbol entry)
                        (aif (assoc 'signature (ctags-tag-aux-fields entry))
                          (cdr it)
                          "")
                        (file-name-nondirectory (aref *ctags-file-sequence*
                                                      (ctags-tag-file-idx entry)))
                        (ctags-tag-line entry)
                        (ctags-tag-file entry)
                        (ctags-tag-line entry)))
              :preamble-function
              (lambda ()
                "Choose symbol\n\n")))))))

(defun ctags-symbols-go-back ()
  (interactive)
  (if ctags-symbols-go-to-symbol-home-stack
    (let ((top (pop ctags-symbols-go-to-symbol-home-stack)))
      (switch-to-buffer (car top))
      (goto-char (cadr top)))
    (error "no more previous go-to-definition entries")))

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
