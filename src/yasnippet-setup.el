;; yasnippet-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 March 2012
;; Description:

(require 'common)
(require 'more-scheme)

(setf yas-ignore-filenames-as-triggers t
      yas-snippet-dirs (concat +prog-data-path+ "/snippets")
      yas-prompt-functions '(yas-ido-prompt)
      yas-skip-and-clear-key "DEL"
      yas-key-syntaxes (list "^ >" "w_." "w_" "w")
      ;; don't reactivate fields on undo/redo
      yas-snippet-revival nil
      ;; Make `yas-expand' return nil if it fails to expand a snippet.
      yas-fallback-behavior nil)

(defun yas--parse-templates (&optional file)
  "Parse the templates in the current buffer. For every mention of
key variable a snippet definition would be returned.

Returns list of snippet definitions, see `yas-parse-template'.

This is additional helper function, similar to `yas-parse-template' but
returns list of snippet definitions instead of just one thus greatly
simlifying encoding of several keys for one snippet."
  (goto-char (point-min))
  (let* ((type 'snippet)
         (name (and file
                    (file-name-nondirectory file)))
         (keys (unless yas-ignore-filenames-as-triggers
                 (and name
                      (list (file-name-sans-extension name)))))
         template
         bound
         condition
         (group (and file
                     (yas--calculate-group file)))
         expand-env
         binding
         uuid)
    (if (re-search-forward "^# --\n" nil t)
      (progn (setq template
                   (buffer-substring-no-properties (point)
                                                   (point-max)))
             (setq bound (point))
             (goto-char (point-min))
             (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
               (let ((variable (match-string-no-properties 1))
                     (value (match-string-no-properties 2)))
                 (cond
                   ((string= "uuid" variable)
                    (setq uuid value))
                   ((string= "type" variable)
                    (setq type (if (string= "command" value)
                                 'command
                                 'snippet)))
                   ((string= "key" variable)
                    (push value keys))
                   ((string= "name" variable)
                    (setq name value))
                   ((string= "condition" variable)
                    (setq condition (yas--read-lisp value)))
                   ((string= "group" variable)
                    (setq group value))
                   ((string= "expand-env" variable)
                    (setq expand-env (yas--read-lisp value
                                                     'nil-on-error)))
                   ((string= "binding" variable)
                    (setq binding value))))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (unless (or keys binding)
      (setq keys (list (and file (file-name-nondirectory file)))))
    (when (eq type 'command)
      (setq template (yas--read-lisp (concat "(progn" template ")"))))
    (when group
      (setq group (split-string group "\\.")))
    (map (lambda (key)
           (list key template name condition group expand-env file binding uuid))
         keys)))

;; this causes yasnippet to consider only *.snip files
(redefun yas--subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (delete-if (lambda (file)
               (let ((filename (file-name-nondirectory file)))
                 (or (string-match-pure? "^\\." filename)
                     (string-match-pure? "^#.*#$" filename)
                     (string-match-pure? "~$" filename)
                     (if file?
                       (or (file-directory-p file)
                           ;; modified here
                           (not (string-match-pure? "\\.snip$" filename)))
                       (not (file-directory-p file))))))
             (directory-files directory t)))

;; use yas--parse-templates instead of yas--parse-template
(redefun yas--load-directory-2 (directory mode-sym)
  ;; Load .yas-setup.el files wherever we find them
  ;;
  (yas--load-yas-setup-file (expand-file-name ".yas-setup" directory))
  (let* ((default-directory directory)
         (snippet-defs nil))
    ;; load the snippet files
    ;;
    (with-temp-buffer
      (dolist (file (yas--subdirs directory 'no-subdirs-just-files))
        (when (file-readable-p file)
          (insert-file-contents file nil nil nil t)
          (dolist (template (yas--parse-templates file))
            (push template snippet-defs)))))
    (when snippet-defs
      (yas-define-snippets mode-sym
                           snippet-defs))
    ;; now recurse to a lower level
    ;;
    (dolist (subdir (yas--subdirs directory))
      (yas--load-directory-2 subdir
                             mode-sym))))

(defun yas-skip-and-clear-or-delete-backward-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-backward-char'."
  (interactive)
  (let ((field (or field
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          (t
           (call-interactively 'delete-backward-char)))))

(def-keys-for-map yas-keymap
  ("<backspace>"     yas-skip-and-clear-or-delete-backward-char)
  ("<delete>"        yas-skip-and-clear-or-delete-char)
  ("S-<backspace>"   yas-skip-and-clear-or-delete-char)

  ("<S-iso-lefttab>" yas-prev-field)
  ("S-<tab>"         yas-prev-field))

;; (yas-compile-directory yas-snippet-dirs)
;; now load snippets using enhanced functions (re)defined above
(yas-load-directory yas-snippet-dirs)

(eval-after-load
    "org"
  '(progn
     (defadvice org-fix-tags-on-the-fly (around
                                         org-fix-tags-on-the-fly-yasnippet-field-fix
                                         activate
                                         compile
                                         preactivate
                                         protect)
       "Solution to problem of `org-fix-tags-on-the-fly' being called after
every org-self-insert-command when yasnippet's field happens to be located
in org's headline."
       (yas--inhibit-overlay-hooks
         ad-do-it))))

(defvar-local yas-expand-fallback
  (lambda () (error "yas-expand-fallback not set to proper callback")))

(defun yas-expand-or-fallback ()
  (interactive)
  (or (yas-expand)
      (funcall yas-expand-fallback)))

(provide 'yasnippet-setup)

;; Local Variables:
;; End:

;; yasnippet-setup.el ends here
