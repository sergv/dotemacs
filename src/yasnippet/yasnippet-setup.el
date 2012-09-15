;; yasnippet-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 March 2012
;; Description:

(require 'common)
(require 'more-scheme)

;; Yasnippet
(setf yas/ignore-filenames-as-triggers t)
(require 'yasnippet)
(setf yas/root-directory (concat +prog-data-path+ "/snippets")
      yas/prompt-functions '(yas/dropdown-prompt yas/completing-prompt)
      yas/skip-and-clear-key "DEL"
      yas/key-syntaxes (list "^ >" "w_." "w_" "w"))
(yas/initialize)

;; so yasnippet is in loaded state here

(defun yas/parse-templates (&optional file)
  "Parse the templates in the current buffer. For every mention of
key variable a snippet definition would be returned.

Returns list of snippet definitions, see `yas/parse-template'.

This is additional helper function, similar to `yas/parse-template' but
returns list of snippet definitions instead of just one thus greatly
simlifying encoding of several keys for one snippet."
  (goto-char (point-min))
  (let* ((name (and file
                    (file-name-nondirectory file)))
         (keys (unless yas/ignore-filenames-as-triggers
                 (and name
                      (list (file-name-sans-extension name)))))
         template
         bound
         condition
         (group (and file
                     (yas/calculate-group file)))
         expand-env
         binding)
    (if (re-search-forward "^# --\n" nil t)
      (progn
        (setq template
              (buffer-substring-no-properties (point)
                                              (point-max)))
        (setq bound (point))
        (goto-char (point-min))
        (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
          (let ((variable (match-string-no-properties 1))
                (value    (match-string-no-properties 2)))
            (cond
              ((string=? "name" variable)
               (setq name value))
              ((string=? "condition" variable)
               (setq condition value))
              ((string=? "group" variable)
               (setq group value))
              ((string=? "expand-env" variable)
               (setq expand-env value))
              ((string=? "key" variable)
               (push value keys))
              ((string=? "binding" variable)
               (setq binding value))))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (mapcar (lambda (key)
              (list key template name condition group expand-env file binding))
            keys)))

;; this causes yasnippet to consider only *.snip files
(redefun yas/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (remove-if (lambda (file)
               (let ((filename (file-name-nondirectory file)))
                 (or (string-match-pure? "^\\." filename)
                     (string-match-pure? "~$" filename)
                     (if file?
                       (or (file-directory-p file)
                           ;; modified here
                           (not (string-match-pure? "\\.snip$" filename)))
                       (not (file-directory-p file))))))
             (directory-files directory t)))

;; this makes use of yas/parse-templates
(redefun yas/load-directory-1 (directory
                               &optional
                               parents
                               no-hierarchy-parents
                               making-groups-sym)
  "Recursively load snippet templates from DIRECTORY."
  ;; TODO: Rewrite this horrible, horrible monster I created
  (unless (file-exist? (concat directory "/" ".yas-skip"))
    (let* ((major-mode-and-parents (unless making-groups-sym
                                     (yas/compute-major-mode-and-parents
                                      (concat directory "/dummy")
                                      nil
                                      no-hierarchy-parents)))
           (yas/ignore-filenames-as-triggers
             (or yas/ignore-filenames-as-triggers
                 (file-exist? (concat directory
                                      "/.yas-ignore-filenames-as-triggers"))))
           (mode-sym (and major-mode-and-parents
                          (car major-mode-and-parents)))
           (parents (if making-groups-sym
                      parents
                      (rest major-mode-and-parents)))
           (snippet-defs nil)
           (make-groups-p
             (or making-groups-sym
                 (file-exist? (concat directory "/" ".yas-make-groups")))))
      (with-temp-buffer
        (dolist (file (yas/subdirs directory 'no-subdirs-just-files))
          (when (file-readable-p file)
            (insert-file-contents file nil nil nil t)
            (setf snippet-defs
                  ;; modified here
                  (append (yas/parse-templates file)
                          snippet-defs)))))
      (yas/define-snippets (or mode-sym
                               making-groups-sym)
                           snippet-defs
                           parents)
      (dolist (subdir (yas/subdirs directory))
        (if make-groups-p
          (yas/load-directory-1 subdir parents 't (or mode-sym
                                                      making-groups-sym))
          (yas/load-directory-1 subdir (list mode-sym)))))))


(defun yas/skip-and-clear-or-delete-backward-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-backward-char'."
  (interactive)
  (let ((field (or field
                   (and yas/active-field-overlay
                        (overlay-buffer yas/active-field-overlay)
                        (overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
                (not (yas/field-modified-p field))
                (eq (point) (marker-position (yas/field-start field))))
           (yas/skip-and-clear field)
           (yas/next-field 1))
          (t
           (call-interactively 'delete-backward-char)))))

(def-keys-for-map yas/keymap
  ("<backspace>"     yas/skip-and-clear-or-delete-backward-char)
  ("<delete>"        yas/skip-and-clear-or-delete-char)
  ("S-<backspace>"   yas/skip-and-clear-or-delete-char)

  ("<S-iso-lefttab>" yas/prev-field)
  ("S-<tab>"         yas/prev-field))

;; now load snippets using enhanced functions (re)defined above
(yas/load-directory yas/root-directory)


(provide 'yasnippet-setup)

;; Local Variables:
;; End:

;; yasnippet-setup.el ends here
