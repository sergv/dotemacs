;; common-heavy.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom)
;; (require 'common)
(require 'custom-predicates)
(require 'more-scheme)
(require 'macro-util)


(defun find-filename-in-tree-recursive (&optional case-sensetive)
  "Read filename regexp and try to find it in current dir's tree or in trees
obtained by following upward in filesystem. Do case-sensitive name matches
if CASE-SENSETIVE is t."
  (interactive (list current-prefix-arg))
  (let* ((filename-re (read-string-no-default "filename regexp: " ""))
         (path (reverse (split-string (aif (buffer-file-name (current-buffer))
                                        (file-name-directory it)
                                        (expand-file-name default-directory))
                                      "/"
                                      t)))
         (subdirs-visited '())
         (found? nil)
         (files nil) ;; found files
         (case-fold-search (not case-sensetive)))
    (letrec ((path-join (lambda (path)
                          (concat "/" (join-lines (reverse path) "/")))))
      (while (and (not found?)
                  (not (null? path)))
        (let ((subdir (funcall path-join path)))
          (message "searching in %s" subdir)
          (setf files
                (find-rec subdir
                          :filep
                          (lambda (p)
                            (string-match-pure? filename-re
                                                (file-name-nondirectory p)))
                          :do-not-visitp
                          (lambda (p)
                            (or (version-control-directory?
                                 (file-name-nondirectory p))
                                (any? (lambda (subdir)
                                        (string-prefix? subdir p))
                                      subdirs-visited)))))
          (when (not (null? files))
            (setf found? t))
          (push subdir subdirs-visited)
          (setf path (cdr path))))
      (if found?
        (progn
          (assert (not (null? files)))
          (if (= 1 (length files))
            (find-file (car files))
            (select-start-selection files
                                    :on-selection
                                    (lambda (idx)
                                      (select-exit)
                                      (find-file (nth idx files)))
                                    :predisplay-function
                                    (lambda (x)
                                      (concat "file: " (file-name-nondirectory x) "\n"
                                              x "\n"))
                                    :preamble-function
                                    (lambda ()
                                      "Multiple files found\n\n")
                                    :separator-function
                                    (lambda () ""))))
        (error "No file found for \"%s\" regexp" filename-re)))))

(defun string-trim-whitespace (str)
  "Trim whitespaces from string."
  (let ((find-bound
         (lambda (from-end)
           (position-if-not (lambda (char) (member* char (string->list " \t\n\r\v\f")
                                                    :test #'char=))
                            str :from-end from-end))))
    (let ((start (funcall find-bound nil))
          (end (funcall find-bound t)))
      (if start
        (subseq str start (1+ end))
        ""))))

;;;;

(defun extract-unicode ()
  ;; note - every single bit of this function is made to let this function
  ;; work as fast as it can an as large buffers as possible
  ;; (on 2k lines performance is acceptable)
  (let ((filter*
         (lambda (pred items)
           (let ((result nil))
             (dolist (item items)
               (if (funcall pred item)
                 (push item result)))
             result))))
    (let ((chars (string->list (buffer-substring-no-properties (point-min)
                                                               (point-max)))))
      (sort (remove-duplicates (funcall filter* (lambda (c) (< 127 c)) chars)) '<))))

(defun input-unicode ()
  (interactive)
  (let* ((symbs (map 'char->string (extract-unicode)))
         (symb (completing-read-vanilla "> " symbs)))
    (remove-text-properties 0 (length symb) '(font-lock-face nil) symb)
    (insert symb)))

;;;;

(defun merge-emacs-configs (new-config-dir curr-config-dir)
  "Merge changes from NEW-CONFIG-DIR into CURR-CONFIG-DIR by successively calling
ediff on files that differ and saving files in CURR-CONFIG-DIR that were updated
while executing ediff.

Use like this to pick changes that will go into CURR-CONFIG-DIR:
\(merge-emacs-configs \"/home/sergey/emacs.new\" \"/home/sergey/emacs\"\)."
  (setf new-config-dir (strip-trailing-slash new-config-dir)
        curr-config-dir (strip-trailing-slash curr-config-dir))
  (let ((ignored-files-re (concat "^.*"
                                  (regexp-opt *ignored-file-name-endings*)
                                  "$"))
        (ignored-dirs-re (concat "\\(?:^\\|/\\)"
                                 (regexp-opt *ignored-directories*)
                                 "/.*$")))
    (dolist (p (map (lambda (p)
                      (file-relative-name p new-config-dir))
                    (find-rec new-config-dir
                              :filep
                              (lambda (p)
                                (let ((fname (file-name-nondirectory p)))
                                  (and (or (string-match-pure? "^.*\\.el$"
                                                               fname)
                                           (string-match-pure? "^.*/?scripts/.*$"
                                                               p)
                                           ;; yasnippet snippets
                                           (string-match-pure? "^.*/?snippets/.*$"
                                                               p))
                                       ;; emacs locks?
                                       (not (string-match-pure? "^\\.#.*"
                                                                fname))
                                       ;; various binary files
                                       (not (string-match-pure? ignored-files-re
                                                                fname))
                                       (not (string-match-pure? ignored-dirs-re
                                                                fname))))))))
      (let* ((new  (concat new-config-dir "/" p))
             (curr (concat curr-config-dir "/" p)))
        (message "Files %s and %s" new curr)
        (condition-case err
            (progn
              (assert (file-exist? new))
              (if (file-exist? curr)
                (if (different-files-fast? new curr)
                  (let ((new-buf  (find-file-noselect new))
                        (curr-buf (find-file-noselect curr)))
                    (ediff-diff-files-recursive-edit new curr :read-only nil)
                    (kill-buffer new-buf)
                    (with-current-buffer curr-buf
                      (save-buffer))
                    (redisplay t))
                  (progn
                    (message "Files %s and %s are the same, skipping" new curr)))
                (when (y-or-n? (format "Copy %s to %s?" new curr))
                  (copy-file new curr nil t t t))))
          (error
           (message "Error occurred while processing files %s and %s:\n%s"
                    new
                    curr
                    err)))))))

(defun merge-emacs-configs-default ()
  "Merge from +emacs-config-path+/tmp/emacs into `+emacs-config-path+'."
  (interactive)
  (let ((current-conf-dir +emacs-config-path+)
        (new-conf-dir (concat +emacs-config-path+ "/tmp/emacs")))
    (assert (file-directory? current-conf-dir))
    (if (not (file-directory? new-conf-dir))
      (error "Config under %s not found" new-conf-dir)
      (merge-emacs-configs new-conf-dir current-conf-dir))))

;;;;

(defun remove-tabs (start end)
  "Replace all tab characters in region between START and END with
number of spaces equal to `tab-width'."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char end)
      (save-excursion
        (unless (re-search-backward "\t" start t)
          (error "No tabs found")))
      (let ((str (make-string tab-width ?\s)))
        (while (re-search-backward "\t" start t)
          (replace-match str))))))

;;;

(provide 'common-heavy)

;; Local Variables:
;; End:

;; common-heavy.el ends here
