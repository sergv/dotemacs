;; eproj-symbnav.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(require 'select-mode)
(require 'eproj)
(provide 'eproj-customization)

;;; tag/symbol navigation (navigation over homes)

(defparameter eproj-symbnav/previous-homes nil
  "Previous locations (markers) from which symbol search was invoked.")

(defparameter eproj-symbnav/selected-loc nil
  "Home entry corresponding to the most recently visited tag.")

(defparameter eproj-symbnav/next-homes nil
  "Next locations that were visited but now obscured by going back.")

(defstruct (eproj-home-entry
            (:conc-name eproj-home-entry/))
  buffer
  position ;; marker, not number
  symbol ;; == name - string, or nil if this entry was not selected explicitly
  )

(defun eproj-home-entry=? (entry-a entry-b)
  (and (eq? (eproj-home-entry/buffer entry-a)
            (eproj-home-entry/buffer entry-b))
       (= (eproj-home-entry/position entry-a)
          (eproj-home-entry/position entry-b))
       (eq? (eproj-home-entry/symbol entry-a)
            (eproj-home-entry/symbol entry-b))))

(defvar-local eproj-symbnav/identifier-type 'symbol
  "Type of identifiers to look for when retrieving name at point to
search for in tags. This should be a symbol
as accepted by `bounds-of-thing-at-point'.")

(defun eproj-symbnav/identifier-at-point (&optional noerror)
  (or (awhen (get-region-string-no-properties)
        (trim-whitespace it))
      (let ((bounds (bounds-of-thing-at-point eproj-symbnav/identifier-type)))
        (cond ((not (null bounds))
               (funcall (eproj-language/normalize-identifier-before-navigation-procedure
                         (gethash (eproj/resolve-synonym-modes major-mode)
                                  eproj/languages-table))
                        (buffer-substring-no-properties (car bounds)
                                                        (cdr bounds))))
              ((null noerror)
               (error "No identifier at point found"))
              (t
               nil)))))

(defun eproj-symbnav/show-home (entry)
  (when (not (null entry))
    (with-current-buffer (eproj-home-entry/buffer entry)
      (concat (eproj-home-entry/symbol entry)
              "@"
              (if buffer-file-name
                (file-name-nondirectory buffer-file-name)
                "<no-buffer>")
              ":"
              (save-excursion
                (number->string
                 (line-number-at-pos
                  (marker-position (eproj-home-entry/position entry)))))))))

(defun eproj-symbnav/describe ()
  (interactive)
  (message "Previous homes: %s\nSelected loc: %s\nNext homes: %s\n"
           (-map #'eproj-symbnav/show-home eproj-symbnav/previous-homes)
           (eproj-symbnav/show-home eproj-symbnav/selected-loc)
           (-map #'eproj-symbnav/show-home eproj-symbnav/next-homes)))

(defun eproj-symbnav/reset ()
  (interactive)
  (setf eproj-symbnav/previous-homes nil
        eproj-symbnav/selected-loc nil
        eproj-symbnav/next-homes nil))


(defun eproj-symbnav/switch-to-home-entry (home-entry)
  (unless (buffer-live-p (eproj-home-entry/buffer home-entry))
    (setf (eproj-home-entry/buffer home-entry)
          (find-file-noselect
           (buffer-file-name (eproj-home-entry/buffer home-entry)))))
  (switch-to-buffer (eproj-home-entry/buffer home-entry))
  (goto-char (eproj-home-entry/position home-entry)))

(defun eproj-symbnav/resolve-entry-file-in-project (entry proj)
  (let ((file
         (eproj-resolve-abs-or-rel-name (eproj-tag/file entry)
                                        (eproj-project/root proj))))
    (unless (file-exists-p file)
      (error "file %s does not exist" file))
    file))

(defun eproj-symbnav/locate-entry-in-current-buffer (entry)
  (goto-line (eproj-tag/line entry))
  (save-match-data
    (when (re-search-forward (regexp-quote (eproj-tag/symbol entry))
                             (line-end-position)
                             t)
      (goto-char (match-beginning 0))))
  ;; remove annoying "Mark set" message
  (message ""))

(defun eproj-symbnav/show-entry-in-other-window (entry entry-proj)
  (find-file-other-window
   (eproj-symbnav/resolve-entry-file-in-project entry
                                                entry-proj))
  (eproj-symbnav/locate-entry-in-current-buffer entry))

(defun eproj-symbnav/go-to-symbol-home (&optional use-regexp)
  (interactive "P")
  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (case-fold-search (and (not (null current-prefix-arg))
                                (<= 16 (car current-prefix-arg))))
         (identifier (if use-regexp
                       (read-regexp "enter regexp to search for")
                       (eproj-symbnav/identifier-at-point nil)))
         (effective-major-mode (eproj/resolve-synonym-modes major-mode))
         (orig-file-name (cond
                           (buffer-file-name
                            (expand-file-name buffer-file-name))
                           ((and (boundp 'magit-buffer-file-name)
                                 magit-buffer-file-name)
                            (expand-file-name magit-buffer-file-name))))
         (current-home-entry (make-eproj-home-entry :buffer (current-buffer)
                                                    :position (point-marker)
                                                    :symbol nil))
         (jump-to-home
          (lambda (entry entry-proj)
            (let ((file
                   (eproj-symbnav/resolve-entry-file-in-project entry entry-proj)))
              (push current-home-entry eproj-symbnav/previous-homes)
              (setf eproj-symbnav/next-homes nil)
              (find-file file)
              (eproj-symbnav/locate-entry-in-current-buffer entry)
              (setf eproj-symbnav/selected-loc
                    (make-eproj-home-entry :buffer (current-buffer)
                                           :position (point-marker)
                                           :symbol (eproj-tag/symbol entry))))))
         (next-home-entry (car-safe eproj-symbnav/next-homes)))
    ;; load tags if there're none
    (unless (or (eproj-project/tags proj)
                (assq effective-major-mode (eproj-project/tags proj)))
      (eproj-reload-project! proj)
      (unless (eproj-project/tags proj)
        (error "Project %s loaded no names\nProject: %s"
               (eproj-project/root proj)
               proj))
      (unless (assq effective-major-mode (eproj-project/tags proj))
        (error "No names in project %s for language %s"
               (eproj-project/root proj)
               effective-major-mode)))
    (if (and eproj-symbnav-remember-choices
             next-home-entry
             (when-let (next-symbol (eproj-home-entry/symbol next-home-entry))
               (if use-regexp
                 (string-match-p identifier next-symbol)
                 (string= identifier next-symbol))))
      (progn
        (eproj-symbnav/switch-to-home-entry next-home-entry)
        (push current-home-entry
              eproj-symbnav/previous-homes)
        (setf eproj-symbnav/selected-loc (pop eproj-symbnav/next-homes)))
      (let* ((entry->string
              (eproj-language/tag->string-procedure
               (aif (gethash effective-major-mode eproj/languages-table)
                 it
                 (error "unsupported language %s" effective-major-mode))))
             (expanded-project-root
              (expand-file-name (eproj-project/root proj)))
             (tag->string
              (lambda (tag tag-proj)
                (let ((txt (funcall entry->string tag-proj tag))
                      (expanded-tag-file
                       (expand-file-name
                        (eproj-resolve-abs-or-rel-name
                         (eproj-tag/file tag)
                         (eproj-project/root tag-proj)))))
                  (cond ((string= orig-file-name
                                  expanded-tag-file)
                         (propertize txt 'face 'font-lock-negation-char-face))
                        ((string= (eproj-project/root proj)
                                  (eproj-project/root tag-proj))
                         ;; use italic instead of underscore
                         (propertize txt 'face 'italic))
                        (t
                         txt)))))
             (entry-tag #'first)
             (entry-string #'second)
             (entry-proj #'third)
             (entries
              ;; I'm not entirely sure where duplicates come from, but it's cheap
              ;; to remove them and at the same time I'm reluctant to tweak my
              ;; Emacs because of it's dynamically-typed lisp.
              (list->vector
               (remove-duplicates-from-sorted-list-by
                (lambda (a b)
                  ;; compare results of tag->string
                  (string= (funcall entry-string a) (funcall entry-string b)))
                (sort
                 (-map (lambda (tag-entry)
                         (destructuring-bind (tag . tag-proj)
                             tag-entry
                           (list tag
                                 (funcall tag->string tag tag-proj)
                                 tag-proj)))
                       (eproj-get-matching-tags proj
                                                effective-major-mode
                                                identifier
                                                use-regexp))
                 (lambda (a b)
                   ;; compare results of tag->string
                   (string< (funcall entry-string a) (funcall entry-string b))))))))
        (pcase (length entries)
          (`0
           (error "No entries for %s %s"
                  (if use-regexp "regexp" "identifier")
                  identifier))
          (`1
           (funcall jump-to-home
                    (funcall entry-tag (elt entries 0))
                    (funcall entry-proj (elt entries 0))))
          (_
           (let ((kmap (make-sparse-keymap)))
             (def-keys-for-map kmap
               ("SPC" (lambda () (interactive)
                        (let ((entry (elt entries (select-get-selected-index))))
                          (eproj-symbnav/show-entry-in-other-window
                           (funcall entry-tag entry)
                           (funcall entry-proj entry))))))
             (select-start-selection
              entries
              :buffer-name "Symbol homes"
              :after-init (lambda ()
                            (select-extend-keymap kmap))
              :on-selection
              (lambda (idx selection-type)
                (select-exit)
                (let ((entry (elt entries idx)))
                  (funcall jump-to-home
                           (funcall entry-tag entry)
                           (funcall entry-proj entry))))
              :item-show-function
              entry-string
              :preamble-function
              (lambda () "Choose symbol\n\n")))))))))

(defun eproj-symbnav/go-back ()
  (interactive)
  (if (null eproj-symbnav/previous-homes)
    (error "no more previous go-to-definition entries")
    (progn
      (when (or (null eproj-symbnav/next-homes)
                (and (not (null eproj-symbnav/next-homes))
                     (not (eproj-home-entry=? eproj-symbnav/selected-loc
                                              (car eproj-symbnav/next-homes)))))
        (push eproj-symbnav/selected-loc eproj-symbnav/next-homes))
      (let ((prev-home (pop eproj-symbnav/previous-homes)))
        (setf eproj-symbnav/selected-loc prev-home)
        (eproj-symbnav/switch-to-home-entry prev-home)))))

(defun setup-eproj-symbnav ()
  (awhen (current-local-map)
    (def-keys-for-map it
      ("C-." eproj-symbnav/go-to-symbol-home)
      ("C-," eproj-symbnav/go-back)))
  (def-keys-for-map vim:normal-mode-local-keymap
    ("C-." eproj-symbnav/go-to-symbol-home)
    ("C-," eproj-symbnav/go-back)
    ("g ." eproj-symbnav/go-to-symbol-home)
    ("g ," eproj-symbnav/go-back)))

(provide 'eproj-symbnav)

;; Local Variables:
;; End:

;; eproj-symbnav.el ends here
