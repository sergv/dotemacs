;; eproj-symbnav.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(eval-when-compile (require 'subr-x))

(require 'select-mode)
(require 'eproj)
(require 'eproj-customization)

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
  (if (region-active-p)
      (trim-whitespace (get-region-string-no-properties))
    (let ((bounds (bounds-of-thing-at-point eproj-symbnav/identifier-type)))
      (cond ((not (null bounds))
             (if-let ((lang (gethash (eproj/resolve-synonym-modes major-mode)
                                     eproj/languages-table)))
                 (let ((str (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))))
                   (aif (eproj-language/normalise-identifier-before-navigation-procedure lang)
                       (funcall it str)
                     str))
               (error "Did not find a language for current mode: %s" major-mode)))
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

;;;###autoload
(defun eproj-symbnav/describe ()
  (interactive)
  (notify "Previous homes: %s\nSelected loc: %s\nNext homes: %s\n"
          (-map #'eproj-symbnav/show-home eproj-symbnav/previous-homes)
          (eproj-symbnav/show-home eproj-symbnav/selected-loc)
          (-map #'eproj-symbnav/show-home eproj-symbnav/next-homes)))

;;;###autoload
(defun eproj-symbnav/reset ()
  (interactive)
  (setf eproj-symbnav/previous-homes nil
        eproj-symbnav/selected-loc nil
        eproj-symbnav/next-homes nil))

(defun eproj-symbnav/on-switch ()
  "Notify user that switch has taken place."
  (recenter)
  (xref-pulse-momentarily))

(defun eproj-symbnav/on-back ()
  "Notify user that return to previous location has taken place."
  (xref-pulse-momentarily))

(defun eproj-symbnav/switch-to-home-entry (home-entry)
  (unless (buffer-live-p (eproj-home-entry/buffer home-entry))
    (setf (eproj-home-entry/buffer home-entry)
          (find-file-noselect
           (buffer-file-name (eproj-home-entry/buffer home-entry)))))
  (switch-to-buffer (eproj-home-entry/buffer home-entry))
  (goto-char (eproj-home-entry/position home-entry)))

(defun eproj-symbnav/resolve-tag-file-in-project (tag proj)
  (let ((file
         (eproj--resolve-to-abs-path (eproj-tag/file tag)
                                     (eproj-project/root proj))))
    (unless (file-exists-p file)
      (error "file %s does not exist" file))
    file))

(defun eproj-symbnav/locate-tag-in-current-buffer (tag-name tag)
  (goto-line-dumb (eproj-tag/line tag))
  (save-match-data
    (when (re-search-forward (regexp-quote tag-name)
                             (line-end-position)
                             t)
      (goto-char (match-beginning 0))))
  ;; remove annoying "Mark set" message
  (notify ""))

(defun eproj-symbnav/show-tag-in-other-window (tag-name tag entry-proj)
  (find-file-other-window
   (eproj-symbnav/resolve-tag-file-in-project tag
                                              entry-proj))
  (eproj-symbnav/locate-tag-in-current-buffer tag-name tag))

;;;###autoload
(defun eproj-symbnav/go-to-symbol-home (&optional use-regexp?)
  (interactive "P")
  (let ((identifier (if use-regexp?
                        (read-regexp "enter regexp to search for")
                      (eproj-symbnav/identifier-at-point nil))))
    (eproj-symbnav/go-to-symbol-home-impl identifier use-regexp?)))

(defun eproj-symbnav-current-home-entry ()
  (make-eproj-home-entry :buffer (current-buffer)
                         :position (point-marker)
                         :symbol nil))

(defun eproj-symbnav/go-to-symbol-home-impl (identifier use-regexp?)
  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (case-fold-search (and (not (null current-prefix-arg))
                                (<= 16 (car current-prefix-arg))))
         (effective-major-mode (eproj/resolve-synonym-modes major-mode))
         (orig-file-name (cond
                           (buffer-file-name
                            (expand-file-name buffer-file-name))
                           ((and (boundp 'magit-buffer-file-name)
                                 magit-buffer-file-name)
                            (expand-file-name magit-buffer-file-name))))
         (current-home-entry (eproj-symbnav-current-home-entry))
         (next-home-entry (car-safe eproj-symbnav/next-homes)))
    ;; Load tags if there're none.
    (unless (or (eproj--get-tags proj)
                (assq effective-major-mode (eproj--get-tags proj)))
      (eproj-reload-project! proj)
      (unless (eproj--get-tags proj)
        (error "Project %s loaded no names\nProject: %s"
               (eproj-project/root proj)
               proj))
      (unless (assq effective-major-mode (eproj--get-tags proj))
        (error "No names in project %s for language %s"
               (eproj-project/root proj)
               effective-major-mode)))
    (if (and eproj-symbnav-remember-choices
             next-home-entry
             (when-let (next-symbol (eproj-home-entry/symbol next-home-entry))
               (if use-regexp?
                   (string-match-p identifier next-symbol)
                 (string= identifier next-symbol))))
        (progn
          (eproj-symbnav/switch-to-home-entry next-home-entry)
          (eproj-symbnav/on-switch)
          (push (eproj-symbnav--current-home-entry)
                eproj-symbnav/previous-homes)
          (setf eproj-symbnav/selected-loc (pop eproj-symbnav/next-homes)))

      (eproj-symbnav/choose-symbol-home-to-jump-to
       identifier
       effective-major-mode
       orig-file-name
       proj
       (eproj-symbnav-current-home-entry)
       (eproj-get-matching-tags proj
                                effective-major-mode
                                identifier
                                use-regexp?)))))

(defun eproj-symbnav/choose-symbol-home-to-jump-to (identifier current-major-mode current-buffer-file-name current-proj current-home-entry tag-entries)
  (let* ((lang (aif (gethash current-major-mode eproj/languages-table)
                   it
                 (error "unsupported language %s" current-major-mode)))
         (entry->string-func (eproj-language/tag->string-func lang))
         (show-tag-kind-procedure (eproj-language/show-tag-kind-procedure lang))
         (tag->sort-token
          (lambda (tag-name tag)
            (list
             tag-name
             (funcall show-tag-kind-procedure tag)
             (eproj-tag/file tag)
             (eproj-tag/line tag))))
         (sort-tokens<
          (lambda (x y)
            (let ((symbol-x (first x))
                  (symbol-y (first y))
                  (tag-kind-x (second x))
                  (tag-kind-y (second y))
                  (file-x (third x))
                  (file-y (third y))
                  (line-x (fourth x))
                  (line-y (fourth y)))
              (cl-assert (stringp symbol-x))
              (cl-assert (stringp symbol-y))
              (cl-assert (stringp tag-kind-x))
              (cl-assert (stringp tag-kind-y))
              (cl-assert (stringp file-x))
              (cl-assert (stringp file-y))
              (cl-assert (numberp line-x))
              (cl-assert (numberp line-y))
              (or (string< symbol-x symbol-y)
                  (and (string= symbol-x symbol-y)
                       (or (string< tag-kind-x tag-kind-y)
                           (and (string= tag-kind-x tag-kind-y)
                                (or (string< file-x file-y)
                                    (and (string= file-x file-y)
                                         (< line-x line-y))))))))))
         (tag->string
          (lambda (tag-proj tag-name tag)
            (let ((txt (funcall entry->string-func tag-proj tag-name tag))
                  (expanded-tag-file
                   (expand-file-name
                    (eproj--resolve-to-abs-path
                     (eproj-tag/file tag)
                     (eproj-project/root tag-proj)))))
              (cond ((string= current-buffer-file-name
                              expanded-tag-file)
                     (propertize txt 'face 'font-lock-negation-char-face))
                    ((string= (eproj-project/root current-proj)
                              (eproj-project/root tag-proj))
                     ;; use italic instead of underscore
                     (propertize txt 'face 'italic))
                    (t
                     txt)))))
         (entry-sort-token #'first)
         (entry-tag-name #'second)
         (entry-tag #'third)
         (entry-string #'fourth)
         (entry-proj #'fifth)
         (entries
          ;; I'm not entirely sure where duplicates come from, but it's cheap
          ;; to remove them and at the same time I'm reluctant to tweak my
          ;; Emacs because of it's dynamically-typed lisp.
          (list->vector
           (sort
            (remove-duplicates-by-hashing-projections
             entry-sort-token
             #'equal
             (-map (lambda (tag-entry)
                     (destructuring-bind (tag-name tag tag-proj)
                         tag-entry
                       (list (funcall tag->sort-token tag-name tag)
                             tag-name
                             tag
                             (funcall tag->string tag-proj tag-name tag)
                             tag-proj)))
                   tag-entries))
            (lambda (a b)
              ;; compare results of tag->sort-token
              (funcall sort-tokens<
                       (funcall entry-sort-token a)
                       (funcall entry-sort-token b))))))

         (jump-to-home
          (lambda (tag-name tag entry-proj)
            (let ((file
                   (eproj-symbnav/resolve-tag-file-in-project tag entry-proj)))
              (push current-home-entry eproj-symbnav/previous-homes)
              (setf eproj-symbnav/next-homes nil)
              (find-file file)
              (eproj-symbnav/locate-tag-in-current-buffer tag-name tag)
              (eproj-symbnav/on-switch)
              (setf eproj-symbnav/selected-loc
                    (make-eproj-home-entry :buffer (current-buffer)
                                           :position (point-marker)
                                           :symbol tag-name))))))
    (pcase (length entries)
      (`0
       (error "No entries for %s" identifier))
      (`1
       (let ((entry (elt entries 0)))
         (funcall jump-to-home
                  (funcall entry-tag-name entry)
                  (funcall entry-tag entry)
                  (funcall entry-proj entry))))
      (_
       (let ((kmap (make-sparse-keymap)))
         (def-keys-for-map kmap
           ("SPC" (lambda () (interactive)
                    (let ((entry (elt entries (select-mode-get-selected-index))))
                      (eproj-symbnav/show-tag-in-other-window
                       (funcall entry-tag-name entry)
                       (funcall entry-tag entry)
                       (funcall entry-proj entry))))))
         (select-mode-start-selection
          entries
          :buffer-name "Symbol homes"
          :after-init (lambda ()
                        (select-mode-setup)
                        (select-mode-extend-keymap-with kmap))
          :on-selection
          (lambda (idx entry _selection-type)
            (select-mode-exit)
            (funcall jump-to-home
                     (funcall entry-tag-name entry)
                     (funcall entry-tag entry)
                     (funcall entry-proj entry)))
          :item-show-function
          entry-string
          :preamble "Choose symbol\n\n"))))))

;;;###autoload
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
        (eproj-symbnav/switch-to-home-entry prev-home)
        (eproj-symbnav/on-back)))))

;;;###autoload
(defun* setup-eproj-symbnav (&key (bind-keybindings t))
  (when bind-keybindings
    (awhen (current-local-map)
      (def-keys-for-map it
        ("C-." eproj-symbnav/go-to-symbol-home)
        ("C-," eproj-symbnav/go-back)))
    (def-keys-for-map vim:normal-mode-local-keymap
      ("C-." eproj-symbnav/go-to-symbol-home)
      ("C-," eproj-symbnav/go-back))))

(provide 'eproj-symbnav)

;; Local Variables:
;; End:

;; eproj-symbnav.el ends here
