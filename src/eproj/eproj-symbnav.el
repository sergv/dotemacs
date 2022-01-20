;; eproj-symbnav.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'macro-util))

(declare-function eproj-reload-project! "eproj")

(require 'select-mode)
(require 'eproj)
(require 'eproj-customization)

;;; tag/symbol navigation (navigation over homes)

(defvar eproj-symbnav/previous-homes nil
  "Previous locations (markers) from which symbol search was invoked.")

(defvar eproj-symbnav/selected-loc nil
  "Home entry corresponding to the most recently visited tag.")

(defvar eproj-symbnav/next-homes nil
  "Next locations that were visited but now obscured by going back.")

(cl-defstruct (eproj-home-entry
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
         (eproj-resolve-to-abs-path (eproj-tag/file tag) proj)))
    (unless (file-exists-p file)
      (error "file %s does not exist" file))
    file))

(defun eproj-symbnav/locate-tag-in-current-buffer (tag-name tag)
  (goto-line-dumb (eproj-tag/line tag))
  (if tag-name
      (save-match-data
        (let ((tag-name-re (regexp-quote tag-name)))
          (when (re-search-forward tag-name-re
                                   (line-end-position)
                                   t)
            (goto-char (match-beginning 0)))))
    (awhen (eproj-tag/column tag)
      (move-to-column it)))
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

(defun eproj-symbnav-get-file-name ()
  (cond
    (buffer-file-name
     (expand-file-name buffer-file-name))
    ((and (boundp 'magit-buffer-file-name)
          magit-buffer-file-name)
     (expand-file-name magit-buffer-file-name))))

(defun eproj-symbnav/ensure-tags-loaded! (effective-major-mode proj)
  ;; Load tags if there're none.
  (unless (or (eproj--get-tags proj)
              (assq effective-major-mode (eproj--get-tags proj)))
    (eproj-reload-project! proj)
    (unless (eproj--get-tags proj)
      (error "Project %s loaded no names\nProject: %s"
             (eproj-project/root proj)
             proj))
    (unless (assq effective-major-mode (eproj--get-tags proj))
      (error "No names in project %s for mode %s"
             (eproj-project/root proj)
             effective-major-mode))))

(defun eproj-symbnav/go-to-symbol-home-impl (identifier use-regexp?)
  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (case-fold-search (and current-prefix-arg
                                (<= 16 (car current-prefix-arg))))
         (effective-major-mode (eproj/resolve-synonym-modes major-mode))
         (next-home-entry (car-safe eproj-symbnav/next-homes)))

    (eproj-symbnav/ensure-tags-loaded! effective-major-mode proj)

    (if (and eproj-symbnav-remember-choices
             next-home-entry
             (when-let (next-symbol (eproj-home-entry/symbol next-home-entry))
               (if use-regexp?
                   (string-match-p identifier next-symbol)
                 (string= identifier next-symbol))))
        (progn
          (eproj-symbnav/switch-to-home-entry next-home-entry)
          (eproj-symbnav/on-switch)
          (push (eproj-symbnav-current-home-entry)
                eproj-symbnav/previous-homes)
          (setf eproj-symbnav/selected-loc (pop eproj-symbnav/next-homes)))

      (let* ((lang (aif (gethash effective-major-mode eproj/languages-table)
                       it
                     (error "unsupported language %s" effective-major-mode)))
             (tag->string (eproj-language/tag->string-func lang))
             (tag->kind (eproj-language/show-tag-kind-procedure lang)))
        (eproj-symbnav/choose-location-to-jump-to
         identifier
         tag->string
         tag->kind
         (eproj-symbnav-get-file-name)
         proj
         (eproj-symbnav-current-home-entry)
         (eproj-get-matching-tags proj
                                  effective-major-mode
                                  identifier
                                  use-regexp?)
         t
         "Choose symbol\n\n")))))

(defface eproj-symbnav-file-name
  '((t :inherit compilation-info))
  "Face to put on file names."
  :group 'eproj)

(defface eproj-symbnav-line-number
  '((t :inherit compilation-line-number))
  "Face to put on line numbers."
  :group 'eproj)

(defface eproj-symbnav-column-number
  '((t :inherit compilation-column-number))
  "Face to put on line numbers."
  :group 'eproj)

;;;###autoload
(defun eproj-symbnav/choose-location-to-jump-to
    (identifier
     tag->string
     tag->kind
     current-buffer-file-name
     current-proj ;; may be nil
     current-home-entry
     tag-entries ;; list of (tag-name tag-struct tag-proj) triples
     enable-shortcut? ;; Jump to destination if there’s only one tag
     preamble
     )
  (cl-assert (functionp tag->string))
  (cl-assert (functionp tag->kind))
  (cl-assert (or (null current-proj) (eproj-project-p current-proj)))
  (let* ((tag->sort-token
          (lambda (tag-name tag)
            (list tag-name
                  (funcall tag->kind tag)
                  (eproj-tag/file tag)
                  (eproj-tag/line tag))))
         (sort-tokens<
          (lambda (x y)
            (let ((symbol-x (cl-first x))
                  (symbol-y (cl-first y))
                  (tag-kind-x (cl-second x))
                  (tag-kind-y (cl-second y))
                  (file-x (cl-third x))
                  (file-y (cl-third y))
                  (line-x (cl-fourth x))
                  (line-y (cl-fourth y)))
              (cl-assert (or (null symbol-x) (stringp symbol-x)))
              (cl-assert (or (null symbol-y) (stringp symbol-y)))
              (cl-assert (or (stringp tag-kind-x) (null tag-kind-x)) nil "Unexpected tag kind: %s" tag-kind-x)
              (cl-assert (or (stringp tag-kind-y) (null tag-kind-y)) nil "Unexpected tag kind: %s" tag-kind-y)
              (cl-assert (stringp file-x))
              (cl-assert (stringp file-y))
              (cl-assert (numberp line-x))
              (cl-assert (numberp line-y))
              (or (string<-safe symbol-x symbol-y)
                  (and (equal symbol-x symbol-y)
                       (or (and tag-kind-x
                                tag-kind-y
                                (string< tag-kind-x tag-kind-y))
                           (and (or (not tag-kind-x)
                                    (not tag-kind-y)
                                    (string= tag-kind-x tag-kind-y))
                                (or (string< file-x file-y)
                                    (and (string= file-x file-y)
                                         (< line-x line-y))))))))))
         (tag->string
          (if current-proj
              (lambda (tag-proj tag-name tag)
                (let* ((expanded-tag-file
                        (expand-file-name
                         (eproj-resolve-to-abs-path (eproj-tag/file tag) tag-proj)))
                       (tag-name-pretty
                        (when tag-name
                          (cond ((string= current-buffer-file-name
                                          expanded-tag-file)
                                 (propertize tag-name 'face 'font-lock-negation-char-face))
                                ((string= (eproj-project/root current-proj)
                                          (eproj-project/root tag-proj))
                                 ;; use italic instead of underscore
                                 (propertize tag-name 'face 'italic))
                                (t
                                 tag-name)))))
                  (funcall tag->string tag-proj tag-name-pretty tag)))
            (lambda (tag-proj tag-name tag)
              (let* ((expanded-tag-file
                      (expand-file-name
                       (eproj-resolve-to-abs-path (eproj-tag/file tag) tag-proj)))
                     (tag-name-pretty
                      (when tag-name
                        (cond ((string= current-buffer-file-name
                                        expanded-tag-file)
                               (propertize tag-name 'face 'font-lock-negation-char-face))
                              (t
                               tag-name)))))
                (funcall tag->string tag-proj tag-name-pretty tag)))))
         (entry-sort-token #'cl-first)
         (entry-tag-name #'cl-second)
         (entry-tag #'cl-third)
         (entry-string #'cl-fourth)
         (entry-proj #'cl-fifth)
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
                     (cl-destructuring-bind (tag-name tag tag-proj)
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
      ((and (guard enable-shortcut?)
            `1)
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
          :buffer-name "*Symbol navigation*"
          :after-init (lambda ()
                        (select-mode-setup)
                        (select-mode-extend-keymap-with kmap))
          :on-selection
          (lambda (_idx entry _selection-type)
            (select-mode-exit)
            (funcall jump-to-home
                     (funcall entry-tag-name entry)
                     (funcall entry-tag entry)
                     (funcall entry-proj entry)))
          :item-show-function entry-string
          :preamble preamble))))))

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
(cl-defun setup-eproj-symbnav (&key (bind-keybindings t))
  (when bind-keybindings
    (awhen (current-local-map)
      (def-keys-for-map it
        ("C-." eproj-symbnav/go-to-symbol-home)
        ("C-," eproj-symbnav/go-back)))
    (def-keys-for-map vim-normal-mode-local-keymap
      ("C-." eproj-symbnav/go-to-symbol-home)
      ("C-," eproj-symbnav/go-back))))

(provide 'eproj-symbnav)

;; Local Variables:
;; End:

;; eproj-symbnav.el ends here
