;; search.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 16 January 2012
;; Description:

;; This package is indented to replace buggy isearch and even more
;; buggy vim-search. It's aim is simple and bugless interactive
;; search without much bells and whistles but with highlighting,
;; search for thing at point and history

;; So, lets' do the business
;; Search has two major parts - initiating search with minibuffer prompting
;; for regexp and repeating searches with last regexp used
;; All this stuff comes with matches highlighting
;;
;; Initiated search may be aborted in which case point is returned to
;; where the search was started. While at minibuffer keys S-<up> and S-<down>
;; enable to move between matches

(eval-when-compile
  (require 'cl)
  (require 'macro-util)

  (defvar vim--current-universal-argument-provided?))

(require 'persistent-sessions-global-vars)
(require 'solarized)

;;; search faces

(defgroup search nil
  "A mode for presenting and selecting from a list of items."
  :group 'search)

(defface search-highlight-face '((t (:inherit lazy-highlight)))
  "Face to highlight main matches for regexp being searched for."
  :group 'search)

(defface search-red-face `((t (:background ,+solarized-red+)))
  "Red face to highlight matches."
  :group 'search)
(defface search-orange-face `((t (:background ,+solarized-orange+)))
  "Orange face to highlight matches."
  :group 'search)
(defface search-yellow-face `((t (:background ,+solarized-yellow+)))
  "Yellow face to highlight matches."
  :group 'search)
(defface search-green-face `((t (:background ,+solarized-green+)))
  "Green face to highlight matches."
  :group 'search)
(defface search-cyan-face `((t (:background ,+solarized-cyan+)))
  "Cyan face to highlight matches."
  :group 'search)
(defface search-blue-face `((t (:background ,+solarized-blue+)))
  "Blue face to highlight matches."
  :group 'search)
(defface search-violet-face `((t (:background ,+solarized-violet+)))
  "Violet face to highlight matches."
  :group 'search)
(defface search-magenta-face `((t (:background ,+solarized-magenta+)))
  "Magenta face to highlight matches."
  :group 'search)


(defface search-modeline-highlight-face '((t (:inherit lazy-highlight)))
  "Main face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-red-face `((t (:background ,+solarized-red+)))
  "Red face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-orange-face `((t (:background ,+solarized-orange+)))
  "Orange face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-yellow-face `((t (:background ,+solarized-yellow+)))
  "Yellow face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-green-face `((t (:background ,+solarized-green+)))
  "Green face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-cyan-face `((t (:background ,+solarized-cyan+)))
  "Cyan face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-blue-face `((t (:background ,+solarized-blue+)))
  "Blue face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-violet-face `((t (:background ,+solarized-violet+)))
  "Violet face to show number of matches in mode-line."
  :group 'search)
(defface search-modeline-magenta-face `((t (:background ,+solarized-magenta+)))
  "Magenta face to show number of matches in mode-line."
  :group 'search)

;;; buffer-local search state

(defvar-local search--highlight-face-index 0
  "Index into `+search-highlight-faces+' that determines currently used face for
highlighting searches.")

(defvar-local search--current-regexp nil
  "Regexp being searched now.")

(defvar-local search--start-marker nil
  "Marker which points to location from which search was initiated.")

(defvar-local search--match-overlays nil
  "List of overlays which highlight matches for regexp being searched for.")

(defvar-local search--mode-line-matches-info (make-hash-table :test #'eq)
  "Hash table mapping successive values of ‘search--highlight-face-index’ to formatted strings
denoting number of matches for the given index.")

(defvar-local search--mode-line-matches nil
  "String showing number of matches for last search session to use in ‘mode-line-format’. Aggregates
info in ‘search--mode-line-matches-info’.")

(defvar-local search--direction-forward? nil
  "Whether we're searching in the forward direction right now. Either t or nil.")

(defvar-local search--case-sensetive nil
  "Becomes set to t during case-sensetive matches.")

(defvar-local search-syntax-table nil
  "Syntax table to use when doing searches in this buffer.")

(defvar-local search-ignore-syntax-text-properties nil
  "Set ‘parse-sexp-lookup-properties’ to nil when doing searches. This makes regexps like \\_<
pay attention only to syntax table when doing searches rather than taking text properties
into account.")


(defconst +search-highlight-faces+
  (vector
   (cons 'search-highlight-face 'search-modeline-highlight-face)
   (cons 'search-red-face 'search-modeline-red-face)
   (cons 'search-orange-face 'search-modeline-orange-face)
   (cons 'search-yellow-face 'search-modeline-yellow-face)
   (cons 'search-green-face 'search-modeline-green-face)
   (cons 'search-cyan-face 'search-modeline-cyan-face)
   (cons 'search-blue-face 'search-modeline-blue-face)
   (cons 'search-violet-face 'search-modeline-violet-face)))

(defsubst search--reset-search-highlight-face-index! ()
  (setf search--highlight-face-index 0))

(defun search--increment-search-highlight-face-index ()
  (setf search--highlight-face-index
        (mod (+ 1 search--highlight-face-index)
             (length +search-highlight-faces+))))

;;; global search state

(defvar *search-minibuffer-history* nil
  "List of previously entered regexps.")

(sessions-mark-global-var-for-save '*search-minibuffer-history*)

(defvar *search-init-window* nil
  "Window with buffer being searched in.")

(defvar *search-init-buffer* nil
  "Buffer being searched in.")

(defmacro search--with-initiated-buffer (&rest body)
  `(with-selected-window *search-init-window*
     (with-current-buffer *search-init-buffer*
       ,@body)))

;;; constants

(defvar *search-minibuffer-keymap*
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      ("<enter>"   search-done)
      ("<return>"  search-done)
      ("RET"       search-done)
      ("<escape>"  search-abort)

      ("<f6>"      search-return-to-start)
      ("C-p"       vim-cmd-paste-after-no-adjust)
      ("C-w"       backward-delete-word)
      ("C-S-w"     backward-delete-word*)

      ("<up>"      previous-history-element)
      ("<down>"    next-history-element)
      ("S-<up>"    search--next-from-minibuf)
      ("S-<down>"  search--prev-from-minibuf)

      ("<next>"    search--next-from-minibuf)
      ("<prior>"   search--prev-from-minibuf)

      ("C-SPC"     delete-minibuffer-contents))
    map))

(defconst +search-highlight-limit+ 1000
  "Number of matches that should be highlighted.
Highlighting starts at the beginning of buffer.")

;; Matches longer than approximately 10 lines are probably an error.
(defconst +search-maximum-highlight-length+ 1000
  "Maximum character length of regexp match that should be highlighted.")

(defconst +search-ignore-regexps+
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (re '(".*" ".+" ".**" ".*+" ".+*" ".++"
                  "\\(.*\\)*" "\\(.+\\)*" "\\(.*\\)+" "\\(.+\\)+"
                  "\\(?:.*\\)*" "\\(?:.+\\)*" "\\(?:.*\\)+" "\\(?:.+\\)+"))
      (puthash re t tbl))
    tbl)
  "Regexps for which neither highlighting nor searching should occur.")

;;; search functions

;;;; main search interface

;;;###autoload
(defun search-start-forward (&optional case-sensetive)
  "Initiate forward seach."
  (interactive (list current-prefix-arg))
  (search--start-minibuffer-search t "Search: " case-sensetive))

;;;###autoload
(defun search-start-backward (&optional case-sensetive)
  "Initiate backward seach."
  (interactive (list current-prefix-arg))
  (search--start-minibuffer-search nil "Search backward: " case-sensetive))

;;;###autoload
(defun search-start-forward-new-color (&optional case-sensetive)
  "Initiate forward seach using new highlighting color."
  (interactive (list current-prefix-arg))
  (search--increment-search-highlight-face-index)
  (search-start-forward case-sensetive))

;;;###autoload
(defun search-start-backward-new-color (&optional case-sensetive)
  "Initiate backward seach using new highlighting color."
  (interactive (list current-prefix-arg))
  (search--increment-search-highlight-face-index)
  (search-start-backward case-sensetive))

;;;; search engine

(defun search--start-minibuffer-search (is-forward? prompt case-sensetive)
  "Initiate search through minibuffer. Most of the work will be done in minibuffer."
  ;; prepare search state
  (search--setup-search-for nil
                            is-forward?
                            :case-sensetive case-sensetive)
  (add-hook 'minibuffer-setup-hook #'search--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'search--minibuffer-exit)
  (let ((init-regexp
         (when (region-active-p)
           (regexp-quote
            (get-region-string-no-properties)))))
    (setf search--current-regexp init-regexp)
    (search--with-initiated-buffer
     (search--update init-regexp))
    (read-from-minibuffer prompt
                          init-regexp
                          *search-minibuffer-keymap*
                          nil
                          '*search-minibuffer-history*)))

(cl-defun search--setup-search-for (regex
                                    is-forward?
                                    &key
                                    (save-position t)
                                    (case-sensetive t))
  "Set up internal search variables for use of `search--next-impl',
`search--prev-impl' etc for REGEX."
  (setf search--current-regexp regex
        search--start-marker   (point-marker)
        search--direction-forward? is-forward?
        search--case-sensetive case-sensetive
        *search-init-buffer*  (current-buffer)
        *search-init-window*  (selected-window))
  (when save-position
    (vim-save-position))
  (when regex
    (search--with-initiated-buffer
     (search--highlight-matches regex))))

;;;; Minibuffer-related functions

(defun search--minibuffer-setup ()
  (remove-hook 'minibuffer-setup-hook #'search--minibuffer-setup)
  (add-hook 'after-change-functions #'search--update-after-change
            t ;; append
            t ;; local
            ))

(defun search--minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook   #'search--minibuffer-exit)
  (remove-hook 'minibuffer-setup-hook  #'search--minibuffer-setup)
  (remove-hook 'after-change-functions #'search--update-after-change t))

(defun search--update-after-change (_start _end _old-len)
  "Do `search-update' in initiated buffer. Must be called from minibuffer only."
  ;; Get search regexp before we visit buffer that initiated search.
  (let ((re (search--get-current-regexp)))
    (search--with-initiated-buffer
     (setf search--current-regexp re)
     (condition-case nil
         (search--update search--current-regexp)
       ;; swallow an error or we'll be kicked out of the hook
       (error nil)))))

;;;###autoload
(defun search-abort ()
  (interactive)
  (search--with-initiated-buffer
   (goto-char search--start-marker)
   (search--clean-overlays-with-face-index search--highlight-face-index))
  (exit-minibuffer))

;;;###autoload
(defun search-done ()
  (interactive)
  (exit-minibuffer))

;;;###autoload
(defun search-return-to-start ()
  "Jump to place where current search session was started."
  (interactive)
  (search--with-initiated-buffer
   (goto-char search--start-marker)))

(defun search--next-from-minibuf (count)
  (interactive "p")
  (search--with-initiated-buffer (search--lookup-next count)))

(defun search--prev-from-minibuf (count)
  (interactive "p")
  (search--with-initiated-buffer (search--lookup-prev count)))

;;;; Rest of search engine

(defun search--regex-valid? (regex)
  (and (stringp regex)
       (< 0 (length regex))
       (not (string-match-p "\\\\$" regex))
       (not (gethash regex +search-ignore-regexps+))))

(defmacro searh--prepare-syntax (&rest body)
  (declare (indent 0))
  `(let ((parse-sexp-lookup-properties (not search-ignore-syntax-text-properties)))
     (with-optional-syntax-table search-syntax-table
       ,@body)))

(defun search--update (regexp)
  "Update search for REGEXP in current buffer."
  (when (search--regex-valid? regexp)
    (search--highlight-matches regexp)
    ;; This forgets current location and jumps to the first match of
    ;; updated regexp.
    (goto-char search--start-marker)
    (search--lookup-next 1)))

(defun search--get-current-regexp ()
  "Retrieve entered regexp from minibuffer. Must be called from minibuffer only."
  (minibuffer-contents-no-properties))

(defun search--optionally-use-info-from-last-search ()
  "Refresh search information if necessary and omit cluttering vim position."
  ;; We could be in buffer that is different from where we initiated
  ;; search, in which case regexp being searched for and the search
  ;; direction from the previous search session are used to set up new
  ;; search session in the current buffer.
  (unless (eq? (current-buffer) *search-init-buffer*)
    (search--setup-search-for
     (buffer-local-value 'search--current-regexp *search-init-buffer*)
     (buffer-local-value 'search--direction-forward? *search-init-buffer*)
     :save-position nil
     :case-sensetive (buffer-local-value 'search--case-sensetive *search-init-buffer*))))

;;;###autoload
(defun search-next (count)
  (interactive "p")
  (search--optionally-use-info-from-last-search)
  (search--lookup-next count))

;;;###autoload
(defun search-prev (count)
  (interactive "p")
  (search--optionally-use-info-from-last-search)
  (search--lookup-prev count))

(defun search--lookup-next (count)
  (search-search-in-direction search--direction-forward? nil count))

(defun search--lookup-prev (count)
  (search-search-in-direction search--direction-forward? t count))

(cl-defun search-search-in-direction (forward? reversed count)
  (if forward?
      (if reversed
          (search--prev-impl count)
        (search--next-impl count))
    (if reversed
        (search--next-impl count)
      (search--prev-impl count))))

(defun search--next-impl (count)
  "Move to the next match for `search--current-regexp' in current-buffer."
  (unless (gethash search--current-regexp +search-ignore-regexps+)
    (let ((case-fold-search (not search--case-sensetive)))
      (searh--prepare-syntax
       (wrap-search-around
           forward
         (re-search-forward search--current-regexp nil t)
         :not-found-message
         (concat "Nothing found for regexp " search--current-regexp)
         :count count)))))

(defun search--prev-impl (count)
  "Move to the previous match for `search--current-regexp' in current-buffer."
  (unless (gethash search--current-regexp +search-ignore-regexps+)
    (let ((case-fold-search (not search--case-sensetive)))
      (searh--prepare-syntax
       (wrap-search-around
           backward
         (re-search-backward search--current-regexp nil t)
         :not-found-message
         (concat "Nothing found for regexp " search--current-regexp)
         :count count)))))

;; Some highlight-specific parameters

(defun search--highlight-matches (regexp)
  "Highlight matches of regexp in current buffer."
  (search--clean-overlays-with-face-index search--highlight-face-index)
  (when (and (<= 1 (length regexp))
             (not (gethash regexp +search-ignore-regexps+)))
    (save-excursion
      (goto-char (point-min))
      (let* ((i 0)
             (case-fold-search (not search--case-sensetive))
             (highlight-faces (aref +search-highlight-faces+
                                    search--highlight-face-index))
             (highlight-face (car highlight-faces)))
        (searh--prepare-syntax
          (while (and (< i +search-highlight-limit+)
                      (re-search-forward regexp nil t))
            (let ((text-length (- (match-end 0) (match-beginning 0))))
              (when-let ((overlay (when (< text-length +search-maximum-highlight-length+)
                                    (make-overlay (match-beginning 0)
                                                  (match-end 0)))))
                (overlay-put overlay 'face highlight-face)
                ;; Original-face stores real highlighting face for the overlay.
                ;; The intention is that 'face attribute may be set to nil
                ;; in order to disable highlighting, but also it may need
                ;; to be restored later. The 'original-face preserves face until
                ;; it's restored.
                (overlay-put overlay 'original-face highlight-face)
                (overlay-put overlay 'is-search-highlighting-overlay t)
                (overlay-put overlay 'search-overlay-face-index search--highlight-face-index)
                (push overlay search--match-overlays))
              (cl-incf i)))
          (puthash search--highlight-face-index
                   (propertize
                    (if (= i +search-highlight-limit+)
                        (concat (number->string +search-highlight-limit+) "+")
                      (number->string i))
                    'face
                    (cdr highlight-faces))
                   search--mode-line-matches-info)
          (setf search--mode-line-matches
                (concat " "
                        (mapconcat (lambda (i)
                                     (let ((str (gethash i search--mode-line-matches-info)))
                                       (if (and (= i search--highlight-face-index)
                                                (< 1 (hash-table-count search--mode-line-matches-info)))
                                           (let ((str2 (copy-sequence str)))
                                             (add-face-text-property 0
                                                                     (length str)
                                                                     '(:inherit highlight)
                                                                     nil
                                                                     str2)
                                             str2)
                                         str)))
                                   (-iota (hash-table-count search--mode-line-matches-info))
                                   "/"))))))))

(defun search--clean-overlays-with-face-index (idx)
  "Remove all search overlays for given face index IDX. Must be
called in buffer that initiated search."
  (save-excursion
    (setf search--match-overlays
          (delete-if-with-action!
           (lambda (o)
             (= (overlay-get o 'search-overlay-face-index) idx))
           search--match-overlays
           #'delete-overlay))))

(defun search--clean-all-overlays! ()
  "Remove all search overlays in current buffer."
  (save-excursion
    (dolist (o search--match-overlays)
      (delete-overlay o))
    (setf search--match-overlays nil
          search--mode-line-matches nil)
    (clrhash search--mode-line-matches-info)
    (remove-overlays (point-min) (point-max) 'is-search-highlighting-overlay t)))

(defun search-disable-highlighting ()
  "Disable highlighting in current buffer."
  (search--clean-all-overlays!)
  (search--reset-search-highlight-face-index!))

(defun search-disable-all-highlighting ()
  "Disable highlighting in all buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (search--clean-all-overlays!)
      (search--reset-search-highlight-face-index!))))

;;;###autoload
(defun search-highlighting-is-enabled? ()
  (awhen (car-safe search--match-overlays)
    (not (null (overlay-get it 'face)))))

;;;###autoload
(defun search-toggle-highlighting ()
  "Toggle highlighting of matches in current buffer."
  (interactive)
  (when search--match-overlays
    (let ((enabled (search-highlighting-is-enabled?)))
      (if enabled
          (dolist (overlay search--match-overlays)
            (overlay-put overlay 'face nil))
        (dolist (overlay search--match-overlays)
          (overlay-put overlay
                       'face
                       (overlay-get overlay 'original-face)))))))

;;;

;;;###autoload
(defmacro search-def-autoexpand-advices (expand-command modes)
  `(progn
     ,@(cl-loop
         for command in '(search--next-from-minibuf
                          search--prev-from-minibuf
                          search-next
                          search-prev
                          search-for-symbol-at-point-forward
                          search-for-symbol-at-point-backward
                          search-for-word-at-point-forward
                          search-for-word-at-point-backward)
         collect `(make-advice-expand-on-search ,command ,expand-command ,modes))))

;;;

(cl-defmacro search--make-search-for-thing (name
                                            alt-name
                                            get-bounds-expr
                                            mk-action-after
                                            &key
                                            is-forward
                                            regex-start-func
                                            regex-end-func
                                            (error-message nil)
                                            (force-include-bounds-to 'not-provided))
  "BOUNDS-FUNC should return cons pair (START . END), everything else is
obvious"
  (declare (indent 4))
  (let* ((include-bounds?-var '#:non-strict)
         (bounds-var '#:bounds)
         (substr-var '#:substr)
         (count-var '#:count)
         (make-search-func
          (lambda (name create-reset?)
            `(defun ,name (,count-var)
               "Do search in the specified direction of a text at point (or from currently selected region"
               (interactive "p")
               (let* ((,include-bounds?-var ,(if (eq force-include-bounds-to 'not-provided)
                                                 '(not vim--current-universal-argument-provided?)
                                               force-include-bounds-to))
                      (,bounds-var ,get-bounds-expr)
                      (,substr-var (progn
                                     ,@(when error-message
                                         (cl-assert (stringp error-message))
                                         (list
                                          `(unless ,bounds-var
                                             (error ,error-message))))
                                     (buffer-substring-no-properties
                                      (car ,bounds-var)
                                      (cdr ,bounds-var)))))
                 (vim-save-position)
                 (goto-char
                  ,(if is-forward
                       `(cdr ,bounds-var)
                     `(car ,bounds-var)))
                 ,(unless create-reset?
                    '(search--increment-search-highlight-face-index))
                 (search--setup-search-for
                  (if ,include-bounds?-var
                      (concat (funcall ,regex-start-func ,substr-var)
                              (regexp-quote ,substr-var)
                              (funcall ,regex-end-func ,substr-var))
                    (regexp-quote ,substr-var))
                  ,is-forward
                  :case-sensetive t
                  :save-position nil)
                 ,(funcall mk-action-after count-var))))))
    `(progn
       ,(funcall make-search-func name t)
       ,(funcall make-search-func alt-name nil))))

;; Haskell search

(defsubst search-for-haskell-symbol-at-point-regex-start-func (pat)
  (if (string-match-p "^[a-zA-Z0-9]" pat)
      ;; Don't use \\_<,\\_> since they rely on
      ;; syntax table which was tampered with in haskell
      ;; mode so that e.g. regexp "\\_<Node" won't match
      ;; the input "x:Node (x - 1)".
      "\\<"
    ""))

(defsubst search-for-haskell-symbol-at-point-regex-end-func (pat)
  (if (string-match-p "[a-zA-Z0-9]$" pat)
      "\\>"
    ""))

(defsubst search-for-ghc-core-symbol-at-point-regex-start-func (pat)
  (if (string-match-p "^[a-zA-Z0-9$]" pat)
      "\\_<"
    ""))

(defsubst search-for-ghc-core-symbol-at-point-regex-end-func (pat)
  (search-for-haskell-symbol-at-point-regex-end-func pat))

;;;###autoload (autoload 'search-for-haskell-symbol-at-point-forward "search" nil t)
;;;###autoload (autoload 'search-for-haskell-symbol-at-point-forward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-haskell-symbol-at-point-forward
    search-for-haskell-symbol-at-point-forward-new-color
    (bounds-of-thing-at-point 'haskell-symbol)
    (lambda (x) `(search--next-impl ,x))
  :is-forward t
  :regex-start-func #'search-for-haskell-symbol-at-point-regex-start-func
  :regex-end-func #'search-for-haskell-symbol-at-point-regex-end-func
  :error-message "No symbol at point")

;;;###autoload (autoload 'search-for-haskell-symbol-at-point-backward "search" nil t)
;;;###autoload (autoload 'search-for-haskell-symbol-at-point-backward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-haskell-symbol-at-point-backward
    search-for-haskell-symbol-at-point-backward-new-color
    (bounds-of-thing-at-point 'haskell-symbol)
    (lambda (x) `(search--prev-impl ,x))
  :is-forward nil
  :regex-start-func #'search-for-haskell-symbol-at-point-regex-start-func
  :regex-end-func #'search-for-haskell-symbol-at-point-regex-end-func
  :error-message "No symbol at point")

;;;###autoload (autoload 'search-for-ghc-core-symbol-at-point-forward "search" nil t)
;;;###autoload (autoload 'search-for-ghc-core-symbol-at-point-forward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-ghc-core-symbol-at-point-forward
    search-for-ghc-core-symbol-at-point-forward-new-color
    (bounds-of-thing-at-point 'ghc-core-symbol)
    (lambda (x) `(search--next-impl ,x))
  :is-forward t
  :regex-start-func #'search-for-ghc-core-symbol-at-point-regex-start-func
  :regex-end-func #'search-for-ghc-core-symbol-at-point-regex-end-func
  :error-message "No symbol at point")

;;;###autoload (autoload 'search-for-ghc-core-symbol-at-point-backward "search" nil t)
;;;###autoload (autoload 'search-for-ghc-core-symbol-at-point-backward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-ghc-core-symbol-at-point-backward
    search-for-ghc-core-symbol-at-point-backward-new-color
    (bounds-of-thing-at-point 'ghc-core-symbol)
    (lambda (x) `(search--prev-impl ,x))
  :is-forward nil
  :regex-start-func #'search-for-ghc-core-symbol-at-point-regex-start-func
  :regex-end-func #'search-for-ghc-core-symbol-at-point-regex-end-func
  :error-message "No symbol at point")

;; Lispocentric searches
;;;###autoload (autoload 'search-for-symbol-at-point-forward "search" nil t)
;;;###autoload (autoload 'search-for-symbol-at-point-forward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-symbol-at-point-forward
    search-for-symbol-at-point-forward-new-color
    (bounds-of-thing-at-point 'symbol)
    (lambda (x) `(search--next-impl ,x))
  :is-forward t
  :regex-start-func (constantly "\\_<")
  :regex-end-func (constantly "\\_>")
  :error-message "No symbol at point")

;;;###autoload (autoload 'search-for-symbol-at-point-backward "search" nil t)
;;;###autoload (autoload 'search-for-symbol-at-point-backward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-symbol-at-point-backward
    search-for-symbol-at-point-backward-new-color
    (bounds-of-thing-at-point 'symbol)
    (lambda (x) `(search--prev-impl ,x))
  :is-forward nil
  :regex-start-func (constantly "\\_<")
  :regex-end-func (constantly "\\_>")
  :error-message "No symbol at point")

;;;

(defsubst search--get-bounds-covered-by-vim-motion (motion)
  (let ((m (save-excursion
             (funcall motion))))
    (cons (vim-motion-begin-pos m)
          (vim-motion-end-pos m))))

;;;###autoload (autoload 'search-for-word-at-point-forward "search" nil t)
;;;###autoload (autoload 'search-for-word-at-point-forward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-word-at-point-forward
    search-for-word-at-point-forward-new-color
    (search--get-bounds-covered-by-vim-motion
     #'vim:motion-inner-word)
    (lambda (x) `(search--next-impl ,x))
  :is-forward t
  :regex-start-func (constantly "\\<")
  :regex-end-func (constantly "\\>")
  :error-message "No word at point")

;;;###autoload (autoload 'search-for-word-at-point-backward "search" nil t)
;;;###autoload (autoload 'search-for-word-at-point-backward-new-color "search" nil t)
(search--make-search-for-thing
    search-for-word-at-point-backward
    search-for-word-at-point-backward-new-color
    (search--get-bounds-covered-by-vim-motion
     #'vim:motion-inner-word)
    (lambda (x) `(search--prev-impl ,x))
  :is-forward nil
  :regex-start-func (constantly "\\<")
  :regex-end-func (constantly "\\>")
  :error-message "No word at point")

(add-to-list 'debug-ignored-errors "\\`No \\(?:symbol\\|word\\) at point\\'")

(provide 'search)

;; Local Variables:
;; End:

;; search.el ends here
