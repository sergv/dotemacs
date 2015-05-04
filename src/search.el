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

(require 'solarized+)

(defparameter *search-current-regexp* nil
  "Regexp being searched now.")
(defparameter *search-start-marker* nil
  "Marker which points to location from which search was initiated.")
(defparameter *search-match-overlays* nil
  "List of overlays which highlight matches for regexp being searched for.")

(defface search-highlight-face '((t (:inherit lazy-highlight)))
  "Face to highlight main matches for regexp being searched for.")

(defface search-red-face `((t (:background ,+solarized-red+)))
  "Alternative red face.")
(defface search-orange-face `((t (:background ,+solarized-orange+)))
  "Alternative orange face.")
(defface search-yellow-face `((t (:background ,+solarized-yellow+)))
  "Alternative yellow face.")
(defface search-green-face `((t (:background ,+solarized-green+)))
  "Alternative green face.")
(defface search-cyan-face `((t (:background ,+solarized-cyan+)))
  "Alternative cyan face.")
(defface search-blue-face `((t (:background ,+solarized-blue+)))
  "Alternative blue face.")
(defface search-violet-face `((t (:background ,+solarized-violet+)))
  "Alternative violet face.")
(defface search-magenta-face `((t (:background ,+solarized-magenta+)))
  "Alternative magenta face.")

(defparameter *search-highlight-faces*
  [search-highlight-face
   search-red-face
   search-orange-face
   search-yellow-face
   search-green-face
   search-cyan-face
   search-blue-face
   search-violet-face
   search-magenta-face])
(defparameter *search-highlight-face-index* 0
  "Index into `*search-highlight-faces*' that determines currently used face for
highlighting searches.")

(defun search/reset-search-highlight-face-index ()
  (setf *search-highlight-face-index* 0))

(defun search/increment-search-highlight-face-index ()
  (setf *search-highlight-face-index*
        (min (+ 1 *search-highlight-face-index*)
             (- (length *search-highlight-faces*) 1))))

(defparameter *search-minibuffer-keymap*
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      ("<enter>"   search-done)
      ("<return>"  search-done)
      ("RET"       search-done)
      ("<escape>"  search-abort)

      ("<f6>"      search-return-to-start)
      ("C-p"       yank)
      ("C-w"       backward-delete-word)
      ("C-S-w"     backward-delete-word*)

      ("<up>"      previous-history-element)
      ("<down>"    next-history-element)
      ("S-<up>"    search/next-from-minibuf)
      ("S-<down>"  search/prev-from-minibuf)

      ("<next>"    search/next-from-minibuf)
      ("<prior>"   search/prev-from-minibuf)

      ("C-SPC"     delete-minibuffer-contents))
    map))

(defparameter *search-minibuffer-history* nil
  "List of previously entered regexps.")
(defparameter *search-init-window* nil
  "Window with buffer being searched in.")
(defparameter *search-init-buffer* nil
  "Buffer being searched in.")
(defparameter *search-direction* nil
  "Direction currentl being searched in. Either 'forward or 'backward.")
(defparameter *search-case-sensetive* nil
  "Becomes set to te during case-sensetive matches.")

(defun* search/setup-search-for (regex
                                 direction
                                 &key
                                 (save-position t)
                                 (case-sensetive t)
                                 (reset-highlighting t))
  "Set up internal search variables for use of `search/next-impl',
`search/prev-impl' etc for REGEX."
  (when reset-highlighting
    (search-disable-highlighting))
  (setf *search-current-regexp* regex
        *search-start-marker* (point-marker)
        *search-init-buffer* (current-buffer)
        *search-init-window* (selected-window)
        *search-direction* direction
        *search-case-sensetive* case-sensetive)
  (when save-position
    (vim:save-position))
  (when regex
    (search/highlight-matches regex)))


(defun search-start-forward (&optional case-sensetive)
  "Initiate forward seach."
  (interactive (list current-prefix-arg))
  (search/start 'forward "Search: " case-sensetive nil))

(defun search-start-backward (&optional case-sensetive)
  "Initiate backward seach."
  (interactive (list current-prefix-arg))
  (search/start 'backward "Search backward: " case-sensetive nil))


(defun search-start-forward-new-color (&optional case-sensetive)
  "Initiate forward seach using new highlighting color."
  (interactive (list current-prefix-arg))
  (search/increment-search-highlight-face-index)
  (search-start-forward case-sensetive))

(defun search-start-backward-new-color (&optional case-sensetive)
  "Initiate backward seach using new highlighting color."
  (interactive (list current-prefix-arg))
  (search/increment-search-highlight-face-index)
  (search-start-backward case-sensetive))


(defun search/start (direction prompt case-sensetive reset-highlighting)
  "Initiate search. Most of the work will be done in minibuffer."
  (search/setup-search-for nil direction
                           :case-sensetive case-sensetive
                           :reset-highlighting reset-highlighting)

  (add-hook 'minibuffer-setup-hook #'search/minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'search/minibuffer-exit)

  (let ((init-regexp
         (if (region-active?)
           (regexp-quote
            (get-region-string-no-properties))
           nil)))
    (setf *search-current-regexp* init-regexp)
    (search/update init-regexp)
    (read-from-minibuffer prompt
                          init-regexp
                          *search-minibuffer-keymap*
                          nil
                          ;; 'regexp-history
                          '*search-minibuffer-history*)))



(defparameter *search-minibuffer* nil
  "Becomes set to current minibuffer with current prompt for regexp.
When not prompting in minibuffer then this variable is set to nil.")


(defun search/minibuffer-setup ()
  (remove-hook 'minibuffer-setup-hook #'search/minibuffer-setup)
  (setf *search-minibuffer* (current-buffer))
  (add-hook 'after-change-functions #'search/update-after-change t t))

(defun search/minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook #'search/minibuffer-exit)
  (remove-hook 'after-change-functions #'search/update-after-change t)

  (setf *search-minibuffer* nil))

(defun search/regex-valid? (regex)
  (and (string? regex)
       (< 0 (length regex))
       (not (string-match-pure? "\\\\$" regex))))

(defun search/update-after-change (start end old-len)
  (setf *search-current-regexp* (search/get-current-regexp))
  (condition-case nil
      (search/update *search-current-regexp*)
    ;; swallow an error or we'll be kicked out of the hook
    (error nil)))

(defun search/update (regexp)
  (when (search/regex-valid? regexp)
    (search/highlight-matches regexp)
    ;; this forgets current location and jums to first match of
    ;; updated regexp
    (search/with-initiated-buffer
     (goto-char *search-start-marker*)
     (search/next-from-minibuf))))


(defun search/get-current-regexp ()
  "Retrieve entered regexp from minibuffer."
  (search/with-prompt-buffer
   (minibuffer-contents-no-properties)))


(defmacro search/with-initiated-buffer (&rest body)
  `(with-selected-window *search-init-window*
     (with-current-buffer *search-init-buffer*
       ,@body)))

(defmacro search/with-prompt-buffer (&rest body)
  `(with-current-buffer *search-minibuffer*
     ,@body))

(defun search/next-from-minibuf ()
  (interactive)
  (search/with-initiated-buffer
   (search-search-in-direction *search-direction*)))

(defun search/prev-from-minibuf ()
  (interactive)
  (search/with-initiated-buffer
   (search-search-in-direction *search-direction* :reversed t)))


(defun search-optionally-refresh-info ()
  "Refresh search information if necessary and omit cluttering vim position."
  ;; we could be in buffer that is different from where we initiated search,
  ;; in which case regexp being searched for and search direction from the
  ;; previous search are used to set up new search session in the current
  ;; buffer
  (unless (eq? (current-buffer) *search-init-buffer*)
    (search/setup-search-for *search-current-regexp*
                             *search-direction*
                             :save-position nil
                             :case-sensetive *search-case-sensetive*
                             :reset-highlighting t)))

(defun search-next ()
  (interactive)
  (search-optionally-refresh-info)
  (search-search-in-direction *search-direction*))

(defun search-prev ()
  (interactive)
  (search-optionally-refresh-info)
  (search-search-in-direction *search-direction* :reversed t))



(defun* search-search-in-direction (direction &key reversed)
  (case direction
    (forward  (if reversed
                (search/prev-impl)
                (search/next-impl)))
    (backward (if reversed
                (search/next-impl)
                (search/prev-impl)))))

(defun search/next-impl ()
  "Move to the next match for `*search-current-regexp*'."
  (unless (gethash *search-current-regexp* *search-ignore-regexps*)
    (let ((p (point))
          (minibuffer-message-timeout 1)
          (case-fold-search (not *search-case-sensetive*)))
      (unless (re-search-forward *search-current-regexp* nil t)
        (goto-char (point-min))
        (if (re-search-forward *search-current-regexp* nil t)
          (message "Wrapped at bottom")
          (progn
            (message "Nothing found for %s" *search-current-regexp*)
            (goto-char p)))))))

(defun search/prev-impl ()
  "Move to the previous match for `*search-current-regexp*'."
  (unless (gethash *search-current-regexp* *search-ignore-regexps*)
    (let ((p (point))
          (minibuffer-message-timeout 1)
          (case-fold-search (not *search-case-sensetive*)))
      (unless (re-search-backward *search-current-regexp* nil t)
        (goto-char (point-max))
        (if (re-search-backward *search-current-regexp* nil t)
          (message "Wrapped at top")
          (progn
            (message "Nothing found for %s" *search-current-regexp*)
            (goto-char p)))))))


;; Some highlight-specific parameters

(defparameter *search-highlight-limit* 1000
  "Number of matches that should be highlighted.
Highlighting starts at the beginning of buffer")
;; matches longer than approximately 10 lines are probably an error
(defparameter *search-maximum-highlight-length* 1000
  "Maximum character length of regexp match that should be highlighted.")
(defparameter *search-ignore-regexps*
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (re '("\\(.*\\)*" "\\(.+\\)*" "\\(.*\\)+" "\\(.+\\)+"
                  "\\(?:.*\\)*" "\\(?:.+\\)*" "\\(?:.*\\)+" "\\(?:.+\\)+"))
      (puthash re re tbl))
    tbl)
  "Regexps for which neither highlighting nor searching should occur.")


(defun search/highlight-matches (regexp)
  (search/with-initiated-buffer
   (search/clean-overlays-with-face-index *search-highlight-face-index*)
   (when (and (<= 1 (length regexp))
              (not (gethash regexp *search-ignore-regexps*)))
     (save-excursion
       (goto-char (point-min))
       (let ((i 0)
             (case-fold-search (not *search-case-sensetive*)))
         (while (and (< i *search-highlight-limit*)
                     (re-search-forward regexp nil t))
           (let* ((text-length (- (match-end 0) (match-beginning 0)))
                  (overlay (when (< text-length *search-maximum-highlight-length*)
                             (make-overlay (match-beginning 0)
                                           (match-end 0))))
                  (highlight-face (aref *search-highlight-faces*
                                        *search-highlight-face-index*)))
             (overlay-put overlay 'face highlight-face)
             ;; original-face stores highlighting face for the overlay
             ;; The intention is that 'face attribute may be set to nil
             ;; in order to disable highlighting, but after it may need
             ;; to be restored.
             ;; The original-face preserves face until it's restored.
             (overlay-put overlay 'original-face highlight-face)
             (overlay-put overlay 'is-search-highlighting-overlay t)
             (overlay-put overlay 'search-overlay-face-index *search-highlight-face-index*)
             (push overlay *search-match-overlays*)
             (incf i))))))))

(defun search/clean-overlays-with-face-index (idx)
  "Remove all search overlays. Must be called in buffer that initiated search."
  (save-excursion
    (setf *search-match-overlays*
          (delete-if-with-action!
           (lambda (o)
             (= (overlay-get o 'search-overlay-face-index) idx))
           *search-match-overlays*
           #'delete-overlay))))

(defun search/clean-all-overlays ()
  "Remove all search overlays. Must be called in buffer that initiated search."
  (save-excursion
    (dolist (o *search-match-overlays*)
      (delete-overlay o))
    (setf *search-match-overlays* nil)
    (remove-overlays (point-min) (point-max) 'is-search-highlighting-overlay t)))


(defun search-return-to-start ()
  "Jump to place where current search session was started."
  (interactive)
  (search/with-initiated-buffer
   (goto-char *search-start-marker*)))


(defun search-abort ()
  (interactive)
  (search/with-initiated-buffer
   (goto-char *search-start-marker*)
   (search/clean-overlays-with-face-index *search-highlight-face-index*))
  (exit-minibuffer))

(defun search-done ()
  (interactive)
  (exit-minibuffer))

(defun search-disable-highlighting ()
  (search/clean-all-overlays)
  (search/reset-search-highlight-face-index))

(defun search-toggle-highlighting ()
  (interactive)
  (when *search-match-overlays*
    (let ((enabled (overlay-get (car *search-match-overlays*) 'face)))
      (if enabled
        (dolist (overlay *search-match-overlays*)
          (overlay-put overlay 'face nil))
        (dolist (overlay *search-match-overlays*)
          (overlay-put overlay
                       'face
                       (overlay-get overlay 'original-face)))))))

;;;

(defmacro search-def-autoexpand-advices (expand-command
                                         modes)
  `(progn
     ,@(loop
         for command in '(search/next-from-minibuf
                          search/prev-from-minibuf
                          search-next
                          search-prev
                          search-for-symbol-at-point-forward
                          search-for-symbol-at-point-backward
                          search-for-word-at-point-forward
                          search-for-word-at-point-backward)
         collect `(defadvice:expand-on-search ,command ,expand-command ,modes))))

;;;

(defmacro* search/make-search-for-thing (name
                                         alt-name
                                         bounds-func
                                         action-after
                                         direction
                                         &key
                                         (regex-start-func (constantly "\\_<"))
                                         (regex-end-func   (constantly "\\_>"))
                                         (error-message nil))
  "BOUNDS-FUNC should return cons pair (START . END), everything else is
obvious"
  (let* ((bounds-var (gensym "bounds"))
         (substr-var (gensym "substr"))
         (non-strict-var (gensym "non-strict"))
         (make-search-func
          (lambda (name reset)
            `(defun ,name (&optional ,non-strict-var)
               (interactive (list current-prefix-arg))
               (let ((,bounds-var (funcall #',bounds-func)))
                 (if (null ,bounds-var)
                   ,(when error-message `(error ,error-message))
                   (let ((,substr-var (buffer-substring-no-properties (car ,bounds-var)
                                                                      (cdr ,bounds-var))))
                     (vim:save-position)
                     (goto-char (cdr ,bounds-var))
                     ,@(when (not reset)
                         '((search/increment-search-highlight-face-index)))
                     (search/setup-search-for
                      (concat (unless ,non-strict-var
                                (funcall #',regex-start-func ,substr-var))
                              (regexp-quote ,substr-var)
                              (unless ,non-strict-var
                                (funcall #',regex-end-func ,substr-var)))
                      ,direction
                      :case-sensetive t
                      :reset-highlighting ,reset)
                     (funcall #',action-after))))))))
    `(progn
       ,(funcall make-search-func name t)
       ,(funcall make-search-func alt-name nil)
       ;; (defun ,name (&optional ,non-strict-var)
       ;;   (interactive (list current-prefix-arg))
       ;;   (let ((,bounds-var (funcall #',bounds-func)))
       ;;     (if (null ,bounds-var)
       ;;       ,(when error-message `(error ,error-message))
       ;;       (let ((,substr-var (buffer-substring-no-properties (car ,bounds-var)
       ;;                                                          (cdr ,bounds-var))))
       ;;         (vim:save-position)
       ;;         (goto-char (cdr ,bounds-var))
       ;;         (search/setup-search-for
       ;;          (concat (unless ,non-strict-var
       ;;                    (funcall #',regex-start-func ,substr-var))
       ;;                  (regexp-quote ,substr-var)
       ;;                  (unless ,non-strict-var
       ;;                    (funcall #',regex-end-func ,substr-var)))
       ;;          ,direction
       ;;          :case-sensetive t
       ;;          :reset-highlighting t)
       ;;         (funcall #',action-after)))))
       ;; (defun ,alt-name (&optional ,non-strict-var)
       ;;   (interactive (list current-prefix-arg))
       ;;   (search/increment-search-highlight-face-index)
       ;;   (,name ,non-strict-var))
       )))

(autoload 'forward-haskell-symbol "haskell-misc" t)

;; Haskell search

(defsubst search-for-haskell-symbol-at-point-regex-start-func (pat)
  (if (string-match-pure? "^[a-zA-Z0-9]" pat)
    ;; Don't use \\_<,\\_> since they rely on
    ;; syntax table which was tampered with in haskell
    ;; mode so that e.g. regexp "\\_<Node" won't match
    ;; the input "x:Node (x - 1)".
    "\\<"
    ""))

(defsubst search-for-haskell-symbol-at-point-regex-end-func (pat)
  (if (string-match-pure? "[a-zA-Z0-9]$" pat)
    "\\>"
    ""))

(search/make-search-for-thing search-for-haskell-symbol-at-point-forward
                              search-for-haskell-symbol-at-point-forward-new-color
                              (lambda () (bounds-of-thing-at-point 'haskell-symbol))
                              search/next-impl
                              'forward
                              :regex-start-func search-for-haskell-symbol-at-point-regex-start-func
                              :regex-end-func search-for-haskell-symbol-at-point-regex-end-func
                              :error-message "No symbol at point")
(search/make-search-for-thing search-for-haskell-symbol-at-point-backward
                              search-for-haskell-symbol-at-point-backward-new-color
                              (lambda () (bounds-of-thing-at-point 'haskell-symbol))
                              search/prev-impl
                              'backward
                              :regex-start-func search-for-haskell-symbol-at-point-regex-start-func
                              :regex-end-func search-for-haskell-symbol-at-point-regex-end-func
                              :error-message "No symbol at point")

;; Lispocentric searches
(search/make-search-for-thing search-for-symbol-at-point-forward
                              search-for-symbol-at-point-forward-new-color
                              (lambda () (bounds-of-thing-at-point 'symbol))
                              search/next-impl
                              'forward
                              :error-message "No symbol at point")

(search/make-search-for-thing search-for-symbol-at-point-backward
                              search-for-symbol-at-point-backward-new-color
                              (lambda () (bounds-of-thing-at-point 'symbol))
                              search/prev-impl
                              'backward
                              :error-message "No symbol at point")

;;;

(defsubst util:get-bounds-covered-by-vim-motion (motion)
  (let ((m (save-excursion
             (funcall motion))))
    (cons (vim:motion-begin-pos m)
          (vim:motion-end-pos m))))

(search/make-search-for-thing search-for-word-at-point-forward
                              search-for-word-at-point-forward-new-color
                              (lambda () (util:get-bounds-covered-by-vim-motion
                                     #'vim:motion-inner-word))
                              search/next-impl
                              'forward
                              :regex-start-func (constantly "\\<")
                              :regex-end-func (constantly "\\>")
                              :error-message "No word at point")

(search/make-search-for-thing search-for-word-at-point-backward
                              search-for-word-at-point-backward-new-color
                              (lambda ()
                                (util:get-bounds-covered-by-vim-motion
                                 #'vim:motion-inner-word))
                              search/prev-impl
                              'backward
                              :regex-start-func (constantly "\\<")
                              :regex-end-func (constantly "\\>")
                              :error-message "No word at point")

(add-to-list 'debug-ignored-errors "\\`No \\(?:symbol\\|word\\) at point\\'")


(provide 'search)

;; Local Variables:
;; End:

;; search.el ends here
