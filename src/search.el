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

(defparameter *search-current-regexp* nil
  "Regexp being searched now.")
(defparameter *search-start-marker* nil
  "Marker which points to location from which search was initiated.")
(defparameter *search-match-overlays* nil
  "List of overlays which highlight matches for regexp being searched for.")

(defface search-highlight-face '((t (:inherit lazy-highlight)))
  "Face to highlight matches for regexp being searched for.")

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
      ("S-<up>"    search-next-from-minibuf)
      ("S-<down>"  search-prev-from-minibuf)

      ("<next>"    search-next-from-minibuf)
      ("<prior>"   search-prev-from-minibuf)

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

(defun* search-setup-search-for (regex
                                 direction
                                 &key
                                 (save-position t)
                                 (case-sensetive t))
  "Set up internal search variables for use of `search-next-impl',
`search-prev-impl' etc for REGEX."
  (search-clean-overlays)
  (setf *search-current-regexp* regex
        *search-start-marker* (point-marker)
        *search-init-buffer* (current-buffer)
        *search-init-window* (selected-window)
        *search-direction* direction
        *search-case-sensetive* case-sensetive)
  (when save-position
    (vim:save-position))
  (when regex
    (search-highlight-matches regex)))


(defun search-start-forward (&optional case-sensetive)
  "Initiate forward seach."
  (interactive (list current-prefix-arg))
  (search-start 'forward "Search: " case-sensetive))

(defun search-start-backward (&optional case-sensetive)
  "Initiate backward seach."
  (interactive (list current-prefix-arg))
  (search-start 'backward "Search backward: " case-sensetive))


(defun search-start (direction prompt case-sensetive)
  "Initiate search. Most of the work will be done in minibuffer."
  (search-setup-search-for nil direction :case-sensetive case-sensetive)

  (add-hook 'minibuffer-setup-hook #'search-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'search-minibuffer-exit)

  (read-from-minibuffer prompt
                        nil
                        *search-minibuffer-keymap*
                        nil
                        ;; 'regexp-history
                        '*search-minibuffer-history*))



(defparameter *search-minibuffer* nil
  "Becomes set to current minibuffer with current prompt for regexp.
When not prompting in minibuffer then this variable is set to nil.")


(defun search-minibuffer-setup ()
  (remove-hook 'minibuffer-setup-hook #'search-minibuffer-setup)
  (setf *search-minibuffer* (current-buffer))
  (add-hook 'after-change-functions #'search-update t t))

(defun search-minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook #'search-minibuffer-exit)
  (remove-hook 'after-change-functions #'search-update t)

  (setf *search-minibuffer* nil))

(defun search-regex-valid-p (regex)
  (not (string-match-p "\\\\$" regex)))

(defun search-update (start end old-len)
  (setf *search-current-regexp* (search-get-current-regexp))
  (condition-case nil
      (when (search-regex-valid-p *search-current-regexp*)
        (search-highlight-matches *search-current-regexp*)
        ;; this forgets current locatin and jums to first match of
        ;; updated regexp
        (search-with-initiated-buffer
         (goto-char *search-start-marker*)
         (search-next-from-minibuf)))
    ;; swallow an error or we'll be kicked out of the hook
    (error nil)))


(defun search-get-current-regexp ()
  "Retrieve entered regexp from minibuffer."
  (search-with-prompt-buffer
   (minibuffer-contents-no-properties)))


(defmacro search-with-initiated-buffer (&rest body)
  `(with-selected-window *search-init-window*
     (with-current-buffer *search-init-buffer*
       ,@body)))

(defmacro search-with-prompt-buffer (&rest body)
  `(with-current-buffer *search-minibuffer*
     ,@body))

(defun search-next-from-minibuf ()
  (interactive)
  (search-with-initiated-buffer
   (search-search-in-direction *search-direction*)))

(defun search-prev-from-minibuf ()
  (interactive)
  (search-with-initiated-buffer
   (search-search-in-direction *search-direction* :reversed t)))


(defun search-optionally-refresh-info ()
  "Refresh search information if necessary and omit cluttering vim position."
  ;; we could be in buffer that is different from where we initiated search,
  ;; in which case regexp being searched for and search direction from the
  ;; previous search are used to set up new search session in the current
  ;; buffer
  (unless (eq? (current-buffer) *search-init-buffer*)
    (search-setup-search-for *search-current-regexp*
                             *search-direction*
                             :save-position nil
                             :case-sensetive *search-case-sensetive*)))

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
                (search-prev-impl)
                (search-next-impl)))
    (backward (if reversed
                (search-next-impl)
                (search-prev-impl)))))

(defun search-next-impl ()
  "Move to the next match for `*search-current-regexp*'."
  (unless (member* *search-current-regexp*
                   *search-ignore-regexps* :test #'string=)
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

(defun search-prev-impl ()
  "Move to the previous match for `*search-current-regexp*'."
  (unless (member* *search-current-regexp* *search-ignore-regexps* :test #'string=)
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
  '("\\(.*\\)*" "\\(.+\\)*" "\\(.*\\)+" "\\(.+\\)+"
    "\\(?:.*\\)*" "\\(?:.+\\)*" "\\(?:.*\\)+" "\\(?:.+\\)+")
  "Regexps for which neither highlighting nor searching should occur.")


(defun search-highlight-matches (regexp)
  (search-with-initiated-buffer
   (search-clean-overlays)
   (when (and (<= 1 (length regexp))
              (not (member* regexp *search-ignore-regexps* :test #'string=)))
     (save-excursion
       (goto-char (point-min))
       (let ((i 0)
             (case-fold-search (not *search-case-sensetive*)))
         (while (and (< i *search-highlight-limit*)
                     (re-search-forward regexp nil t))
           (let* ((text-length (- (match-end 0) (match-beginning 0)))
                  (overlay (when (< text-length *search-maximum-highlight-length*)
                             (make-overlay (match-beginning 0)
                                           (match-end 0)))))
             (overlay-put overlay 'face 'search-highlight-face)
             (push overlay *search-match-overlays*)
             (incf i))))))))

(defun search-clean-overlays ()
  (save-excursion
    (dolist (o *search-match-overlays*)
      (delete-overlay o))
    (setf *search-match-overlays* nil)))


(defun search-return-to-start ()
  "Jump to place where current search session was started."
  (interactive)
  (search-with-initiated-buffer
   (goto-char *search-start-marker*)))


(defun search-abort ()
  (interactive)
  (search-with-initiated-buffer
   (goto-char *search-start-marker*)
   (search-clean-overlays))
  (exit-minibuffer))

(defun search-done ()
  (interactive)
  (exit-minibuffer))


(defun search-toggle-highlighting ()
  (interactive)
  (when *search-match-overlays*
    (let ((enabled (overlay-get (car *search-match-overlays*) 'face)))
      (if enabled
        (dolist (overlay *search-match-overlays*)
          (overlay-put overlay 'face nil))
        (dolist (overlay *search-match-overlays*)
          (overlay-put overlay 'face 'search-highlight-face))))))

;;;

(defmacro search-def-autoexpand-advices (expand-command
                                         modes)
  `(progn
     ,@(loop
         for command in '(search-next-from-minibuf
                          search-prev-from-minibuf
                          search-next
                          search-prev
                          search-for-symbol-at-point-forward
                          search-for-symbol-at-point-backward
                          search-for-slime-symbol-at-point-forward
                          search-for-slime-symbol-at-point-backward
                          search-for-word-at-point-forward
                          search-for-word-at-point-backward)
         collect `(defadvice:expand-on-search ,command ,expand-command ,modes))))

;;;

(defmacro* search-make-search-for-thing (name
                                         bounds-func
                                         action-after
                                         direction
                                         &key
                                         (regex-start-func (constantly "\\_<"))
                                         (regex-end-func   (constantly "\\_>"))
                                         (error-message nil))
  "BOUNDS-FUNC should return cons pair (START . END), everything else is
obvious"
  (let ((bounds-var (gensym "bounds"))
        (substr-var (gensym "substr"))
        (non-strict-var (gensym "non-strict")))
    `(defun ,name (&optional ,non-strict-var)
       (interactive (list current-prefix-arg))
       (let ((,bounds-var (funcall ,bounds-func)))
         (if (null ,bounds-var)
           ,(when error-message `(error ,error-message))
           (let ((,substr-var (buffer-substring-no-properties (car ,bounds-var)
                                                              (cdr ,bounds-var))))
             (vim:save-position)
             (goto-char (cdr ,bounds-var))
             (search-setup-search-for
              (concat (unless ,non-strict-var
                        (funcall ,regex-start-func ,substr-var))
                      (regexp-quote ,substr-var)
                      (unless ,non-strict-var
                        (funcall ,regex-end-func ,substr-var)))
              ,direction
              :case-sensetive t)
             (funcall ,action-after)))))))

(autoload 'forward-haskell-symbol "haskell-misc" t)

;; Haskell search

(let* ((forward-re "^[a-zA-Z0-9]")
       (backward-re "[a-zA-Z0-9]$")
       (regex-start-func (lambda (pat)
                           (if (string-match-pure? forward-re pat)
                             ;; Don't use \\_<,\\_> since they rely on
                             ;; syntax table which was tampered with in haskell
                             ;; mode so that e.g. regexp "\\_<Node" won't match
                             ;; the input "x:Node (x - 1)".
                             "\\<"
                             "")))
       (regex-end-func (lambda (pat)
                         (if (string-match-pure? backward-re pat)
                           "\\>"
                           ""))))
  (search-make-search-for-thing search-for-haskell-symbol-at-point-forward
                                (lambda () (bounds-of-thing-at-point 'haskell-symbol))
                                #'search-next-impl
                                'forward
                                :regex-start-func regex-start-func
                                :regex-end-func regex-end-func
                                :error-message "No symbol at point")
  (search-make-search-for-thing search-for-haskell-symbol-at-point-backward
                                (lambda () (bounds-of-thing-at-point 'haskell-symbol))
                                #'search-prev-impl
                                'backward
                                :regex-start-func regex-start-func
                                :regex-end-func regex-end-func
                                :error-message "No symbol at point"))

;; Lispocentric searches
(search-make-search-for-thing search-for-symbol-at-point-forward
                              (lambda () (bounds-of-thing-at-point 'symbol))
                              #'search-next-impl
                              'forward
                              :error-message "No symbol at point")

(search-make-search-for-thing search-for-symbol-at-point-backward
                              (lambda () (bounds-of-thing-at-point 'symbol))
                              #'search-prev-impl
                              'backward
                              :error-message "No symbol at point")

(search-make-search-for-thing search-for-slime-symbol-at-point-forward
                              #'slime-bounds-of-symbol-at-point
                              #'search-next-impl
                              'forward
                              :error-message "No symbol at point")

(search-make-search-for-thing search-for-slime-symbol-at-point-backward
                              #'slime-bounds-of-symbol-at-point
                              #'search-prev-impl
                              'backward
                              :error-message "No symbol at point")

;;;

(defsubst util:get-bounds-covered-by-vim-motion (motion)
  (let ((m (save-excursion
             (funcall motion))))
    (cons (vim:motion-begin-pos m)
          (vim:motion-end-pos m))))

(search-make-search-for-thing search-for-word-at-point-forward
                              (lambda () (util:get-bounds-covered-by-vim-motion
                                          #'vim:motion-inner-word))
                              #'search-next-impl
                              'forward
                              :regex-start-func (constantly "\\<")
                              :regex-end-func (constantly "\\>")
                              :error-message "No word at point")

(search-make-search-for-thing search-for-word-at-point-backward
                              (lambda ()
                                (util:get-bounds-covered-by-vim-motion
                                 #'vim:motion-inner-word))
                              #'search-prev-impl
                              'backward
                              :regex-start-func (constantly "\\<")
                              :regex-end-func (constantly "\\>")
                              :error-message "No word at point")

(add-to-list 'debug-ignored-errors "\\`No \\(?:symbol\\|word\\) at point\\'")


(provide 'search)

;; Local Variables:
;; End:

;; search.el ends here
