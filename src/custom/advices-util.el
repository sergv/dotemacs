;; advices-util.el --- -*- lexical-binding: nil; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(require 'macro-util)

;; (defmacro util:update-alist-entry (alist key entry)
  ;; `(setq ,alist
         ;; (cons ,entry
               ;; (delq (assoc ,key ,alist)
                     ;; ,alist))))

(defmacro make-synchronizing-advice (func
                                     adv-name
                                     lock-var
                                     acquire-pred
                                     acquire-action
                                     release-pred
                                     release-action)
  "Define advice ADV-NAME around FUNC that will surround calls to FUNC
with locking over LOCK-VAR"
  (let ((acquire (gensym)))

    `(defadvice ,func (around ,adv-name activate compile)
       (let ((,acquire (and (not ,lock-var)
                            ,acquire-pred)))
         (when ,acquire
           (setq ,lock-var t)
           ,acquire-action)

         ad-do-it

         (when (and ,acquire
                    ,release-pred)
           (setq ,lock-var nil)
           ,release-action)))))

(defmacro make-light-synchronizing-advice (func adv-name lock-var acquire-pred release-action)
  `(make-synchronizing-advice
    ,func
    ,adv-name
    ,lock-var
    ,acquire-pred
    nil
    t
    ,release-action))

;;;;------------------------------------------------------------

(defun util:reindent-region (start end)
  "Custom function that reindents region linewise,
differs from indent-region with silent behavior( i.e. no messages)
and possibly more rude behavior"
  (save-excursion
    (let ((m (set-marker (make-marker) end)))
      (indent-for-tab-command)
      (goto-char start)
      (while (< (point) (marker-position m))
        (beginning-of-line)
        (indent-for-tab-command)
        (forward-line 1)))))

(defvar *util:lisp:indent-advice-lock* nil
  "This variable becomes t whenever there's one of *-indent-advice's
\(for `vim:cmd-paste-behind' `vim:cmd-paste-before' `vim:cmd-paste-pop'
     and `vim:cmd-paste-pop-next'\) operates on current buffer")

(defmacro defadvice:indent-after-yank (func &optional modes)
  "Define indent-after-yank advice around FUNC that will
reindent text after yanking in major modes from list MODES.
Do nothing if MODES is empty."
  (let ((adv-name (make-joined-name func "-indent-advice"))
        (mode-list (util:flatten
                    (util:eval-if-symbol modes))))
    (if-list-nonempty mode-list
      `(make-light-synchronizing-advice
        ,func
        ,adv-name
        *util:lisp:indent-advice-lock*
        (memq major-mode ',mode-list)
        (util:reindent-region (vim:paste-info-begin vim:last-paste)
                              (vim:paste-info-end   vim:last-paste))))))

;;;;------------------------------------------------------------

(defvar *util:expand-on-search-advice-lock* nil
  "This variable becomes t whenever one of advices defined
via `defadvice:expand-on-search' becomes active")

(defmacro defadvice:expand-on-search (func expand-func &optional modes)
  "Define expand-on-search advice that will call EXPAND-FUNC after FUNC
returns in major modes from list MODES. Do nothing if MODES is empty.
Also perform synchronization such that no retursive calls of EXPAND-FUNC
will be possible."
  (let ((adv-name (make-joined-name func "-expand-on-search"))
        (mode-list (util:flatten
                    (util:eval-if-symbol modes))))
    (if-list-nonempty mode-list
      `(make-light-synchronizing-advice
        ,func
        ,adv-name
        *util:expand-on-search-advice-lock*
        (memq major-mode ',mode-list)
        ,expand-func))))

;;;;------------------------------------------------------------

;; don't used anywhere
;; (defmacro defadvice:icicle-on-sole-completion (func)
  ;; `(defadvice ,func (around
                     ;; ,(make-joined-name func "-on-sole-completion")
                     ;; activate
                     ;; compile)
     ;; (let ((icicle-top-level-when-sole-completion-flag nil))
       ;; ad-do-it)))

(defmacro defadvice:icicle-do-not-insert-default-value (func)
  `(defadvice ,func (around
                     ,(make-joined-name func "-do-not-insert-default-value")
                     activate
                     compile)
     (let ((icicle-default-value nil))
       ad-do-it)))

;;;;------------------------------------------------------------

(defmacro defadvice:auto-comment (func)
  "Define advice around FUNC that will insert comments at
beginning of line whenever previous line was commented out.
But if prefix-arg is not nil then comment would not be inserted.

Intended to be used with comment-util-mode."
  `(defadvice ,func (around
                     ,(make-joined-name func "-auto-comment")
                     activate
                     compile)
     (let ((prev-line (buffer-substring-no-properties
                       (line-beginning-position)
                       (point))
                      ;; (current-line)
                      )
           (comment-line-regexp (when *comment-util-current-format*
                                  (comment-format-line-regexp
                                   *comment-util-current-format*))))
       ad-do-it
       (unless current-prefix-arg
         (save-match-data
           (when (and comment-line-regexp
                      (string-match? (concat "^\\(\\s-*\\(?:"
                                             comment-line-regexp
                                             "\\)\\)")
                                     prev-line))
             (skip-to-indentation)
             (delete-region (line-beginning-position)
                            (point))
             (insert (concat (match-string 1 prev-line)
                             (make-string *comment-util-space-count*
                                          ?\s)))))))))

;;;;------------------------------------------------------------

(defmacro defadvice:remember-position-on-query (func)
  "Make vim-like remembering position when query is made
to find Haskell function."
  `(defadvice ,func (around
                     ,(make-joined-name func
                                        "-remember-prev-pos")
                     activate
                     compile)
     (let ((buf (current-buffer))
           (pos (point)))
       ad-do-it
       (when (eq buf
                 (current-buffer))
         ;; save into ` register
         (vim:save-position pos)))))

(provide 'advices-util)

;; Local Variables:
;; End:

;; advices-util.el ends here
