;; advices-util.el --- -*- lexical-binding: nil; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'macro-util)

(defmacro make-synchronizing-advice (func
                                     adv-name
                                     lock-var
                                     acquire-pred
                                     acquire-action
                                     release-pred
                                     release-action)
  "Define advice ADV-NAME around FUNC that will surround calls to FUNC
with locking over LOCK-VAR"
  (let ((acquired-var '#:is-acquired))
    `(defadvice ,func (around ,adv-name activate compile)
       (let ((,acquired-var (and (not ,lock-var)
                                 ,acquire-pred)))
         (when ,acquired-var
           (setq ,lock-var t)
           ,acquire-action)

         ad-do-it

         (when (and ,acquired-var
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

;;;

(defvar make-advice-expand-on-search--lock nil)

(defmacro make-advice-expand-on-search (func expand-func &optional modes)
  "Define expand-on-search advice that will call EXPAND-FUNC after FUNC
returns in major modes from list MODES. Do nothing if MODES is empty.
Also perform synchronization such that no retursive calls of EXPAND-FUNC
will be possible."
  (let ((adv-name (string->symbol (concat (symbol->string func) "-expand-on-search")))
        (mode-list (-flatten
                    (util/eval-if-symbol modes))))
    (when (not (null? mode-list))
      `(make-light-synchronizing-advice
        ,func
        ,adv-name
        make-advice-expand-on-search--lock
        (memq major-mode ',mode-list)
        ,expand-func))))

;;;

(defmacro advices/auto-comment (func)
  "Define advice around FUNC that will insert comments at
beginning of line if previous line was commented out.

But in case of non-nil prefix-arg no comment will be inserted.

Intended to be used with comment-util-mode."
  `(defadvice ,func (around
                     ,(string->symbol (concat (symbol->string func) "-auto-comment"))
                     activate
                     compile)
     (let* ((comment-line-regexp (when *comment-util-current-format*
                                   (comment-format-line-regexp
                                    *comment-util-current-format*)))
            (enable-advice? (and (not current-prefix-arg)
                                 comment-line-regexp))

            (prev-line (when enable-advice?
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (point)))
                       ;; (current-line)
                       ))
       ad-do-it
       (when enable-advice?
         (save-match-data
           (when (string-match (concat "\\`\\(\\s-*"
                                       comment-line-regexp
                                       "\\)")
                               prev-line)
             (skip-to-indentation)
             (delete-region (line-beginning-position)
                            (point))
             (insert (concat (match-string 1 prev-line)
                             (make-string *comment-util-space-count*
                                          ?\s)))))))))

(provide 'advices-util)

;; Local Variables:
;; End:

;; advices-util.el ends here
