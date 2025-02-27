;; haskell-block-indent.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 30 March 2018
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)
(require 'current-column-fixed)
(require 'haskell-regexen)

;;;###autoload
(defun haskell-backspace-with-block-dedent (&optional count)
  (interactive "*p")
  (haskell-backspace-with-block-dedent--impl (or count 1) nil))

;;;###autoload
(defun haskell-space-with-block-indent (&optional count)
  "Insert space character and potentially indent to the right
haskell block at current indentation level."
  (interactive "*p")
  (haskell-space-with-block-indent--impl (or count 1) nil))

(defun haskell-backspace-with-block-dedent--impl (count apply-starting-from-exact?)
  (let ((col (current-column-fixed))
        (line (count-lines-fixed (point-min) (point))))
    (if (= 0 col)
        (pseudoparedit-backspace count)
      (cl-destructuring-bind (function-applied? . at-indentation?)
          (with-inhibited-modification-hooks
            (haskell--apply-to-block
             apply-starting-from-exact?
             (lambda (start)
               (goto-char start)
               (if (eobp)
                   (backward-delete-char count)
                 (delete-char count)))))
        (if function-applied?
            (progn
              (goto-line-dumb line)
              (if at-indentation?
                  (skip-to-indentation)
                (move-to-column (max 0 (- col count)))))
          (pseudoparedit-backspace count))))))

(defun haskell-space-with-block-indent--impl (count apply-starting-from-exact?)
  (let ((col (current-column-fixed))
        (line (count-lines-fixed (point-min) (point))))
    (cl-destructuring-bind
        (function-applied? . at-indentation?)
        (with-inhibited-modification-hooks
          (haskell--apply-to-block
           apply-starting-from-exact?
           (lambda (start)
             (goto-char start)
             (dotimes (_ count)
               (insert-char ?\s)))))
      (if function-applied?
          (progn
            (goto-line-dumb line)
            (if at-indentation?
                (skip-to-indentation)
              (move-to-column (+ col count))))
        (dotimes (_ count)
          (insert-char ?\s))))))

(defun haskell--apply-to-block (apply-starting-from-exact? f)
  "Call function F on start of every non-trivial line within
block at current indentation. Preprocessor lines and empty lines
as defined by ‘haskell-on-blank-line?’ will be ignored.

Returns t if operation commenced and nil otherwise."
  (let* ((start (line-beginning-position))
         (start-indent (current-column-fixed))
         (at-indentation-entry
          (save-excursion
            (skip-to-indentation)
            (let ((col (current-column-fixed-uncached)))
              (cons (<= start-indent col)
                    (= start-indent col)))))
         (at-indentation? (car at-indentation-entry))
         (at-exact-indentation? (cdr at-indentation-entry))
         (classify-current-line
          (lambda ()
            (save-excursion
              (beginning-of-line)
              (cond
                ((looking-at-p (format "^[ ]\\{,%d\\}where\\_>" start-indent))
                 nil ;; stop classification
                 )
                ((haskell-on-blank-line?)
                 'skip)
                ((let ((indent (progn
                                 (skip-indentation-forward)
                                 (current-column-fixed-uncached))))
                   (if apply-starting-from-exact?
                       (<= start-indent indent)
                     (< start-indent indent)))
                 'line-to-indent)
                (t
                 nil ;; stop classification
                 )))))
         (indentation-performed?
          (if at-indentation?
              (let* ((current-line-type nil)
                     (collected-lines (cons (cons 'line-to-indent start) nil))
                     (this-line-cons collected-lines))
                (with-marker (first-line-marker (copy-marker start))
                  (with-marker (last-line-marker (copy-marker start))
                    (when (/= start-indent 0)
                      (forward-line -1)
                      (while (and (not (bobp))
                                  (setf current-line-type
                                        (funcall classify-current-line)))
                        (beginning-of-line)
                        (setf this-line-cons
                              ;; Add to the end of list.
                              (setcdr-sure this-line-cons
                                           (cons (cons current-line-type (point))
                                                 nil)))
                        ;; (set-marker first-line-marker (point))
                        (forward-line -1))
                      (goto-char start))
                    (forward-line 1)
                    (while (and (not (eobp))
                                (setf current-line-type
                                      (funcall classify-current-line)))
                      (beginning-of-line)
                      ;; Add to the beginning of list.
                      (setf collected-lines (cons
                                             (cons current-line-type (point))
                                             collected-lines))
                      (forward-line 1))

                    ;; (message "collected-lines: %S"
                    ;;          (--map (progn (goto-char (cdr it)) (current-line))
                    ;;                 collected-lines))
                    ;; (message "collected-lines:")
                    ;; (dolist (x collected-lines)
                    ;;   (goto-char (cdr x))
                    ;;   (message "%s, %S" (car x) (buffer-substring-no-properties (point) (line-end-position))))

                    (dolist (x collected-lines)
                      (when (eq 'line-to-indent (car x))
                        (funcall f (cdr x))))
                    t)))
            nil)))
    (cons indentation-performed?
          at-exact-indentation?)))

(provide 'haskell-block-indent)

;; Local Variables:
;; End:

;; haskell-block-indent.el ends here
