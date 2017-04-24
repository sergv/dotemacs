;; haskell-smart-operators-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 20 February 2017
;; Description:
;; Bind characters for operators that will automatically maintain whitespace
;; around themselves.
;;
;; Special thanks: Chris Done's magnificent `structured-haskell-mode', which
;; inspired this work.

(require 'haskell-completions)
(require 'haskell-ghc-support)

(defvar haskell-smart-operators--operator-chars
  (let ((tbl (make-hash-table :test #'eq)))
    (loop
      for c across "!#$%&*+-./:<=>?@\\^|~"
      do (puthash c t tbl))
    tbl)
  "Characters that may constitute operators.")

(defun haskell-smart-operators--insert (str)
  (insert str))

(defun haskell-smart-operators--in-string-or-comment? ()
  "Are we in string or comment?"
  (let* ((state (parse-partial-sexp (line-beginning-position)
                                    (point)))
         (inside-string? (elt state 3))
         (inside-comment? (elt state 4)))
    (or inside-comment?
        inside-string?
        (and (eq 'font-lock-string-face
                 (get-text-property (point) 'face))
             (if (and (char-equal (char-after) ?\")
                      (/= (point-min) (point)))
                 (eq 'font-lock-string-face
                     (get-text-property (- (point) 1) 'face))
               t)))))

(defun haskell-smart-operators--literal-insertion? ()
  "Should a node have literal insertion?"
  (let ((before (char-before))
        (after (char-after)))
    (or (haskell-smart-operators--in-string-or-comment?)
        (and before
             after
             (or
              ;; Test positions: '_|_', \\_|_'
              (and (memq before '(?\' ?\\))
                   (char-equal after ?\'))
              ;; Test positions: "_|_", \\_|_"
              (and (memq before '(?\" ?\\))
                   (char-equal after ?\")))))))

(defun haskell-smart-operators--insert-char-appending-to-prev-operator (char)
  "Insert CHAR while optionally removing whitespace to the previous character
if it's operator character and CHAR is operator character too.

Similar to `shm-insert-string'."
  (when (and (gethash char haskell-smart-operators--operator-chars)
             ;; (not (haskell-smart-operators--literal-insertion?))
             )
    ;; Delete spaces backwards if there's operator or open paren char
    ;; somewhere.
    (let ((delete-whitespace?
           (save-excursion
             (skip-syntax-backward " ")
             (let ((before (char-before (point))))
               (and (not (bobp))
                    (or (gethash before haskell-smart-operators--operator-chars)
                        (char-equal before ?\())
                    (if (char-equal before ?|)
                        ;; (not (string-match-p "^[ \t]*|"
                        ;;                      (buffer-substring-no-properties
                        ;;                       (line-beginning-position)
                        ;;                       (- (point) 1))))
                        (save-excursion
                          (goto-char (line-beginning-position))
                          (not (looking-at-p "^[ \t]*|")))
                      t))))))
      (when delete-whitespace?
        (while (and (not (bobp))
                    (char-equal ?\s (char-syntax (char-before)))
                    (not (get-char-property (1- (point)) 'read-only)))
          (delete-char -1)))))
  (haskell-smart-operators--insert (make-string 1 char)))

(defun haskell-smart-operators--on-empty-string? ()
  (string-match-p "^[ \t]*$"
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (point))))

(defun haskell-smart-operators--on-a-line-with-guard? ()
  (save-excursion
    ;; Old version...
    ;; (not (string-match-p "^[ \t]*|"
    ;;                      (buffer-substring-no-properties
    ;;                       (line-beginning-position)
    ;;                       (- (point) 1))))
    (beginning-of-line)
    (looking-at-p "^[ \t]+|")))

(defun haskell-smart-operators--insert-char-surrounding-with-spaces (char)
  "Insert CHARacter while trying to surround it with spaces and
stick it to the previous operator on line."
  (if (or current-prefix-arg
          (haskell-smart-operators--literal-insertion?)
          (not (gethash char haskell-smart-operators--operator-chars)))
      (insert (make-string 1 char))
    (let ((before (char-before)))
      ;; Decide whether to insert space before the operator.
      (if (and (not (haskell-smart-operators--on-empty-string?))
               (or (not before) ;; at beginning of buffer
                   ;; Distance ourselves from | that is a potential guard.
                   (and (char-equal before ?|)
                        (haskell-smart-operators--on-a-line-with-guard?))
                   (and (not (char-equal before ?\s))
                        (not (char-equal before ?\())
                        ;; Do not split [| pair when we're inserting the |.
                        (not (and (char-equal char ?|)
                                  (char-equal before ?\[)))
                        (not (gethash before haskell-smart-operators--operator-chars)))))
          (haskell-smart-operators--insert " ")
        ;; Delete spaces backwards if there's operator or open paren char
        ;; before the spaces.
        (let ((delete-whitespace?
               (save-excursion
                 (skip-syntax-backward " ")
                 (let ((char-before-spaces (char-before (point))))
                   (and char-before-spaces ;; not at beginning of buffer
                        (or (gethash char-before-spaces haskell-smart-operators--operator-chars)
                            (char-equal char-before-spaces ?\())
                        (if (char-equal char-before-spaces ?|)
                            ;; Check that it's not a guard.
                            (not (haskell-smart-operators--on-a-line-with-guard?))
                          t))))))
          (when delete-whitespace?
            (while (and (not (bobp))
                        (char-equal ?\s (char-syntax (char-before)))
                        (not (get-char-property (1- (point)) 'read-only)))
              (delete-char -1)))))
      ;; Insert operator char.
      ;; (haskell-smart-operators--insert-char-appending-to-prev-operator char)
      (haskell-smart-operators--insert (make-string 1 char))
      ;; Decide whether to insert space after the operator.
      (when (not (and (char-equal char ?\\)
                      before ;; not at beginning of buffer
                      (char-equal before ?\()))
        (let ((after (char-after)))
          (when (or (not after) ;; at end of buffer
                    ;; If the next thing is lambda then we don't want to merge
                    ;; with it's \.
                    (char-equal after ?\\)
                    (and (not (char-equal after ?\s))
                         (not (char-equal after ?\)))
                         ;; Do not split |] pair when we're inserting the |.
                         (not (and (char-equal char ?|)
                                   (char-equal after ?\])))
                         (not (gethash after haskell-smart-operators--operator-chars))))
            (haskell-smart-operators--insert " ")))))))

;;;###autoload
(defun haskell-smart-operators-self-insert (arg)
  "Insert charater and take care to surround it with spaces."
  (interactive "p")
  (unless (characterp last-command-event)
    (error "Last event is not a character: %s" last-command-event))
  (haskell-smart-operators--insert-char-surrounding-with-spaces
   last-command-event))

;;;###autoload
(defun haskell-smart-operators-$ ()
  "Swap parens with a dollar."
  (interactive)
  (let ((start-pos nil))
    (when (save-excursion
            (skip-syntax-forward " ")
            (let ((next-char (char-after)))
              (when (and next-char
                         (char= next-char ?\())
                (setf start-pos (point))
                t)))
      (goto-char start-pos)
      ;; delete parenthesized sexp
      (save-excursion
        (forward-sexp)
        (delete-char -1))
      (delete-char 1))
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?\$)))

(defun haskell-smart-operators--point-surrounded-by2 (before2 before1 after1 after2)
  "Check if previous 2 characters before point are BEFORE2 and BEFORE1 and
that next 2 characters are AFTER1 and AFTER2."
  (let* ((pt-before    (save-excursion
                         (skip-syntax-backward " ")
                         (point)))
         (pt-after     (save-excursion
                         (skip-syntax-forward " ")
                         (point)))
         (real-before2 (char-before (- pt-before 1)))
         (real-before1 (char-before pt-before))
         (real-after1  (char-after pt-after))
         (real-after2  (char-after (+ pt-after 1))))
    (list
     pt-before
     pt-after
     (and real-before2
          real-before1
          real-after1
          real-after2
          (char-equal real-before2 before2)
          (char-equal real-before1 before1)
          (char-equal real-after1  after1)
          (char-equal real-after2  after2)))))

(defun haskell-smart-operators--point-surrounded-by (before after)
  "Check if point is surrounded by BEFORE and AFTER characters."
  (let* ((pt-before   (save-excursion
                        (skip-syntax-backward " ")
                        (point)))
         (pt-after    (save-excursion
                        (skip-syntax-forward " ")
                        (point)))
         (real-before (char-before pt-before))
         (real-after  (char-after pt-after)))
    (list
     pt-before
     pt-after
     (and real-before
          real-after
          (char-equal real-before before)
          (char-equal real-after after)))))

;;;###autoload
(defun haskell-smart-operators-hyphen ()
  "Insert hyphen. Expand into {- _|_ -} if inside { *}."
  (interactive)
  (destructuring-bind
      (pt-before pt-after is-surrounded?)
      (haskell-smart-operators--point-surrounded-by ?\{ ?\})
    (if is-surrounded?
        (progn
          (delete-region pt-before pt-after)
          (insert "-  -")
          (forward-char -2))
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-))))

;;;###autoload
(defun haskell-smart-operators-comma ()
  "Insert comma followed by space."
  (interactive)
  (insert ", "))

;;;###autoload
(defun haskell-smart-operators-hash ()
  "Smart insertion of #."
  (interactive)
  (destructuring-bind
      (pt-pragma-start pt-pragma-end is-surrounded-for-pragma?)
      (haskell-smart-operators--point-surrounded-by2 ?\{ ?- ?- ?\})
    (destructuring-bind
        (pt-c2hs-start pt-c2hs-end is-surrounded-for-c2hs?)
        (haskell-smart-operators--point-surrounded-by ?\{ ?\})
      (cond (is-surrounded-for-pragma?
             (delete-region pt-pragma-start pt-pragma-end)
             (insert "#  #")
             (forward-char -2)
             (let ((pragma (ido-completing-read "Pragma: "
                                                haskell-completions--pragma-names)))
               (insert pragma " ")
               (when (string= pragma "LANGUAGE")
                 (insert
                  (ido-completing-read
                   "Language: "
                   haskell-ghc-supported-extensions)))))
            ;; for c2hs
            (is-surrounded-for-c2hs?
             (delete-region pt-c2hs-start pt-c2hs-end)
             (insert "##")
             (forward-char -1))
            ;; Don't surround with #includes, #lang/#opts abbrevs, etc with spaces
            ((= (point)
                (line-beginning-position))
             (insert "#"))
            (t
             (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))))))

(defvar haskell-smart-operators-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dolist (key (list (kbd "=") (kbd "+") (kbd "*") (kbd "<") (kbd ">")
                       (kbd "!") (kbd "%") (kbd "^") (kbd "&") (kbd "/")
                       (kbd "?") (kbd "|") (kbd "~")
                       ;; Can't detectect whether it's in pattern and thus less useful than `shm/@'.
                       (kbd "@")
                       ;; Less powerful than `shm/:'.
                       (kbd ":")))
      (define-key keymap key #'haskell-smart-operators-self-insert))
    (define-key keymap (kbd "-") #'haskell-smart-operators-hyphen)
    (define-key keymap (kbd "#") #'haskell-smart-operators-hash)
    (define-key keymap (kbd ",") #'haskell-smart-operators-comma)
    (define-key keymap (kbd "$") #'haskell-smart-operators-$)
    keymap))

;;;###autoload
(define-minor-mode haskell-smart-operators-mode
  "Toggle haskell-smart-operators-mode."
  nil ;; init
  nil ;; modeline
  nil ;; keymap
  :global nil
  (message (if haskell-smart-operators-mode
               "disabled"
             "enabled")))

(provide 'haskell-smart-operators-mode)

;; Local Variables:
;; End:

;; haskell-smart-operators-mode.el ends here
