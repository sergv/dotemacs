;;; el-swank-fuzzy.el --- fuzzy symbol completion.

;; Copyright (C) 2009 Brian Downing <bdowning@lavos.net>
;; Copyright (C) 2009 Tobias C. Rittweiler <tcr@freebits.de>
;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Original CL implementation authors (from swank-fuzzy.lisp) below,
;; Original Authors: Brian Downing <bdowning@lavos.net>
;;                   Tobias C. Rittweiler <tcr@freebits.de>
;;                   and others
;; Author: Takeshi Banse <takebi@laafc.net>
;; keywords: matching, completion, string

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This program is based on the swank-fuzzy.lisp.
;; Thanks the CL implementation authors for that useful software.

;; I love the SLIME/swank's FUZZY-COMPLETIONS. It provides fairly sane
;; completion results neary invariably. So I translated the original CL
;; implementation into Emacs Lisp. Thankfully, the core part of the scoring
;; algorithm in this package is almost identical that in the CL implementation.
;; (Thanks Emacs Lisp cl package!). Visit the SLIME site
;; http://common-lisp.net/project/slime/

;; This file provides two functions,
;; `el-swank-fuzzy-completions' and `el-swank-fuzzy-matches',
;; which you can use for the SLIME/swank's FUZZY-COMPLETIONS alike completion
;; function for Emacs Lisp.

;;; Code:

(require 'cl)

(defvar el-swank-fuzzy-recursion-soft-limit 30
  "This is a soft limit for recursion in
RECURSIVELY-COMPUTE-MOST-COMPLETIONS.  Without this limit,
completing a string such as \"ZZZZZZ\" with a symbol named
\"ZZZZZZZZZZZZZZZZZZZZZZZ\" will result in explosive recursion to
find all the ways it can match.

Most natural language searches and symbols do not have this
problem -- this is only here as a safeguard.")
(declaim (fixnum el-swank-fuzzy-recursion-soft-limit))

(defsubst swfy-compute-highest-scoring-completion (short full)
  "Finds the highest scoring way to complete the abbreviation
SHORT onto the string FULL, using CHAR= as a equality function for
letters.  Returns two values:  The first being the completion
chunks of the highest scorer, and the second being the score."
  (let* ((scored-results
          (mapcar #'(lambda (result)
                      (cons (swfy-score-completion result short full) result))
                  (swfy-compute-most-completions short full)))
         (winner (first (sort* scored-results #'> :key #'caar))))
    (values (rest winner) (caar winner))))

(defsubst swfy-compute-most-completions (short full)
  "Finds most possible ways to complete FULL with the letters in SHORT.
Calls RECURSIVELY-COMPUTE-MOST-COMPLETIONS recursively.  Returns
a list of (&rest CHUNKS), where each CHUNKS is a description of
how a completion matches."
  (let ((*all-chunks* nil))
    (declare (special *all-chunks*))
    (swfy-recursively-compute-most-completions short full 0 0 nil nil nil t)
    *all-chunks*))

(defun swfy-recursively-compute-most-completions
    (short full
     short-index initial-full-index
     chunks current-chunk current-chunk-pos
     recurse-p)
  "Recursively (if RECURSE-P is true) find /most/ possible ways
to fuzzily map the letters in SHORT onto FULL, using CHAR= to
determine if two letters match.

A chunk is a list of elements that have matched consecutively.
When consecutive matches stop, it is coerced into a string,
paired with the starting position of the chunk, and pushed onto
CHUNKS.

Whenever a letter matches, if RECURSE-P is true,
RECURSIVELY-COMPUTE-MOST-COMPLETIONS calls itself with a position
one index ahead, to find other possibly higher scoring
possibilities.  If there are less than
*FUZZY-RECURSION-SOFT-LIMIT* results in *ALL-CHUNKS* currently,
this call will also recurse.

Once a word has been completely matched, the chunks are pushed
onto the special variable *ALL-CHUNKS* and the function returns."
  (declare ;(optimize speed)
   (fixnum short-index initial-full-index)
   (simple-string short full)
   (special *all-chunks*))
  (flet ((char= (c1 c2) (eq c1 c2))
         (short-cur ()
           "Returns the next letter from the abbreviation, or NIL
            if all have been used."
           (if (= short-index (length short))
               nil
             (aref short short-index)))
         (add-to-chunk (char pos)
           "Adds the CHAR at POS in FULL to the current chunk,
            marking the start position if it is empty."
           (unless current-chunk
             (setf current-chunk-pos pos))
           (push char current-chunk))
         (collect-chunk ()
           "Collects the current chunk to CHUNKS and prepares for
            a new chunk."
           (when current-chunk
             (push (list current-chunk-pos
                         (coerce (reverse current-chunk) 'string)) chunks)
             (setf current-chunk nil
                   current-chunk-pos nil))))
    ;; If there's an outstanding chunk coming in collect it.  Since
    ;; we're recursively called on skipping an input character, the
    ;; chunk can't possibly continue on.
    (when current-chunk (collect-chunk))
    (do ((pos initial-full-index (1+ pos)))
        ((= pos (length full)))
      (let ((cur-char (aref full pos)))
        (if (and (short-cur)
                 (char= cur-char (short-cur)))
            (progn
              (when recurse-p
                ;; Try other possibilities, limiting insanely deep
                ;; recursion somewhat.
                (swfy-recursively-compute-most-completions
                 short full short-index (1+ pos)
                 chunks current-chunk current-chunk-pos
                 (not (> (length *all-chunks*)
                         el-swank-fuzzy-recursion-soft-limit))))
              (incf short-index)
              (add-to-chunk cur-char pos))
          (collect-chunk))))
    (collect-chunk)
    ;; If we've exhausted the short characters we have a match.
    (if (short-cur)
        nil
      (let ((rev-chunks (reverse chunks)))
        (push rev-chunks *all-chunks*)
        rev-chunks))))

;;;;; Fuzzy completion scoring

(defvar-loc el-swank-fuzzy-completion-symbol-prefixes "*+-%&?<"
  "Letters that are likely to be at the beginning of a symbol.
Letters found after one of these prefixes will be scored as if
they were at the beginning of ths symbol.")
(defvar-loc el-swank-fuzzy-completion-symbol-suffixes "*+->"
  "Letters that are likely to be at the end of a symbol.
Letters found before one of these suffixes will be scored as if
they were at the end of the symbol.")
(defvar-loc el-swank-fuzzy-completion-word-separators "-/."
  "Letters that separate different words in symbols.  Letters
after one of these symbols will be scores more highly than other
letters.")

(defun el-swank-set-completion-syntax (syntax)
  "Set `el-swank-fuzzy-completion-symbol-prefixes',
`el-swank-fuzzy-completion-symbol-suffixes' and
`el-swank-fuzzy-completion-word-separators' depending on value
of SYNTAX."
  (case syntax
    (lisp
     (setf
      el-swank-fuzzy-completion-symbol-prefixes "*+-%&<"
      el-swank-fuzzy-completion-symbol-suffixes "*+->?p"
      el-swank-fuzzy-completion-word-separators "-/. "))
    (text
     (setf
      el-swank-fuzzy-completion-symbol-prefixes "*+-<"
      el-swank-fuzzy-completion-symbol-suffixes "*+->"
      el-swank-fuzzy-completion-word-separators "-/. "))
    (t ;; the default value
     (setf
      el-swank-fuzzy-completion-symbol-prefixes "*+-%&?<"
      el-swank-fuzzy-completion-symbol-suffixes "*+->"
      el-swank-fuzzy-completion-word-separators "-/."))))



(defun swfy-score-completion (completion short full)
  "Scores the completion chunks COMPLETION as a completion from
the abbreviation SHORT to the full string FULL.  COMPLETION is a
list like:
    ((0 \"mul\") (9 \"v\") (15 \"b\"))
Which, if SHORT were \"mulvb\" and full were \"multiple-value-bind\",
would indicate that it completed as such (completed letters
capitalized):
    MULtiple-Value-Bind

Letters are given scores based on their position in the string.
Letters at the beginning of a string or after a prefix letter at
the beginning of a string are scored highest.  Letters after a
word separator such as #\- are scored next highest.  Letters at
the end of a string or before a suffix letter at the end of a
string are scored medium, and letters anywhere else are scored
low.

If a letter is directly after another matched letter, and its
intrinsic value in that position is less than a percentage of the
previous letter's value, it will use that percentage instead.

Finally, a small scaling factor is applied to favor shorter
matches, all other things being equal."
  (letrec ((at-beginning-p
             (lambda (pos)
               (= pos 0)))
           (after-prefix-p
             (lambda (pos)
               (and (= pos 1)
                    (find (aref full 0)
                          el-swank-fuzzy-completion-symbol-prefixes))))
           (word-separator-p
             (lambda (pos)
               (find (aref full pos) el-swank-fuzzy-completion-word-separators)))
           (after-word-separator-p
             (lambda (pos)
               (find (aref full (1- pos))
                     el-swank-fuzzy-completion-word-separators)))
           (at-end-p
             (lambda (pos)
               (= pos (1- (length full)))))
           (before-suffix-p
             (lambda (pos)
               (and (= pos (- (length full) 2))
                    (find (aref full (1- (length full)))
                          el-swank-fuzzy-completion-symbol-suffixes))))
           (score-or-percentage-of-previous
             (lambda (base-score pos chunk-pos)
               (if (zerop chunk-pos)
                 base-score
                 (max base-score
                      (+ (* (funcall score-char (1- pos) (1- chunk-pos)) 0.85)
                         (expt 1.2 chunk-pos))))))
           (score-char
             (lambda (pos chunk-pos)
               (funcall score-or-percentage-of-previous
                        (cond ((funcall at-beginning-p pos)         10)
                              ((funcall after-prefix-p pos)         10)
                              ((funcall word-separator-p pos)       1)
                              ((funcall after-word-separator-p pos) 8)
                              ((funcall at-end-p pos)               6)
                              ((funcall before-suffix-p pos)        6)
                              (t                                    1))
                        pos chunk-pos)))
           (score-chunk
             (lambda (chunk)
               (loop for chunk-pos below (length (second chunk))
                     for pos from (first chunk)
                     summing (funcall score-char pos chunk-pos)))))
    (let* ((chunk-scores (mapcar score-chunk completion))
           (length-score (/ 10.0 (1+ (- (length full) (length short))))))
      (values
       (+ (reduce #'+ chunk-scores) length-score)
       (list (mapcar* #'list chunk-scores completion) length-score)))))


;;;; Entry point.
(defun* el-swank-fuzzy-completions
    (string candidates
            &key (timeout 1500) (filter #'(lambda (x) t)) (prefix-length 2))
  "Returns a list of two values:

  An list of fuzzy completions for a symbol designator STRING.
  The list will be started by score, most likely match first.

  A flag that indicates whether or not TIMEOUT has
  been exhausted during computations. If that parameter's value is
  NIL or 0, no time limit is assumed.

The main result is a list of completion objects, where a completion
object is:

    (COMPLETED-STRING SCORE (&rest CHUNKS) CLASSIFICATION-STRING)

where a CHUNK is a description of a matched substring:

    (OFFSET SUBSTRING)

and FLAGS is short string describing properties of the symbol (see
CLASSIFY-SYMBOL and STRING-CLASSIFICATION->STRING. Currently does
not contain anything.)

E.g., completing \"mvb\" in a package that uses COMMON-LISP would
return something like:

    ((\"multiple-value-bind\" 26.588236 ((0 \"m\") (9 \"v\") (15 \"b\"))
     \"dummy\")
     ...)

The fuzzy match will be performed against the symbols satisfying FILTER.

PREFIX-LENGTH is the STRING's length of substring to which the prefix
match should be performed before the fuzzy match."
  (multiple-value-bind (matchings remaining-time)
      (swfy-find-matching-symbols-with-prefix-length string
                                                     timeout
                                                     candidates
                                                     filter
                                                     prefix-length)
    (values (mapcar #'swfy-fuzzy-matching.candidate
                    (sort matchings
                          #'swfy-fuzzy-matching-greater))
            (<= remaining-time 0))))

;;; Alternative to el-swank-fuzzy-completions, return
;; list of candidates only
(defun* el-swank-fuzzy-matches (string
                                candidates
                                &key
                                (timeout 1500)
                                (filter #'(lambda (x) t))
                                (prefix-length 1))
  (car
   (el-swank-fuzzy-completions string
                               candidates
                               :timeout timeout
                               :filter filter
                               :prefix-length prefix-length)))

;;;;


(defstruct (swfy-fuzzy-matching (:conc-name swfy-fuzzy-matching.)
                                (:constructor swfy-make-fuzzy-matching)
                                )
  candidate      ; The candidate that has been found to match.
  score          ; The higher the better SYMBOL is a match.
  )


(defun swfy-fuzzy-matching-greater (m1 m2)
  (let ((score1 (swfy-fuzzy-matching.score m1))
        (score2 (swfy-fuzzy-matching.score m2)))
    (cond ((> score1 score2) t)
          ((< score1 score2) nil)
          (t
           (let ((name1 (swfy-fuzzy-matching.candidate m1))
                 (name2 (swfy-fuzzy-matching.candidate m2)))
             (string< name1 name2))))))


(defsubst swfy-negate-time (y x)
  (- (truncate (+ (* 10000000 (+ (first y) (second y))) (third y)) 10000)
     (truncate (+ (* 10000000 (+ (first x) (second x))) (third x)) 10000)))

(defmacro* with-swfy-timedout ((name time-in-msec) &body body)
  (let ((started (gensym)))
    `(let ((,started (current-time)))
       (flet ((,name ()
                (let* ((elapsed (current-time))
                       (negated (swfy-negate-time elapsed ,started)))
                  (values (< ,time-in-msec negated)
                          (- ,time-in-msec negated)))))
         (progn ,@body)))))

(defun swfy-find-matching-symbols-with-prefix-length (string
                                                      time-limit-in-msec
                                                      candidates
                                                      filter
                                                      length)
  (let ((regexp (format "^%s" (regexp-quote (substring string 0 length))))
        completions)
    (with-swfy-timedout (timedout2-p time-limit-in-msec)
      (block loop
        (mapc (lambda (candidate)
                (when (and (funcall filter candidate)
                           (string-match regexp candidate))
                  (multiple-value-bind (timedout-p rest-time-limit)
                      (timedout2-p)
                    (cond
                      (timedout-p (return-from loop))
                      (t
                       (multiple-value-bind (match-result score)
                           (swfy-compute-highest-scoring-completion
                            (substring string length)
                            (substring candidate length))
                         (when match-result
                           (push
                            (swfy-make-fuzzy-matching
                             :candidate candidate
                             :score score)
                            completions))))))))
              candidates))
      (values completions (nth-value 1 (timedout2-p))))))


(provide 'el-swank-fuzzy)

;;; el-swank-fuzzy.el ends here
