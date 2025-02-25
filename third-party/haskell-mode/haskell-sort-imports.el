;;; haskell-sort-imports.el --- Sort the list of Haskell imports at the point alphabetically -*- lexical-binding: t -*-

;; Copyright (C) 2010  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If the region is active it sorts the imports within the
;; region.

;; This will align and sort the columns of the current import
;; list.  It's more or less the coolest thing on the planet.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common-heavy)
(require 'dash)
(require 'haskell-regexen)
(require 'ht)
(require 'text-property-utils)

;;;###autoload
(defun haskell-sort-imports ()
  "Sort the import list at point. It sorts the current group
i.e. an import list separated by blank lines on either side.

If the region is active, it will restrict the imports to sort
within that region."
  (interactive)
  (save-match-data
    (when (haskell-sort-imports-at-import?)
      (let* ((points (haskell-sort-imports-decl-points))
             (current-string (buffer-substring (car points) (cdr points)))
             (current-offset (- (point) (car points))))
        (if (region-active-p)
            (progn
              (goto-char (region-beginning))
              (haskell-sort-imports-goto-import-start))
          (haskell-sort-imports-goto-group-start))
        (let* ((start (point))
               (imports (haskell-sort-imports-collect-imports))
               (sorted (haskell-sort-imports--group-imports imports)))
          (when (not (equal imports sorted))
            (delete-region start (point))
            (mapc (lambda (import) (insert import "\n")) sorted))
          (goto-char start)
          (when (search-forward current-string nil t 1)
            (forward-char (- (length current-string)))
            (forward-char current-offset)))))))

(cl-defstruct haskell-import
  mod-name
  is-qualified?
  qualified-as-name
  is-hiding?

  import-list ;; haskell-import-list struct

  str-before-import-list

  ;; import-list-pos ;; either nil or integer position of the opening space after which opening paren starts.
  ;; import-list
  )

(cl-defstruct haskell-import-list
  start-str ;; ( with surrounding space
  sep       ;; , with surrounding space
  end-str   ;; ) with surrounding space
  entries   ;; list of stings
  )

(defun haskell-sort-imports--longest-str (x y)
  (if (< (length x) (length y))
      y
    x))

(defun haskell-sort-imports--symmetric-append-import-lists (xs ys)
  (cl-assert (or (haskell-import-list-p xs) (null xs)))
  (cl-assert (or (haskell-import-list-p ys) (null xs)))
  (if (and xs ys)
      (let* ((xs-start (haskell-import-list-start-str xs))
             (ys-start (haskell-import-list-start-str ys))
             (base
              (if (< (length xs-start) (length ys-start))
                  ys
                xs))
             (other
              (if (< (length xs-start) (length ys-start))
                  xs
                ys))
             (sep (or (haskell-import-list-sep base)
                      (haskell-import-list-sep other)
                      ", "))
             (xs-entries (haskell-import-list-entries xs))
             (ys-entries (haskell-import-list-entries ys)))
        (cl-assert (stringp sep))
        (make-haskell-import-list
         :start-str (haskell-import-list-start-str base)
         :sep       sep
         :end-str   (haskell-import-list-end-str base)
         :entries   (if xs-entries
                        (if ys-entries
                            (append xs-entries (cons sep ys-entries))
                          xs-entries)
                      ys-entries)))
    ;; If one of import lists is missing (i.e. wildcard) then the other one gets subsumed.
    nil))

(defun haskell-sort-imports--is-separator? (str)
  (and str
       (string-match-p (rx bos (* (any ?\s ?\t ?\n ?\r)) "," (* (any ?\s ?\t ?\n ?\r)) eos)
                       str)))

(defun haskell-sort-imports--asymmetric-append-import-lists (regular-import with-hiding-import k)
  (let ((regular (haskell-import-import-list regular-import))
        (with-hiding (haskell-import-import-list with-hiding-import)))
    (cl-assert (or (haskell-import-list-p regular) (null regular)))
    ;; hiding must always come with an import list
    (cl-assert (haskell-import-list-p with-hiding))
    (if regular
        (let ((regular-entries (haskell-import-list-entries regular))
              (filtered-entries nil)
              (do-skip nil)
              (do-push nil))

          (dolist (entry (haskell-import-list-entries with-hiding))
            (cond
              (do-push
               (push entry filtered-entries)
               (setf do-push nil))
              (do-skip
               (setf do-skip nil))
              ((member entry regular-entries)
               (setf do-skip t))
              (t
               (push entry filtered-entries)
               (setf do-push t))))

          (when (and do-skip
                     (haskell-sort-imports--is-separator? (car filtered-entries)))
            (pop filtered-entries))

          (funcall k
                   t ;; TODO
                   (make-haskell-import-list
                    :start-str (haskell-import-list-start-str with-hiding)
                    :sep       (haskell-import-list-sep with-hiding)
                    :end-str   (haskell-import-list-end-str with-hiding)
                    :entries   (nreverse filtered-entries))
                   (haskell-import-str-before-import-list with-hiding-import)))
      ;; If regular is missing (i.e. wildcard) then hiding gets subsumed.
      (funcall k nil nil (haskell-import-str-before-import-list regular-import)))))

(defvar haskell-sort-imports--parens-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\( "\(\)" tbl)
    (modify-syntax-entry ?\) "\)\(" tbl)
    tbl))

(defun haskell-sort-imports--parse-import-list-in-buffer (start end)
  "Parse either Haskell import/export list, i.e. list of comma-separated
entities. Entities must be valid Haskell import/export names. E.g.

(foo, bar)

(foo, pattern Quux, Foo(..))

( frobnicate
, decombobulate
, Fizz(Buzz, Quux)
)
"
  (cl-flet*
      ((skip-whitespace ()
         (skip-chars-forward " \t\n\r" ))
       (skip-whitespace-backward ()
         (skip-chars-backward " \t\n\r"))
       (malformed-input-error ()
         (error "Malformed import list ‘%s’, cannot recognize the part: ‘%s’"
                str
                (buffer-substring-no-properties (point) (point-max))))
       (advance-char (c)
         (unless (eq (char-after) c)
           (malformed-input-error))
         (forward-char)))
    (save-restriction
      (narrow-to-region start end)
      (with-syntax-table haskell-sort-imports--parens-syntax-table
        (goto-char start)
        (let ((open-start (point)))
          (skip-whitespace)
          (advance-char 40 ;; ?(
                        )
          (skip-whitespace)

          (let ((open-end (point))
                (first-sep nil)
                (entries nil)

                (continue t)
                (result nil))

            (while continue
              (skip-whitespace)

              (let ((entry-start (point)))
                (skip-chars-forward "^,()")

                (when (eq (char-after) ?\()
                  (forward-sexp 1))

                (skip-whitespace-backward)

                (when (< entry-start (point))
                  (push (buffer-substring entry-start (point)) entries))

                (let ((ws-before-end (point)))
                  (skip-whitespace)
                  (pcase (char-after)
                    (?,
                     (advance-char ?,)
                     (skip-whitespace)
                     (let ((sep (buffer-substring ws-before-end (point))))
                       (push sep entries)
                       (unless first-sep
                         (setf first-sep sep))))
                    (41 ;; ?)
                     (setf continue nil
                           result
                           (make-haskell-import-list
                            :start-str (buffer-substring open-start open-end)
                            :sep       first-sep
                            :end-str   (buffer-substring (if entries ws-before-end (point)) (point-max))
                            :entries   (nreverse entries))))))))

            result))))))

(defun haskell-sort-imports--parse-import-list (str)
  (with-temp-buffer
    (insert str)
    (haskell-sort-imports--parse-import-list-in-buffer (point-min) (point-max))))

(cl-defstruct haskell-import-group-key
  mod-name
  qualified-as ;; may be nil
  ;; is-wildcard?
  )

(defun haskell-sort-imports--derive-import-group-key (import)
  (cl-assert (haskell-import-p import))
  (make-haskell-import-group-key
   :mod-name (haskell-import-mod-name import)
   :qualified-as (when (haskell-import-is-qualified? import)
                   (or (haskell-import-qualified-as-name import)
                       (haskell-import-mod-name import)))
   ;; :is-wildcard? (or (haskell-import-is-hiding? import)
   ;;                   (not (haskell-import-import-list-pos import)))
   ))

(defun haskell-sort-imports--parse-import-statement (str)
  (cl-assert (stringp str))
  (if (string-match haskell-regexen/pre-post-qualified-import-line str)
      (let ((is-qualified? (and (or (match-beginning 2)
                                    (match-beginning 3))
                                t))
            (import-list-pos (match-beginning 11)))
        (make-haskell-import
         :mod-name (match-string 10 str)
         :is-qualified? is-qualified?
         :qualified-as-name (when is-qualified?
                              (match-string 12 str))
         :is-hiding? (and (match-beginning 5) t)

         :import-list (when import-list-pos
                        (haskell-sort-imports--parse-import-list
                         (substring str import-list-pos)))

         :str-before-import-list (substring str nil import-list-pos)))
    (error "Malformed import: %s" str)))

(defun haskell-import-merge (a b)
  "Return T if \"import A\" can be replaced by \"import B\" without
introducing compilation errors. I.e. checks that A defines less names
than B."
  (let ((mod-name
         (if (string= (haskell-import-mod-name a)
                      (haskell-import-mod-name b))
             (text-property-utils-merge-properties-unsafe (haskell-import-mod-name a)
                                                          (haskell-import-mod-name b))
           (error "Attempting to merge imports of different modules: %s and %s" a b)))
        (is-qualified? (haskell-import-is-qualified? a))
        (qualified-as-name (haskell-import-qualified-as-name a))
        (merged-str-before-import-list
         (haskell-sort-imports--longest-str
          (haskell-import-str-before-import-list a)
          (haskell-import-str-before-import-list b))))
    (cl-assert (equal (haskell-import-mod-name a)
                      (haskell-import-mod-name b)))
    (cl-assert (equal (haskell-import-is-qualified? a)
                      (haskell-import-is-qualified? b)))
    (cl-assert (equal (haskell-import-qualified-as-name a)
                      (haskell-import-qualified-as-name b)))
    ;; (cl-assert (equal (haskell-import-str-before-import-list a)
    ;;                   (haskell-import-str-before-import-list b)))
    (pcase (cons a b)
      (`(,(pred haskell-import-is-hiding?) . ,(pred haskell-import-is-hiding?))
       (make-haskell-import
        :mod-name mod-name
        :is-qualified? is-qualified?
        :qualified-as-name qualified-as-name
        :is-hiding? t

        :import-list (haskell-sort-imports--symmetric-append-import-lists (haskell-import-import-list a)
                                                                          (haskell-import-import-list b))

        :str-before-import-list merged-str-before-import-list))
      (`(,(pred haskell-import-is-hiding?) . ,(pred (not haskell-import-is-hiding?)))
       (haskell-sort-imports--asymmetric-append-import-lists
        b
        a
        (lambda (is-hiding? merged-import-list merged-str-before-import)
          (make-haskell-import
           :mod-name mod-name
           :is-qualified? is-qualified?
           :qualified-as-name qualified-as-name

           :is-hiding? is-hiding?
           :import-list merged-import-list
           :str-before-import-list merged-str-before-import))))
      (`(,(pred (not haskell-import-is-hiding?)) . ,(pred haskell-import-is-hiding?))
       (haskell-sort-imports--asymmetric-append-import-lists
        a
        b
        (lambda (is-hiding? merged-import-list merged-str-before-import)
          (make-haskell-import
           :mod-name mod-name
           :is-qualified? is-qualified?
           :qualified-as-name qualified-as-name

           :is-hiding? is-hiding?
           :import-list merged-import-list
           :str-before-import-list merged-str-before-import))))
      (`(,(pred (not haskell-import-is-hiding?)) . ,(pred (not haskell-import-is-hiding?)))
       (make-haskell-import
        :mod-name mod-name
        :is-qualified? is-qualified?
        :qualified-as-name qualified-as-name

        :is-hiding? nil
        :import-list (haskell-sort-imports--symmetric-append-import-lists (haskell-import-import-list a)
                                                                          (haskell-import-import-list b))

        :str-before-import-list merged-str-before-import-list)))))

(defun haskell-sort-imports--combine-group (group-key grouped-imports)
  (foldl #'haskell-import-merge (car grouped-imports) (cdr grouped-imports)))

(defun haskell-sort-imports--reconstruct-import-statement (import)
  (cl-assert (haskell-import-p import))
  (if-let (import-list (haskell-import-import-list import))
      (concat (haskell-import-str-before-import-list import)
              (haskell-import-list-start-str import-list)
              (apply #'concat (haskell-import-list-entries import-list))
              (haskell-import-list-end-str import-list))
    (haskell-import-str-before-import-list import)))

(defun haskell-sort-imports--group-imports (imports)
  (let ((grouped (make-hash-table :test #'equal)))
    (dolist (i (remove-duplicates-hashing imports #'equal))
      (let* ((parsed (haskell-sort-imports--parse-import-statement i))
             (key (haskell-sort-imports--derive-import-group-key parsed)))
        (puthash key
                 (cons parsed
                       (gethash key grouped nil))
                 grouped)))

    ;; grouped
    (-map #'haskell-sort-imports--reconstruct-import-statement
          (sort (ht-map #'haskell-sort-imports--combine-group grouped)
                (lambda (a b)
                  (let ((mod-name-a (haskell-import-mod-name a))
                        (mod-name-b (haskell-import-mod-name b)))
                    (or (string< mod-name-a mod-name-b)
                        (and (string= mod-name-a mod-name-b)
                             (let ((is-hiding-a (haskell-import-is-hiding? a))
                                   (is-hiding-b (haskell-import-is-hiding? b)))
                               (or (bool-< is-hiding-a is-hiding-b)
                                   (and (equal is-hiding-a is-hiding-b)
                                        (let ((is-qualified-a (haskell-import-is-qualified? a))
                                              (is-qualified-b (haskell-import-is-qualified? b)))
                                          (bool-< is-qualified-a is-qualified-b)))))))))))))

(defun haskell-sort-imports--extract-mod-name (str)
  "Normalize an import, if possible, so that it can be sorted."
  (if (string-match haskell-regexen/pre-post-qualified-import-line str)
      (match-string 10 str)
    str))

(defun haskell-sort-imports-collect-imports ()
  (let ((imports (list)))
    (while (looking-at-p "import")
      (let* ((points (haskell-sort-imports-decl-points))
             (string (buffer-substring (car points) (cdr points))))
        (goto-char (min (1+ (cdr points))
                        (point-max)))
        (setq imports (cons string imports))))
    (nreverse imports)))

(defun haskell-sort-imports-goto-group-start ()
  "Go to the start of the import group."
  (when (or (and (search-backward-regexp (rx (or (seq bol "#") (seq (char ?\n) (char ?\n)))) nil t 1)
                 (goto-char (+ 2 (line-end-position))))
            (when (search-backward-regexp "^module " nil t 1)
              (goto-char (1+ (line-end-position))))
            (goto-char (point-min)))
    (beginning-of-line)
    (while (looking-at-p "^[ \t]*\\(?:--\\|$\\)")
      (forward-line))
    t))

(defun haskell-sort-imports-at-import? ()
  "Are we at an import?"
  (save-excursion
    (haskell-sort-imports-goto-import-start)
    (looking-at-p "import")))

(defun haskell-sort-imports-goto-import-start ()
  "Go to the start of the import."
  (goto-char (car (haskell-sort-imports-decl-points))))

(defun haskell-sort-imports-decl-points ()
  "Get the points of the declaration."
  (save-excursion
    (let ((start (or (progn (goto-char (line-end-position))
                            (search-backward-regexp "^[^ \n]" nil t 1)
                            (unless (or (looking-at-p "^-}$")
                                        (looking-at-p "^{-$"))
                              (point)))
                     0))
          (end (progn (goto-char (1+ (point)))
                      (or (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                            (forward-char -1)
                            (search-backward-regexp "[^\n ]" nil t)
                            (line-end-position))
                          (when (search-forward-regexp "\n" nil t 1)
                            (1- (point)))
                          (point-max)))))
      (cons start end))))

(provide 'haskell-sort-imports)

;;; haskell-sort-imports.el ends here
