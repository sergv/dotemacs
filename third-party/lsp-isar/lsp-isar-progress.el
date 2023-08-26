;;; lsp-isar-progress.el --- Progress buffer for Isabelle ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Mathias Fleury

;; URL: https://bitbucket.org/zmaths/isabelle2019-vsce/

;; Keywords: lisp
;; Version: 0

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;
;; The idea is simply to send the progress request regularly (every
;; second or so).
;;
;; Sometimes, the processing takes a lot of time and several request
;; are sent through. To avoid that, we delay the next request from
;; ``lsp-isar-progress-request-max-delay'' seconds
;;

;;; Commentary:

;; blabla

;;; Code:
(require 'common)

(require 'lsp-mode)
(require 'dash)
(require 'lsp-isar-types)

;; progress
(defvar lsp-isar-progress-request-max-delay 3 "Maximum delay for printing.")
(defvar lsp-isar-progress--request-delay 0 "Intial delay before printing.")

(defvar lsp-isar-progress-message-map (make-hash-table :test #'equal)
  "Map from theory files to cons of theory status object form LSP
and rendered message (populated lazily, thus may be nil).")

(defun lsp-isar-progress--get (filename)
  (when-let (progress (gethash filename lsp-isar-progress-message-map))
    (cl-assert (consp progress))
    (aif (cdr progress)
        it
      (let ((rendered (lsp-isar-progress--render (car progress))))
        (setcdr progress rendered)
        rendered))))

(defun lsp-isar-progress--render (theory-status)
  (-let [(&lsp-isar:TheoryProgress :name :unprocessed :failed :running :finished :consolidated :warned) theory-status]
    (let* ((filename name)
           (total (+ unprocessed running warned failed finished))
           (processed (+ warned finished))
           (msg (concat (set-string-face-property 'compilation-error (number->string failed))
                        "/"
                        (set-string-face-property 'compilation-warning (number->string warned))
                        ";"
                        (number->string processed)
                        (unless (zerop running)
                          (concat "+" (number->string running)))
                        "/"
                        (number->string total))))
      msg)))

(lsp-defun lsp-isar-progress--update (_workspace (&lsp-isar:Progress :nodes-status))
  "Record new progress info to be rendered later on demand."
  (setq lsp-isar-progress--request-delay 0)
  (clrhash lsp-isar-progress-message-map)
  (seq-doseq (theory-status nodes-status)
    (-let [(&lsp-isar:TheoryProgress :name) theory-status]
      (let ((filename name))
        (puthash filename (cons theory-status nil) lsp-isar-progress-message-map)))))

(defun lsp-isar-progress--request-buffer ()
  "Request progress update."
  (with-demoted-errors "Error while requesting Isar progress: %s"
      (progn
        (when (<= lsp-isar-progress--request-delay 0)
          (let ((my-message (lsp-make-notification "PIDE/progress_request" nil)))
            (lsp-send-notification my-message)
            (setq lsp-isar-progress--request-delay lsp-isar-progress-request-max-delay)))
        (setq lsp-isar-progress--request-delay (- lsp-isar-progress--request-delay 1)))))

(defun lsp-isar-progress-activate-progress-update ()
  "Activate the progress request."
  (run-at-time 0 1 #'lsp-isar-progress--request-buffer))

(provide 'lsp-isar-progress)

;;; lsp-isar-progress.el ends here
