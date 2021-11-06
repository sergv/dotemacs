;;; company-lean.el --- A company backend for lean-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;; Maintainer: Sebastian Ullrich <sebasti@nullri.ch>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "24.3") (dash "2.18.0") (s "1.10.0") (f "0.19.0") (company "0.9.3") (lean-mode "3.3.0"))
;; URL: https://github.com/leanprover/lean-mode

;; Released under Apache 2.0 license as described in the file LICENSE.

;;; Commentary:

;; Provides context-sensitive auto completion for lean-mode.

;;; Code:

(require 'company)
(require 'company-etags)
(require 'dash)
(require 'f)
(require 's)
(require 'cl-lib)
(require 'lean-util)
(require 'lean-server)

(defcustom company-lean-type-foreground (face-foreground 'font-lock-keyword-face)
  "Color of type parameter in auto-complete candidates"
  :group 'lean
  :type 'color)

;;;###autoload
(defun company-lean-hook ()
  (set (make-local-variable 'company-backends) '(company-lean))
  (setq-local company-tooltip-limit 20)                      ; bigger popup window
  (setq-local company-minimum-prefix-length 5)
  (setq-local company-idle-delay nil)                        ; decrease delay before autocompletion popup shows
  ;(setq-local company-echo-delay 0)                          ; remove annoying blinking
  (setq-local company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (company-mode t))

(cl-defun company-lean--make-candidate (prefix &key text type (tactic_params 'empty) doc source &allow-other-keys)
  (cl-destructuring-bind (&key file line _column) source
    (let ((source (cond
                   (file (cons file line))
                   (line (cons (current-buffer) (lean-pos-at-line-col line 0))))))
      (propertize text
                  'type   type
                  'tactic_params tactic_params
                  'doc    doc
                  'source source
                  'prefix prefix))))

(defun company-lean--handle-singleton-candidate (prefix candidates)
  "Handle singleton candidate. If the candidate does not start
  with prefix, we add prefix itself as a candidate to prevent
  from auto-completion."
  (let ((candidate (car candidates)))
    (cond ((s-prefix? prefix candidate) candidates)
          (t                            `(,candidate ,prefix)))))

(cl-defun company-lean--exec (&key skip-completions)
  "Synchronously queries completions for the current point from server and returns a plist with keys :prefix and :candidates., or nil if no completion should be triggered."
  (lean-server-sync)
  (let* ((col (lean-line-offset))
         (response (lean-server-send-synchronous-command
                    'complete (list :file_name (buffer-file-name)
                                    :line (line-number-at-pos)
                                    :column col
                                    :skip_completions (or skip-completions :json-false))))
         (candidates (plist-get response :completions))
         (prefix (plist-get response :prefix)))
    (when candidates
      (setq candidates
            (--map (apply 'company-lean--make-candidate prefix it)
                   candidates))
      (when (= (length candidates) 1)
        (setq candidates
              (company-lean--handle-singleton-candidate prefix candidates))))
    (when (plist-member response :prefix)
      (list :prefix prefix :candidates candidates))))

(defun company-lean--annotation (candidate)
  (let ((type (get-text-property 0 'type candidate))
        (tactic_params (get-text-property 0 'tactic_params candidate)))
    (when type
      (let* ((annotation-str (if (not (eq tactic_params 'empty))
                                 (format " %s" (mapconcat 'identity tactic_params " "))
                               (format " : %s" type)))
             (annotation-len (length annotation-str))
             (candidate-len  (length candidate))
             (entry-width    (+ candidate-len
                                annotation-len))
             (allowed-width  (truncate (* 0.90 (window-body-width)))))
        (when (> entry-width allowed-width)
          (setq annotation-str
                (concat
                 (substring-no-properties annotation-str
                                         0
                                         (- allowed-width candidate-len 3))
                 "...")))
        annotation-str))))

(defun company-lean--location (arg)
  (get-text-property 0 'source arg))

(defun company-lean--match (arg)
  "Return the end of matched region"
  (let ((prefix (get-text-property 0 'prefix arg)))
    (when (and prefix (eq (s-index-of prefix arg) 0))
        (length prefix))))

(defun company-lean--meta (arg)
  (get-text-property 0 'doc arg))

(defun company-lean (command &optional arg &rest ignored)
  (cl-case command
    (prefix (plist-get (company-lean--exec :skip-completions t) :prefix))
    (candidates (plist-get (company-lean--exec) :candidates))
    (annotation (company-lean--annotation arg))
    (location (company-lean--location arg))
    (match (company-lean--match arg))
    (meta (company-lean--meta arg))
    (no-cache t)
    (require-match 'never)
    (sorted t)))

;; ADVICES
;; =======

(defadvice company--window-width
    (after company-lean--window-width activate)
  (when (eq major-mode 'lean-mode)
    (setq ad-return-value (truncate (* 0.95 (window-body-width))))))

(defun company-lean--replace-regex-return-position (regex rep string &optional start)
  "Find regex and replace with rep on string.

Return replaced string and start and end positions of replacement."
  (let* ((start   (or start 0))
         (m-start (string-match regex string start))
         (m-end   (match-end 0))
         pre-string post-string matched-string replaced-string result)
    (cond (m-start
           (setq pre-string     (substring string 0 m-start))
           (setq matched-string (substring string m-start m-end))
           (setq post-string    (substring string m-end))
           (string-match regex matched-string)
           (setq replaced-string
                 (replace-match rep nil nil matched-string))
           (setq result (concat pre-string
                                replaced-string
                                post-string))
           `(,result ,m-start ,(+ m-start (length replaced-string)))
           ))))

(defun company-lean--replace-regex-add-properties-all (regex rep string properties)
  "Find all occurrences of regex in string, and replace them with
rep. Then, add text-properties on the replaced region."
  (let ((replace-result-items (company-lean--replace-regex-return-position regex rep string))
        (result string))
    (while replace-result-items
      (pcase replace-result-items
        (`(,replaced-string ,m-start ,m-end)
         (setq result replaced-string)
         (add-text-properties m-start m-end properties result)
         (setq replace-result-items
               (company-lean--replace-regex-return-position regex rep result m-end)))))
    result))

(eval-after-load 'company
  '(defadvice company-fill-propertize
     (after company-lean-fill-propertize activate)
     (when (eq major-mode 'lean-mode)
       (let* ((selected (ad-get-arg 3))
              (foreground-color company-lean-type-foreground)
              (background-color (if selected (face-background 'company-tooltip-selection)
                                  (face-background 'company-tooltip)))
              (face-attrs
               (cond (background-color `(:foreground ,foreground-color
                                         :background ,background-color))
                     (t `(:foreground ,foreground-color))))
              (properties `(face       ,face-attrs
                                       mouse-face company-tooltip))
              (old-return ad-return-value)
              (old-len    (length old-return))
              new-return new-len)
         (setq new-return
               (company-lean--replace-regex-add-properties-all
                (rx "?" word-start (group (+ (not white))) word-end)
                "\\1"
                ad-return-value
                properties))
         (setq new-len (length new-return))
         (while (< (length new-return) old-len)
           (setq new-return
                 (concat new-return " ")))
         (when background-color
           (add-text-properties new-len old-len properties new-return))
         (setq ad-return-value new-return)))))

;;;###autoload
(add-hook 'lean-mode-hook #'company-lean-hook)

(provide 'company-lean)
;;; company-lean.el ends here
