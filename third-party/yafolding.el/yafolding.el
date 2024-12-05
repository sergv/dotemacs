;;; yafolding.el --- Yet another folding extension for Emacs

;; Copyright (C) 2013-2017 Zeno Zeng

;; Author: Zeno Zeng <zenoofzeng@gmail.com>
;; keywords: folding
;; Time-stamp: <2017-03-05 11:16:23 Zeno Zeng>
;; Version: 0.4.0


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Folding code blocks based on indentation

;;; Code:

(require 'indentation)

(declare-function discover-add-context-menu "discover")

(defgroup yafolding nil
  "Fold code blocks based on indentation level"
  :prefix "yafolding-"
  :link '(url-link :tag "yafolding on github" "https://github.com/zenozeng/yafolding.el")
  :group 'applications)

(defface yafolding-ellipsis-face
  '()
  "Face for folded blocks"
  :group 'yafolding)

(defcustom yafolding-ellipsis-content "..."
  "Text to show in place of a folded block."
  :tag "Ellipsis"
  :type 'string
  :group 'yafolding)

(defun yafolding-get-overlays (beg end)
  "Get all overlays between BEG and END."
  (let ((res nil))
    (dolist (ov (overlays-in beg end))
      (when (memq 'yafolding (overlay-properties ov))
        (push ov res)))
    res))

(defvar yafolding-empty-line-function #'indent-on-blank-line?
  "Check whether line with point at 0th column should be considered empty.")

(defsubst yafolding-should-ignore-current-line-p ()
  "Return if should ignore current line."
  (funcall yafolding-empty-line-function))

(defun yafolding-get-indent-level ()
  "Get the indent level of current line."
  (interactive)
  (if (and (yafolding-should-ignore-current-line-p)
           (< (line-number-at-pos) (line-number-at-pos (point-max))))
      (save-excursion
        (forward-line 1)
        (yafolding-get-indent-level))
      (let ((indent-level 0)
            (last-indentation (indentation-size)))
        (save-excursion
          (while (and (> (indentation-size) 0)
                      (> (line-number-at-pos) 1))
            (forward-line -1)
            (when (< (indentation-size) last-indentation)
              (setq last-indentation (indentation-size))
              (setq indent-level (+ 1 indent-level)))))
        indent-level)))

(defun yafolding-show-region (beg end)
  "Delete all yafolding overlays between BEG and END."
  (mapc #'delete-overlay (yafolding-get-overlays beg end)))

(defun yafolding-show-all ()
  "Delete all yafolding overlays."
  (interactive)
  (yafolding-show-region (point-min) (point-max)))

(defun yafolding-hide-all (&optional indent-level)
  "Hide all elements based on INDENT-LEVEL."
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (save-excursion
    (goto-char (point-min))
    (while (< (line-number-at-pos)
            (line-number-at-pos (point-max)))
      (if (and (= (yafolding-get-indent-level) indent-level)
             (not (yafolding-should-ignore-current-line-p)))
        (yafolding-hide-element))
    (forward-line 1))))

(defun yafolding-toggle-all (&optional indent-level)
  "Toggle folding of the entire file.

If given, toggle all entries that start at INDENT-LEVEL."
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (if (yafolding-get-overlays (point-min) (point-max))
      (yafolding-show-all)
    (yafolding-hide-all indent-level)))

(defun yafolding-ellipsis ()
  "Return propertized ellipsis content."
  (concat " "
          (propertize yafolding-ellipsis-content 'face 'yafolding-ellipsis-face)
          " "))

(defun yafolding-hide-region (beg end)
  "Hide region between BEG and END."
  (when (> end beg)
      (yafolding-show-region beg end)
      (let ((before-string
             (yafolding-ellipsis))
            (new-overlay (make-overlay beg end)))
        (overlay-put new-overlay 'invisible t)
        (overlay-put new-overlay 'intangible t)
        (overlay-put new-overlay 'evaporate t)
        (overlay-put new-overlay 'modification-hooks
                     (list (lambda (overlay &optional a b c d)
                             (delete-overlay overlay))))
        (overlay-put new-overlay 'before-string before-string)
        (overlay-put new-overlay 'category 'yafolding))))

(defun yafolding-debug ()
  "Show yafolding information of the current position."
  (interactive)
  (message "indentation: %d, indent level: %d, ingore current line: %s, element-region: %d - %d, (L%d - L%d)"
           (indentation-size)
           (yafolding-get-indent-level)
           (yafolding-should-ignore-current-line-p)
           (car (yafolding-get-element-region))
           (cdr (yafolding-get-element-region))
           (line-number-at-pos (car (yafolding-get-element-region)))
           (line-number-at-pos (cdr (yafolding-get-element-region)))))

(defun yafolding-get-element-region ()
  "Get (beg . end) of current element."
  (let* ((beg (line-end-position))
         (end beg)
         (indentation (indentation-size))
         (buffer-invisibility-spec nil))
    (save-excursion
      (forward-line 1)
      (goto-char (line-beginning-position))
      (while (and (not (eobp))
                  (or (> (indentation-size) indentation)
                      (yafolding-should-ignore-current-line-p)))
        (unless (yafolding-should-ignore-current-line-p)
          (setq end (line-end-position)))
        (forward-line 1)))
    (cons beg end)))

(defun yafolding-hide-element ()
  "Hide current element."
  (interactive)
  (let ((region (yafolding-get-element-region)))
    (yafolding-hide-region (car region) (cdr region))))

(defun yafolding-show-element ()
  "Show current element."
  (interactive)
  (yafolding-show-region (line-beginning-position)
                         (+ 1 (line-end-position))))

(defun yafolding-toggle-element ()
  "Toggle current element."
  (interactive)
  (if (yafolding-get-overlays (line-beginning-position)
                              (+ 1 (line-end-position)))
      (yafolding-show-element)
    (yafolding-hide-element)))


(add-hook 'isearch-mode-hook
          (lambda() (mapc (lambda (overlay)
                            (overlay-put overlay 'invisible nil))
                          (yafolding-get-overlays (point-min) (point-max)))))

(add-hook 'isearch-mode-end-hook
          (lambda() (mapc (lambda (overlay)
                            (overlay-put overlay 'invisible t))
                          (yafolding-get-overlays (point-min) (point-max)))))

(defun yafolding-go-parent-element ()
  "Go back to parent element."
  (interactive)
  (re-search-backward (concat "^.\\{,"
                              (number-to-string (max 0 (- (indentation-size) 1)))
                              "\\}[^ \t]+")))

(defun yafolding-hide-parent-element ()
  "Hide the parent element."
  (interactive)
  (ignore-errors
    (yafolding-go-parent-element)
    (yafolding-hide-element)))

;; For this feature, you need to install discover.el
;; https://www.github.com/mickeynp/discover.el
(when (boundp 'discover-add-context-menu)
  (discover-add-context-menu
   :context-menu '(yafolding
		   (description "folding based on indentation")
		   (actions
		    ("yafolding"
		     ("h" "hide element" yafolding-hide-element)
		     ("s" "show element" yafolding-show-element)
		     ("t" "toggle element" yafolding-toggle-element)
		     ("H" "hide all" yafolding-hide-all)
		     ("S" "show all" yafolding-show-all)
		     ("T" "toggle all" yafolding-toggle-all)
		     ("p" "go parent element" yafolding-go-parent-element)
		     ("P" "hide parent element" yafolding-hide-parent-element)
		     ("m" "mode" yafolding-mode)))))

  (defalias 'yafolding-discover 'makey-key-mode-popup-yafolding))

;;;###autoload
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

;;;###autoload
(define-minor-mode yafolding-mode
  "Toggle yafolding mode."
  :keymap yafolding-mode-map)

(provide 'yafolding)
;;; yafolding.el ends here
