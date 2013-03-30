;; render-formula.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Aug-Sep 2012
;; Description:

(require 'common)
(require 'solarized+)

(defvar *formula-images-cache* (make-hash-table :test 'equal))

(defun render-buffer-flush-cache ()
  (setf *formula-images-cache* (make-hash-table :test 'equal)
        *formula-index* 0)
  (clear-image-cache))

(add-hook 'color-theme-solarized+-theme-changed-hook
          #'render-buffer-flush-cache)


(defvar *formula-index* 0
  "Global numbering of formulas to index filenames so that emacs
won't be confused by the same filename used for different images.")

(defvar render-formula-latex-input-buf "#latex-input#"
  "Buffer with text that is fed to latex.")
(defvar render-formula-latex-output-buf "#latex-output#"
  "Buffer for latex errors.")

(defun* render-formula (str &key
                            (point-size 10)
                            (font-size "normalsize")
                            (foreground-color nil)
                            (background-color nil)
                            (dpi 150))
  "Returns latex formula from STR rendered as image."
  (assert (member font-size
                  '("tiny"
                    "scriptsize"
                    "footnotesize"
                    "small"
                    "normalsize"
                    "large"
                    "Large"
                    "LARGE"
                    "huge"
                    "Huge")))
  (aif (gethash str *formula-images-cache* nil)
    it
    (let* ((left-eq-numbering? nil)
           (bg-color (color-name-to-rgb
                      (or background-color
                          (frame-parameter nil 'background-color))))
           (fg-color (color-name-to-rgb
                      (or foreground-color
                          (frame-parameter nil 'foreground-color))))
           (tmp-filename (format "formula%d" *formula-index*))
           (tmp-path (concat +tmp-path+ "/render-formula"))
           (tmp-file (concat tmp-path "/" tmp-filename ".tex"))
           (dvi-file (concat tmp-path "/" tmp-filename ".dvi"))
           (img-file (concat tmp-path "/" tmp-filename ".png"))
           (latex-bufs (list (get-buffer-create
                              render-formula-latex-input-buf)
                             (get-buffer-create
                              render-formula-latex-output-buf))))

      (dolist (buf latex-bufs)
        (with-current-buffer buf
          (erase-buffer)))
      (with-current-buffer (get-buffer-create render-formula-latex-input-buf)
        (insert
         (format "\\documentclass[%dpt%s]{article}\n"
                 point-size
                 (if left-eq-numbering?
                   ",leqno"
                   ""))
         "\\usepackage{amsmath}\n"
         "\\usepackage{amssymb}\n"
         "\\usepackage{mathrsfs}\n" ;; very calligraphic fonts
         "\\usepackage{color}\n"
         "\\renewcommand{\\emptyset}{\\varnothing}\n"
         "\\newcommand{\\union}{\\cup}\n"
         "\\newcommand{\\intersect}{\\cap}\n"
         "\\newcommand{\\intersection}{\\cap}\n"
         "\\begin{document}\n"
         (apply 'format "\\pagecolor[rgb]{%f,%f,%f}\n"
                bg-color)
         "\\pagestyle{empty}\n"

         (concat (format "\\begin{%s}\n" font-size)
                 (apply 'format "\\color[rgb]{%f,%f,%f}\n"
                        fg-color)
                 (if (string-match-pure? "^\\\\begin{[^{}\n]+}" str)
                   str
                   (concat "\\begin{math}\\displaystyle\n"
                           str
                           "\\end{math}\n"))
                 (format "\\end{%s}\n" font-size))

         "\\end{document}\n")
        ;; (buffer-substring-no-properties (point-min) (point-max))


        (if (= 0
               (call-process-region (point-min) (point-max)
                                    "latex"
                                    nil ;; don't delete
                                    render-formula-latex-output-buf
                                    nil ;; don't display
                                    ;; latex arguments
                                    "-file-line-error"
                                    "-halt-on-error"
                                    "-shell-escape"
                                    "-output-directory" tmp-path
                                    "-jobname" tmp-filename))
          (progn
            (mapc #'remove-buffer latex-bufs))
          (progn
            (dolist (buf latex-bufs)
              (with-current-buffer buf
                (text-mode)))
            (mapc #'pop-to-buffer latex-bufs)
            (error "LaTeX error"))))
      ;; dvi is now at dvi-file
      (call-process "dvipng"
                    nil ;; infile
                    nil ;; buffer
                    nil ;; display
                    "-T"
                    "tight"
                    "-D"
                    (number->string dpi)
                    dvi-file
                    "-o"
                    img-file)
      ;; now everything interesting is in img-file
      (let ((img (create-image img-file
                               'png
                               nil
                               :ascent 'center)))
        (setf *formula-index* (+ 1 *formula-index*))
        (puthash str img *formula-images-cache*)
        img))))



(defvar-local render-buffer-rendered? nil
  "Is set to t by `render-formula-toggle-formulas' when latex code in buffer is
displayed as images.")

(defvar +render-buffer-latex-re+
  (rx "\$\$"
      (? "[["
         (group
          (+? (or "\\]]"
                  "]\\]"
                  "\\]\\]"
                  anything)))
         "]]")
      (group
       (+? anything))
      "\$\$")
  "Occurrences of regexp in brackets will be removed from formula")

(defun render-buffer-disable-formula (start end)
  (remove-list-of-text-properties start
                                  end
                                  '(display
                                    render-formula
                                    intangible
                                    read-only)))

(defun render-buffer-clean-string (regexp str)
  (if regexp
    (save-match-data
     (with-temp-buffer
       (insert str)
       (goto-char (point-min))
       (while (re-search-forward regexp nil t)
         (replace-match ""))
       (buffer-substring-no-properties (point-min) (point-max))))
    str))

(defun render-buffer-off ()
  (save-excursion
   (with-disabled-undo
    (with-preserved-buffer-modified-p
     (with-inhibited-modification-hooks
      (with-inhibited-read-only
       (goto-char (point-min))
       (while (re-search-forward +render-buffer-latex-re+ nil t)
         (when (get-char-property (match-beginning 0) 'render-formula)
           (render-buffer-disable-formula (match-beginning 0)
                                          (match-end 0))))))))))

(defun render-buffer-on ()
  (save-excursion
   (with-disabled-undo
    (with-preserved-buffer-modified-p
     (with-inhibited-modification-hooks
      (goto-char (point-min))
      (while (re-search-forward +render-buffer-latex-re+ nil t)
        (unless (get-char-property (match-beginning 0) 'display)
          (let ((s (render-buffer-clean-string
                    (match-string-no-properties 1)
                    (match-string-no-properties 2))))
            (add-text-properties
             (match-beginning 0)
             (match-end 0)
             (list 'display (render-formula (trim-whitespaces s))
                   'render-formula t
                   'intangible t
                   'read-only "Disable latex images first")))
          (goto-char (match-end 0)))))))))

(defun render-formula-toggle-formulae ()
  (interactive)
  (if render-buffer-rendered?
    (render-buffer-off)
    (render-buffer-on))
  (setf render-buffer-rendered? (not render-buffer-rendered?)))

(defface render-formula-regexp-face
    '((t (:foreground "blue")))
  "Face to highlight regexp after \$\$.")

(defface render-formula-formula-face
    '((t (:foreground "blue")))
  "Face to highlight latex code between \$\$'s.")

(define-minor-mode render-formula-mode
  "Minor mode for rendering latex formulas in buffer as images."
  nil ;; init
  nil ;; modeline
  nil ;; keymap
  :group util
  :global nil
  (if render-formula-mode
    (progn
      (font-lock-add-keywords
       nil
       `((,+render-buffer-latex-re+
          (1 'render-formula-regexp-face
             ;; do override
             t
             ;; use lax match since group 1 may not always match
             t)
          (2 'render-formula-formula-face t ;; do override
             ))))
      (setq-local font-lock-multiline 'undecided))
    (progn
      (font-lock-remove-keywords
       nil
       `((,+render-buffer-latex-re+
          (1 'render-formula-regexp-face t t)
          (2 'render-formula-formula-face t)))))))


(provide 'render-formula)

;; Local Variables:
;; End:

;; render-formula.el ends here
