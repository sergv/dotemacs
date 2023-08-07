;; render-formula.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Aug-Sep 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)
(require 'solarized)

(defvar *formula-images-cache* (make-hash-table :test 'equal))

;;;###autoload
(defun render-buffer-flush-cache ()
  (setf *formula-images-cache* (make-hash-table :test 'equal)
        *formula-index* 0)
  (ignore-errors
    (clear-image-cache)))

;;;###autoload
(add-hook 'solarized-theme-mode-changed-hook
          #'render-buffer-flush-cache)


(defvar *formula-index* 0
  "Global numbering of formulas to index filenames so that emacs
won't be confused by the same filename used for different images.")

(defvar render-formula-latex-input-buf "#latex-input#"
  "Buffer with text that is fed to latex.")
(defvar render-formula-latex-output-buf "#latex-output#"
  "Buffer for latex errors.")
(defvar render-formula-conversion-errors-buf "#errors-converting-to-png#"
  "Buffer for errors during conversion to png.")
(defconst +render-formula-standard-packages+
  '("amsmath"
    "amssymb"
    "mathrsfs" ;; very calligraphic fonts
    "color")
  "Packages that should always be included when rendering formulas")
(defconst +render-formula-conditional-packages+
  (list
   (list (lambda (text)
           (and (string-match-p (rx "\\begin{tikzpicture}") text)
                (string-match-p (rx "\\end{tikzpicture}") text)))
         "tikz"))
  "Specification of packages, (<condition> <package>+) that should be
included if condition returns true when applied to text that is
currently being rendered.")

(defconst +render-formula-ps-to-png-exec+ (concat +execs-path+ "/ps-to-png.py"))

(defconst +render-formula-use-dvipng+ nil
  "Whether to use dvipng or emulate it yourself through ghostscript and
pnm utils suite.")

(defconst +render-formula-tmp-path+
  (concat +tmp-path+ "/render-formula"))
(unless (file-directory-p +render-formula-tmp-path+)
  (mkdir +render-formula-tmp-path+
         t ;; parents
         ))

;;;###autoload
(cl-defun render-formula (str &key
                              (point-size 10)
                              (font-size "Large")
                              (foreground-color nil)
                              (background-color nil)
                              (dpi 150))
  "Returns latex formula from STR rendered as image."
  (cl-assert (member font-size
                     '("tiny"
                       "scriptsize"
                       "footnotesize"
                       "small"
                       "normalsize"
                       "large"
                       "Large"
                       "LARGE"
                       "huge"
                       "Huge"))
             nil
             "invalid font-size: %s"
             font-size)
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
           ;; ps will be nedded if we're not using dvipng
           (ps-file (concat +render-formula-tmp-path+ "/" tmp-filename ".ps"))
           (dvi-file (concat +render-formula-tmp-path+ "/" tmp-filename ".dvi"))
           (img-file (concat +render-formula-tmp-path+ "/" tmp-filename ".png"))
           (latex-bufs (list (get-buffer-create
                              render-formula-latex-input-buf)
                             (get-buffer-create
                              render-formula-latex-output-buf)))
           (conversion-error-buf (get-buffer-create
                                  render-formula-conversion-errors-buf))
           (packages
            (append +render-formula-standard-packages+
                    (-mapcat #'cdr
                             (-filter (lambda (cond-spec)
                                        (funcall (car cond-spec) str))
                                      +render-formula-conditional-packages+)))))

      (dolist (buf (cons conversion-error-buf latex-bufs))
        (with-current-buffer buf
          (erase-buffer)))
      (with-current-buffer (get-buffer-create render-formula-latex-input-buf)
        (insert
         (format "\\documentclass[%dpt%s]{article}\n"
                 point-size
                 (if left-eq-numbering?
                     ",leqno"
                   "")))
        (mapc (lambda (pkg)
                (insert "\\usepackage{" pkg "}\n"))
              packages)
        (insert
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
                 (if (string-match-p "^\\\\begin{[^{}\n]+}" str)
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
                                    "-output-directory" +render-formula-tmp-path+
                                    "-jobname" tmp-filename))
            (progn
              (mapc (lambda (buf) (remove-buffer nil buf)) latex-bufs))
          (progn
            (dolist (buf latex-bufs)
              (with-current-buffer buf
                (text-mode)))
            (mapc #'pop-to-buffer latex-bufs)
            (error "LaTeX error"))))
      ;; dvi is now at dvi-file
      (if (if +render-formula-use-dvipng+
              (= 0
                 (call-process "dvipng"
                               nil                  ;; infile
                               conversion-error-buf ;; buffer
                               nil                  ;; display
                               "-T"
                               "tight"
                               "-D"
                               (number->string dpi)
                               dvi-file
                               "-o"
                               img-file))
            (and (= 0
                    (call-process "dvi2ps"
                                  nil                  ;; infile
                                  conversion-error-buf ;; buffer
                                  nil                  ;; display
                                  "-R"
                                  (number->string dpi)
                                  "-c"
                                  ps-file
                                  dvi-file))
                 (= 0
                    (call-process +render-formula-ps-to-png-exec+
                                  nil                  ;; infile
                                  conversion-error-buf ;; buffer
                                  nil                  ;; display
                                  ;; make all background transparent
                                  ;; "--transparent" (apply 'format "rgb:%02x/%02x/%02x" bg-color)
                                  "--dpi"
                                  (number->string dpi)
                                  ps-file
                                  img-file))))
          ;; now everything interesting is in img-file
          (let ((img (create-image img-file
                                   'png
                                   nil
                                   :ascent 'center)))
            (remove-buffer nil conversion-error-buf)
            (setf *formula-index* (+ 1 *formula-index*))
            (puthash str img *formula-images-cache*)
            img)
        (progn
          (pop-to-buffer conversion-error-buf)
          (error "problem transforming dvi file into image"))))))

(defvar-local render-buffer-rendered? nil
  "Is set to t by `render-formula-toggle-formulas' when latex code in buffer is
displayed as images.")

(defvar +render-buffer-latex-re+
  (rx (or
       (seq "\\\(" (group-n 2 (+? anything)) "\\\)")
       (seq "\\\[" (group-n 2 (+? anything)) "\\\]")
       (seq "\$\$"
            (? "[["
               ;; Command string will be interpreted as follows:
               ;; it will be read as usual emacs lisp source and evaluated
               ;;
               ;; It should evaluate to a list of the following entries:
               ;; - (remove <string>+) - all strings here will be treated as regexps and
               ;;                        will be removed from formula source
               ;;
               ;; Evaluation context will contain following symbols:
               ;; comment - regexp with current mode's comments suitable for stripping
               ;; strip-comments - evaluates to `((remove ,comment)), just for convenience
               (group-n 1
                        (+? (or "\\]]"
                                "]\\]"
                                "\\]\\]"
                                anything)))
               "]]")
            (group-n 2 (+? anything))
            "\$\$")))
  "Occurrences of regexp in brackets will be removed from formula")

(defun render-buffer-disable-formula (start end)
  (remove-list-of-text-properties start
                                  end
                                  '(display
                                    render-formula
                                    intangible
                                    read-only)))

(defun render-buffer/remove-all-matches (regexp str)
  "Remove all matches of REGEXP from STR."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match ""))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun render-buffer/cleanup-formula-string (formula-str command)
  "Clean up FORMULA-STR w.r.t. COMMAND which will be interpreted
as emacs lisp program that should return list of command cells to be
carried out on FORMULA-STR."
  (if command
    (let* ((comment (aif (comment-util-current-format)
                        (concat "^\\s-*\\(?:"
                                (comment-format-line-regexp
                                 it)
                              "\\)")
                      (progn
                        (message "no comment format defined for %s"
                                 major-mode)
                        nil)))
           (eval-result
            (condition-case _
                (eval `(let* ((comment ,comment)
                              (strip-comments `((remove ,comment))))
                         ,(car (read-from-string command)))
                      t)
              (end-of-file nil))))
      (unless (list? eval-result)
        (error "command %S evaluated to invalid value: %s"
               command
               eval-result))
      (foldl (lambda (str command-cell)
               (unless (and (list? command-cell)
                            (not (null command-cell)))
                 (error "invalid command cell, list expected: %s"
                        command-cell))
               (cond
                 ((eq? (car command-cell) 'remove)
                  (foldl (lambda (s rx)
                           (cl-assert (string? rx))
                           (render-buffer/remove-all-matches rx s))
                         str
                         (cdr command-cell)))
                 (t
                  (error "unrecognized command cell: %s" command-cell))))
             formula-str
             eval-result))
    formula-str))

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
           (let ((command (match-string-no-properties 1))
                 (formula-str (match-string-no-properties 2)))
             (let ((s (render-buffer/cleanup-formula-string formula-str
                                                            command)))
               (add-text-properties
                (match-beginning 0)
                (match-end 0)
                (list 'display (render-formula (trim-whitespace s))
                      'render-formula t
                      'intangible t
                      'read-only "Disable latex images first"))
               (goto-char (match-end 0)))))))))))

;;;###autoload
(defun render-formula-toggle-formulae ()
  (interactive)
  (if render-buffer-rendered?
    (render-buffer-off)
    (render-buffer-on))
  (setf render-buffer-rendered? (not render-buffer-rendered?)))

(defgroup render-formula
  nil
  "Rendering of tex formulas"
  :group 'tools)

(defface render-formula-regexp-face
  '((t (:foreground "blue")))
  "Face to highlight regexp after \$\$."
  :group 'render-formula)

(defface render-formula-formula-face
  '((t (:foreground "blue")))
  "Face to highlight latex code between \$\$'s."
  :group 'render-formula)

;;;###autoload
(define-minor-mode render-formula-mode
  "Minor mode for rendering latex formulas in buffer as images."
  :init-value nil
  :lighter nil
  :keymap nil
  :group util
  :global nil
  (let ((keywords
         `((,+render-buffer-latex-re+
            (1 'render-formula-regexp-face
               ;; do override
               t
               ;; use lax match since group 1 may not always match
               t)
            (2 'render-formula-formula-face t ;; do override
               )))))
    (if render-formula-mode
      (progn
        (font-lock-add-keywords nil keywords)
        (setq-local font-lock-multiline 'undecided))
      (font-lock-remove-keywords nil keywords))))


(provide 'render-formula)

;; Local Variables:
;; End:

;; render-formula.el ends here
