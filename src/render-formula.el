

(require 'common)

;; (frame-parameter nil 'alpha)

(defvar *formula-images-cache* (make-hash-table :test 'equal))

(defun render-buffer-flush-cache ()
  (setf *formula-images-cache* (make-hash-table :test 'equal)
        *formula-index* 0)
  (clear-image-cache))


(defvar *formula-index* 0
  "Global numbering of formulas to index filenames so that emacs
won't be confused by the same filename used for different images.")

(defvar render-formula-latex-output-buf "#latex-output#"
  "Buffer for latex errors.")

(defun* render-formula (str &key (point-size 10) (font-size "normalsize"))
  "Returns rendered image."
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
                      (frame-parameter nil 'background-color)))
           (fg-color (color-name-to-rgb
                      (frame-parameter nil 'foreground-color)))
           (tmp-filename (format "formula%d" *formula-index*))
           (tmp-path (concat +tmp-path+ "/render-formula"))
           (tmp-file (concat tmp-path "/" tmp-filename ".tex"))
           (dvi-file (concat tmp-path "/" tmp-filename ".dvi"))
           (img-file (concat tmp-path "/" tmp-filename ".png")))
      (with-temp-buffer
        (insert
         (format "\\documentclass[%dpt%s]{article}\n"
                 point-size
                 (if left-eq-numbering?
                   ",leqno"
                   ""))
         "\\usepackage{amsmath}\n"
         "\\usepackage{amssymb}\n"
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
                 "\\begin{math}\\displaystyle\n"
                 str
                 "\\end{math}\n"
                 (format "\\end{%s}\n" font-size))

         "\\end{document}\n")
        ;; (buffer-substring-no-properties (point-min) (point-max))

        (with-current-buffer (get-buffer-create
                              render-formula-latex-output-buf)
          (erase-buffer)
          (text-mode))
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
          (remove-buffer render-formula-latex-output-buf)
          (progn
            (pop-to-buffer render-formula-latex-output-buf)
            (error "LaTeX error"))))
      ;; dvi is now at dvi-file
      (call-process "dvipng"
                    nil ;; infile
                    nil ;; buffer
                    nil ;; display
                    "-T"
                    "tight"
                    "-D"
                    "150"
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



(defvar-loc render-buffer-rendered? nil
  "Is set to t by `render-buffer' when latex code in buffer is
displayed as images.")

(defvar +render-buffer-latex-re+ "\\$\\$\\(\\(?:.\\|\n\\)+?\\)\\$\\$")

(defun render-buffer-disable-formula (start end)
  (remove-list-of-text-properties start
                                  end
                                  '(display
                                    render-formula
                                    intangible
                                    read-only)))


(defun render-buffer-off ()
  (save-excursion
   (with-disabled-undo
    (with-preserved-buffer-modified-p
     (with-inhibited-modification-hooks
      (with-inhibited-readonly
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
          (let ((s (substring-no-properties (match-string 1) 0)))
            (add-text-properties
             (match-beginning 0)
             (match-end 0)
             (list 'display (render-formula s)
                   'render-formula t
                   'intangible t
                   'read-only "Disable latex images first")))
          (goto-char (match-end 0)))))))))

(defun render-buffer ()
  (interactive)
  (if render-buffer-rendered?
    (render-buffer-off)
    (render-buffer-on))
  (setf render-buffer-rendered? (not render-buffer-rendered?)))


(defface render-formula-formula-face
    '((t (:foreground "blue")))
  "Face to highlight latex code between $$'s.")

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
          (1 'render-formula-formula-face t ;; do override
             )))))
    (progn
      (font-lock-remove-keywords
       nil
       `((,+render-buffer-latex-re+
          (1 'render-formula-formula-face t)))))))



;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'render-formula)






