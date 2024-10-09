;; treesit-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  9 August 2023
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'macro-util)
  (require 'treesit-utils))

(require 'el-patch)
(require 'set-up-paths)

(defvar treesit-max-buffer-size)
(defvar treesit-font-lock-level)
(defvar treesit-extra-load-path)

(setf treesit-max-buffer-size (* 100 1024 1024)
      treesit-font-lock-level 4)

(add-to-list 'treesit-extra-load-path (concat +emacs-config-path+ "/lib"))

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'treesit)
  (when (treesit-language-available-p 'json)
    (add-to-list 'major-mode-remap-alist
                 '(json-mode . json-ts-mode)))
  (when (treesit-language-available-p 'haskell)
    (add-to-list 'major-mode-remap-alist
                 '(haskell-mode . haskell-ts-mode)))
  (when (treesit-language-available-p 'kotlin)
    (add-to-list 'major-mode-remap-alist
                 '(kotlin-mode . kotlin-ts-mode))))

(defun treesit-parse-file (path language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (with-temp-buffer
    (insert-file-contents path)
    (treesit-parser-root-node
     (treesit-parser-create language))))

(defun treesit-utils-find-topmost-parent (node pred)
  (let ((result nil)
        (p node))
    (while p
      (when (funcall pred p)
        (setf result p))
      (setf p (treesit-node-parent p)))
    result))

(defun treesit-utils-find-topmost-parent-limited (node pred limit)
  (let ((result nil)
        (p node))
    (while (and p
                (> limit 0))
      (when (funcall pred p)
        (setf result p))
      (setf p (treesit-node-parent p)
            limit (- limit 1)))
    result))

(defun treesit-grand-parent-bol (_n parent &rest _)
  (awhen (treesit-node-parent parent)
    (save-excursion
      (goto-char (treesit-node-start it))
      (back-to-indentation)
      (point))))

(defun treesit-node-text-no-properties-unsafe (node)
  (cl-assert (not (null node)))
  (cl-assert (treesit-node-p node))
  (cl-assert (eq (current-buffer) (treesit-node-buffer node)))
  (buffer-substring-no-properties (treesit-node-start node) (treesit-node-end node)))

(add-to-list 'treesit-simple-indent-presets
             (cons 'grand-parent-bol
                   #'treesit-grand-parent-bol))

;;;###autoload
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (el-patch-feature 'treesit))

(el-patch-defun treesit-indent-region (beg end)
  "Indent the region between BEG and END.
Similar to `treesit-indent', but indent a region instead."
  (treesit-update-ranges beg end)
  ;; We indent `treesit--indent-region-batch-size' lines at a time, to
  ;; reduce the number of times the parser needs to re-parse.  In each
  ;; batch, we go through each line and calculate the anchor and
  ;; offset as usual, but instead of modifying the buffer, we save
  ;; these information in a vector.  Once we've collected ANCHOR and
  ;; OFFSET for each line in the batch, we go through each line again
  ;; and apply the changes.  Now that buffer is modified, we need to
  ;; reparse the buffer before continuing to indent the next batch.
  (let* ((meta-len 2)
         (vector-len (* meta-len treesit--indent-region-batch-size))
         ;; This vector saves the indent meta for each line in the
         ;; batch.  It is a vector [ANCHOR OFFSET ANCHOR OFFSET...].
         ;; ANCHOR is a marker on the anchor position, and OFFSET is
         ;; an integer.  ANCHOR and OFFSET are either both nil, or
         ;; both valid.
         (meta-vec (make-vector vector-len 0))
         (lines-left-to-move 0)
         (end (copy-marker end t))
         (idx 0)
         (starting-pos 0)
         (announce-progress (> (- end beg) 80000)))
    (save-excursion
      (goto-char beg)
      ;; First pass.  Go through each line and compute the
      ;; indentation.
      (while (and (eq lines-left-to-move 0) (< (point) end))
        (setq idx 0
              starting-pos (point))
        (while (and (eq lines-left-to-move 0)
                    (< idx treesit--indent-region-batch-size)
                    (< (point) end))
          (if (looking-at (rx (* whitespace) eol) t)
              ;; Unlike in `indent-line' where we sometimes pre-indent
              ;; an empty line, We don't indent empty lines in
              ;; `indent-region'.  Set ANCHOR and OFFSET to nil.
              (setf (aref meta-vec (* idx meta-len)) nil
                    (aref meta-vec (+ 1 (* idx meta-len))) nil)
            (pcase-let* ((`(,anchor . ,offset) (treesit--indent-1))
                         (marker (aref meta-vec (* idx meta-len))))
              (if (not (and anchor offset))
                  ;; No indent for this line, either...
                  (if (markerp marker)
                      (progn
                        ;; ... Set marker and offset to do a dummy
                        ;; indent, or...
                        (back-to-indentation)
                        (move-marker marker (point))
                        (setf (aref meta-vec (+ 1 (* idx meta-len))) 0))
                    ;; ...Set anchor to nil so no indent is performed.
                    (setf (aref meta-vec (* idx meta-len)) nil))
                (el-patch-remove
                  ;; Set ANCHOR.
                  (if (markerp marker)
                      (move-marker marker anchor)
                    (setf (aref meta-vec (* idx meta-len))
                          (copy-marker anchor t)))
                  ;; SET OFFSET.
                  (setf (aref meta-vec (+ 1 (* idx meta-len))) offset))
                (el-patch-add
                  (treesit-with-evaluated-anchor-and-offset
                      (anchor-pos anchor)
                      (offset-num offset)
                    ;; Set ANCHOR.
                    (if (markerp marker)
                        (move-marker marker anchor-pos)
                      (setf (aref meta-vec (* idx meta-len))
                            (copy-marker anchor-pos t)))
                    ;; SET OFFSET.
                    (setf (aref meta-vec (+ 1 (* idx meta-len))) offset-num))))))
          (cl-incf idx)
          (setq lines-left-to-move (forward-line 1)))
        ;; Now IDX = last valid IDX + 1.
        (goto-char starting-pos)
        ;; Second pass, go to each line and apply the indentation.
        (dotimes (jdx idx)
          (let ((anchor (aref meta-vec (* jdx meta-len)))
                (offset (aref meta-vec (+ 1 (* jdx meta-len)))))
            (when (and anchor offset)
              (let ((col (save-excursion
                           (goto-char anchor)
                           (+ offset (current-column)))))
                (indent-line-to col))))
          (forward-line 1))
        (when announce-progress
          (message "Indenting region...%s%%"
                   (/ (* (- (point) beg) 100) (- end beg)))))
      ;; Delete markers.
      (dotimes (idx treesit--indent-region-batch-size)
        (let ((marker (aref meta-vec (* idx meta-len))))
          (when (markerp marker)
            (move-marker marker nil))))
      (move-marker end nil))))

;; Debug indentation:
;; treesit--indent-verbose

;; (add-to-list 'treesit-simple-indent-presets
;;              (cons 'n-p-gp-debug
;;                    #'treesit-n-p-gp-debug))
;;
;; (defun treesit-n-p-gp-debug (node-t parent-t grand-parent-t)
;;   (lambda (node parent &rest _)
;;     (message "node = %s, parent = %s"
;;              (pp-to-string node)
;;              (pp-to-string parent))
;;     (and (or (null node-t)
;;              (string-match-p
;;               node-t (or (treesit-node-type node) "")))
;;          (or (null parent-t)
;;              (string-match-p
;;               parent-t (treesit-node-type parent)))
;;          (or (null grand-parent-t)
;;              (and
;;               (treesit-node-parent parent)
;;               (string-match-p
;;                grand-parent-t
;;                (treesit-node-type
;;                 (treesit-node-parent parent))))))))

;; (treesit-query-validate 'json '((pair key: (string) @default))

;; Parse with elisp
;; (let* ((files (find-rec* :root "/home/sergey/projects/haskell/projects/"
;;                          :globs-to-find '("*.hs" "*.lhs")
;;                          :ignored-absolute-dirs
;;                          '("/home/sergey/projects/haskell/projects/compilers/ghc/testsuite/"
;;                            "/home/sergey/projects/haskell/projects/compilers/ghc.old/testsuite/")
;;                          :ignored-directories '("*test*" "*testsuite*")))
;;        (total (length files))
;;        (i 0))
;;   (dolist (path files)
;;     (message "Parsing %s/%s %s: %S" i total path
;;              (condition-case nil
;;                  (progn
;;                    (treesit-parse-file path 'haskell)
;;                    "OK")
;;                (error "FAIL")))
;;     (cl-incf i)
;;     (redisplay)
;;     ;; (push (cons path (treesit-parse-file path 'haskell))
;;     ;;       asts)
;;     ))
;;
;; (treesit--explorer-draw-node
;;  (treesit-parse-file "/home/sergey/projects/haskell/projects/vector-quicksort/src/Data/Vector/Algorithms/Quicksort.hs" 'haskell))

(provide 'treesit-setup)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; End:

;; treesit-setup.el ends here
