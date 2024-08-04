;; treesit-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  9 August 2023
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

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

(add-to-list 'treesit-simple-indent-presets
             (cons 'grand-parent-bol
                   #'treesit-grand-parent-bol))

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
