
;; this whole file will be loaded on demand using c-c++-setup.el

(require 'custom)

(require 'c-eldoc)
(require 'ctypes)
(require 'find-file)
(require 'c-abbrev+)

;; (defun c-c++-get-dual-file (file)
;;   (let* ((header-suffix-re "[Hh]\\(?:[Hh]\\|\\+\\+\\|[Pp][Pp]\\|[Xx][Xx]\\)?")
;;          (source-suffix-re "[Cc]\\(?:[Cc]\\|\\+\\+\\|[Pp][Pp]\\|[Xx][Xx]\\)?")
;;          (fname (file-name-sans-extension
;;                  (file-name-nondirectory file)))
;;          (ext   (file-name-extension file))
;;          (files (remove-if #'file-directory-p
;;                            (directory-files
;;                             (file-name-directory file))))
;;          suffix)
;;     (setq suffix
;;           (cond
;;             ((string-match-p (concat "^" header-suffix-re "$")
;;                              ext)
;;              source-suffix-re)
;;             ((string-match-p (concat "^" source-suffix-re "$")
;;                              ext)
;;              header-suffix-re)
;;             (t (error
;;                 "Error: current file is not header nor C/C++ source file: %S"
;;                 file))))
;;     (find-if (lambda (x) (string-match-p (concat "^" fname "." suffix "$")
;;                                     x))
;;              files)))
;;
;; (defvar-loc c-c++-is-dual-file-for)
;;
;; (defun c-c++-switch-header-and-source ()
;;   "Switch between header .*.(h|hh|H|HH|h++|H++|HXX) and source
;; .*.(c|cc|cpp|c++|C|CC|CPP|C++|cxx|CXX) file."
;;   (interactive)
;;   ;; use simple cache
;;   (if (and c-c++-is-dual-file-for
;;            ;; only if these files have the same name sans extension
;;            ;; if that's not the case then these files are not consieder
;;            ;; dual
;;            (string= (file-name-sans-extension
;;                      (file-name-nondirectory (buffer-file-name)))
;;                     (file-name-sans-extension
;;                      (file-name-nondirectory c-c++-is-dual-file-for))))
;;     (find-file c-c++-is-dual-file-for)
;;
;;     (let ((other-file (c-c++-get-dual-file (buffer-file-name))))
;;       (unless other-file
;;         (error (concat "Error: dual for file %S "
;;                        "doesn't exists in current directory %S")
;;                buffer-file-name
;;                (file-name-directory buffer-file-name)))
;;       (setq c-c++-is-dual-file-for other-file)
;;       (find-file other-file))))
;;


(defun c-setup ()
  (init-common :use-render-formula t)
  (autopair-mode 1)

  (hs-minor-mode 1)
  (c-turn-on-eldoc-mode)

  (modify-syntax-entry ?_ "_")

  (setf whitespace-line-column 80
        whitespace-style '(tabs lines-tail)
        ;; affects only tab display
        tab-width 4)

  (set (make-variable-buffer-local 'vim:shift-width) 8)

  (if-buffer-has-file
   (set (make-local-variable 'compile-command)
        (let* ((fname  (file-name-nondirectory buffer-file-name))
               (target (file-name-sans-extension fname)))
          (setq compile-command
                (mapconcat #'identity
                           (list "gcc"
                                 "-W"
                                 "-Wall"
                                 "-O2"
                                 "-I."
                                 "-o"
                                 target
                                 fname)
                           " ")))))

  (if-has-makefile-command
   (set (make-local-variable 'compile-command)
        (concat "make " (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))))

  (which-function-mode -1)

  (setq c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  ;; (subword-mode t)

  (setq vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC" ff-find-related-file)
    ("g TAB"   c-indent-defun)

    ("g t"     c-end-of-defun)
    ("g n"     c-beginning-of-defun)

    ("z o"     hs-show-block)
    ("z v"     hs-show-block)
    ("z c"     hs-hide-block)
    ("z C"     hs-hide-all)
    ("z O"     hs-show-all))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g t" c-end-of-defun)
    ("g n" c-beginning-of-defun))

  (c-abbrev+-setup))



(provide 'c-setup)
