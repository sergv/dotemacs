;; ctags-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 27 December 2012
;; Description:

(eval-when-compile
  (require 'cl))
(require 'solarized+)

(defvar *ctags-exec* "ctags-exuberant"
  ;; (concat +emacs-config-path+ "/tmp/python/ctags-5.8/ctags")
  )

(setf +ctags-language-flags+
  '(("c"
     "--language-force=c"
     "--c-kinds=-cdefgmnpstuv"
     "--c-kinds=+sv"
     "--fields=+iaSzkK"
     "--extra=+q")
    ("c++"
     "--c++-kinds=+cdefgmnpstuv"
     "--fields=+iaSzkK"
     "--extra=+q")
    ("python"
     "--language-force=python"
     "--python-kinds=+cfmvi"
     "--fields=+SzkK")))

(defconst +ctags-language-extensions+
  `(("c"
     "\\.[ch]\\'")
    ("c++"
     "\\.\\(?:c\\|cc\\|cxx\\|cpp\\|c++\\|h\\|hh\\|hxx\\|hpp\\|h++\\|inl\\|inc\\|incl\\)\\'")
    ("python"
     "\\.\\(?:py\\|pyx\\|pxd\\|pxi\\)\\'")))

(defconst +ctags-line-re+
  (rx bol
      (group (+ (not (any ?\t ?\s ?\n))))
      "\t"
      (group (+ (not (any ?\t ?\s ?\n))))
      "\t"
      (or (group (+ digit))
          (or (seq "/^"
                   (+ (or (not (any ?\n ?/))
                          "\\\\/"))
                   "$/")
              (seq "?^"
                   (+ (or (not (any ?\n ?/))
                          "\\/"))
                   "$?")))
      (or ";\""
          eol)))



(defun run-ctags-on-files (language root-dir files out-buffer)
  (with-current-buffer out-buffer
    (goto-char (point-max))
    (unless (looking-at-pure? "^$")
      (insert "\n"))
    (let ((ext-re (cadr (assoc language +ctags-language-extensions+))))
      (with-temp-buffer
        (cd root-dir)
        (dolist (file (filter (lambda (f)
                                (string-match-pure? ext-re f))
                              files))
          (insert file "\n"))
        (apply #'call-process-region
               (point-min)
               (point-max)
               *ctags-exec*
               nil
               out-buffer
               nil
               "-f"
               "-"
               "-L"
               "-"
               "--excmd=number"
               (cdr (assoc language +ctags-language-flags+)))))))

(defvar *ctags-projects*
  (make-hash-table :test #'equal))

(defstruct ctags-tag
  symbol
  file
  line
  kind
  aux-fields ;; alist of ({access|class|signature|namespace} . <value>)
  )

(defun c++-load-ctags-project ()

  (interactive)
  (unless *have-git?*
    (error "git is not available => no support for projects"))
  (when *have-git?*
    (git-update-file-repository))
  (let ((ctags-buf (aif git-repository
                     (with-current-buffer (get-buffer-create (concat "*"
                                                                     it
                                                                     "-ctags*"))
                       (cd it)
                       (current-buffer))
                     (get-buffer-create (concat "*"
                                                (buffer-file-name)
                                                "-ctags*")))))
    (aif git-repository
      (run-ctags-on-files "c++"
                          it
                          (git-get-tracked-files it)
                          ctags-buf)
      (run-ctags-on-files "c++"
                          (file-name-directory (buffer-file-name))
                          (list (buffer-file-name))
                          ctags-buf))
    (pop-to-buffer ctags-buf)
    (ctags-mode)
    (save-excursion
     (goto-char (point-min))
     (while (not (eob?))
       (when (looking-at +ctags-line-re+)
         (let ((symbol (match-string-no-properties 1))
               (file (match-string-no-properties 2))
               (line (match-string-no-properties (string->number 3))))
           (goto-char (match-end 0))
           ))
       (forward-line 1)))))


;; (require 'more-scheme)
;;
;; (defstruct ctags-tag
;;   name
;;   file
;;   line
;;   kind)
;;
;; (let ((tags-buf (get-buffer-create "*python-tags*"))
;;       (tags '()))
;;   (with-temp-buffer
;;     (dolist (file '("/home/sergey/projects/python/misc/fractal/fractal.py"))
;;       (insert file "\n"))
;;     (with-current-buffer tags-buf
;;       (erase-buffer))
;;     (call-process-region (point-min)
;;
;;                          (point-max)
;;                          "/home/sergey/emacs/tmp/python/ctags-5.8/ctags"
;;                          nil
;;                          tags-buf
;;                          nil
;;                          "-f"
;;                          "-"
;;                          "-L"
;;                          "-"
;;                          "--excmd=number"
;;                          "--language-force=python"
;;                          ;; "--python-kinds=-fmv"
;;                          ;; "--python-kinds=+ci"
;;                          "--python-kinds=+cfmvi"
;;                          ;; "--fields=+iaSzkKn"
;;                          "--fields=+SzKn"
;; n"
;;                          "--fields=+KSz"
;;                          "--extra=+q")
;;     (with-current-buffer tags-buf
;;       ;; (delete-non-matching-lines "kind:namespace" (point-min) (point-max))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\([^\t]+\\)\t\\([^\t]+\\)\t\\([0-9]+\\);\"\tkind:\\([^\t\n ]+\\)$"
;;                                 nil
;;                                 t)
;;         (push (make-ctags-tag :name (match-string-no-properties 1)
;;                               :file (match-string-no-properties 2)
;;                               :line (string->number (match-string-no-properties 3))
;;                               :kind (string->symbol (match-string-no-properties 4)))
;;               tags)))
;;     tags))
;;
;;
;; (defun get-python-includes (source)
;;   "Get list of imported modules in SOURCE file."
;;   (let ((imports '()))
;;     (with-temp-buffer
;;       (insert-file-contents source)
;;       (goto-char (point-min))
;;       (while (re-search-forward "import[ \t]+")))))
;;

(defface ctags-symbol-face
    `((t (:foreground ,+solarized-blue+)))
  "Face to highlight warnings like error, assert."
  :group 'ctags-mode-faces)

(defface ctags-file-face
    `((t (:foreground ,+solarized-yellow+)))
  "Face to highlight warnings like error, assert."
  :group 'ctags-mode-faces)

(defface ctags-regexp-face
    `((t (:foreground ,+solarized-cyan+)))
  "Face to highlight warnings like error, assert."
  :group 'ctags-mode-faces)

(defface ctags-aux-face
    `((t (:foreground ,+solarized-orange+)))
  "Face to highlight auxiliary words, like bind:, access: etc."
  :group 'ctags-mode-faces)

(defconst +ctags-mode-font-lock-keywords+
  `((,+ctags-line-re+
     (1 'ctags-symbol-face)
     (2 'ctags-file-face)
     (3 'ctags-regexp-face))
    (,(rx (group bow
                 (or "kind"
                     "access"
                     "class"
                     "file"
                     "signature"
                     "namespace"))
          ":")
     (1 'ctags-aux-face))))


(defun ctags-go-to-name-at-point ()
  "Jump to location pointed at by current ctags line."
  (interactive)
  (save-excursion
   (save-match-data
    (beginning-of-line)
    (if (looking-at +ctags-line-re+)
      (begin
        (message "(match-string-no-properties 3): %s"
                 (pp-to-string (match-string-no-properties 3)))
        (let ((line (aif (match-string-no-properties 3)
                      (string->number it)
                      nil))
              (buf (find-file-noselect (match-string-no-properties 2))))
          (pop-to-buffer buf)
          (with-current-buffer buf
            (if line
              (goto-line line)
              (error "cannot use ctags format with regexps")))))
      (error "not on ctags line")))))

(defvar ctags-mode-map
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      ("<return>" ctags-go-to-name-at-point)
      ("M-."      ctags-go-to-name-at-point))
    map))

(define-derived-mode ctags-mode text-mode "Ctags"
  "Major mode for exloring and navigating ctags tags."
  (setq-local comment-style 'indent)
  (setq-local comment-start "!_TAG_[^ \t\n]+")
  (setq-local comment-end "")
  (setq-local comment-padding " ")
  (setq-local comment-start-skip "!_TAG_[^ \t\n]+ *")
  (setq-local comment-use-global-state t)

  (setf font-lock-defaults
        '(+ctags-mode-font-lock-keywords+
          nil
          ;; be case-sensetive
          nil)))


(add-to-list 'auto-mode-alist
             '("tags\\'" . ctags-mode))

(provide 'ctags-mode)

;; Local Variables:
;; End:

;; ctags-mode.el ends here
