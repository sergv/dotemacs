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

(defvar *ctags-language-flags*
  '((c-mode
     "--language-force=c"
     "--c-kinds=-cdefgmnpstuv"
     "--c-kinds=+sv"
     "--fields=+SzkK"
     "--extra=+q")
    (c++-mode
     "--c++-kinds=+cdefgmnpstuv"
     "--fields=+iaSzkK"
     "--extra=+q")
    (python-mode
     "--language-force=python"
     "--python-kinds=+cfmvi"
     "--fields=+SzkK")))

(defvar *ctags-language-extensions*
  `((c-mode
     "\\.[ch]\\'")
    (c++-mode
     "\\.\\(?:c\\|cc\\|cxx\\|cpp\\|c++\\|h\\|hh\\|hxx\\|hpp\\|h++\\|inl\\|inc\\|incl\\)\\'")
    (python-mode
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

(defconst +ctags-aux-fields+
  '("kind"
    "access"
    "class"
    "file"
    "signature"
    "namespace"
    "struct"
    "enum"
    "union"
    "inherits"
    "typeref"
    "function"))

(defconst +ctags-aux-fields-re+
  (eval-when-compile
    (macroexpand
     `(rx (or ,@+ctags-aux-fields+)))))


(defun run-ctags-on-files (mjr-mode root-dir files out-buffer)
  (with-current-buffer out-buffer
    (goto-char (point-max))
    (unless (looking-at-pure? "^$")
      (insert "\n"))
    (let ((ext-re (cadr (assq mjr-mode *ctags-language-extensions*))))
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
               (cdr (assq mjr-mode *ctags-language-flags*)))))))

;; hash table of (project-root . names) bindings
(defvar *ctags-projects*
  (make-hash-table :test #'equal))

(defstruct ctags-tag
  symbol
  file
  line
  kind
  aux-fields ;; alist of cons pairs
  )

(defun proj-get-project-root ()
  (when *have-git?*
    (git-update-file-repository))
  (or git-repository
      (and (buffer-file-name) (file-name-directory (buffer-file-name)))
      default-directory))

(defun proj-get-project-ctags-symbols (load-symbols-func)
  (let ((root (proj-get-project-root)))
    (aif (gethash root
                  *ctags-projects*
                  nil)
      it
      (begin
        (funcall load-symbols-func)
        (aif (gethash root
                  *ctags-projects*
                  nil)
          it
          (error "Cannot load ctags symbols for %s project" root))))))

(defun proj-load-ctags-project (project-root lang)
  "Exctract ctags names from C++ project current buffer's file is part of."
  (let ((ctags-buf (get-buffer-create (concat "*"
                                              (if git-repository
                                                git-repository
                                                (buffer-file-name))
                                              "-ctags*"))))
    (with-current-buffer ctags-buf
      (cd project-root)
      (erase-buffer))
    (run-ctags-on-files lang
                        project-root
                        (if git-repository
                          (git-get-tracked-files git-repository)
                          (list (buffer-file-name)))
                        ctags-buf)
    (pop-to-buffer ctags-buf)
    (ctags-mode)
    (save-excursion
     (save-match-data
      (goto-char (point-min))
      (let ((tags-table (make-hash-table :test #'equal)))
        (while (not (eob?))
          (when (looking-at +ctags-line-re+)
            (let ((symbol (match-string-no-properties 1))
                  (file (match-string-no-properties 2))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"

              (let ((fields
                      (delq
                       nil
                       (mapcar (lambda (entry)
                                 (if (string-match? (concat "^\\("
                                                            +ctags-aux-fields-re+
                                                            "\\):\\(.*\\)$")
                                                    entry)
                                   (aif (< 0 (length (match-string-no-properties 2 entry)))
                                     (cons (string->symbol
                                            (match-string-no-properties 1 entry))
                                           (match-string-no-properties 2 entry))
                                     nil)
                                   (error "invalid entry: %s" entry)))
                               (split-string (buffer-substring-no-properties
                                              (point)
                                              (line-end-position))
                                             "\t"
                                             t)))))
                (puthash symbol
                         (cons (make-ctags-tag
                                :symbol symbol
                                :file file
                                :line line
                                :kind (cdr (assoc* 'kind fields))
                                :aux-fields (filter (lambda (x)
                                                      (not (eq? 'kind (car x))))
                                                    fields))
                               (gethash symbol
                                        tags-table
                                        nil))
                         tags-table))))
          (forward-line 1))
        (puthash project-root
                 tags-table
                 *ctags-projects*))))))



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
  (eval-when-compile
    `((,+ctags-line-re+
       (1 'ctags-symbol-face)
       (2 'ctags-file-face)
       (3 'ctags-regexp-face))
      (,(eval `(rx (group bow
                          (or ,@+ctags-aux-fields+))
                   ":"))
       (1 'ctags-aux-face)))))


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
