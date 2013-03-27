;; ctags-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 27 December 2012
;; Description:

(eval-when-compile
  (require 'cl))
(require 'solarized+)
(require 'eproj)

(defvar *ctags-exec* ;; "ctags-exuberant"
  (concat +emacs-config-path+ "/tmp/python/ctags-5.8/ctags"))

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
     "\\.\\(?:py\\|pyx\\|pxd\\|pxi\\)\\'")
    (java-mode
     "\\.\\(?:java\\)\\'")))



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
        (dolist (file files)
          (when (string-match-pure? ext-re file)
            (insert file "\n")))
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

;; vector of filenames
(defvar *ctags-file-sequence*
  (make-vector 2 nil)
  "Vector if filenames read from tags")
;; hash table of (filename . id) bindings
(defvar *ctags-file-idxs*
  (make-hash-table :test #'equal)
  "Mapping from filenames to their respective indices in `*ctags-file-sequence*'.")

(defun ctags/latest-defined-index (vect)
  (let ((i 0))
    (while (and (< i (length vect))
                (aref vect i))
      (setf i (+ i 1)))
    (- i 1)))

(defun ctags/grow-vector (vect idx item)
  (if (<= (length vect) idx)
    (let ((v (make-vector (length vect) nil)))
      (setf (aref v 0) item)
      (vconcat vect v))
    (begin
      (setf (aref vect idx) item)
      vect)))

(defun ctags-file->id (file)
  "Get id for FILE."
  (gethash file *ctags-file-idxs*))

(defun ctags-tag-file (tag)
  "Fetch file where TAG is supposedly defined."
  (aref *ctags-file-sequence* (ctags-tag-file-idx tag)))


(defstruct ctags-tag
  symbol
  file-idx
  line
  kind
  aux-fields ;; alist of cons pairs
  )

(defun ctags-get-tags-from-buffer (buffer &optional root)
  "Returns hash-table of (tag . ctags-tag) bindings parsed from buffer BUFFER."
  (with-current-buffer buffer
    (save-match-data
     (goto-char (point-min))
     (let ((tags-table (make-hash-table :test #'equal)))
       (while (not (eob?))
         (when (looking-at +ctags-line-re+)
           (let ((symbol (match-string-no-properties 1))
                 (file (concat (when root (concat root "/"))
                               (match-string-no-properties 2)))
                 (line (string->number (match-string-no-properties 3))))
             (goto-char (match-end 0))
             ;; now we're past ;"
             (let ((fields
                     (delq
                      nil
                      (map (lambda (entry)
                             (if (string-match? (concat "^\\("
                                                        +ctags-aux-fields-re+
                                                        "\\):\\(.*\\)$")
                                                entry)
                               (when (< 0 (length (match-string-no-properties 2 entry)))
                                 (cons (string->symbol
                                        (match-string-no-properties 1 entry))
                                       (match-string-no-properties 2 entry)))
                               (error "invalid entry: %s" entry)))
                           (split-string (buffer-substring-no-properties
                                          (point)
                                          (line-end-position))
                                         "\t"
                                         t)))))
               (unless (ctags-file->id file)
                 (let ((file-idx (+ (ctags/latest-defined-index *ctags-file-sequence*)
                                    1)))
                   (setf *ctags-file-sequence* (ctags/grow-vector *ctags-file-sequence*
                                                                  file-idx
                                                                  file))
                   (puthash file file-idx *ctags-file-idxs*)))
               (let ((new-tag (make-ctags-tag
                               :symbol     symbol
                               :file-idx   (ctags-file->id file)
                               :line       line
                               :kind       (cdr (assoc* 'kind fields))
                               :aux-fields (filter (lambda (x)
                                                     (not (eq? 'kind (car x))))
                                                   fields))))
                 (puthash symbol
                          (cons new-tag
                                (gethash symbol
                                         tags-table
                                         nil))
                          tags-table)))))
         (forward-line 1))
       tags-table))))

(defun eproj-load-single-ctags-project (root)
  (let* ((proj (eproj-get-project root)))
    (setf (eproj-project-names proj) nil)
    (dolist (lang (eproj-project-languages proj))
      (let ((ctags-buf (get-buffer-create (concat " *"
                                                  (eproj-project-root proj)
                                                  "-ctags-"
                                                  (symbol->string lang)
                                                  "*"))))
        (with-current-buffer ctags-buf
          (cd root)
          (erase-buffer)
          ;; (ctags-mode)
          )
        (run-ctags-on-files lang
                            (eproj-project-root proj)
                            (eproj-get-project-files proj)
                            ctags-buf)

        (push (cons lang (ctags-get-tags-from-buffer ctags-buf root))
              (eproj-project-names proj))
        (kill-buffer ctags-buf)))))

(defun eproj-load-ctags-project (proj)
  "Reload project PROJ and all it's related projects."
    (map (lambda (root)
           (eproj-load-single-ctags-project root))
         (cons (eproj-project-root proj)
               (eproj-get-all-related-projects (eproj-project-root proj)))))

(defun eproj-reload-projects ()
  (interactive)
  (maphash (lambda (root proj)
             (eproj-load-single-ctags-project root))
           *eproj-projects*))

(defun eproj-reset-projects ()
  (interactive)
  (setf *eproj-projects* (make-hash-table :test #'equal)))


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
    (if (looking-at? +ctags-line-re+)
      (let ((line (aif (match-string-no-properties 3)
                    (string->number it)
                    nil))
            (buf (find-file-noselect (match-string-no-properties 2))))
        (pop-to-buffer buf)
        (with-current-buffer buf
          (if line
            (goto-line line)
            (error "cannot use ctags format with regexps"))))
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
