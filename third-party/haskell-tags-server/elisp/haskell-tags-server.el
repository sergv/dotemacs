;; haskell-tags-server.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  3 October 2016
;; Description:

(require 'dash)
(require 'haskell-mode)
(require 'queue)

(require 'select-mode)

(defgroup haskell-tags-server nil
  "Navigate Haskell sources using tags server."
  :group 'haskell
  :link '(url-link :tag "Github" "https://github.com/sergv/haskell-tags-server"))

(defcustom haskell-tags-server-executable "haskell-tags-server"
  "Path to the haskell-tags-server executable."
  :group 'haskell-completions
  :type 'stringp)

(defcustom haskell-tags-server-port 4872
  "Port to user for connecting to Haskell tags server."
  :group 'haskell-completions
  :type 'integerp)

(defcustom haskell-tags-server-extra-args nil
  "Extra arguments to supply to started haskell-tags-server process."
  :group 'haskell-completions
  :type 'integerp)

(defconst haskell-tags-server--network-buffer-name " *haskell-tags-server-network*")
(defconst haskell-tags-server--subprocess-buffer-name " *haskell-tags-server-subprocess*")

(defvar haskell-tags-server--network-buffer nil)
(defvar haskell-tags-server--subprocess-buffer nil)
(defvar haskell-tags-server--subprocess-proc nil)

(defvar haskell-tags-server--reported-shallow-dirs   (make-hash-table :test #'equal))
(defvar haskell-tags-server--reported-recursive-dirs (make-hash-table :test #'equal))
(defvar haskell-tags-server--reported-ignored-globs  (make-hash-table :test #'equal))

(defun haskell-tags-server--connect (buf)
  (let ((proc (haskell-tags-server--open-socket buf)))
    (if proc
        proc
      (progn
        (when haskell-tags-server--subprocess-buffer
          (kill-buffer haskell-tags-server--subprocess-buffer))
        (setf haskell-tags-server--subprocess-buffer
              (get-buffer-create haskell-tags-server--subprocess-buffer-name))
        (let ((server-proc (make-process
                            :name "haskell-tags-server"
                            :buffer haskell-tags-server--subprocess-buffer
                            :command (append
                                      (list haskell-tags-server-executable
                                            "--port"
                                            (number-to-string haskell-tags-server-port)
                                            "--verbosity"
                                            "error"
                                            ;; "--eager-tagging"
                                            )
                                      haskell-tags-server-extra-args)
                            :noquery t
                            :sentinel #'haskell-tags-server--subprocess-sentinel)))
          (let ((tries 50)
                (done nil))
            (while (and (not proc)
                        (/= tries 0))
              (setf tries (- tries 1)
                    proc (haskell-tags-server--open-socket buf))
              (sleep-for 0.1))
            (if proc
                (progn
                  (setf haskell-tags-server--subprocess-proc server-proc)
                  proc)
              (let ((output (with-current-buffer haskell-tags-server--subprocess-buffer
                              (buffer-substring-no-properties (point-min) (point-max)))))
                (delete-process server-proc)
                (kill-buffer haskell-tags-server--subprocess-buffer)
                (error "Failed to initiate connection to the freshly created haskell-tags-server subprocess. Process' output:\n%s"
                       output)))))))))

(defun haskell-tags-server--subprocess-sentinel (proc event-description)
  (unless (memq (process-status proc) '(run open))
    (message "The haskell-tags-server subprocess terminated: %s" event-description)))

(defun haskell-tags-server--open-socket (buf)
  (condition-case err
      (make-network-process
       :name "haskell-tags-server"
       :host 'local
       :service haskell-tags-server-port
       :family 'ipv4
       :buffer buf
       :coding 'no-conversion
       :filter-multibyte nil
       ;; Don't query to close this process on Emacs exit.
       :noquery t
       :sentinel #'haskell-tags-server--network-sentinel)
    (file-error nil)))

(defun haskell-tags-server--blocking-call (func-name args)
  (let* ((done nil)
         (i 0)
         (result (list nil))
         (proc
          (haskell-tags-server--call func-name
                                     args
                                     (lambda (res)
                                       (setf done t)
                                       (setcar result res)))))
    (while (and (not done)
                (< i (eval-when-compile (* 2 60 100))))
      (setf i (+ i 1))
      (sit-for 0.01)
      ;; (when (eval-when-compile (fboundp #'thread-yield))
      ;;   (thread-yield))
      )
    (if done
        (car result)
      (error "Failed to get response from server within 2 minutes"))))


;; (defvar-local haskell-tags-server--request nil
;;   "A string request to send once connection will be established.")
(defvar-local haskell-tags-server--response-handler nil
  "A function of single argument - string response.")

(defun haskell-tags-server--call (func-name args callback)
  (cl-assert (symbolp func-name))
  (cl-assert (listp args))
  (let ((buf (generate-new-buffer haskell-tags-server--network-buffer-name))
        (invocation (list func-name args)))
    (with-current-buffer buf
      (setq-local haskell-tags-server--response-handler callback)
      ;; Create process and make a request once a connection will be
      ;; established (i.e. socket will be opened).
      (let ((socket (haskell-tags-server--connect buf)))
        (process-send-string socket (haskell-tags-server--encode-request invocation))
        (process-send-eof socket)
        socket))))

(defun haskell-tags-server--encode-request (x)
  (let ((print-length nil)
        (print-level nil))
    (prin1-to-string x)))

(defun haskell-tags-server--decode-response (encoded)
  (read encoded))

(defun haskell-tags-server--network-sentinel (proc event-description)
  (pcase event-description
    ;; Gets sent by `delete-process'.
    ;; ("deleted\n")
    ("open\n")
    ("connection broken by remote peer\n" ;; Socket was closed OR process died
     (unwind-protect
         (let* ((buf (process-buffer proc))
                (handler (buffer-local-value 'haskell-tags-server--response-handler
                                             buf))
                (response (with-current-buffer buf
                            (buffer-substring-no-properties (point-min) (point-max)))))
           (when handler
             (cl-assert (functionp handler))
             (funcall handler response)))
       (progn
         (delete-process proc)
         (kill-buffer (process-buffer proc)))))
    (_
     (delete-process proc)
     (kill-buffer (process-buffer proc)))))

(defun haskell-tags-server--trim-whitespace (str)
  "Trim leading and tailing whitespace from STR."
  (when str
    (cl-assert (stringp str))
    (save-match-data
      (replace-regexp-in-string "\\(?:\\`[ \t\v\f\r\n]*\\|[ \t\v\f\r\n]*\\'\\)" "" str))))

(defvar haskell-tags-server--haskell-symbol-re
  (rx (or (group (+ ;; (regexp "[-!#$%&*+./<=>?@^|~:\\]")
                  (any ?\- ?\! ?\# ?\$ ?\% ?\& ?\* ?\+ ?\. ?\/ ?\< ?\= ?\> ?\? ?\@ ?^ ?\| ?\~ ?\: ?\\ )))
          (group
           (seq
            ;; Optional qualification - important to resolve qualified imports.
            (? upper
               (* (char alnum ?_ ))
               ".")
            ;; Allow _ as a first char to fit GHC.
            (or (regexp "\\<[_a-z]")
                ;; Allow ' preceding conids because of DataKinds/PolyKinds.
                (regexp "'*[A-Z]")
                (syntax word))
            (group
             (* (regexp "\\(?:['a-zA-Z_0-9#]\\|\\sw\\)")))))))
  "Regexp to recognize haskell symbols as generic entities for search
(with e.g. \"*\" in vim).")

(defvar haskell-tags-server--identifier-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?#  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    (modify-syntax-entry ?\' "w" tbl)
    (modify-syntax-entry ?,  "/" tbl) ;; Disable , since it's part of syntax
    (modify-syntax-entry ?.  "_" tbl) ;; So that we match qualified names.
    tbl)
  "Special syntax table for haskell that allows to recognize symbols that contain
both unicode and ascii characters.")


(defun haskell-tags-server--bounds-of-haskell-symbol ()
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (save-excursion
    (save-match-data
      (with-syntax-table haskell-tags-server--identifier-syntax-table
        (forward-char 1)
        (let ((start nil)
              (end nil)
              (beginning-quotes "'"))
          (if (zerop (skip-syntax-backward "w_"))
              (progn
                (skip-syntax-backward "._")
                ;; To get qualified part
                (skip-syntax-backward "w_")
                (skip-chars-forward beginning-quotes))
            (progn
              (skip-chars-forward beginning-quotes)))
          (setf start (point))
          (when (looking-at (rx (+ (char upper) (* (char alnum ?_)) ".")))
            (goto-char (match-end 0)))
          (when (zerop (skip-syntax-forward "w_"))
            (skip-syntax-forward "._"))
          (setf end (point))
          (cons start end))))))

(defun haskell-tags-server--identifier-at-point ()
  "Grab a Haskell identifier around current point."
  (if (region-active-p)
      (haskell-tags-server--trim-whitespace
       (buffer-substring-no-properties
        (region-beginning)
        (region-end)))
    (let ((bounds (haskell-tags-server--bounds-of-haskell-symbol)))
      (cond (bounds
             (buffer-substring-no-properties (car bounds)
                                             (cdr bounds)))
            (t
             (error "No identifier at point found"))))))


(defvar haskell-tags-server--previous-homes nil
  "Previous locations from which symbol search was invoked.")

(defvar haskell-tags-server--selected-loc nil
  "Home entry corresponding to the most recently visited tag.")

(defvar haskell-tags-server--next-homes nil
  "Next locations that were visited but now obscured by going back.")

(defun make-haskell-tags-server-home-entry (buffer point symbol is-regex)
  (cl-assert (bufferp buffer))
  (list buffer point symbol is-regex))

(defun haskell-tags-server-home-entry--buffer (entry)
  (car entry))

(defsetf haskell-tags-server-home-entry--buffer (entry) (val)
  `(setf (car ,entry) ,val))

(defun haskell-tags-server-home-entry--point (entry)
  (cadr entry))

(defun haskell-tags-server-home-entry--search-query (entry)
  (caddr entry))

(defun haskell-tags-server-home-entry--is-regex? (entry)
  (cadddr entry))

(defun haskell-tags-server--home-entry=? (entry-a entry-b)
  (and (eq (haskell-tags-server-home-entry--buffer entry-a)
           (haskell-tags-server-home-entry--buffer entry-b))
       (= (haskell-tags-server-home-entry--point entry-a)
          (haskell-tags-server-home-entry--point entry-b))
       ;; (eq (haskell-tags-server-home-entry--search-query entry-a)
       ;;     (haskell-tags-server-home-entry--search-query entry-b))
       ))

(defun haskell-tags-server--switch-to-home-entry (home-entry)
  (unless (buffer-live-p (haskell-tags-server-home-entry--buffer home-entry))
    (setf (haskell-tags-server-home-entry--buffer home-entry)
          (find-file-noselect
           (buffer-file-name (haskell-tags-server-home-entry--buffer home-entry)))))
  (switch-to-buffer (haskell-tags-server-home-entry--buffer home-entry))
  (goto-char (haskell-tags-server-home-entry--point home-entry)))


(defun haskell-tags-server--strip-qualified-prefix (str)
  (save-match-data
    (if (string-match "^\\([[:upper:]][[:alnum:]]*\\.\\)+" str)
        (replace-match "" nil nil str)
      str)))

(defun haskell-tags-server--goto-loc (filename line prev-loc search-query use-regexp? visit-file-name)
  "Go to a given position in a given file and maintain navigation information."
  (cl-assert (stringp filename))
  (cl-assert (numberp line))
  (cl-assert (stringp search-query))
  (unless (file-exists-p filename)
    (error "File %s does not exist" filename))
  (funcall visit-file-name filename)
  ;; Remove any narrowing because `line' is an absolute line number in
  ;; a file (counted relative to the beginning of the file, not to the
  ;; beginning of the accessible portion of the buffer).
  (save-restriction
    (widen)
    (goto-line line)
    (save-match-data
      (when (re-search-forward
             (if use-regexp?
                 (concat "\\<" (regexp-quote search-query) "\\>")
               ;; Must strip qualification prefix here in order to be able
               ;; to locate bare name within current module.
               (haskell-tags-server--strip-qualified-prefix search-query))
             (line-end-position)
             t)
        (goto-char (match-beginning 0))))

    (push prev-loc haskell-tags-server--previous-homes)
    (setf haskell-tags-server--selected-loc
          (make-haskell-tags-server-home-entry (current-buffer)
                                               (point-marker)
                                               search-query
                                               use-regexp?)
          haskell-tags-server--next-homes nil)))

;;;###autoload
(defun haskell-tags-server-finish ()
  (interactive)
  (haskell-tags-server--blocking-call 'finish nil)
  (message "Successfully finished haskell tags server"))

(defun haskell-tags-server-finish-started-subprocess ()
  (when (and haskell-tags-server--subprocess-proc
             (eq (process-status haskell-tags-server--subprocess-proc) 'run))
    (haskell-tags-server-finish)))

(add-hook 'kill-emacs-hook #'haskell-tags-server-finish-started-subprocess)

;;;###autoload
(defun haskell-tags-server-go-back ()
  (interactive)
  (if (null haskell-tags-server--previous-homes)
      (error "No more previous go-to-definition entries")
    (progn
      (when (or (null haskell-tags-server--next-homes)
                (and haskell-tags-server--next-homes
                     (not (haskell-tags-server--home-entry=?
                           haskell-tags-server--selected-loc
                           (car haskell-tags-server--next-homes)))))
        (push haskell-tags-server--selected-loc haskell-tags-server--next-homes))
      (let ((prev-home (pop haskell-tags-server--previous-homes)))
        (setf haskell-tags-server--selected-loc prev-home)
        (haskell-tags-server--switch-to-home-entry prev-home)))))

;;;###autoload
(defun haskell-tags-server-goto-definition (is-local? use-regexp? namespace)
  "Go to the definition of a name at point."
  (let* ((buffer-name (buffer-name))
         (curr-buf (current-buffer))
         (filename (buffer-file-name))
         (identifier (if use-regexp?
                         (read-regexp "Enter regexp to search for"
                                      (ignore-errors
                                        (haskell-tags-server--identifier-at-point))
                                      'haskell-tags-server-regexp-history)
                       (haskell-tags-server--identifier-at-point)))
         (search-func
          (if use-regexp?
              'find-regex
            'find))
         (temp-file-name nil)
         (target (if filename
                     filename
                   (progn
                     (setf temp-file-name
                           (make-temp-file
                            "tags-query"
                            nil ;; not a directory
                            nil ;; no suffix
                            (buffer-substring-no-properties
                             (point-min)
                             (point-max)) ;; initial contents
                            ))
                     (command-line-normalize-file-name temp-file-name))))
         (args (list target
                     identifier
                     (if is-local? 'local 'global)
                     namespace))
         (next-home-entry
          (car-safe haskell-tags-server--next-homes)))
    (let ((current-home-entry
           (make-haskell-tags-server-home-entry (current-buffer)
                                                (point-marker)
                                                nil
                                                nil)))
      (unless target
        (error "Could not come up with a target file name. Aborting"))
      ;; Try to avoid search if there's entry in the next homes that
      ;; matches current query.
      (if (and next-home-entry
               (let ((next-symbol
                      (haskell-tags-server-home-entry--search-query next-home-entry))
                     (next-symbol-regex?
                      (haskell-tags-server-home-entry--is-regex? next-home-entry)))
                 (if use-regexp?
                     (if next-symbol-regex?
                         nil
                       (string-match-p identifier next-symbol))
                   (if next-symbol-regex?
                       nil
                     (string= identifier next-symbol)))))
          (progn
            (haskell-tags-server--switch-to-home-entry next-home-entry)
            (push current-home-entry
                  haskell-tags-server--previous-homes)
            (setf haskell-tags-server--selected-loc
                  (pop haskell-tags-server--next-homes)))
        (let ((response
               (haskell-tags-server--blocking-call search-func args)))
          (haskell-tags-server--handle-reply
           response
           current-home-entry
           identifier
           use-regexp?
           (if temp-file-name
               (lambda (str)
                 (cl-assert (stringp str))
                 (if (string= str temp-file-name)
                     (concat "buffer:" buffer-name)
                   str))
             #'identity)
           (if temp-file-name
               (lambda (str)
                 (cl-assert (stringp str))
                 (if (string= str temp-file-name)
                     (switch-to-buffer curr-buf)
                   (find-file str)))
             #'find-file)))))))


(defmacro haskell-tags-server--for-buffer-with-file (filename &rest body)
  "Execute BODY in buffer with contents of FILENAME. If FILENAME is already
opened in some buffer, then reuse it, and insert its contents in temporary
buffer if no such buffer exists."
  (declare (indent 1))
  (let ((buf-var (gensym))
        (exec-func (gensym)))
    `(let ((,exec-func (lambda () ,@body)))
       (let ((,buf-var (get-file-buffer ,filename)))
         (if ,buf-var
             (with-current-buffer ,buf-var
               (funcall ,exec-func))
           (with-temp-buffer
             (insert-file-contents ,filename
                                   t ;; make current buffer visit inserted file
                                   )
             (funcall ,exec-func)))))))

(defsubst haskell-tags-server--loc-entry--name (entry)
  (car entry))

(defsubst haskell-tags-server--loc-entry--filename (entry)
  (cadr entry))

(defsubst haskell-tags-server--loc-entry--line (entry)
  (caddr entry))

(defsubst haskell-tags-server--loc-entry--type (entry)
  (cadddr entry))

(defun haskell-tags-server--handle-reply (response prev-loc search-query use-regexp? show-file-name visit-file-name)
  (cl-assert (stringp response))
  (pcase (haskell-tags-server--decode-response response)
    (`(ok (loc-known ,loc-entry))
     (haskell-tags-server--goto-loc
      (haskell-tags-server--loc-entry--filename loc-entry)
      (haskell-tags-server--loc-entry--line loc-entry)
      prev-loc
      search-query
      use-regexp?
      visit-file-name))
    (`(ok (loc-ambiguous ,entries))
     (cl-assert (listp entries))
     (select-mode-start-selection
      entries
      :buffer-name "Symbols"
      :after-init #'select-mode-setup
      :on-selection
      (lambda (idx loc-entry _selection-type)
        (select-mode-exit)
        (haskell-tags-server--goto-loc
         (haskell-tags-server--loc-entry--filename loc-entry)
         (haskell-tags-server--loc-entry--line loc-entry)
         prev-loc
         search-query
         use-regexp?
         visit-file-name))
      :item-show-function
      (lambda (symbol-entry)
        (haskell-tags-server--tag->string (haskell-tags-server--loc-entry--name symbol-entry)
                                          (funcall show-file-name
                                                   (haskell-tags-server--loc-entry--filename symbol-entry))
                                          (haskell-tags-server--loc-entry--line symbol-entry)
                                          (haskell-tags-server--loc-entry--type symbol-entry)))
      :preamble
      (format "Results for %s '%s'\n\n"
              (if use-regexp? "regexp" "identifier")
              search-query)))
    (`(ok not-found)
     (message "No entries for %s %s"
              (if use-regexp? "regexp" "identifier")
              search-query))
    (`(error ,data)
     (error "haskell-tags-server error: %s" data))
    (unexpected
     (error "Unexpected reply from haskell-tags-server: %s" unexpected))))


(defun haskell-tags-server--tag->string (name filename line type)
  (let* ((type-str (symbol-name type))
         (is-module? (string= type-str "Module")))
    (concat name
            " ["
            type-str
            "]\n"
            filename
            ":"
            (number-to-string line)
            "\n"
            (if is-module?
                ""
              (concat
               (haskell-tags-server--extract-haskell-tag-signature filename line)
               "\n")))))

(defun haskell-tags-server--extract-haskell-tag-signature (filename line)
  (haskell-tags-server--for-buffer-with-file
   filename
   (save-excursion
     (goto-char (point-min))
     (forward-line (1- line))
     (haskell-tags-server--extract-haskell-block)
     ;; alternative implementation with regexps
     ;; (save-match-data
     ;;   (goto-line1 line)
     ;;   (if (looking-at "^\\([^ \t\n\r\f\v].* ::\\(?: .*\n\\|\n\\)\\(?:^[ \t]+.+\n\\)*\\)")
     ;;     (match-string-no-properties 1)
     ;;     (current-line)))
     )))

(defun haskell-tags-server--extract-haskell-block ()
  "Extract indented Haskell block that starts on the current line."
  (beginning-of-line)
  (let ((start (point)))
    (cl-symbol-macrolet
        ((advance
          (progn
            (forward-line 1)
            (beginning-of-line)
            (skip-chars-forward " \t"))))
      (skip-chars-forward " \t")
      (let ((col (current-column)))
        ;; actualy this is a loop with postcondition
        advance
        (while (< col (current-column))
          advance)
        (let ((previous-line-end (line-end-position 0)))
          (buffer-substring-no-properties start previous-line-end))))))

(provide 'haskell-tags-server)

;; Local Variables:
;; End:

;; haskell-tags-server.el ends here
