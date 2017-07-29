;;; shm-ast.el --- AST functions

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'shm-customizations)
(require 'shm-node)
(require 'shm-in)
(require 'shm-overlays)

(require 'ring)
(require 'cl)
(require 'cl-lib)

(defvar shm-lighter " SHM?"
  "The lighter for structured Haskell mode.")

(defvar shm-decl-asts nil
  "This is partly an optimization and partly for more
functionality. We could parse the whole module, but that would be
wasteful and expensive to lookup nodes every time we want a
node. So it's cheaper to have the granularity of lookup start at
the declaration's point and the node's span.

Second it's better because a module may have unparseable content
in it, but that doesn't mean we don't want structured editing to
stop working on declarations that are fine. I've found in my use
of SHM that this is a common use-case worth taking into account.")

(defvar shm-last-parse-start 0
  "This is used to avoid unnecessary work, if the start of the
  declaration hasn't changed, and the end (see
  `shm-last-parse-end') since we last parsed, don't bother
  re-parsing.")

(defvar shm-last-parse-end 0
  "See `shm-last-parse-start' for explanation.")

(defvar shm-history-stack nil
  "Stack for story node history.")

(defcustom shm-history-stack-max-length
  10
  "Maximum length of the node history stack."
  :group 'shm
  :type 'integer)

(defun shm/reparse ()
  "Re-parse the current node.

This is used on the reparsing timer, but also on commands that
really need accurate AST information *right now*, so this will
force a reparse immediately (if necessary)."
  (interactive)
  (shm-decl-ast t)
  (when (/= shm-last-point (point))
    (shm-set-node-overlay)))

(defun shm-decl-ast (&optional reparse)
  "Return the AST representing the current declaration at point.

If the AST has already been loaded, that is returned immediately,
otherwise it's regenerated. See the Internal AST section below
for more information."
  (let ((p (shm-decl-points)))
    (when p
      (shm-get-decl-ast (car p)
                        (cdr p)
                        reparse))))

(defun shm-set-decl-ast (point ast)
  "Store the given decl AST at the given POINT. If there is
already an AST for a decl at the given point then remove that one
and instate this one."
  (setq shm-decl-asts
        (cons
         (cons (set-marker (make-marker) point) ast)
         (remove-if (lambda (pair)
                      (when (= (marker-position (car pair))
                               point)
                        (set-marker (car pair) nil)
                        t))
                    shm-decl-asts)))
  ast)

(defun shm-get-decl-ast (start end &optional reparse)
  "Get the AST of the declaration starting at POINT."
  (let ((pair (car (remove-if-not (lambda (decl-pair)
                                    (= (marker-position (car decl-pair))
                                       start))
                                  shm-decl-asts))))
    (if (and (not reparse)
             pair)
        (cdr pair)
      (progn
        (when (or (/= start shm-last-parse-start)
                  (/= end shm-last-parse-end))
          (setq shm-last-parse-start start)
          (setq shm-last-parse-end end)
          (let ((spans (shm-get-ast (if (bound-and-true-p structured-haskell-repl-mode)
                                        'stmt
                                      'decl)
                                    start end)))
            (when spans
              (when (bound-and-true-p structured-haskell-repl-mode)
                (shm-font-lock-region start end))
              (let ((ast (shm-get-nodes spans start end)))
                (cl-assert (not (null ast)))
                (modeline-set-syntax-check-result 'ok)
                (when pair
                  (shm-delete-markers pair))
                (shm-set-decl-ast start ast)
                ;; Delete only quarantine overlays.
                (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
                (shm/init)
                ast))))))))

(defun shm-font-lock-region (start end)
  "When in a REPL, we don't typically have font locking, so we
  should manually perform a font-lock whenever we get a valid
  parse."
  (unless (= (1+ start) end)
    (let ((point (point))
          (inhibit-modification-hooks t)
          (list buffer-undo-list)
          (string (buffer-substring-no-properties start end)))
      (unless (string-match "^:" string)
        (let ((fontified (shm-fontify-as-mode string
                                              'haskell-mode))
              (overlays (mapcar (lambda (o)
                                  (list o
                                        (overlay-start o)
                                        (overlay-end o)))
                                (overlays-in start end))))
          (delete-region start end)
          (insert fontified)
          (goto-char point)
          ;; Restore overlay positions
          (loop for o in overlays
                do (move-overlay (nth 0 o) (nth 1 o) (nth 2 o)))
          (setq buffer-undo-list list))))))

(defun shm-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert "x=" text)
    (font-lock-fontify-buffer)
    (buffer-substring (+ (point-min) (length "x=")) (point-max))))

(defun shm--strip-preprocessor (source-code)
  (save-match-data
    (with-temp-buffer
      (insert source-code)
      (goto-char (point-min))
      (while (search-forward-regexp "^#.*$" nil t)
        (replace-match ""))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defcustom shm-server-executable "structured-haskell-mode-server"
  "Range of ports to use when starting `structured-haskell-mode-server'
executable from the Emacs.")

(defcustom shm-server-port-range '(40000 . 42000)
  "Range of ports to use when starting `structured-haskell-mode-server'
executable from the Emacs.")

(defcustom shm-server-default-port 8132
  "Default port to use for connecting to already running `structured-haskell-mode-server'
instance.")

(defvar shm--server-connection-process nil)
(defvar shm--server-process nil)
(defvar shm--server-process-port nil)

(defun shm--connect-bert-rpc (port)
  (bert-rpc-connect
   :name "structured-haskell-mode"
   :port port
   :buffer (get-buffer-create " *structured-haskell-mode-interaction*")
   :interaction 'synchronous))

(defun shm--start-server-on-port (port)
  "Try to start executable with random port and connect to it. Returns port,
sets `shm--server-process' and `shm--server-process-port' variables."
  (when (or (not shm--server-process-port)
            (and shm--server-process
                 (not (eq 'run (process-status shm--server-process)))))
    (message "Starting shm server...")
    (setf shm--server-process
          (with-temp-buffer
            (let ((proc
                   (make-process
                    :name "structured-haskell-mode-server"
                    :buffer (current-buffer)
                    :command (list shm-server-executable
                                   "--port"
                                   (number->string port))
                    :noquery t)))
              (when (or (not proc)
                        (not (eq 'run (process-status proc))))
                (error "Failed to start %s executable: %s"
                       shm-server-executable
                       (buffer-substring-no-properties (point-min)
                                                       (point-max))))
              (set-process-buffer proc nil)
              proc))
          shm--server-process-port port)
    (sleep-for 1))
  shm--server-process-port)

(defun shm--connect ()
  (if (and shm--server-connection-process
           (eq (process-status shm--server-connection-process) 'open))
      shm--server-connection-process
    (progn
      (when shm--server-connection-process
        (bert-rpc-disconnect shm--server-connection-process))
      (setf shm--server-connection-process
            (condition-case err
                ;; Try to connect to running instance, may fail if there's
                ;; nothing running on specified port.
                (shm--connect-bert-rpc shm-server-default-port)
              (error
               (let* ((low-port    (car shm-server-port-range))
                      (high-port   (cdr shm-server-port-range))
                      (random-port (+ low-port (random (- high-port low-port)))))
                 (shm--connect-bert-rpc
                  (shm--start-server-on-port random-port))))))
      shm--server-connection-process)))

(defvar structured-haskell-mode--results (make-hash-table :test #'equal))

(defun shm--call-process (proc function call-id parse-type exts source)
  (declare (indent 1))
  (cl-assert (stringp call-id))
  (unwind-protect
      (let ((result
             (bert-rpc-call-sync proc
               'structured-haskell-mode
               function
               (list call-id parse-type exts source))))
        (cl-assert (vectorp result))
        (cl-assert (or (= 2 (length result))
                       (= 3 (length result))))
        (let ((result-outcome (aref result 0)))
          (cl-assert (symbolp result-outcome))
          (pcase result-outcome
            (`error
             (error "Error from shm server: %s" (aref result 1)))
            (`success
             (let ((result-id (aref result 1))
                   (result-data (aref result 2)))
               (cl-assert (stringp result-id))
               (if (string= result-id call-id)
                   result-data
                 (progn
                   (puthash result-id result-data structured-haskell-mode--results)
                   (while (not (gethash call-id structured-haskell-mode--results))
                     (bert-rpc-accept-process-output proc))
                   (gethash call-id structured-haskell-mode--results)))))
            (_
             (error "Unexpected result from shm server: %s" result)))))
    (remhash call-id structured-haskell-mode--results)))

(defun shm--call-server (func parse-type source)
  (shm--call-process
      (shm--connect)
    func
    (sha1 source)
    parse-type
    nil
    (encode-coding-string source 'utf-8)))


(defun shm--resolve-parse-error-line (start line col)
  "Resolve parse error on line LINE and column COL counting from
buffer position START."
  (cl-assert (numberp start))
  (cl-assert (numberp line))
  (cl-assert (numberp col))
  (save-excursion
    (goto-char start)
    (unless (= 1 line)
      (forward-line (1- line)))
    (move-to-column (1- col))
    (values (line-number-at-pos) (point))))

(defstruct (shm-parse-error
            (:constructor shm-parse-error--construct))
  message
  line-number      ;; Line number reported by shm server.
  column           ;; Column reported by shm server.
  real-line-number ;; Real line number in the source buffer.
  real-column      ;; Real column number in the source buffer.
  position         ;; Buffer position that real-line-number and real-column point to.
  )

(defun* make-shm-parse-error (&key message line-number column real-line-number real-column position)
  (cl-assert (stringp message))
  (cl-assert (numberp line-number))
  (cl-assert (numberp column))
  (cl-assert (numberp real-line-number))
  (cl-assert (numberp real-column))
  (cl-assert (numberp position))
  (shm-parse-error--construct
   :message          message
   :line-number      line-number
   :column           column
   :real-line-number real-line-number
   :real-column      real-column
   :position         position))

(defun shm--format-parse-error (parse-error)
  (format "%d:%d: %s"
          (shm-parse-error-real-line-number parse-error)
          (shm-parse-error-real-column parse-error)
          (shm-parse-error-message parse-error)))

(defmacro* shm--fold-server-response
    (parsing-start
     data
     (success-pat on-success)
     (parse-error-var on-parse-error))
  (declare (indent 2))
  (let ((data-var             '#:data)
        (msg-var              '#:msg)
        (line-var             '#:line)
        (col-var              '#:col)
        (real-line-number-var '#:real-line-number)
        (pos-var              '#:pos))
    `(let ((,data-var ,data))
       (pcase ,data-var
         (,success-pat
          ,on-success)
         (`[parse_error ,,msg-var ,,line-var ,,col-var]
          (let ((,parse-error-var
                 (multiple-value-bind
                     (,real-line-number-var ,pos-var)
                     (shm--resolve-parse-error-line ,parsing-start ,line-var ,col-var)
                   (make-shm-parse-error
                    :message ,msg-var
                    :line-number ,line-var
                    :column ,col-var
                    :real-line-number ,real-line-number-var
                    :real-column (1- ,col-var)
                    :position ,pos-var))))
            ,on-parse-error))
         (`[arguments_error ,msg]
          (error "Protocol error while calling SHM server: %s" msg))
         (_
          (error "Unexpected response from SHM server: %s" ,data-var))))))

(defun shm-get-ast (type start end)
  "Get the AST for the given region at START and END. Parses with TYPE.

This currently launches a fresh process and uses this buffer
nonsense, for any parse, which sucks, but is fast enough _right
now_. Later on a possibility to make this much faster is to have
a persistent running parser server and than just send requests to
it, that should bring down the roundtrip time significantly, I'd
imagine."
  (cl-assert (symbolp type))
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (when t ;; (> end start)
      (let* ((source (shm--strip-preprocessor
                      (buffer-substring-no-properties start end))))
        (shm--fold-server-response
            start
            (shm--call-server 'parse type source)
          (`[spans ,spans]
           spans)
          (parse-error
           (let ((pretty-message
                  (shm--format-parse-error parse-error)))
             (modeline-set-syntax-check-result
                 'error
               (list 'help-echo pretty-message
                     'mouse-face 'mode-line-highlight))
             (when shm-display-quarantine
               (let ((pos (shm-parse-error-position parse-error)))
                 (shm-quarantine-overlay pos
                                         (save-excursion
                                           (goto-char pos)
                                           (line-end-position)))))
             (error "SHM parse error: %s" pretty-message)
             nil)))))))

(defun shm-check-ast (type start end)
  "Check whether the region of TYPE from START to END parses.

This doesn't generate or return an AST, it just checks whether it
parses."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (cl-assert (symbolp type))
    (let ((source (shm--strip-preprocessor
                   (buffer-substring-no-properties start end)))
          (fixed-type
           ;; In other words, always parse with
           ;; the more generic “decl” when
           ;; something starts at column 0,
           ;; because HSE distinguishes between a
           ;; “declaration” and an import, a
           ;; module declaration and a language
           ;; pragma.
           (if (save-excursion (goto-char start)
                               (= (point) (line-beginning-position)))
               'decl
             type)))
      (shm--fold-server-response
          start
          (shm--call-server 'check type source)
        (`ok t)
        (parse-error
         (progn
           (message "SHM parse error: %s"
                    (shm--format-parse-error parse-error))
           nil))))))

(defun shm-language-extensions ()
  "Get the number of spaces to indent."
  (if (boundp 'haskell-language-extensions)
      haskell-language-extensions
    shm-language-extensions))

(defvar shm-get-nodes--start-markers-cache
  (make-hash-table :test #'equal :size 1001)
  "Hash table cache from positions to markers pointing to these
positions. Used to avoid allocating equal markers by
`shm-get-nodes.'")

(defvar shm-get-nodes--end-markers-cache
  (make-hash-table :test #'equal :size 1001)
  "Hash table cache from positions to markers pointing to these
positions that move if something is inserted before them. Used to
avoid allocating equal markers by `shm-get-nodes.'")

(defun shm-get-nodes (ast start end)
  "Get the nodes of the given AST.

We convert all the line-col numbers to Emacs points and then
create markers out of them. We also store the type of the node,
e.g. Exp, and the case of the node, e.g. Lit or Case or Let,
which is helpful for doing node-specific operations like
indentation.

Any optimizations welcome."
  (let ((start-column (save-excursion (goto-char start)
                                      (current-column))))
    (cond ((or (vectorp ast)
               (listp ast))
           (save-excursion
             (unwind-protect
                 (cl-map 'vector
                         (lambda (node)
                           (cl-assert (vectorp node))
                           (cl-assert (= 6 (length node)))
                           (let ((type (aref node 0))
                                 (constructor (aref node 1))
                                 (start-line (aref node 2))
                                 (start-col (aref node 3))
                                 (end-line (aref node 4))
                                 (end-col (aref node 5)))
                             (shm-make-node
                              type
                              (if (stringp constructor)
                                  (string->symbol constructor)
                                (progn
                                  (cl-assert (symbolp constructor))
                                  constructor))
                              (progn (goto-char start)
                                     (forward-line (1- start-line))
                                     ;; This trick is to ensure that the first
                                     ;; line's columns are offsetted for
                                     ;; regions that don't start at column
                                     ;; zero.
                                     (goto-char (+ (if (= start-line 1)
                                                       start-column
                                                     0)
                                                   (1- (+ (point) start-col))))
                                     (let ((pt (point)))
                                       (aif (gethash pt shm-get-nodes--start-markers-cache)
                                           it
                                         (let ((marker (set-marker (make-marker) pt)))
                                           (puthash pt marker shm-get-nodes--start-markers-cache)
                                           marker))))
                              (progn (goto-char start)
                                     (forward-line (1- end-line))
                                     ;; Same logic as commented above.
                                     (goto-char (+ (if (= end-line 1)
                                                       start-column
                                                     0)
                                                   (1- (+ (point) end-col))))
                                     ;; This avoids the case of:
                                     (while (save-excursion (goto-char (line-beginning-position))
                                                            (or (looking-at-p "[ ]+-- ")
                                                                (looking-at-p "[ ]+$")))
                                       (forward-line -1)
                                       (goto-char (line-end-position)))
                                     (let ((pt (point)))
                                       (aif (gethash pt shm-get-nodes--end-markers-cache)
                                           it
                                         (let ((marker (set-marker (make-marker) pt)))
                                           (set-marker-insertion-type marker t)
                                           (puthash pt marker shm-get-nodes--end-markers-cache)
                                           marker)))))))
                         ast)
               (progn
                 ;; Clear hashes after use
                 (clrhash shm-get-nodes--start-markers-cache)
                 (clrhash shm-get-nodes--end-markers-cache)))))
          (t nil))))

(defun shm-decl-points (&optional use-line-comments)
  "Get the start and end position of the current
declaration. This assumes that declarations start at column zero
and that the rest is always indented by one space afterwards, so
Template Haskell uses with it all being at column zero are not
expected to work."
  (cond
   ;; If we're in a block comment spanning multiple lines then let's
   ;; see if it starts at the beginning of the line (or if any comment
   ;; is at the beginning of the line, we don't care to treat it as a
   ;; proper declaration.
   ((and (not use-line-comments)
         (shm-in-comment)
         (save-excursion (beginning-of-line)
                         (shm-in-comment)))
    nil)
   ((save-excursion
      (beginning-of-line)
      (or (looking-at-p "^-}$")
          (looking-at-p "^{-$")))
    nil)
   ((bound-and-true-p structured-haskell-repl-mode)
    (case major-mode
      (haskell-interactive-mode
       ;; If the prompt start is available.
       (when (boundp 'haskell-interactive-mode-prompt-start)
         ;; Unless we're running code.
         (unless (> (point)
                    (save-excursion (goto-char haskell-interactive-mode-prompt-start)
                                    (line-end-position)))
           ;; When we're within the prompt and not on some output lines or whatever.
           (when (and (>= (point) haskell-interactive-mode-prompt-start)
                      (not (= haskell-interactive-mode-prompt-start
                              (line-end-position))))
             (let ((whole-line (buffer-substring-no-properties
                                haskell-interactive-mode-prompt-start
                                (line-end-position))))
               ;; Don't activate if we're doing a GHCi command.
               (unless (and (string-match-p "^:" whole-line)
                            (not (string-match-p "^:[tk] " whole-line)))
                 (cons (save-excursion
                         (goto-char haskell-interactive-mode-prompt-start)
                         (when (looking-at-p ":[kt] ")
                           (search-forward " " (point-max) t 1))
                         (point))
                       (line-end-position))))))))))
   ;; Otherwise we just do our line-based hack.
   (t
    (save-match-data
      (save-excursion
        (let* ((skip-at-first-column-re (rx (or "#" "--" "|]" "\\")))
               (start (or (letrec
                              ((jump
                                (lambda ()
                                  (let ((start (point))
                                        moved)
                                    (search-backward-regexp "^[^ \n]" nil t 1)
                                    (setf moved (not (= start (point))))

                                    (cond
                                      ((and moved
                                            (save-excursion (beginning-of-line)
                                                            (and (not (bobp))
                                                                 (looking-at-p skip-at-first-column-re))))
                                       (funcall jump))
                                      (t (unless (looking-at-p
                                                  (rx bol (or "{-" "-}") eol))
                                           (point))))))))
                            (goto-char (line-end-position))
                            (funcall jump))
                          0))
               (end (if (save-excursion
                          (goto-char start)
                          (looking-at-p "module\\_>"))
                        (search-forward-regexp "\\_<where\\_>" nil t 1)
                      (progn (goto-char (1+ (point)))
                             (or (letrec
                                     ((jump
                                       (lambda ()
                                         (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                                           (cond
                                             ((save-excursion (beginning-of-line)
                                                              (looking-at-p skip-at-first-column-re))
                                              (funcall jump))
                                             (t (forward-char -1)
                                                (search-backward-regexp "[^\n ]" nil t)
                                                (forward-char)
                                                (point)))))))
                                   (funcall jump))
                                 (point-max))))))
          (cons start end)))))))

(defun shm-delete-markers (decl)
  "Delete the markers in DECL."
  (mapc #'shm-node-delete-markers
        (cdr decl)))

(defun shm/init (&optional force-renew)
  "Initialize the current node overlay at point.

FORCE-RENEW would be used when the buffer has changed and
therefore the current overlay should be re-initialized."
  (interactive)
  (when force-renew
    (setq shm-current-node-overlay nil))
  (shm-set-node-overlay))

(defun shm-current-node-pair ()
  "Return the current workable node at point.

Workable means that it is something that we want to be able to
parse.

For example, if we're looking at a Name,

foobar

then that is all well and good, but we don't want to edit a Name,
nor a QName (the parent), we want to edit an Exp (parent-parent)
whose constructor will be a Var."
  (let ((current (shm-node-backwards)))
    (when current
      (if (and shm-current-node-overlay
               (overlay-buffer shm-current-node-overlay)
               (or (= (shm-node-start (cdr current))
                      (overlay-start shm-current-node-overlay))
                   (= (shm-node-end (cdr current))
                      (overlay-end shm-current-node-overlay))))
          (overlay-get shm-current-node-overlay 'node-pair)
        (shm-workable-node current)))))

(defun shm-set-node-overlay (&optional node-pair jump-direction no-record)
  "Set the current overlay for the current node. Optionally pass
NODE-PAIR to use the specific node-pair (index + node)."
  (setq shm-current-node-overlay nil)
  (shm-delete-overlays (point-min)
                       (point-max)
                       'shm-current-overlay)
  (let* ((node-pair (or node-pair
                        (shm-current-node-pair)))
         (node (cdr node-pair)))
    (when jump-direction
      (if (eq jump-direction 'end)
          (goto-char (shm-node-end node))
        (goto-char (shm-node-start node))))
    (setq shm-last-point (point))
    (setq shm-current-node-overlay
          (when node
            (shm-current-overlay (shm-node-start node)
                                 (shm-node-end node)
                                 node-pair)))
    (unless no-record
      (shm-history-record (point) node-pair))))

(defun shm/goto-last-point ()
  "Jump to the most recent node."
  (interactive)
  (let ((stack (shm-history-stack))
        (point (point)))
    (when (not (ring-empty-p stack))
      (let* ((i (if (= (point) (car (ring-ref stack 0)))
                    1
                  0))
             (pair (ring-ref stack i)))
        (when pair
          (goto-char (car pair))
          (shm-set-node-overlay (cdr pair) nil t)
          (loop for j from 0 to i
                do (ring-remove stack 0)))))))

(defun shm-history-jump (point)
  "Jump to POINT and set the current node to whatever node was
  last current at that point."
  (goto-char point)
  (let ((stack (shm-history-stack)))
    (when (not (ring-empty-p stack))
      (let ((pair (assoc point (ring-elements stack))))
        (when pair
          (shm-set-node-overlay (cdr pair)))))))

(defun shm-history-record (point node-pair)
  "Record POINT and NODE in the node history."
  (ring-insert (shm-history-stack)
               (cons point node-pair)))

(defun shm-history-stack ()
  "Get the node history of the current buffer."
  (if (and (local-variable-p 'shm-history-stack)
           shm-history-stack)
      shm-history-stack
    (set (make-local-variable 'shm-history-stack)
         (make-ring shm-history-stack-max-length))))

(defun shm-node-backwards (&optional start type bound)
  "Get the current node searching bottom up starting from START,
and optionally just searching for nodes of type TYPE. BOUND
restricts how far to look back.

This is the fundamental way to look for a node in the declaration
vector.

Backwards means we go from the last node in the list and go
backwards up the list, it doesn't mean backwards as in up the
tree."
  (let* ((vector (shm-decl-ast))
         (point (point)))
    (loop for i
          downfrom (if start
                       (max -1 start)
                     (1- (length vector)))
          to -1
          until (or (= i -1)
                    (let ((node (aref vector i)))
                      (or (and bound
                               (< (shm-node-start node)
                                  bound))
                          (and (>= point (shm-node-start node))
                               (<= point (shm-node-end node))
                               (or (not type)
                                   (equal type
                                          (shm-node-type node)))))))
          finally (return
                   (when (and (>= i 0)
                              (not (and bound
                                        (< (shm-node-start (aref vector i))
                                           bound))))
                     (cons i
                           (aref vector i)))))))

(defun shm-workable-node (current-pair)
  "Assume that the given CURRENT node is not workable, and look
at the parent. If the parent has the same start/end position,
then the parent is the correct one to work with."
  (let* ((parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair))
         (current (cdr current-pair)))
    (if parent
        (if (and (= (shm-node-start current)
                    (shm-node-start parent))
                 (= (shm-node-end current)
                    (shm-node-end parent)))
            (if (equal (shm-node-type current) (shm-node-type parent))
                current-pair
              (shm-workable-node parent-pair))
          current-pair)
      current-pair)))

(defun shm-node-parent (node-pair &optional type bound)
  "Return the direct parent of the given node-pair.

The start and end point of the parent can be the same as the
child, and in fact is common."
  (cl-assert (symbolp type))
  (save-excursion
    (goto-char (shm-node-start (cdr node-pair)))
    (let* ((actual-parent-pair (shm-node-backwards (1- (car node-pair))
                                                   type
                                                   bound))
           (maybe-parent-parent-pair (when (car actual-parent-pair)
                                       (shm-node-backwards (1- (car actual-parent-pair)))))
           (actual-parent (cdr actual-parent-pair))
           (maybe-parent-parent (cdr maybe-parent-parent-pair)))
      (cond ((and actual-parent-pair
                  maybe-parent-parent-pair
                  (eq (shm-node-type-name actual-parent)
                      (shm-node-type-name maybe-parent-parent))
                  (and shm-skip-applications
                       (shm-node-app-p actual-parent))
                  (eq (shm-node-cons actual-parent)
                      (shm-node-cons maybe-parent-parent)))
             (shm-node-parent actual-parent-pair))
            (t actual-parent-pair)))))

(defun shm-node-child-pair (node-pair)
  "Return the immediate child-pair of the given parent."
  (let ((vector (shm-decl-ast))
        (i (car node-pair)))
    (when (< i (1- (length vector)))
      (cons (1+ i)
            (aref vector (1+ i))))))

(defun shm-node-child (node-pair)
  "Return the immediate child of the given parent."
  (cdr (shm-node-child-pair node-pair)))

(defun shm-node-ancestor-at-point (node-pair point)
  "Find the highest up ancestor that still starts at this point."
  (let ((parent-pair (shm-node-parent node-pair)))
    (if parent-pair
        (if (= (shm-node-start (cdr parent-pair))
               point)
            (shm-node-ancestor-at-point parent-pair point)
          node-pair)
      node-pair)))

(defun shm-node-previous (node-pair)
  "Get the previous node of NODE-PAIR."
  (let ((vector (shm-decl-ast)))
    (loop for i
          downfrom (car node-pair)
          to -1
          until (or (= i -1)
                    (let ((node (aref vector i)))
                      (<= (shm-node-end node)
                          (shm-node-start (cdr node-pair)))))
          finally (return
                   (when (>= i 0)
                     (shm-workable-node (cons i
                                              (aref vector i))))))))

(defun shm-node-next (node-pair)
  "Get the next node of NODE-PAIR."
  (let* ((vector (shm-decl-ast))
         (len (length vector)))
    (loop for i
          from 0
          to len
          until (or (= i len)
                    (let ((node (aref vector i)))
                      (>= (shm-node-start node)
                          (shm-node-end (cdr node-pair)))))
          finally (return
                   (when (< i len)
                     (shm-workable-node (cons i
                                              (aref vector i))))))))

(defun shm-get-qop-string (node)
  "Get the string of the operator, if the node is an operator."
  (when (eq (shm-node-type-name node) 'QOp)
    (buffer-substring-no-properties (shm-node-start node)
                                    (shm-node-end node))))

(defun shm/goto-parent (&optional node-pair direction)
  "Set the current node overlay to the parent node-pair"
  (interactive)
  (let ((direction (or direction 'start)))
    (if shm-current-node-overlay
        (let* ((o shm-current-node-overlay)
               (parent-pair (shm-node-parent (or node-pair
                                                 (shm-current-workable-node)))))
          (when parent-pair
            (let ((parent (cdr parent-pair)))
              (if (and o
                       (overlay-buffer o)
                       (>= (shm-node-start parent)
                           (overlay-start o))
                       (<= (shm-node-end parent)
                           (overlay-end o)))
                  (shm/goto-parent parent-pair direction)
                (shm-set-node-overlay parent-pair direction)))))
      (when node-pair
        (shm-set-node-overlay node-pair direction)))))

(defun shm-current-node ()
  "Return just the current node, without its index.

See `shm-current-node-pair' for what 'current' means."
  (cdr (shm-current-node-pair)))

(defun shm-actual-node ()
  "Return just the actual current node, without its index.

Normally node functions only care about the current workable
node. This function will return the *actual* node at point. See
`shm-current-node-pair' for what 'workable' means."
  (cdr (shm-node-backwards)))

(defun shm-current-workable-node ()
  "Returns the same as `shm-current-node' but including the index."
  (let ((current (shm-node-backwards)))
    (when current
      (shm-workable-node current))))

(defun shm-decl-node (start)
  "Get the top-level node of the declaration."
  (let* ((vector (save-excursion (goto-char start)
                                 (shm-decl-ast))))
    (aref vector 0)))

(defun shm-current-node-string ()
  "Get the text of the current shm node"
  (shm-node-string (shm-current-node)))

(defun shm-in-pattern? ()
  "Check whether point is located in haskell function pattern."
  (let ((node-pair (shm-current-node-pair)))
    (when node-pair
      (let ((workable-node (cdr-safe (shm-workable-node node-pair))))
        (when workable-node
          (let ((constructor (shm-node-cons workable-node)))
            (string-match-p "^P.*" (symbol-name constructor))
            ;; maybe more efficient but certainly less generic
            ;; (memq constructor
            ;;       '(PVar
            ;;         PLit
            ;;         PNeg
            ;;         PNPlusK
            ;;         PInfixApp
            ;;         PApp
            ;;         PTuple
            ;;         PList
            ;;         PParen
            ;;         PRec
            ;;         PAsPat
            ;;         PWildCard
            ;;         PIrrPat
            ;;         PatTypeSig
            ;;         PViewPat
            ;;         PRPat
            ;;         PXTag
            ;;         PXETag
            ;;         PXPcdata
            ;;         PXPatTag
            ;;         PXRPats
            ;;         PExplTypeArg
            ;;         PQuasiQuote
            ;;         PBangPat
            ;;         PFieldPat
            ;;         PFieldPun
            ;;         PFieldWildcard))
            ))))))

(defun shm-any-parent-satisfies? (pred node-pair)
  "Check all parents of NODE-PAIR if any of them satisfies predicate PRED,
which should take node pair."
  (save-excursion
    (let ((curr-pair node-pair)
          (found nil))
      (while (and (not found)
                  (not (null curr-pair)))
        (setf found (funcall pred curr-pair))
        (setf curr-pair (shm-node-parent curr-pair)))
      found)))

(defun shm-inside-do-block? (node-pair)
  (shm-any-parent-satisfies?
   (lambda (node-pair)
     (eq 'Do (shm-node-cons (cdr node-pair))))
   node-pair))

(defun shm-inside-list-comprehension? (node-pair)
  (shm-any-parent-satisfies?
   (lambda (node-pair)
     (eq 'ListComp (shm-node-cons (cdr node-pair))))
   node-pair))

(provide 'shm-ast)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; byte-compile-warnings: (not cl-macros)
;; End:
