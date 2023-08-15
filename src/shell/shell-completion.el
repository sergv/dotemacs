;; shell-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 December 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (require 'set-up-platform)
  (require 'subr-x))

(require 'pcomplete)
(require 'completion-setup)
(require 'haskell-misc)
;; (require 'json)

;;;; Utilities

;; Use this to debug completion functions

;; (defun pcomplete/test ()
;;   (message "pcomplete-args = %s"
;;            (pp-to-string (-map #'strip-text-properties pcomplete-args)))
;;   (let* ((last-arg
;;           (pcomplete-arg 'last))
;;          (last-arg-starts-with-single-dash
;;           (string-match-p "^-\\([^-]\\|$\\)" last-arg))
;;          (last-arg-starts-with-two-dashes
;;           (string-match-p "^--\\([^-]\\|$\\)" last-arg))
;;          (got-end-of-flags
;;           (and
;;            (not last-arg-starts-with-two-dashes)
;;            (member "--"
;;                    (-take pcomplete-last pcomplete-args)))))
;;     (message "last-arg = %s, last-arg-starts-with-single-dash = %s, got-end-of-flags = %s"
;;              (pp-to-string (strip-text-properties last-arg))
;;              (pp-to-string last-arg-starts-with-single-dash)
;;              (pp-to-string got-end-of-flags))
;;     (pcomplete-here '("setup"))
;;     (let ((n 0)
;;           (pcomplete-arg-idx nil))
;;       (while (progn
;;                ;; (message "n = %s"
;;                ;;          (pp-to-string n))
;;                ;; (message "(pcomplete-arg 1) = %s"
;;                ;;          (pp-to-string (strip-text-properties (pcomplete-arg 1))))
;;                ;; (message "(pcomplete-arg 0) = %s"
;;                ;;          (pp-to-string (strip-text-properties (pcomplete-arg 0))))
;;                ;; (message "(pcomplete-arg) = %s"
;;                ;;          (pp-to-string (strip-text-properties (pcomplete-arg))))
;;                ;; (message "pcomplete-index = %s, pcomplete-stub = %s"
;;                ;;          (pp-to-string pcomplete-index)
;;                ;;          (pp-to-string pcomplete-stub))
;;                (cond
;;                  ((pcomplete-match "--stack-yaml" pcomplete-arg-idx)
;;                   (pcomplete-here (pcomplete-entries "\\.yaml\\'")))
;;                  ((string-match-p "^--" (pcomplete-arg))
;;                   (message "doing (pcomplete-here '(\"--stack-yaml\"))")
;;                   (pcomplete-here '("--stack-yaml")))
;;                  (t nil)))
;;         (incf n)
;;         ;; (pcomplete-here '("--stack-yaml"))
;;         ;; (pcomplete-here (pcomplete-entries "\\.yaml\\'"))
;;         )
;;       (message "After loop: n = %s"
;;                (pp-to-string n))
;;       ;; (while t
;;       ;;   (pcomplete-here (pcomplete-entries "\\.\\(?:hs\\|md\\|cabal\\)\\'"))
;;       ;;   ;; (pcomplete-here '("--stack-yaml"))
;;       ;;   ;; (pcomplete-here (pcomplete-entries "\\.yaml\\'"))
;;       ;;   )
;;       )))
;;
;; (defun strip-text-properties (txt)
;;   (set-text-properties 0 (length txt) nil txt)
;;   txt)

(defun pcmpl-git-commits ()
  "Return list of commits to complete against."
  (cons "HEAD"
        (pcmpl-git-get-refs "heads\\|tags")))

(defun pcmpl-git-branch-names ()
  "Return list of branch names to complete against."
  (cons "HEAD"
        (magit-list-branch-names)))

(defun pcmpl-git-commits-and-files ()
  "Return list of commits to complete against."
  (append (pcmpl-git-commits)
          (directory-files default-directory
                           nil
                           directory-files-no-dot-files-regexp)))

(defalias 'pcmpl-git-rev 'pcmpl-git-commits)

(defun pcmpl-git-get-remotes ()
  "Return a list of git repository remotes."
  (with-temp-buffer
    (call-process "git"
                  nil
                  (current-buffer)
                  nil
                  "remote"
                  "show"
                  "-n")
    (split-string (buffer-substring-no-properties (point-min)
                                                  (point-max))
                  "\n"
                  t)))

(defun pcmpl-git-get-refs (type)
  "Return a list of git refs filtered by TYPE."
  (save-match-data
    (with-temp-buffer
      (call-process "git"
                    nil
                    (current-buffer)
                    nil
                    "for-each-ref"
                    "refs/"
                    "--format=%(refname)")
      (let ((re (concat "^refs/"
                        "\\(?:" type "\\)"
                        "/\\(.*\\)")))
        (-non-nil
         (-map (lambda (ref)
                 (when (string-match? re ref)
                   (match-string-no-properties 1 ref)))
               (split-string (buffer-substring-no-properties (point-min)
                                                             (point-max))
                             "\n"
                             t)))))))

(cl-defstruct (pcmpl-flag
               (:conc-name pcmpl-flag/)
               (:constructor pcmpl-flag/create))
  names           ;; Nonempty list of strings
  completion-expr ;; Closed expression to generate completions, may be nil
  )

(cl-defun make-pcmpl-flag (&key names completion-expr)
  (cl-assert (or (string? names)
                 (-all? #'string? names)))
  (pcmpl-flag/create :names names :completion-expr completion-expr))

(cl-defun pcpmpl/make-name-regex (flag)
  (cl-assert (pcmpl-flag-p flag))
  (regexp-opt (pcmpl-flag/names flag) 'symbols))

(defun pcmpl/make-flags (raw-flag-specs)
  "RAW-FLAG-SPECS is a list of either strings or 1- or 2-element lists where
first item is flag name and second is completion expression. Flag name can
be either singular string or a list of strings."
  (-map
   (lambda (entry)
     (cond
       ((string? entry)
        (make-pcmpl-flag
         :names (list entry)
         :completion-expr nil))
       ((and entry
             (listp entry)
             (= 2 (length entry)))
        (let ((flag-name (first entry))
              (compl-expr (cadr-safe entry)))
          (cl-assert (or (string? flag-name)
                         (and
                          (listp flag-name)
                          (-all? #'string? flag-name)))
                     nil
                     "process-opts: invalid flag name: %s"
                     flag-name)
          (make-pcmpl-flag :names (if (string? flag-name)
                                      (list flag-name)
                                    flag-name)
                           :completion-expr compl-expr)))
       (t
        (error "process-opts: invalid entry %s" entry))))
   raw-flag-specs))

;;;; The pcomplete macro

;;;###autoload
(cl-defmacro defpcmpl (name definition &key (evaluate-definition nil))
  "Define completion function NAME which should start with pcomplete/.
DEFINITION is described by the following grammar:

non-terminals:
start      = <or> | <opts>
or         = (or <positional>* <opts>*)
opts       = (opts <flags>? <args>?)
positional = (<positional-arg> <start>?) | ((<positional-arg>+) <start>?)

flags      = (flags <flag>*)
args       = (args <completion-expr>)

flag       = <flag-string> | (<flag-string> <completion-expr>)

terminals:
flag-string     - emacs string describing flag, must include - or --
completion-expr - elisp expression, like (pcomplete-here (pcmpl-entries))
positional-arg  - emacs string, usually without - or --

N.B. Small deviations to the grammar may be tolerated, but they're mostly
useless, e.g. (opts (args)) would be accepted but to no effect.


<or> just lists alternatives, mostly useful for positional arguments.

<opts> stands for given flags followed by args, no more positional arguments."
  (declare (indent 1))
  (cl-assert (string-match-p "pcomplete/" (symbol->string name)))
  (let ((got-end-of-flags-var '#:got-end-of-flags))
    (letrec ((process
              (lambda (definition positional-depth)
                (cl-assert (and (listp definition)
                                (not (null definition)))
                           nil
                           "invalid definition %s"
                           definition)
                (cond ((eq? 'or (first definition))
                       (funcall process-or definition positional-depth))
                      ((eq? 'opts (first definition))
                       (funcall process-opts definition))
                      (t
                       (error "process: unsupported definition: %S" definition)))))
             ;; Alternatives
             (process-or
              (lambda (definition positional-depth)
                (when-let (defs (rest definition))
                  (let* ((positional-def?
                          (lambda (def)
                            (or (string? (first def))
                                (and
                                 (listp (first def))
                                 (-all? #'string? (first def))))))
                         (positional-defs
                          (-filter positional-def? defs))
                         (other-defs
                          (--filter (not (funcall positional-def? it)) defs))
                         (names
                          (-map (lambda (x) (if (listp x) (first x) (list x)))
                                positional-defs))
                         (pcomplete-arg-var '#:positional-arg))
                    `(progn
                       ,@(when names
                           (list `(pcomplete-here ',names)))

                       (let ((,pcomplete-arg-var (pcomplete-arg 'first ,positional-depth)))
                         (cond ,@(-map (lambda (def)
                                         (funcall process-positional
                                                  def
                                                  pcomplete-arg-var
                                                  positional-depth))
                                       positional-defs)
                               ,@(when (not (null other-defs))
                                   (list `(t
                                           ,@(--map (funcall process it positional-depth)
                                                    other-defs)))))))))))
             ;; -s and --long flags followed by positional arguments
             (process-opts
              (lambda (definition)
                (let ((info (rest definition)))
                  (cl-assert (-all? (lambda (entry)
                                      (and (listp entry)
                                           (not (null entry))
                                           (memq (first entry) '(flags args))))
                                    info)
                             nil
                             "<opts> clause must contain either (flags ...) or (args ...) entries only: %S"
                             info)
                  (let* ((flags (pcmpl/make-flags (cdr-safe (assoc 'flags info))))
                         (flags-with-args (-filter #'pcmpl-flag/completion-expr flags))
                         ;; Positional arguments.
                         (args (assoc 'args info)))
                    (cl-assert (--all? (-all? (lambda (x) (string-match-p "^--?[^-].*" x))
                                              (pcmpl-flag/names it))
                                       flags)
                               nil
                               "All flags names must start with dash or two dashes: %S\nFailed flags: %S"
                               flags
                               (--filter (not (string-match-p "^--?[^-].*$"
                                                              (pcmpl-flag/names it)))
                                         flags))
                    (when args
                      (cl-assert (cdr args)
                                 nil
                                 "Meaningless (args ...) clause without completion action, (args <action>) expected: %s"
                                 args))
                    (when (or flags args)
                      (cl-multiple-value-bind (single-dash-flags double-dash-flags)
                          (--separate (string-match-p "^-[^-]" it)
                                      (-mapcat #'pcmpl-flag/names flags))
                        `(while
                             (unless ,got-end-of-flags-var
                               (let (
                                     ,@(when (or single-dash-flags
                                                 double-dash-flags)
                                         (list
                                          '(current-arg (pcomplete-arg)))))
                                 (cond
                                   ,@(-map (lambda (flag)
                                             `((pcomplete-match ,(pcpmpl/make-name-regex flag))
                                               (pcomplete-here ,(pcmpl-flag/completion-expr flag))))
                                           flags-with-args)
                                   ,@(when single-dash-flags
                                       (list
                                        `((string-match-p "^-\\([^-]\\|$\\)" current-arg)
                                          (pcomplete-here ',single-dash-flags))))
                                   ,@(when double-dash-flags
                                       (list
                                        `((string-match-p "^--" current-arg)
                                          (pcomplete-here ',double-dash-flags))))
                                   (t
                                    ,@(if (cdr args)
                                          (--map (list 'pcomplete-here* it) (cdr args))
                                        ;; Return nil to while loop to show that
                                        ;; there are no more completions.
                                        '(nil)))))))))))))
             ;; Positional arguments for subcommands
             (process-positional
              (lambda (definition pcomplete-arg-var positional-depth)
                (let ((name (first definition)))
                  (cl-assert (or (string? name)
                                 (and (listp name)
                                      (-all? #'string? name))))
                  `(,(cond
                       ((string? name)
                        `(string= ,pcomplete-arg-var ,name))
                       ((listp name)
                        `(member ,pcomplete-arg-var ',name))
                       (t
                        (error "Invalid name while processing positional option: %s" name)))
                    ,@(awhen (cadr-safe definition)
                        (list (funcall process
                                       it
                                       (+ positional-depth 1)))))))))
      `(defun ,name ()
         (let ((,got-end-of-flags-var
                (member "--" (-take pcomplete-last pcomplete-args))))
           ,(funcall process
                     (if evaluate-definition
                         (eval definition t)
                       definition)
                     ;; Positional depth must start from 1 so that
                     ;; first positional argument matched will be past
                     ;; command name.
                     1))))))

(cl-defun pcmpl-entries (&key select ignore)
  "Like `pcomplete-entries' but ignores files mathing IGNORE
regexp and files under version-control directories."
  (let ((pcomplete-file-ignore ignore)
        (pcomplete-dir-ignore
         (eval-when-compile
           (concat "\\`"
                   (regexp-opt (cons "." (cons ".." +version-control-directories+)))
                   "/\\'"))))
    (pcomplete-entries select)))

(defun pcmpl-dirs ()
  (let ((pcomplete-dir-ignore
         (eval-when-compile
           (concat "\\`"
                   (regexp-opt (cons "." (cons ".." +version-control-directories+)))
                   "/\\'"))))
    (pcomplete-dirs)))

(defun pcmpl-entries-ignoring-common ()
  "Like `pcomplete-entries' but ignores files from `+ignored-file-extensions+' and
under version-control directories."
  (pcmpl-entries
   :ignore
   (eval-when-compile
     (concat "\\`.*"
             (regexp-opt +ignored-file-extensions+)
             "\\'"))))

;;;; Version control

;;;###autoload (autoload 'pcomplete/git "shell-completion" nil t)
(defpcmpl pcomplete/git
  (or ("add"
       (opts
        (flags "-n"
               "--dry-run"
               "-v"
               "--verbose"
               "-i"
               "--interactive"
               "-p"
               "--patch"
               "-e"
               "--edit"
               "-f"
               "--force"
               "-u"
               "--update"
               "-N"
               "--intent-to-add"
               "-A"
               "--all"
               "--ignore-removal"
               "--refresh"
               "--ignore-errors"
               "--ignore-missing")
        (args (pcmpl-entries))))
      ("bisect"
       (or
        ("help")
        ("start"
         (opts
          (flags "--no-checkout")
          (args (pcmpl-git-rev))))
        ("bad"
         (opts
          (args (pcmpl-git-rev))))
        ("good"
         (opts
          (args (pcmpl-git-rev))))
        ("skip"
         (opts
          (args (pcmpl-git-rev))))
        ("next")
        ("reset"
         (opts
          (args (pcmpl-git-commits))))
        ("visualize")
        ("replay")
        ("log")
        ("run")))
      (("branch" "br")
       (opts
        (flags "-v"
               "--verbose"
               "-q"
               "--quiet"
               "-t"
               "--track"
               "--set-upstream"
               "-u"
               "--set-upstream-to"
               "--unset-upstream"
               "--color"
               "-r"
               "--remotes"
               "--contains"
               "--abbrev"
               "-a"
               "--all"
               "-d"
               "--delete"
               "-D"
               "-m"
               "--move"
               "-M"
               "--list"
               "-l"
               "--create-reflog"
               "--edit-description"
               "-f"
               "--force"
               "--no-merged"
               "--merged"
               "--column")
        (args (pcmpl-git-commits))))
      (("ch" "checkout")
       (opts
        (flags "-q"
               "--quiet"
               (("-b" "-B" "--orphan") (pcmpl-git-branch-names))
               "-l"
               "--detach"
               "-t"
               "--track"
               "--orphan"
               "-2"
               "--ours"
               "-3"
               "--theirs"
               "-f"
               "--force"
               "-m"
               "--merge"
               "--overwrite-ignore"
               "--conflict"
               "-p"
               "--patch"
               "--ignore-skip-worktree-bits")
        (args (pcmpl-git-commits))))
      ("clone"
       (opts
        (flags "-v"
               "--verbose"
               "-q"
               "--quiet"
               "--progress"
               "-n"
               "--no-checkout"
               "--bare"
               "--mirror"
               "-l"
               "--local"
               "--no-hardlinks"
               "-s"
               "--shared"
               "--recursive"
               "--recurse-submodules"
               "--template"
               "--reference"
               "-o"
               "--origin"
               "-b"
               "--branch"
               "-u"
               "--upload-pack"
               "--depth"
               "--single-branch"
               "--separate-git-dir"
               "-c"
               "--config")))
      (("co" "commit")
       (opts
        (flags "-q"
               "--quiet"
               "-v"
               "--verbose"
               "-F"
               "--file"
               "--author"
               "--date"
               "-m"
               "--message"
               "-c"
               "--reedit-message"
               "-C"
               "--reuse-message"
               "--fixup"
               "--squash"
               "--reset-author"
               "-s"
               "--signoff"
               "-t"
               "--template"
               "-e"
               "--edit"
               "--cleanup"
               "--status"
               "-S"
               "--gpg-sign"
               "-a"
               "--all"
               "-i"
               "--include"
               "--interactive"
               "-p"
               "--patch"
               "-o"
               "--only"
               "-n"
               "--no-verify"
               "--dry-run"
               "--short"
               "--branch"
               "--porcelain"
               "--long"
               "-z"
               "--null"
               "--amend"
               "--no-post-rewrite"
               "-u"
               "--untracked-files")
        (args (pcmpl-entries))))
      ("diff"
       (opts
        (flags "--no-index"
               "--cached"
               "-p"
               "-u"
               "--patch"
               "-s"
               "--no-patch"
               "-U"
               "--unified"
               "--raw"
               "--patch-with-raw"
               "--minimal"
               "--patience"
               "--histogram"
               "--diff-algorithm=patience"
               "--diff-algorithm=minimal"
               "--diff-algorithm=histogram"
               "--diff-algorithm=myers"
               "--diff-algorithm=default"
               "--stat"
               "--numstat"
               "--shortstat"
               "--dirstat"
               "--summary"
               "--patch-with-stat"
               "-z"
               "--name-only"
               "--name-status"
               "--submodule"
               "--color"
               "--no-color"
               "--word-diff"
               "--word-diff-regex"
               "--color-words"
               "--no-renames"
               "--check"
               "--full-index"
               "--binary"
               "--abbrev"
               "-B"
               "--break-rewrites"
               "-M"
               "--find-renames"
               "-C"
               "--find-copies"
               "--find-copies-harder"
               "-D"
               "--irreversible-delete"
               "-l"
               "--diff-filter"
               "-S"
               "-G"
               "--pickaxe-all"
               "--pickaxe-regex"
               "-O"
               "-R"
               "--relative"
               "-a"
               "--text"
               "--ignore-space-at-eol"
               "-b"
               "--ignore-space-change"
               "-w"
               "--ignore-all-space"
               "--ignore-blank-lines"
               "--inter-hunk-context"
               "-W"
               "--function-context"
               "--exit-code"
               "--quiet"
               "--ext-diff"
               "--no-ext-diff"
               "--textconv"
               "--no-textconv"
               "--ignore-submodules"
               "--src-prefix"
               "--dst-prefix"
               "--no-prefix")
        (args (pcmpl-git-commits-and-files))))
      ("fetch"
       (opts
        (flags "-v"
               "--verbose"
               "-q"
               "--quiet"
               "--all"
               "-a"
               "--append"
               "--upload-pack"
               "-f"
               "--force"
               "-m"
               "--multiple"
               "-t"
               "--tags"
               "-n"
               "-p"
               "--prune"
               "--recurse-submodules"
               "--dry-run"
               "-k"
               "--keep"
               "-u"
               "--update-head-ok"
               "--progress"
               "--depth"
               "--unshallow")))
      ("grep"
       (opts
        (flags "--cached"
               "--no-index"
               "--untracked"
               "--exclude-standard"
               "-v"
               "--invert-match"
               "-i"
               "--ignore-case"
               "-w"
               "--word-regexp"
               "-a"
               "--text"
               "-I"
               "--textconv"
               "--max-depth"
               "-E"
               "--extended-regexp"
               "-G"
               "--basic-regexp"
               "-F"
               "--fixed-strings"
               "-P"
               "--perl-regexp"
               "-n"
               "--line-number"
               "-h"
               "-H"
               "--full-name"
               "-l"
               "--files-with-matches"
               "--name-only"
               "-L"
               "--files-without-match"
               "-z"
               "--null"
               "-c"
               "--count"
               "--color"
               "--break"
               "--heading"
               "-C"
               "--context"
               "-B"
               "--before-context"
               "-A"
               "--after-context"
               "-NUM"
               "-p"
               "--show-function"
               "-W"
               "--function-context"
               "-f"
               "-e"
               "--and"
               "--or"
               "--not"
               "-q"
               "--quiet"
               "--all-match"
               "-O"
               "--open-files-in-pager"
               "--ext-grep")
        (args (pcmpl-entries))))
      ("init"
       (opts
        (flags "--bare"
               "--shared"
               ("--separate-git-dir" (pcomplete-dirs)))
        (args (pcmpl-entries))))
      ("log"
       (opts
        (flags "-q"
               "--quiet"
               "--source"
               "--use-mailmap"
               "-p"
               "--log-size"
               "--decorate"
               "--decorate=short"
               "--decorate=full"
               "--decorate=no"
               "--no-decorate"
               "-L"
               "--follow"
               "-n"
               "--max-count"
               "--since"
               "--after"
               "--until"
               "--before"
               "--author"
               "--committer"
               "--grep-reflog"
               "--grep"
               "--all-match"
               "--invert-grep"
               "-i"
               "--regexp-ignore-case"
               "-E"
               "--extended-regexp"
               "-F"
               "--fixed-strings"
               "--remove-empty"
               "--merges"
               "--no-merges"
               "--min-parents"
               "--max-parents"
               "--no-min-parents"
               "--no-max-parents"
               "--first-parent"
               "--not"
               "--all"
               "--branches"
               "--tags"
               "--remotes"
               "--glob"
               "--exclude"
               "--reflog"
               "--ignore-missing"
               "--bisect"
               "--stdin"
               "--cherry-mark"
               "--cherry-pick"
               "--left-only"
               "--right-only"
               "--cherry"
               "-g"
               "--walk-reflogs"
               "--merge"
               "--boundary"
               "--full-history"
               "--dense"
               "--sparse"
               "--simplify-merges"
               "--ancestry-path"
               "--date-order"
               "--author-date-order"
               "--topo-order"
               "--reverse"
               "--pretty"
               "--format"
               "--abbrev-commit"
               "--no-abbrev-commit"
               "--oneline"
               "--encoding"
               "--notes"
               "--no-notes"
               "--show-notes"
               "--standard-notes"
               "--no-standard-notes"
               "--show-signature"
               "--relative-date"
               "--date"
               "--parents"
               "--chidlren"
               "--left-right"
               "--graph"
               "--show-linear-break"
               "-c"
               "--cc"
               "-m"
               "-r"
               "-t"
               "-p"
               "-u"
               "--patch"
               "-s"
               "--no-patch"
               "-U"
               "--unified"
               "--raw"
               "--patch-with-raw"
               "--minimal"
               "--patience"
               "--histogram"
               "--diff-algorithm"
               "--diff-algorithm=patience"
               "--diff-algorithm=minimal"
               "--diff-algorithm=histogram"
               "--diff-algorithm=meyers"
               "--stat"
               "--numstat"
               "--shortstat"
               "--dirstat"
               "--summary"
               "--patch-with-stat"
               "-z"
               "--name-only"
               "--name-status"
               "--submodule"
               "--color"
               "--no-color"
               "--word-diff"
               "--word-diff-regex"
               "--color-words"
               "--no-renames"
               "--check"
               "--ws-error-highlight"
               "--full-index"
               "--binary"
               "--abbrev"
               "-B"
               "--break-rewrites"
               "-M"
               "--find-renames"
               "-C"
               "--find-copies"
               "--find-copies-harder"
               "-D"
               "--irreversible-delete"
               "-l"
               "--diff-filter"
               "-S"
               "-G"
               "--pickaxe-all"
               "--pickaxe-regex"
               "-O"
               "-R"
               "--relative"
               "-a"
               "--text"
               "--ignore-space-at-eol"
               "-b"
               "--ignore-space-change"
               "-w"
               "--ignore-all-space"
               "--ignore-blank-lines"
               "--inter-hunk-context"
               "-W"
               "--function-context"
               "--ext-diff"
               "--no-ext-diff"
               "--text-conv"
               "--no-text-conv"
               "--ignore-submodules"
               "--src-prefix"
               "--dst-prefix"
               "--no-prefix")
        (args (pcmpl-git-commits-and-files))))
      (("m" "me" "merge")
       (opts
        (flags "-n"
               "--stat"
               "--summary"
               "--log"
               "--squash"
               "--commit"
               "-e"
               "--edit"
               "--ff"
               "--ff-only"
               "--rerere-autoupdate"
               "--verify-signatures"
               "-s"
               "--strategy"
               "-X"
               "--strategy-option"
               "-m"
               "--message"
               "-v"
               "--verbose"
               "-q"
               "--quiet"
               "--abort"
               "--progress"
               "-S"
               "--gpg-sign"
               "--overwrite-ignore")
        (args (pcmpl-git-commits))))
      ("merge-base"
       (opts
        (flags "--all"
               "--octopus"
               "--is-ancestor"
               "--independentn"
               "--fork-point")
        (args (pcmpl-git-commits))))
      ("mv"
       (opts
        (flags "-v"
               "--verbose"
               "-n"
               "--dry-run"
               "-f"
               "--force"
               "-k")
        (args (pcmpl-entries))))
      ("p4"
       (or ("submit")
           ("rebase")
           ("clone")))
      ("pull"
       (opts
        (flags "-n"
               "--no-stat"
               "--commit"
               "--no-commit"
               "--squash"
               "--no-squash"
               "--no-ff"
               "--ff"
               "--no-rebase"
               "--rebase"
               "--rebase=preserve"
               "-s")))
      ("push"
       (opts
        (flags "-v"
               "--verbose"
               "-q"
               "--quiet"
               "--repo"
               "--all"
               "--mirror"
               "--delete"
               "--tags"
               "-n"
               "--dry-run"
               "--porcelain"
               "-f"
               "--force"
               "--force-with-lease"
               "--recurse-submodules"
               "--thin"
               "--receive-pack"
               "--exec"
               "-u"
               "--set-upstream"
               "--progress"
               "--prune"
               "--no-verify"
               "--follow-tags")
        (args
         (pcmpl-git-get-remotes)
         (pcmpl-git-branch-names))))
      ("rebase"
       (opts
        (flags "-i"
               "-x"
               "--interactive"
               "--exec"
               "--no-ff"
               "--abort"
               "--continue"
               "--skip")
        (args (pcmpl-git-rev))))
      ("remote"
       (or
        ("add"
         (opts
          (flags
           "-v"
           "--verbose"
           ("-t" (pcmpl-git-branch-names))
           "-f"
           "--tags"
           "--no-tags"
           "--mirror"
           "--mirror=fetch"
           "--mirror=push")))
        ("rename"
         (opts
          (flags
           "-v"
           "--verbose"))
         (args
          (pcmpl-git-get-remotes)
          (pcmpl-git-get-remotes)))
        ("remove"
         (opts
          (flags
           "-v"
           "--verbose"))
         (args
          (pcmpl-git-get-remotes)))
        ("set-head"
         (opts
          (flags
           "-v"
           "--verbose"
           "-a"
           "--auto"
           "-d"
           "--delete")
          (args
           (pcmpl-git-get-remotes)
           (pcmpl-git-get-refs "heads"))))
        ("set-branches"
         (opts
          (flags
           "-v"
           "--verbose"
           "--add")
          (args
           (pcmpl-git-get-remotes))))
        ("get-url"
         (opts
          (flags
           "-v"
           "--verbose"
           "--push"
           "--add")
          (args
           (pcmpl-git-get-remotes))))
        ("set-url"
         (opts
          (flags
           "-v"
           "--verbose"
           "--push"
           "--add"
           "--delete")
          (args
           (pcmpl-git-get-remotes))))
        ("show"
         (opts
          (flags
           "-v"
           "--verbose"
           "-n"
           "--format=fuller")
          (args
           (pcmpl-git-get-remotes))))
        ("prune"
         (opts
          (flags
           "-v"
           "--verbose"
           "-n"
           "--dry-run")
          (args
           (pcmpl-git-get-remotes))))
        ("update"
         (opts
          (flags
           "-v"
           "--verbose"
           "-p"
           "--prune")
          (args
           (pcmpl-git-get-remotes))))))
      ("reset"
       (opts
        (flags "--mixed"
               "--soft"
               "--hard"
               "--merge"
               "--keep")
        (args (pcmpl-git-commits-and-files))))
      ("rm"
       (opts
        (flags "-f"
               "-r"
               "-n"
               "--cached"
               "--dry-run"
               "--force")
        (args (pcmpl-entries))))
      ("show"
       (opts
        (flags "-q"
               "-L"
               "--quiet"
               "--source"
               "--use-mailmap"
               "--decorate")
        (args (pcmpl-entries))))
      (("st" "status")
       (opts
        (flags
         "-s"
         "--short"
         "-b"
         "--branch"
         "--porcelain"
         "--long"
         "-u"
         "--untracked-files="
         "-uno"
         "--untracked-files=no"
         "-unormal"
         "--untracked-files-normal"
         "-uall"
         "--untracked-files=all"
         "--ignore-submodules="
         "--ignore-submodules=none"
         "--ignore-submodules=untracked"
         "--ignore-submodules=dirty"
         "--ignore-submodules=all"
         "--ignored"
         "-z"
         "--column="
         "--no-column")))
      ("submodule"
       (or
        ("add"
         (opts
          (flags "-b"
                 "-f"
                 "--force"
                 "--name"
                 "--reference")
          (args (pcmpl-dirs))))
        ("status"
         (opts
          (flags "--cached"
                 "--recursive")
          (args (pcmpl-dirs))))

        ("init"
         (opts
          (args (pcmpl-dirs))))
        ("deinit"
         (opts
          (flags "-f"
                 "--force")
          (args (pcmpl-dirs))))
        ("update"
         (opts
          (flags "--init"
                 "--remote"
                 "-N"
                 "--no-fetch"
                 "-f"
                 "--force"
                 "--rebase"
                 "--reference"
                 "--merge"
                 "--recursive")
          (args (pcmpl-dirs))))
        ("summary"
         (opts
          (flags "--cached"
                 "--files"
                 "--summary-limit"
                 "--commit")
          (args (pcmpl-dirs))))
        ("foreach"
         (opts
          (flags "--recursive")))
        ("sync"
         (opts
          (flags "--recursive")
          (args (pcmpl-dirs))))))
      ("tag"
       (opts
        (args (pcmpl-git-get-refs "tags"))))))

;;;; Haskell

(defun pcmpl-haskell-source-files ()
  (pcmpl-entries
   :select
   (eval-when-compile
     (concat
      "\\."
      (regexp-opt +haskell-extensions+)
      "\\'"))))

(defun pcmpl-haskell-hi-files ()
  (pcmpl-entries
   :select "\\.hi\\'"))

(defun pcmpl-haskell-hp-files ()
  (pcmpl-entries :select (rx ".hp" eos)))

(defun pcmpl-haskell-source-or-obj-files ()
  (pcmpl-entries
   :select
   (eval-when-compile
     (concat
      "\\."
      (regexp-opt
       (append
        `("o"
          "p_o"
          "p_hi"
          "hi"
          "a"
          ,@(fold-platform-os-type
             '("so")
             '("lib" "dll")))
        +haskell-extensions+))
      "\\'"))))

(defun pcmpl-haskell-cabal-project-file ()
  (pcmpl-entries
   :select
   (eval-when-compile
     (rx ".project"
         (* "." (+ any))
         (? ".local")
         (* "." (+ any))))))

;;;###autoload (autoload 'pcomplete/runghc "shell-completion" nil t)
(defpcmpl pcomplete/runghc
  (opts
   (args (pcmpl-haskell-source-or-obj-files))))

;;;###autoload (autoload 'pcomplete/runhaskell "shell-completion" nil t)
(defpcmpl pcomplete/runhaskell
  (opts
   (args (pcmpl-haskell-source-or-obj-files))))

;;;###autoload (autoload 'pcomplete-ghc-flags "shell-completion" nil)
(defvar pcomplete-ghc-flags
  '("-fdiagnostics-color=always"
    "-fdiagnostics-color=auto"
    "-fdiagnostics-color=never"
    "-fdiagnostics-show-caret"
    "-fno-diagnostics-show-caret"
    "-ferror-spans"
    "-fhide-source-paths"
    "-fprint-equality-relations"
    "-fno-print-equality-relations"
    "-fprint-expanded-synonyms"
    "-fno-print-expanded-synonyms"
    "-fprint-explicit-coercions"
    "-fno-print-explicit-coercions"
    "-fprint-explicit-foralls"
    "-fno-print-explicit-foralls"
    "-fprint-explicit-kinds"
    "-fno-print-explicit-kinds"
    "-fprint-explicit-runtime-rep"
    "-fno-print-explicit-runtime-reps"
    "-fprint-explicit-runtime-reps"
    "-fno-print-explicit-runtime-reps"
    "-fprint-potential-instances"
    "-fno-print-potential-instances"
    "-fprint-typechecker-elaboration"
    "-fno-print-typechecker-elaboration"
    "-fprint-unicode-syntax"
    "-fno-print-unicode-syntax"
    "-fshow-hole-constraints"
    "-Rghc-timing"
    "-v"
    "-v1"
    "-v2"
    "-v3"

    "--frontend"
    "--help"
    "-?"
    "--info"
    "--interactive"
    "--make"
    "--mk-dll"
    "--numeric-version"
    "--print-libdir"
    ("--show-iface" (pcmpl-haskell-hi-files))
    "--show-options"
    "--supported-extensions"
    "--supported-languages"
    "--version"
    "-V"
    "-e"
    "-M"

    "-C"
    "-c"
    "-E"
    "-F"
    "-S"
    "-x"

    (("--exclude-module" "-dep-makefile" "-o") (pcmpl-entries))
    (("-i" "-I" "-L" "--dumpdir" "-hidir" "-odir" "-outputdir" "-stubdir" "-tmpdir") (pcmpl-dirs))
    "-ddump-mod-cycles"
    "-dep-suffix"
    "-hcsuf"
    "-hisuf"
    "-include-pkg-deps"
    "-osuf"

    "-keep-hc-file"
    "-keep-hc-files"
    "-keep-hi-files"
    "-no-keep-hi-files"
    "-keep-llvm-file"
    "-keep-llvm-files"
    "-keep-o-files"
    "-no-keep-o-files"
    "-keep-s-file"
    "-keep-s-files"
    "-keep-tmp-files"

    "-ddump-hi"
    "-ddump-hi-diffs"
    "-ddump-minimal-imports"

    "-fforce-recomp"
    "-fno-force-recomp"
    "-fignore-hpc-changes"
    "-fno-ignore-hpc-changes"
    "-fignore-optim-changes"
    "-fno-ignore-optim-changes"

    "-fbreak-on-error"
    "-fno-break-on-error"
    "-fbreak-on-exception"
    "-fno-break-on-exception"
    "-fghci-hist-size="
    "-flocal-ghci-history"
    "-fno-local-ghci-history"
    "-fprint-bind-result"
    "-fno-print-bind-result"
    "-fshow-loaded-modules"
    "-ghci-script"
    "-ignore-dot-ghci"
    "-interactive-print"

    (("-package-db" "-package-env") (pcmpl-entries-ignoring-common))
    "-clear-package-db"
    "-distrust"
    "-distrust-all-packages"
    "-fpackage-trust"
    "-global-package-db"
    "-hide-all-packages"
    "-hide-package"
    "-ignore-package"
    "-no-auto-link-packages"
    "-no-global-package-db"
    "-no-user-package-db"
    "-package"
    "-package-id"
    "-this-unit-id"
    "-trust"
    "-user-package-db"

    "-fdefer-out-of-scope-variables"
    "-fno-defer-out-of-scope-variables"
    "-fdefer-type-errors"
    "-fno-defer-type-errors"
    "-fdefer-typed-holes"
    "-fno-defer-typed-holes"
    "-fhelpful-errors"
    "-fno-helpful-errors"
    "-fmax-pmcheck-iterations="
    "-fshow-warning-groups"
    "-fno-show-warning-groups"
    "-W"
    "-w"
    "-Wall"
    "-w"
    "-Wall-missed-specialisations"
    "-Wno-all-missed-specialisations"
    "-Wamp"
    "-Wno-amp"
    "-Wcompat"
    "-Wno-compat"
    "-Wcpp-undef"
    "-Wdeferred-out-of-scope-variables"
    "-Wno-deferred-out-of-scope-variables"
    "-Wdeferred-type-errors"
    "-Wno-deferred-type-errors"
    "-Wdeprecated-flags"
    "-Wno-deprecated-flags"
    "-Wdeprecations"
    "-Wno-deprecations"
    "-Wdodgy-exports"
    "-Wno-dodgy-exports"
    "-Wdodgy-foreign-imports"
    "-Wno-dodgy-foreign-import"
    "-Wdodgy-imports"
    "-Wno-dodgy-imports"
    "-Wduplicate-constraints"
    "-Wno-duplicate-constraints"
    "-Wduplicate-exports"
    "-Wno-duplicate-exports"
    "-Wempty-enumerations"
    "-Wno-empty-enumerations"
    "-Werror"
    "-Wwarn"
    "-Weverything"
    "-Whi-shadowing"
    "-Wno-hi-shadowing"
    "-Widentities"
    "-Wno-identities"
    "-Wimplicit-prelude"
    "-Wno-implicit-prelude"
    "-Wincomplete-patterns"
    "-Wno-incomplete-patterns"
    "-Wincomplete-record-updates"
    "-Wno-incomplete-record-updates"
    "-Wincomplete-uni-patterns"
    "-Wno-incomplete-uni-patterns"
    "-Winline-rule-shadowing"
    "-Wno-inline-rule-shadowing"
    "-Wmissed-specialisations"
    "-Wno-missed-specialisations"
    "-Wmissing-export-lists"
    "-fnowarn-missing-export-lists"
    "-Wmissing-exported-signatures"
    "-Wno-missing-exported-signatures"
    "-Wmissing-exported-sigs"
    "-Wno-missing-exported-sigs"
    "-Wmissing-fields"
    "-Wno-missing-fields"
    "-Wmissing-home-modules"
    "-Wno-missing-home-modules"
    "-Wmissing-import-lists"
    "-fnowarn-missing-import-lists"
    "-Wmissing-local-signatures"
    "-Wno-missing-local-signatures"
    "-Wmissing-local-sigs"
    "-Wno-missing-local-sigs"
    "-Wmissing-methods"
    "-Wno-missing-methods"
    "-Wmissing-monadfail-instances"
    "-Wno-missing-monadfail-instances"
    "-Wmissing-pattern-synonym-signatures"
    "-Wno-missing-pattern-synonym-signatures"
    "-Wmissing-signatures"
    "-Wno-missing-signatures"
    "-Wmonomorphism-restriction"
    "-Wno-monomorphism-restriction"
    "-Wname-shadowing"
    "-Wno-name-shadowing"
    "-Wno-compat"
    "-Wcompat"
    "-Wnoncanonical-monad-instances"
    "-Wno-noncanonical-monad-instances"
    "-Wnoncanonical-monadfail-instances"
    "-Wno-noncanonical-monadfail-instances"
    "-Wnoncanonical-monoid-instances"
    "-Wno-noncanonical-monoid-instances"
    "-Worphans"
    "-Wno-orphans"
    "-Woverflowed-literals"
    "-Wno-overflowed-literals"
    "-Woverlapping-patterns"
    "-Wno-overlapping-patterns"
    "-Wpartial-fields"
    "-Wno-partial-fields"
    "-Wpartial-type-signatures"
    "-Wno-partial-type-signatures"
    "-Wredundant-constraints"
    "-Wno-redundant-constraints"
    "-Wsafe"
    "-Wno-safe"
    "-Wsemigroup"
    "-Wno-semigroup"
    "-Wsimplifiable-class-constraints"
    "-Wno-overlapping-patterns"
    "-Wtabs"
    "-Wno-tabs"
    "-Wtrustworthy-safe"
    "-Wno-safe"
    "-Wtype-defaults"
    "-Wno-type-defaults"
    "-Wtyped-holes"
    "-Wno-typed-holes"
    "-Wunbanged-strict-patterns"
    "-Wno-unbanged-strict-patterns"
    "-Wunrecognised-pragmas"
    "-Wno-unrecognised-pragmas"
    "-Wunrecognised-warning-flags"
    "-Wno-unrecognised-warning-flags"
    "-Wunsafe"
    "-Wno-unsafe"
    "-Wunsupported-calling-conventions"
    "-Wno-unsupported-calling-conventions"
    "-Wunsupported-llvm-version"
    "-Wno-monomorphism-restriction"
    "-Wunticked-promoted-constructors"
    "-Wno-unticked-promoted-constructors"
    "-Wunused-binds"
    "-Wno-unused-binds"
    "-Wunused-do-bind"
    "-Wno-unused-do-bind"
    "-Wunused-foralls"
    "-Wno-unused-foralls"
    "-Wunused-imports"
    "-Wno-unused-imports"
    "-Wunused-local-binds"
    "-Wno-unused-local-binds"
    "-Wunused-matches"
    "-Wno-unused-matches"
    "-Wunused-pattern-binds"
    "-Wno-unused-pattern-binds"
    "-Wunused-top-binds"
    "-Wno-unused-top-binds"
    "-Wunused-type-patterns"
    "-Wno-unused-type-patterns"
    "-Wwarn"
    "-Werror"
    "-Wwarnings-deprecations"
    "-Wno-warnings-deprecations"
    "-Wwrong-do-bind"
    "-Wno-wrong-do-bind"


    "-O"
    "-O1"
    "-O0"
    "-O2"
    "-Odph"


    "-fcall-arity"
    "-fno-call-arity"
    "-fcase-folding"
    "-fno-case-folding"
    "-fcase-merge"
    "-fno-case-merge"
    "-fcmm-elim-common-blocks"
    "-fno-cmm-elim-common-blocks"
    "-fcmm-sink"
    "-fno-cmm-sink"
    "-fcpr-anal"
    "-fno-cpr-anal"
    "-fcross-module-specialise"
    "-fno-cross-module-specialise"
    "-fcse"
    "-fno-cse"
    "-fdicts-cheap"
    "-fno-dicts-cheap"
    "-fdicts-strict"
    "-fno-dicts-strict"
    "-fdmd-tx-dict-sel"
    "-fno-dmd-tx-dict-sel"
    "-fdo-eta-reduction"
    "-fno-do-eta-reduction"
    "-fdo-lambda-eta-expansion"
    "-fno-do-lambda-eta-expansion"
    "-feager-blackholing"
    "-fenable-rewrite-rules"
    "-fno-enable-rewrite-rules"
    "-fexcess-precision"
    "-fno-excess-precision"
    "-fexitification"
    "-fno-exitification"
    "-fexpose-all-unfoldings"
    "-fno-expose-all-unfoldings"
    "-ffloat-in"
    "-fno-float-in"
    "-ffull-laziness"
    "-fno-full-laziness"
    "-ffun-to-thunk"
    "-fno-fun-to-thunk"
    "-fignore-asserts"
    "-fno-ignore-asserts"
    "-fignore-interface-pragmas"
    "-fno-ignore-interface-pragmas"
    "-flate-dmd-anal"
    "-fno-late-dmd-anal"
    "-fliberate-case"
    "-fno-liberate-case"
    "-fliberate-case-threshold="
    "-fno-liberate-case-threshold"
    "-fllvm-pass-vectors-in-regs"
    "-fno-llvm-pass-vectors-in-regs"
    "-floopification"
    "-fno-loopification"
    "-fmax-inline-alloc-size="
    "-fmax-inline-memcpy-insns="
    "-fmax-inline-memset-insns="
    "-fmax-relevant-binds="
    "-fno-max-relevant-bindings"
    "-fmax-simplifier-iterations="
    "-fmax-uncovered-patterns="
    "-fmax-valid-substitutions="
    "-fno-max-valid-substitutions"
    "-fmax-worker-args="
    "-fno-opt-coercion"
    "-fno-pre-inlining"
    "-fno-state-hack"
    "-fomit-interface-pragmas"
    "-fno-omit-interface-pragmas"
    "-fomit-yields"
    "-fno-omit-yields"
    "-foptimal-applicative-do"
    "-fno-optimal-applicative-do"
    "-fpedantic-bottoms"
    "-fno-pedantic-bottoms"
    "-fregs-graph"
    "-fno-regs-graph"
    "-fregs-iterative"
    "-fno-regs-iterative"
    "-fsimpl-tick-factor="
    "-fsimplifier-phases="
    "-fsolve-constant-dicts"
    "-fno-solve-constant-dicts"
    "-fspec-constr"
    "-fno-spec-constr"
    "-fspec-constr-count="
    "-fno-spec-constr-count"
    "-fspec-constr-keen"
    "-fno-spec-constr-keen"
    "-fspec-constr-threshold="
    "-fno-spec-constr-threshold"
    "-fspecialise"
    "-fno-specialise"
    "-fspecialise-aggressively"
    "-fno-specialise-aggressively"
    "-fstatic-argument-transformation"
    "-fno-static-argument-transformation"
    "-fstg-cse"
    "-fno-stg-cse"
    "-fstrictness"
    "-fno-strictness"
    "-fstrictness-before="
    "-funbox-small-strict-fields"
    "-fno-unbox-small-strict-fields"
    "-funbox-strict-fields"
    "-fno-unbox-strict-fields"
    "-funfolding-creation-threshold="
    "-funfolding-dict-discount="
    "-funfolding-fun-discount="
    "-funfolding-keeness-factor="
    "-funfolding-use-threshold="
    "-fvectorisation-avoidance"
    "-fno-vectorisation-avoidance"
    "-fvectorise"
    "-fno-vectorise"

    "-fno-prof-auto"
    "-fprof-auto"
    "-fno-prof-cafs"
    "-fprof-cafs"
    "-fno-prof-count-entries"
    "-fprof-count-entries"
    "-fprof-auto"
    "-fno-prof-auto"
    "-fprof-auto-calls"
    "-fno-prof-auto-calls"
    "-fprof-auto-exported"
    "-fno-prof-auto"
    "-fprof-auto-top"
    "-fno-prof-auto"
    "-fprof-cafs"
    "-fno-prof-cafs"
    "-prof"
    "-ticky"

    "-fhpc"

    "-cpp"
    "-D"
    "-U"

    "-dynamic-too"
    "-fasm"
    "-fllvm"
    "-fbyte-code"
    "-fno-code"
    "-fobject-code"
    "-fPIC"
    "-fPIE"
    "-fwrite-interface"

    "-c"
    "-debug"
    "-dylib-install-name"
    "-dynamic"
    "-dynload"
    "-eventlog"
    "-fno-embed-manifest"
    "-fno-gen-manifest"
    "-fno-shared-implib"
    "-framework"
    "-framework-path"
    "-fwhole-archive-hs-libs"
    "-l"
    "-main-is"
    "-no-hs-main"
    "-no-rtsopts-suggestions"
    "-package"
    "-pie"
    "-rdynamic"
    "-rtsopts"
    "-rtsopts=none"
    "-rtsopts=some"
    "-rtsopts=all"
    "-shared"
    "-split-objs"
    "-split-sections"
    "-static"
    "-staticlib"
    "-threaded"
    "-with-rtsopts="

    "-fplugin-opt="
    "-fplugin="
    "-hide-all-plugin-packages"
    "-plugin-package"
    "-plugin-package-id"

    "-pgma"
    "-pgmc"
    "-pgmdll"
    "-pgmF"
    "-pgmi"
    "-pgmL"
    "-pgml"
    "-pgmlc"
    "-pgmlibtool"
    "-pgmlo"
    "-pgmP"
    "-pgms"
    "-pgmwindres"

    "-opta"
    "-optc"
    "-optdll"
    "-optF"
    "-opti"
    "-optL"
    "-optl"
    "-optlc"
    "-optlo"
    "-optP"
    "-optwindres"

    "-msse2"
    "-msse4.2"

    "-dcmm-lint"
    "-dcore-lint"
    "-ddump-asm"
    "-ddump-asm-expanded"
    "-ddump-asm-liveness"
    "-ddump-asm-native"
    "-ddump-asm-regalloc"
    "-ddump-asm-regalloc-stages"
    "-ddump-asm-stats"
    "-ddump-bcos"
    "-ddump-cmm"
    "-ddump-cmm-caf"
    "-ddump-cmm-cbe"
    "-ddump-cmm-cfg"
    "-ddump-cmm-cps"
    "-ddump-cmm-from-stg"
    "-ddump-cmm-info"
    "-ddump-cmm-proc"
    "-ddump-cmm-procmap"
    "-ddump-cmm-raw"
    "-ddump-cmm-sink"
    "-ddump-cmm-sp"
    "-ddump-cmm-split"
    "-ddump-cmm-switch"
    "-ddump-cmm-verbose"
    "-ddump-core-stats"
    "-ddump-cse"
    "-ddump-deriv"
    "-ddump-ds"
    "-ddump-ec-trace"
    "-ddump-foreign"
    "-ddump-if-trace"
    "-ddump-inlinings"
    "-ddump-json"
    "-ddump-llvm"
    "-ddump-occur-anal"
    "-ddump-opt-cmm"
    "-ddump-parsed"
    "-ddump-parsed-ast"
    "-ddump-prep"
    "-ddump-rn"
    "-ddump-rn-ast"
    "-ddump-rn-stats"
    "-ddump-rn-trace"
    "-ddump-rule-firings"
    "-ddump-rule-rewrites"
    "-ddump-rules"
    "-ddump-simpl"
    "-ddump-simpl-iterations"
    "-ddump-simpl-stats"
    "-ddump-spec"
    "-ddump-splices"
    "-ddump-stg"
    "-ddump-str-signatures"
    "-ddump-stranal"
    "-ddump-tc"
    "-ddump-tc-ast"
    "-ddump-tc-trace"
    "-ddump-timings"
    "-ddump-to-file"
    "-ddump-types"
    "-ddump-vect"
    "-ddump-vt-trace"
    "-ddump-worker-wrapper"
    "-dfaststring-stats"
    "-dinitial-unique="
    "-dno-debug-output"
    "-ddebug-output"
    "-dppr-case-as-let"
    "-dppr-cols="
    "-dppr-debug"
    "-dppr-user-length"
    "-dshow-passes"
    "-dstg-lint"
    "-dsuppress-all"
    "-dsuppress-coercions"
    "-dsuppress-idinfo"
    "-dsuppress-module-prefixes"
    "-dsuppress-stg-free-vars"
    "-dsuppress-ticks"
    "-dsuppress-type-applications"
    "-dsuppress-type-signatures"
    "-dsuppress-unfoldings"
    "-dsuppress-uniques"
    "-dsuppress-var-kinds"
    ("-dth-dec-file" (pcmpl-entries))
    "-dunique-increment=⟨i⟩"
    "-dverbose-core2core"
    "-dverbose-stg2stg"
    "-falignment-sanitisation"
    "-fcatch-bottoms"
    "-fllvm-fill-undef-with-garbage"
    "-g"

    "-fexternal-interpreter"
    "-fglasgow-exts"
    "-fno-glasgow-exts"
    "-ghcversion-file"
    "-H"
    "-j"))

;;;###autoload (autoload 'pcomplete/ghc "shell-completion" nil t)
(defpcmpl pcomplete/ghc
  `(opts
    (flags ,@pcomplete-ghc-flags)
    (args (pcmpl-haskell-source-or-obj-files)))
  :evaluate-definition t)

;;;###autoload (autoload 'pcomplete/cabal "shell-completion" nil t)
(defpcmpl pcomplete/cabal
  (let* ((programs '("alex"
                     "ar"
                     "c2hs"
                     "cpphs"
                     ;;"ffihugs"
                     "gcc"
                     "ghc"
                     "ghc-pkg"
                     ;;"greencard"
                     "haddock"
                     "happy"
                     "hmake"
                     "hpc"
                     "hsc2hs"
                     "hscolour"
                     ;;"hugs"
                     ;;"jhc"
                     "ld"
                     ;;"lhc"
                     ;;"lhc-pkg"
                     ;;"nhc98"
                     "pkg-config"
                     "ranlib"
                     "strip"
                     "tar"
                     "uhc"))
         (help-flags '("-h"
                       "--help"))
         (help-verbosity-flags `(,@help-flags
                                 "-v"
                                 "--verbose"
                                 "--verbose=0"
                                 "--verbose=1"
                                 "--verbose=2"
                                 "--verbose=3"))
         (builddir-flags '(("--builddir" (pcmpl-dirs))))
         (solver-flags '("--solver=modular"))
         (fetch-flags '("--max-backjumps"
                        "--reorder-goals"
                        "--count-conflicts"
                        "--minimize-conflict-set"
                        "--independent-goals"
                        "--shadow-installed-packages"
                        "--strong-flags"
                        "--allow-boot-library-installs"
                        "--reject-unconstrained-dependencies=none"
                        "--reject-unconstrained-dependencies=all"))
         (program-options-flags
          (-mapcat (lambda (p)
                     `((,(concat "--" p)
                        (pcmpl-entries-ignoring-common))
                       ,(concat "--" p "-option")
                       ,(concat "--" p "-options")))
                   programs))
         (deps-flags `("--only-dependencies"
                       "--dependencies-only"))
         (configure-flags `(,@help-verbosity-flags
                            ,@builddir-flags
                            "-g"
                            "--ghc"
                            ;; "--nhc98"
                            ;; "--jhc"
                            ;; "--lhc"
                            ;; "--hugs"
                            "--uhc"
                            "-w"
                            "--with-compiler"
                            "--with-hc-pkg"
                            (("--prefix"
                              "--bindir"
                              "--libdir"
                              "--libsubdir"
                              "--libexecdir"
                              "--datadir"
                              "--datasubdir"
                              "--docdir"
                              "--htmldir"
                              "--haddockdir"
                              "--sysconfdir"
                              "-b"
                              "--scratchdir")
                             (pcmpl-dirs))

                            "--program-prefix"
                            "--program-suffix"
                            "-p"
                            "--library-profiling-detail=default"
                            "--library-profiling-detail=none"
                            "--library-profiling-detail=exported-functions"
                            "--library-profiling-detail=toplevel-functions"
                            "--library-profiling-detail=all-functions"
                            "--profiling-detail=default"
                            "--profiling-detail=none"
                            "--profiling-detail=exported-functions"
                            "--profiling-detail=toplevel-functions"
                            "--profiling-detail=all-functions"
                            ,@(--mapcat (list (concat "--enable-" it)
                                              (concat "--disable-" it))
                                        '("library-vanilla"
                                          "library-profiling"
                                          "shared"
                                          "static"
                                          "executable-dynamic"
                                          "executable-static"
                                          "profiling"
                                          "executable-profiling"
                                          "library-for-ghci"
                                          "split-objs"
                                          "spit-sections"
                                          "library-stripping"
                                          "executable-stripping"
                                          "deterministic"
                                          "tests"
                                          "coverage"
                                          "library-coverage"
                                          "benchmarks"
                                          "relocatable"
                                          "documentation"
                                          "per-component"))

                            "-O"
                            "--enable-optimization"
                            "--enable-optimization=0"
                            "--enable-optimization=1"
                            "--enable-optimization=2"
                            "--disable-optimization"

                            "--enable-debug-info"
                            "--enable-debug-info=1"
                            "--enable-debug-info=2"
                            "--enable-debug-info=3"
                            "--disable-debug-info"

                            "--configure-option"
                            "--user"
                            "--global"
                            ("--package-db" (pcmpl-entries-ignoring-common))
                            "-f"
                            "--flags"
                            "--allow-depending-on-private-libs"
                            (("--extra-include-dirs" "--extra-lib-dirs" "--extra-prog-path")
                             (pcmpl-dirs))
                            ,@program-options-flags
                            "--cabal-lib-version"
                            "--constraint"
                            "--preference"
                            ,@fetch-flags
                            ,@solver-flags
                            "--allow-older"
                            "--allow-newer"
                            "--write-ghc-environment-files=always"
                            "--write-ghc-environment-files=never"
                            "--write-ghc-environment-files=ghc8.4.4+"

                            "--dry-run"

                            "--instantiate-with"
                            "--ipid"
                            "--cid"

                            "--doc-index-file"

                            "--reinstall"
                            "--avoid-reinstalls"
                            "--force-reinstalls"
                            "--upgrade-dependencies"
                            "--index-state="
                            ("--symlink-bindir" (pcmpl-dirs))
                            "--build-summary="
                            "--build-log="
                            "--remote-build-reporting"
                            "--remote-build-reporting=none"
                            "--remote-build-reporting=anonymous"
                            "--remote-build-reporting=detailed"
                            "--report-planning-failure"

                            "--one-shot"
                            "--run-tests"
                            "-j"
                            "--jobs"
                            "--offline"
                            ("--project-file" (pcmpl-haskell-cabal-project-file)))))
    `(or
      ("install"
       (opts
        (flags ,@configure-flags
               ,@deps-flags
               "--enable-documentation"
               "--disable-documentation"
               "--doc-index-file"
               ,@fetch-flags
               "--haddock-hoogle"
               "--haddock-html"
               "--haddock-html-location=URL"
               "--haddoc-for-hackage"
               "--haddock-executables"
               "--haddock-tests"
               "--haddock-benchmarks"
               "--haddock-all"
               "--haddock-internal"
               (("--haddock-css" "--haddock-hscolour-css") (pcmpl-entries-ignoring-common))
               "--haddock-quickjump"
               "--haddock-hyperlink-source"
               "--haddock-contents-location=URL"

               ("-package-env" (pcmpl-entries-ignoring-common))
               "--lib"
               "--overwrite-policy=always"
               "--overwrite-policy=never"
               "--install-method=copy"
               "--install-method=symlink"
               ("--installdir" (pcmpl-dirs)))))
      ("update"
       (opts
        (flags ,@help-verbosity-flags)))
      ("list"
       (opts
        (flags ,@help-verbosity-flags
               "--installed"
               "--simple-output")))
      ("info"
       (opts
        (flags ,@help-verbosity-flags)))
      ("fetch"
       (opts
        (flags ,@help-verbosity-flags
               "--dependencies"
               "--no-dependencies"
               ,@solver-flags
               ,@fetch-flags)))
      ("get"
       (opts
        (flags ,@help-verbosity-flags
               (("-d" "--destdir") (pcmpl-dirs))
               "-s"
               "--source-repository"
               "--source-repository=head"
               "--source-repository=this"
               "--pristine")))
      ("check"
       (opts
        (flags ,@help-flags)))
      ("sdist")
      ("upload")
      ("report")
      ("run"
       (opts
        (flags ,@help-verbosity-flags
               ,@builddir-flags
               ,@deps-flags
               "-j"
               "--jobs"
               ,@program-options-flags)))
      ("init"
       (opts
        (flags "-h"
               "--help"
               "-n"
               "--non-interactive"
               "-q"
               "--quiet"
               "--no-comments"
               "-m"
               "--minimal"
               "--overwrite"
               "--package-dir"
               "-p"
               "--package-name"
               "--version"
               "--cabal-version"
               "-l"
               "--license"
               "-a"
               "--author"
               "-e"
               "--email"
               "-u"
               "--homepage"
               "-s"
               "--synopsis"
               "-c"
               "--category"
               "-x"
               "--extra-source-file"
               "--is-library"
               "--is-executable"
               "--language"
               "-o"
               "--expose-module"
               "--extension"
               "-d"
               "--dependency"
               "--source-dir"
               "--build-tool"
               "-v"
               "--verbose"
               "--verbose=0"
               "--verbose=1"
               "--verbose=2"
               "--verbose=3")))
      ("configure"
       (opts
        (flags ,@configure-flags)))
      ("build"
       (opts
        (flags ,@configure-flags
               ,@deps-flags
               "-j"
               "--jobs")
        (args (pcomplete-here '("all")))))
      ("repl"
       (opts
        (flags ,@help-verbosity-flags
               ,@builddir-flags
               "-j"
               "--jobs"
               "--build-depends")))
      ("sandbox"
       (or ("init")
           ("delete")
           ("add-source"
            (opts
             (args (pcmpl-entries-ignoring-common))))
           ("hc-pkg")
           ("list-sources")))
      ("copy")
      ("haddock")
      ("clean"
       (opts
        (flags
         ,@help-verbosity-flags
         ,@builddir-flags
         "-s"
         "--save-configure")))
      ("hscolour")
      ("register")
      ("test"
       (opts
        (flags ,@help-verbosity-flags
               ,@builddir-flags
               "--test-log="
               "--test-machine-log="
               "--test-show-details=always"
               "--test-show-details=never"
               "--test-show-details=failures"
               "--test-show-details=streaming"
               "--test-show-details=direct"
               "--test-keep-tix-files"
               "--test-fail-when-no-test-suites"
               "--test-wrapper"
               "--test-options"
               "--test-option"
               "-j"
               "--jobs")))
      ("bench"
       (opts
        (flags ,@builddir-flags)))
      ("help")))
  :evaluate-definition t)

;;;###autoload (autoload 'pcomplete/hp2ps "shell-completion" nil t)
(defpcmpl pcomplete/hp2ps
  (opts
   (flags "-b"
          "-d"
          "-efin"
          "-efmm"
          "-efpt"
          "-g"
          "-i+"
          "-i-"
          "-M"
          "-mN"
          "-m0"
          "-p"
          "-s"
          "-tf"
          "-y"
          "-c")
   (args (pcmpl-haskell-hp-files))))

;;;###autoload (autoload 'pcomplete/hp2pdf "shell-completion" nil t)
(defpcmpl pcomplete/hp2pdf
  (opts
   (args (pcmpl-haskell-hp-files))))

;;;###autoload (autoload 'pcomplete/hp2pretty "shell-completion" nil t)
(defpcmpl pcomplete/hp2pretty
  (opts
   (flags
    "--uniform-scale=none"
    "--uniform-scale=time"
    "--uniform-scale=memory"
    "--uniform-scale=both")
   (args (pcmpl-haskell-hp-files))))

;;;###autoload (autoload 'pcomplete/eventlog2html "shell-completion" nil t)
(defpcmpl pcomplete/eventlog2html
  (opts
   (flags
    "--sort=size"
    "--sort=stdev"
    "--sort=name"
    "--sort=gradient"
    "--reverse"
    "--limit-detailed"
    (("-p" "--heap-profile") (pcmpl-haskell-hp-files))
    "--no-include-js"
    "-j"
    "--json"
    "--no-traces"
    "--include-trace-events"
    "--y-axis"
    "-i"
    "--include"
    "-x"
    "--exclude"
    ("-o" (pcmpl-entries :select (rx ".html" eos))))
   (args (pcmpl-entries :select (rx ".eventlog" eos)))))

;;;###autoload (autoload 'pcomplete/profiterole "shell-completion" nil t)
(defpcmpl pcomplete/profiterole
  (opts
   (args (pcmpl-entries :select (rx ".prof" eos)))))


;;;###autoload (autoload 'pcomplete/hp2svg "shell-completion" nil nil)
(defalias 'pcomplete/hp2svg 'pcomplete/hp2pretty)

;;;###autoload (autoload 'pcomplete/stack "shell-completion" nil t)
(defpcmpl pcomplete/stack
  (let ((standard-flags
         '("--help"
           "--version"
           "--numeric-version"
           "--verbosity"
           "--verbosity=silent"
           "--verbosity=error"
           "--verbosity=warn"
           "--verbosity=info"
           "--verbosity=debug"
           "-v"
           "--verbose"
           "--work-dir"
           "--system-ghc"
           "--no-system-ghc"
           "--install-ghc"
           "--no-install-ghc"
           "--arch"
           "--os"
           "--ghc-variant"
           "--ghc-variant=integersimple"
           "-j"
           "--jobs"
           "--extra-include-dirs"
           "--extra-lib-dirs"
           "--skip-ghc-check"
           "--no-skip-ghc-check"
           "--skip-msys"
           "--no-skip-msys"
           "--local-bin-path"
           "--modify-code-page"
           "--no-modify-code-page"
           "--allow-different-user"
           "--no-allow-different-user"
           "--resolver"
           "--compiler"
           "--terminal"
           "--no-terminal"
           ("--stack-yaml" (pcmpl-entries :select "\\.yaml\\'"))))
        (build-args
         '("--dry-run"
           "--pedantic"
           "--fast"
           "--dependencies-only"
           "--only-snapshot"
           "--only-dependencies"
           "--file-watch"
           "--file-watch-poll"
           "--only-configure"
           "--trace"
           "--profile"
           "--no-strip"
           "--library-profiling"
           "--no-library-profiling"
           "--executable-profiling"
           "--no-executable-profiling"
           "--library-stripping"
           "--no-library-stripping"
           "--executable-stripping"
           "--no-executable-stripping"
           "--haddock"
           "--no-haddock"
           "--open"
           "--no-open"
           "--haddock-deps"
           "--no-haddock-deps"
           "--haddock-internal"
           "--no-haddock-internal"
           "--copy-bins"
           "--no-copy-bins"
           "--prefetch"
           "--no-prefetch"
           "--keep-going"
           "--no-keep-going"
           "--force-dirty"
           "--no-force-dirty"
           "--test"
           "--no-test"
           "--rerun-tests"
           "--no-rerun-tests"
           "--coverage"
           "--no-run-tests"
           "--bench"
           "--no-bench"
           "--no-run-benchmarks"
           "--reconfigure"
           "--no-reconfigure"
           "--cabal-verbose"
           "--no-cabal-verbose"
           "--split-objs"
           "--no-split-objs"
           "--help"

           "--ghc-options"
           "--flag"
           "--exec"
           "--haddock-arguments"
           "--test-arguments"
           "--benchmark-arguments")))
    `(or
      ("build"
       (opts
        (flags
         ,@standard-flags
         ,@build-args)))
      ("install"
       (opts
        (flags
         ,@standard-flags
         ,@build-args)))
      ("uninstall"
       (opts
        (flags
         ,@standard-flags)))
      ("test"
       (opts
        (flags
         ,@standard-flags
         ,@build-args)))
      ("bench"
       (opts
        (flags
         ,@standard-flags
         ,@build-args)))
      ("haddock"
       (opts
        (flags
         ,@standard-flags)))
      ("new"
       (opts
        (flags
         ,@standard-flags)))
      ("templates"
       (opts
        (flags
         ,@standard-flags)))
      ("init"
       (opts
        (flags
         ,@standard-flags)))
      ("solver"
       (opts
        (flags
         ,@standard-flags)))
      ("setup"
       (opts
        (flags
         ,@standard-flags)))
      ("path"
       (opts
        (flags
         ,@standard-flags)))
      ("unpack"
       (opts
        (flags
         ,@standard-flags)))
      ("update"
       (opts
        (flags
         ,@standard-flags)))
      ("upgrade"
       (opts
        (flags
         ,@standard-flags)))
      ("upload"
       (opts
        (flags
         ,@standard-flags)))
      ("sdist"
       (opts
        (flags
         ,@standard-flags)))
      ("dot"
       (opts
        (flags
         ,@standard-flags)))
      ("exec"
       (opts
        (flags
         ,@standard-flags)))
      ("ghc"
       (opts
        (flags
         ,@standard-flags)))
      ("ghci"
       (opts
        (flags
         ,@standard-flags)))
      ("repl"
       (opts
        (flags
         ,@standard-flags)))
      ("runghc"
       (opts
        (flags
         ,@standard-flags)))
      ("runhaskell"
       (opts
        (flags
         ,@standard-flags)))
      ("eval"
       (opts
        (flags
         ,@standard-flags)))
      ("clean"
       (opts
        (flags
         ,@standard-flags)))
      ("list-dependencies"
       (opts
        (flags
         ,@standard-flags)))
      ("query"
       (opts
        (flags
         ,@standard-flags)))
      ("ide"
       (opts
        (flags
         ,@standard-flags)))
      ("docker"
       (opts
        (flags
         ,@standard-flags)))
      ("config"
       (opts
        (flags
         ,@standard-flags)))
      ("image"
       (opts
        (flags
         ,@standard-flags)))
      ("hpc"
       (opts
        (flags
         ,@standard-flags)))
      ("sig"
       (opts
        (flags
         ,@standard-flags)))))
  :evaluate-definition t)

;;;###autoload (autoload 'pcomplete/hpack "shell-completion" nil t)
(defpcmpl pcomplete/hpack
  (opts
   (flags
    "--help"
    "--version"
    "--silent"
    "-f"
    "--fource")
   (args (pcmpl-entries :select "\\`package\\.yaml\\'"))))

;;;###autoload (autoload 'pcomplete/hlint "shell-completion" nil t)
(defpcmpl pcomplete/hlint
  (let ((standard-flags
         '("-v"
           "--verbose"
           "-q"
           "--quiet"
           "--help"
           "--numeric-version"
           "-V"
           "--version"))
        (language-flags
         '((("-X" "--language") (get-haskell-language-extensions))))
        (extension-flags
         '("-e"
           "--extension"))
        (hint-flags
         '(("--datadir" (pcmpl-dirs))
           (("-r" "--report") (pcmpl-entries))
           (("-h" "--hint") (pcmpl-entries))
           "-w"
           "--with"))
        (cpp-flags
         '("-p"
           "--path"
           "--cpp-define"
           ("--cpp-include" (pcmpl-dirs))
           ("--cpp-file" (pcmpl-entries))
           "--cpp-simple"
           "--cpp-ansi")))
    `(or
      ("lint"
       (opts
        (flags ,@standard-flags
               ,@hint-flags
               "-i"
               "--ignore"
               "-s"
               "--show"
               ,@extension-flags
               ,@language-flags
               "--cross"
               "-f"
               "--find"
               "-d"
               ,@cpp-flags
               "--json"
               "--no-summary"
               "--no-exit-code"
               "--serialise"
               "--refactor"
               "--refactor-options"
               "--with-refactor"
               "-d"
               "--default"

               "-c"
               "--color"
               "--color=always"
               "--color=never"
               "--color=auto"
               "--colour")
        (args (pcmpl-haskell-source-files))))
      ("grep"
       (opts
        (flags
         ,@standard-flags
         ,@extension-flags
         ,@language-flags
         ,@cpp-flags))
       (args
        (pcmpl-haskell-source-files)))
      ("test"
       (opts
        (flags
         ,@standard-flags
         ,@hint-flags
         ("--proof" (pcmpl-entries))
         ("--tempdir" (pcmpl-dirs))
         "--quickcheck"
         "--typecheck")))
      ("hse"
       (opts
        (flags
         ,@standard-flags
         ,@language-flags)))))
  :evaluate-definition t)

;;;; C, low-level stuff

;;;###autoload (autoload 'pcomplete/nm "shell-completion" nil t)
(defpcmpl pcomplete/nm
  (opts
   (flags "-a"
          "--debug-syms"
          "-A"
          "--print-file-name"
          "-B"
          "-C"
          "--demangle"
          "--demangle=auto"
          "--demangle=gnu"
          "--demangle=lucid"
          "--demangle=arm"
          "--demangle=hp"
          "--demangle=edg"
          "--demangle=gnu-v3"
          "--demangle=java"
          "--demangle=gnat"
          "--no-demangle"
          "-D"
          "--dynamic"
          "--defined-only"
          "-e"
          "-f"
          "--format"
          "--format=bsd"
          "--format=sysv"
          "--format=posix"
          "-g"
          "--extern-only"
          "-l"
          "--line-numbers"
          "-n"
          "--numeric-sort"
          "-o"
          "-p"
          "--no-sort"
          "-P"
          "--portability"
          "-r"
          "--reverse-sort"
          "--plugin"
          "-S"
          "--print-size"
          "-s"
          "--print-armap"
          "--size-sort"
          "--special-syms"
          "--synthetic"
          "-t"
          "--radix"
          "--target"
          "-u"
          "--undefined-only"
          "-X"
          "-h"
          "--help"
          "-V"
          "--version")
   (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/readelf "shell-completion" nil t)
(defpcmpl pcomplete/readelf
  (opts
   (flags "-a"
          "--all"
          "-h"
          "--file-header"
          "-l"
          "--program-headers"
          "--segments"
          "-S"
          "--section-headers"
          "--sections"
          "-g"
          "--section-groups"
          "-t"
          "--section-details"
          "-e"
          "--headers"
          "-s"
          "--syms"
          "--symbols"
          "--dyn-syms"
          "-n"
          "--notes"
          "-r"
          "--relocs"
          "-u"
          "--unwind"
          "-d"
          "--dynamic"
          "-V"
          "--version-info"
          "-A"
          "--arch-specific"
          "-c"
          "--archive-index"
          "-D"
          "--use-dynamic"
          "-x"
          "--hex-dump"
          "-p"
          "--string-dump"
          "-R"
          "--relocated-dump"
          "-z"
          "--decompress"

          ("--debug-dump" '("rawline" "decodedline" "info" "abbrev" "pubnames" "aranges" "macro"
                            "frames" "frames-interp" "str" "loc" "Ranges" "pubtypes" "gdb_index"
                            "trace_info" "trace_abbrev" "trace_aranges" "addr" "cu_index"
                            "links" "follow-links"))
          "--debug-dump=rawline"
          "--debug-dump=decodedline"
          "--debug-dump=info"
          "--debug-dump=abbrev"
          "--debug-dump=pubnames"
          "--debug-dump=aranges"
          "--debug-dump=macro"
          "--debug-dump=frames"
          "--debug-dump=frames-interp"
          "--debug-dump=str"
          "--debug-dump=loc"
          "--debug-dump=Ranges"
          "--debug-dump=pubtypes"
          "--debug-dump=gdb_index"
          "--debug-dump=trace_info"
          "--debug-dump=trace_abbrev"
          "--debug-dump=trace_aranges"
          "--debug-dump=addr"
          "--debug-dump=cu_index"
          "--debug-dump=links"
          "--debug-dump=follow-links"

          "--dwarf-depth"
          "--dwarf-start"

          "--ctf"
          "--ctf-parent"
          "--ctf-symbols"
          "--ctf-strings"

          "-I"
          "--histogram"
          "-W"
          "--wide")
   (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/objdump "shell-completion" nil t)
(defpcmpl pcomplete/objdump
  (opts
   (flags
    "-a"
    "--archive-headers"
    "-f"
    "--file-headers"
    "-p"
    "--private-headers"
    "-P"
    "--private"
    "-h"
    "--section-headers"
    "--headers"
    "-x"
    "--all-headers"
    "-d"
    "--disassemble"
    "-D"
    "--disassemble-all"
    "--disassemble=<sym>"
    "-S"
    "--source"
    "--source-comment"
    "-s"
    "--full-contents"
    "-g"
    "--debugging"
    "-e"
    "--debugging-tags"
    "-G"
    "--stabs"

    ("--dwarf" '("rawline" "decodedline" "info" "abbrev" "pubnames" "aranges" "macro" "frames"
                 "frames-interp" "str" "loc" "Ranges" "pubtypes" "gdb_index" "trace_info"
                 "trace_abbrev" "trace_aranges" "addr" "cu_index" "links" "follow-links"))
    "--dwarf=rawline"
    "--dwarf=decodedline"
    "--dwarf=info"
    "--dwarf=abbrev"
    "--dwarf=pubnames"
    "--dwarf=aranges"
    "--dwarf=macro"
    "--dwarf=frames"
    "--dwarf=frames-interp"
    "--dwarf=str"
    "--dwarf=loc"
    "--dwarf=Ranges"
    "--dwarf=pubtypes"
    "--dwarf=gdb_index"
    "--dwarf=trace_info"
    "--dwarf=trace_abbrev"
    "--dwarf=trace_aranges"
    "--dwarf=addr"
    "--dwarf=cu_index"
    "--dwarf=links"
    "--dwarf=follow-links"

    "-t"
    "--syms"
    "-T"
    "--dynamic-syms"
    "-r"
    "--reloc"
    "-R"
    "--dynamic-reloc"

    (("-M" "--disassembler-options") '("intel" "att" "att-mnemonic" "intel-mnemonic" "x86-64" "i386"
                                       "i8086" "addr64" "addr32" "addr16" "data32" "data16" "suffix"
                                       "amd64" "intel64"))
    "-l"
    "--line-numbers"
    "-F"
    "--file-offsets"
    (("-C" "--demangle") '("auto" "gnu" "lucid" "arm" "hp" "edg" "gnu-v3" "java" "gnat"))

    "-w"
    "--wide"
    "--special-syms"
    "--inlines")
   (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/readelf "shell-completion" nil t)
(defpcmpl pcomplete/ldd
  (opts
   (flags "-d"
          "--data-relocs"
          "-r"
          "--function-relocs"
          "-u"
          "--unused"
          "-v"
          "--verbose")
   (args (pcmpl-entries))))


(defun pcmpl-gcc-assembler-flags ()
  '())

(defun pcmpl-gcc-preprocessor-flags ()
  '())

(defun pcmpl-gcc-linker-flags ()
  '())

;;;###autoload (autoload 'pcomplete/gcc "shell-completion" nil t)
(defpcmpl pcomplete/gcc
  (opts
   (flags "-pass-exit-codes"
          "--help"
          "--help=common"
          "--help=optimizers"
          "--help=params"
          "--help=target"
          "--help=warnings"
          "--help=joined"
          "--help=separate"
          "--help=undocumented"
          "--help=^joined"
          "--help=^separate"
          "--help=^undocumented"
          "--target-help"
          "--help"
          "--version"
          "-dumpspecs"
          "-dumpversion"
          "-dumpmachine"
          "-print-search-dirs"
          "-print-libgcc-file-name"
          "-print-file-name=<lib>"
          "-print-prog-name=<prog>"
          "-print-multiarch"
          "-print-multi-directory"
          "-print-multi-lib"
          "-print-multi-os-director"
          "-print-sysroot"
          "-print-sysroot-headers-s"
          "-Wa,"
          "-Wp,"
          "-Wl,"
          ("-Xassembler" (pcmpl-gcc-assembler-flags))
          ("-Xpreprocessor" (pcmpl-gcc-preprocessor-flags))
          ("-Xlinker" (pcmpl-gcc-linker-flags))
          "-save-temps"
          "-save-temps"
          "-no-canonical-prefixes"
          "-pipe"
          "-time"
          ("-specs" (pcmpl-entries-ignoring-common))
          "-std"
          "-std=c++11"
          "-std=c99"
          "-std=c89"
          ("--sysroot" (pcmpl-dirs))
          ("-B" (pcmpl-dirs))
          "-v"
          "-###"
          "-E"
          "-S"
          "-c"
          ("-o" (pcmpl-entries))
          "-pie"
          "-shared"
          ("-x" '("c" "c++" "assembler" "none"))

          "-static"

          (("-I" "-L") (pcmpl-dirs))
          "-Os"
          "-O2"
          "-O3"
          "-marh"
          "-march=native"
          "-fomit-frame-pontier")
   (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/clang "shell-completion" nil t)
(defpcmpl pcomplete/clang
  (opts
   (flags
    (("-B" "--prefix" "-I" "--include-directory" "-F" "--sysroot" "-working-directory" "-fprofile-dir" "-L" "--library-directory") (pcmpl-dirs))
    "-I-"

    "--analyze"
    "--analyzer-no-default-checks"
    "--analyzer-output=html"
    "--analyzer-output=plist"
    "--analyzer-output=plist-multi-file"
    "--analyzer-output=plist-html"
    "--analyzer-output=text"
    "-ansi"
    "--ansi"

    "-help"
    "--help"
    "--help-hidden"

    "-nobuiltininc"
    "-nocudainc"
    "-nodefaultlibs"
    "-nofixprebinding"
    "-nogpulib"
    "-nocudalib"
    "-nolibc"
    "-nomultidefs"
    "-nopie"
    "-no-pie"
    "-noprebind"
    "-noprofilelib"
    "-noseglinkedit"
    "-nostartfiles"
    "-nostdinc"
    "--no-standard-includes"
    "-nostdinc++"
    "-nostdlib"
    "--no-standard-libraries"
    "-nostdlib++"
    "-nostdlibinc"


    "-p"
    "--profile"
    "-pagezero_size<arg>"
    "-pg"
    "-pie"
    "-pipe"
    "--pipe"
    "-prebind"
    "-prebind_all_twolevel_modules"
    "-preload"
    "--print-diagnostic-categories"
    "-print-effective-triple"
    "--print-effective-triple"

    "-print-ivar-layout"
    "-print-libgcc-file-name"
    "--print-libgcc-file-name"
    "-print-multi-directory"
    "--print-multi-directory"
    "-print-multi-lib"
    "--print-multi-lib"
    "-print-resource-dir"
    "--print-resource-dir"
    "-print-search-dirs"
    "--print-search-dirs"
    "-print-target-triple"
    "--print-target-triple"

    "-private_bundle"
    "-pthread"
    "-no-pthread"
    "-pthreads"
    "-rdynamic"
    "-read_only_relocs <arg>"
    "-relocatable-pch"
    "--relocatable-pch"
    "-verify-pch"

    "-shared"
    "--shared"
    "-shared-libgcc"
    "-shared-libsan"
    "-shared-libasan"
    "-single_module"

    "-static"
    "--static"
    "-static-libgcc"
    "-static-libsan"
    "-static-libstdc++"
    "-static-openmp"
    "-static-pie"

    "--target-help"

    "-time"
    "-traditional"
    "--traditional"
    "-traditional-cpp"
    "--traditional-cpp"
    "-twolevel_namespace"
    "-twolevel_namespace_hints"

    "-v"
    "--verbose"
    "--verify-debug-info"
    "--version"
    "-w"
    "--no-warnings"

    "-whatsloaded"
    "-whyload"

    "-E"
    "--preprocess"
    "-S"
    "--assemble"
    "-c"
    "--compile"
    "-emit-interface-stubs"
    "-emit-llvm"
    "-fsyntax-only"
    "-module-file-info"
    "--precompile"

    "-fcrash-diagnostics-dir"

    "-fcomplete-member-pointers"
    "-fno-complete-member-pointers"
    "-fdeclspec"
    "-fno-declspec"
    "-fdepfile-entry="
    "-fdiagnostics-fixit-info"
    "-fno-diagnostics-fixit-info"
    "-fdiagnostics-format="
    "-fdiagnostics-parseable-fixits"
    "-fdiagnostics-print-source-range-info"
    "-fdiagnostics-show-category="
    "-fdiscard-value-names"
    "-fno-discard-value-names"
    "-fexperimental-isel"
    "-fno-experimental-isel"
    "-fexperimental-new-pass-manager"
    "-fno-experimental-new-pass-manager"
    "-ffine-grained-bitfield-accesses"
    "-fno-fine-grained-bitfield-accesses"
    "-finline-functions"
    "-fno-inline-functions"
    "-finline-hint-functions"
    "-fno-crash-diagnostics"
    "-fno-sanitize-blacklist"
    "-fparse-all-comments"
    "-frecord-command-line"
    "-fno-record-command-line"
    "-frecord-gcc-switches"
    "-fsanitize-address-field-padding="
    "-fsanitize-address-globals-dead-stripping"
    "-fsanitize-address-poison-custom-array-cookie"
    "-fno-sanitize-address-poison-custom-array-cookie"
    "-fsanitize-address-use-after-scope"
    "-fno-sanitize-address-use-after-scope"
    "-fsanitize-address-use-odr-indicator"
    "-fno-sanitize-address-use-odr-indicator"
    "-fsanitize-blacklist="
    "-fsanitize-cfi-canonical-jump-tables"
    "-fno-sanitize-cfi-canonical-jump-tables"
    "-fsanitize-cfi-cross-dso"
    "-fno-sanitize-cfi-cross-dso"
    "-fsanitize-cfi-icall-generalize-pointers"
    "-fsanitize-coverage="
    "-fno-sanitize-coverage="
    "-fsanitize-hwaddress-abi="
    "-fsanitize-link-c++-runtime"
    "-fno-sanitize-link-c++-runtime"
    "-fsanitize-link-runtime"
    "-fno-sanitize-link-runtime"
    "-fsanitize-memory-track-origins"
    "-fno-sanitize-memory-track-origins"
    "-fsanitize-memory-track-origins="
    "-fsanitize-memory-use-after-dtor"
    "-fno-sanitize-memory-use-after-dtor"
    "-fsanitize-minimal-runtime"
    "-fno-sanitize-minimal-runtime"
    "-fsanitize-recover"
    "-fno-sanitize-recover"
    "-fsanitize-recover="
    "-fno-sanitize-recover="
    "-fsanitize-stats"
    "-fno-sanitize-stats"
    "-fsanitize-thread-atomics"
    "-fno-sanitize-thread-atomics"
    "-fsanitize-thread-func-entry-exit"
    "-fno-sanitize-thread-func-entry-exit"
    "-fsanitize-thread-memory-access"
    "-fno-sanitize-thread-memory-access"
    "-fsanitize-trap="
    "-fno-sanitize-trap="
    "-fsanitize-undefined-strip-path-components="
    "-fsanitize-undefined-trap-on-error"
    "-fno-sanitize-undefined-trap-on-error"
    "-fsanitize="
    "-fno-sanitize="

    "-mcpu="
    "-mtune="

    (("-std" "--std") '("c++98" "c++03" "gnu++98" "gnu++03" "c++11" "gnu++11" "c++14" "gnu++14" "c++17" "gnu++17" "c++2a" "gnu++2a"))

    "-D"
    "-U"
    "-C"
    "--comments"
    "-CC"
    "--comments-in-macros"
    "--define-macro"
    "--undefine-macro"

    "-print-supported-cpus"
    "--print-supported-cpus"
    "-H"
    "--trace-includes"
    "-P"
    "--no-line-commands"

    (("-include" "--include") (pcmpl-entries-ignoring-common))

    "-M"
    "--dependencies"
    "-MD"
    "--write-dependencies"

    "--extra-warnings"

    "-fconstant-cfstrings"
    "-fno-constant-cfstrings"
    "-fconstant-string-class="
    "-fconstexpr-backtrace-limit="
    "-fconstexpr-depth="
    "-fconstexpr-steps="
    "-fcoroutines-ts"
    "-fno-coroutines-ts"
    "-fcoverage-mapping"
    "-fno-coverage-mapping"
    "-fcreate-profile"
    "-fcs-profile-generate"
    "-fcs-profile-generate=<directory>"
    "-fcxx-exceptions"
    "-fno-cxx-exceptions"
    "-fcxx-modules"
    "-fno-cxx-modules"
    "-fdata-sections"
    "-fno-data-sections"
    "-fdebug-info-for-profiling"
    "-fno-debug-info-for-profiling"
    "-fdebug-macro"
    "-fno-debug-macro"
    "-fdebug-pass-arguments"
    "-fdebug-pass-structure"
    "-fdebug-prefix-map=<arg>"
    "-fdebug-ranges-base-address"
    "-fno-debug-ranges-base-address"
    "-fdebug-types-section"
    "-fno-debug-types-section"
    "-fdelayed-template-parsing"
    "-fno-delayed-template-parsing"
    "-fdelete-null-pointer-checks"
    "-fno-delete-null-pointer-checks"
    "-fdenormal-fp-math="
    "-fdiagnostics-absolute-paths"
    "-fdiagnostics-color"
    "-fno-diagnostics-color"
    "-fdiagnostics-color=<arg>"
    "-fdiagnostics-hotness-threshold="
    "-fdiagnostics-show-hotness"
    "-fno-diagnostics-show-hotness"
    "-fdiagnostics-show-note-include-stack"
    "-fno-diagnostics-show-note-include-stack"
    "-fdiagnostics-show-option"
    "-fno-diagnostics-show-option"
    "-fdiagnostics-show-template-tree"
    "-fdigraphs"
    "-fno-digraphs"
    "-fdollars-in-identifiers"
    "-fno-dollars-in-identifiers"
    "-fdouble-square-bracket-attributes"
    "-fno-double-square-bracket-attributes"
    "-fdwarf-directory-asm"
    "-fno-dwarf-directory-asm"
    "-fdwarf-exceptions"
    "-felide-constructors"
    "-fno-elide-constructors"
    "-feliminate-unused-debug-symbols"
    "-fno-eliminate-unused-debug-symbols"
    "-femit-all-decls"
    "-femulated-tls"
    "-fexceptions"
    "-fno-exceptions"
    "-ffor-scope"
    "-fno-for-scope"
    "-fforce-emit-vtables"
    "-fno-force-emit-vtables"
    "-fforce-enable-int128"
    "-fno-force-enable-int128"
    "-ffast-math"
    "-fno-fast-math"
    "-ffinite-math-only"
    "-fno-finite-math-only"
    "-ffixed-point"
    "-fno-fixed-point"
    "-ffreestanding"
    "-ffunction-sections"
    "-fno-function-sections"
    "-fgnu-inline-asm"
    "-fno-gnu-inline-asm"
    "-fgnu-keywords"
    "-fno-gnu-keywords"
    "-fgnu-runtime"
    "-fgnu89-inline"
    "-fno-gnu89-inline"
    "-fhonor-infinities"
    "-fhonor-infinites"
    "-fno-honor-infinities"
    "-fhonor-nans"
    "-fno-honor-nans"
    "-fhosted"
    "-flto"
    "-fno-lto"
    "-flto-jobs="
    "-flto="
    "-fmath-errno"
    "-fno-math-errno"
    "-fmerge-all-constants"
    "-fno-merge-all-constants"
    "-fmodules"
    "-fno-modules"
    "-fmodules-decluse"
    "-fno-modules-decluse"
    "-fjump-tables"
    "-fno-jump-tables"
    "-fno-elide-type"
    "-fno-max-type-align"
    "-fno-operator-names"
    "-fno-rtti-data"
    "-fno-strict-modules-decluse"
    "-fno-working-directory"
    "-fomit-frame-pointer"
    "-fno-omit-frame-pointer"
    "-fopenmp"
    "-fno-openmp"
    "-fopenmp-simd"
    "-fno-openmp-simd"
    "-fopenmp-version="
    "-fopenmp="
    "-foptimize-sibling-calls"
    "-fno-optimize-sibling-calls"
    "-fpack-struct"
    "-fno-pack-struct"
    "-fpack-struct="
    "-fpascal-strings"
    "-fno-pascal-strings"
    "-mpascal-strings"
    "-fpcc-struct-return"
    "-fpch-preprocess"
    "-fpic"
    "-fno-pic"
    "-fpie"
    "-fno-pie"
    "-fplt"
    "-fno-plt"
    "-fpreserve-as-comments"
    "-fno-preserve-as-comments"
    "-fprofile-arcs"
    "-fno-profile-arcs"
    "-fprofile-generate"
    "-fno-profile-generate"
    "-fprofile-instr-generate"
    "-fno-profile-instr-generate"
    "-fprofile-instr-use"
    "-fno-profile-instr-use"
    "-fprofile-use"
    "-fprofile-sample-accurate"
    "-fauto-profile-accurate"
    "-fno-profile-sample-accurate"
    "-fprofile-sample-use"
    "-fauto-profile"
    "-fno-profile-sample-use"
    "-freciprocal-math"
    "-fno-reciprocal-math"
    "-freg-struct-return"
    "-fregister-global-dtors-with-atexit"
    "-fno-register-global-dtors-with-atexit"
    "-frelaxed-template-template-args"
    "-fno-relaxed-template-template-args"
    "-freroll-loops"
    "-fno-reroll-loops"
    "-fretain-comments-from-system-headers"
    "-frewrite-imports"
    "-fno-rewrite-imports"
    "-frewrite-includes"
    "-fno-rewrite-includes"
    "-fropi"
    "-fno-ropi"
    "-frtti"
    "-fno-rtti"
    "-frwpi"
    "-fno-rwpi"
    "-fsave-optimization-record"
    "-fno-save-optimization-record"
    "-fseh-exceptions"
    "-fshort-enums"
    "-fno-short-enums"
    "-fshort-wchar"
    "-fno-short-wchar"
    "-fshow-column"
    "-fno-show-column"
    "-fshow-source-location"
    "-fno-show-source-location"
    "-fsignaling-math"
    "-fno-signaling-math"
    "-fsigned-bitfields"
    "-fsigned-char"
    "-fno-signed-char"
    "--signed-char"
    "-fsigned-zeros"
    "-fno-signed-zeros"
    "-fsized-deallocation"
    "-fno-sized-deallocation"
    "-fsjlj-exceptions"
    "-fslp-vectorize"
    "-fno-slp-vectorize"
    "-ftree-slp-vectorize"
    "-fspell-checking"
    "-fno-spell-checking"
    "-fsplit-dwarf-inlining"
    "-fno-split-dwarf-inlining"
    "-fsplit-lto-unit"
    "-fno-split-lto-unit"
    "-fsplit-stack"
    "-fstack-protector"
    "-fno-stack-protector"
    "-fstack-protector-all"
    "-fstack-protector-strong"
    "-fstack-size-section"
    "-fno-stack-size-section"
    "-fstandalone-debug"
    "-fno-limit-debug-info"
    "-fno-standalone-debug"
    "-fstrict-aliasing"
    "-fno-strict-aliasing"
    "-fstrict-enums"
    "-fno-strict-enums"
    "-fstrict-float-cast-overflow"
    "-fno-strict-float-cast-overflow"
    "-fstrict-overflow"
    "-fno-strict-overflow"
    "-fstrict-return"
    "-fno-strict-return"
    "-fstrict-vtable-pointers"
    "-fno-strict-vtable-pointers"
    "-fstruct-path-tbaa"
    "-fno-struct-path-tbaa"
    "-ftest-coverage"
    "-fthreadsafe-statics"
    "-fno-threadsafe-statics"
    "-ftime-report"
    "-ftime-trace"
    "-ftrapping-math"
    "-fno-trapping-math"
    "-ftrapv"
    "-ftrigraphs"
    "-fno-trigraphs"
    "-trigraphs"
    "--trigraphs"
    "-funique-section-names"
    "-fno-unique-section-names"
    "-funit-at-a-time"
    "-fno-unit-at-a-time"
    "-funroll-loops"
    "-fno-unroll-loops"
    "-funsafe-math-optimizations"
    "-fno-unsafe-math-optimizations"
    "-funsigned-bitfields"
    "-funsigned-char"
    "-fno-unsigned-char"
    "--unsigned-char"
    "-funwind-tables"
    "-fno-unwind-tables"
    "-fuse-cxa-atexit"
    "-fno-use-cxa-atexit"
    "-fuse-init-array"
    "-fno-use-init-array"
    "-fvectorize"
    "-fno-vectorize"
    "-ftree-vectorize"
    "-fverbose-asm"
    "-dA"
    "-fno-verbose-asm"
    "-fvisibility-global-new-delete-hidden"
    "-fvisibility-inlines-hidden"
    "-fuse-line-directives"
    "-fno-use-line-directives"
    "-fwrapv"
    "-fno-wrapv"
    "-fwritable-strings"
    "-fxray-always-emit-customevents"
    "-fno-xray-always-emit-customevents"
    "-fxray-always-emit-typedevents"
    "-fno-xray-always-emit-typedevents"
    "-fxray-instrument"
    "-fno-xray-instrument"
    "-fxray-link-deps"
    "-fzvector"
    "-fno-zvector"
    "-mzvector"
    "-pedantic"
    "--pedantic"
    "-no-pedantic"
    "--no-pedantic"
    "-pedantic-errors"
    "--pedantic-errors"
    "-cl-denorms-are-zero"
    "-cl-fast-relaxed-math"
    "-cl-finite-math-only"
    "-cl-fp32-correctly-rounded-divide-sqrt"
    "-cl-kernel-arg-info"
    "-cl-mad-enable"
    "-cl-no-signed-zeros"
    "-cl-opt-disable"
    "-cl-single-precision-constant"
    "-cl-std=<arg>"
    "-cl-strict-aliasing"
    "-cl-uniform-work-group-size"
    "-cl-unsafe-math-optimizations"
    "-m16"
    "-m32"
    "-m64"
    "-mabi="
    "-march="
    "-masm="
    "-mcpu="
    "-malign-double"
    "-mbackchain"
    "-mno-backchain"
    "-mcrc"
    "-mno-crc"
    "-mfloat-abi="
    "-mfpmath="
    "-mfpu="
    "-mglobal-merge"
    "-mno-global-merge"
    "-mhard-float"
    "-mkernel"
    "-mlong-calls"
    "-mno-long-calls"
    "-mstackrealign"
    "-mno-stackrealign"
    "-mthumb"
    "-mno-thumb"
    "-mvx"
    "-mno-vx"
    "-mx32"
    "-fcall-saved-x10"
    "-fcall-saved-x11"
    "-fcall-saved-x12"
    "-fcall-saved-x13"
    "-fcall-saved-x14"
    "-fcall-saved-x15"
    "-fcall-saved-x18"
    "-fcall-saved-x8"
    "-fcall-saved-x9"
    "-ffixed-x1"
    "-ffixed-x10"
    "-ffixed-x11"
    "-ffixed-x12"
    "-ffixed-x13"
    "-ffixed-x14"
    "-ffixed-x15"
    "-ffixed-x18"
    "-ffixed-x2"
    "-ffixed-x20"
    "-ffixed-x21"
    "-ffixed-x22"
    "-ffixed-x23"
    "-ffixed-x24"
    "-ffixed-x25"
    "-ffixed-x26"
    "-ffixed-x27"
    "-ffixed-x28"
    "-ffixed-x3"
    "-ffixed-x4"
    "-ffixed-x5"
    "-ffixed-x6"
    "-ffixed-x7"
    "-ffixed-x9"
    "-mfix-cortex-a53-835769"
    "-mno-fix-cortex-a53-835769"
    "-mgeneral-regs-only"
    "-mcode-object-v3"
    "-mno-code-object-v3"
    "-mcumode"
    "-mno-cumode"
    "-msram-ecc"
    "-mno-sram-ecc"
    "-mxnack"
    "-mno-xnack"
    "-ffixed-r9"
    "-mcmse"
    "-mexecute-only"
    "-mno-execute-only"
    "-mpure-code"
    "-mno-movt"
    "-mno-neg-immediates"
    "-mnocrc"
    "-mrestrict-it"
    "-mno-restrict-it"
    "-mtp="
    "-munaligned-access"
    "-mno-unaligned-access"
    "-mieee-rnd-near"
    "-mmemops"
    "-mno-memops"
    "-mnvj"
    "-mno-nvj"
    "-mnvs"
    "-mno-nvs"
    "-mpackets"
    "-mno-packets"
    "-mhvx"
    "-mno-hvx"
    "-mhvx-length="
    "-mhvx="
    "-mabicalls"
    "-mno-abicalls"
    "-mabs="
    "-mcheck-zero-division"
    "-mno-check-zero-division"
    "-mcompact-branches="
    "-mdouble-float"
    "-mdsp"
    "-mno-dsp"
    "-mdspr2"
    "-mno-dspr2"
    "-membedded-data"
    "-mno-embedded-data"
    "-mextern-sdata"
    "-mno-extern-sdata"
    "-mfp32"
    "-mfp64"
    "-mginv"
    "-mno-ginv"
    "-mgpopt"
    "-mno-gpopt"
    "-mindirect-jump=<arg>"
    "-mips16"
    "-mldc1-sdc1"
    "-mno-ldc1-sdc1"
    "-mlocal-sdata"
    "-mno-local-sdata"
    "-mmadd4"
    "-mno-madd4"
    "-mmicromips"
    "-mno-micromips"
    "-mmsa"
    "-mno-msa"
    "-mmt"
    "-mno-mt"
    "-mnan=<arg>"
    "-mno-mips16"
    "-msingle-float"
    "-mvirt"
    "-mno-virt"
    "-mxgot"
    "-mno-xgot"
    "-maltivec"
    "-mno-altivec"
    "-mcmpb"
    "-mno-cmpb"
    "-mcrbits"
    "-mno-crbits"
    "-mcrypto"
    "-mno-crypto"
    "-mdirect-move"
    "-mno-direct-move"
    "-mfloat128"
    "-mno-float128"
    "-mfprnd"
    "-mno-fprnd"
    "-mhtm"
    "-mno-htm"
    "-minvariant-function-descriptors"
    "-mno-invariant-function-descriptors"
    "-misel"
    "-mno-isel"
    "-mlongcall"
    "-mno-longcall"
    "-mmfocrf"
    "-mmfcrf"
    "-mno-mfocrf"
    "-mpopcntd"
    "-mno-popcntd"
    "-mpower8-vector"
    "-mno-power8-vector"
    "-mpower9-vector"
    "-mno-power9-vector"
    "-mqpx"
    "-mno-qpx"
    "-msecure-plt"
    "-mspe"
    "-mno-spe"
    "-mvsx"
    "-mno-vsx"
    "-matomics"
    "-mno-atomics"
    "-mbulk-memory"
    "-mno-bulk-memory"
    "-mexception-handling"
    "-mno-exception-handling"
    "-mmultivalue"
    "-mno-multivalue"
    "-mmutable-globals"
    "-mno-mutable-globals"
    "-mnontrapping-fptoint"
    "-mno-nontrapping-fptoint"
    "-msign-ext"
    "-mno-sign-ext"
    "-msimd128"
    "-mno-simd128"
    "-mtail-call"
    "-mno-tail-call"
    "-munimplemented-simd128"
    "-mno-unimplemented-simd128"
    "-m3dnow"
    "-mno-3dnow"
    "-m3dnowa"
    "-mno-3dnowa"
    "-madx"
    "-mno-adx"
    "-maes"
    "-mno-aes"
    "-mavx"
    "-mno-avx"
    "-mavx2"
    "-mno-avx2"
    "-mavx512bf16"
    "-mno-avx512bf16"
    "-mavx512bitalg"
    "-mno-avx512bitalg"
    "-mavx512bw"
    "-mno-avx512bw"
    "-mavx512cd"
    "-mno-avx512cd"
    "-mavx512dq"
    "-mno-avx512dq"
    "-mavx512er"
    "-mno-avx512er"
    "-mavx512f"
    "-mno-avx512f"
    "-mavx512ifma"
    "-mno-avx512ifma"
    "-mavx512pf"
    "-mno-avx512pf"
    "-mavx512vbmi"
    "-mno-avx512vbmi"
    "-mavx512vbmi2"
    "-mno-avx512vbmi2"
    "-mavx512vl"
    "-mno-avx512vl"
    "-mavx512vnni"
    "-mno-avx512vnni"
    "-mavx512vp2intersect"
    "-mno-avx512vp2intersect"
    "-mavx512vpopcntdq"
    "-mno-avx512vpopcntdq"
    "-mbmi"
    "-mno-bmi"
    "-mbmi2"
    "-mno-bmi2"
    "-mcldemote"
    "-mno-cldemote"
    "-mclflushopt"
    "-mno-clflushopt"
    "-mclwb"
    "-mno-clwb"
    "-mclzero"
    "-mno-clzero"
    "-mcx16"
    "-mno-cx16"
    "-menqcmd"
    "-mno-enqcmd"
    "-mf16c"
    "-mno-f16c"
    "-mfma"
    "-mno-fma"
    "-mfma4"
    "-mno-fma4"
    "-mfsgsbase"
    "-mno-fsgsbase"
    "-mfxsr"
    "-mno-fxsr"
    "-mgfni"
    "-mno-gfni"
    "-minvpcid"
    "-mno-invpcid"
    "-mlwp"
    "-mno-lwp"
    "-mlzcnt"
    "-mno-lzcnt"
    "-mmmx"
    "-mno-mmx"
    "-mmovbe"
    "-mno-movbe"
    "-mmovdir64b"
    "-mno-movdir64b"
    "-mmovdiri"
    "-mno-movdiri"
    "-mmwaitx"
    "-mno-mwaitx"
    "-mpclmul"
    "-mno-pclmul"
    "-mpconfig"
    "-mno-pconfig"
    "-mpku"
    "-mno-pku"
    "-mpopcnt"
    "-mno-popcnt"
    "-mprefetchwt1"
    "-mno-prefetchwt1"
    "-mprfchw"
    "-mno-prfchw"
    "-mptwrite"
    "-mno-ptwrite"
    "-mrdpid"
    "-mno-rdpid"
    "-mrdrnd"
    "-mno-rdrnd"
    "-mrdseed"
    "-mno-rdseed"
    "-mretpoline-external-thunk"
    "-mno-retpoline-external-thunk"
    "-mrtm"
    "-mno-rtm"
    "-msahf"
    "-mno-sahf"
    "-msgx"
    "-mno-sgx"
    "-msha"
    "-mno-sha"
    "-mshstk"
    "-mno-shstk"
    "-msse"
    "-mno-sse"
    "-msse2"
    "-mno-sse2"
    "-msse3"
    "-mno-sse3"
    "-msse4.1"
    "-mno-sse4.1"
    "-msse4.2"
    "-mno-sse4.2"
    "-msse4"
    "-msse4a"
    "-mno-sse4a"
    "-mssse3"
    "-mno-ssse3"
    "-mtbm"
    "-mno-tbm"
    "-mvaes"
    "-mno-vaes"
    "-mvpclmulqdq"
    "-mno-vpclmulqdq"
    "-mwaitpkg"
    "-mno-waitpkg"
    "-mwbnoinvd"
    "-mno-wbnoinvd"
    "-mx87"
    "-m80387"
    "-mno-x87"
    "-mxop"
    "-mno-xop"
    "-mxsave"
    "-mno-xsave"
    "-mxsavec"
    "-mno-xsavec"
    "-mxsaveopt"
    "-mno-xsaveopt"
    "-mxsaves"
    "-mno-xsaves"
    "-mrelax"
    "-mno-relax"
    "-msave-restore"
    "-mno-save-restore"
    "-mlong-double-128"
    "-mlong-double-64"
    "-mlong-double-80"

    "-O"
    "-O0"
    "-O1"
    "-O2"
    "-O3"
    "--optimize"
    "-Ofast"
    "-g"
    "--debug"
    "-gdwarf-2"
    "-gdwarf-3"
    "-gdwarf-4"
    "-gdwarf"
    "-gdwarf-5"
    "-gfull"
    "-gused"
    "-g0"
    "-g2"
    "-g3"
    "-ggdb0"
    "-ggdb1"
    "-ggdb2"
    "-ggdb3"
    "-gline-directives-only"
    "-gline-tables-only"
    "-g1"
    "-gmlt"
    "-gmodules"
    "-ggdb"
    "-glldb"
    "-gsce"
    "-gcolumn-info"
    "-gno-column-info"
    "-gdwarf-aranges"
    "-gembed-source"
    "-gno-embed-source"
    "-ggnu-pubnames"
    "-gno-gnu-pubnames"
    "-gpubnames"
    "-gno-pubnames"
    "-grecord-command-line"
    "-gno-record-command-line"
    "-grecord-gcc-switches"
    "-gsplit-dwarf"

    "-gstrict-dwarf"
    "-gno-strict-dwarf"
    "-gz"

    "-cpp"
    "-nocpp"
    "-faggressive-function-elimination"
    "-fno-aggressive-function-elimination"
    "-falign-commons"
    "-fno-align-commons"
    "-fall-intrinsics"
    "-fno-all-intrinsics"
    "-fautomatic"
    "-fno-automatic"
    "-fbackslash"
    "-fno-backslash"
    "-fbacktrace"
    "-fno-backtrace"
    "-fblas-matmul-limit=<arg>"
    "-fbounds-check"
    "-fno-bounds-check"
    "-fcheck-array-temporaries"
    "-fno-check-array-temporaries"
    "-fcheck=<arg>"
    "-fcoarray=<arg>"
    "-fconvert=<arg>"
    "-fcray-pointer"
    "-fno-cray-pointer"
    "-fd-lines-as-code"
    "-fno-d-lines-as-code"
    "-fd-lines-as-comments"
    "-fno-d-lines-as-comments"
    "-fdefault-double-8"
    "-fno-default-double-8"
    "-fdefault-integer-8"
    "-fno-default-integer-8"
    "-fdefault-real-8"
    "-fno-default-real-8"
    "-fdollar-ok"
    "-fno-dollar-ok"
    "-fdump-fortran-optimized"
    "-fno-dump-fortran-optimized"
    "-fdump-fortran-original"
    "-fno-dump-fortran-original"
    "-fdump-parse-tree"
    "-fno-dump-parse-tree"
    "-fexternal-blas"
    "-fno-external-blas"
    "-ff2c"
    "-fno-f2c"
    "-ffixed-form"
    "-fno-fixed-form"
    "-ffree-form"
    "-fno-free-form"
    "-ffrontend-optimize"
    "-fno-frontend-optimize"
    "-fimplicit-none"
    "-fno-implicit-none"
    "-finit-local-zero"
    "-fno-init-local-zero"
    "-finteger-4-integer-8"
    "-fno-integer-4-integer-8"
    "-fintrinsic-modules-path"
    "-fno-intrinsic-modules-path"
    "-fmax-errors="
    "-fmax-identifier-length"
    "-fno-max-identifier-length"
    "-fmax-stack-var-size="
    "-fmax-subrecord-length="
    "-fmodule-private"
    "-fno-module-private"
    "-fpack-derived"
    "-fno-pack-derived"
    "-fprotect-parens"
    "-fno-protect-parens"
    "-frange-check"
    "-fno-range-check"
    "-freal-4-real-10"
    "-fno-real-4-real-10"
    "-freal-4-real-16"
    "-fno-real-4-real-16"
    "-freal-4-real-8"
    "-fno-real-4-real-8"
    "-freal-8-real-10"
    "-fno-real-8-real-10"
    "-freal-8-real-16"
    "-fno-real-8-real-16"
    "-freal-8-real-4"
    "-fno-real-8-real-4"
    "-frealloc-lhs"
    "-fno-realloc-lhs"

    "-frecursive"
    "-fno-recursive"
    "-frepack-arrays"
    "-fno-repack-arrays"
    "-fsecond-underscore"
    "-fno-second-underscore"
    "-fsign-zero"
    "-fno-sign-zero"
    "-fstack-arrays"
    "-fno-stack-arrays"
    "-funderscoring"
    "-fno-underscoring"
    "-fwhole-file"
    "-fno-whole-file"

    "-X"
    "-T"
    "-Tbss"
    "-Tdata"
    "-Ttext"
    "-Wl,"
    "-X"
    "-Xlinker"
    "--for-linker"
    "-Z"
    "-l"
    "-u"
    "-rpath"
    "-undef"
    "-undefined"
    "--no-undefined"
    "-z")
   (args
    (pcmpl-entries))))

;;;; Rust

;; (defun pcmpl-cargo-metadata ()
;;   "Return a list of cargo targets."
;;   (save-match-data
;;     (with-temp-buffer
;;       (call-process "cargo"
;;                     nil
;;                     (current-buffer)
;;                     nil
;;                     "metadata"
;;                     "--no-deps"
;;                     "--format-version=1")
;;       (goto-char (point-min))
;;       (json-read-object))))

(defun pcmpl-rust-package-names ()
  (vector->list (rust-metadata-package-names (rust-metadata-get-full-metadata))))

(defun pcmpl-rust-binary-targets ()
  (rust-metadata-binary-targets (rust-metadata-get-full-metadata)))

(defun pcmpl-rust-example-targets ()
  (rust-metadata-example-targets (rust-metadata-get-full-metadata)))

(defun pcmpl-rust-test-targets ()
  (rust-metadata-test-targets (rust-metadata-get-full-metadata)))

(defun pcmpl-rust-bench-targets ()
  (rust-metadata-bench-targets (rust-metadata-get-full-metadata)))

;;;###autoload (autoload 'pcomplete/cargo "shell-completion" nil t)
(defpcmpl pcomplete/cargo
  (let ((help-flags
         '("-h"
           "--help"))
        (common-flags
         '("-V"
           "--version"
           "--list"
           "--explain"
           "-v"
           "--verbose"
           "-q"
           "--quiet"
           "--color=auto"
           "--color=always"
           "--color=never"
           "--frozen"
           "--locked"
           "--offline"
           "-Z"))
        (target-flags
         '("-p"
           ("--package" (pcmpl-rust-package-names))
           ("--target" (rust-target-triples))
           ("--target-dir" (pcmpl-dirs))
           "--release"
           ("--profile" '("dev" "release" "test" "bench"))))
        (build-flags
         '("--workspace"
           "--exclude"
           "-j"
           "--jobs"
           "--lib"
           ("--bin" (pcmpl-rust-binary-targets))
           "--bins"
           ("--example" (pcmpl-rust-example-targets))
           "--examples"
           ("--test" (pcmpl-rust-test-targets))
           "--tests"
           ("--bench" (pcmpl-rust-bench-targets))
           "--benches"
           "--all-targets"
           "--features"
           "--all-features"
           "--no-default-features"
           ("--out-dir" (pcmpl-dirs))
           ("--manifest-path" (pcmpl-entries :select ".*\\.toml\\'"))
           ("--message-format" '("human" "short" "json" "json-diagnostic-short" "json-diagnostic-rendered-ansi" "json-render-diagnostics"))
           "--build-plan")))
    `(or (("build" "check" "doc")
          (opts
           (flags ,@common-flags
                  ,@help-flags
                  ,@target-flags
                  ,@build-flags)))
         ("clean"
          (opts
           (flags ,@common-flags
                  ,@help-flags
                  ,@target-flags
                  "--doc")))
         ("new"
          (opts
           (flags ,@common-flags
                  ,@help-flags
                  "--bin"
                  "--lib"
                  "--edition=2015"
                  "--edition=2018"
                  "--name"
                  "--vcs=git"
                  "--vcs=hg"
                  "--vcs=pijul"
                  "--vcs=fossil")))

         ("run"
          (opts
           (flags ,@target-flags
                  ("--bin" (pcmpl-rust-binary-targets))
                  ("--example" (pcmpl-rust-example-targets))
                  "--features"
                  "--all-features"
                  "--no-default-features")))
         (("build" "check" "doc")
          (opts
           (flags ,@common-flags
                  ,@help-flags
                  ,@target-flags
                  ,@build-flags)))
         (("init"
           "test"
           "bench"
           "update"
           "search"
           "publish"
           "install"
           "uninstall")
          (opts
           (flags ,@help-flags)))
         (opts
          (flags ,@common-flags ,@help-flags)
          (args (pcmpl-entries)))))
  :evaluate-definition t)

;;;; Vanilla Unix & GNU tools

;;;###autoload (autoload 'pcomplete/cp "shell-completion" nil t)
(defpcmpl pcomplete/cp
  (opts (flags "-r"
               "--recursive"
               "-f"
               "--force"
               "-l"
               "--link"
               "-P"
               "--no-dereference"
               "-n"
               "--no-clobber"
               "-s"
               "--symbolic-link"
               "-v"
               "--verbose")
        (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/ls "shell-completion" nil t)
(defpcmpl pcomplete/ls
  (opts (flags "-a"
               "--all"
               "-A"
               "--almost-all"
               "--author"
               "-b"
               "--escape"
               "--block-size"
               "-B"
               "--ignore-backups"
               "-c"
               "-C"
               "--color"
               "-d"
               "--directory"
               "-D"
               "--dired"
               "-f"
               "-F"
               "--classify"
               "--file-type"
               "--format"
               "--full-time"
               "-g"
               "--group-directories-first"
               "-G"
               "--no-group"
               "-h"
               "--human-readable"
               "--si"
               "-H"
               "--dereference-command-line"
               "--dereference-command-line-symlink-to-dir"
               "--hide"
               "--indicator-style"
               "-i"
               "--inode"
               "-I"
               "--ignore"
               "-k"
               "--kibibytes"
               "-l"
               "-L"
               "--dereference"
               "-m"
               "-n"
               "--numeric-uid-gid"
               "-N"
               "--literal"
               "-o"
               "-p"
               "--indicator-style"
               "-q"
               "--hide-control-chars"
               "--show-control-chars"
               "-Q"
               "--quote-name"
               "--quoting-style"
               "-r"
               "--reverse"
               "-R"
               "--recursive"
               "-s"
               "--size"
               "-S"
               "--sort"
               "--time"
               "--time-style"
               "-t"
               "-T"
               "--tabsize"
               "-u"
               "-U"
               "-v"
               "-w"
               "--width"
               "-x"
               "-X"
               "-Z"
               "--context"
               "-1"
               "--help"
               "--version")
        (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/cat "shell-completion" nil t)
(defpcmpl pcomplete/cat
  (opts (flags "--help")
        (args (pcmpl-entries-ignoring-common))))

;;;###autoload (autoload 'pcomplete/mv "shell-completion" nil t)
(defpcmpl pcomplete/mv
  (opts (flags "--backup"
               "-b"
               "-f"
               "--force"
               "-i"
               "--interactive"
               "-n"
               "--no-clobber"
               "--strip-trailing-slashes"
               "-S"
               "--suffix"
               "-t"
               "--target-directory"
               "-T"
               "--no-target-directory"
               "-u"
               "--update"
               "-v"
               "--verbose"
               "--help"
               "--version")
        (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/bash "shell-completion" nil t)
(defpcmpl pcomplete/bash
  (let ((short-options
         '("assoc_expand_once"
           "autocd "
           "cdable_vars"
           "cdspell "
           "checkhash"
           "checkjobs"
           "checkwinsize"
           "cmdhist "
           "compat31"
           "compat32"
           "compat40"
           "compat41"
           "compat42"
           "compat43"
           "compat44"
           "complete_fullquote"
           "direxpand"
           "dirspell"
           "dotglob "
           "execfail"
           "expand_aliases"
           "extdebug"
           "extglob "
           "extquote"
           "failglob"
           "force_fignore"
           "globasciiranges"
           "globstar"
           "gnu_errfmt"
           "histappend"
           "histreedit"
           "histverify"
           "hostcomplete"
           "huponexit"
           "inherit_errexit"
           "interactive_comments"
           "lastpipe"
           "lithist "
           "localvar_inherit"
           "localvar_unset"
           "login_shell"
           "mailwarn"
           "no_empty_cmd_completion"
           "nocaseglob"
           "nocasematch"
           "nullglob"
           "progcomp"
           "progcomp_alias"
           "promptvars"
           "restricted_shell"
           "shift_verbose"
           "sourcepath"
           "xpg_echo")))
    `(opts
      (flags "-c"
             "-i"
             "-l"
             "-r"
             "-s"
             "-D"
             ("-O" ',short-options)
             ;; ("+O"
             ;;  (opt (flags ,@short-options)))
             "--debugger"
             "--dump-po-strings"
             "--dump-strings"
             "--help"
             (("--init-file" "--rcfile") (pcmpl-entries-ignoring-common))
             "-l"
             "--login"
             "--noediting"
             "--noprofile"
             "--norc"
             "--posix"
             "--restricted"
             "--verbose"
             "--version")
      (args (pcmpl-entries-ignoring-common))))
  :evaluate-definition t)

;;;###autoload (autoload 'pcomplete/diff "shell-completion" nil t)
(defpcmpl pcomplete/diff
  (opts
   (flags "--normal"
          "-q"
          "--brief"
          "-s"
          "--report-identical-files"
          "-c"
          "-C"
          "--context"
          "--context="
          "-u"
          "-U"
          "--unified"
          "--unified="
          "-e"
          "--ed"
          "-n"
          "--rcs"
          "-y"
          "--side-by-side"
          "-W"
          "--width="
          "--left-column"
          "--suppress-common-lines-p"
          "--show-c-function"
          "-F"
          "--show-function-line="
          "--label"
          "--expand-tabs"
          "-T"
          "--initial-tab"
          "--tabsize="
          "--suppress-blank-empty"
          "-l"
          "--paginate-r"
          "--recursive"
          "--no-dereference"
          "-N"
          "--new-file"
          "--unidirectional-new-file"
          "--ignore-file-name-case"
          "--no-ignore-file-name-case"
          "-x"
          "--exclude="
          "-X"
          "-S"
          (("--exclude-from" "--starting-file" "--from-file" "--to-file") (pcmpl-entries-ignoring-common))
          "-i"
          "--ignore-case"
          "-E"
          "--ignore-tab-expansion"
          "-Z"
          "--ignore-trailing-space"
          "-b"
          "--ignore-space-change"
          "-w"
          "--ignore-all-space"
          "-B"
          "--ignore-blank-lines"
          "-I"
          "--ignore-matching-lines="
          "-a"
          "--text"
          "--strip-trailing-cr-D"
          "--ifdef="
          "--GTYPE-group-format="
          "--line-format=LFMT"
          "--LTYPE-line-format="
          "-d"
          "--minimal"
          "--horizon-lines="
          "--speed-large-files--help"
          "-v"
          "--version")
   (args (pcmpl-entries-ignoring-common))))

;;;###autoload (autoload 'pcomplete/source "shell-completion" nil t)
(defpcmpl pcomplete/source
  (opts (args (pcmpl-entries-ignoring-common))))

;;;###autoload (autoload 'pcomplete/ssh-add "shell-completion" nil t)
(defpcmpl pcomplete/ssh-add
  (opts (flags "-l"
               "-L"
               "-k"
               "-c"
               "-t"
               "-d"
               "-D"
               "-x"
               "-X"
               "-s"
               "-e")
        (args (pcmpl-entries))))

;;;###autoload (autoload 'pcomplete/find "shell-completion" nil t)
(defpcmpl pcomplete/find
  (opts
   (flags
    "-H"
    "-L"
    "-P"
    "-O3"
    (("-maxdepth" "-mindepth") '("1" "2" "3"))
    "-a"
    "-o"
    "-not"
    "-iname"
    "-name"
    "-path"
    ("-type" '("f" "d"))
    "-print"
    "-print0")
   (args
    (pcmpl-dirs))))

;;;###autoload (autoload 'pcomplete/du "shell-completion" nil t)
(defpcmpl pcomplete/du
  (opts
   (flags
    (("-d" "--max-depth") '("1" "2" "3"))
    "-h"
    "--human-readable"
    "--inodes"
    "--help"
    "-a"
    "--all"
    "-L"
    "--dereference")
   (args
    (pcmpl-dirs))))

;;;###autoload (autoload 'pcomplete/busybox "shell-completion" nil t)
(defpcmpl pcomplete/busybox
  (or
   ("bash"
    (opts (args (pcomplete/bash))))
   ("cat"
    (opts (args (pcomplete/cat))))
   ("cp"
    (opts (args (pcomplete/cp))))
   ("chown"
    (opts (args (pcomplete/chown))))
   ("chgrp"
    (opts (args (pcomplete/chgrp))))
   ("diff"
    (opts (args (pcomplete/diff))))
   ("ls"
    (opts (args (pcomplete/ls))))
   ("mv"
    (opts (args (pcomplete/mv))))
   ("rm"
    (opts (args (pcomplete/rm))))
   ("rmdir"
    (opts (args (pcomplete/rmdir))))
   ("tar"
    (opts (args (pcomplete/tar))))
   ("xargs"
    (opts (args (pcomplete/xargs))))
   ("which"
    (opts (args (pcomplete/which))))))

;;;###autoload (autoload 'pcomplete/untar "shell-completion" nil t)
(defpcmpl pcomplete/untar
  `(opts
    (args
     (pcmpl-entries :select ,+tar-regexp+)))
  :evaluate-definition t)

;;;###autoload (autoload 'pcomplete/ln "shell-completion" nil t)
(defpcmpl pcomplete/ln
  (opts
   (flags
    "-b"
    "--backup"
    "-d"
    "-f"
    "--directory"
    "-f"
    "--force"
    "-i"
    "--interactive"
    "-L"
    "--logical"
    "-n"
    "--no-dereference"
    "-P"
    "--physical"
    "-r"
    "--relative"
    "-s"
    "--symbolic"
    "-S"
    "--suffix"
    (("-t" "--target-directony") (pcmpl-dirs))
    "-T"
    "--no-target-directony"
    "-v"
    "--verbose"
    "--help"
    "--version")
   (args
    (pcmpl-entries-ignoring-common)
    (pcmpl-entries-ignoring-common))))

;;;###autoload (autoload 'pcomplete/unzip "shell-completion" nil t)
(defpcmpl pcomplete/unzip
  (opts
   (flags
    ("-d" (pcmpl-dirs))
    "-Z"
    "-A"
    "-c"
    "-f"
    "-l"
    "-p"
    "-t"
    "-T"
    "-u"
    "-v"
    "-z"
    "-a"
    "-b"
    "-B"
    "-C"
    "-D"
    "-E"
    "-F"
    "-i"
    "-j"
    "-J"
    "-K"
    "-L"
    "-M"
    "-n"
    "-N"
    "-o"
    "-P"
    "-q"
    "-qq"
    "-s"
    "-S"
    "-U"
    "-V"
    "-W"
    "-X"
    "-Y"
    "-$"
    "-/"
    "-:"
    "-^"
    "-2")
   (args
    (pcmpl-entries :select "\\.zip\\'"))))

;;;###autoload
(defalias 'pcomplete/l 'pcomplete/ls)
;;;###autoload
(defalias 'pcomplete/la 'pcomplete/ls)
;;;###autoload
(defalias 'pcomplete/ll 'pcomplete/ls)

;;;###autoload (autoload 'pcomplete/emacs-repo-tool "shell-completion" nil t)
(defpcmpl pcomplete/emacs-repo-tool
  (or
   ("generate-grafts"
    (opts
     (flags
      ("--output" (pcmpl-entries-ignoring-common))
      "-f"
      "--force"
      "-h"
      "--help")))))

;;;; Nix

(defun pcmpl-nix-get-channels ()
  "Return a list of git repository remotes."
  (with-temp-buffer
    (call-process "nix-channel"
                  nil
                  (current-buffer)
                  nil
                  "--list")
    (split-string (buffer-substring-no-properties (point-min)
                                                  (point-max))
                  "\n"
                  t)))

;;;###autoload (autoload 'pcomplete/nix-channel "shell-completion" nil t)
(defpcmpl pcomplete/nix-channel
  (or
   ("--add")
   ("--list")
   ("--rollback")
   (("--remove" "--update")
    (opts
     (args (pcmpl-nix-get-channels))))))

;;;###autoload (autoload 'pcomplete/nix-env "shell-completion" nil t)
(defpcmpl pcomplete/nix-env
  (opts
   (flags
    "--help"
    "--dry-run"
    (("-f" "--file") (pcmpl-entries))
    (("-p" "--profile") (pcmpl-entries))
    (("-v" "--verbose") '("0" "1" "2" "3" "4" "5"))
    "-quiet"
    "-Q"
    "--no-build-output"
    "-j"
    "--max-jobs"
    "--cores"
    "--max-silent-time"
    "--timeout"
    "-k"
    "--keep-going"
    "-K"
    "--keep-failed"
    "--fallback"
    "--no-build-hook"
    "--readonly-mode"
    "--arg"
    "--argstr"
    "-A"
    "--attr"
    "-E"
    "--expr"
    ("-I" (pcmpl-entries))
    "--option"
    "--repair"

    "-i"
    "--install"
    "--prebuilt-only"
    "--from-expression"
    "-P"
    "--preserve-installed"

    "-u"
    "--upgrade"
    "--lt"
    "--leq"
    "--eq"
    "--always"

    "-e"
    "--uninstall"

    "--set-flag"

    "-q"
    "--query"
    "--installed"
    "--available"
    "-a"
    "-s"
    "--status"
    "--no-name"
    "-P"
    "--attr-path"
    "--out-path"
    "-c"
    "--compare-versions"
    "--system"
    "--drv-path"
    "--description"
    "--meta"
    "--xml"
    "--json"

    "-S"
    "--switch-profile"

    "--list-generations"
    "--delete-generations"
    "-G"
    "--switch-generation"

    "--rollback")))

;;;; Epilogue

(provide 'shell-completion)

;; Local Variables:
;; End:

;; shell-completion.el ends here
