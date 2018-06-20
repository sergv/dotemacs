;; shell-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 December 2012
;; Description:

(eval-when-compile (require 'subr-x))

(require 'pcomplete)
(require 'completion-setup)
(require 'haskell-misc)

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

(defstruct (pcmpl-flag
            (:conc-name pcmpl-flag/)
            (:constructor pcmpl-flag/create))
  names           ;; Nonempty list of strings
  completion-expr ;; Closed expression to generate completions, may be nil
  )

(defun* make-pcmpl-flag (&key names completion-expr)
  (cl-assert (or (string? names)
                 (-all? #'string? names)))
  (pcmpl-flag/create :names names :completion-expr completion-expr))

(defun pcpmpl/make-name-regex (flag)
  (cl-assert (pcmpl-flag-p flag))
  (regexp-opt (pcmpl-flag/names flag)))

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
             (list? entry)
             (= 2 (length entry)))
        (let ((flag-name (first entry))
              (compl-expr (cadr-safe entry)))
          (cl-assert (or (string? flag-name)
                         (and
                          (list? flag-name)
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

;;; simple pcomplete macro

;;;###autoload
(defmacro* defpcmpl (name definition &key (evaluate-definition nil))
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
completion-expr - elisp expression, like (pcomplete-here (pcomplete-entries))
positional-arg  - emacs string, usually without -s

N.B. Small deviations to the grammar may be tolerated, but they're mostly
useless, e.g. (opts (args)) would be accepted but to no effect.


<or> just lists alternatives, mostly useful for positional arguments.

<opts> stands for given flags followed by args, no more positional arguments."
  (declare (indent 1))
  (cl-assert (string-match-p "pcomplete/" (symbol->string name)))
  (let ((got-end-of-flags-var '#:got-end-of-flags))
    (letrec ((process
              (lambda (definition positional-depth)
                (cl-assert (and (list? definition)
                                (not (null? definition)))
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
                                 (list? (first def))
                                 (-all? #'string? (first def))))))
                         (positional-defs
                          (-filter positional-def? defs))
                         (other-defs
                          (--filter (not (funcall positional-def? it)) defs))
                         (names
                          (-map (lambda (x) (if (list? x) (first x) (list x)))
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
                               ,@(when (not (null? other-defs))
                                   (list `(t
                                           ,@(--map (funcall process it positional-depth)
                                                    other-defs)))))))))))
             ;; -s and --long flags followed by positional arguments
             (process-opts
              (lambda (definition)
                (let ((info (rest definition)))
                  (cl-assert (-all? (lambda (entry)
                                      (and (list? entry)
                                           (not (null? entry))
                                           (memq (first entry) '(flags args))))
                                    info)
                             nil
                             "<opts> clause must contain either (flags ...) or (args ...) entries only: %S"
                             info)
                  (let* ((flags (pcmpl/make-flags (cdr-safe (assoc 'flags info))))
                         (flags-with-args (-filter #'pcmpl-flag/completion-expr flags))
                         ;; Positional arguments.
                         (args (assoc 'args info)))
                    (cl-assert (--all? (-all? (comp (partial #'string-match-p "^--?[^-].*"))
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
                      (multiple-value-bind (single-dash-flags double-dash-flags)
                          (--separate (string-match-p "^-[^-]" it)
                                      (-mapcat #'pcmpl-flag/names flags))
                        `(while
                             (unless ,got-end-of-flags-var
                               (let ((current-arg (pcomplete-arg)))
                                 (cond
                                   ,@(-map (lambda (flag)
                                             `((pcomplete-match ,(pcpmpl/make-name-regex flag))
                                               ,@(awhen (pcmpl-flag/completion-expr flag) (list it))))
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
                                          (cdr args)
                                        ;; Return nil to while loop to show that
                                        ;; there are no more completions.
                                        '(nil)))))))))))))
             ;; Positional arguments for subcommands
             (process-positional
              (lambda (definition pcomplete-arg-var positional-depth)
                (let ((name (first definition)))
                  (cl-assert (or (string? name)
                                 (and (list? name)
                                      (-all? #'string? name))))
                  `(,(cond
                       ((string? name)
                        `(string= ,pcomplete-arg-var ,name))
                       ((list? name)
                        `(member ,pcomplete-arg-var ,name))
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

(defun pcmpl-entries-ignoring (re)
  "Like `pcomplete-entries' but ignores files mathing RE."
  (let ((pcomplete-file-ignore re)
        (pcomplete-dir-ignore
         (eval-when-compile
           (concat "\\`"
                   (regexp-opt +version-control-directories+)
                   "\\'"))))
    (pcomplete-entries)))

(defun pcmpl-entries-ignoring-common ()
  (pcmpl-entries-ignoring
   (eval-when-compile
     (concat "\\`.*"
             (regexp-opt +ignored-file-extensions+)
             "\\'"))))

;;; Version control

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
        (args (pcomplete-here (pcomplete-entries)))))
      ("bisect"
       (or
        ("help")
        ("start"
         (opts
          (flags "--no-checkout")
          (args (pcomplete-here (pcmpl-git-rev)))))
        ("bad"
         (opts
          (args (pcomplete-here (pcmpl-git-rev)))))
        ("good"
         (opts
          (args (pcomplete-here (pcmpl-git-rev)))))
        ("skip"
         (opts
          (args (pcomplete-here (pcmpl-git-rev)))))
        ("next")
        ("reset"
         (opts
          (args (pcomplete-here (pcmpl-git-commits)))))
        ("visualize")
        ("replay")
        ("log")
        ("run")))
      ("branch"
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
        (args (pcomplete-here (pcmpl-git-commits)))))
      ("checkout"
       (opts
        (flags "-q"
               "--quiet"
               (("-b" "-B" "--orphan") (pcomplete-here (pcmpl-git-branch-names)))
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
        (args (pcomplete-here (pcmpl-git-commits)))))
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
      ("commit"
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
        (args (pcomplete-here (pcomplete-entries)))))
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
        (args (pcomplete-here (pcmpl-git-commits-and-files)))))
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
        (args (pcomplete-here (pcomplete-entries)))))
      ("init"
       (opts
        (flags "--bare"
               "--shared"
               ("--separate-git-dir" (pcomplete-here (pcomplete-dirs))))
        (args (pcomplete-here (pcomplete-entries)))))
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
        (args (pcomplete-here (pcmpl-git-commits-and-files)))))
      ("merge"
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
        (args (pcomplete-here (pcmpl-git-commits)))))
      ("mv"
       (opts
        (flags "-v"
               "--verbose"
               "-n"
               "--dry-run"
               "-f"
               "--force"
               "-k")
        (args (pcomplete-here (pcomplete-entries)))))
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
         (pcomplete-here (pcmpl-git-get-remotes))
         (pcomplete-here (pcmpl-git-branch-names)))))
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
        (args (pcomplete-here (pcmpl-git-rev)))))
      ("remote"
       (or
        ("add"
         (opts
          (flags
           "-v"
           "--verbose"
           ("-t" (pcomplete-here (pcmpl-git-branch-names)))
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
          (pcomplete-here (pcmpl-git-get-remotes))
          (pcomplete-here (pcmpl-git-get-remotes))))
        ("remove"
         (opts
          (flags
           "-v"
           "--verbose"))
         (args
          (pcomplete-here (pcmpl-git-get-remotes))))
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
           (pcomplete-here (pcmpl-git-get-remotes))
           (pcomplete-here (pcmpl-git-get-refs "heads")))))
        ("set-branches"
         (opts
          (flags
           "-v"
           "--verbose"
           "--add")
          (args
           (pcomplete-here (pcmpl-git-get-remotes)))))
        ("get-url"
         (opts
          (flags
           "-v"
           "--verbose"
           "--push"
           "--add")
          (args
           (pcomplete-here (pcmpl-git-get-remotes)))))
        ("set-url"
         (opts
          (flags
           "-v"
           "--verbose"
           "--push"
           "--add"
           "--delete")
          (args
           (pcomplete-here (pcmpl-git-get-remotes)))))
        ("show"
         (opts
          (flags
           "-v"
           "--verbose"
           "-n")
          (args
           (pcomplete-here (pcmpl-git-get-remotes)))))
        ("prune"
         (opts
          (flags
           "-v"
           "--verbose"
           "-n"
           "--dry-run")
          (args
           (pcomplete-here (pcmpl-git-get-remotes)))))
        ("update"
         (opts
          (flags
           "-v"
           "--verbose"
           "-p"
           "--prune")
          (args
           (pcomplete-here (pcmpl-git-get-remotes)))))))
      ("reset"
       (opts
        (flags "--mixed"
               "--soft"
               "--hard"
               "--merge"
               "--keep")
        (args (pcomplete-here (pcmpl-git-commits-and-files)))))
      ("rm"
       (opts
        (flags "-f"
               "-r"
               "-n"
               "--cached"
               "--dry-run"
               "--force")
        (args (pcomplete-here (pcomplete-entries)))))
      ("show"
       (opts
        (flags "-q"
               "-L"
               "--quiet"
               "--source"
               "--use-mailmap"
               "--decorate")
        (args (pcomplete-here (pcomplete-entries)))))
      ("status"
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
          (args (pcomplete-here (pcomplete-dirs)))))
        ("status"
         (opts
          (flags "--cached"
                 "--recursive")
          (args (pcomplete-here (pcomplete-dirs)))))

        ("init"
         (opts
          (args (pcomplete-here (pcomplete-dirs)))))
        ("deinit"
         (opts
          (flags "-f"
                 "--force")
          (args (pcomplete-here (pcomplete-dirs)))))
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
          (args (pcomplete-here (pcomplete-dirs)))))
        ("summary"
         (opts
          (flags "--cached"
                 "--files"
                 "--summary-limit"
                 "--commit")
          (args (pcomplete-here (pcomplete-dirs)))))
        ("foreach"
         (opts
          (flags "--recursive")))
        ("sync"
         (opts
          (flags "--recursive")
          (args (pcomplete-here (pcomplete-dirs)))))))
      ("tag"
       (opts
        (args (pcomplete-here (pcmpl-git-get-refs "tags")))))))

;;; Haskell

(defun pcmpl-haskell-source-files ()
  (pcomplete-entries
   (eval-when-compile
     (concat
      "\\."
      (regexp-opt +haskell-extensions+)
      "\\'"))))

(defun pcmpl-haskell-source-or-obj-files ()
  (pcomplete-entries
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
             '("lib"
               "dll")))
        +haskell-extensions+))
      "\\'"))))

;;;###autoload (autoload 'pcomplete/runghc "shell-completion" nil t)
(defpcmpl pcomplete/runghc
  (opts
   (args (pcomplete-here (pcmpl-haskell-source-or-obj-files)))))

;;;###autoload (autoload 'pcomplete/runhaskell "shell-completion" nil t)
(defpcmpl pcomplete/runhaskell
  (opts
   (args (pcomplete-here (pcmpl-haskell-source-or-obj-files t)))))

;;;###autoload (autoload 'pcomplete-ghc-flags "shell-completion" nil)
(defparameter pcomplete-ghc-flags
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
    ("--show-iface" (pcomplete-here (pcomplete-entries)))
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

    (("--show-iface" "--exclude-module" "-dep-makefile" "-o" "-ohi") (pcomplete-here (pcomplete-entries)))
    (("-i" "-I" "-L" "--dumpdir" "-hidir" "-odir" "-outputdir" "-stubdir" "-tmpdir") (pcomplete-here (pcomplete-dirs)))
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

    (("-package-db" "-package-env") (pcomplete-here (pcmpl-entries-ignoring-common)))
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
    ("-dth-dec-file" (pcomplete-here (pcomplete-entries)))
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
    (args (pcomplete-here (pcmpl-haskell-source-or-obj-files))))
  :evaluate-definition t)

;;;###autoload (autoload 'pcomplete/cabal "shell-completion" nil t)
(defpcmpl pcomplete/cabal
  (let* ((programs '("alex"
                     "ar"
                     "c2hs"
                     "cpphs"
                     "ffihugs"
                     "gcc"
                     "ghc"
                     "ghc-pkg"
                     "greencard"
                     "haddock"
                     "happy"
                     "hmake"
                     "hpc"
                     "hsc2hs"
                     "hscolour"
                     "hugs"
                     "jhc"
                     "ld"
                     "lhc"
                     "lhc-pkg"
                     "nhc98"
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
         (builddir-flags '(("--builddir" (pcomplete-here (pcomplete-dirs)))))
         (solver-flags '("--solver"
                         "--solver=topdown"
                         "--solver=modular"
                         "--solver=choose"))
         (fetch-flags '("--max-backjumps"
                        "--reorder-goals"
                        "--shadow-installed-packages"))
         (program-options-flags
          (-mapcat (lambda (p)
                     `((,(concat "--" p)
                        (pcomplete-here (pcmpl-entries-ignoring-common)))
                       ,(concat "--" p "-option")
                       ,(concat "--" p "-options")))
                   programs))
         (run-flags `(,@help-verbosity-flags
                      ,@builddir-flags
                      "-j"
                      "--jobs"
                      "--only"
                      ,@program-options-flags))
         (configure-flags `(,@help-verbosity-flags
                            ,@builddir-flags
                            "-g"
                            "--ghc"
                            "--nhc98"
                            "--jhc"
                            "--lhc"
                            "--hugs"
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
                             (pcomplete-here (pcomplete-dirs)))

                            "--program-prefix"
                            "--program-suffix"
                            "--enable-library-vanilla"
                            "--disable-library-vanilla"
                            "-p"
                            "--enable-library-profiling"
                            "--disable-library-profiling"
                            "--enable-shared"
                            "--disable-shared"
                            "--enable-executable-dynamic"
                            "--disable-executable-dynamic"
                            "--enable-executable-profiling"
                            "--disable-executable-profiling"
                            "--enable-profiling"
                            "--disable-profiling"
                            "-O"
                            "--enable-optimization"
                            "--enable-optimization=0"
                            "--enable-optimization=1"
                            "--enable-optimization=2"
                            "--disable-optimization"
                            "--enable-library-for-ghci"
                            "--disable-library-for-ghci"
                            "--enable-split-objs"
                            "--disable-split-objs"
                            "--enable-executable-stripping"
                            "--disable-executable-stripping"
                            "--configure-option"
                            "--user"
                            "--global"
                            ("--package-db" (pcomplete-here (pcmpl-entries-ignoring-common)))
                            "-f"
                            "--flags"
                            (("--extra-include-dirs" "--extra-lib-dirs" "--extra-prog-path")
                             (pcomplete-here (pcomplete-dirs)))
                            "--enable-tests"
                            "--disable-tests"
                            "--enable-library-coverage"
                            "--disable-library-coverage"
                            "--enable-benchmarks"
                            "--disable-benchmarks"
                            ,@program-options-flags
                            "--cabal-lib-version"
                            "--constraint"
                            "--preference"
                            ,@solver-flags
                            "--allow-newer")))
    `(or
      ("install"
       (opts
        (flags ,@configure-flags
               "--enable-documentation"
               "--disable-documentation"
               "--doc-index-file"
               "--dry-run"
               ,@fetch-flags
               "--reinstall"
               "--avoid-reinstalls"
               "--force-reinstalls"
               "--upgrade-dependencies"
               "--only-dependencies"
               "--dependencies-only"
               "--root-cmd"
               ("--symlink-bindir" (pcomplete-here (pcomplete-dirs)))
               "--build-summary"
               "--build-log"
               "--remote-build-reporting"
               "--remote-build-reporting=none"
               "--remote-build-reporting=anonymous"
               "--remote-build-reporting=detailed"
               "--one-shot"
               "-j"
               "--jobs"
               "--haddock-hoogle"
               "--haddock-html"
               "--haddock-html-location=URL"
               "--haddock-executables"
               "--haddock-internal"
               (("--haddock-css" "--haddock-hscolour-css") (pcomplete-here (pcmpl-entries-ignoring-common)))
               "--haddock-hyperlink-source"
               "--haddock-contents-location")))
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
               (("-d" "--destdir") (pcomplete-here (pcomplete-dirs)))
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
        (flags
         ,@run-flags)))
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
        (flags ,@help-verbosity-flags
               ,@builddir-flags
               "-j"
               "--jobs"
               "--only"
               ,@program-options-flags)))
      ("repl"
       (opts
        (flags
         ,@run-flags)))
      ("sandbox"
       (or ("init")
           ("delete")
           ("add-source"
            (opts
             (args (pcomplete-here (pcmpl-entries-ignoring-common)))))
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
               "--log"
               "--machine-log"
               "--show-details"
               "--show-details=always"
               "--show-details=never"
               "--show-details=failures"
               "--keep-tix-files"
               "--test-options"
               "--test-option"
               "-j"
               "--jobs"
               "--only")))
      ("bench")
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
   (args (pcomplete-here (pcomplete-entries "\\.hp\\'")))))

;;;###autoload (autoload 'pcomplete/hp2pdf "shell-completion" nil t)
(defpcmpl pcomplete/hp2pdf
  (opts
   (args (pcomplete-here (pcomplete-entries "\\.hp\\'")))))

;;;###autoload (autoload 'pcomplete/hp2pretty "shell-completion" nil t)
(defpcmpl pcomplete/hp2pretty
  (opts
   (flags
    "--uniform-scale=none"
    "--uniform-scale=time"
    "--uniform-scale=memory"
    "--uniform-scale=both")
   (args (pcomplete-here (pcomplete-entries "\\.hp\\'")))))

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
           ("--stack-yaml" (pcomplete-here (pcomplete-entries "\\.yaml\\'")))))
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
   (args (pcomplete-here (pcomplete-entries "\\`package\\.yaml\\'")))))

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
         '((("-X" "--language") (pcomplete-here haskell-language-extensions))))
        (extension-flags
         '("-e"
           "--extension"))
        (hint-flags
         '(("--datadir" (pcomplete-here (pcomplete-dirs)))
           (("-r" "--report") (pcomplete-here (pcomplete-entries)))
           (("-h" "--hint") (pcomplete-here (pcomplete-entries)))
           "-w"
           "--with"))
        (cpp-flags
         '("-p"
           "--path"
           "--cpp-define"
           ("--cpp-include" (pcomplete-here (pcomplete-dirs)))
           ("--cpp-file" (pcomplete-here (pcomplete-entries)))
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
        (args (pcomplete-here (pcmpl-haskell-source-files)))))
      ("grep"
       (opts
        (flags
         ,@standard-flags
         ,@extension-flags
         ,@language-flags
         ,@cpp-flags))
       (args
        (pcomplete-here (pcmpl-haskell-source-files))))
      ("test"
       (opts
        (flags
         ,@standard-flags
         ,@hint-flags
         ("--proof" (pcomplete-here (pcomplete-entries)))
         ("--tempdir" (pcomplete-here (pcomplete-dirs)))
         "--quickcheck"
         "--typecheck")))
      ("hse"
       (opts
        (flags
         ,@standard-flags
         ,@language-flags)))))
  :evaluate-definition t)

;;; C, low-level stuff

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
   (args (pcomplete-here (pcomplete-entries)))))

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
          ("-Xassembler" (pcomplete-here (pcmpl-gcc-assembler-flags)))
          ("-Xpreprocessor" (pcomplete-here (pcmpl-gcc-preprocessor-flags)))
          ("-Xlinker" (pcomplete-here (pcmpl-gcc-linker-flags)))
          "-save-temps"
          "-save-temps"
          "-no-canonical-prefixes"
          "-pipe"
          "-time"
          ("-specs" (pcomplete-here (pcmpl-entries-ignoring-common)))
          "-std"
          "-std=c++11"
          "-std=c99"
          "-std=c89"
          ("--sysroot" (pcomplete-here (pcomplete-dirs)))
          ("-B" (pcomplete-here (pcomplete-dirs)))
          "-v"
          "-###"
          "-E"
          "-S"
          "-c"
          ("-o" (pcomplete-here (pcomplete-entries)))
          "-pie"
          "-shared"
          ("-x" (pcomplete-here '("c" "c++" "assembler" "none")))

          "-static"

          (("-I" "-L") (pcomplete-here (pcomplete-dirs)))
          "-Os"
          "-O2"
          "-O3"
          "-marh"
          "-march=native"
          "-fomit-frame-pontier")
   (args (pcomplete-here (pcomplete-entries)))))

;;; simpler definitions, vanilla Unix & GNU tools

;;;###autoload (autoload 'pcomplete/cp "shell-completion" nil t)
(defpcmpl pcomplete/cp
  (opts (flags "-r")
        (args (pcomplete-here (pcomplete-entries)))))

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
        (args (pcomplete-here (pcomplete-entries)))))

;;;###autoload (autoload 'pcomplete/cat "shell-completion" nil t)
(defpcmpl pcomplete/cat
  (opts (flags "--help")
        (args (pcomplete-here (pcmpl-entries-ignoring-common)))))

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
        (args (pcomplete-here (pcomplete-entries)))))

;;;###autoload (autoload 'pcomplete/bash "shell-completion" nil t)
(defpcmpl pcomplete/bash
  (let ((short-options nil))
    `(opts
      (flags "-c"
             "-i"
             "-l"
             "-r"
             "-s"
             "-D"
             ("-O"
              (opt (flags ,@short-options)))
             ;; ("+O"
             ;;  (opt (flags ,@short-options)))
             "--debugger"
             "--dump-po-strings"
             "--dump-strings"
             "--help"
             (("--init-file" "--rcfile") (pcomplete-here (pcmpl-entries-ignoring-common)))
             "-l"
             "--login"
             "--noediting"
             "--noprofile"
             "--norc"
             "--posix"
             "--restricted"
             "--verbose"
             "--version")
      (args (pcomplete-here (pcmpl-entries-ignoring-common)))))
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
          (("--exclude-from" "--starting-file" "--from-file" "--to-file") (pcomplete-here (pcmpl-entries-ignoring-common)))
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
   (args (pcomplete-here (pcmpl-entries-ignoring-common)))))

;;;###autoload (autoload 'pcomplete/source "shell-completion" nil t)
(defpcmpl pcomplete/source
  (opts (args (pcomplete-here (pcmpl-entries-ignoring-common)))))

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
        (args (pcomplete-here (pcomplete-entries)))))

;;;###autoload (autoload 'pcomplete/find "shell-completion" nil t)
(defpcmpl pcomplete/find
  (opts
   (flags
    "-H"
    "-L"
    "-P"
    "-O3"
    (("-maxdepth" "-mindepth") (pcomplete-here '("1" "2" "3")))
    "-a"
    "-o"
    "-not"
    "-iname"
    "-name"
    "-path"
    ("-type" (pcomplete-here '("f" "d")))
    "-print"
    "-print0")
   (args
    (pcomplete-here* (pcomplete-dirs)))))

;;;###autoload (autoload 'pcomplete/du "shell-completion" nil t)
(defpcmpl pcomplete/du
  (opts
   (flags
    (("-d" "--max-depth") (pcomplete-here '("1" "2" "3")))
    "-h"
    "--human-readable"
    "--inodes"
    "--help"
    "-a"
    "--all"
    "-L"
    "--dereference"
    )
   (args
    (pcomplete-here* (pcomplete-dirs)))))

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
  (opts
   (args
    (pcomplete-here* (pcomplete-entries "\\.tar\\(?:\\.\\(?:gz\\|bz2\\|xz\\|lz\\|lzip\\|7z\\)\\)?\\'")))))

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
    (("-t" "--target-directony") (pcomplete-here (pcomplete-dirs)))
    "-T"
    "--no-target-directony"
    "-v"
    "--verbose"
    "--help"
    "--version")
   (args
    (pcomplete-here (pcmpl-entries-ignoring-common))
    (pcomplete-here (pcmpl-entries-ignoring-common)))))

;;;###autoload (autoload 'pcomplete/unzip "shell-completion" nil t)
(defpcmpl pcomplete/unzip
  (opts
   (flags
    ("-d" (pcomplete-here (pcomplete-dirs)))
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
    (pcomplete-here (pcmpl-entries "\\.zip\\'")))))

;;;###autoload
(defalias 'pcomplete/l 'pcomplete/ls)
;;;###autoload
(defalias 'pcomplete/la 'pcomplete/ls)
;;;###autoload
(defalias 'pcomplete/ll 'pcomplete/ls)

(defpcmpl pcomplete/emacs-repo-tool
  (or
   ("generate-grafts"
    (opts
     (flags
      ("--output" (pcomplete-here (pcmpl-entries-ignoring-common)))
      "-f"
      "--force"
      "-h"
      "--help")))))

(provide 'shell-completion)

;; Local Variables:
;; End:

;; shell-completion.el ends here
