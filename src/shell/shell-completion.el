;; shell-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 December 2012
;; Description:

(require 'pcomplete)
(require 'haskell-misc)

(eval-after-load "pcomplete"
  '(progn
     (require 'pcmpl-gnu)
     (require 'pcmpl-linux)
     (require 'pcmpl-rpm)
     (require 'pcmpl-unix)

     (setf pcomplete-autolist nil
           pcomplete-recexact nil
           pcomplete-cycle-completions t
           pcomplete-command-completion-function
           (lambda ()
             (pcomplete-here
              (pcomplete-entries nil
                                 (lambda (filename)
                                   (or (file-executable-p filename)
                                       (string-match-pure? (rx (or ".hs"
                                                                   ".sh"
                                                                   ".py"
                                                                   ".exe"
                                                                   ".bat"
                                                                   ".cmd")
                                                               eol)
                                                           filename)))))))))

(defun pcmpl-git-commits ()
  "Return list of commits to complete against."
  (append '("HEAD")
          (pcmpl-git-get-refs "heads\\|tags")))

(defun pcmpl-git-commits-and-files ()
  "Return list of commits to complete against."
  (append (pcmpl-git-commits)
          (directory-files default-directory
                           nil
                           directory-files-no-dot-files-regexp)))

(defalias 'pcmpl-git-rev 'pcmpl-git-commits)

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
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
        (delq nil
              (map (lambda (ref)
                     (when (string-match? re ref)
                       (match-string-no-properties 1 ref)))
                   (split-string (buffer-substring-no-properties (point-min)
                                                                 (point-max))
                                 "\n"
                                 t)))))))

;;; dsl draft

;; dsl example
;; ("branch"
;;  ;; alternatives
;;  (or (* (or (xor "--color"
;;                  "--no-color")
;;             ;; these xors lift this dsl into context-sensetive domain
;;             (xor "-r"
;;                  "-a")
;;             "--list"
;;             (xor ("-v"
;;                   (opt "--abbrev"))
;;                  "--noabbrev")
;;             ))
;;      ("--unset-upstream"
;;       (eval (pcmpl-git-get-refs "heads")))
;;      ((xor "-m" "-M")
;;       (eval (pcmpl-git-get-refs "heads"))
;;       (eval (pcmpl-git-get-refs "heads")))
;;      ((xor "-d" "-D")
;;       (? "-r")
;;       (+ (eval (pcmpl-git-get-refs "heads"))))))

;; how about

;; ("foo"
;;  (* (or "-a"
;;         "-b"))
;;  (* (eval (pcomplete-entries))))

;;; simple pcomplete macro

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
  (assert (string-match-pure? "pcomplete/" (symbol->string name)))
  (let ((got-end-of-flags-var (gensym "got-end-of-flags"))
        (last-arg-starts-with-single-dash-var (gensym "last-arg-starts-with-single-dash?"))
        (last-arg-starts-with-two-dashes-var (gensym "last-arg-starts-with-two-dashes?")))
    (letrec ((process
              (lambda (definition level)
                (assert (and (list? definition)
                             (not (null? definition)))
                        nil
                        "invalid definition %s"
                        definition)
                (cond ((eq? 'or (first definition))
                       (funcall process-or definition level))
                      ((eq? 'opts (first definition))
                       (funcall process-opts definition))
                      (t
                       (error "process: unsupported definition: %S" definition)))))
             (process-or
              (lambda (definition level)
                (when-let (defs (rest definition))
                  (let* ((positional-def?
                          (lambda (def)
                            (or (string? (first def))
                                (and
                                 (list? (first def))
                                 (all? #'string? (first def))))))
                         (positional-defs
                          (filter positional-def? defs))
                         (other-defs
                          (filter (comp #'not positional-def?) defs))
                         (names
                          (map #'(lambda (x) (if (list? x) (first x) (list x)))
                               positional-defs)))
                    `(progn
                       ,@(when names
                           (list `(pcomplete-here ',names)))

                       (cond ,@(map (lambda (def)
                                      (funcall process-positional
                                               def
                                               level))
                                    positional-defs)
                             ,@(when (not (null? other-defs))
                                 (list `(t
                                         ,@(map (lambda (def)
                                                  (funcall process
                                                           def
                                                           level))
                                                other-defs))))))))))
             (process-opts
              (lambda (definition)
                (let* ((info (rest definition))
                       (flags (cdr-safe (assoc 'flags info)))
                       (args (assoc 'args info))
                       (get-flag-name
                        (lambda (flag)
                          (cond ((string? flag)
                                 flag)
                                ((and (list? flag)
                                      (not (null? flag))
                                      (string? (first flag)))
                                 (first flag))
                                (t
                                 (error "process-opts: invalid flag, not string or non-empty list with string as a first element: %S" flag)))))
                       (complex-flag? (lambda (flag) (list? flag)))
                       (short-flag? (comp (partial #'string-match-pure? "^-[^-].*")))
                       (long-flag? (comp (partial #'string-match-pure? "^--[^-].*")))
                       (expand-complex
                        (lambda (flag-def)
                          (assert (and (list? flag-def)
                                       (= 2 (length flag-def)))
                                  nil
                                  "invalid flag def, (<name> <compl>) expected: %S"
                                  flag-def)
                          (let ((name (first flag-def))
                                (compl (second flag-def)))
                            ;; todo: use -1 here instead of last?
                            `((string= (pcomplete-arg 'last -1) ,name)
                              ,compl)))))
                  (assert (all? (lambda (entry)
                                  (and (list? entry)
                                       (not (null? entry))
                                       (memq (first entry) '(flags args))))
                                info)
                          nil
                          "<opts> clause must contain either (flags ...) or (args ...) entries only: %S"
                          info)
                  (assert (all? (comp (partial #'string-match-pure?
                                               "^--?[^-].*")
                                      get-flag-name)
                                flags)
                          nil
                          "All flags names must start with dash or two dashes: %S\nFailed flags: %S"
                          flags
                          (filter (comp #'not
                                        (partial #'string-match-pure?
                                                 "^--?[^-].*")
                                        get-flag-name)
                                  flags))
                  (let ((short (filter short-flag?
                                       (map get-flag-name flags)))
                        (short-complex (filter (lambda (flag)
                                                 (and (funcall short-flag?
                                                               (funcall get-flag-name
                                                                        flag))
                                                      (funcall complex-flag? flag)))
                                               flags))
                        (long (concatMap (lambda (flag)
                                           (save-match-data
                                             (if (string-match? "=$" flag)
                                               (list flag (replace-match "" nil nil flag))
                                               (list flag))))
                                         (filter long-flag?
                                                 (map get-flag-name flags))))
                        (long-complex (filter (lambda (flag)
                                                (and (funcall long-flag?
                                                              (funcall get-flag-name
                                                                       flag))
                                                     (funcall complex-flag? flag)))
                                              flags)))
                    (when (or (not (null? short))
                              (not (null? long))
                              (not (null? args)))
                      `(while
                           (cond
                             ,@(when (not (null? short))
                                 (list
                                  `((and (not ,got-end-of-flags-var)
                                         ,last-arg-starts-with-single-dash-var)
                                    (pcomplete-here '(,@short
                                                      ;; ,@long
                                                      )))))
                             ,@(when (not (null? long))
                                 (list
                                  `((and (not ,got-end-of-flags-var)
                                         ,last-arg-starts-with-two-dashes-var)
                                    (pcomplete-here '(,@long)))))
                             ,@(when (or (not (null? args))
                                         (not (null? short-complex))
                                         (not (null? long-complex)))
                                 (when (not (null? args))
                                   (assert (not (null? (cadr args)))
                                           nil
                                           "Meaningless (args ...) clause without completion action, (args <action>) expected: %s"
                                           args))
                                 (list `(t
                                         (cond
                                           ,@(when (not (null? short-complex))
                                               (map expand-complex short-complex))
                                           ,@(when (not (null? long-complex))
                                               (map expand-complex long-complex))
                                           (t
                                            ,(cadr args)))))))))))))
             (process-positional
              (lambda (definition level)
                (let ((name (first definition)))
                  (assert (or (string? name)
                              (and (list? name)
                                   (all? #'string? name))))
                  `(,(if (string? name)
                       `(string= (pcomplete-arg ,level) ,name)
                       `(member (pcomplete-arg ,level) ,name))
                    ,@(awhen (cadr-safe definition)
                        (list (funcall process
                                       it
                                       (+ level 1)))))))))
      `(defun ,name ()
         (let* ((,last-arg-starts-with-single-dash-var
                 (string-match-pure? "^-\\([^-]\\|$\\)" (pcomplete-arg 'last)))
                (,last-arg-starts-with-two-dashes-var
                 (string-match-pure? "^--\\([^-]\\|$\\)" (pcomplete-arg 'last)))
                (,got-end-of-flags-var
                 (and (not ,last-arg-starts-with-two-dashes-var)
                      (member "--" pcomplete-args))))
           ,(funcall process
                     (if evaluate-definition
                       (eval definition t)
                       definition)
                     1))))))

(defun pcmpl-entries-ignoring (re)
  "Like `pcomplete-entries' but ignores files mathing RE."
  (let ((pcomplete-file-ignore re)
        (pcomplete-dir-ignore
         (regexp-opt *version-control-directories*)))
    (pcomplete-entries)))

(defun pcmpl-entries-ignoring-common ()
  (pcmpl-entries-ignoring (concat (regexp-opt *ignored-file-name-endings*)
                                  "$")))

;;; Version control

;;;###autoload
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
               ("-b" (pcomplete-here (pcmpl-git-get-refs "heads")))
               ("-B" (pcomplete-here (pcmpl-git-get-refs "heads")))
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
               "--decorate"
               "-L")
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
               "--follow-tags")))
      ("rebase"
       (opts
        (flags "-i"
               "-x"
               "--interactive"
               "--exec"
               "--no-ff")
        (args (pcomplete-here (pcmpl-git-rev)))))
      ("remote"
       (or
        ("add"
         (opts
          (flags
           "-v"
           "--verbose"
           ("-t" (pcomplete-here (pcmpl-git-get-refs "heads")))
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
           "--verbose")))
        ("remove"
         (opts
          (flags
           "-v"
           "--verbose")))
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
           (pcomplete-here (pcmpl-git-get-refs "heads")))))
        ("set-branches"
         (opts
          (flags
           "-v"
           "--verbose"
           "--add")))
        ("set-url"
         (opts
          (flags
           "-v"
           "--verbose"
           "--push"
           "--add"
           "--delete")))
        ("show"
         (opts
          (flags
           "-v"
           "--verbose"
           "-n")))
        ("prune"
         (opts
          (flags
           "-v"
           "--verbose"
           "-n"
           "--dry-run")))
        ("update"
         (opts
          (flags
           "-v"
           "--verbose"
           "-p"
           "--prune")))))
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

(defun pcmpl-haskell-source-or-obj-files (&optional ignore-obj)
  (pcmpl-entries-ignoring
   (concat
    (regexp-opt (if ignore-obj
                  *ignored-file-name-endings*
                  (remove-if (lambda (ext)
                               (string-match-pure? (rx "."
                                                       (or "o"
                                                           "p_o"
                                                           "p_hi"
                                                           "prof_o"
                                                           "hi"
                                                           "so"
                                                           "a"
                                                           "lib"
                                                           "dll")
                                                       eol)
                                                   ext))
                             *ignored-file-name-endings*)))
    "$")))

;;;###autoload
(defpcmpl pcomplete/runghc
  (opts
   (args (pcomplete-here (pcmpl-haskell-source-or-obj-files)))))

;;;###autoload
(defpcmpl pcomplete/runhaskell
  (opts
   (args (pcomplete-here (pcmpl-haskell-source-or-obj-files t)))))

;;;###autoload
(defparameter pcomplete-ghc-flags
  '("-?"
    "-help"
    "-v"
    "-v1"
    "-v2"
    "-v3"
    "-V"
    "--supported-extensions"
    "--supported-languages"
    "--info"
    "--version"
    "--numeric-version"
    "--print-libdir"
    "-ferror-spans"
    "-H"
    "-Rghc-timing"

    "-E"
    "-C"
    "-S"
    "-c"
    "-x"

    "--interactive"
    "--make"
    "-e"
    "-M"

    "-hcsuf"
    ("-hidir" (pcomplete-here (pcomplete-dirs)))
    "-hisuf"
    "-o"
    ("-odir" (pcomplete-here (pcomplete-dirs)))
    "-ohi"
    "-osuf"
    ("-studdir" (pcomplete-here (pcomplete-dirs)))
    ("-dumpdir" (pcomplete-here (pcomplete-dirs)))
    ("-outputdir" (pcomplete-here (pcomplete-dirs)))

    "-keep-hc-file"
    "-keep-hc-files"
    "-keep-llvm-file"
    "-keep-llvm-files"
    "-keep-s-file"
    "-keep-s-files"
    "-keep-tmp-files"

    ("-tmpdir" (pcomplete-here (pcomplete-dirs)))

    ("-i" (pcomplete-here (pcomplete-dirs)))

    "-ddump-hi"
    "-ddump-hi-diffs"
    "-ddump-minimal-imports"
    ("--show-iface" (pcomplete-here (pcomplete-entries)))

    "-fforce-recomp"

    "-ignore-dot-ghci"
    "-ghci-script"
    "-fbreak-on-exception"
    "-fno-break-on-exception"
    "-fbreak-on-error"
    "-fno-break-on-error"
    "-fprint-evld-with-show"
    "-fno-print-evld-with-show"
    "-fprint-bind-result"
    "-fno-print-bind-result"
    "-fno-print-bind-contents"
    "-fno-implicit-import-qualified"
    "-interactive-print"

    "-package-name"
    "-package"
    "-hide-all-packages"
    "-hide-package"
    "-ignore-package"
    ("-package-db" (pcomplete-here (pcmpl-entries-ignoring-common)))
    "-clear-package-db"
    "-no-global-package-db"
    "-global-package-db"
    "-no-user-package-db"
    "-user-package-db"
    "-no-auto-link-packages"
    "-trust"
    "-distrust"
    "-distrust-all"

    "-W"
    "-w"
    "-Wall"
    "-Werror"
    "-Wwarn"
    "-fdefer-type-errors"
    "-fhelpful-errors"
    "-fwarn-deprecated-flags"
    "-fwarn-duplicate-exports"
    "-fwarn-hi-shadowing"
    "-fwarn-identities"
    "-fwarn-implicit-prelude"
    "-fwarn-incomplete-patterns"
    "-fwarn-incomplete-uni-patterns"
    "-fwarn-incomplete-record-updates"
    "-fwarn-lazy-unlifted-bindings"
    "-fwarn-missing-fields"
    "-fwarn-missing-import-lists"
    "-fwarn-missing-methods"
    "-fwarn-missing-signatures"
    "-fwarn-missing-local-sigs"
    "-fwarn-monomorphism-restriction"
    "-fwarn-name-shadowing"
    "-fwarn-orphans"
    "-fwarn-auto-orphans"
    "-fwarn-overlapping-patterns"
    "-fwarn-tabs"
    "-fwarn-type-defaults"
    "-fwarn-unrecognised-pragmas"
    "-fwarn-unused-binds"
    "-fwarn-unused-imports"
    "-fwarn-unused-matches"
    "-fwarn-unused-do-bind"
    "-fwarn-wrong-do-bind"
    "-fwarn-unsafe"
    "-fwarn-safe"
    "-fwarn-warnings-deprecations"

    "-fno-defer-type-errors"
    "-fno-helpful-errors"
    "-fno-warn-deprecated-flags"
    "-fno-warn-duplicate-exports"
    "-fno-warn-hi-shadowing"
    "-fno-warn-identities"
    "-fno-warn-implicit-prelude"
    "-fno-warn-incomplete-patterns"
    "-fno-warn-incomplete-uni-patterns"
    "-fno-warn-incomplete-record-updates"
    "-fno-warn-lazy-unlifted-bindings"
    "-fno-warn-missing-fields"
    "-fno-warn-missing-import-lists"
    "-fno-warn-missing-methods"
    "-fno-warn-missing-signatures"
    "-fno-warn-missing-local-sigs"
    "-fno-warn-monomorphism-restriction"
    "-fno-warn-name-shadowing"
    "-fno-warn-orphans"
    "-fnowarn-auto-orphans"
    "-fno-warn-overlapping-patterns"
    "-fno-warn-tabs"
    "-fno-warn-type-defaults"
    "-fno-warn-unrecognised-pragmas"
    "-fno-warn-unused-binds"
    "-fno-warn-unused-imports"
    "-fno-warn-unused-matches"
    "-fno-warn-unused-do-bind"
    "-fno-warn-wrong-do-bind"
    "-fno-warn-unsafe"
    "-fno-warn-safe"
    "-fno-warn-warnings-deprecations"

    "-O"
    "-O0"
    "-O2"

    "-prof"
    "-fprof-auto"
    "-fno-prof-auto"
    "-fprof-auto-top"
    "-fno-prof-auto"
    "-fprof-auto-exported"
    "-fno-prof-auto"
    "-fprof-cafs"
    "-fno-prof-cafs"
    "-fno-prof-count-entries"
    "-fprof-count-entries"
    "-ticky"


    "-fhpc"
    ("-hpcdir" (pcomplete-here (pcomplete-dirs)))

    "-F"

    "-cpp"
    "-D"
    "-U"
    ("-I" (pcomplete-here (pcomplete-dirs)))

    "-fasm"
    "-fllvm"
    "-fno-code"
    "-fbyte-code"
    "-fobject-code"

    "-shared"
    "-fPIC"
    "-dynamic"
    "-dynload"
    "-framework"
    "-framework-path"
    "-l"
    ("-L" (pcomplete-here (pcomplete-dirs)))
    "-main-is"
    "--mk-dll"
    "-no-hs-main"
    "-rtsopts"
    "-rtsopts=none"
    "-rtsopts=some"
    "-rtsopts=all"
    "-with-rtsopts"
    "-no-link"
    "-split-objs"
    "-static"
    "-threaded"
    "-debug"
    "-eventlog"
    "-fno-gen-manifest"
    "-fno-embed-manifest"
    "-fno-shared-implib"
    ("-dylib-install-name" (pcomplete-here (pcomplete-dirs)))

    "-fplugin"
    "-fplugin-opt"

    "-pgmL"
    "-pgmP"
    "-pgmc"
    "-pgms"
    "-pgma"
    "-pgml"
    "-pgmdll"
    "-pgmF"
    "-pgmwindres"

    "-optL"
    "-optP"
    "-optF"
    "-optc"
    "-optlo"
    "-optlc"
    "-optm"
    "-opta"
    "-optl"
    "-optdll"
    "-optwindres"

    "-msse2"
    "-monly-2-regs"
    "-monly-3-regs"
    "-monly-4-regs"

    "-dcore-lint"
    "-ddump-to-file"
    "-ddump-asm"
    "-ddump-bcos"
    "-ddump-cmm"
    "-ddump-core-stats"
    "-ddump-cpranal"
    "-ddump-cse"
    "-ddump-deriv"
    "-ddump-ds"
    "-ddump-flatC"
    "-ddump-foreign"
    "-ddump-hpc"
    "-ddump-inlinings"
    "-ddump-llvm"
    "-ddump-occur-anal"
    "-ddump-opt-cmm"
    "-ddump-parsed"
    "-ddump-prep"
    "-ddump-rn"
    "-ddump-rule-firings"
    "-ddump-rule-rewrites"
    "-ddump-rules"
    "-ddump-vect"
    "-ddump-simpl"
    "-ddump-simpl-phases"
    "-ddump-simpl-iterations"
    "-ddump-spec"
    "-ddump-splices"
    "-ddump-stg"
    "-ddump-stranal"
    "-ddump-tc"
    "-ddump-types"
    "-ddump-worker-wrapper"
    "-ddump-if-trace"
    "-ddump-tc-trace"
    "-ddump-vt-trace"
    "-ddump-rn-trace"
    "-ddump-rn-stats"
    "-ddump-simpl-stats"
    "-dno-debug-output"
    "-dppr-debug"
    "-dppr-noprags"
    "-dppr-user-length"
    "-dppr-colsNNN"
    "-dppr-case-as-let"
    "-dsuppress-all"
    "-dsuppress-uniques"
    "-dsuppress-idinfo"
    "-dsuppress-module-prefixes"
    "-dsuppress-type-signatures"
    "-dsuppress-type-applications"
    "-dsuppress-coercions"
    "-dsource-stats"
    "-dcmm-lint"
    "-dstg-lint"
    "-dstg-stats"
    "-dverbose-core2core"
    "-dverbose-stg2stg"
    "-dshow-passes"
    "-dfaststring-stats"
    "-dth-dec-file"

    "-fno-hi-version-check"
    "-dno-black-holing"
    "-fhistory-size"
    "-funregisterised"
    "-fno-ghci-history"
    "-fno-ghci-sandbox"))

;;;###autoload
(defpcmpl pcomplete/ghc
  `(opts
    (flags ,@pcomplete-ghc-flags)
    (args (pcomplete-here (pcmpl-haskell-source-or-obj-files))))
  :evaluate-definition t)

;;;###autoload
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
                            ("--prefix" (pcomplete-here (pcomplete-dirs)))
                            ("--bindir" (pcomplete-here (pcomplete-dirs)))
                            ("--libdir" (pcomplete-here (pcomplete-dirs)))
                            ("--libsubdir" (pcomplete-here (pcomplete-dirs)))
                            ("--libexecdir" (pcomplete-here (pcomplete-dirs)))
                            ("--datadir" (pcomplete-here (pcomplete-dirs)))
                            ("--datasubdir" (pcomplete-here (pcomplete-dirs)))
                            ("--docdir" (pcomplete-here (pcomplete-dirs)))
                            ("--htmldir" (pcomplete-here (pcomplete-dirs)))
                            ("--haddockdir" (pcomplete-here (pcomplete-dirs)))
                            ("--sysconfdir" (pcomplete-here (pcomplete-dirs)))
                            ("-b" (pcomplete-here (pcomplete-dirs)))
                            ("--scratchdir" (pcomplete-here (pcomplete-dirs)))
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
                            ,@(when (cabal-install-version-at-least? 1 22 0 0)
                                '("--enable-profiling"))
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
                            ("--extra-include-dirs" (pcomplete-here (pcomplete-dirs)))
                            ("--extra-lib-dirs" (pcomplete-here (pcomplete-dirs)))
                            ("--extra-prog-path" (pcomplete-here (pcomplete-dirs)))
                            "--enable-tests"
                            "--disable-tests"
                            "--enable-library-coverage"
                            "--disable-library-coverage"
                            "--enable-benchmarks"
                            "--disable-benchmarks"
                            ,@(concatMap (lambda (p)
                                           `((,(concat "--" p)
                                              (pcomplete-here (pcmpl-entries-ignoring-common)))
                                             ,(concat "--" p "-option")
                                             ,(concat "--" p "-options")))
                                         programs)
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
               ("--haddock-css" (pcomplete-here (pcmpl-entries-ignoring-common)))
               "--haddock-hyperlink-source"
               ("--haddock-hscolour-css" (pcomplete-here (pcmpl-entries-ignoring-common)))
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
               ("-d" (pcomplete-here (pcomplete-dirs)))
               ("--destdir" (pcomplete-here (pcomplete-dirs)))
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
      ("run")
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
               ,@(concatMap (lambda (p)
                              `((,(concat "--with-" p)
                                 (pcomplete-here (pcmpl-entries-ignoring-common)))
                                ,(concat "--" p "-option")
                                ,(concat "--" p "-options")))
                            programs))))
      ("repl")
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
      ("clean")
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

;;;###autoload
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
   (args (pcomplete-here (pcomplete-entries ".*\\.hp\\'")))))

;;; C, low-level stuff

;;;###autoload
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

;;;###autoload
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

          ("-I" (pcomplete-here (pcomplete-dirs)))
          ("-L" (pcomplete-here (pcomplete-dirs)))
          "-Os"
          "-O2"
          "-O3"
          "-marh"
          "-march=native"
          "-fomit-frame-pontier")
   (args (pcomplete-here (pcomplete-entries)))))

;;; simpler definitions, vanilla Unix & GNU tools

;;;###autoload
(defpcmpl pcomplete/cp
  (opts (flags "-r")
        (args (pcomplete-here (pcomplete-entries)))))

;;;###autoload
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

;;;###autoload
(defpcmpl pcomplete/cat
  (opts (flags "--help")
        (args (pcomplete-here (pcmpl-entries-ignoring-common)))))

;;;###autoload
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

;;;###autoload
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
             ("--init-file" (pcomplete-here (pcmpl-entries-ignoring-common)))
             ("--rcfile" (pcomplete-here (pcmpl-entries-ignoring-common)))
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

;;;###autoload
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
          ("--exclude-from" (pcomplete-here (pcmpl-entries-ignoring-common)))
          "-S"
          ("--starting-file" (pcomplete-here (pcmpl-entries-ignoring-common)))
          ("--from-file" (pcomplete-here (pcmpl-entries-ignoring-common)))
          ("--to-file" (pcomplete-here (pcmpl-entries-ignoring-common)))
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

;;;###autoload
(defpcmpl pcomplete/source
  (opts (args (pcomplete-here (pcmpl-entries-ignoring-common)))))

;;;###autoload
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

;;;###autoload
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

(provide 'shell-completion)

;; Local Variables:
;; End:

;; shell-completion.el ends here
