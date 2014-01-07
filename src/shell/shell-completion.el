;; shell-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  4 December 2012
;; Description:

(require 'pcomplete)

(eval-after-load "pcomplete"
  '(progn
     (require 'pcmpl-gnu)
     (require 'pcmpl-linux)
     (require 'pcmpl-rpm)
     (require 'pcmpl-unix)

     (setf pcomplete-autolist nil
           pcomplete-recexact nil
           pcomplete-cycle-completions t)))

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
      (let ((re (concat "^refs/" type "/\\(.*\\)")))
        (delq nil
              (map (lambda (ref)
                     (when (string-match? re ref)
                       (match-string-no-properties 1 ref)))
                   (split-string (buffer-substring-no-properties (point-min)
                                                                 (point-max))
                                 "\n"
                                 t)))))))

(defvar shell-completion--git-commands
  '("add"
    "bisect"
    "branch"
    "checkout"
    "clone"
    "commit"
    "diff"
    "fetch"
    "grep"
    "init"
    "log"
    "merge"
    "mv"
    "pull"
    "push"
    "rebase"
    "reset"
    "rm"
    "show"
    "status"
    "tag"))

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

(defmacro defpcmpl (name definition)
  (declare (indent 1))
  (assert (string-match-pure? "pcomplete/" (symbol->string name)))
  (let ((got-end-of-flags-var (gensym "got-end-of-flags"))
        (last-arg-is-single-dash-var (gensym "last-arg-is-single-dash?"))
        (last-arg-is-two-dashes-var (gensym "last-arg-is-two-dashes?")))
    (letrec ((process
              (lambda (definition level)
                (assert (and (list? definition)
                             (not (null? definition))))
                (cond ((eq? 'or (first definition))
                       (funcall process-or definition level))
                      ((eq? 'opts (first definition))
                       (funcall process-opts definition))
                      (else
                       (error "unsupported definition: %s" definition)))))
             (process-or
              (lambda (definition level)
                (when-let (defs (rest definition))
                  (let* ((positional-defs
                          (filter (comp #'string? #'first) defs))
                         (other-defs
                          (filter (comp #'not #'string? #'first) defs))
                         (names
                          (map #'first positional-defs)))
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
                       (short (cdr-safe (assoc 'short info)))
                       (long (cdr-safe (assoc 'long info)))
                       (end (cadr-safe (assoc 'end info))))
                  (assert (all? (comp (partial string-match-pure? "^-[^-].*"))
                                short)
                          nil
                          "All short options must start with single dash")
                  (assert (all? (comp (partial string-match-pure? "^--[^-].*"))
                                long)
                          nil
                          "All long options must start with two dashes")
                  (when (or (not (null? short))
                            (not (null? long))
                            (not (null? end)))
                    `(while
                         (cond
                           ,@(when (not (null? short))
                               (list
                                `((and (not ,got-end-of-flags-var)
                                       ,last-arg-is-single-dash-var)
                                  (pcomplete-here '(,@short
                                                    ;; ,@long
                                                    )))))
                           ,@(when (not (null? long))
                               (list
                                `((and (not ,got-end-of-flags-var)
                                       ,last-arg-is-two-dashes-var)
                                  (pcomplete-here '(,@long)))))
                           ,@(when (not (null? end))
                               (list `(t
                                       ,end)))))))))
             (process-positional
              (lambda (definition level)
                (assert (string? (first definition)))
                (when (not (null? (cadr-safe definition)))
                  (let ((name (first definition)))
                    `((string= (pcomplete-arg ,level) ,name)
                      ,(funcall process
                                (cadr definition)
                                (+ level 1))))))))
      `(defun ,name ()
         (let* ((,last-arg-is-single-dash-var
                 (string= "-" (pcomplete-arg 'last)))
                (,last-arg-is-two-dashes-var
                 (string= "--" (pcomplete-arg 'last)))
                (,got-end-of-flags-var
                 (and (not ,last-arg-is-two-dashes-var)
                      (member "--" pcomplete-args))))
           ,(funcall process definition 1))))))


(defpcmpl pcomplete/git
  (or ("add"
       (opts
        (end (pcomplete-here (pcomplete-entries)))))
      ("bisect"
       (or ("good")
           ("bad")
           ("start")))
      ("branch"
       (opts
        (short "-d" "-D")
        (long "--list")
        (end (pcomplete-here (pcmpl-git-get-refs "heads")))))
      ("checkout"
       (opts
        (end (pcomplete-here (pcmpl-git-get-refs "heads")))))
      ("clone"
       (opts
        (short "-l"
               "-n"
               "-s"
               "-o"
               "-b"
               "-u"
               "-c")
        (long "--bare"
              "--mirror"
              "--local"
              "--progress"
              "--no-checkout"
              "--no-hardlinks"
              "--shared"
              "--recursive"
              "--recursive-submodules"
              "--template"
              "--reference"
              "--origin"
              "--branch"
              "--upload-pack"
              "--depth"
              "--single-branch"
              "--separate-git-dir"
              "--config")))
      ("commit"
       (opts
        (short "-F"
               "-m"
               "-c"
               "-C"
               "-s"
               "-t"
               "-e"
               "-S"

               "-a"
               "-i"
               "-p"
               "-o"
               "-n"
               "-z"
               "-u")
        (long "--file"
              "--author"
              "--date"
              "--message"
              "--reedit-message"
              "--reuse-message"
              "--fixup"
              "--squash"
              "--reset-author"
              "--signoff"
              "--template"
              "--edit"
              "--cleanup"
              "--status"
              "--gpg-sign"

              "--all"
              "--include"
              "--interactive"
              "--patch"
              "--only"
              "--no-verify"
              "--dry-run"
              "--short"
              "--branch"

              ;; "--porcelain"
              "--long"
              "--null"
              "--amend"
              "--no-post-rewrite"
              "--untracked-files")
        (end (pcomplete-here (pcomplete-entries)))))
      ("diff"
       (opts
        (short "-p"
               "-u"
               "-s"
               "-U"
               "-z"
               "-B"
               "-M"
               "-C"
               "-D"
               "-l"
               "-S"
               "-G"
               "-O"
               "-R"
               "-a"
               "-b"
               "-w"
               "-W")
        (long "--no-index"
              "--cached"
              "--patch"
              "--no-patch"
              "--unified"
              "--raw"
              "--minimal"
              "--patience"
              "--histogram"
              "--diff-algorithm"
              "--stat"
              "--numstat"
              "--shortstat"
              "--dirstat"
              "--summary"
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
              "--break-rewrites"
              "--find-renames"
              "--find-copies"
              "--find-copies-harder"
              "--irreversible-delete"
              "--diff-filter"
              "--pickaxe-all"
              "--pickaxe-regex"
              "--relative"
              "--text"
              "--ignore-space-at-eol"
              "--ignore-space-change"
              "--ignore-all-space"
              "--ignore-blank-lines"
              "--inter-hunk-context"
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
        (end (pcomplete-here (pcomplete-entries)))))
      ("fetch"
       (opts
        (short "-a"
               "-f"
               "-t"
               "-n"
               "-p"
               "-k"
               "-u")
        (long "--all"
              "--multiple"
              "--append"
              "--fonce"
              "--tags"
              "--no-tags"
              "--prune"
              "--recurse-submodules"
              "--dry-run"
              "--keep"
              "--update-head-ok"
              "--progress"
              "--depth"
              "--unshallow")))
      ("grep"
       (opts
        (short "-v"
               "-i"
               "-w"
               "-a"
               "-I"
               "-E"
               "-G"
               "-F"
               "-p"
               "-n"
               "-h"
               "-H"
               "-l"
               "-L"
               "-z"
               "-c"
               "-C"
               "-B"
               "-A"
               "-p"
               "-W"
               "-f"
               "-e"
               "-O"
               "-q")
        (long "--cached"
              "--no-index"
              "--untracked"
              "--exclude-standard"
              "--invert-match"
              "--ignore-case"
              "--word-regexp"
              "--text"
              "--textconv"
              "--max-depth"
              "--extended-regexp"
              "--basic-regexp"
              "--fixed-strings"
              "--perl-regexp"
              "--line-number"
              "--full-name"
              "--files-with-matches"
              "--name-only"
              "--files-without-match"
              "--null"
              "--count"
              "--break"
              "--heading"
              "--context"
              "--before-context"
              "--after-context"
              "--show-function"
              "--function-context"
              "--and"
              "--or"
              "--not"
              "--all-match"
              "--ext-grep"
              "--quiet")
        (end (pcomplete-here (pcomplete-entries)))))
      ("init"
       (opts
        (long "--bare"
              "--shared"
              "--separate-git-dir")
        (end (pcomplete-here (pcomplete-entries)))))
      ("log"
       (opts
        (short "-q"
               "-L")
        (long "--quiet"
              "--source"
              "--use-mailmap"
              "--decorate")
        (end (pcomplete-here (pcomplete-entries)))))
      ("merge")
      ("mv"
       (opts
        (end (pcomplete-here (pcomplete-entries)))))
      ("p4"
       (or ("submit")
           ("rebase")
           ("clone")))
      ("pull")
      ("push")
      ("rebase"
       (opts
        (short "-i"
               "-x")
        (long "--interactive"
              "--exec"
              "--no-ff")
        (end (pcomplete-here (pcmpl-git-get-refs "heads")))))
      ("reset"
       (opts
        (long "--mixed"
              "--soft"
              "--hard"
              "--merge"
              "--keep")
        (end (pcomplete-here (pcomplete-entries)))))
      ("rm"
       (opts
        (short "-f"
               "-r"
               "-n")
        (long "--cached"
              "--dry-run"
              "--force")
        (end (pcomplete-here (pcomplete-entries)))))
      ("show"
       (opts
        (short "-q"
               "-L")
        (long "--quiet"
              "--source"
              "--use-mailmap"
              "--decorate")
        (end (pcomplete-here (pcomplete-entries)))))
      ("status")
      ("tag"
       (opts
        (end (pcomplete-here (pcmpl-git-get-refs "tags")))))))

(defpcmpl pcomplete/nm
  (opts (short "-a"
               "-C"
               "-D"
               "-f"
               "-g"
               "-l"
               "-n"
               "-o"
               "-p"
               "-P"
               "-r"
               "-S"
               "-s"
               "-t"
               "-u")
        (long "--debug-syms"
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
              "--dynamic"
              "--format"
              "--format=bsd"
              "--format=sysv"
              "--format=posix"
              "--extern-only"
              "--line-numbers"
              "--numeric-sort"
              "--no-sort"
              "--portability"
              "--reverse-sort"
              "--pulgin"
              "--print-size"
              "--print-armap"
              "--size-sort"
              "--special-syms"
              "--synthetic"
              "--radix"
              "--target"
              "--undefined-only")
        (end (pcomplete-here (pcomplete-entries)))))

(defpcmpl pcomplete/cp
  (opts (short "-r")
        (end (pcomplete-here (pcomplete-entries)))))

;; hand-crafted git completion prototype
;; (defun pcomplete/git ()
;;   "Completion for git."
;;   (pcomplete-here shell-completion--git-commands)
;;   (cond ((pcomplete-match (rx (or "add" "mv" "rm")) 1)
;;          (while (pcomplete-here (pcomplete-entries))))
;;         ((string= (pcomplete-arg 1) "branch")
;;          ;; (pcomplete-match (rx "branch") 1)
;;          (message "(pcomplete-argi 'last) = %s"
;;                   (pp-to-string (pcomplete-arg 'last)))
;;          (while
;;              (cond ((string= (pcomplete-arg) "-")
;;                     (pcomplete-here '("-a"
;;                                       "-r"
;;                                       )))
;;                    ((string= (pcomplete-arg) "--")
;;                     (pcomplete-here '("--list"
;;                                       "--track")))))
;;          (pcomplete-here* (pcmpl-git-get-refs "heads")))
;;         ((pcomplete-match (rx "checkout") 1)
;;          (pcomplete-here* (pcmpl-git-get-refs "heads")))
;;         ((pcomplete-match (rx "log") 1)
;;          (while (pcomplete-here (pcomplete-entries))))
;;         ((pcomplete-match (rx "tag") 1)
;;          (pcomplete-here* (pcmpl-git-get-refs "tags")))))


(defun pcomplete/ls ()
  "Completion for ls."
  (let ((pcomplete-help "helpful message"))
    (while t (pcomplete-here
              (funcall (lambda (f)
                         (lambda (string pred action)
                           (remove-if (lambda (x)
                                        (member x '("." ".." "./" "../")))
                                      (funcall f string pred action))))
                       (pcomplete-entries))))))

(defun pcomplete/cat ()
  "Completion for cat"
  (while t
    (let ((pcomplete-help (identity "helpful message")))
      (pcomplete-here
       (funcall (lambda (f)
                  (let ((func f))
                    (lambda (string pred action)
                      (remove-if (lambda (x)
                                   (member x '("." ".." "./" "../")))
                                 (funcall func string pred action)))))
                (pcomplete-entries))))))

(provide 'shell-completion)

;; Local Variables:
;; End:

;; shell-completion.el ends here
