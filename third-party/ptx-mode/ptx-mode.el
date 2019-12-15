;; ptx-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 23 August 2018
;; Description:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Commentary:
;; This mode is based on PTX extension for Atom https://github.com/H4E/language-ptx.
;;
;; Quick setup:
;; (add-to-list 'auto-mode-alist '("\\.ptx\\'" . ptx-mode))

(eval-when-compile (require 'cl-lib))

(defmacro ptx-mode--rxx (definitions &rest main-expr)
  "Return `rx' invokation of main-expr that has symbols defined in
DEFINITIONS substituted by definition body. DEFINITIONS is list
of let-bindig forms, (<symbol> <body>). No recursion is permitted -
no defined symbol should show up in body of its definition or in
body of any futher definition."
  (declare (indent 1))
  (let ((def (cl-find-if (lambda (def) (not (= 2 (length def)))) definitions)))
    (when def
      (error "ptx-mode--rxx: every definition should consist of two elements: (name def), offending definition: %s"
             def)))
  `(rx ,@(cl-reduce (lambda (def expr)
                      (cl-subst (cadr def) (car def) expr
                                :test #'eq))
                    definitions
                    :initial-value main-expr
                    :from-end t)))

(defvar ptx-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?/  ". 12" tbl)
    (modify-syntax-entry ?\n ">"    tbl)

    (modify-syntax-entry ?\. "."    tbl)
    (modify-syntax-entry ?\_ "w"    tbl)

    (modify-syntax-entry ?\s "-"    tbl)

    (modify-syntax-entry ?\( "()  " tbl)
    (modify-syntax-entry ?\) ")(  " tbl)
    (modify-syntax-entry ?\{ "(}  " tbl)
    (modify-syntax-entry ?\} "){  " tbl)
    (modify-syntax-entry ?\[ "(]  " tbl)
    (modify-syntax-entry ?\] ")[  " tbl)

    tbl)
  "Syntax table in use in happy-mode buffers.")

(defvar ptx-mode-font-lock-keywords
  `((,(ptx-mode--rxx
          ((hex-num (any (?0 . ?9) (?a . ?f) (?A . ?F))))
        (or (seq (? (any ?\+ ?\-))
                 bow
                 (+ (any (?0 . ?9)))
                 (? "."
                    (* (any (?0 . ?9)))))
            (seq "."
                 (* (any (?0 . ?9))))
            (seq bow
                 (or (seq "0"
                          (or (seq (any ?x ?X)
                                   (+ hex-num))
                              (seq (any ?o ?O)
                                   (+ (any (?0 . ?7))))
                              (seq (any ?b ?B)
                                   (+ (any (?0 . ?1))))
                              ))
                     (seq (any (?1 . ?9))
                          (* (any (?0 . ?9)))))
                 (? "U"))
            (seq bow
                 "0"
                 (or (seq (any ?f ?F)
                          (repeat 8 hex-num))
                     (seq (any ?d ?D)
                          (repeat 16 hex-num)))))
        eow)
     (0 'font-lock-constant-face))

    (,(rx bow
          (or
           "WARP_SZ"
           (seq "%"
                (or "clock"
                    "clock64"
                    "ctaid"
                    "gridid"
                    "laneid"
                    "nctaid"
                    "nsmid"
                    "ntid"
                    "nwarpid"
                    "smid"
                    "tid"
                    "warpid"))
           (seq "%lanemask_"
                (or "eq" "ge" "gt" "le" "lt"))
           (seq "%envreg<"
                (or (seq (any (?1 . ?2))
                         (any (?0 . ?9)))
                    (seq "3"
                         (any (?0 . ?1)))
                    (any (?0 . ?9)))
                ">")
           (seq "pm"
                (any (?0 . ?7))))
          eow)
     (0 'font-lock-builtin-face))

    (,(rx bol
          (* (syntax whitespace))
          (or "$"
              "%"
              (or (syntax word)
                  (syntax symbol)))
          (* (or "$"
                 (syntax word)
                 (syntax symbol)))
          ":")
     (0 'font-lock-preprocessor-face))

    (,(rx "."
          (or (seq (any ?u ?f ?b ?s)
                   (or "2"
                       "4"
                       "8"
                       "16"
                       "32"
                       "64"))
              "f16x2"
              "pred"
              (seq "v"
                   (any ?2 ?4))))
     (0 'font-lock-type-face))

    (,(ptx-mode--rxx
          ((memory-space (or ".const"
                             ".global"
                             ".local"
                             ".param"
                             ".shared")))
        bow
        (or (seq (or "ld"
                     "st")
                 (? "."
                    (or "weak"
                        "volatile"
                        "relaxed.scope"
                        "acquire.scope"))
                 (? memory-space)
                 (? "."
                    (or "ca"
                        "cg"
                        "cs"
                        "lu"
                        "cv")))
            (seq "prefetch"
                 (? (or ".global"
                        ".local"))
                 (or ".L1" ".L2"))
            "prefetchu.L1"
            (seq (or "set"
                     "setp")
                 "."
                 (or "eq"
                     "ne"
                     "lt"
                     "gt"
                     "ge"
                     "lo"
                     "ls"
                     "hi"
                     "hs"
                     "equ"
                     "neu"
                     "ltu"
                     "leu"
                     "gtu"
                     "geu"
                     "num"
                     "nan")
                 (? "."
                    (or "and"
                        "or"
                        "xor"))
                 (? ".ftz"))
            (seq (or (seq "cvta"
                          (? ".to"))
                     "isspacep")
                 memory-space)
            (seq "shf"
                 (? (or ".l"
                        ".r"))
                 (? (or ".clamp"
                        ".wrap")))
            (seq "shfl"
                 (? ".sync")
                 (or ".up" ".down" ".bfly" ".idx"))
            (seq (or (or "abs"
                         "add"
                         "addc"
                         "and"
                         "atom"
                         "bar"
                         "bfe"
                         "bfi"
                         "bfind"
                         "bra"
                         "brev"
                         "brkpt"
                         "call"
                         "clz"
                         "cnot"
                         "copysign"
                         "cos"
                         "cvt"
                         "div"
                         "ex2"
                         "exit"
                         "fma"
                         "ldu"
                         "lg2"
                         "mad"
                         "madc"
                         "max"
                         "membar"
                         "min"
                         "mov"
                         "mul"
                         "neg"
                         "not"
                         "or"
                         "pmevent"
                         "popc"
                         "prmt"
                         "rcp"
                         "red"
                         "rem"
                         "ret"
                         "rsqrt"
                         "sad"
                         "selp"
                         "shl"
                         "shr"
                         "sin"
                         "slct"
                         "sqrt"
                         "sub"
                         "subc"
                         "suld"
                         "suq"
                         "sured"
                         "sust"
                         "testp"
                         "tex"
                         "tld4"
                         "trap"
                         "txq"
                         "vabsdiff"
                         "vmad"
                         "vmax"
                         "vmin"
                         "vote"
                         "vset"
                         "vshl"
                         "vshr"
                         "vsub"
                         "xor")
                     (seq (or "vavrg"
                              "vmax"
                              "vmin"
                              "mad"
                              "vset"
                              "vsub"
                              "mul"
                              "vabsdiff"
                              "vadd")
                          "24"))
                 (? (or ".lo"
                        ".hi"
                        ".wide"))))
        eow)
     (0 'font-lock-keyword-face))

    (,(rx "."
          (or "address_size"
              "align"
              "branchtarget"
              "branchtargets"
              "callprototype"
              "calltargets"
              "const"
              "entry"
              "extern"
              "file"
              "func"
              "global"
              "loc"
              "local"
              "maxnctapersm"
              "maxnreg"
              "maxntid"
              "minnctapersm"
              "param"
              "pragma"
              "reg"
              "reqntid"
              "section"
              "shared"
              "sreg"
              "target"
              "tex"
              "version"
              "visible"
              "weak"))
     (0 'font-lock-builtin-face))

    ;; Special directives
    (,(rx "."
          (or "attribute"
              "managed"
              "ptr"
              "relaxed"
              "sys"

              "approx"
              "ftz"
              "rnd"
              "sat"))
     (0 'font-lock-builtin-face))

    ;; Rounding modes
    (,(rx "."
          (or "rn"
              "rz"
              "rm"
              "rp")
          (? "i"))
     (0 'font-lock-negation-char-face))

    (,(rx (seq ".entry"
               (* (syntax whitespace))
               (group
                (+ (syntax word)))
               (* (syntax whitespace))
               "("))
     (1 'font-lock-function-name-face))

    ;; Registers
    (,(rx "%"
          (+ (or (syntax word)
                 (syntax symbol)
                 (any (?0 . ?9))))
          ;; (? "<"
          ;;    (+ (any (?0 . ?9)))
          ;;    ">")
          )
     (0 'font-lock-variable-name-face))

    ;; Operators
    (,(rx (or "+"
              "-"
              "!"
              "~"
              "*"
              "/"
              "%"
              "<<"
              ">>"
              "<"
              "<="
              ">"
              ">="
              "=="
              "!="
              "&"
              "^"
              "|"
              "&&"
              "||"
              "?:")
          )
     (0 'font-lock-variable-name-face)))
  "Highlight definitions of PTX constructs for font-lock.")

;;;###autoload
(define-derived-mode ptx-mode prog-mode "PTX"
  "Major mode for editing PTX files."
  (set (make-local-variable 'font-lock-defaults)
       '(ptx-mode-font-lock-keywords
         nil ;; perform syntactic fontification
         nil ;; do not ignore case
         nil ;; no special syntax provided
         ))

  (setq-local require-final-newline t)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-column 32)
  (setq-local comment-start-skip "//+ *")
  (setq-local parse-sexp-ignore-comments t)
  (make-local-variable 'block-indent-level)
  (make-local-variable 'auto-fill-hook))

(provide 'ptx-mode)

;; Local Variables:
;; End:

;; ptx-mode.el ends here
