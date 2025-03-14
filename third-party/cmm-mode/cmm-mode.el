;;; cmm-mode.el --- Major mode for C-- source code

;; Copyright (c) 2012, Johan Tibell
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(defconst cmm-font-lock-defaults
  `((,(rx bow
          (or "bits8" "bits16" "bits32" "bits64" "float32" "float64" "I8" "I16" "I32"
              "CInt" "CLong" "I64" "CInt" "CLong" "L_" "F_" "D_")
          eow)
     . font-lock-type-face)
    (,(rx bow
          (or "aborts" "align" "aligned" "also" "as" "big" "bits" "byteorder" "case"
              "const," "continuation" "cut" "cuts" "else" "equal" "export" "foreign"
              "goto" "if" "import" "in," "invariant" "invisible" "jump" "little" "memsize"
              "pragma" "reads" "register," "return" "returns" "section" "semi" "span"
              "stackdata" "switch" "target" "targets" "to," "typedef" "unicode" "unwinds"
              "writes")
          eow)
     . font-lock-keyword-face)))

(defvar cmm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for cmm-mode")

;;;###autoload
(define-derived-mode cmm-mode prog-mode
  "Cmm"
  "A major mode for editing Cmm files."

  ;; code for syntax highlighting
  (setq-local comment-start "// "
              comment-start-skip "//+ *"
              font-lock-defaults '(cmm-font-lock-defaults
                                   nil ;; perform syntactic fontification (e.g. strings, comments)
                                   nil ;; do not ignore case
                                   nil ;; no special syntax provided
                                   )))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "." (? "dump-") "cmm") #'cmm-mode))

(provide 'cmm-mode)

;;; cmm-mode.el ends here
