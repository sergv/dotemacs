;; common-font.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 30 January 2025
;; Description:

(defvar current-font)

(defun pretty-ligatures-supported? ()
  (and (bound-and-true-p current-font)
       (cond
         ((stringp current-font)
          (string-match-p "Iosevka Slab Lig" current-font))
         ((fontp current-font)
          (string-match-p "Iosevka Slab Lig" (font-get current-font :name)))
         (t
          (error "Invalid current font: %s" current-font)))))

(defvar default-composition-function-table nil)
(defvar texture-healed-composition-function-table nil)

(defun common-font--init-function-tables-after-init ()
  (setf default-composition-function-table composition-function-table)

  (setf texture-healed-composition-function-table
        (let ((table (make-char-table nil)))
          (set-char-table-parent table composition-function-table)
          ;; Compose ASCII with harfbuzz to take advantage of the TXTR feature (requires custom
          ;; emacs build as well).
          ;; See also:
          ;; ! https://github.com/mickeynp/ligature.el/issues/53#issuecomment-1828732077
          ;; https://github.com/githubnext/monaspace/issues/12
          ;; https://github.com/harfbuzz/harfbuzz/discussions/4490
          (let ((composition-spec
                 `([
                    ;; Take care to make regexp match only whole words delimited by punctuation or
                    ;; whitespace.
                    ;;
                    ;; Matching multiple words is bad here because harfbuzz compositor
                    ;; will fontify them with the same font. For example, wher first character
                    ;; of the text to be composited has bold property then whole range
                    ;; will get composited with the bold face. However, only first word
                    ;; may have been using bold property while others didn’t. But due to
                    ;; current Emacs implementation, harfbuzz has to use the same font
                    ;; for whole block. The fix is to tweak regexp to make the block
                    ;; smaller.
                    ,(rx (or bol
                             (any ?\s ?\t ?\! ?\" ?\' ?\( ?\) ?\, ?\- ?\. ?\: ?\; ?\< ?\> ?\? ?\@ ?\[ ?\] ?\_ ?\{ ?\| ?\}))
                         (or bow symbol-start)
                         (+ (any
                             ?\- ?\. ?\_
                             (?a . ?z)
                             (?A . ?Z)))
                         (or eow symbol-end)
                         (or (any ?\s ?\t ?\! ?\" ?\' ?\( ?\) ?\, ?\- ?\. ?\: ?\; ?\< ?\> ?\? ?\@ ?\[ ?\] ?\_ ?\{ ?\| ?\}
                                  ;; Somehow newline does not work as space does and
                                  ;; does not lead to glyph enlargement. No ideas why yet.
                                  ?\r ?\n)
                             eol))
                    0
                    font-shape-gstring
                    ])))
            (dolist (c '(?\s
                         ?\! ?\" ?\' ?\( ?\) ?\, ?\- ?\. ?\: ?\; ?\< ?\> ?\? ?\@ ?\[ ?\] ?\_ ?\{ ?\| ?\}
                         (?a . ?z)
                         (?A . ?Z)))
              (set-char-table-range table c composition-spec))
            table)))

  (setf composition-function-table texture-healed-composition-function-table))

(provide 'common-font)

;; Local Variables:
;; End:

;; common-font.el ends here
