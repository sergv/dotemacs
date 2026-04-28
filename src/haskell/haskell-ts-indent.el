;; haskell-ts-indent.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 November 2024
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'dash)
  (require 'macro-util))

(defvar haskell-indent-offset)

(require 'common)
(require 'common-whitespace)
(require 'haskell-lexeme)
(require 'haskell-ts-getters)
(require 'treesit)
(require 'treesit-utils)

(defconst haskell-ts--treesit-simple-indent-presets
  (append
   (list (cons 'match
               (lambda
                 (&optional node-type parent-type node-field
                            node-index-min node-index-max)
                 (lambda (node parent _)
                   (and (pcase node-type
                          ('nil t)
                          ('null (null node))
                          (_ (awhen (treesit-node-type node)
                               (string= node-type it))))
                        (or (null parent-type)
                            (awhen (treesit-node-type parent)
                              (string= parent-type it)))
                        (or (null node-field)
                            (awhen (treesit-node-field-name node)
                              (string= node-field it)))
                        (or (null node-index-min)
                            (>= (treesit-node-index node)
                                node-index-min))
                        (or (null node-index-max)
                            (<= (treesit-node-index node)
                                node-index-max))))))
         (cons 'n-p-gp
               (lambda (node-t parent-t grand-parent-t)
                 (lambda (node parent _)
                   (and (or (null node-t)
                            (awhen (treesit-node-type node)
                              (if (stringp node-t)
                                  (string= node-t it)
                                (member it node-t))))
                        (or (null parent-t)
                            (awhen (treesit-node-type parent)
                              (if (stringp parent-t)
                                  (string= parent-t it)
                                (member it parent-t))))
                        (or (null grand-parent-t)
                            (when-let ((gp (treesit-node-parent parent))
                                       (gpt (treesit-node-type gp)))
                              (if (stringp grand-parent-t)
                                  (string= grand-parent-t gpt)
                                (member gpt grand-parent-t))))))))
         (cons 'parent-is (lambda (&rest types)
                            (lambda (_n parent _)
                              (member (treesit-node-type parent) types))))
         (cons 'grand-parent-is (lambda (&rest types)
                                  (lambda (_n parent _)
                                    (awhen (treesit-node-parent parent)
                                      (member (treesit-node-type it) types)))))
         (cons 'node-is (lambda (&rest types)
                          (lambda (node _ _)
                            (awhen (treesit-node-type node)
                              (member it types)))))
         (cons 'field-is (lambda (&rest names)
                           (lambda (node _ _)
                             (awhen (treesit-node-field-name node)
                               (member it names)))))

         (cons 'between-siblings
               (lambda (prev next)
                 (lambda (node _ _)
                   (when-let ((prev-node (treesit-node-prev-sibling node))
                              (next-node (treesit-node-next-sibling node)))
                     (and (string= (treesit-node-type prev-node) prev)
                          (string= (treesit-node-type next-node) next)))))))

   (--map (assq it treesit-simple-indent-presets)
          '(no-node
            comment-end
            catch-all
            query
            first-sibling
            nth-sibling
            parent
            comment-start
            prev-adaptive-prefix
            grand-parent
            great-grand-parent

            parent-bol

            standalone-parent
            prev-sibling
            no-indent
            prev-line
            column-0
            and
            or
            not
            list))))

(defun haskell-ts-indent--prev-adaptive-prefix (_n parent bol)
  (save-match-data
    (let (comment-start-bol
          this-line-has-prefix)
      (save-excursion
        (goto-char (treesit-node-start parent))
        (setq comment-start-bol (line-beginning-position))

        (goto-char bol)
        (setq this-line-has-prefix
              (and (looking-at adaptive-fill-regexp)
                   (not (string-match-p
                         (rx bos (* whitespace) eos)
                         (match-string 0)))))

        (forward-line -1)
        (and (>= (point) comment-start-bol)
             adaptive-fill-regexp
             (looking-at adaptive-fill-regexp)
             ;; If previous line is an empty line, don't
             ;; indent.
             (not (haskell-on-blank-line?)
                  ;; (haskell-on-blank-line-from-any-column?)
                  )
             ;; Return the anchor.  If the indenting line
             ;; has a prefix and the previous line also
             ;; has a prefix, indent to the beginning of
             ;; prev line's prefix rather than the end of
             ;; prev line's prefix. (Bug#61314).
             (or (and this-line-has-prefix
                      (match-beginning 1))
                 (match-end 0)))))))

(defun haskell-ts-indent--make-trivial-computed-indent (x)
  (cl-assert (treesit-node-p x))
  (make-treesit-computed-indent :anchor-node x :flags nil))

(defun haskell-ts-indent--select-parens-anchor (curr-type curr prev consider-spaces-after-open-paren?)
  (cl-assert (stringp curr-type))
  (cl-assert (treesit-node-p curr))
  (cl-assert (or (null prev) (treesit-node-p prev)))
  (when (string= "parens" curr-type)
    (if (and consider-spaces-after-open-paren?
             prev)
        (let ((open-paren (haskell-ts-getters--get-opening-paren curr)))
          ;; If there’s space after parens then what’s within is the anchor.
          ;; If there’s no space then it’s compact and parens are the anchor.
          (if (extended-whitespace-char? (char-after (treesit-node-end open-paren)))
              prev
            curr))
      curr)))

(defun haskell-ts-indent--standalone-non-infix-parent--generic (node parent bol support-functions? support-field-update? return-list-or-tuple-child?)
  (save-excursion
    (let ((prev2 nil)
          (prev1 node)
          (curr parent)
          (tmp nil))
      (catch 'term
        (while curr
          (cl-assert (treesit-node-p curr))
          (cl-assert (or (null prev1) (treesit-node-p prev1)))
          (cl-assert (or (null prev2) (treesit-node-p prev2)))
          (when treesit--indent-verbose
            (message "haskell-ts-indent--standalone-non-infix-parent--generic: curr = %s" curr))
          (let ((curr-type (treesit-node-type curr)))
            (when (and support-functions?
                       (string= "function" curr-type))
              (let ((result-child (treesit-node-child-by-field-name curr "result")))
                (throw 'term
                       (haskell-ts-indent--make-trivial-computed-indent
                        (if (equal result-child prev1)
                            prev1
                          curr)))))
            (when (and support-field-update?
                       (string= "field_update" curr-type))
              (when-let ((field-name (treesit-node-child-by-field-name curr "field")))
                (throw 'term (haskell-ts-indent--make-trivial-computed-indent field-name))))
            (when (string= "infix" curr-type)
              (let ((left-child (haskell-ts-getters--infix-left-operand curr))
                    (right-child (haskell-ts-getters--infix-right-operand curr))
                    (op-child (haskell-ts-getters--infix-operator curr)))
                (cond
                  ((or (equal prev1 left-child)
                       (and left-child
                            op-child
                            ;; The cursor is on neither child of the infix operator.
                            ;; The cursor is in the liminal (white)space between the operator
                            ;; and its left argument.
                            (< (treesit-node-end left-child)
                               (point))
                            (< (point)
                               (treesit-node-start op-child))))
                   ;; Continue: we’re left operand of an infix operator,
                   ;; operator comes after us so if we’re not at bol then
                   ;; whe don’t care where operator is.
                   ;; (throw 'term prev1)
                   )
                  ((or (equal prev1 right-child)
                       (equal prev1 op-child)
                       (and right-child
                            op-child
                            ;; The cursor is on neither child of the infix operator.
                            ;; The cursor is in the liminal (white)space between the operator
                            ;; and its right argument.
                            (< (treesit-node-end op-child)
                               (point))
                            (< (point)
                               (treesit-node-start right-child))))
                   ;; Operator may be on a line of its own, take it into account.
                   (when (haskell-ts--is-standalone-node? op-child)
                     (if (haskell-ts--is-standalone-node? right-child)
                         (when (equal prev1 right-child)
                           (throw 'term (make-treesit-computed-indent
                                         :anchor-node op-child
                                         :flags '(right-child-of-standalone-op))))
                       (throw 'term (haskell-ts-indent--make-trivial-computed-indent right-child))))
                   ;; Otherwise whole operator application may occupy
                   ;; its own line, i.e. its left child may be at the
                   ;; line start so continue processing current node
                   ;; as is.
                   )
                  (t
                   ;; Should not happen but just don’t do anything then.
                   (error "Unexpected infix field, node = %s, child = %s, point = %s, op = %s"
                          curr
                          prev1
                          (point)
                          op-child)))))
            (when-let* (((string= "signature" curr-type))
                        (double-colon (haskell-ts-indent--get-signature-double-colon curr))
                        (type-child (treesit-node-child-by-field-name curr "type"))
                        ;; If we came from signature’s type...
                        ((equal prev1 type-child))
                        (double-colon (haskell-ts-indent--get-signature-double-colon curr))
                        ;; ... and colon was standalone ...
                        ((haskell-ts--is-standalone-node? double-colon)))
              ;; ... then type after colon is our anchor
              (throw 'term (haskell-ts-indent--make-trivial-computed-indent prev1)))

            (awhen (haskell-ts-indent--select-parens-anchor curr-type curr prev1 t)
              (throw 'term (haskell-ts-indent--make-trivial-computed-indent it)))

            (cond
              ((and (or (string= "let" curr-type)
                        (string= "let_in" curr-type))
                    (string= "qualifier" (treesit-node-field-name curr)))
               (when prev2
                 (throw 'term (haskell-ts-indent--make-trivial-computed-indent prev2))))
              ((or (string= "list" curr-type)
                   (string= "list_comprehension" curr-type)
                   (string= "tuple" curr-type)
                   (string= "unboxed_tuple" curr-type))
               (throw 'term
                      (haskell-ts-indent--make-trivial-computed-indent (if return-list-or-tuple-child?
                                                                           prev1
                                                                         curr))))
              ((and (string= "match" curr-type)
                    (treesit-node-child-by-field-name curr "guards"))
               (throw 'term
                      (haskell-ts-indent--make-trivial-computed-indent
                       (haskell-ts-indent--get-match-guard-pipe-opt curr))))
              ((and (string= "bind" curr-type)
                    (setf tmp (treesit-node-child-by-field-name curr "arrow"))
                    (haskell-ts--is-standalone-node? tmp))
               (throw 'term
                      (haskell-ts-indent--make-trivial-computed-indent tmp)))
              ((haskell-ts--is-standalone-node? curr)
               (cond
                 ((string= "match" curr-type)
                  (if-let* ((expr (treesit-node-child-by-field-name curr "expression")))
                      (throw 'term (haskell-ts-indent--make-trivial-computed-indent expr))
                    (throw 'term (haskell-ts-indent--make-trivial-computed-indent curr))))
                 ((string= "let" curr-type)
                  (when prev2
                    (throw 'term (haskell-ts-indent--make-trivial-computed-indent prev2))))
                 ((string= "let_in" curr-type)
                  (let ((in (haskell-ts-indent--get-let-node-in curr)))
                    (cond
                      ((and prev1
                            (< (treesit-node-start in) (treesit-node-start prev1))
                            (haskell-ts--is-standalone-node? in))
                       (throw 'term (haskell-ts-indent--make-trivial-computed-indent in)))
                      (prev2
                       (throw 'term (haskell-ts-indent--make-trivial-computed-indent prev2))))))
                 (t
                  (throw 'term (haskell-ts-indent--make-trivial-computed-indent curr)))))))
          (setq prev2 prev1
                prev1 curr
                curr (treesit-node-parent curr)))))))

;; This is the most general of the lot and thus is a reasonable default.
(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t t nil))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-or-tuple-parent (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t t t))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-no-list-or-tuple-parent (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t nil t))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol nil t nil))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-or-tuple-parent (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol nil t t))

(defun haskell-ts-indent--standalone-record-start (node parent bol)
  (let ((typ (treesit-node-type parent)))
    (cond
      ((when (or (string= "record" typ)
                 (string= "fields" typ))
         (haskell-ts-indent--get-record-or-fields-open-brace parent)))
      (t
       (haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update node parent bol)))))

(defun haskell-ts-indent--standalone-vertical-infix-operator-parent (node parent bol)
  (save-excursion
    (let ((curr parent)
          (prev1 node)
          (prev2 nil))
      (catch 'term
        (while curr
          (let ((curr-type (treesit-node-type curr)))
            (awhen (haskell-ts-indent--select-parens-anchor curr-type curr prev1 t)
              (throw 'term it))
            (cond
              ((member curr-type '("list" "tuple" "unboxed_tuple"))
               (throw 'term prev1))
              ((and (string= "match" curr-type)
                    (treesit-node-child-by-field-name curr "expression"))
               (throw 'term (haskell-ts-indent--get-match-equals curr)))
              ((haskell-ts--is-standalone-node? curr)
               (throw 'term curr))
              (t
               (setq prev2 prev1
                     prev1 curr
                     curr (treesit-node-parent curr))))))))))

(defun haskell-ts-indent--prev-sib (node parent bol)
  (let ((n (treesit-node-prev-sibling node)))
    (while (and n
                (string= "comment" (treesit-node-type n)))
      (setq n (treesit-node-prev-sibling n)))
    (if (string= "bind" (treesit-node-type n))
        (save-excursion
          (goto-char (treesit-node-start n))
          (skip-whitespace-forward)
          (point))
        n)))

(defun haskell-ts-indent--under-local-binds-anchor (node parent bol)
  (if node
      (haskell-ts-indent--prev-sib node parent bol)
    ;; Handle no-node empty line case.
    parent))

(defun haskell-ts--positions-on-the-same-line? (pos1 pos2)
  (let ((end (max pos1 pos2)))
    (save-excursion
      (goto-char (min pos1 pos2))
      (skip-chars-forward "^\r\n" end)
      (eq (point) end))))

(defun haskell-ts-haddock--haddock-arg-doc-anchor (node parent bol)
  (declare (ignore bol))
  (haskell-ts-haddock--haddock-arg-doc-anchor--impl parent
                                                    (treesit-node-start node)))

(defun haskell-ts-haddock--haddock-arg-doc-anchor--impl (start haddock-start)
  (let ((curr start))
    (catch 'term
      (while curr
        (let ((typ (treesit-node-type curr)))
          (cond
            ((string= typ "function")
             (when-let ((arr (treesit-node-child-by-field-name curr "arrow")))
               (when (and (or (null haddock-start)
                              ;; Check that arrow is before haddock comment.
                              (< (treesit-node-start arr)
                                 haddock-start))
                          (haskell-ts--is-standalone-node? arr))
                 (throw 'term arr))
               ;; Here ‘curr’ is the immediate parent of our ‘haddock’ node
               (when-let ((param
                           (let ((n (treesit-node-child-count curr t))
                                 (i 0)
                                 (continue t)
                                 (non-comment-child nil))
                             (while (and continue (< i n))
                               (let ((child (treesit-node-child curr i t)))
                                 (when (not (string= (treesit-node-type child)
                                                     "haddock"))
                                   (setf continue nil
                                         non-comment-child child)))
                               (setf i (+ i 1)))
                             (when (and non-comment-child
                                        (haskell-ts--is-standalone-node? non-comment-child))
                               (throw 'term non-comment-child))))))))
            ((string= typ "signature")
             (when-let ((double-colon (haskell-ts-indent--get-signature-double-colon curr)))
               (when (haskell-ts--is-standalone-node? double-colon)
                 (throw 'term double-colon))))
            ((string= typ "declarations")
             (throw 'term nil))
            ((haskell-ts--is-standalone-node? curr)
             (throw 'term curr))
            (
             ;; skip
             ))
          (setf curr (treesit-node-parent curr)))))))

(defun haskell-ts-haddock--haddock-result-doc-anchor (node parent bol)
  (let ((prev (treesit-node-prev-sibling node)))
    (unless (string= (treesit-node-type prev)
                     "signature")
      (error "Node to this haddock node (%s) must be a signature but it’s %s"
             node
             prev))
    (catch 'term
      (let* ((sig prev)
             (sig-end (treesit-node-end sig)))
        (when-let ((double-colon (haskell-ts-indent--get-signature-double-colon sig)))
          (when (haskell-ts--is-standalone-node? double-colon)
            (throw 'term double-colon)))

        (when-let ((last-type (treesit-node-descendant-for-range sig (- sig-end 1) sig-end)))
          (haskell-ts-haddock--haddock-arg-doc-anchor--impl last-type nil))))))

(defun haskell-ts--is-standalone-node? (node)
  (save-excursion
    (let ((start (treesit-node-start node)))
      (goto-char start)
      (skip-chars-backward " \t")
      (eq (point) (line-beginning-position)))))

(defun haskell-ts-indent--get-topmost-function-node (node)
  (cl-assert (treesit-node-p node))
  (let ((result nil))
    (while (and node
                (not (member (treesit-node-type node) '("function" "context" "forall"))))
      (setf node (treesit-node-parent node)))
    ;; Go up only one level to not traverse through nested functions.
    (dolist (target '("function" "context" "forall"))
      (when (and node
                 (string= (treesit-node-type node) target))
        (setf result node
              node (treesit-node-parent node))))
    result))

(defun haskell-ts-indent--function-arrow-anchor (node parent bol)
  (when-let* ((candidate (haskell-ts-indent--get-topmost-function-node node)))
    (let ((candidate-type (treesit-node-type candidate)))
      (cond
        ((string= candidate-type "context")
         (haskell-ts-indent--get-context-context candidate))
        (t
         candidate)))))

(defun haskell-ts-indent--function-arrow-indent (node parent bol)
  (lambda (matched-anchor)
    (if-let* ((anchor-paren-context
               (when-let* ((p (when-let* ((pp (treesit-node-parent matched-anchor)))
                                (if (string= (treesit-node-type pp) "context")
                                    (treesit-node-parent pp)
                                  pp)))
                           (p-type (treesit-node-type p))
                           ((or (string= "parens" p-type)
                                (string= "tuple" p-type)
                                (string= "unboxed_tuple" p-type)
                                (string= "list" p-type))))
                 p)))

        ;; Check that there’s enough space to put ‘->’ back like this
        ;; (      Foo
        ;;     -> Bar
        ;; )
        ;;
        ;; (  Foo
        ;; -> Bar
        ;; )
        (let ((open-paren (haskell-ts-getters--get-opening-generic-paren
                           anchor-paren-context)))
          (if (<= 2
                  (- (treesit-node-start matched-anchor)
                     (treesit-node-end open-paren)))
              -3
            (- (treesit-node-start open-paren)
               (treesit-node-start matched-anchor))))
      -3)))

(defun haskell-ts-indent--type-function-anchor--impl
    (node parent bol consider-spaces-after-open-paren? consider-context?)
  (let ((prev node)
        (curr parent))
    (catch 'term
      (while curr
        (let ((curr-type (treesit-node-type curr)))
          (when-let* ((candidate (haskell-ts-indent--select-parens-anchor curr-type curr prev consider-spaces-after-open-paren?))
                      ;; Never select forall as our anchor
                      ;; ((not (string= (treesit-node-type candidate) "forall")))
                      )
            (throw 'term (if (string= (treesit-node-type candidate) "function")
                             (treesit-node-parent candidate)
                           candidate)))
          (cond
            ((and consider-context?
                  (string= curr-type "context"))
             (awhen (haskell-ts-indent--get-context-arrow curr)
               (throw 'term it)))
            ((string= curr-type "signature")
             (when-let ((double-colon (haskell-ts-indent--get-signature-double-colon curr)))
               (throw 'term (if (haskell-ts--is-standalone-node? double-colon)
                                double-colon
                              (haskell-ts-indent--get-signature-name curr))))))
          (setf prev curr
                curr (treesit-node-parent curr)))))))

(defun haskell-ts-indent--type-function-context-anchor (node parent bol)
  (haskell-ts-indent--type-function-anchor--impl node parent bol nil nil))

(defun haskell-ts-indent--type-function--find-above-forall (node)
  (treesit-utils-find-closest-parent-until
   node
   (lambda (x)
     (string= (treesit-node-type x) "forall"))
   (lambda (x)
     (string= (treesit-node-type x) "signature"))))

(defun haskell-ts-indent--type-function-first-arg-anchor (node parent bol)
  (if-let* (((string= (treesit-node-type parent) "function"))
            (arrow (haskell-ts-indent--get-function-arrow parent))
            ((not (haskell-ts--is-standalone-node? arrow)))
            (above-forall (haskell-ts-indent--type-function--find-above-forall parent)))
      above-forall
    (haskell-ts-indent--type-function-anchor--impl node parent bol nil t)))

(defun haskell-ts-indent--type-function-second-or-later-arg-anchor (node parent bol)
  (cl-assert (string= (treesit-node-type parent) "function"))
  (let ((grandparent (treesit-node-parent parent)))
    (cl-assert (string= (treesit-node-type grandparent) "function"))
    (let ((above-arrow (haskell-ts-indent--get-function-arrow grandparent)))
      (if (haskell-ts--is-standalone-node? above-arrow)
          above-arrow
        (let ((above-param (haskell-ts-indent--get-function-parameter grandparent)))
          (if (haskell-ts--is-standalone-node? above-param)
              above-param
            (if-let* ((above-forall (haskell-ts-indent--type-function--find-above-forall grandparent)))
                above-forall
              above-param)))))))

(defun haskell-ts-indent--type-function-in-context-first-arg-anchor (node parent bol)
  (cl-assert (string= (treesit-node-type parent) "function"))
  (let ((context (treesit-node-parent parent)))
    (cl-assert (string= (treesit-node-type context) "context"))
    (let ((ctx-arrow (haskell-ts-indent--get-context-arrow context)))
      (if (haskell-ts--is-standalone-node? ctx-arrow)
          ctx-arrow
        (let ((ctx-contents (haskell-ts-indent--get-context-context context)))
          (if (haskell-ts--is-standalone-node? ctx-contents)
              ctx-contents
            (if-let* ((forall (treesit-node-parent context))
                      ((string= "forall" (treesit-node-type forall))))
                forall
              ctx-contents)))))))

(defun haskell-ts-indent--type-function-result-anchor (node parent bol)
  (cl-assert (string= (treesit-node-type parent) "function"))
  (let ((func-arrow (haskell-ts-indent--get-function-arrow parent)))
    (if (haskell-ts--is-standalone-node? func-arrow)
        func-arrow
      (haskell-ts-indent--get-function-parameter parent))))

(defun haskell-ts-indent--function-indent (_ _ _)
  #'haskell-ts-indent--function-indent-anchor)

(defun haskell-ts-indent--function-indent-anchor (matched-anchor)
  (let* ((typ (treesit-matched-anchor-node-type matched-anchor))
         (parent (treesit-node-parent matched-anchor))
         (parent-typ (awhen parent
                       (treesit-matched-anchor-node-type it))))
    (cond
      ((or (string= "::" typ)
           (string= "=>" typ)
           (string= "forall" typ))
       0)
      ((and (or (string= "parens" parent-typ)
                (string= "tuple" parent-typ))
            ;; Check that there’s enough space to put ‘->’ back like this
            ;; (      Foo
            ;;     -> Bar
            ;; )
            (>= (- (treesit-node-start matched-anchor)
                   (treesit-node-end (haskell-ts-getters--get-opening-paren parent)))
                2))
       -3)
      ((or (string= "parens" typ)
           (string= "tuple" typ))
       0)
      (t
       haskell-indent-offset))))

(defun haskell-ts-indent--context-anchor (anchor)
  (pcase (treesit-node-type anchor)
    ("signature"
     (haskell-ts-indent--get-signature-double-colon anchor))
    ("parens"
     anchor)
    (_
     (error "Unhandled anchor: %s" anchor))))

(defun haskell-ts-indent--context-arrow-anchor (node parent bol)
  (cl-assert (string= (treesit-node-type node) "=>"))
  (cl-assert (string= (treesit-node-type parent) "context"))
  (let* ((p (treesit-node-parent parent))
         (anchor (if (and p
                          (string= (treesit-node-type p) "forall"))
                     (treesit-node-parent p)
                   p)))
    (haskell-ts-indent--context-anchor anchor)))

(defun haskell-ts-indent--context-dot-anchor (node parent bol)
  (cl-assert (string= (treesit-node-type node) "."))
  (cl-assert (string= (treesit-node-type parent) "forall"))
  (let ((anchor (treesit-node-parent parent)))
    (haskell-ts-indent--context-anchor anchor)))

(defun haskell-ts-indent--first-guard-or-parent (node parent bol)
  (let ((bind-node parent))
    (cl-assert (member (treesit-node-type bind-node) '("bind" "multi_way_if" "function")))
    (let ((first-match nil)
          (continue? t)
          (i 0)
          (children-count (treesit-node-child-count bind-node)))
      (while (and continue?
                  (< i children-count))
        (let ((child (treesit-node-child bind-node i)))
          (when (and (string= "match" (treesit-node-field-name-for-child bind-node i))
                     (string= "match" (treesit-node-type child)))
            (setf continue? nil
                  first-match child)))
        (setf i (+ i 1)))
      (if first-match
          (if (equal node first-match)
              bind-node
            (progn
              (cl-assert (string= "match" (treesit-node-type first-match)))
              (let ((pipe-node (treesit-node-child first-match 0)))
                (cl-assert (string= "|" (treesit-node-type pipe-node)))
                pipe-node)))
        bind-node))))

(defun haskell-ts-indent--list-generator-anchor (node _ _)
  (treesit-utils-find-closest-parent-limited
   node
   (lambda (x)
     (string= (treesit-node-type x) "generator"))
   5))

(defun haskell-ts-indent--record-field-update-anchor (_ parent _)
  (cl-assert (string= "record" (treesit-node-type parent)))
  (haskell-ts-indent--get-record-or-fields-open-brace parent))

(defconst haskell-ts-indent-rules
  (eval-when-compile
    (let ((rules
           `(((and (node-is "haddock")
                   (parent-is "function" "signature"))
              haskell-ts-haddock--haddock-arg-doc-anchor
              0)

             ((and (node-is "haddock")
                   (between-siblings "signature" "function"))
              haskell-ts-haddock--haddock-result-doc-anchor
              0)

             ((n-p-gp '("comment" "haddock") '("declarations" "haskell") nil) prev-sibling 0)

             ((n-p-gp nil "prefix" "data_constructor")
              grand-parent
              haskell-indent-offset)

             ((node-is "cpp") column-0 0)

             ((parent-is "field") parent haskell-indent-offset)

             (,(lambda (node parent bol)
                 (and node
                      (member (treesit-node-type node) '("record" "fields"))
                      (when-let ((open-brace (haskell-ts-indent--get-record-or-fields-open-brace node)))
                        (eq (treesit-node-start node)
                            (treesit-node-start open-brace)))))
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update
              haskell-indent-offset)
             ((n-p-gp '("," "}") '("record" "fields") nil)
              haskell-ts-indent--standalone-record-start
              0
              ;; haskell-indent-offset
              )

             ;; If then else
             ((n-p-gp '("then" "else") "conditional" nil) parent 0)
             ((field-is "then" "else") parent haskell-indent-offset)

             ((or (parent-is "field_update")
                  (node-is "infix"))
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-or-tuple-parent
              haskell-indent-offset)

             ;; Other infix rules

             ;; Lambda
             ((parent-is "lambda")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update
              haskell-indent-offset)

             ((node-is "in") parent 0)
             ((match nil "let_in" "expression" nil nil)
              ,(lambda (node parent bol)
                 (let* ((in-node (haskell-ts-indent--get-let-node-in parent))
                        (in-node-start (treesit-node-start in-node))
                        (parent-start (treesit-node-start parent)))
                   (if (haskell-ts--positions-on-the-same-line? in-node-start parent-start)
                       parent
                     in-node)))
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "let_in" (treesit-matched-anchor-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             ;; list
             ((node-is "]") parent 0)
             ((n-p-gp "," "list" nil) parent 0)
             ((n-p-gp "," "qualifiers" "list_comprehension") grand-parent 0)
             ((n-p-gp "|" "list_comprehension" nil) parent 0)
             ((parent-is "list" "list_comprehension") parent haskell-indent-offset)
             ((or (parent-is "generator")
                  (grand-parent-is "generator"))
              haskell-ts-indent--list-generator-anchor
              haskell-indent-offset)

             ;; Assumes that this will only hit when "operator" node is at beginning of line.
             ((n-p-gp "operator" "infix" nil)
              haskell-ts-indent--standalone-vertical-infix-operator-parent
              ,(lambda (_ parent _)
                 (lambda (matched-anchor)
                   (cl-assert (treesit-node-p matched-anchor))
                   (if-let* (((string= "infix" (treesit-node-type matched-anchor)))
                             (right (haskell-ts-getters--infix-right-operand parent))
                             ((not (haskell-ts--is-standalone-node? right))))
                       haskell-indent-offset
                     0))))

             ;; Fallback
             ((parent-is "infix")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-or-tuple-parent
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (cond
                     ((and (treesit-computed-indent-p matched-anchor)
                           (memq 'right-child-of-standalone-op
                                 (treesit-computed-indent-flags matched-anchor)))
                      0)
                     ((string= "let_in" (treesit-matched-anchor-node-type matched-anchor))
                      0)
                     (t
                      haskell-indent-offset)))))

             ((parent-is "apply")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-or-tuple-parent
              haskell-indent-offset)

             ;; ((lambda (node parent bol)
             ;;    (let ((n (treesit-node-prev-sibling node)))
             ;;      (while (string= "comment" (treesit-node-type n))
             ;;        (setq n (treesit-node-prev-sibling n)))
             ;;      (string= "do" (treesit-node-type n))))
             ;;  haskell-ts-indent--standalone-vertical-infix-operator-parent
             ;;  haskell-indent-offset)
             ;; ((parent-is "do") haskell-ts-indent--prev-sib 0)

             ((n-p-gp "<-" "bind" "do")
              parent
              haskell-indent-offset)

             ((and (parent-is "bind")
                   (field-is "expression"))
              ,(lambda (n p bol)
                 (let ((arrow (treesit-node-child-by-field-name p "arrow")))
                   (if (and arrow
                            (haskell-ts--is-standalone-node? arrow))
                       arrow
                     p)))
              haskell-indent-offset)

             ((parent-is "do")
              ,(lambda (n p bol)
                 (when-let* ((matched-anchor
                              (haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-or-tuple-parent n p bol)))
                   (if (string= "do" (treesit-matched-anchor-node-type matched-anchor))
                       ;; If do node is our topmost guide then take its bol...
                       (save-excursion
                         (goto-char (treesit-node-start matched-anchor))
                         (skip-to-indentation)
                         (point))
                     ;; ...otherwise the carefully selected anchor is our guide so keep it.
                     ;; Sometimes bol of the ‘do’ node is not what we want because of e.g. ‘let’:
                     ;; let foo x = do
                     ;;       _|_...
                     matched-anchor)))
              haskell-indent-offset)

             ((node-is "alternatives")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-or-tuple-parent
              haskell-indent-offset)
             ((parent-is "alternatives") haskell-ts-indent--prev-sib 0)

             ;; Here node is typically nil but we don’t want to match the ‘no-node’ rule below.
             ((parent-is "string")
              (lambda (node parent bol-pos)
                (save-excursion
                  (goto-char bol-pos)
                  (cond
                    ((or (looking-at-p (rx "\"\"\"" eol))
                         (smart-operators--on-empty-line?)
                         (and (eq (char-after) ?\\)
                              (save-excursion
                                (forward-line 0)
                                (skip-chars-backward " \t\n\r")
                                (eq (char-before) ?\\))))
                     (haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-or-tuple-parent
                      node
                      parent
                      bol-pos))
                    (t
                     ;; Current beginnig of line is the target position -
                     ;; the effect of this and 0 offset later is to leave
                     ;; indentation unchanged.
                     bol-pos))))
              (lambda (_ _ _)
                (lambda (matched-anchor)
                  (cond
                    ((numberp matched-anchor)
                     0)
                    ((and (treesit-computed-indent-p matched-anchor)
                          (member (treesit-matched-anchor-node-type matched-anchor) '("string" "literal")))
                     0)
                    (t
                     haskell-indent-offset)))))

             ((parent-is "quasiquote_body")
              (lambda (_ _ bol-pos)
                ;; Current beginnig of line is the target position -
                ;; the effect of this and 0 offset later is to leave
                ;; indentation unchanged.
                bol-pos)
              0)

             ((n-p-gp "|]" "quasiquote" nil)
              (lambda (_ parent _)
                (haskell-ts-getters--get-quasiquote-opening-bracket parent))
              0)

             ((and no-node
                   (parent-is "comment" "haddock"))
              parent
              0)

             ((and (parent-is "let" "let_in")
                   (or (node-is "local_binds" "comment")
                       no-node))
              ,(lambda (node parent bol-pos)
                 (if node
                     (treesit-node-prev-sibling node)
                   (if-let ((child (treesit-node-first-child-for-pos parent bol-pos)))
                       (treesit-node-prev-sibling child)
                     parent)))
              ,(lambda (node parent bol-pos)
                 (lambda (matched-anchor)
                   (if (string= (treesit-node-type matched-anchor) "let")
                       haskell-indent-offset
                     0))))

             ;; Also handles binds within let.
             ;; Needs to come before ‘no-node’ to handle empty lines in where blocks.
             ((parent-is "local_binds")
              haskell-ts-indent--under-local-binds-anchor
              0)

             (no-node
              ,(lambda (node parent bol-pos)
                 (let ((typ (treesit-node-type parent)))
                   (cond
                     ;; If we’re on toplevel
                     ((string= typ "declarations")
                      (let ((pos (haskell-ts-indent--prev-adaptive-prefix node parent bol-pos)))
                        (if-let* ((prev-node (treesit-node-at pos))
                                  (typ (treesit-node-type prev-node)))
                            (cond
                              ((string= typ "data")
                               (make-treesit-computed-indent
                                :anchor-node prev-node
                                :flags '(indent-once)))
                              ((and (string= typ "deriving")
                                    (when-let* ((p1 (treesit-node-parent prev-node))
                                                (p2 (treesit-node-parent p1)))
                                      (string= (treesit-node-type p2) "data_type")))
                               (haskell-ts-indent--make-trivial-computed-indent prev-node))
                              (t
                               pos))
                          (haskell-ts-indent--make-trivial-computed-indent parent))))
                     ((string= typ "data_constructors")
                      (make-treesit-computed-indent
                       :anchor-node parent
                       :flags '(indent-once)))
                     ((string= typ "class_declarations")
                      (haskell-ts-indent--make-trivial-computed-indent parent))
                     ((string= typ "case")
                      (when-let ((indent (haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-or-tuple-parent node parent bol-pos)))
                        (push 'indent-once (treesit-computed-indent-flags indent))
                        indent))
                     (t
                      (haskell-ts-indent--prev-adaptive-prefix node parent bol-pos)))))
              ,(lambda (_node parent bol-pos)
                 ;; Since there’s no node, bol-pos is our cursor position too.
                 (lambda (matched-anchor)
                   (cond
                     ((treesit-computed-indent-p matched-anchor)
                      (if (memq 'indent-once (treesit-computed-indent-flags matched-anchor))
                          haskell-indent-offset
                        0))
                     ((or (and (treesit-haskell--is-inside-node? bol-pos parent)
                               (not (string= (treesit-node-type parent) "declarations")))
                          (save-excursion
                            (indent-backward-up-indentation-or-sexp #'haskell-on-blank-line-from-any-column? nil t)
                            (looking-at-p "\\_<where\\_>")))
                      haskell-indent-offset)
                     (t
                      0)))))

             ((parent-is "data_constructors") grand-parent haskell-indent-offset)
             ((node-is "gadt_constructors") parent haskell-indent-offset)
             ((parent-is "gadt_constructors") grand-parent haskell-indent-offset)

             ;; instance

             ;; first instance function
             ((node-is "class_declarations" "instance_declarations")
              parent
              haskell-indent-offset)

             ;; subsequent instance functions
             ((parent-is "class_declarations" "instance_declarations")
              grand-parent
              haskell-indent-offset)

             ;; where
             ((lambda (node _ _)
                (let ((n (treesit-node-prev-sibling node)))
                  (while (string= "comment" (treesit-node-type n))
                    (setq n (treesit-node-prev-sibling n)))
                  (string= "where" (treesit-node-type n))))
              (lambda (node parent _bol)
                ;; In situation
                ;; ```
                ;; foo = ...
                ;;   where
                ;;     _|_-- comment
                ;;     ...
                ;; ```
                ;; the node is "comment" but parent is "function" instead of "where"
                ;; so it has to be worked around.
                (let ((n (treesit-node-prev-sibling node)))
                  (while (string= "comment" (treesit-node-type n))
                    (setq n (treesit-node-prev-sibling n)))
                  n))
              haskell-indent-offset)
             ((node-is "where") parent haskell-indent-offset)

             ((n-p-gp "field_update" "record" nil)
              haskell-ts-indent--record-field-update-anchor
              haskell-indent-offset)

             ;; Must come after where because parents of comments are sometimes
             ;; incorrect and comment under ‘where’ may be attributed to the enclosing
             ;; function.
             ;;
             ;; Must also come after "local_binds" so that indentation within lets
             ;; is handled there.
             ((or (n-p-gp "{" "record" nil)
                  (node-is "comment" "haddock"))
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-or-tuple-parent
              haskell-indent-offset)

             ((n-p-gp "match" '("bind" "multi_way_if" "function") nil)
              haskell-ts-indent--first-guard-or-parent
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "|" (treesit-matched-anchor-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             (,(lambda (n p _)
                 (and (member (treesit-node-type n) '("=" "->"))
                      (string= "match" (treesit-node-type p))
                      (treesit-node-child-by-field-name p "guards")))
              ,(lambda (_ p _)
                 (haskell-ts-indent--get-match-guard-pipe p))
              0)

             ((parent-is "match")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-no-list-or-tuple-parent
              haskell-indent-offset)

             ((parent-is "comment" "imports" "haskell" "declarations") column-0 0)

             ((funcall n-p-gp "exports" "header" nil)
              parent
              haskell-indent-offset)

             ((n-p-gp '(")" ",") "exports" nil)
              parent
              0)

             ((parent-is "exports")
              (lambda (n _ _)
                (treesit-node-start (treesit-node-prev-sibling n)))
              0)
             ((n-p-gp nil "signature" "foreign_import") grand-parent haskell-indent-offset)

             ((n-p-gp
               '("," ")" "#)")
               '("parens" "tuple" "unboxed_tuple" "prefix_tuple")
               nil)
              parent
              0)

             ((and (parent-is "tuple" "unboxed_tuple" "prefix_tuple")
                   (field-is "element"))
              parent
              haskell-indent-offset)

             ((node-is "deriving") parent haskell-indent-offset)

             ((or (n-p-gp "->" "function" nil)
                  (n-p-gp "=>" "context" nil))
              haskell-ts-indent--function-arrow-anchor
              haskell-ts-indent--function-arrow-indent)

             ((n-p-gp "." "forall" nil)
              haskell-ts-indent--context-dot-anchor
              haskell-ts-indent--function-indent)

             ((n-p-gp "::" "signature" nil)
              parent
              haskell-indent-offset)

             ((and (n-p-gp nil "function" "context")
                   (field-is "parameter"))
              haskell-ts-indent--type-function-in-context-first-arg-anchor
              ,(lambda (node parent _)
                 (lambda (matched-anchor)
                   (if (string= (treesit-node-type matched-anchor) "=>")
                       (+ haskell-indent-offset 1)
                     haskell-indent-offset))))

             ((and (n-p-gp nil "context" '("forall" "parens" "tuple" "signature"))
                   (field-is "context"))
              haskell-ts-indent--type-function-context-anchor
              ,(lambda (node parent _)
                 (lambda (matched-anchor)
                   (let ((ctx-arrow-standalone?
                          (haskell-ts--is-standalone-node?
                           (haskell-ts-indent--get-context-arrow parent))))
                     (cond
                       ((and (string= (treesit-node-type matched-anchor) "parens")
                             ctx-arrow-standalone?)
                        (+ haskell-indent-offset 3))
                       ((or (string= (treesit-node-type matched-anchor) "::")
                            ctx-arrow-standalone?)
                        (+ haskell-indent-offset 1))
                       (t
                        haskell-indent-offset))))))

             ;; First argument in a function’s type signature.
             ((or
               ;; First argument when there’s context
               (and (n-p-gp nil "context" '("forall" "parens" "signature"))
                    (field-is "type"))
               ;; No context, try to catch fist argument of a vanilla
               ;; function
               (and (n-p-gp nil "function" '("forall" "parens" "signature" "context"))
                    (field-is "parameter")))
              haskell-ts-indent--type-function-first-arg-anchor
              ,(lambda (node parent _)
                 (lambda (matched-anchor)
                   (if-let* ((parent)
                             ((string= (treesit-node-type parent) "function"))
                             (arrow (haskell-ts-indent--get-function-arrow parent))
                             ((not (haskell-ts--is-standalone-node? arrow))))
                       haskell-indent-offset
                     (+ haskell-indent-offset 1)))))

             ;; Second or later argument in a function’s type signature.
             ((or
               ;; First argument when there’s context
               ;; (n-p-gp nil "context" '("forall" "parens" "signature"))
               ;; No context, try to catch fist argument of a vanilla
               ;; function
               (and (n-p-gp nil "function" "function")
                    (field-is "parameter")))
              haskell-ts-indent--type-function-second-or-later-arg-anchor
              ,(lambda (node parent _)
                 (lambda (matched-anchor)
                   (if (string= (treesit-node-type matched-anchor) "->")
                       (+ haskell-indent-offset 1)
                     haskell-indent-offset))))

             ;; Last argument in a function’s type signature.
             ((or
               ;; First argument when there’s context
               ;; (n-p-gp nil "context" '("forall" "parens" "signature"))
               ;; No context, try to catch fist argument of a vanilla
               ;; function
               (and (parent-is "function")
                    (field-is "result")))
              haskell-ts-indent--type-function-result-anchor
              ,(lambda (node parent _)
                 (lambda (matched-anchor)
                   (if (string= (treesit-node-type matched-anchor) "->")
                       3
                     2))))

             ((parent-is "parens")
              parent
              haskell-indent-offset)

             ;; No backup - we would like to default to something else.
             ;; ;; Backup
             ;; (catch-all parent haskell-indent-offset)
             )))
      (dolist (rule rules)
        (unless (and (listp rule)
                     (= 3 (length rule)))
          (error "Malformed rule in haskell-ts-indent-rules: %s"
                 rule)))
      rules)))

(defun haskell-ts-indent-setup ()
  (setq-local treesit-simple-indent-presets
              haskell-ts--treesit-simple-indent-presets
              treesit-simple-indent-rules
              (list (cons 'haskell haskell-ts-indent-rules)
                    (cons 'hsc haskell-ts-indent-rules))
              treesit-indent-region--indent-line-impl
              #'haskell-ts-indent-line--indent-1))

(defun haskell-ts-indent-line--indent-1 ()
  "Version of ‘treesit--indent-1’ tailored for Haskell syntax."
  (let* ((bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (local-parsers (treesit-local-parsers-at bol nil t))
         (smallest-node
          (cond ((car local-parsers)
                 (let ((local-parser (caar local-parsers))
                       (host-parser (cdar local-parsers)))
                   (if (eq (treesit-node-start
                            (treesit-parser-root-node local-parser))
                           bol)
                       (treesit-node-at bol host-parser)
                     (treesit-node-at bol local-parser))))
                ((null (treesit-parser-list)) nil)
                ((eq 1 (length (treesit-parser-list nil nil t)))
                 (treesit-node-at bol))
                ((treesit-language-at bol)
                 (treesit-node-at bol (treesit-language-at bol)))
                (t (treesit-node-at bol))))
         (root (treesit-parser-root-node
                (treesit-node-parser smallest-node)))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (when treesit--indent-verbose
                    (message "indent-1 loop, node = %s" node))
                  (and (eq bol (treesit-node-start node))
                       (not (treesit-node-eq node root))
                       (let ((typ (treesit-node-type node)))
                         (not (or (string= typ "context")
                                  ;; Check for type function, not value function.
                                  ;; Type is what we don’t want to ascend over, value is ok.
                                  (and (string= typ "function")
                                       (treesit-node-child-by-field-name node "parameter"))))))))))
    (let*
        ((parser (if smallest-node
                     (treesit-node-parser smallest-node)
                   nil))
         ;; NODE would be nil if BOL is on a whitespace.  In that case
         ;; we set PARENT to the "node at point", which would
         ;; encompass the whitespace.
         (parent (if (and node parser)
                     (treesit-node-parent node)
                   (treesit-node-on bol bol))))
      (when treesit--indent-verbose
        (message (mapconcat #'identity
                            '("indent-1:"
                              "node         = %s"
                              "parent       = %s"
                              "grand-parent = %s"
                              "bol          = %s"
                              "field-name   = %s")
                            "\n")
                 node
                 parent
                 (treesit-node-parent parent)
                 bol
                 (treesit-node-field-name node)))
      (funcall treesit-indent-function node parent bol))))

(defun haskell-ts-indent-line ()
  (treesit-update-ranges (line-beginning-position)
                         (line-end-position))
  (when-let ((indent-res
              (pcase-let* ((`(,anchor . ,offset) (haskell-ts-indent-line--indent-1)))
                (unless (and anchor offset)
                  (error "No treesitter indentation anchor found for line ‘%s_|_%s’"
                         (buffer-substring-no-properties (line-beginning-position) (point))
                         (buffer-substring-no-properties (point) (line-end-position))))
                (when treesit--indent-verbose
                  (message "haskell-ts-indent-line: anchor = %s" anchor))
                (when (and anchor offset)
                  (with-undo-amalgamate
                    (treesit-with-evaluated-anchor-and-offset
                        (anchor-pos anchor)
                        (offset-num offset)
                      ;; Indent with treesitter
                      (let* ((target-indent (+ (save-excursion
                                                 (goto-char anchor-pos)
                                                 (current-column-fixed))
                                               offset-num))
                             (is-on-empty-line?
                              (save-excursion
                                (beginning-of-line)
                                (skip-chars-forward " \t")
                                (eolp)))
                             (old-fingerprint
                              (unless is-on-empty-line?
                                (haskell-misc--indent-line--fingerprint))))
                        (undo-boundary)
                        (let ((delta (- (point-max) (point))))
                          (indent-line-to target-indent)
                          (if (or is-on-empty-line?
                                  (progn
                                    (treesit-update-ranges (line-beginning-position)
                                                           (line-end-position))
                                    (let ((new-fingerprint
                                           (haskell-misc--indent-line--fingerprint)))
                                      (equal old-fingerprint new-fingerprint))))
                              ;; Now point is at the end of indentation. If we started
                              ;; from within the line, go back to where we started.
                              (let ((d (- (point-max) delta)))
                                (when (> d (point))
                                  (goto-char d))
                                t)
                            (progn
                              ;; Our one-line indentation produces different AST
                              ;; so undo it and try more distruptive but safer
                              ;; method that keeps spaces alignment of current block.
                              (undo-start)
                              (undo-more 1)
                              ;; Undo then indent with preserving spaces
                              (let* ((current-indent (indentation-size))
                                     (diff (abs (- current-indent target-indent))))
                                (save-position-marker-unsafe
                                  (skip-to-indentation)
                                  (if (and (not (zerop diff))
                                           (< target-indent current-indent))
                                      (haskell-backspace-with-block-dedent--impl diff t)
                                    (haskell-space-with-block-indent--impl diff t))
                                  t))))))))))))
    ;; Normalize spaces between if and | in a multiway if ‘if...|’ construct.
    (let ((line-start-pos (line-beginning-position))
          (line-end-pos (line-end-position)))
      (treesit-update-ranges line-start-pos line-end-pos)
      (let* ((curr-node (treesit-utils-largest-node-starting-at line-start-pos))
             (multiway-if-pipes
              (treesit-query-capture curr-node
                                     (haskell-ts-query-resolve haskell-misc--multiway-if-query)
                                     nil
                                     nil
                                     t ;; Don’t capture names, return list of matched nodes we asked for.
                                     )))
        (dolist (pipe-node multiway-if-pipes)
          (let ((start (treesit-node-start pipe-node)))
            (when (< start line-end-pos)
              (let ((p
                     (save-excursion
                       (goto-char start)
                       (skip-chars-backward " \t")
                       (point))))
                (when (not (= 2 (- start p)))
                  (when-let* ((node-before (treesit-node-at p)))
                    (when (string= "if" (treesit-node-type node-before))
                      (delete-region p start)
                      (insert "  "))))))))))
    indent-res))

(provide 'haskell-ts-indent)

;; Local Variables:
;; End:

;; haskell-ts-indent.el ends here
