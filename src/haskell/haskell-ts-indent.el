;; haskell-ts-indent.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 November 2024
;; Description:

(eval-when-compile
  (require 'dash))

(defvar haskell-indent-offset)

(require 'common)
(require 'common-whitespace)
(require 'haskell-lexeme)
(require 'treesit)

(defconst haskell-ts--treesit-simple-indent-presets
  (append
   (list (cons 'match
               (lambda
                 (&optional node-type parent-type node-field
                            node-index-min node-index-max)
                 (lambda (node parent &rest _)
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
                 (lambda (node parent &rest _)
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
                              (string= grand-parent-t gpt)))))))
         (cons 'parent-is (lambda (&rest types)
                            (lambda (_n parent &rest _)
                              (member (treesit-node-type parent) types))))

         (cons 'node-is (lambda (&rest types)
                          (lambda (node &rest _)
                            (awhen (treesit-node-type node)
                              (member it types)))))
         (cons 'field-is (lambda (&rest names)
                           (lambda (node &rest _)
                             (awhen (treesit-node-field-name node)
                               (member it names)))))

         (cons 'between-siblings
               (lambda (prev next)
                 (lambda (node &rest _)
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

(defun haskell-ts-indent--standalone-non-infix-parent--generic (node parent bol support-functions? support-field-update? return-list-child?)
  (save-excursion
    (let ((prev2 nil)
          (prev1 node)
          (curr parent))
      (catch 'term
        (while curr
          (let ((curr-type (treesit-node-type curr)))
            (when (and support-functions?
                       (string= "function" curr-type))
              (let ((result-child (treesit-node-child-by-field-name curr "result")))
                (if (equal result-child prev1)
                    (throw 'term prev1)
                  (throw 'term curr))))
            (when (and support-field-update?
                       (string= "field_update" curr-type))
              (when-let ((field-name (treesit-node-child-by-field-name curr "field")))
                (throw 'term field-name)))
            (when (string= "infix" curr-type)
              (let ((left-child (treesit-node-child-by-field-name
                                 curr
                                 "left_operand"))
                    (right-child (treesit-node-child-by-field-name
                                  curr
                                  "right_operand"))
                    (op-child (treesit-node-child-by-field-name
                               curr
                               "operator")))
                (cond
                  ((equal prev1 left-child)
                   ;; Continue: we’re left operand of an infix operator,
                   ;; operator comes after us so if we’re not at bol then
                   ;; whe don’t care where operator is.
                   ;; (throw 'term prev1)
                   )
                  ((or (equal prev1 right-child)
                       (equal prev1 op-child))
                   ;; Operator may be on a line of its own, take it into account.
                   (when (and (haskell-ts--is-standalone-node? op-child)
                              (not (haskell-ts--is-standalone-node? right-child)))
                     (throw 'term prev1))
                   ;; Otherwise whole operator application may occupy
                   ;; its own line, i.e. its left child may be at the
                   ;; line start so continue processing current node
                   ;; as is.
                   )
                  (t
                   ;; Should not happen but just don’t do anything then.
                   (error "Unexpected infix field, node = %s, child = %s"
                          curr
                          prev1)))))
            (cond
              ((string= "parens" curr-type)
               (throw 'term curr))
              ((string= "list" curr-type)
               (throw 'term (if return-list-child?
                                prev1
                              curr)))
              ((string= "tuple" curr-type)
               (throw 'term prev1))
              ((and (string= "match" curr-type)
                    (treesit-node-child-by-field-name curr "guards"))
               (throw 'term (haskell-ts-indent--get-match-guard-pipe-opt curr)))
              ((haskell-ts--is-standalone-node? curr)
               (cond
                 ((string= "match" curr-type)
                  (if-let* ((expr (treesit-node-child-by-field-name curr "expression")))
                      (throw 'term expr)
                    (throw 'term curr)))
                 ((or (string= "let" curr-type)
                      (string= "let_in" curr-type))
                  (when prev2
                    (throw 'term prev2)))
                 (t
                  (throw 'term curr))))))
          (setq prev2 prev1
                prev1 curr
                curr (treesit-node-parent curr)))))))

;; This is the most general of the lot and thus is a reasonable default.
(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t t nil))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-parent (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t t t))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t nil nil))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol nil t nil))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-parent (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol nil t t))

(defun haskell-ts-indent--get-record-or-fields-open-brace (node)
  (cl-assert (member (treesit-node-type node) '("record" "fields")))
  (let* ((typ (treesit-node-type node))
         (open-brace-idx-candidates
          (cond
            ((string= "record" typ)
             '(0 1))
            ((string= "fields" typ)
             '(0))
            (t
             (error "Inexpected record-like node: %s" node))))
         (result
          (--some (when-let ((open-brace (treesit-node-child node it)))
                    (when (string= "{" (treesit-node-type open-brace))
                      open-brace))
                  open-brace-idx-candidates)))
    (cl-assert (or (null result)
                   (string= "{" (treesit-node-type result)))
               nil
               "Not an open brace node: %s, node = %s, parent = %s"
               open-brace
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-signature-double-colon (node)
  (cl-assert (string= "signature" (treesit-node-type node)))
  (let ((result (treesit-node-child node 1)))
    (cl-assert (or (null result)
                   (string= "::" (treesit-node-type result)))
               nil
               "Not a double colon: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-match-equals (node)
  (cl-assert (string= "match" (treesit-node-type node)))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (or (null result)
                   (string= "=" (treesit-node-type result)))
               nil
               "Not an equals %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-match-guard-pipe (node)
  (cl-assert (string= "match" (treesit-node-type node)))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (string= "|" (treesit-node-type result))
               nil
               "Not a pipe: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-match-guard-pipe-opt (node)
  (cl-assert (string= "match" (treesit-node-type node)))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (or (null result)
                   (string= "|" (treesit-node-type result)))
               nil
               "Not a pipe: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

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
            (cond
              ((string= "parens" curr-type)
               (throw 'term curr))
              ((string= "tuple" curr-type)
               (throw 'term prev1))
              ((string= "list" curr-type)
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

(defun haskell-ts-indent--type-function-anchor (node parent bol)
  (let ((prev node)
        (curr parent))
    (catch 'term
      (while curr
        (let ((typ (treesit-node-type curr)))
          (cond
            ((string= typ "signature")
             (when-let ((double-colon (haskell-ts-indent--get-signature-double-colon curr)))
               (throw 'term double-colon)))
            ((or (string= typ "parens")
                 (string= typ "tuple"))
             (throw 'term curr)))
          (setf prev curr
                curr (treesit-node-parent curr)))))))

(defun haskell-ts-indent--first-guard-or-parent (node parent bol)
  (let ((bind-node parent))
    (cl-assert (member (treesit-node-type bind-node) '("bind" "multi_way_if")))
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
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update
              haskell-indent-offset)

             ;; Other infix rules

             ;; Assumes that this will only hit when "operator" node is at beginning of line.
             ((n-p-gp "operator" "infix" nil)
              haskell-ts-indent--standalone-vertical-infix-operator-parent
              0)

             ;; Fallback
             ((parent-is "infix")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update
              haskell-indent-offset)

             ;; Lambda
             ((parent-is "lambda")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update
              haskell-indent-offset)

             ((parent-is "class_declarations") prev-sibling 0)

             ((node-is "in") parent 0)
             ((match nil "let_in" "expression" nil nil)
              ,(lambda (node parent bol)
                 (let* ((in-node (treesit-node-child parent 2))
                        (in-node-start (treesit-node-start in-node))
                        (parent-start (treesit-node-start parent)))
                   (if (haskell-ts--positions-on-the-same-line? in-node-start parent-start)
                       parent
                     in-node)))
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "let_in" (treesit-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             ;; list
             ((node-is "]") parent 0)
             ((n-p-gp "," "list" nil) parent 0)
             ((parent-is "list") parent haskell-indent-offset)

             ((parent-is "apply")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-parent
              haskell-indent-offset)

             ((node-is "quasiquote") grand-parent haskell-indent-offset)
             ((parent-is "quasiquote_body") (lambda (_ _ c) c) 0)

             ;; ((lambda (node parent bol)
             ;;    (let ((n (treesit-node-prev-sibling node)))
             ;;      (while (string= "comment" (treesit-node-type n))
             ;;        (setq n (treesit-node-prev-sibling n)))
             ;;      (string= "do" (treesit-node-type n))))
             ;;  haskell-ts-indent--standalone-vertical-infix-operator-parent
             ;;  haskell-indent-offset)
             ;; ((parent-is "do") haskell-ts-indent--prev-sib 0)

             ((parent-is "do")
              ,(lambda (n p bol)
                 (when-let* ((matched-anchor
                              (haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-parent n p bol)))
                   (if (string= "do" (treesit-node-type matched-anchor))
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
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update-no-list-parent
              haskell-indent-offset)
             ((parent-is "alternatives") haskell-ts-indent--prev-sib 0)

             ;; Here node is typically nil but we don’t want to match the ‘no-node’ rule below.
             ((parent-is "string") haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update-no-list-parent 0)

             (no-node prev-adaptive-prefix 0)

             ((parent-is "data_constructors") grand-parent haskell-indent-offset)
             ((node-is "gadt_constructors") parent haskell-indent-offset)
             ((parent-is "gadt_constructors") grand-parent haskell-indent-offset)

             ;; instance

             ;; first instance function
             ((node-is "instance_declarations")
              parent
              haskell-indent-offset)

             ;; subsequent instance functions
             ((parent-is "instance_declarations")
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
             ;; Also handles binds within let.
             ((parent-is "local_binds")
              haskell-ts-indent--prev-sib
              0)
             ((node-is "where") parent haskell-indent-offset)

             ;; Must come after where because parents of comments are sometimes
             ;; incorrect and comment under ‘where’ may be attributed to the enclosing
             ;; function.
             ((or (parent-is "record")
                  (node-is "comment" "haddock"))
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "field_name" (treesit-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             ((n-p-gp "match" '("bind" "multi_way_if") nil)
              haskell-ts-indent--first-guard-or-parent
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "|" (treesit-node-type matched-anchor))
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
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function
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

             ((n-p-gp '("," ")" "#)") '("tuple" "unboxed_tuple") nil) parent 0)

             ((and (parent-is "tuple" "unboxed_tuple")
                   (field-is "element"))
              parent
              haskell-indent-offset)

             ((node-is "deriving") parent haskell-indent-offset)

             ((n-p-gp "->" "function" nil)
              haskell-ts-indent--type-function-anchor
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "::" (treesit-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             ((n-p-gp "|]" "quasiquote" nil)
              parent
              0)

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
              (list (cons 'haskell haskell-ts-indent-rules))))

(provide 'haskell-ts-indent)

;; Local Variables:
;; End:

;; haskell-ts-indent.el ends here
