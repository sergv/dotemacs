;;; treesit-sexp.el --- Tree-sitter based s-expression navigation -*- lexical-binding: t -*-

;; SPDX-License-Identifier: 0BSD
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; Author: Alexis Purslane
;; Version: 0.1.0
;; Keywords: convenience, lisp, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides tree-sitter based s-expression movement functions
;; that respect syntax structure.  It moves by semantic units rather than
;; character positions, skipping over punctuation like commas and semicolons
;; while honoring delimiter boundaries.

;; The main functions are:
;;   `treesit-sexp-forward' - move forward by s-expressions
;;   `treesit-sexp-backward' - move backward by s-expressions

(require 'treesit-utils)

;;; Code:

(defun treesit-sexp--find-inner-node (node pos)
  "Find the innermost node that contains POS by walking up from NODE."
  (let ((n node))
    (while (and n
                (not (treesit-utils-is-inside-node? pos n)))
      (setq n (treesit-node-parent n)))
    n))

(defun treesit-sexp--walk-parents (curr direction inner boundary)
  "Walk up parent chain from CURR based on DIRECTION and BOUNDARY."
  (let ((compare-pos (if (eq direction 'forward) '>= '<)))
    (while (let ((p (treesit-node-parent curr)))
             (and p (not (treesit-node-eq p inner))
                  (funcall compare-pos
                           (if (eq direction 'forward)
                               (treesit-node-start p)
                             (treesit-node-end p))
                           boundary)))
      (setq curr (treesit-node-parent curr)))
    (if (eq direction 'forward)
        (treesit-node-end curr)
      (treesit-node-start curr))))

(defconst treesit-sexp--punctuation-node-type '("," "." ";" "->"))

(defconst treesit-sexp--forward-wall-node-type '("}" ")" "]" ">"))
(defconst treesit-sexp--backward-wall-node-type '("(" "{" "[" "<"))

(defun treesit-sexp--forward-sexp-in-node (node direction pos)
  "Handle sexp finding when there’s node around us."
  (message "treesit-sexp--forward-sexp-in-node: node = %s, direction = %s, pos = %s"
           (pp-to-string node)
           (pp-to-string direction)
           (pp-to-string pos))
  (let ((target-sexp nil)
        (i (if (eq direction 'forward) 0 (1- (treesit-node-child-count node))))
        (step (if (eq direction 'forward) #'1+ #'1-))
        (compare-start (if (eq direction 'forward) #'>= #'<))
        (compare-pos (if (eq direction 'forward) #'>= #'<))
        (wall-types (if (eq direction 'forward)
                        treesit-sexp--forward-wall-node-type
                      treesit-sexp--backward-wall-node-type)))

    (while (and (if (eq direction 'forward)
                    (< i (treesit-node-child-count node))
                  (>= i 0))
                (not target-sexp))
      (let ((c (treesit-node-child node i)))
        (message "looking at child %s: c = %s"
                 i
                 (pp-to-string c))
        (when (funcall compare-start (treesit-node-start c) pos)
          (let ((type (treesit-node-type c))
                (named? (treesit-node-check c 'named)))
            (cond
              ((and (not named?) (member type wall-types))
               (throw 'hit-wall nil))
              ((and (not named?) (member type treesit-sexp--punctuation-node-type))
               nil)
              (t (setq target-sexp c)))))
        (setq i (funcall step i))))

    (message "target-sexp = %s"
             (pp-to-string target-sexp))

    (cond
      (target-sexp
       (treesit-sexp--walk-parents
        target-sexp
        direction
        node
        (if (eq direction 'forward)
            (treesit-node-start target-sexp)
          (treesit-node-end target-sexp))))
      ((eq direction 'forward)
       (treesit-node-end node))
      (t
       (treesit-node-start node)))))

(defun treesit-sexp--find-fallback (node direction pos)
  "Find sexp when not inside an inner node.
Searches siblings and then parent tree for valid sexp at/after POS."
  (let ((compare-pos (if (eq direction 'forward) '>= '<))
        (compare-start (if (eq direction 'forward) '< '>))
        (next-sibling-func (if (eq direction 'forward)
                               'treesit-node-next-sibling
                             'treesit-node-prev-sibling)))
    (let ((n node)
          target)
      (while (and n
                  (or (funcall compare-start (treesit-node-start n) pos)
                      (and (not (treesit-node-check n 'named))
                           (member (treesit-node-type n) treesit-sexp--punctuation-node-type))))
        (setq n (or (funcall next-sibling-func n) (treesit-node-parent n))))

      (if (and n (funcall compare-pos (treesit-node-start n) pos))
          (treesit-sexp--walk-parents
           n direction nil
           (if (eq direction 'forward)
               (treesit-node-start n)
             (treesit-node-end n)))
        (if (eq direction 'forward)
            (point-max)
          (point-min))))))

(defun treesit-sexp--treesit-available-p ()
  "Check if tree-sitter is available and the current buffer has a parser."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-parser-list)))

;; (defun treesit-sexp-forward (&optional arg interactive)
;;   "Move forward ARG s-expressions, skipping punctuation like , . : ;.
;; Stops at `Wall' delimiters ( ) { } [ ] without signaling an error.
;; When TreeSitter is not available, falls back to `forward-sexp'."
;;   (interactive "p")
;;   (if (not (treesit-sexp--treesit-available-p))
;;       (let ((forward-sexp-function (if (eq forward-sexp-function #'treesit-sexp-forward)
;;                                        nil
;;                                      forward-sexp-function)))
;;         (forward-sexp arg interactive))
;;     (let ((count (abs (or arg 1)))
;;           (direction (if (< (or arg 0) 0) 'backward 'forward)))
;;       (catch 'hit-wall
;;         (dotimes (_ count)
;;           (message "DEBUG = %s"
;;                    (pp-to-string (debug-current-line)))
;;           (let* ((pos (point))
;;                  (lookup-pos (if (eq direction 'backward) (1- pos) pos))
;;                  (node (treesit-node-at lookup-pos))
;;                  (inner (treesit-sexp--find-inner-node node pos))
;;                  (target (if inner
;;                              (treesit-sexp--forward-sexp-in-node inner direction pos)
;;                            (treesit-sexp--find-fallback node direction pos))))
;;             (when target
;;               (if (eq direction 'forward)
;;                   (when (> target (point))
;;                     (goto-char (min target (point-max))))
;;                 (when (< target (point))
;;                   (goto-char (max target (point-min))))))))))))
;;
;; (defun treesit-sexp-backward (&optional arg interactive)
;;   "Move backward ARG s-expressions, skipping punctuation like , . : ;.
;; Stops at `Wall' delimiters ( { [ < without signaling an error.
;; When TreeSitter is not available, falls back to `backward-sexp'."
;;   (interactive "p")
;;   (if (not (treesit-sexp--treesit-available-p))
;;       (backward-sexp arg interactive)
;;     (treesit-sexp-forward (- (or arg 1)) interactive)))

(defun treesit-sexp-up-list (&optional arg escape-strings no-syntax-crossing)
  "Move forward out of ARG levels of parentheses.
When TreeSitter is not available or when called interactively,
falls back to `up-list'.

With negative ARG, move backward but still to a shallower spot.
If ESCAPE-STRINGS is non-nil, move out of enclosing strings as well.
If NO-SYNTAX-CROSSING is non-nil, prefer to break out of any enclosing
string instead of moving to the end of a list broken across multiple strings."
  (interactive "p")
  (if (or (not (treesit-sexp--treesit-available-p)) (called-interactively-p 'any))
      (up-list arg escape-strings no-syntax-crossing)
    (let ((count (abs (or arg 1)))
          (direction (if (< (or arg 0) 0) 'backward 'forward)))
      (dotimes (_ count)
        (let* ((pos (point))
               (node (treesit-node-at pos)))
          (if (eq direction 'forward)
              (if-let ((parent (treesit-node-parent node)))
                  (goto-char (treesit-node-end parent))
                (goto-char (point-max)))
            (let ((inner (treesit-sexp--find-inner-node node pos)))
              (if inner
                  (if (= pos (treesit-node-start inner))
                      (when-let ((parent (treesit-node-parent inner)))
                        (goto-char (treesit-node-start parent)))
                    (goto-char (treesit-node-start inner)))
                (when-let ((parent (treesit-node-parent node)))
                  (goto-char (treesit-node-start parent)))))))))))

(defun treesit-sexp-forward-list (&optional arg interactive)
  "Move forward across one balanced group of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do it that many times.
Negative arg -N means move backward across N groups of parentheses.
When TreeSitter is not available, falls back to `forward-list'."
  (interactive "^p\nd")
  (if (or (not (treesit-sexp--treesit-available-p)) interactive)
      (forward-list arg interactive)
    (or arg (setq arg 1))
    (let ((count (abs arg))
          (direction (if (< arg 0) 'backward 'forward))
          (wall-types (eval-when-compile
                        (append treesit-sexp--forward-wall-node-type
                                treesit-sexp--forward-wall-node-type))))
      (dotimes (_ count)
        (let* ((pos (point))
               (node (treesit-node-at pos))
               (inner (treesit-sexp--find-inner-node node pos))
               (target (or inner node))
               (sibling-func (if (eq direction 'forward) 'treesit-node-next-sibling 'treesit-node-prev-sibling))
               (next target)
               (found nil))
          (while (and next (not found))
            (let ((next-sibling (funcall sibling-func next)))
              (message "next-sibling = %s"
                       (pp-to-string next-sibling))
              (if next-sibling
                  (progn
                    (setq next next-sibling)
                    (when (and (member (treesit-node-type next) wall-types)
                               (if (eq direction 'forward)
                                   (> (treesit-node-start next) pos)
                                 (< (treesit-node-end next) pos)))
                      (setq found next)))
                (setq next nil))))
          (when found
            (goto-char (if (eq direction 'forward)
                           (treesit-node-end found)
                         (treesit-node-start found)))))))))

(defun treesit-sexp-backward-list (&optional arg interactive)
  "Move backward across one balanced group of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do it that many times.
Negative arg -N means move forward across N groups of parentheses.
When TreeSitter is not available, falls back to `backward-list'."
  (interactive "^p\nd")
  (or arg (setq arg 1))
  (if (or (not (treesit-sexp--treesit-available-p)) interactive)
      (backward-list arg interactive)
    (treesit-sexp-forward-list (- arg) interactive)))

(defun treesit-sexp-down-list (&optional arg interactive)
  "Move forward down one level of parentheses.
Find the next sibling node with children and position inside it."
  (interactive "^p\nd")
  (if (or (not (treesit-sexp--treesit-available-p)) interactive)
      (down-list arg interactive)
    (or arg (setq arg 1))
    (let ((direction (if (> arg 0) 'forward 'backward))
          (count (abs arg)))
      (dotimes (_ count)
        (let* ((node (treesit-node-at (point)))
               (target-node
                (catch 'found
                  (let ((curr (if (eq direction 'forward)
                                  (treesit-node-next-sibling node)
                                (treesit-node-prev-sibling node))))
                    (while curr
                      (when (> (treesit-node-child-count curr) 0)
                        (throw 'found curr))
                      (setq curr (if (eq direction 'forward)
                                     (treesit-node-next-sibling curr)
                                   (treesit-node-prev-sibling curr))))))))
          (when target-node
            (goto-char (if (eq direction 'forward)
                           (1+ (treesit-node-start target-node))
                         (1- (treesit-node-end target-node))))))))))

(defun treesit-sexp-mark-sexp (&optional arg allow-extend)
  "Set mark ARG sexps from point or move mark one sexp.
When called from Lisp with ALLOW-EXTEND omitted or nil, mark is
set ARG sexps from point.
With ARG and ALLOW-EXTEND both non-nil (interactively, with prefix
argument), the place to which mark goes is the same place `treesit-sexp-forward'
would move to with the same argument; if the mark is active, it moves
ARG sexps from its current position, otherwise it is set ARG sexps
from point.
When invoked interactively without a prefix argument and no active
region, mark moves one sexp forward.
When invoked interactively without a prefix argument, and region
is active, mark moves one sexp away of point (i.e., forward
if mark is at or after point, back if mark is before point), thus
extending the region by one sexp.  Since the direction of region
extension depends on the relative position of mark and point, you
can change the direction by `exchange-point-and-mark'.
When TreeSitter is not available, falls back to `mark-sexp'."
  (interactive "P\np")
  (if (not (treesit-sexp--treesit-available-p))
      (mark-sexp arg allow-extend)
    (cond ((and allow-extend
                (or (and (eq last-command this-command) (mark t))
                    (and transient-mark-mode mark-active)))
           (setq arg (if arg (prefix-numeric-value arg)
                       (if (< (mark) (point)) -1 1)))
           (set-mark
            (save-excursion
              (goto-char (mark))
              (condition-case error
                  (treesit-sexp-forward arg)
                ((scan-error)
                 (user-error (if (equal (cadr error)
                                        "Containing expression ends prematurely")
                                 "No more sexp to select"
                               (cadr error)))))
              (point))))
          (t
           (push-mark
            (save-excursion
              (condition-case error
                  (treesit-sexp-forward (prefix-numeric-value arg))
                ((scan-error)
                 (user-error (if (equal (cadr error)
                                        "Containing expression ends prematurely")
                                 "No sexp to select"
                               (cadr error)))))
              (point))
            nil t)))))

(defun treesit-sexp-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
With ARG, do it that many times.  Negative ARG means move forward
to the ARGth following beginning of defun.
When TreeSitter is not available or cannot find function boundaries,
falls back to `beginning-of-defun'."
  (interactive "^p")
  (if (or (not (treesit-sexp--treesit-available-p)) (called-interactively-p 'any))
      (beginning-of-defun arg)
    (or arg (setq arg 1))
    (let ((count (abs arg))
          (direction (if (< arg 0) 'forward 'backward))
          (defun-types '("function_definition" "function_declaration" "function_item" "method_definition" "class_declaration" "class_definition" "impl_item" "interface_declaration" "type_declaration")))
      (dotimes (_ count)
        (let* ((pos (point))
               (node (treesit-node-at pos))
               (found nil))
          (if (eq direction 'backward)
              (while (and node (not found))
                (let ((parent (treesit-node-parent node)))
                  (if parent
                      (if (member (treesit-node-type parent) defun-types)
                          (setq found parent)
                        (setq node parent))
                    (setq node nil))))
            (while (and node (not found))
              (let ((next-sibling (treesit-node-next-sibling node)))
                (if next-sibling
                    (if (member (treesit-node-type next-sibling) defun-types)
                        (setq found next-sibling)
                      (setq node next-sibling))
                  (setq node nil)))))
          (when found
            (goto-char (treesit-node-start found))
            (beginning-of-line)))))))

(defun treesit-sexp-end-of-defun (&optional arg interactive)
  "Move forward to next end of defun.
With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.
When TreeSitter is not available or cannot find function boundaries,
falls back to `end-of-defun'."
  (interactive "^p\nd")
  (if (or (not (treesit-sexp--treesit-available-p)) interactive (called-interactively-p 'any))
      (end-of-defun arg interactive)
    (or (not (eq this-command 'treesit-sexp-end-of-defun))
        (eq last-command 'treesit-sexp-end-of-defun)
        (and transient-mark-mode mark-active)
        (push-mark))
    (if (or (null arg) (= arg 0)) (setq arg 1))
    (let ((count (abs arg))
          (direction (if (< arg 0) 'backward 'forward))
          (defun-types '("function_definition" "function_declaration" "function_item" "method_definition" "class_declaration" "class_definition" "impl_item" "interface_declaration" "type_declaration")))
      (dotimes (_ count)
        (let* ((pos (point))
               (node (treesit-node-at pos))
               (found nil))
          (if (eq direction 'forward)
              (while (and node (not found))
                (let ((parent (treesit-node-parent node)))
                  (if parent
                      (if (member (treesit-node-type parent) defun-types)
                          (setq found parent)
                        (setq node parent))
                    (setq node nil))))
            (while (and node (not found))
              (let ((prev-sibling (treesit-node-prev-sibling node)))
                (if prev-sibling
                    (if (member (treesit-node-type prev-sibling) defun-types)
                        (setq found prev-sibling)
                      (setq node prev-sibling))
                  (setq node nil)))))
          (when found
            (goto-char (treesit-node-end found))
            (skip-syntax-forward " ")
            (when (eq (char-after) ?\n) (forward-char 1))))))))

;;;###autoload
(define-minor-mode treesit-sexp-mode
  "Minor mode for tree-sitter based sexp navigation.
When enabled, sexp navigation functions use tree-sitter for better
accuracy when tree-sitter parsers are available."
  :global nil
  :require 'treesit-sexp
  :group 'treesit-sexp
  (if treesit-sexp-mode
      (progn
        (setq-local paredit-forward-sexp-function #'treesit-sexp-forward)
        (setq-local forward-sexp-function 'treesit-sexp-forward)
        (setq-local forward-list-function 'treesit-sexp-forward-list)
        (setq-local backward-list-function 'treesit-sexp-backward-list)
        (setq-local up-list-function 'treesit-sexp-up-list)
        (setq-local down-list-function 'treesit-sexp-down-list)
        (setq-local mark-sexp-function 'treesit-sexp-mark-sexp)
        (setq-local beginning-of-defun-function 'treesit-sexp-beginning-of-defun)
        (setq-local end-of-defun-function 'treesit-sexp-end-of-defun))
    (kill-local-variable 'forward-sexp-function)
    (kill-local-variable 'forward-list-function)
    (kill-local-variable 'backward-list-function)
    (kill-local-variable 'up-list-function)
    (kill-local-variable 'down-list-function)
    (kill-local-variable 'mark-sexp-function)
    (kill-local-variable 'beginning-of-defun-function)
    (kill-local-variable 'end-of-defun-function)))

;;;###autoload
(defun treesit-sexp-global-mode-setup ()
  "Setup treesit-sexp-mode for tree-sitter enabled buffers."
  (when (treesit-sexp--treesit-available-p)
    (treesit-sexp-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-treesit-sexp-mode treesit-sexp-mode
  treesit-sexp-global-mode-setup
  :require 'treesit-sexp
  :group 'treesit-sexp)


;; forward-sexp
;; (foo _|_bar baz) -> (foo bar_|_ baz)
;; (foo _|_(bar quux) baz) -> (foo (bar quux)_|_ baz)
;; (_|_foo (bar quux) baz) -> (foo_|_ (bar quux) baz)
;;
;; forward-list
;; (foo _|_bar baz) -> ERROR
;; (foo _|_(bar quux) baz) -> (foo (bar quux)_|_ baz)
;; (_|_foo (bar quux) baz) -> (foo (bar quux)_|_ baz)

(defun treesit-sexp--is-atom-node? (node)
  (eq 0 (treesit-node-child-count node)))

(defun treesit-sexp--is-phantom-node? (node)
  (eq (treesit-node-start node)
      (treesit-node-end node)))

;; (local-set-key (key-parse "<f8>") #'treesit-sexp-forward-sexp)
;; (progn
;;   (global-set-key (key-parse "<f8>") #'treesit-sexp-forward-sexp)
;;   (global-set-key (key-parse "<f7>") #'treesit-sexp-backward-sexp)
;;
;;   (global-set-key (key-parse "<f4>") #'forward-sexp)
;;   (global-set-key (key-parse "<f3>") #'backward-sexp))


(defvar treesit-sexp-open-node-types '("(" "[" "{"))
(defvar treesit-sexp-close-node-types '(")" "]" "}"))

(defvar treesit-sexp-delimiter-node-types
  (append treesit-sexp-open-node-types
          treesit-sexp-close-node-types))

(defun treesit-sexp-backward-sexp (&optional count)
  (interactive "p")
  (treesit-sexp-forward-sexp (- count)))

(defun treesit-sexp--node-at-strictly-after (pos &optional parser-or-lang named)
  (let ((node (treesit-node-at pos)))
    node
    ;; (if (<= pos (treesit-node-start node))
    ;;     node)
    ))

(defun treesit-sexp--node-at-strictly-before (pos &optional parser-or-lang named)
  (let ((node (treesit-node-at-backwards pos)))
    node
    ))

(defun treesit-node-at-backwards (pos &optional parser-or-lang named)
  "Return the leaf node at position POS.

A leaf node is a node that doesn't have any child nodes.

The returned node's span covers POS: the node's beginning is before
or at POS, and the node's end is after POS.

If no such node exists, but there's a leaf node which ends at POS,
return that node.

Otherwise (e.g., when POS is on whitespace between two leaf
nodes), return the first leaf node after POS.

If there is no leaf node after POS, return the first leaf node
before POS.

Return nil if no leaf node can be returned.  If NAMED is non-nil,
only look for named nodes.

If PARSER-OR-LANG is a parser, use that parser; if PARSER-OR-LANG
is a language, find the first parser for that language in the
current buffer, or create one if none exists; If PARSER-OR-LANG
is nil, try to guess the language at POS using `treesit-language-at'.

If there's a local parser at POS, the local parser takes priority
unless PARSER-OR-LANG is a parser, or PARSER-OR-LANG is a
language and doesn't match the language of the local parser."
  (let* ((root
          ;; 1. Given a parser, just use the parser's root node.
          (cond ((treesit-parser-p parser-or-lang)
                 (treesit-parser-root-node parser-or-lang))
                ;; 2. Given a language, try local parser, then global
                ;; parser.
                (parser-or-lang
                 (let ((parser (car (treesit-parsers-at
                                     pos parser-or-lang))))
                   (when parser
                     (treesit-parser-root-node parser))))
                ;; 3. No given language, try to get a language at point.
                ;; If we got a language, only use parser of that
                ;; language, otherwise use any parser we can find.  When
                ;; finding parser, try local parser first, then global
                ;; parser.
                (t
                 ;; LANG can be nil.  Use the parser deepest by embed level.
                 (let ((parser (car (treesit-parsers-at pos))))
                   (when parser
                     (treesit-parser-root-node parser))))))
         (node root)
         (node-after root)
         (pos-1 (max (1- pos) (point-min)))
         next)
    (when node
      ;; This is very fast so no need for C implementation.
      (while (setq next (treesit-node-first-child-for-pos
                         node pos-1 named))
        (setq node next))
      ;; If POS is at the start of buffer, after all the text, we will
      ;; end up with NODE = root node.  Instead of returning nil,
      ;; return the first leaf node in the tree for convenience.
      (if (treesit-node-eq node root)
          (progn
            (while (setq next (treesit-node-child node 1 named))
              (setq node next))
            node)
        ;; Normal case, where we found a node.
        (if (<= (treesit-node-end node) pos)
            node
          ;; So the node we found is completely after POS, try to find
          ;; a node whose end equals to POS.
          (while (setq next (treesit-node-first-child-for-pos
                             node-after pos named))
            (setq node-after next))
          (if (eq (treesit-node-start node-after) pos)
              node-after
            node))))))

;; (defun treesit-sexp--node-at-strictly-after (pos &optional parser-or-lang named)
;;   "Like ‘treesit-node-at’ but when point is at node end it returns the next node"
;;   (let* ((root
;;           ;; 1. Given a parser, just use the parser's root node.
;;           (cond ((treesit-parser-p parser-or-lang)
;;                  (treesit-parser-root-node parser-or-lang))
;;                 ;; 2. Given a language, try local parser, then global
;;                 ;; parser.
;;                 (parser-or-lang
;;                  (let ((parser (car (treesit-parsers-at
;;                                      pos parser-or-lang))))
;;                    (when parser
;;                      (treesit-parser-root-node parser))))
;;                 ;; 3. No given language, try to get a language at point.
;;                 ;; If we got a language, only use parser of that
;;                 ;; language, otherwise use any parser we can find.  When
;;                 ;; finding parser, try local parser first, then global
;;                 ;; parser.
;;                 (t
;;                  ;; LANG can be nil.  Use the parser deepest by embed level.
;;                  (let ((parser (car (treesit-parsers-at pos))))
;;                    (when parser
;;                      (treesit-parser-root-node parser))))))
;;          (node root)
;;          ;; (node-before root)
;;          (pos-1 (max (1- pos) (point-min)))
;;          next)
;;     (when node
;;       ;; This is very fast so no need for C implementation.
;;       (while (setq next (treesit-node-first-child-for-pos
;;                          node pos named))
;;         (setq node next))
;;       ;; If POS is at the end of buffer, after all the text, we will
;;       ;; end up with NODE = root node.  Instead of returning nil,
;;       ;; return the last leaf node in the tree for convenience.
;;       (if (treesit-node-eq node root)
;;           (progn
;;             (while (setq next (treesit-node-child node -1 named))
;;               (setq node next))
;;             node)
;;         node
;;         ;; Normal case, where we found a node.
;;         (if (<= (treesit-node-start node) pos)
;;             node
;;           ;; So the node we found is completely after POS, try to find
;;           ;; a node whose end equals to POS.
;;           ;; (while (setq next (treesit-node-first-child-for-pos
;;           ;;                    node-before pos-1 named))
;;           ;;   (setq node-before next))
;;           node
;;           ;; (if (eq (treesit-node-end node-before) pos)
;;           ;;     node-before
;;           ;;   node)
;;           )))))
;;
;; (defun treesit-sexp--node-at-strictly-before (pos &optional parser-or-lang named)
;;   "Like ‘treesit-node-at’ but try to always return a node before point"
;;   (let* ((root
;;           ;; 1. Given a parser, just use the parser's root node.
;;           (cond ((treesit-parser-p parser-or-lang)
;;                  (treesit-parser-root-node parser-or-lang))
;;                 ;; 2. Given a language, try local parser, then global
;;                 ;; parser.
;;                 (parser-or-lang
;;                  (let ((parser (car (treesit-parsers-at
;;                                      pos parser-or-lang))))
;;                    (when parser
;;                      (treesit-parser-root-node parser))))
;;                 ;; 3. No given language, try to get a language at point.
;;                 ;; If we got a language, only use parser of that
;;                 ;; language, otherwise use any parser we can find.  When
;;                 ;; finding parser, try local parser first, then global
;;                 ;; parser.
;;                 (t
;;                  ;; LANG can be nil.  Use the parser deepest by embed level.
;;                  (let ((parser (car (treesit-parsers-at pos))))
;;                    (when parser
;;                      (treesit-parser-root-node parser))))))
;;          (node root)
;;          (node-before root)
;;          (pos-1 (max (1- pos) (point-min)))
;;          next)
;;     (when node
;;       ;; This is very fast so no need for C implementation.
;;       (while (setq next (treesit-node-first-child-for-pos
;;                          node pos named))
;;         (setq node next))
;;       ;; If POS is at the end of buffer, after all the text, we will
;;       ;; end up with NODE = root node.  Instead of returning nil,
;;       ;; return the last leaf node in the tree for convenience.
;;       (if (treesit-node-eq node root)
;;           (progn
;;             (while (setq next (treesit-node-child node -1 named))
;;               (setq node next))
;;             node)
;;         ;; Normal case, where we found a node.
;;         (if (<= (treesit-node-start node) pos)
;;             node
;;           ;; So the node we found is completely after POS, try to find
;;           ;; a node whose end equals to POS.
;;           (while (setq next (treesit-node-first-child-for-pos
;;                              node-before pos-1 named))
;;             (setq node-before next))
;;           node-before
;;           ;; (if (eq (treesit-node-end node-before) pos)
;;           ;;     node-before
;;           ;;   node)
;;           )))))
;;

(defun treesit-sexp-forward-sexp--get-initial-node (backward?)
  (let*
      ((parser-or-lang nil)
       (pos (if backward?
                (- (point) 1)
              (point)))
       (root
        ;; 1. Given a parser, just use the parser's root node.
        (cond ((treesit-parser-p parser-or-lang)
               (treesit-parser-root-node parser-or-lang))
              ;; 2. Given a language, try local parser, then global
              ;; parser.
              (parser-or-lang
               (let ((parser (car (treesit-parsers-at
                                   pos parser-or-lang))))
                 (when parser
                   (treesit-parser-root-node parser))))
              ;; 3. No given language, try to get a language at point.
              ;; If we got a language, only use parser of that
              ;; language, otherwise use any parser we can find.  When
              ;; finding parser, try local parser first, then global
              ;; parser.
              (t
               ;; LANG can be nil.  Use the parser deepest by embed level.
               (let ((parser (car (treesit-parsers-at pos))))
                 (when parser
                   (treesit-parser-root-node parser))))))
       (covering-node
        (treesit-node-descendant-for-range
         root
         pos
         pos
         nil))
       (initial-node nil))
    (if (treesit-sexp--is-atom-node? covering-node)
        (setf initial-node covering-node)
      (let* ((continue? t)
             (children-count (treesit-node-child-count covering-node))
             (i (if backward?
                    (- children-count 1)
                  0))
             (limit (if backward?
                        -1
                      children-count)))
        (if backward?
            (while (and continue?
                        (< limit i))
              (let ((node (treesit-node-child covering-node i)))
                (when (<= (treesit-node-end node) pos)
                  (setf continue? nil
                        initial-node node))
                (setf i (- i 1))))
          (while (and continue?
                      (< i limit))
            (let ((node (treesit-node-child covering-node i)))
              (when (<= pos (treesit-node-start node))
                (setf continue? nil
                      initial-node node))
              (setf i (+ i 1)))))))
    (when initial-node
      (if (treesit-sexp--is-atom-node? initial-node)
          initial-node
        (progn
          (if backward?
              (while (not (treesit-sexp--is-atom-node? initial-node))
                (setq initial-node (treesit-sexp-last-non-phantom-non-named-child initial-node)
                      ))
            (while (not (treesit-sexp--is-atom-node? initial-node))

              (setq initial-node (treesit-node-child initial-node
                                                     0
                                                     nil ;; not only named
                                                     ))))
          initial-node)))))

(defun treesit-sexp-last-non-phantom-non-named-child (node)
  (let ((continue? t)
        (result nil)
        (i (- (treesit-node-child-count node) 1)))
    (while (and continue?
                (< -1 i))
      (let ((child (treesit-node-child node i)))
        (unless (treesit-sexp--is-phantom-node? child)
          (setf continue? nil
                result child))
        (setf i (- i 1))))
    result))

(defun treesit-sexp-forward-sexp (&optional count)
  (interactive "p")
  (let* ((i 0)
         (limit (abs (or count 1)))
         (backward? (and count
                         (< count 0)))
         (continue? t)
         (curr-node (treesit-sexp-forward-sexp--get-initial-node backward?)))
    (message "point = %s, start-node = %s"
             (point)
             (pp-to-string curr-node))
    (if (not curr-node)
        (goto-char (if backward?
                       (point-min)
                     (point-max)))
      (while (and continue?
                  (< i limit))
        (let* ((p nil))
          (when-let* ((new-pos-and-node
                       (treesit-search-forward-goto2 curr-node
                                                     #'treesit-sexp--is-atom-node?
                                                     backward?)))
            (aif new-pos-and-node
                (progn
                  (goto-char (car it))
                  (setf curr-node (cdr it)))
              (setf curr-node nil)))
          (if curr-node
              (let ((typ (treesit-node-type curr-node)))
                (cond
                  ((and (member typ (if backward?
                                        treesit-sexp-close-node-types
                                      treesit-sexp-open-node-types))
                        (not (null (setq p (treesit-node-parent curr-node)))))
                   (goto-char (if backward?
                                  (treesit-node-start p)
                                (treesit-node-end p))))
                  ((member typ (if backward?
                                   treesit-sexp-open-node-types
                                 treesit-sexp-close-node-types))
                   ;; Report that we reached the end the way ‘forward-sexp’ would report that.
                   (user-error (if (> count 0)
                                   "No next sexp"
                                 "No previous sexp")))
                  (t
                   )))
            (setf continue? nil)))
        (setf i (+ i 1))))))

(defun treesit-search-forward-goto2
    (node predicate backward?)
  "Search forward for a node and move to its end position.

Stop at the first node after NODE that matches PREDICATE.
PREDICATE can be either a regexp that matches against each node's
type case-insensitively, or a function that takes a node and
returns nil/non-nil for match/no match.

If a node matches, move to that node and return the node,
otherwise return nil.  If START is non-nil, stop at the
beginning rather than the end of a node.

This function guarantees that the matched node it returns makes
progress in terms of buffer position: the start/end position of
the returned node is always STRICTLY greater/less than that of
NODE.

BACKWARD? is the same as in `treesit-search-forward'."
  (message "treesit-search-forward-goto2: started, node = %s" node)
  (cond
    ((and node
          (member (treesit-node-type node)
                  treesit-sexp-delimiter-node-types))
     (cons (point) node))
    ((and node
          (if backward?
          (eq (point) (treesit-node-end node))
        (eq (point) (treesit-node-start node))))
     (cons (if backward?
               (treesit-node-start node)
             (treesit-node-end node))
           node))
    (t
     (when-let* ((start-pos (if backward?
                                (treesit-node-start node)
                              (treesit-node-end node)))
                 (current-pos start-pos))
       ;; When searching forward and stopping at beginnings, or search
       ;; backward stopping at ends, it is possible to "roll back" in
       ;; position.  Take three nodes N1, N2, N3 as an example, if we
       ;; start at N3, search for forward for beginning, and N1 matches,
       ;; we would stop at beg of N1, which is backwards!  So we skip N1
       ;; and keep going.
       ;;
       ;;   |<--------N1------->|
       ;;   |<--N2-->| |<--N3-->|
       (let ((continue? t))
         (while (and continue?
                     node
                     (if backward?
                         (>= current-pos start-pos)
                       (<= current-pos start-pos)))
           (message "before search: node = %s, current-pos = %s, start-pos = %s"
                    node
                    current-pos
                    start-pos)
           (setq node (treesit-search-forward node
                                              predicate
                                              backward?
                                              t ;; search for all nodes, not only named
                                              ))

           (message "after search: node = %s, current-pos = %s, start-pos = %s"
                    node
                    current-pos
                    start-pos)
           (setq current-pos (if backward?
                                 (treesit-node-start node)
                               (treesit-node-end node)))))
       (message "treesit-search-forward-goto2: done, current-pos = %s" current-pos)
       (when (and node
                  (if backward?
                      (< current-pos (point))
                    (> current-pos (point))))
         ;; When there is a match and match made progress, go to the
         ;; result position.
         (cons (if backward?
                   (treesit-node-end node)
                 (treesit-node-start node))
               node))))))

(provide 'treesit-sexp)

;;; treesit-sexp.el ends here
