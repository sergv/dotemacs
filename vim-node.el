;;; vim-node.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-node)

(defstruct (vim:node
            (:constructor vim:make-node))
  key         ; The key bound to this node
  next        ; A map of the following commands
  next-keymap ; The following keymap if 'next' has no continuation.
  cmd         ; The command bound to this node, an arbitrary meta information.
  function    ; If non-nil this function is call when the is activated.
  )


(defun vim:get-subnode (node key)
  "Returns the sub-node associated with key."
  (let ((sub-node (assoc key (vim:node-next node))))
    (if sub-node
        (cdr sub-node)
      nil)))


(defun* vim:add-node (node keys cmd &key
                           (next-keymap nil)
                           (function nil))
  "Adds the command bound to sequence chars to node."

  (labels
      ((add-node (node pos len cmd n-keymap func)
                 (case (- len pos)
                   (0 (error "Can't add empty key-sequence to map."))
   
                   (1
                    (let* ((key (elt keys pos))
                           (subnode (vim:get-subnode node key)))
                      (if subnode
                          (progn
                            (setf (vim:node-cmd subnode) cmd)
                            (setf (vim:node-next-keymap subnode) n-keymap)
                            (setf (vim:node-function subnode) func))
                        (progn
                          (setq subnode (vim:make-node :key key
                                                       :cmd cmd
                                                       :next-keymap n-keymap
                                                       :function func))
                          (setf (vim:node-next node)
                                (cons (cons key subnode) (vim:node-next node)))))
                      subnode))
                   
                   (t
                    (let* ((key (elt keys pos))
                           ;; get or create the next node
                           (parent (or (vim:get-subnode node key)
                                       (add-node node pos 1 nil nil nil))))
                      (add-node parent (1+ pos) len cmd n-keymap func))))))
    
    (add-node node 0 (length keys) cmd next-keymap function)))



