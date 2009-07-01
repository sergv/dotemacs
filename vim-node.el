(provide 'vim-node)

;; The following types of commands are supported
;;
;;  - simple: A command without motion.
;;
;;  - simple-arg: A command without motion and with argument.
;;
;;  - complex: A operator with motion.
;;
;;  - motion: A motion command without argument.
;;
;;  - motion-arg: A motion command with argument.
;;
;;  - map: A mapping command.
;;
;;  - special: The function of this command is called but otherwise
;;                the key-state looks like this command has never been
;;                seen, but this command may modify the key-state
;;                directly.
;;
;;  - ignore: This command looks like it is not part of the keymap.
;;
(defstruct (vim:command
            (:constructor vim:make-command))
  type        ; The type of the command.
  function    ; Function to be invoked.
  motion-type ; inclusive, exclusive, inclusive-linewise or exclusive-linewise
  )

(defstruct (vim:node
            (:constructor vim:make-node))
  key  ; The key bound to this node
  cmd  ; The command bound to this node
  next ; A map of the following commands
  )

(defun vim:simple-command-p (cmd)
  "Returns t iff cmd is a simple-command."
  (eq (vim:command-type cmd) 'simple))

(defun vim:simple-arg-command-p (cmd)
  "Returns t iff cmd is a simple-command with an argument."
  (eq (vim:command-type cmd) 'simple-arg))

(defun vim:complex-command-p (cmd)
  "Returns t iff cmd is a complex-command."
  (eq (vim:command-type cmd) 'complex))

(defun vim:motion-command-p (cmd)
  "Returns t iff cmd is a motion-command."
  (eq (vim:command-type cmd) 'motion))

(defun vim:motion-arg-command-p (cmd)
  "Returns t iff cmd is a motion-command with an argument."
  (eq (vim:command-type cmd) 'motion-arg))

(defun vim:map-command-p (cmd)
  "Returns t iff cmd is a map-command."
  (eq (vim:command-type cmd) 'map))

(defun vim:special-command-p (cmd)
  "Returns t iff cmd is a special-command."
  (eq (vim:command-type cmd) 'special))

(defun vim:ignore-command-p (cmd)
  "Returns t iff cmd is a ignore-command."
  (eq (vim:command-type cmd) 'ignore))


(defun vim:get-subnode (node key)
  "Returns the sub-node associated with key."
  (let ((sub-node (assoc key (vim:node-next node))))
    (if sub-node
        (cdr sub-node)
      nil)))


(defun vim:add-node (node keys cmd)
  "Adds the command bound to sequence chars to node."

  (labels
      ((add-node (node pos len cmd)
                 (case (- len pos)
                   (0 (error "Can't add empty key-sequence to map."))
   
                   (1
                    (let* ((key (elt keys pos))
                           (subnode (vim:get-subnode node key)))
                      (if subnode
                          (progn
                            (setf (vim:node-cmd subnode) cmd))
                        (progn
                          (setq subnode (vim:make-node :key key :cmd cmd))
                          (setf (vim:node-next node)
                                (cons (cons key subnode) (vim:node-next node)))))
                      subnode))
                   
                   (t
                    (let* ((key (elt keys pos))
                           ;; get or create the next node
                           (parent (or (vim:get-subnode node key)
                                       (add-node node pos 1 nil))))
                      (add-node parent (1+ pos) len cmd))))))
    
    (add-node node 0 (length keys) cmd)))


(defun* vim:def-motion (keys func
                             &key
                             (mode vim:normal-mode)
                             (arg nil)
                             (type 'inclusive))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type (if arg 'motion-arg 'motion)
                                  :motion-type type
                                  :function func)))

(defun* vim:def-simple (keys func &key (mode vim:normal-mode) (arg nil))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type (if arg 'simple-arg 'simple)
                                  :function func)))

(defun* vim:def-complex (keys func &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type 'complex
                                  :function func)))

(defun* vim:def-map (keys rhs &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type 'map
                                  :function rhs)))

(defun* vim:def-special (keys func &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type 'special
                                  :function func)))

(defun* vim:def-ignore (keys &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type 'ignore)))

