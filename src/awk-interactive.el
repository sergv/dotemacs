;; awk-interactive.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 31 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; Use awk or sed on current region in interactive manner

(eval-when-compile
  (require 'cl)
  (require 'macro-util)

  (defvar awk-mode-map))

(require 'common)

(defconst awk-init-content
  (concat
   "# Keybindings:\n"
   "# C-m/f9 - execute awk program\n"
   "# S-f9   - restore original input\n"
   "# f6     - exit\n"
   "# up     - previous program\n"
   "# down   - next program\n"
   "#\n"
   "# Important variables:\n"
   "# FS - input field separator\n"
   "# NF - number of fields in current input record\n"
   "# NR - number of input records seen so far\n"
   "# String functions:\n"
   "# gensub(regexp, str, (\"g\"|[1-9])[, where]); returns new string\n"
   "# gsub(regexp, str[, where]); modifies string\n"
   "# sub(regexp, str[, where]); modifies string\n"
   "\n"
   "BEGIN { FS = \" +\" }\n"
   "{   <_> }\n"
   "END {  }\n")
  "Initial contents of awk buffer for entering program.")

(defconst awk-buffer-name "*AWK*"
  "Name of awk buffer.")

(defconst awk-bin "awk")


(defvar awk-original-input nil
  "String that will be passed to awk program as input.")

(defvar awk-output-begin nil
  "Marker that points to place where output should be inserted.")

(defvar awk-output-end-pos nil
  "Position that points to end of awk output in buffer.")

(defface awk-selection-face '((t (:underline "#268bd2")))
  "Face to highlight input and output
in buffer from where `awk' was invoked."
  :group 'c)

(defvar awk-overlay nil
  "Overlay that highlights input and output
in buffer from where `awk' was invoked.")

(defvar awk-window-config nil
  "Window configuration before awk invokation.")


(defvar awk-programs (make-hash-table :test #'equal :size 257)
  "Hash of text programs entered by user. Key is the program id.")

(defvar awk-program-ids nil
  "List of identifiers of programs entered by user.
Identifiers point to the global storage of programs `awk-programs'.")

(defvar awk-program-id 1
  "Currently active AWK program id. Corresponds to awk invokation")


;;;###autoload
(defun awk-on-region (begin end)
  "Initialize interactive awk session on selected region."
  (setf awk-original-input (buffer-substring-no-properties begin end)
        awk-window-config  (current-window-configuration)
        awk-output-begin   (copy-marker begin)
        awk-output-end-pos end
        awk-overlay        (make-overlay (marker-position awk-output-begin)
                                         awk-output-end-pos))
  (push awk-program-id awk-program-ids)
  ;; (delete-region begin end)
  (overlay-put awk-overlay
               'face
               'awk-selection-face)

  (switch-to-buffer-other-window awk-buffer-name)
  (erase-buffer)
  (awk-mode)
  ;; make these bindings very local
  (let ((map (copy-keymap awk-mode-map)))
    (def-keys-for-map map
      (("C-m" "<f9>") awk-send-input)
      ("<return>"     newline-and-indent)
      ("S-<f9>"       awk-restore-original-input)
      ("<f6>"         awk-exit)
      ("<up>"         awk-previous-program)
      ("<down>"       awk-next-program))
    (use-local-map map))
  (use-repl-modeline)

  (insert awk-init-content)
  (save-match-data
    (goto-char (point-min))
    (re-search-forward "<_>")
    (replace-match ""))
  (set-buffer-modified-p nil))

(defun awk-restore-original-input ()
  "Restore original input from buffer."
  (interactive)
  (with-current-buffer (marker-buffer awk-output-begin)
    (delete-region (marker-position awk-output-begin)
                   awk-output-end-pos)
    (goto-char (marker-position awk-output-begin))
    (insert awk-original-input)
    (setf awk-output-end-pos (point))
    (move-overlay awk-overlay
                  (marker-position awk-output-begin)
                  awk-output-end-pos)))

(defun awk-remove-program-comments (program-buffer)
  (with-temp-buffer
    (insert-buffer-substring-no-properties program-buffer)
    (goto-char (point-min))
    (flush-lines "^[ \t]*#")
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

(defun awk-send-input ()
  "Send input in `awk-original-input' to awk using program entered in
buffer `awk-buffer-name'. Output from awk will be inserted in buffer
in place of input."
  (interactive)
  (let ((awk-program (awk-remove-program-comments (get-buffer awk-buffer-name))))
    (with-current-buffer (marker-buffer awk-output-begin)
      (when awk-output-end-pos
        (delete-region (marker-position awk-output-begin)
                       awk-output-end-pos)
        (setf awk-output-end-pos nil))

      (goto-char (marker-position awk-output-begin))
      (let ((out-buf (current-buffer))
            (locale-old (getenv "LC_ALL")))
        ;; this is necessary to make awk understand [a-z], [A-Z], etc. correctly
        (setenv "LC_ALL" "C")
        (with-temp-buffer
          (insert awk-original-input)
          (call-process-region (point-min)
                               (point-max)
                               awk-bin
                               nil ;; don't delete from temp buffer
                               (list out-buf ;; mix stderr and stdout
                                     t)
                               t ;; do redisplay
                               ;; arguments to awk-bin
                               awk-program))
        (setenv "LC_ALL" locale-old))
      (setf awk-output-end-pos (point))
      (move-overlay awk-overlay
                    (marker-position awk-output-begin)
                    awk-output-end-pos))))

(defun awk-store-program ()
  "Store currently inputed program in `awk-programs'
with id value of `awk-program-id'."
  (puthash awk-program-id
           (buffer-substring (point-min) (point-max))
           awk-programs))

;;;###autoload
(defun awk-exit ()
  "Exit from current awk session saving inputed program."
  (interactive)
  (awk-store-program)

  (kill-buffer)
  (awk-restore-window-config)
  (goto-char (marker-position awk-output-begin))
  (delete-overlay awk-overlay)
  (setf awk-overlay nil)

  (setf awk-original-input nil
        awk-output-begin   nil
        awk-output-end-pos nil
        awk-program-id     (1+ awk-program-id)))

(defun awk-previous-program ()
  "Insert previous inputed awk program in buffer."
  (interactive)
  (if awk-program-ids
      (progn
        (when (= (car awk-program-ids) awk-program-id)
          (awk-store-program))
        (erase-buffer)
        (rotate-entry-list 'awk-program-ids)
        (insert (gethash (car awk-program-ids) awk-programs)))
    (error "awk-previous-program: error: no programs inputed, aborting")))

(defun awk-next-program ()
  "Insert next inputed awk program in buffer."
  (interactive)
  (if awk-program-ids
      (progn
        (when (= (car awk-program-ids) awk-program-id)
          (awk-store-program))
        (erase-buffer)
        (rotate-entry-list-backward 'awk-program-ids)
        (insert (gethash (car awk-program-ids) awk-programs)))
    (error "awk-next-program: error: no programs inputed, aborting")))

(defun awk-restore-window-config ()
  "Restore window configuration to the state before awk invokation."
  (when awk-window-config
    (set-window-configuration awk-window-config)
    (setf awk-window-config nil)))

;;; user functions

;;;###autoload
(defun awk-start ()
  "Start awk interaction using selected region or whole buffer."
  (interactive)
  ;; do not spawn two simultaneous awk sessions
  (when (get-buffer awk-buffer-name)
    (with-current-buffer (get-buffer awk-buffer-name)
      (awk-exit)))
  (if (region-active-p)
      (let ((begin (region-beginning))
            (end   (region-end)))
        (deactivate-mark)
        (vim:visual-mode-exit:wrapper)
        (awk-on-region begin end))
    (awk-on-region (point-min) (point-max))))

(provide 'awk-interactive)

;; Local Variables:
;; End:

;; awk-interactive.el ends here
