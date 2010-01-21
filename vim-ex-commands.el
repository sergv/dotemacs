(provide 'vim-ex-commands)

(defun* vim:save-buffer (file-name &key begin end mustbenew append)
  "Saves the lines from `begin' to `end' to file `file-name'."
  (when file-name
    (with-current-buffer vim:ex-current-buffer
      (when (eq file-name t)
        (setq file-name (buffer-file-name))
        (unless file-name
          (error "Please specify a file-name for this buffer!")))

      (let (beg-pos end-pos)
        (when begin
          (setq beg-pos (save-excursion 
                          (goto-line begin)
                          (line-beginning-position)))
          (setq end-pos
                (setq end-pos (if end
                                  (save-excursion
                                    (goto-line end)
                                    (line-end-position))
                                beg-pos))))
        
        (if (and (null beg-pos)
                 (string= file-name (buffer-file-name)))
            (save-buffer)
          (write-region beg-pos end-pos file-name append nil nil mustbenew))))))


(vim:defcmd vim:cmd-write (motion (argument:file file) nonrepeatable)
  "Saves file `file'."
  (vim:save-buffer file
                   :begin (and motion (vim:motion-first-line motion))
                   :end (and motion (vim:motion-last-line motion))
                   :mustbenew t))

(vim:defcmd vim:cmd-write-q (motion (argument:file file) nonrepeatable)
  "Overwrites file `file'."
  (vim:save-buffer file
                   :begin (and motion (vim:motion-first-line motion))
                   :end (and motion (vim:motion-last-line motion))
                   :mustbenew nil))

(vim:defcmd vim:cmd-write-all (nonrepeatable)
  "Saves all buffers."
  (save-some-buffers nil))

(vim:defcmd vim:cmd-write-all-q (nonrepeatable)
  "Overwrites all buffers."
  (save-some-buffers t))

(vim:defcmd vim:cmd-edit ((argument:file file) nonrepeatable)
  "Visits a certain file."
  (if file
      (find-file file)
    (when (buffer-file-name)
      (find-file (buffer-file-name)))))

(vim:defcmd vim:cmd-buffer ((argument:buffer buffer) nonrepeatable)
  "Switches to another buffer."
  (if buffer
      (switch-to-buffer buffer)
    (switch-to-buffer (other-buffer))))

(vim:defcmd vim:cmd-quit (nonrepeatable)
  "Closes the current window, exits Emacs if this is the last window."
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error (save-buffers-kill-emacs))))))

(vim:defcmd vim:cmd-quit-q (nonrepeatable)
  "Closes the current window, exits Emacs if this is the last window."
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error (kill-emacs))))))

(vim:defcmd vim:cmd-quit-all (nonrepeatable)
  "Exits Emacs, asking for saving."
  (save-buffers-kill-emacs))

(vim:defcmd vim:cmd-quit-all-q (nonrepeatable)
  "Exits Emacs, without saving."
  (kill-emacs))

(vim:defcmd vim:cmd-save-and-quit (nonrepeatable)
  "Exits Emacs, without saving."
  (save-buffers-kill-emacs 1))

(vim:defcmd vim:cmd-save-and-close ((argument:file file) nonrepeatable)
  "Saves the current buffer and closes the window."
  (vim:cmd-write :argument file)
  (vim:cmd-quit))

(vim:defcmd vim:cmd-save-and-close-q ((argument:file file) nonrepeatable)
  "Saves the current buffer and closes the window."
  (vim:cmd-write-q :argument file)
  (vim:cmd-quit))
