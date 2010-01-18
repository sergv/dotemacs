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


(vim:defexcmd vim:excmd-write ((file-argument file))
  "Saves file `file'."
  (vim:save-buffer file :begin begin :end end :mustbenew t))

(vim:defexcmd vim:excmd-write-q ((file-argument file))
  "Overwrites file `file'."
  (vim:save-buffer file :begin begin :end end :mustbenew nil))

(vim:defexcmd vim:excmd-write-all ((file-argument file))
  "Saves all buffers."
  (save-some-buffers nil))

(vim:defexcmd vim:excmd-write-all-q ((file-argument file))
  "Overwrites all buffers."
  (save-some-buffers t))

(vim:defexcmd vim:excmd-edit ((file-argument file))
  "Visits a certain file."
  (case file
    ((t) (when (buffer-file-name)
           (find-file (buffer-file-name))))
    ((nil) t)
    (t (find-file file))))

(vim:defexcmd vim:excmd-buffer ((buffer-argument buffer))
  "Switches to another buffer."
  (switch-to-buffer buffer))
