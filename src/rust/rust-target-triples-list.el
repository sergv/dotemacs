;; rust-target-triples-list.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 23 February 2020
;; Description:

(require 'common)

;;;###autoload (autoload 'rust-target-triples "rust-target-triples-list" nil t)
(defun-once rust-target-triples
  (if-let (rustc-exec (cached-executable-find "rustc"))
      (with-temp-buffer
        (call-process rustc-exec
                      nil
                      (current-buffer)
                      nil
                      "--print"
                      "target-list")
        (split-string (buffer-substring-no-properties (point-min) (point-max))
                      "[\n\r]+"
                      t
                      "[ \t]+"))
    (error "Cannot find rustc in PATH - refusing to proceed")))

(provide 'rust-target-triples-list)

;; Local Variables:
;; End:

;; rust-target-triples-list.el ends here
