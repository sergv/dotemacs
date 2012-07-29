;; set-up-platform.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 25 July 2012
;; Description:

(require 'common)
(require 'more-scheme)

(defvar +platform+ 'home-linux
  "May be 'home-linux, 'asus-netbook or 'netbook-linux. Range of platforms
may be expanded (extended?) in the future.")



(let ((system-type-file "~/system_type.sh"))
  (if (and (file-exist? system-type-file)
           (file-executable? system-type-file))
    (setf +platform+ (string->symbol
                      (with-temp-buffer
                        (call-process system-type-file
                                      nil
                                      (current-buffer))
                        (string-trim-whitespace
                         (buffer-substring-no-properties (point-min)
                                                         (point-max))))))
    (setf +platform+ 'home-linux)))

(unless (memq +platform+ '(home-linux asus-netbook netbook-linux))
  (error "+platform+ %s should be one of home-linux asus-netbook netbook-linux"
         +platform+))

;; Local Variables:
;; lexical-binding: t
;; End:

;; set-up-platform.el ends here
