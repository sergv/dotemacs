;; compilation-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 18 March 2018
;; Description:

(require 'ert)

(require 'compilation-setup)
(require 'haskell-compile)

(defvar compilation-tests--resources-root
  (concat +emacs-config-path+ "/tests/test-data"))

(defun compilation-error< (err-a err-b)
  (let ((file-a (compilation-error/filename err-a))
        (file-b (compilation-error/filename err-b)))
    (or (string< file-a file-b)
        (and (string= file-a file-b)
             (let ((line-a (compilation-error/line-number err-a))
                   (line-b (compilation-error/line-number err-b))))
             (or (< line-a line-b)
                 (and (= line-a line-b)
                      (let ((col-a (compilation-error/column-number err-a))
                            (col-b (compilation-error/column-number err-b)))
                        (< col-a col-b))))))))

(ert-deftest compilation-tests/haskell-compile-error-regexps-1 ()
  (save-match-data
    (with-temp-buffer
      (insert-file-contents (concat compilation-tests--resources-root
                                    "/haskell-compilation-log-stack.txt"))
      (goto-char (point-min))
      (haskell-compilation-mode)
      (let ((errors nil)
            (last-pos (point))
            (done nil))
        (while (and (not done)
                    (compilation-jump-to-next-error))
          (if (< (point) last-pos)
              (setf done t)
            (let ((err (compilation/get-selected-error)))
              (setf (compilation-error/compilation-root-directory err) nil)
              (push err errors)
              (setf last-pos (point)))))
        (let ((expected-literal-error-entries
               '(("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Data/Attoparsec/Interpreter.hs" 123 7)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Data/Attoparsec/Interpreter.hs" 124 7)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 238 8)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 251 95)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 271 29)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 283 28)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 284 28)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 306 39)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/System/Process/Read.hs" 306 90)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 59 8)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 70 8)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 81 8)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 97 27)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 163 17)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 171 8)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 187 17)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 222 15)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 245 15)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 316 23)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 317 23)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 318 52)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 400 16)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 401 16)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 403 33)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 403 46)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 414 49)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 421 22)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 422 13)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 429 28)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/PackageDump.hs" 464 41)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Installed.hs" 131 38)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Installed.hs" 162 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Installed.hs" 163 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Installed.hs" 164 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Installed.hs" 165 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Network/HTTP/Download/Verified.hs" 145 8)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Network/HTTP/Download/Verified.hs" 175 53)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Network/HTTP/Download/Verified.hs" 240 31)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Network/HTTP/Download/Verified.hs" 277 24)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Network/HTTP/Download/Verified.hs" 313 43)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/New.hs" 200 59)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Source.hs" 458 45)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 433 10)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 434 10)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 435 10)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 436 10)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 437 10)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 450 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 451 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 452 12)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1099 41)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1100 41)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1101 41)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1122 28)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1125 25)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1126 25)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1503 72)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1781 5)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1782 5)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1783 5)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Build/Execute.hs" 1784 5)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Setup.hs" 1504 11)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Setup.hs" 1505 11)
                 ("/home/sergey/projects/haskell/projects/thirdparty/stack/src/Stack/Setup.hs" 1554 63)))
              (expected-errors nil))

          (dolist (entry expected-literal-error-entries)
            (push (make-compilation-error
                   :compilation-root-directory nil
                   :filename (cl-first entry)
                   :line-number (cl-second entry)
                   :column-number (- (cl-third entry) compilation-first-column))
                  expected-errors))

          (sort errors #'compilation-error<)
          (sort expected-errors #'compilation-error<)
          (should (equal (length errors)
                         (length expected-errors)))
          (should (equal errors
                         expected-errors)))))))

(provide 'compilation-tests)

;; Local Variables:
;; End:

;; compilation-tests.el ends here
