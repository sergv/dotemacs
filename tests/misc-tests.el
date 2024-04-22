;; misc-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 April 2024
;; Description:

(require 'ert)

(require 'company-mode-setup)

(ert-deftest misc-tests/delete-duplicate-candidates-from-company-dabbrev-code-1 ()
  (should (equal (delete-duplicate-candidates-from-company-dabbrev-code
                  (copy-list
                   '(#("foo" 0 3
                       (:file "/root/test-file.txt" :line 100 :kind "Function"))
                     #("foo" 0 3
                       (company-backend company-dabbrev-code)))))
                 '(#("foo" 0 3
                     (:file "/root/test-file.txt" :line 100 :kind "Function"))))))

(ert-deftest misc-tests/delete-duplicate-candidates-from-company-dabbrev-code-2 ()
  (should (equal (delete-duplicate-candidates-from-company-dabbrev-code
                  (copy-list
                   '(#("foo" 0 3
                       (company-backend company-dabbrev-code))
                     #("foo" 0 3
                       (:file "/root/test-file.txt" :line 100 :kind "Function")))))
                 '(#("foo" 0 3
                     (:file "/root/test-file.txt" :line 100 :kind "Function"))))))

(ert-deftest misc-tests/delete-duplicate-candidates-from-company-dabbrev-code-3 ()
  (should (equal (delete-duplicate-candidates-from-company-dabbrev-code
                  (copy-list
                   '(#("foo" 0 3
                       (company-backend company-dabbrev-code)))))
                 '(#("foo" 0 3
                     (company-backend company-dabbrev-code))))))

(ert-deftest misc-tests/delete-duplicate-candidates-from-company-dabbrev-code-4 ()
  (should (equal (delete-duplicate-candidates-from-company-dabbrev-code
                  (copy-list
                   '(#("foo" 0 3
                       (company-backend company-dabbrev-code))
                     #("foo" 0 3
                       (:file "/root/test-file.txt" :line 100 :kind "Function"))
                     #("foo" 0 3
                       (company-backend company-dabbrev-code)))))
                 '(#("foo" 0 3
                       (:file "/root/test-file.txt" :line 100 :kind "Function"))))))

(provide 'misc-tests)

;; Local Variables:
;; End:

;; misc-tests.el ends here
