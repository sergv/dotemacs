;; misc-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 April 2024
;; Description:

(require 'ert)

(require 'company-mode-setup)
(require 'transient-fixes)
(require 'shell-setup)

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

(ert-deftest misc-tests/transient-history--merge-entries-1 ()
  (should (equal (transient-history--merge-entries
                  '(transient-history
                    (magit-branch nil)
                    (magit-commit nil)
                    (magit-fetch nil)
                    (magit-gitignore nil)
                    (magit-log ("-n256" "--graph" "--decorate"))
                    (magit-merge nil)
                    (magit-push ("--force-with-lease"))
                    (magit-rebase nil ("--autostash"))
                    (magit-stash nil)
                    (nil))
                  '(transient-history
                    (magit-branch nil)
                    (magit-commit nil)
                    (magit-fetch nil)
                    (magit-gitignore nil)
                    (magit-log ("-n256" "--graph" "--decorate"))
                    (magit-merge nil)
                    (magit-push nil ("--force-with-lease"))
                    (magit-rebase nil ("--autostash"))
                    (magit-stash nil)
                    (nil)))
                 '(transient-history
                   (magit-branch nil)
                   (magit-commit nil)
                   (magit-fetch nil)
                   (magit-gitignore nil)
                   (magit-log ("-n256" "--graph" "--decorate"))
                   (magit-merge nil)
                   (magit-push ("--force-with-lease"))
                   (magit-rebase ("--autostash"))
                   (magit-stash nil)
                   (nil)))))

(ert-deftest misc-tests/transient-history--merge-entries-2 ()
  (should (equal (transient-history--merge-entries
                  '(transient-history
                    (magit-branch nil)
                    (magit-commit nil)
                    (magit-fetch nil)
                    (magit-gitignore nil)
                    (magit-log ("-n256" "--graph" "--decorate"))
                    (magit-merge nil)
                    (magit-push ("--force-with-lease"))
                    (magit-rebase nil ("--autostash"))
                    (magit-stash nil)
                    (nil))
                  '(transient-history
                    (magit-branch nil)
                    (magit-commit nil)
                    (magit-fetch nil)
                    (magit-gitignore nil)
                    (magit-log ("-n256" "--graph" "--decorate"))
                    (magit-merge nil)
                    (magit-push ("--force-with-lease") nil)
                    (magit-rebase nil ("--autostash"))
                    (magit-stash nil)
                    (nil)))
                 '(transient-history
                   (magit-branch nil)
                   (magit-commit nil)
                   (magit-fetch nil)
                   (magit-gitignore nil)
                   (magit-log ("-n256" "--graph" "--decorate"))
                   (magit-merge nil)
                   (magit-push ("--force-with-lease"))
                   (magit-rebase ("--autostash"))
                   (magit-stash nil)
                   (nil)))))

(ert-deftest misc-tests/ivy--regex-fuzzy-1 ()
  (should (equal (ivy--regex-fuzzy " a")
                 '(("\\( \\)[^a\n]*\\(a\\)" . t)))))

(ert-deftest misc-tests/ivy--regex-fuzzy-2 ()
  (should (equal (ivy--regex-fuzzy "a ")
                 '(("\\(a\\)" . t)))))

(ert-deftest misc-tests/ivy--regex-fuzzy-3 ()
  (should (equal (ivy--regex-fuzzy "a b")
                 '(("\\(a\\)" . t) ("\\(b\\)" . t)))))

(ert-deftest misc-tests/shell-dirtrack-1 ()
  (save-match-data
    (let* ((dir "/home/sergey/projects/haskell/projects/stackage-build-docker/work")
           (str (concat "[docker]root@04f0355e49f1:" dir "#")))
      (should (string-match (car shell-dirtrack-entry)
                            str))
      (should (equal dir
                     (match-string (cadr shell-dirtrack-entry) str))))))

(ert-deftest misc-tests/shell-dirtrack-2 ()
  (save-match-data
    (let* ((dir "/home/sergey/projects/haskell/projects/stackage-build-docker/work")
           ;; Where \r comes from is not entirely clear but it does so we have
           ;; to plan for it.
           (str (concat "\r[docker]root@04f0355e49f1:" dir "#")))
      (should (string-match (car shell-dirtrack-entry)
                            str))
      (should (equal dir
                     (match-string (cadr shell-dirtrack-entry) str))))))

(ert-deftest misc-tests/shell-dirtrack-3 ()
  (save-match-data
    (let* ((dir "~/.emacs.d")
           ;; Where \r comes from is not entirely clear but it does so we have
           ;; to plan for it.
           (str (concat "sergey@home:" dir "$")))
      (should (string-match (car shell-dirtrack-entry)
                            str))
      (should (equal dir
                     (match-string (cadr shell-dirtrack-entry) str))))))

(provide 'misc-tests)

;; Local Variables:
;; End:

;; misc-tests.el ends here
