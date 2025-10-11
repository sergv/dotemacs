(require 'ert)
(require 'json-error)

(defmacro json-error-test-highlighting (name json-content expected-line-num expected-line-content)
  "Test macro to verify error highlighting.
NAME is the test name.
JSON-CONTENT is the JSON string to test.
EXPECTED-LINE-NUM is the line number that should be highlighted.
EXPECTED-LINE-CONTENT is the content of the line that should be highlighted."
  `(ert-deftest ,(intern (concat "json-error-test-highlighting-" name)) ()
     ,(format "Test highlighting for %s" name)
     (with-temp-buffer
       (insert ,json-content)
       (setq json-error-highlighting-style 'context)
       (json-error-mode 1)
       (json-error-reparse)
       ;; Check that we have exactly one error
       (should (= (length json-error-parsed-errors) 1))
       ;; Check that we have exactly one overlay
       (let ((overlays (seq-filter (lambda (o) (overlay-get o 'json-error-error))
                                   (overlays-in (point-min) (point-max)))))
         (should (= (length overlays) 1))
         (let* ((ovl (car overlays))
                (start (overlay-start ovl))
                (end (overlay-end ovl))
                (highlighted-line-num (line-number-at-pos start))
                (highlighted-content (buffer-substring-no-properties start end)))
           (should (= highlighted-line-num ,expected-line-num))
           (should (string= highlighted-content ,expected-line-content)))))))

;; Test missing comma
(json-error-test-highlighting "missing-comma-object"
  "{\n  \"a\": 1\n  \"b\": 2\n}"
  2
  "  \"a\": 1")

(json-error-test-highlighting "missing-comma-array"
  "[\n  1\n  2\n]"
  2
  "  1")

;; Test missing colon
(json-error-test-highlighting "missing-colon-simple"
  "{\n  \"a\" 1\n}"
  2
  "  \"a\" 1")

(json-error-test-highlighting "missing-colon-complex"
  "[
  {
    \"createdAt\": \"2025-06-30 17:06:49+00:00\",
    \"title\" \"Buildkite danger pipeline [danger-mark-commit-green-monorepo-tests-danger] executed (build #229)\"
  }
]"
  4
  "    \"title\" \"Buildkite danger pipeline [danger-mark-commit-green-monorepo-tests-danger] executed (build #229)\"")

;; Test unterminated strings
(json-error-test-highlighting "unterminated-string-newline"
  "{\n  \"key\": \"unterminated\n  \"next\": \"value\"\n}"
  2
  "  \"key\": \"unterminated")

(json-error-test-highlighting "unterminated-string-eof"
  "{\n  \"key\": \"value"
  2
  "  \"key\": \"value")

(json-error-test-highlighting "unterminated-string-in-array"
  "[
  {
    \"createdAt\": \"2025-06-30 17:06:49+00:00,
    \"title\": \"Buildkite danger pipeline\"
  }
]"
  3
  "    \"createdAt\": \"2025-06-30 17:06:49+00:00,")

;; Test trailing comma
(json-error-test-highlighting "trailing-comma-object"
  "{\n  \"a\": 1,\n}"
  3
  "}")

(json-error-test-highlighting "trailing-comma-array"
  "[\n  1,\n]"
  3
  "]")

;; Test invalid values
(json-error-test-highlighting "invalid-token"
  "{\n  \"a\": undefined\n}"
  2
  "  \"a\": undefined")

;; Test nested structure errors
(json-error-test-highlighting "nested-missing-comma"
  "{\n  \"outer\": {\n    \"a\": 1\n    \"b\": 2\n  }\n}"
  3
  "    \"a\": 1")

;; Test for 'following' style as well
(ert-deftest json-error-test-highlighting-following-style ()
  "Test that following style highlights from error to EOF"
  (with-temp-buffer
    (insert "{\n  \"key\": \"unterminated\n}")
    (setq json-error-highlighting-style 'following)
    (json-error-mode 1)
    (json-error-reparse)
    (let ((overlays (seq-filter (lambda (o) (overlay-get o 'json-error-error))
                                (overlays-in (point-min) (point-max)))))
      (should (= (length overlays) 1))
      (let ((ovl (car overlays)))
        ;; Should highlight from error position to end of buffer
        (should (= (overlay-end ovl) (point-max)))))))

;; Run the tests
(provide 'json-error-highlighting-test)
