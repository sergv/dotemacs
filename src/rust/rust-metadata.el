;; rust-metadata.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  3 April 2020
;; Description:

(require 'json)

;;;###autoload
(defun rust-metadata-get-full-metadata ()
  (with-temp-buffer
    (call-process "cargo" nil '(t nil) nil
                  "metadata"
                  "--format-version" "1"
                  "--color" "never"
                  "--no-deps")
    (goto-char (point-min))
    (condition-case err
        (let ((json-array-type 'vector)
              (json-object-type 'alist))
          (json-read))
      (error (error "Failed to deserialize cargo metadata output from json: %s\n%s"
                    err
                    (buffer-substring-no-properties (point) (point-max)))))))

;;;###autoload
(defun rust-metadata-targets (meta f)
  (if-let ((packages (cdr-safe (assq 'packages meta))))
      (mapcan (lambda (pkg)
                (aif (cdr-safe (assq 'targets pkg))
                    (funcall f  it)
                  (error "No 'targets within metadata 'packages field: %s" pkg)))
              packages)
    (error "No 'packages within metadata: %s" meta)))

;;;###autoload
(defun rust-metadata-targets-by-type (typ meta)
  (rust-metadata-targets
   meta
   (lambda (targets)
     (--map (cdr-safe (assq 'name it))
            (--filter (v-member typ (cdr-safe (assq 'kind it)))
                      (vector->list targets))))))

;;;###autoload
(defun rust-metadata-package-names (meta)
  (if-let ((packages (cdr-safe (assq 'packages meta))))
      (v--map (cdr-safe (assq 'name it)) packages)
    (error "No 'packages within metadata: %s" meta)))

;;;###autoload
(defun rust-metadata-binary-targets (meta)
  (rust-metadata-targets-by-type "bin" meta))

;;;###autoload
(defun rust-metadata-library-targets (meta)
  (rust-metadata-targets-by-type "lib" meta))

;;;###autoload
(defun rust-metadata-example-targets (meta)
  (rust-metadata-targets-by-type "example" meta))

;;;###autoload
(defun rust-metadata-test-targets (meta)
  (rust-metadata-targets-by-type "test" meta))

;;;###autoload
(defun rust-metadata-benchmark-targets (meta)
  (rust-metadata-targets-by-type "bench" meta))

;;;###autoload
(defun rust-metadata-build-script-targets (meta)
  (rust-metadata-targets-by-type "custom-build" meta))

(provide 'rust-metadata)

;; Local Variables:
;; End:

;; rust-metadata.el ends here
