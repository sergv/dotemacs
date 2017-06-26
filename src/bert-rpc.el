;; bert-rpc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 June 2017
;; Description:

(require 'bert)
(require 'bindat)

(defmacro bert-rpc--fold-interaction (interaction-type on-sync on-async)
  (declare (indent 1))
  `(pcase ,interaction-type
     (`synchronous
      ,on-sync)
     (`asynchronous
      ,on-async)
     (invalid
      (error "Invalid interaction type: %s" invalid))))

(cl-defstruct bert-rpc-promise
  ready ;; whether data was set, boolean
  data)

(defun bert-rpc-promise--return-data (promise data)
  (setf (bert-rpc-promise-ready promise) t
        (bert-rpc-promise-data promise) data))

(defun bert-rpc-promise--extract-data (promise)
  (let ((result (bert-rpc-promise-data promise)))
    (setf (bert-rpc-promise-ready promise) nil
          (bert-rpc-promise-data promise) nil)
    result))

;;;###autoload
(defun* bert-rpc-connect (&key name port buffer interaction async-callback)
  (condition-case err
      (let ((proc
             (make-network-process
              :name name
              :host 'local
              :service port
              :buffer buffer
              :coding 'no-conversion
              :filter-multibyte nil
              ;; Don't query to close this process on Emacs exit.
              :noquery t
              :filter #'bert-rpc--process-filter
              :sentinel #'bert-rpc--sentinel)))
        (let ((sync-promise
               (bert-rpc--fold-interaction interaction
                 (make-bert-rpc-promise :ready nil :data nil)
                 nil)))
          (process-put proc
                       'bert-rpc--response-decoder
                       (bert-rpc--make-decoder
                        (bert-rpc--fold-interaction interaction
                          (lambda (response)
                            (bert-rpc--on-synchronous-message-decoded sync-promise response))
                          (lambda (response)
                            (bert-rpc--on-asynchronous-message-decoded async-callback response)))))
          (process-put proc 'bert-rpc--interaction-type interaction)
          (bert-rpc--fold-interaction interaction
            (progn
              (cl-assert (null async-callback))
              (cl-assert (bert-rpc-promise-p sync-promise))
              (process-put proc 'bert-rpc--sync-promise sync-promise))
            (progn
              (cl-assert (functionp async-callback))
              (process-put proc 'bert-rpc--async-callback async-callback)))

          ;; (set-process-filter-multibyte proc t)
          ;; (set-process-coding-system    proc 'utf-8 'utf-8)
          ;; (set-process-filter           proc #'bert-rpc--process-filter)
          proc))
    (file-error
     (error "Failed to connect: %s" err))))

(defun bert-rpc--sentinel (process message)
  (message "Bert-rpc connection for '%s' closed unexpectedly: %s" (process-name process) message)
  (bert-rpc--close process))

(defun bert-rpc--close (process)
  (kill-buffer (process-buffer process)))

(defun bert-rpc--make-decoder (on-request)
  "Make function that will receive new batch of data from server and decide
whether we have received complete message already or must wait for more data to
arrive."
  (letrec ((size-field-length 4)
           (expected-size-length size-field-length)
           (size-bytes nil)
           (expected-request-size nil)
           (request-chunks nil)
           (decode
            (lambda (data)
              (cl-assert (stringp data))
              (let ((len (length data)))
                (if (= 0 expected-size-length)
                  (progn
                    (cl-assert (numberp expected-request-size))
                    (cl-assert (not (= 0 expected-request-size)))
                    (if (< len expected-request-size)
                      (setf expected-request-size (- expected-request-size len)
                            request-chunks (cons data request-chunks))
                      (let* ((last-request-chunk (subseq data 0 expected-request-size))
                             (trailing-data (subseq data expected-request-size))
                             (request
                              (with-temp-buffer
                                (dolist (chunk (nreverse (cons last-request-chunk request-chunks)))
                                  (insert chunk))
                                (buffer-substring-no-properties (point-min) (point-max)))))
                        (setf expected-size-length size-field-length
                              size-bytes nil
                              expected-request-size nil
                              request-chunks nil)
                        (funcall on-request request)
                        (unless (= 0 (length trailing-data))
                          (funcall decode trailing-data)))))
                  ;; Start receiving new message
                  (if (< len expected-size-length)
                    ;; Store bytes to get the length later
                    (setf expected-size-length (- expected-size-length len)
                          size-bytes (concat size-bytes data))
                    ;; If there's enough bytes to get the length
                    (let* ((raw-length (concat size-bytes
                                               (subseq data 0 expected-size-length)))
                           (trailing-data (subseq data expected-size-length)))
                      (setf expected-request-size (bert-rpc--bert-decode-length raw-length)
                            expected-size-length 0
                            size-bytes nil)
                      (unless (= 0 (length trailing-data))
                        (funcall decode trailing-data)))))))))
    decode))

(defun bert-rpc--process-filter (proc data)
  (cl-assert (stringp data))
  (let ((decoder (process-get proc 'bert-rpc--response-decoder)))
    (cl-assert (functionp decoder))
    (funcall decoder data)))

;;;###autoload
(defun bert-rpc-disconnect (proc)
  (delete-process proc))

(defun bert-rpc--send (proc obj)
  (process-send-string proc
                       (bert-rpc--berp-encode-message (bert-pack obj))))

(defun bert-rpc--berp-encode-message (message)
  (cl-assert (stringp message))
  (concat (bert-rpc--bert-encode-length (length message))
          message))

(defun bert-rpc--bert-encode-length (len)
  (cl-assert (numberp len))
  (bindat-pack '((len u32)) `((len . ,len))))

(defun bert-rpc--bert-decode-length (encoded-len)
  (cl-assert (stringp encoded-len))
  (cdr (assq 'len
             (bindat-unpack '((len u32)) encoded-len))))

;;;###autoload
(defun bert-rpc-call-sync (proc module function args)
  (declare (indent 1))
  (cl-assert (symbolp module))
  (cl-assert (symbolp function))
  (cl-assert (listp args))
  (bert-rpc--send proc
                  (vector 'call module function args))
  (let ((promise (process-get proc 'bert-rpc--sync-promise)))
    (while (not (bert-rpc-promise-ready promise))
      (accept-process-output proc))
    (bert-rpc-promise--extract-data promise)))

(defun bert-rpc--on-synchronous-message-decoded (sync-promise response)
  (let ((event (bert-unpack (string-to-unibyte response))))
    (case (aref event 0)
      ;; -- {reply, Result}
      (reply
       (bert-rpc-promise--return-data sync-promise (aref event 1)))
      ;; -- {error, {Type, Code, Class, Detail, Backtrace}}
      (error
       (let ((err (aref event 1)))
         (message "BERT-RPC error: %s: %s"
                  (aref err 0)
                  (aref err 3)))))))

(defun bert-rpc--on-asynchronous-message-decoded (async-callback response)
  (let ((event (bert-unpack (string-to-unibyte response))))
    (case (aref event 0)
      ;; -- {reply, Result}
      (reply
       (funcall async-callback (aref event 1)))
      ;; -- {error, {Type, Code, Class, Detail, Backtrace}}
      (error
       (let ((err (aref event 1)))
         (message "BERT-RPC error: %s: %s"
                  (aref err 0)
                  (aref err 3)))))))

;;;###autoload
(defun bert-rpc-call-async (proc module function args)
  "Do BERT-RPC call to PROC asynchronously. Upon completion it
will invoke ASYNC-CALLBACK passed to `bert-rpc-connect'."
  (declare (indent 1))
  (cl-assert (symbolp module))
  (cl-assert (symbolp function))
  (cl-assert (listp args))
  (bert-rpc--send proc
                  (vector 'call module function args)))

(provide 'bert-rpc)

;; Local Variables:
;; End:

;; bert-rpc.el ends here
