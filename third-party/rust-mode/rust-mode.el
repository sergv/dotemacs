;;; rust-mode.el --- A major-mode for editing Rust source code -*-lexical-binding: t-*-

;; Version: 1.0.6
;; Author: Mozilla <rust-mode@noreply.github.com>
;; Url: https://github.com/rust-lang/rust-mode
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:

;; This package implements a major-mode for editing Rust source code.

;;; Code:
(require 'rust-common)

(eval-when-compile
  (require 'rx)
  (require 'subr-x))

(require 'rust-cargo)
(require 'rust-rustfmt)

;;; Customization

(defgroup rust-mode nil
  "Support for Rust code."
  :link '(url-link "https://www.rust-lang.org/")
  :group 'languages)

(provide 'rust-mode)

(require 'rust-utils)

;;; rust-mode.el ends here
