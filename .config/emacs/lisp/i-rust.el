;;; -*- lexical-binding: t -*-

;;; Rust language configuration.
;;;
;;; docs: https://github.com/brotzeit/rustic

;;; Configuration

;;; Rustic
(use-package rustic
  :custom
  (rustic-lsp-client 'eglot))

(provide 'i-rust)
