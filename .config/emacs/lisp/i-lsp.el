;;; -*- lexical-binding: t -*-

;;; LSP configurations Eglot and the servers.
;;;
;;; docs: https://github.com/joaotavora/eglot
;;; docs: https://joaotavora.github.io/eglot/

;;; TODO: When updating to Emacs 29 this will be in-built so rework this file.

;;; Configuration

;;; Eglot core
;;;
;;; The actual LSP server must be installed independently of Eglot; Eglot does
;;;   not install the server for you.
;;;
;;; Comes pre-configured with a lot of LSPs, eval `eglot-server-programs` to see
;;;   the default list. To customise LSPs (e.g. not using Solargraph for Ruby
;;;   as an example) you can add the appropriate configuration to that list.
(use-package eglot)

(provide 'i-lsp)
