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
;; (use-package eglot)


;; It's alright, let's try lsp-bridge though.
;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-l")
;;   :hook(
;;         (rust-mode . lsp)
;;         (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

;; lsp-bridge and it's dependencies
(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package markdown-mode)

(use-package lsp-bridge
  :straight (lsp-bridge :type git
                        :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files ("*.el" "*.py" "acm" "core" "langserver"
                                "multiserver" "resources"))
  :after (yasnippet markdown-mode)
  :config
  (global-lsp-bridge-mode))
;; (add-to-list 'load-path "~/.config/emacs/lsp-bridge")
;; (require 'lsp-bridge)
;; (setq lsp-bridge-enable-log t)
;; (global-lsp-bridge-mode)

(provide 'i-lsp)
