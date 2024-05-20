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
;; lsp-mode kinda working [start]
(use-package lsp-mode
  :init
  ;; Performance
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  ;(setenv "LSP_USE_PLISTS" "true")
  (setq lsp-keymap-prefix "C-l")
  :config
   (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nargo" "lsp"))
                    :activation-fn (lsp-activate-on "noir")
                    :server-id 'nargo))
  (add-to-list 'lsp-language-id-configuration
               '(noir-ts-mode . "noir"))
  :hook(
        (go-ts-mode . lsp)
        (noir-ts-mode . lsp)
        (rust-mode . lsp)
        (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)
;; lsp-mode [end]



;; lsp-bridge and it's dependencies
;; (use-package yasnippet
;;   :config
;;   (yas-global-mode t))

;; (use-package markdown-mode)

;; (use-package lsp-bridge
;;   :straight (lsp-bridge :type git
;;                         :host github
;;                         :repo "manateelazycat/lsp-bridge"
;;                         :files ("*.el" "*.py" "acm" "core" "langserver"
;;                                 "multiserver" "resources"))
;;   :after (yasnippet markdown-mode)
;;   :config
;;   (global-lsp-bridge-mode))
;; ;; (add-to-list 'load-path "~/.config/emacs/lsp-bridge")
;; ;; (require 'lsp-bridge)
;; ;; (setq lsp-bridge-enable-log t)
;; ;; (global-lsp-bridge-mode)

;; Noir treesitter.
(use-package noir-ts-mode
  :straight (noir-ts-mode :type git
                          :host github
                          :repo "hhamud/noir-ts-mode"
                          :files ("*.el")))

;; (use-package eglot
;;   :defer t
;;   :custom
;;   (eglot-send-changes-idle-time 0.1)
;;   (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
;;   :hook (typescript-ts-mode . eglot-ensure))

;; TODO: move elsewhere later on
;; eglot lsp for noir i can move elsewhere later on
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(noir-ts-mode . ("nargo" "lsp"))))

;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match
;; that used by the user's shell.

;; This is particularly useful under Mac OS X and macOS, where GUI
;; apps are not started from a shell."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string
;; 			  "[ \t\n]*$" "" (shell-command-to-string
;; 					  "$SHELL --login -c 'echo $PATH'"
;; 						    ))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)

(provide 'i-lsp)
