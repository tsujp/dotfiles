(use-package lsp-mode
  :straight t
  ;;:commands (lsp lsp-deferred)
  :commands (lsp lsp-install-server)
  :init
  (setq lsp-keymap-prefix "C-c l"))
 ;; (lsp-enable-which-key-integration t))

(provide 'init-lsp-mode)
