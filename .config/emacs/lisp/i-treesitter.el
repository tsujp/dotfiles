;;; TODO lexical

;;; Treesitter configuration (general).
;;;
;;; docs: https://emacs-tree-sitter.github.io

;;; Docs summary

;;;; Syntax tree on/off
;; Minor mode `tree-sitter-mode` provides buffer-local syntax tree which can
;;   be toggled on manually with `tree-sitter-mode` or via major mode hooks.

;;;; Syntax highlighting
;; Minor mode `tree-sitter-hl-mode` provides framework for tree queries which
;;   are then used by `tree-sitter-langs` or other language-specific packages to
;;   do the actual colouring of any syntax; replaces default `font-lock-mode`.

;; TODO
;; Should really check for dynamic module support (only if pre emacs 29)
;;   using `(functionp 'module-load)` which should evaluate to `t` but since
;;   we'll change to that soon I cannot be bothered.


;;; Configuration

;;;; Tree sitter core
(use-package tree-sitter)

;;;; Tree sitter syntax highlighting queries
(use-package tree-sitter-langs
  :after tree-sitter
  :config
  ;; Enables `tree-sitter-mode` for all supported major modes.
  (global-tree-sitter-mode)
  ;; Provide syntax tree framework for supported languages (syntax highlighting)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'i-treesitter)
