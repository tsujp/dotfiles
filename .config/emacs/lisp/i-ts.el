;;; -*- lexical-binding: t -*-

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

;;;; Tree sitter grammars
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
(css "https://github.com/tree-sitter/tree-sitter-css")
(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
(json "https://github.com/tree-sitter/tree-sitter-json")
(markdown "https://github.com/ikatyang/tree-sitter-markdown")
(yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
         ))

;;;; TODO: In future change auto-mode-alist, interpreter-mode-alist instead.
;;;; Tree sitter major mode remapping
(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)))

;;;; Tree sitter syntax highlighting queries

(provide 'i-ts)
