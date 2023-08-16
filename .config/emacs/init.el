;;; -*- lexical-binding: t -*-

;;;
;;;       ::::::::::   :::   :::       :::      ::::::::   ::::::::
;;;      :+:         :+:+: :+:+:    :+: :+:   :+:    :+: :+:    :+:
;;;     +:+        +:+ +:+:+ +:+  +:+   +:+  +:+        +:+
;;;    +#++:++#   +#+  +:+  +#+ +#++:++#++: +#+        +#++:++#++
;;;   +#+        +#+       +#+ +#+     +#+ +#+               +#+
;;;  #+#        #+#       #+# #+#     #+# #+#    #+# #+#    #+#
;;; ########## ###       ### ###     ###  ########   ########
;;;

;;; config largely taken/inspired/built-upon from:
;;;   * https://github.com/purcell/emacs.d
;;;   * https://github.com/aaronbieber/dotfiles

;; Uncomment to show backtrace if errors on startup.
(setq debug-on-error t)

(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024))))

;; Add directories with `lisp` in them to load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'init-benchmarking) ; Measure startup time.

;;(cd "~/")

;;; Bootstrap straight.el, use-package.el, and general non-package-specific configuration.
(require 'i-packages)
(require 'i-general)

;;; OS specific general
(if (eq system-type 'darwin)
    (require 'i-macos))

;;; Default packages / package-less.
(require 'i-header-line)
(require 'i-mode-line)

;; TODO: Wrapper function so as to not have to specify the `i` and such that the imported file automatically `provides` the same as it's file name.

;;; Third party packages.
(require 'i-theme)
(require 'i-evil) ; Vim.
;; (require 'i-treesitter)
(require 'i-ts)
(require 'i-project) ; Project management.
(require 'i-lsp)
(require 'i-no-littering) ; No random files.
(require 'i-whitespace) ; Show invisibles.
(require 'i-delimiters) ; Parens, brackets, etc. matching and colouring.
;; (require 'i-mini-frame) ; Minibuffer at top (like sublime text's C-p).
(require 'i-completions) ; Completions frameworks et al.
(require 'i-diminish)
(require 'i-orderless)
(require 'i-rss)
;; TODO: Make programming language configuration easier? In one place I suppose?
(require 'i-rust)
; (require 'i-noir)
;; TODO: Load and unload keycast on demand (e.g. keybinding), see it's file.
(require 'i-keycast) ; Display current command and its keybinding.
(require 'i-which-key) ; Display available keybindings in minibuffer after delay.
                                        ;(require 'i-mu4e) ; Email client for Emacs using mu.

;(use-package vterm)




(provide 'init)
