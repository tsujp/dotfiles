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

;;; Third party packages.
(require 'i-theme)
(require 'i-evil) ; vim
(require 'i-treesitter)
(require 'i-project) ; project management
(require 'i-lsp)
(require 'i-no-littering) ; no random files
(require 'i-whitespace) ; show invisibles
(require 'i-rainbow-delimiters) ; colourful brackets etc
(require 'i-mini-frame) ; minibuffer at top (like sublime text's C-p)
(require 'i-completions) ; completions frameworks et al.
(require 'i-diminish)
(require 'i-orderless)

(provide 'init)
