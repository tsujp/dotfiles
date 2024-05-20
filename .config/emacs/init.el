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

;;; TODO: Put this in it's own file that needs to run as close to startup (after a package manager has been installed) as possible.
;;; Make Emacs use $PATH from users' shell.
(use-package exec-path-from-shell
  :init
  ;(setq exec-path-from-shell-shell-name "bash")
  (setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package modus-themes
  :init
  (load-theme 'modus-vivendi :no-confirm))
;; TODO: Wrapper function so as to not have to specify the `i` and such that the imported file automatically `provides` the same as it's file name.

;;; Third party packages.
;; (require 'i-theme)
;; (require 'i-evil) ; Vim.

;; TRY OUT MEOW

;(use-package meow
 ; :config
  ;(meow-setup)
 ; (meow-global-mode 1))
(require 'i-meow)
;; END TRY

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
;; (require 'i-rust)
; (Require 'i-noir)
;; TODO: Load and unload keycast on demand (e.g. keybinding), see it's file.
(require 'i-keycast) ; Display current command and its keybinding.
(require 'i-which-key) ; Display available keybindings in minibuffer after delay.
                                        ;(require 'i-mu4e) ; Email client for Emacs using mu.

;(use-package vterm)
(require 'i-org)

;(use-package flycheck
;  :init (global-flycheck-mode))


;; (use-package avy
;;   :bind (("C-c j" . avy-goto-line) ("C-c k" . avy-goto-char-timer))
;;   )

;; (use-package boon-qwerty
;;   :straight (boon :type git
;;                   :host nil
;;                   :repo "/Users/tsujp/programming/derek"
;;                   :files ("*.el")))

;; TODO: Not sure I have the time to create an entire modal editing experience
;;       so... use evil for now?
;; (use-package dash)
;; (use-package expand-region)
;; (use-package boon
;;   :straight nil
;;   :after dash expand-region
;;   :load-path "/Users/tsujp/programming/derek"
;;   :config
;;   (require 'derek))
;; end above.

(use-package combobulate
  :straight (combobulate :type git
                         :host github
                         :repo "mickeynp/combobulate"
                         :files ("*.el"))
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (json-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode)))


;; TODO: move elsewhere later on
;; eglot lsp for noir i can move elsewhere later on
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(noir-ts-mode . ("nargo" "lsp"))))


(provide 'init)
