;;; init.el --- full emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;;
;;       ::::::::::   :::   :::       :::      ::::::::   ::::::::
;;      :+:         :+:+: :+:+:    :+: :+:   :+:    :+: :+:    :+:
;;     +:+        +:+ +:+:+ +:+  +:+   +:+  +:+        +:+
;;    +#++:++#   +#+  +:+  +#+ +#++:++#++: +#+        +#++:++#++
;;   +#+        +#+       +#+ +#+     +#+ +#+               +#+
;;  #+#        #+#       #+# #+#     #+# #+#    #+# #+#    #+#
;; ########## ###       ### ###     ###  ########   ########
;;

;; config largely taken/inspired/built-upon from:
;;   * https://github.com/purcell/emacs.d
;;   * https://github.com/aaronbieber/dotfiles

;;; Code:

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PATHS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; GENERAL (short default-feature configuration) - - - - - - - - - - - - - - -

;; TODO: window position in non-WM environments (macOS)
;; frame size and position
(if (eq system-type 'darwin)
    (if (display-graphic-p)
        (progn
          (setq default-frame-alist
                '(
                  (width . 150)
                  (height . 80))))))

;; font
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(font . "Menlo-13"))
      (set-face-attribute 'default t :font "Menlo-13"))
    (progn
      (add-to-list 'default-frame-alist '(font . "Cozette"))
      (set-face-attribute 'default t :font "Cozette")))

    
;; by default any runtime customisations or generated elisp will be put into
;; init.el, we can change that to `custom.el` to keep our init clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; utf8 encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; hilight current line
(global-hl-line-mode)

;; show matching parenthesis without delay
(show-paren-mode 1)
(setq show-paren-delay 0)

;; TODO: check this one; shows logical-line indicators
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (setq-default left-fringe-width nil)

;; show trailing whitespace
;; (setq-default indicate-empty-lines t)
;; (whitespace-mode 'trailing)

;; never indent with tabs (by default) we will (later, and where appropriate)
;; respect editorconfig though
(setq-default indent-tabs-mode nil)

;; no welcome screen message
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; hide scroll, menu, and tool bars
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; replace having to type `yes` with `y` and `no` with `n`
(fset 'yes-or-no-p 'y-or-n-p)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; DEFAULT FEATURES (long default-feature configuration) - - - - - - - - - - -


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PACKAGES  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; TODO: delete/refactor these 3 below
;; (require 'init-utils)
;; (require 'init-site-lisp) ;; must come before elpa
;; (require 'init-elpa)      ;; help install packages; calls (package-initialize)

;; helpers / core
(require 'init-straight)    ; straight.el (must be first)
(require 'init-use-package) ; use-package
(require 'init-no-littering)

;; until PR merged doom themes here to prevent relative path weirdness
(use-package doom-themes
  :load-path "site-lisp/emacs-doom-themes"
  :config
  ;; settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-ristretto t)
  (set-face-background 'hl-line "#332C2C"))

;;(require-package 'diminish)
;;(maybe-require-package 'scratch)
(require 'init-evil)
(require 'init-modeline)
;;(require 'init-rainbow-delimiters)
;;(require 'init-which-key)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
