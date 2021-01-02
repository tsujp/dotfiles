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


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PATHS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; GENERAL - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; utf8 encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; show matching parenthesis immediately
(show-paren-mode 1)
(setq show-paren-delay 0)

;; TODO: check this one; shows logical-line indicators
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)

;; show trailing whitespace
(setq-default indicate-empty-lines t)
(whitespace-mode 'trailing)

;; never indent with tabs (by default) we will respect .editorconfig though
(setq-default indent-tabs-mode nil)

;; no welcome screen message
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PACKAGES  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(require 'init-utils)
(require 'init-elpa)      ;; helpers to install packages; calls (package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; lisp/* packages
(require 'init-no-littering)
(require-package 'diminish)
(maybe-require-package 'scratch)
(require 'init-evil)
(provide 'init)

;; is this needed?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(scratch diminish gnu-elpa-keyring-update fullframe seq use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
