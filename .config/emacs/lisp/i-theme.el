;;; -*- lexical-binding: t -*-

;; TODO currently I only use one theme, I don't see that changing but if in
;;      future there is a need to swap themes often add logic where this file
;;      exports a function which can be called with the theme to select and
;;      then sets it appropriately (instead of say commenting out the old theme
;;      for the new one). Not a problem currently, just IF it's needed.

;;; Themes.

;;; Catpuccin theme for emacs
;;;
;;; docs: https://github.com/catppuccin/emacs
(use-package catppuccin-theme
 :init
  ;; If you change this interactively you must call (catppuccin-reload) after.
 (setq catppuccin-flavor 'mocha) ; 'mocha, 'frappe, 'latte, or 'macchiato
 (load-theme 'catppuccin t))

;;; Doom emacs theme package
;;;
;;; docs: https://github.com/doomemacs/them
(use-package doom-themes
  :after catppuccin-theme
  :config
  ;; Setting these to nil will universally disable that face.
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;;(load-theme 'catppuccin t)
  ;; (load-theme 'doom-monokai-ristretto t)
  ;; Customisations to theme colours (package specific are in their own config)
  (set-face-foreground 'line-number-current-line (catppuccin-get-color 'peach))
  ;; Using surface2 for comment text foreground is a bit too dim, use overlay0.
  ;;   Everything (even treesitter for this theme) inherits colours from
  ;;   font-lock-comment-face so we'll set that.
 ;; (set-face-attribute 'font-lock-comment-face nil
  ;;                    :foreground (catppuccin-get-color 'overlay0))
  ;; Colours for evil snipe highlighting.
 ;; (set-face-attribute 'evil-snipe-first-match-face nil
 ;;                     :foreground (catppuccin-get-color 'crust)
 ;;                     :background (catppuccin-get-color 'red))
;;  (set-face-attribute 'evil-snipe-matches-face nil
 ;;                     :foreground (catppuccin-get-color 'crust)
 ;;                     :background (catppuccin-get-color 'green))
  ;; Improves org-mode's faces
  (doom-themes-org-config))

(provide 'i-theme)
