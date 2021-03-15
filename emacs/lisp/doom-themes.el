;;; init-doom-themes.el -- doom-themes custom -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package doom-themes
  :straight t
  :config
  ;; settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  (set-face-background 'hl-line "#332C2C"))

(provide 'init-doom-themes)
;;; init-doom-themes.el ends here
