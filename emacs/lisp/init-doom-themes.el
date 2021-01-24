;;; init-doom-themes.el -- doom-themes custom -*- lexical-binding: t -*-
;;; Commentary:

;;; Currently this does not work, substitute-in-file-name or expand-file-path etc don't work?
;;; Code:

(use-package doom-themes
  :load-path (substitute-in-file-name "$XDG_CONFIG_HOME/emacs/site-lisp/emacs-doom-themes")
  :config
  ;; settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-ristretto t)
  (set-face-background 'hl-line "#332C2C"))

(provide 'init-doom-themes)
;;; init-doom-themes.el ends here
