;;; yaml-init.el -- yaml major mode -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun set-yaml-variable-name-face ()
  "Set font-lock-variable-name-face for missing syntax highlighting"
  (interactive)
  (set-face-foreground font-lock-variable-name-face "violet"))

(use-package yaml-mode
  :straight t
  :init
  (add-hook 'yaml-mode-hook 'set-yaml-variable-name-face))

(provide 'init-yaml)
;;; yaml-init.el ends here
