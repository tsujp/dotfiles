;;; init-rainbow-delimiters.el
;;; Commentary:
;;; Code:

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here
