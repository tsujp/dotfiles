;;; -*- lexical-binding: t -*-

;;; Rainbow delimiters.

(use-package rainbow-delimiters
  :config
  ;; Starts in most programming modes. For a custom mode use `'foo-mode`.
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'i-rainbow-delimiters)
