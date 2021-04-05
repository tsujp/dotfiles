(use-package rainbow-mode
  :straight t
  :hook
  (prod-mode . rainbow-mode)
  :custom
  (rainbow-x-colors nil))

(provide 'init-rainbow-mode)
