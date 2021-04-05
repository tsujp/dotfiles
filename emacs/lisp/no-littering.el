(use-package no-littering
  :straight t)

(setq auto-save-file-name-transforms
  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
  )

(provide 'init-no-littering)
