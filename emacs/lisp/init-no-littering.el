;; no-littering
(unless (package-installed-p 'no-littering)
   (package-refresh-contents)
   (package-install 'no-littering)
)
(setq auto-save-file-name-transforms
  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
)
(provide 'init-no-littering)
