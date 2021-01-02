;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:

(defun config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ","))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-goggles
    :ensure t
    :config
    (evil-goggles-mode))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (config-evil-leader))

  (use-package evil-snipe
    :ensure t
    :config
    (evil-snipe-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))
)

(provide 'init-evil)
;;; init-evil.el ends here
