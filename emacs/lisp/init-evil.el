;;; init-evil.el -- evil mode and associated configuration
;;; Commentary:
;;; Code:

(defun config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ","))

(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (setq blink-cursor-mode 0
        evil-normal-state-cursor '(box "#BFB3B5")
        evil-insert-state-cursor '((bar . 2) "#F9CC7C")
        evil-visual-state-cursor '(hollow "#85DACC")
        evil-motion-state-cursor '(box "red") ;; TODO
        evil-replace-state-cursor '(box "red") ;; TODO
        evil-operator-state-cursor '(box "red")) ;; TODO

  (use-package evil-goggles
    :straight t
    :config
    (evil-goggles-mode))

  (use-package evil-leader
    :straight t
    :config
    (global-evil-leader-mode)
    (config-evil-leader))

  (use-package evil-snipe
    :straight t
    :config
    (evil-snipe-mode))

  (use-package evil-surround
    :straight t
    :config
    (global-evil-surround-mode 1))
)

(provide 'init-evil)
;;; init-evil.el ends here
