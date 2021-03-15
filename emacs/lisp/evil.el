;;; init-evil.el -- evil mode and associated configuration
;;; Commentary:
;;; Code:

(defun config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ","))

(defun tsujp/save-and-kill-this-buffer ()
  "Save and then kill current buffer."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq blink-cursor-mode 0
        evil-normal-state-cursor '(box "#BFB3B5")
        evil-insert-state-cursor '((bar . 2) "#F9CC7C")
        evil-visual-state-cursor '(box "#85DACC")
        evil-motion-state-cursor '(box "red") ;; TODO
        evil-replace-state-cursor '(box "red") ;; TODO
        evil-operator-state-cursor '(box "red")) ;; TODO
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'tsujp/save-and-kill-this-buffer)

  (use-package evil-collection
    :straight t
    :after evil
    :config
    (evil-collection-init))

  (use-package evil-commentary
    :straight t
    :after evil
    :config (evil-commentary-mode +1))

  (use-package evil-goggles
    :straight t
    :after evil
    :config
    (evil-goggles-mode))

  (use-package evil-leader
    :straight t
    :after evil
    :config
    (global-evil-leader-mode)
    (config-evil-leader))

  (use-package evil-snipe
    :straight t
    :after evil
    :config
    (evil-snipe-mode))

  (use-package evil-surround
    :straight t
    :after evil
    :config
    (global-evil-surround-mode 1)))

(provide 'init-evil)
;;; init-evil.el ends here
