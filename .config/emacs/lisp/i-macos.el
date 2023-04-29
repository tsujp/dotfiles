;;; -*- lexical-binding: t -*-

;;; macOS specific general configuration.

;; Renable menu bar since macOS has a global menubar outside of the application window.
(when (display-graphic-p)
  (menu-bar-mode 1))

;; Remap modifier keys.
(setq mac-command-modifier 'control
      mac-option-modifier 'super
      mac-control-modifier 'meta)

(provide 'i-macos)
