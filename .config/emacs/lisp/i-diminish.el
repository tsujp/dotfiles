;;; -*- lexical-binding: t -*-

;;; Hide certain minor mode displays (lighters) from the modeline.

;;; docs: https://github.com/myrjola/diminish.el
(use-package diminish)

;; TODO: Actually use unimpaired?
(diminish 'evil-collection-unimpaired-mode)
(diminish 'evil-commentary-mode)
(diminish 'whitespace-mode)
(diminish 'evil-goggles-mode)
(diminish 'evil-snipe-local-mode)

(provide 'i-diminish)
