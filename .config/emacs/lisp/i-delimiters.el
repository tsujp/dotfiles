;;; -*- lexical-binding: t -*-

;;; Delimiter matching and display (parentheses, brackets, braces, etc.)
;;; docs: https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html

;;; Built-in packages and options.
(use-package emacs
  :init
  (setq
   show-paren-delay 0 ; no matching paren show delay.
   electric-pair-preserve-balance t)
  :config
  ;; Matching face, shows the matched parentheses to the one at point. Intention
  ;;   for this is to be "wider" than highlight-parentheses so we'll add an
  ;;   underline for slightly easier visual grepping.
  (set-face-attribute 'show-paren-match nil
                      :foreground "#FFFF00"
                      :underline t)
  ;; Unbalanced.
  ;; (set-face-attribute 'show-paren-mismatch nil
  ;;                     :foreground "red") ; TODO: Colour.
  (show-paren-mode t) ;; TODO: Enable this for lisp (or more) internal highlighting.
  (electric-pair-mode t))

;;;; Highlight parentheses
;;;; Used to show surrounding parentheses of where point is since Emacs' i.e.
;;;;   shows the parentheses we are currently within, slightly different from
;;;;   what we're using show-paren-mode for which is only to highlight the
;;;;   matching parentheses of the one at point.
;;;;
;;;; docs: https://git.sr.ht/~tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :init
  (setq
   ;; Setting a single list element for colours means we'll only highlight a
   ;;   single level of surrounding pairs.
   highlight-parentheses-colors '("#FFFF00") ; Electric yellow.
   highlight-parentheses-delay 0) ; If performance tanks default was 0.137
  :config
  (global-highlight-parentheses-mode t))


;;;; Rainbow delimiters
;;;; Colour delimiters according to depth.
;;;;
;;;; docs: https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :custom-face
  (rainbow-delimiters-unmatched-face ((t (:background "red"))))
  (rainbow-delimiters-mismatched-face ((t (:background "purple"))))
  :config
  ;; Starts in most programming modes. For a custom mode use `'foo-mode`.
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'i-delimiters)
