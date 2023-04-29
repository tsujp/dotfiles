;;; -*- lexical-binding: t -*-

;;; Highlight trailing whitespace.

(use-package whitespace
  :hook
  (prog-mode . whitespace-mode)
  (text-mode . whitespace-mode)
  :custom
  (whitespace-style '(face empty indentation::space tab trailing)))

(provide 'i-whitespace)
